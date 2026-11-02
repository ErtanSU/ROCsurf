###############################################################################
## inference.R -- Wald and likelihood ratio intervals (Section 5)
##
##   problem()     log-likelihood, MLE, observed information (numDeriv::hessian)
##                 and the quantities h (written psi in Section 5): VUS, c_H1, c_H2
##   wald_ci()     Wald interval                                   (Section 5.1)
##                 gradients of h: VUS by numDeriv::grad; c_H by the implicit
##                 function theorem, grad c_H = -A^{-1} B, with A and B obtained
##                 by numDeriv::jacobian from the closed-form gradient of H in c
##   lr_limit()    one LR limit, Doganaksoy (2021) formulation:
##                   min / max h(theta)  subject to  l(theta) = kappa (Section 5.2)
##                 solved by SLSQP of NLopt (package nloptr)          (Section 5.3)
##   lr_ci()       both limits
##   profile_curve()  points of the profile log-likelihood (Figure 4)
##   profile_at()  conventional profile log-likelihood at a given value of h
##                 (max l subject to h = h0), used only as a check
###############################################################################

LR_TOL_L <- 1e-6     # tolerance on |l - kappa| for declaring convergence
LR_XTOL  <- 1e-8     # SLSQP relative tolerance on the parameters
LR_MAXEVAL  <- 500L  # SLSQP iteration limit (NLopt option maxeval; nloptr reports it as iterations)
LR_TOL_STAT <- 1e-6  # tolerance on 1 - |cos(grad h, grad l)| when the iteration limit is reached
## numerical derivatives: numDeriv::grad, Richardson extrapolation with r terms
GRAD_ARGS <- list(r = 2)
ngrad <- function(f, x) numDeriv::grad(f, x, method.args = GRAD_ARGS)
sgrad <- function(f, x) tryCatch(ngrad(f, x), error = function(e) rep(NA_real_, length(x)))

problem <- function(fit, xs) {
  D <- fit$dists; th <- fit$theta
  ll <- function(theta) loglik(theta, xs, D)
  ## observed information: block diagonal, one block per class
  I0 <- matrix(0, 6, 6)
  for (k in 1:3) {
    id <- 2 * k - c(1, 0)
    I0[id, id] <- -numDeriv::hessian(function(p) loglik_class(p, xs[[k]], D[k]), th[id])
  }
  V <- tryCatch(solve(I0), error = function(e) NULL)
  ok <- !is.null(V) && all(is.finite(V)) && all(eigen(I0, TRUE, TRUE)$values > 0)

  ## c_H(theta): local maximization of H started from c_H(theta_hat)
  cut0 <- cutoff(fit$model, "H")
  anchor <- cut0$c
  cache <- new.env(hash = TRUE); n_cache <- 0L
  cH <- function(theta) {
    key <- paste(format(theta, digits = 17), collapse = "|")
    if (!is.null(cache[[key]])) return(cache[[key]])
    if (any(theta <= 0)) return(c(NA_real_, NA_real_))
    m <- make_model(theta, D)
    r <- if (!is.null(cache$.last)) cutoff_local(m, "H", cache$.last, warm = TRUE) else list(ok = FALSE)
    if (!r$ok) r <- cutoff_local(m, "H", anchor)          # from c_H(theta_hat)
    if (r$ok) cache$.last <- r$c
    v <- if (r$ok) r$c else c(NA_real_, NA_real_)
    if (n_cache >= 5000L) {                       # at most 5000 stored values; .last is kept
      rm(list = ls(cache, sorted = FALSE), envir = cache); n_cache <<- 0L }
    cache[[key]] <- v; n_cache <<- n_cache + 1L
    v
  }
  h <- list(VUS = function(theta) if (any(theta <= 0)) NA_real_ else vus_glq(make_model(theta, D)),
            cH1 = function(theta) cH(theta)[1],
            cH2 = function(theta) cH(theta)[2])

  ## gradient of VUS(theta) by numDeriv::grad; the GLQ nodes y_j = Q2((v_j + 1)/2) depend only on
  ## theta2, so for theta1 and theta3 they are computed once (the result is the same)
  vus_grad <- function(theta) {
    if (any(theta <= 0)) return(rep(NA_real_, 6))
    q <- glq_nodes(256L); y <- qclass((q$v + 1) / 2, make_model(theta, D)[[2]])
    f13 <- function(t) { m <- make_model(c(t[1:2], theta[3:4], t[3:4]), D)
      0.5 * sum(q$w * pclass(y, m[[1]]) * (1 - pclass(y, m[[3]]))) }
    f2 <- function(t) vus_glq(make_model(c(theta[1:2], t, theta[5:6]), D))
    g13 <- ngrad(f13, theta[c(1, 2, 5, 6)]); g2 <- ngrad(f2, theta[3:4])
    c(g13[1:2], g2, g13[3:4])
  }
  ## gradient of c_H(theta) by the implicit function theorem: grad_c H(c_H(theta); theta) = 0 gives
  ## d c_H / d theta = -A^{-1} B, A = d(grad_c H)/dc (2 x 2), B = d(grad_c H)/d theta (2 x 6)
  gcache <- new.env(hash = TRUE); n_gcache <- 0L
  cH_grad <- function(theta) {
    key <- paste(format(theta, digits = 17), collapse = "|")
    if (!is.null(gcache[[key]])) return(gcache[[key]])
    cc <- cH(theta)
    G <- if (!all(is.finite(cc))) matrix(NA_real_, 2, 6) else {
      m <- make_model(theta, D)
      A <- numDeriv::jacobian(function(c) crit_grad_c(c, m, "H"), cc, method.args = GRAD_ARGS)
      B <- numDeriv::jacobian(function(t) crit_grad_c(cc, make_model(t, D), "H"), theta,
                              method.args = GRAD_ARGS)
      tryCatch(-solve(A, B), error = function(e) matrix(NA_real_, 2, 6))
    }
    if (n_gcache >= 5000L) { rm(list = ls(gcache, sorted = FALSE), envir = gcache); n_gcache <<- 0L }
    gcache[[key]] <- G; n_gcache <<- n_gcache + 1L
    G
  }
  hgrad <- list(VUS = vus_grad,
                cH1 = function(theta) cH_grad(theta)[1, ],
                cH2 = function(theta) cH_grad(theta)[2, ])
  list(theta = th, dists = D, ll = ll, l_hat = ll(th), I0 = I0, V = V, ok = ok && cut0$ok,
       h = h, hgrad = hgrad, cut = cut0)
}

## ---- Wald interval (Section 5.1) -----------------------------------------------------
wald_ci <- function(pr, name, level = 0.95) {
  h   <- pr$h[[name]]
  est <- h(pr$theta)
  g   <- pr$hgrad[[name]](pr$theta)
  se  <- sqrt(max(sum(g * (pr$V %*% g)), 0))
  z   <- stats::qnorm(1 - (1 - level) / 2)
  c(est = est, se = se, lower = est - z * se, upper = est + z * se)
}

## ---- coordinates used by the LR solver (Section 5.3) ----------------------------------
## eta = log(theta); z = (eta - eta_hat) / SE(eta_hat): the parameters on the
## log scale, standardized by their standard errors. No bounds are imposed.
.coords <- function(pr) {
  th <- pr$theta
  Veta <- pr$V / outer(th, th)
  s <- sqrt(diag(Veta))
  list(theta = function(z) exp(log(th) + s * z), Veta = Veta, s = s)
}

## ---- one LR limit (Sections 5.2-5.3) -------------------------------------------------------
## side = -1: lower limit (minimize h); side = +1: upper limit (maximize h)
lr_limit <- function(pr, name, side, kappa, z_start = NULL) {
  h  <- pr$h[[name]]; hg <- pr$hgrad[[name]]; C <- .coords(pr)
  hz  <- function(z) h(C$theta(z))
  hgz <- function(z) { th <- C$theta(z); hg(th) * th * C$s }     # chain rule: d theta / d z = theta * s
  lz  <- function(z) pr$ll(C$theta(z))
  h0 <- h(pr$theta)
  ## starting value: the point giving the Wald-type limit
  if (is.null(z_start)) {
    g_eta <- hg(pr$theta) * pr$theta                              # gradient on the log scale
    r0 <- sqrt(2 * (pr$l_hat - kappa))
    d_eta <- side * r0 * as.numeric(C$Veta %*% g_eta) / sqrt(sum(g_eta * (C$Veta %*% g_eta)))
    z_start <- d_eta / C$s
  }
  eval_f <- function(z) {
    v <- hz(z)
    gr <- if (is.finite(v)) tryCatch(hgz(z), error = function(e) NA) else NA
    if (!is.finite(v) || !all(is.finite(gr))) return(list(objective = 1e10, gradient = rep(0, 6)))
    list(objective = -side * v, gradient = -side * gr)
  }
  eval_g_eq <- function(z) {
    l <- lz(z)
    list(constraints = if (is.finite(l)) l - kappa else -1e10,
         jacobian = matrix(sgrad(lz, z), 1))
  }
  o <- tryCatch(nloptr::nloptr(x0 = z_start, eval_f = eval_f, eval_g_eq = eval_g_eq,
                               opts = list(algorithm = "NLOPT_LD_SLSQP", xtol_rel = LR_XTOL,
                                           maxeval = LR_MAXEVAL)),
                error = function(e) NULL)
  if (is.null(o)) return(list(limit = NA_real_, converged = FALSE, z = z_start))
  v  <- hz(o$solution); lv <- lz(o$solution)
  ## first-order (Lagrange) condition: grad h parallel to grad l, measured by 1 - |cos(angle)|
  stat <- NA_real_
  if (o$status == 5L) {
    gh <- tryCatch(hgz(o$solution), error = function(e) NA); gl <- sgrad(lz, o$solution)
    if (all(is.finite(c(gh, gl)))) stat <- 1 - abs(sum(gh * gl)) / sqrt(sum(gh^2) * sum(gl^2))
  }
  ## accepted: SLSQP met its stopping rule (status 1-4), or it reached the iteration limit
  ## (status 5) at a point satisfying the first-order condition; in both cases the constraint
  ## holds and the limit lies on the correct side of the estimate
  stop_ok <- o$status %in% 1:4 || (o$status == 5L && is.finite(stat) && stat < LR_TOL_STAT)
  conv <- stop_ok && is.finite(v) && is.finite(lv) &&
          abs(lv - kappa) < LR_TOL_L && side * (v - h0) > 0
  list(limit = v, converged = conv, z = o$solution, loglik = lv, iterations = o$iterations,
       status = o$status, stationarity = stat)
}

lr_ci <- function(pr, name, level = 0.95) {
  kappa <- pr$l_hat - stats::qchisq(level, 1) / 2
  lo <- lr_limit(pr, name, -1, kappa); up <- lr_limit(pr, name, +1, kappa)
  c(lower = lo$limit, upper = up$limit, converged = as.numeric(lo$converged && up$converged))
}

## ---- profile log-likelihood curve (Figure 4) --------------------------------------------
## the limits at the levels l_hat - q/2 are the points of the profile curve
profile_curve <- function(pr, name, q = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2,
                                         2.5, 3, stats::qchisq(0.95, 1), 4.5, 5.5, 6.5)) {
  rows <- list(data.frame(h = pr$h[[name]](pr$theta), loglik = pr$l_hat, side = 0, converged = TRUE))
  for (side in c(-1, 1)) for (qq in sort(q)) {
    r <- lr_limit(pr, name, side, pr$l_hat - qq / 2)
    rows[[length(rows) + 1L]] <- data.frame(h = r$limit, loglik = pr$l_hat - qq / 2,
                                            side = side, converged = r$converged)
  }
  out <- do.call(rbind, rows)
  out[order(out$h), ]
}

## ---- conventional profile log-likelihood at h = h0 (check only) -----------------------
profile_at <- function(pr, name, h0, z0 = NULL) {
  h <- pr$h[[name]]; C <- .coords(pr)
  hz <- function(z) h(C$theta(z)); lz <- function(z) pr$ll(C$theta(z))
  o <- nloptr::nloptr(x0 = if (is.null(z0)) rep(0, 6) else z0,
         eval_f = function(z) { l <- lz(z); g <- sgrad(lz, z)
           if (!is.finite(l) || !all(is.finite(g))) return(list(objective = 1e10, gradient = rep(0, 6)))
           list(objective = -l, gradient = -g) },
         eval_g_eq = function(z) { v <- hz(z); g <- sgrad(hz, z)
           if (!is.finite(v) || !all(is.finite(g))) return(list(constraints = 1e10, jacobian = matrix(0, 1, 6)))
           list(constraints = v - h0, jacobian = matrix(g, 1)) },
         opts = list(algorithm = "NLOPT_LD_SLSQP", xtol_rel = 1e-12, ftol_rel = 1e-15, maxeval = 1000))
  c(loglik = lz(o$solution), h = hz(o$solution), status = o$status)
}
