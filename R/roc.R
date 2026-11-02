###############################################################################
## roc.R -- ROC surface quantities (Sections 2-4; Section S3 of the Supplementary Material)
##
##   tpf()            true positive fractions p1, p2, p3          (eqs. 2-4)
##   crit_value()     J, MV, PM and the hybrid index H            (Sections 2.2, 3.1)
##   search_region()  [a, b] and delta0                            (Sections 3.2 and 6)
##   cutoff()         grid search + local maximization of a criterion
##   cutoff_local()   local maximization from a given start (used for c_H(theta))
##   vus_glq()        one-dimensional Gauss-Legendre quadrature    (Section 4.2)
##   vus_weibull_cf(), vus_erlang_cf(), vus_exp_cf()  closed forms  (Section S3, Suppl.)
###############################################################################

CRITS <- c("GJ", "PM", "MV", "H")

## ---- true positive fractions and criteria ------------------------------------------
tpf <- function(cc, model)
  c(pclass(cc[1], model[[1]]),
    pclass(cc[2], model[[2]]) - pclass(cc[1], model[[2]]),
    1 - pclass(cc[2], model[[3]]))

crit_all <- function(p) {
  J <- sum(p) - 1; MV <- prod(p); PM <- sqrt(sum((1 - p)^2))
  c(J = J, MV = MV, PM = PM, H = (J + MV) / PM)
}
## criterion to be MAXIMIZED (PM enters with a minus sign)
crit_of_p <- function(p, crit) {
  v <- crit_all(p)
  switch(crit, GJ = v[["J"]], MV = v[["MV"]], PM = -v[["PM"]], H = v[["H"]])
}
## gradient of the maximized criterion with respect to (p1, p2, p3)
crit_grad_p <- function(p, crit) {
  PM <- sqrt(sum((1 - p)^2))
  switch(crit,
    GJ = c(1, 1, 1),
    MV = c(p[2] * p[3], p[1] * p[3], p[1] * p[2]),
    PM = (1 - p) / PM,
    H  = { A <- sum(p) - 1 + prod(p)
           pr <- c(p[2] * p[3], p[1] * p[3], p[1] * p[2])
           ((1 + pr) * PM + A * (1 - p) / PM) / PM^2 })
}
crit_value <- function(cc, model, crit) crit_of_p(tpf(cc, model), crit)
## gradient with respect to the cut-off points (chain rule, densities)
crit_grad_c <- function(cc, model, crit) {
  g <- crit_grad_p(tpf(cc, model), crit)
  c(g[1] * dclass(cc[1], model[[1]]) - g[2] * dclass(cc[1], model[[2]]),
    g[2] * dclass(cc[2], model[[2]]) - g[3] * dclass(cc[2], model[[3]]))
}

## ---- search region (Sections 3.2 and 6) -----------------------------------------------
search_region <- function(model, q = 1e-4) {
  a <- min(vapply(model, function(m) qclass(q, m), 0))
  b <- max(vapply(model, function(m) qclass(1 - q, m), 0))
  list(a = a, b = b, delta0 = (b - a) / 1000)
}
in_region <- function(cc, R)
  all(is.finite(cc)) && cc[1] >= R$a && cc[2] <= R$b && cc[2] - cc[1] >= R$delta0

## ---- local maximization from a start ----------------------------------------------------
## Newton iterations on the stationarity condition (gradient in closed form,
## Hessian by central differences of the gradient), with step halving on the
## criterion; if they fail, optim (BFGS) followed by the same iterations.
.hess_c <- function(cc, model, crit, eps = 1e-5) {
  Hm <- matrix(0, 2, 2)
  for (k in 1:2) {
    d <- eps * max(1, abs(cc[k])); e <- c(0, 0); e[k] <- d
    Hm[, k] <- (crit_grad_c(cc + e, model, crit) - crit_grad_c(cc - e, model, crit)) / (2 * d)
  }
  (Hm + t(Hm)) / 2
}
.newton_c <- function(cc, model, crit, maxit = 50L, trust = Inf) {
  c_start <- cc
  f0 <- crit_value(cc, model, crit)
  if (!is.finite(f0)) return(NULL)
  for (it in seq_len(maxit)) {
    g <- crit_grad_c(cc, model, crit); Hm <- .hess_c(cc, model, crit)
    if (!all(is.finite(c(g, Hm)))) return(NULL)
    ev <- eigen(Hm, TRUE, TRUE)$values
    if (max(ev) >= 0 && is.finite(trust)) return(NULL)                     # strict (warm) mode
    st <- if (max(ev) < 0) tryCatch(-solve(Hm, g), error = function(e) NULL)
          else g / max(1, sqrt(sum(g^2)))                                 # ascent direction
    if (is.null(st) || !all(is.finite(st))) return(NULL)
    lam <- 1; ok <- FALSE
    for (b in 1:30) {
      cn <- cc + lam * st; fn <- crit_value(cn, model, crit)
      if (is.finite(fn) && fn >= f0 - 1e-14 * abs(f0)) { ok <- TRUE; break }
      lam <- lam / 2
    }
    if (!ok) break
    step <- max(abs(cn - cc)); cc <- cn; f0 <- fn
    if (max(abs(cc - c_start)) > trust) return(NULL)
    if (step < 1e-12 * max(1, abs(cc)) && max(ev) < 0) break
  }
  g <- crit_grad_c(cc, model, crit)
  Hm <- .hess_c(cc, model, crit)
  if (!all(is.finite(c(g, Hm))) || max(abs(g)) > 1e-8 || max(eigen(Hm, TRUE, TRUE)$values) >= 0) return(NULL)
  cc
}
## warm = TRUE: Newton only, kept within a small trust region around 'start'
## (used when 'start' is the solution at a nearby parameter value).
cutoff_local <- function(model, crit, start, R = search_region(model), warm = FALSE) {
  if (warm) {
    cc <- .newton_c(start, model, crit, trust = 0.02 * (R$b - R$a))
    return(if (is.null(cc)) list(c = start, value = NA_real_, ok = FALSE)
           else list(c = cc, value = crit_value(cc, model, crit), ok = in_region(cc, R)))
  }
  cc <- .newton_c(start, model, crit)
  if (is.null(cc)) {
    f  <- function(z) { v <- crit_value(z, model, crit); if (is.finite(v)) -v else 1e10 }
    gr <- function(z) -crit_grad_c(z, model, crit)
    o  <- tryCatch(stats::optim(start, f, gr, method = "BFGS",
                                control = list(reltol = 1e-15, maxit = 500)),
                   error = function(e) NULL)
    cc <- if (is.null(o)) NULL else .newton_c(o$par, model, crit)
  }
  if (is.null(cc)) return(list(c = start, value = crit_value(start, model, crit), ok = FALSE))
  list(c = cc, value = crit_value(cc, model, crit), ok = in_region(cc, R))
}

## ---- global search: grid + local maximization (Sections 3.2, 6 and 7.2) --------------
cutoff <- function(model, crit, n_grid = 400L, n_start = 4L, q = 1e-4) {
  R  <- search_region(model, q)
  g  <- seq(R$a, R$b, length.out = n_grid)
  F1 <- pclass(g, model[[1]]); F2 <- pclass(g, model[[2]]); F3 <- pclass(g, model[[3]])
  p1 <- matrix(F1, n_grid, n_grid)                      # depends on c1 (rows)
  p2 <- outer(F2, F2, function(a, b) b - a)             # F2(c2) - F2(c1)
  p3 <- matrix(1 - F3, n_grid, n_grid, byrow = TRUE)    # depends on c2 (columns)
  J <- p1 + p2 + p3 - 1; MV <- p1 * p2 * p3; PM <- sqrt((1 - p1)^2 + (1 - p2)^2 + (1 - p3)^2)
  V <- switch(crit, GJ = J, MV = MV, PM = -PM, H = (J + MV) / PM)
  V[outer(g, g, function(a, b) b - a) < R$delta0] <- -Inf
  ord <- order(V, decreasing = TRUE)
  cand <- list()
  for (id in ord) {                                      # distinct starting cells
    if (!is.finite(V[id])) break
    i <- (id - 1L) %% n_grid + 1L; j <- (id - 1L) %/% n_grid + 1L
    if (length(cand) && any(vapply(cand, function(z) max(abs(z - c(i, j))) <= 3, TRUE))) next
    cand[[length(cand) + 1L]] <- c(i, j)
    if (length(cand) >= n_start) break
  }
  best <- NULL
  for (ij in cand) {
    r <- cutoff_local(model, crit, c(g[ij[1]], g[ij[2]]), R)
    if (r$ok && (is.null(best) || r$value > best$value)) best <- r
  }
  if (is.null(best)) {                                   # fall back to the grid optimum
    ij <- cand[[1]]; cc <- c(g[ij[1]], g[ij[2]])
    best <- list(c = cc, value = crit_value(cc, model, crit), ok = FALSE)
  }
  best$region <- R
  best
}

## ---- VUS by one-dimensional Gauss-Legendre quadrature (Section 4.2) ------------------
.glq_cache <- new.env()
## nodes and weights on (-1, 1) by the Golub-Welsch algorithm
glq_nodes <- function(N) {
  key <- as.character(N)
  if (!is.null(.glq_cache[[key]])) return(.glq_cache[[key]])
  k <- seq_len(N - 1L); b <- k / sqrt(4 * k^2 - 1)
  Jm <- matrix(0, N, N); Jm[cbind(k, k + 1L)] <- b; Jm[cbind(k + 1L, k)] <- b
  e <- eigen(Jm, symmetric = TRUE)
  o <- order(e$values)
  out <- list(v = e$values[o], w = 2 * e$vectors[1, o]^2)
  .glq_cache[[key]] <- out
  out
}
## VUS = int_0^1 F1(Q2(u)) [1 - F3(Q2(u))] du,  u = (v + 1) / 2
vus_glq <- function(model, N = 256L) {
  q <- glq_nodes(N); y <- qclass((q$v + 1) / 2, model[[2]])
  0.5 * sum(q$w * pclass(y, model[[1]]) * (1 - pclass(y, model[[3]])))
}

## ---- closed forms (Section S3 of the Supplementary Material) ---------------------------
vus_weibull_cf <- function(alpha, beta) {            # common shape alpha
  r1 <- (beta[1] / beta[2])^alpha; r3 <- (beta[3] / beta[2])^alpha
  r13 <- (beta[1] / beta[3])^alpha
  1 / (r1 + r13 + 1) - 1 / ((r3 + 1) * (r13 * (r3 + 1) + 1))
}
vus_erlang_cf <- function(nu, beta) {                # common integer shape nu
  b23 <- 1 / (1 / beta[2] + 1 / beta[3]); b123 <- 1 / sum(1 / beta)
  s1 <- sum(vapply(0:(nu - 1), function(i)
    b23^(nu + i) * factorial(nu + i - 1) / (beta[3]^i * factorial(i)), 0))
  s2 <- 0
  for (i in 0:(nu - 1)) for (j in 0:(nu - 1))
    s2 <- s2 + b123^(nu + i + j) * factorial(nu + i + j - 1) /
               (beta[1]^j * beta[3]^i * factorial(j) * factorial(i))
  (s1 - s2) / (beta[2]^nu * factorial(nu - 1))
}
vus_exp_cf <- function(beta)
  beta[2] * beta[3]^2 / ((beta[2] + beta[3]) * (beta[1] * beta[2] + beta[1] * beta[3] + beta[2] * beta[3]))

## ---- ROC surface (eq. 6), for plotting ---------------------------------------------
roc_s <- function(p1, p3, model)
  pmax(pclass(qclass(1 - p3, model[[3]]), model[[2]]) - pclass(qclass(p1, model[[1]]), model[[2]]), 0)
