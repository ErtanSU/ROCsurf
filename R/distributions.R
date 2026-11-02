###############################################################################
## distributions.R -- the two-parameter families of the paper (Section S4 of the Supplementary Material)
##
##   G2(alpha, beta): gamma, shape alpha, scale beta   (dgamma/pgamma)
##   W2(alpha, beta): Weibull, shape alpha, scale beta (dweibull/pweibull)
##
## A class model is list(dist = "G2" | "W2", par = c(alpha, beta)); a three-class
## model is a list of three such class models.
###############################################################################

dclass <- function(x, m, log = FALSE)
  switch(m$dist, G2 = dgamma(x, m$par[1], scale = m$par[2], log = log),
                 W2 = dweibull(x, m$par[1], m$par[2], log = log))
pclass <- function(q, m)
  switch(m$dist, G2 = pgamma(q, m$par[1], scale = m$par[2]),
                 W2 = pweibull(q, m$par[1], m$par[2]))
qclass <- function(p, m)
  switch(m$dist, G2 = qgamma(p, m$par[1], scale = m$par[2]),
                 W2 = qweibull(p, m$par[1], m$par[2]))
rclass <- function(n, m)
  switch(m$dist, G2 = rgamma(n, m$par[1], scale = m$par[2]),
                 W2 = rweibull(n, m$par[1], m$par[2]))

## three-class model <-> parameter vector theta = (theta_1, theta_2, theta_3)
make_model <- function(theta, dists)
  lapply(1:3, function(k) list(dist = dists[k], par = theta[2 * k - c(1, 0)]))
model_theta <- function(model) unlist(lapply(model, `[[`, "par"))

## ---- log-likelihood ---------------------------------------------------------------
loglik_class <- function(par, x, dist) {
  if (any(!is.finite(par)) || any(par <= 0)) return(-Inf)
  v <- suppressWarnings(sum(dclass(x, list(dist = dist, par = par), log = TRUE)))
  if (is.finite(v)) v else -Inf
}
## eq. (22): sum of the three class terms
loglik <- function(theta, xs, dists)
  sum(vapply(1:3, function(k) loglik_class(theta[2 * k - c(1, 0)], xs[[k]], dists[k]), 0))

## ---- MLE of one class (optim on the log scale, several starting values) ----------
start_values <- function(x, dist) {
  m <- mean(x); v <- stats::var(x)
  a <- switch(dist, G2 = m^2 / v,
                    W2 = { cv <- sqrt(v) / m; max(1.2 / cv, 0.2) })
  b <- switch(dist, G2 = v / m, W2 = m / gamma(1 + 1 / a))
  c(a, b)
}
fit_class <- function(x, dist, n_start = 6L) {
  nll <- function(lp) { v <- -loglik_class(exp(lp), x, dist); if (is.finite(v)) v else 1e100 }
  s0  <- log(start_values(x, dist))
  best <- NULL
  for (s in seq_len(n_start)) {
    st <- if (s == 1L) s0 else s0 + c(0.5, 0.25) * stats::rnorm(2)
    o <- tryCatch(stats::optim(st, nll, method = "BFGS",
                               control = list(reltol = 1e-14, maxit = 2000)),
                  error = function(e) NULL)
    if (!is.null(o) && (is.null(best) || o$value < best$value)) best <- o
  }
  if (is.null(best)) stop("fit_class: no successful optimisation")
  ## Newton polishing of the likelihood equations (log scale)
  lp <- best$par; f <- function(z) -nll(z)
  for (it in 1:20) {
    g <- numDeriv::grad(f, lp); Hm <- numDeriv::hessian(f, lp)
    st <- tryCatch(solve(Hm, g), error = function(e) NULL)
    if (is.null(st) || !all(is.finite(st)) || any(eigen(Hm, TRUE, TRUE)$values >= 0)) break
    lp_new <- lp - st
    if (f(lp_new) < f(lp) - 1e-10) break
    lp <- lp_new
    if (max(abs(st)) < 1e-12) break
  }
  list(dist = dist, par = exp(lp), loglik = f(lp),
       converged = best$convergence == 0 && max(abs(numDeriv::grad(f, lp))) < 1e-5)
}

## ---- MLE of the three-class model ------------------------------------------------
## The likelihood factorizes over the classes, so each class is fitted separately.
fit_model <- function(xs, dists) {
  fits  <- lapply(1:3, function(k) fit_class(xs[[k]], dists[k]))
  theta <- unlist(lapply(fits, `[[`, "par"))
  list(theta = theta, dists = dists, model = make_model(theta, dists),
       loglik = sum(vapply(fits, `[[`, 0, "loglik")),
       converged = all(vapply(fits, `[[`, TRUE, "converged")))
}

## ---- AIC of the candidate families (Section 7.1) -----------------------------------
aic_class <- function(x, dist) {
  4 - 2 * fit_class(x, dist)$loglik
}
