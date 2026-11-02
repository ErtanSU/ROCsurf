###############################################################################
## 06_checks.R -- independent numerical checks of the formulas and the code
##
##  1. VUS: GLQ (N = 256) and the closed form against adaptive quadrature (stats::integrate)
##  2. gradients used in the intervals against numDeriv::grad (c_H: implicit function theorem)
##  2b. c_H unchanged under other quantile levels for the search region
##  2c. c_H(theta) in the intervals = maximizer over the search region; A negative definite (liver)
##  3. Theorem 1: no grid point dominates c_H for (J, MV, -PM) (liver model)
##  4. Remark 2: lower bound H >= 0 on the grid under stochastic ordering (WWW model),
##     and the numerical example given in Remark 2
## Output: <out>/output/checks_report.txt
###############################################################################
source(file.path(if (file.exists("R/load.R")) "." else "..", "R", "load.R"))
source(file.path(REPO, "R", "sim_models.R"))
report_open("checks")
pass <- 0L; fail <- 0L
check <- function(label, value, tol) {
  ok <- is.finite(value) && abs(value) <= tol
  if (ok) pass <<- pass + 1L else fail <<- fail + 1L
  say(sprintf("[%s] %-62s %10.2e (tol %.0e)", if (ok) "PASS" else "FAIL", label, value, tol))
}

dat <- read.table(file.path(REPO, "data", "liver_mz4271.txt"), header = TRUE, comment.char = "#")
xs  <- lapply(1:3, function(k) dat$value[dat$class == k])
set.seed(20261023)
fit <- fit_model(xs, c("G2", "G2", "W2"))
MODELS <- c(SIM_MODELS, list(liver = fit$model))

## 1. VUS by GLQ against adaptive quadrature
for (nm in names(MODELS)) {
  M <- MODELS[[nm]]
  ref <- stats::integrate(function(y) pclass(y, M[[1]]) * (1 - pclass(y, M[[3]])) * dclass(y, M[[2]]),
                          0, Inf, rel.tol = 1e-12, subdivisions = 2000L)$value
  check(sprintf("VUS %s: GLQ - integrate", nm), vus_glq(M) - ref, 1e-6)
  if (nm == "WWW") check("VUS WWW: closed form (S6) - integrate",
                         vus_weibull_cf(7, c(2, 3, 4)) - ref, 1e-9)
}

## 2. gradient of c_H: numDeriv::grad of c_H(theta) against the implicit-function-theorem
##    gradient used in the intervals (pr$hgrad), at theta_hat of the liver data
pr <- problem(fit, xs)
th <- pr$theta; cc <- pr$cut$c
for (j in 1:2) {
  g_num <- ngrad(pr$h[[paste0("cH", j)]], th)
  g_ift <- pr$hgrad[[paste0("cH", j)]](th)
  check(sprintf("grad c_H%d (liver): numDeriv - implicit function", j), max(abs(g_num - g_ift)), 1e-5)
}
check("grad VUS (liver): numDeriv - split computation", max(abs(ngrad(pr$h$VUS, th) - pr$hgrad$VUS(th))), 1e-12)

## 2b. c_H does not depend on the quantile levels defining the search region (Section 6):
##     levels 1%, 0.1%, 0.001%, 0.0001% (and the complementary upper levels) against 0.01%
for (nm in names(MODELS)) {
  c0 <- cutoff(MODELS[[nm]], "H")$c
  d  <- max(vapply(c(1e-2, 1e-3, 1e-5, 1e-6), function(q) {
    r <- cutoff(MODELS[[nm]], "H", q = q)
    if (isTRUE(r$ok)) max(abs(r$c - c0)) else Inf }, 0))
  check(sprintf("c_H %s: other search-region quantile levels - 0.01%%", nm), d, 1e-8)
}

## 2c. c_H(theta) used in the intervals (local maximization started from a nearby solution)
##     equals the maximizer of H over the search region (grid search + local maximization),
##     and A = d(grad_c H)/dc is negative definite, at theta_hat and at the six LR limits (liver)
kappa <- pr$l_hat - stats::qchisq(0.95, 1) / 2; C <- .coords(pr)
thetas <- c(list(pr$theta), unlist(lapply(c("VUS", "cH1", "cH2"), function(q) lapply(c(-1, 1), function(sd)
  C$theta(lr_limit(pr, q, sd, kappa)$z))), recursive = FALSE))
d_glob <- 0; ev_max <- -Inf
for (th_k in thetas) {
  m_k <- make_model(th_k, fit$dists); c_loc <- c(pr$h$cH1(th_k), pr$h$cH2(th_k))
  d_glob <- max(d_glob, abs(c_loc - cutoff(m_k, "H")$c))
  A_k <- numDeriv::jacobian(function(c) crit_grad_c(c, m_k, "H"), c_loc, method.args = GRAD_ARGS)
  ev_max <- max(ev_max, eigen((A_k + t(A_k)) / 2, TRUE, TRUE)$values)
}
check("c_H(theta) (liver, theta_hat and 6 LR limits): local - global maximizer", d_glob, 1e-6)
check("A (liver, theta_hat and 6 LR limits): largest eigenvalue < 0", max(0, ev_max), 0)

## 3. Pareto optimality of c_H on a fine grid (liver)
M <- fit$model; R0 <- search_region(M)
g <- seq(R0$a, R0$b, length.out = 600)
P1 <- matrix(pclass(g, M[[1]]), 600, 600); F2 <- pclass(g, M[[2]])
P2 <- outer(F2, F2, function(a, b) b - a); P3 <- matrix(1 - pclass(g, M[[3]]), 600, 600, byrow = TRUE)
feas <- outer(g, g, function(a, b) b - a) >= R0$delta0
J <- P1 + P2 + P3 - 1; MV <- P1 * P2 * P3; PM <- sqrt((1 - P1)^2 + (1 - P2)^2 + (1 - P3)^2)
v <- crit_all(tpf(cc, M))
dom <- feas & J >= v[["J"]] & MV >= v[["MV"]] & PM <= v[["PM"]] &
       (J > v[["J"]] | MV > v[["MV"]] | PM < v[["PM"]])
check("Theorem 1 (liver): grid points dominating c_H", sum(dom), 0)

## 4. H >= 0 under stochastic ordering (WWW model is stochastically ordered)
M <- SIM_MODELS$WWW; R0 <- search_region(M); g <- seq(R0$a, R0$b, length.out = 400)
P1 <- matrix(pclass(g, M[[1]]), 400, 400); F2 <- pclass(g, M[[2]])
P2 <- outer(F2, F2, function(a, b) b - a); P3 <- matrix(1 - pclass(g, M[[3]]), 400, 400, byrow = TRUE)
H <- (P1 + P2 + P3 - 1 + P1 * P2 * P3) / sqrt((1 - P1)^2 + (1 - P2)^2 + (1 - P3)^2)
check("Remark 2 (WWW): negative part of min H on the grid", min(0, min(H[outer(g, g, "<")])), 1e-12)

## 4b. the example of Remark 2: (p1, p2, p3) = (0.05, 0.10, 0.20) gives J = -0.65, MV = 0.001, H = -0.4231
v <- crit_all(c(0.05, 0.10, 0.20))
say(sprintf("Remark 2 example: J = %.4f, MV = %.4f, H = %.4f", v[["J"]], v[["MV"]], v[["H"]]))
check("Remark 2 example: J + 0.65, MV - 0.001, H (4 decimals) + 0.4231",
      max(abs(c(v[["J"]] + 0.65, v[["MV"]] - 0.001, round(v[["H"]], 4) + 0.4231))), 1e-12)

say(sprintf("\n%d passed, %d failed", pass, fail))
report_close()
