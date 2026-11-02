###############################################################################
## 07_solver_compare.R -- SLSQP (nloptr) versus Rsolnp for the LR limits (Section 5.3)
##
## For the liver data, each 95% LR limit of VUS, c_H1 and c_H2 is computed twice
## from the same starting value, by
##   (a) lr_limit(): SLSQP of NLopt through nloptr (the method used in the paper),
##   (b) Rsolnp::solnp(): the solver used by Doganaksoy (2021),
## on the same constrained problem  min/max h(theta)  subject to  l(theta) = kappa,
## in the same coordinates. Reported: the limits, |l - kappa| at the solution, the
## elapsed time and the number of evaluations of h.
## Output: <out>/output/solver_report.txt
###############################################################################
source(file.path(if (file.exists("R/load.R")) "." else "..", "R", "load.R"))
need("Rsolnp")
report_open("solver")

dat <- read.table(file.path(REPO, "data", "liver_mz4271.txt"), header = TRUE, comment.char = "#")
xs  <- lapply(1:3, function(k) dat$value[dat$class == k])
set.seed(20261023)
fit <- fit_model(xs, c("G2", "G2", "W2"))
pr  <- problem(fit, xs)
kappa <- pr$l_hat - stats::qchisq(0.95, 1) / 2

## Rsolnp version of lr_limit(): same coordinates, objective, constraint and start
lr_limit_solnp <- function(pr, name, side, kappa) {
  h  <- pr$h[[name]]; C <- .coords(pr)
  hz <- function(z) h(C$theta(z)); lz <- function(z) pr$ll(C$theta(z))
  g_eta <- pr$hgrad[[name]](pr$theta) * pr$theta            # same start as lr_limit()
  r0 <- sqrt(2 * (pr$l_hat - kappa))
  d_eta <- side * r0 * as.numeric(C$Veta %*% g_eta) / sqrt(sum(g_eta * (C$Veta %*% g_eta)))
  z0 <- d_eta / C$s
  fun <- function(z) { v <- hz(z); if (is.finite(v)) -side * v else 1e10 }
  eqf <- function(z) { l <- lz(z); if (is.finite(l)) l else -1e10 }
  o <- tryCatch(Rsolnp::solnp(z0, fun = fun, eqfun = eqf, eqB = kappa,
                              control = list(trace = 0, tol = 1e-8)),
                error = function(e) e)
  if (inherits(o, "error")) {
    say("  Rsolnp error: ", conditionMessage(o)); return(list(limit = NA_real_, gap = NA_real_)) }
  list(limit = hz(o$pars), gap = abs(lz(o$pars) - kappa))
}

## count the evaluations of h
counted <- function(pr, name) {
  n <- 0L; f <- pr$h[[name]]
  pr$h[[name]] <- function(theta) { n <<- n + 1L; f(theta) }
  list(pr = pr, n = function() n)
}

rows <- list()
for (name in c("VUS", "cH1", "cH2")) for (side in c(-1, 1)) {
  a <- counted(problem(fit, xs), name)       # a fresh problem for each solver: no shared warm starts
  ta <- system.time(ra <- lr_limit(a$pr, name, side, kappa))[["elapsed"]]
  b <- counted(problem(fit, xs), name)
  tb <- system.time(rb <- lr_limit_solnp(b$pr, name, side, kappa))[["elapsed"]]
  rows[[length(rows) + 1L]] <- data.frame(
    h = name, limit = if (side < 0) "lower" else "upper",
    SLSQP = ra$limit, Rsolnp = rb$limit, abs_diff = abs(ra$limit - rb$limit),
    gap_SLSQP = abs(ra$loglik - kappa), gap_Rsolnp = rb$gap,
    time_SLSQP = ta, time_Rsolnp = tb, evals_SLSQP = a$n(), evals_Rsolnp = b$n())
}
out <- do.call(rbind, rows)
say(sprintf("Liver data, model G2-G2-W2, 95%% LR limits, kappa = l_hat - %.6f", stats::qchisq(0.95, 1) / 2))
say(sprintf("nloptr %s, Rsolnp %s, %s", packageVersion("nloptr"), packageVersion("Rsolnp"), R.version.string))
if (packageVersion("Rsolnp") != "2.0.1")
  say("NOTE: the paper (Section 5.3) reports this comparison for Rsolnp 2.0.1; other versions of Rsolnp may ",
      "give limits that differ in the fourth decimal")
say_table(out)
say(sprintf("\ntotal elapsed time: SLSQP %.2f s, Rsolnp %.2f s; largest |difference| of limits: %.2e",
            sum(out$time_SLSQP), sum(out$time_Rsolnp), max(out$abs_diff, na.rm = TRUE)))
report_close()
