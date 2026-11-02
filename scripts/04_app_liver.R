###############################################################################
## 04_app_liver.R -- Section 7: liver cancer biomarker data
##
## Data : data/liver_mz4271.txt (Wang and Chang, 2011; distributed with the
##        Supporting Information of Shi and Bantis, 2022). Marker m/z 4271.37;
##        classes ordered by the marker: 1 = hepatoma, 2 = chronic liver
##        disease, 3 = normal.
## Model: G2 - G2 - W2; no parameter is held fixed.
##
## Output (<out>/output; <out> is set in R/utils.R):  liver_report.txt,
##   tab_liver_aic.txt, tab_liver_gof.txt, tab_liver_par.txt, tab_liver_cut.txt,
##   tab_liver_ci.txt, tab_liver_pareto.txt        (tab-separated tables)
## Output (<out>/figures): Fig_surface_liver, Fig_glq_liver, Fig_profile_liver (.pdf and .eps)
###############################################################################
source(file.path(if (file.exists("R/load.R")) "." else "..", "R", "load.R"))
need("goftest")
set.seed(20261023)
B_GOF <- 2000L
report_open("liver")

## ---- data ------------------------------------------------------------------------
dat <- read.table(file.path(REPO, "data", "liver_mz4271.txt"), header = TRUE, comment.char = "#")
xs  <- lapply(1:3, function(k) dat$value[dat$class == k])
CL  <- c("Hepatoma", "Chronic liver disease", "Normal")
say(sprintf("n = (%s); means = (%s)", paste(lengths(xs), collapse = ", "),
            paste(f4(vapply(xs, mean, 0)), collapse = ", ")))

## ---- candidate families: AIC (Section 7.1) ------------------------------------------------
## Candidates: two-parameter Weibull (W2) and gamma (G2); for each class the family with the
## smallest AIC is selected.
AIC <- t(vapply(xs, function(x) c(W2 = aic_class(x, "W2"), G2 = aic_class(x, "G2")), numeric(2)))
rownames(AIC) <- CL
D <- colnames(AIC)[apply(AIC, 1, which.min)]
say("\nAIC (smallest value selected)"); say_table(cbind(as.data.frame(round(AIC, 4)), selected = D), digits = 7)
write_table(data.frame(class = CL, AIC_W2 = AIC[, "W2"], AIC_G2 = AIC[, "G2"], selected = D),
            "tab_liver_aic.txt")

## ---- fit and goodness of fit -------------------------------------------------------------
fit <- fit_model(xs, D); M <- fit$model
gof_one <- function(x, dist) {
  m0 <- fit_class(x, dist)
  st <- function(y, m) c(KS = unname(stats::ks.test(y, function(q) pclass(q, m))$statistic),
                         AD = unname(goftest::ad.test(y, function(q) pclass(q, m))$statistic))
  t0 <- st(x, m0)
  tb <- replicate(B_GOF, { y <- rclass(length(x), m0)
    mb <- tryCatch(fit_class(y, dist), error = function(e) NULL)
    if (is.null(mb)) c(NA, NA) else st(y, mb) })
  n_ok <- sum(is.finite(tb[1, ]) & is.finite(tb[2, ]))
  if (n_ok < B_GOF) say(sprintf("  %s: %d of %d bootstrap refits failed", dist, B_GOF - n_ok, B_GOF))
  c(t0, pKS = mean(tb[1, ] >= t0[1], na.rm = TRUE), pAD = mean(tb[2, ] >= t0[2], na.rm = TRUE), nB = n_ok)
}
GOF <- t(vapply(1:3, function(k) gof_one(xs[[k]], D[k]), numeric(5))); rownames(GOF) <- CL
say(sprintf("\nGOODNESS OF FIT (parametric bootstrap, B = %d; p = proportion of bootstrap statistics >= observed;",
            B_GOF))
say("nB = number of bootstrap samples refitted successfully)"); say_table(GOF)

VUS <- vus_glq(M)
say("\ntheta_hat = (", paste(sprintf("%.6f", fit$theta), collapse = ", "), ")")
say(sprintf("log-likelihood = %.6f; converged = %s", fit$loglik, fit$converged))
say(sprintf("VUS (GLQ, N = 256) = %.6f", VUS))

## stochastic ordering of the fitted CDFs
gr <- seq(1e-3, 3 * max(dat$value), length.out = 20000)
d12 <- pclass(gr, M[[1]]) - pclass(gr, M[[2]]); d23 <- pclass(gr, M[[2]]) - pclass(gr, M[[3]])
rng <- function(z) if (any(z)) { r <- range(gr[z]); sprintf("t in [%.2f, %.2f] (union of regions)", r[1], r[2]) } else "none"
say(sprintf("ordering: F1 < F2 at %s, max violation %.2e", rng(d12 < 0), max(0, -d12)))
say(sprintf("          F2 < F3 at t < %.2f and t > %.2f, max violation %.2e",
            max(gr[d23 < 0 & gr < median(dat$value)], -Inf), min(gr[d23 < 0 & gr > median(dat$value)], Inf),
            max(0, -d23)))

## ---- cut-off points ---------------------------------------------------------------------
CUT <- t(vapply(CRITS, function(cr) {
  r <- cutoff(M, cr, n_grid = 400L)
  p <- tpf(r$c, M)
  c(c1 = r$c[1], c2 = r$c[2], p1 = p[1], p2 = p[2], p3 = p[3], crit_all(p), ok = r$ok)
}, numeric(10)))
rk <- cbind(J = rank(-CUT[, "J"]), MV = rank(-CUT[, "MV"]), PM = rank(CUT[, "PM"]), H = rank(-CUT[, "H"])); storage.mode(rk) <- "integer"
R0 <- search_region(M)
say(sprintf("\nsearch region [a, b] = [%.4f, %.4f], delta0 = %.4f", R0$a, R0$b, R0$delta0))
say("CUT-OFF POINTS"); say_table(cbind(CUT, rk))
say(sprintf("smallest c2 - c1 = %.4f (c2 - c1 > delta0 at all solutions: %s)",
            min(CUT[, "c2"] - CUT[, "c1"]), min(CUT[, "c2"] - CUT[, "c1"]) > R0$delta0))

## ---- Wald and LR intervals ----------------------------------------------------------------
pr  <- problem(fit, xs)
kap <- pr$l_hat - stats::qchisq(0.95, 1) / 2
Q   <- c("VUS", "cH1", "cH2")
CI  <- t(vapply(Q, function(q) { w <- wald_ci(pr, q); l <- lr_ci(pr, q)
  c(est = w[["est"]], se = w[["se"]], wl = w[["lower"]], wu = w[["upper"]],
    ll = l[["lower"]], lu = l[["upper"]], conv = l[["converged"]]) }, numeric(7)))
say("\n95% INTERVALS"); say_table(CI)
say("\nCHECK: conventional profile log-likelihood at the LR limits minus kappa")
for (q in Q) for (j in c("ll", "lu")) {
  pa <- profile_at(pr, q, CI[q, j])
  say(sprintf("  %-3s %s: %+.2e", q, j, pa[["loglik"]] - kap))
}

## conventional profile log-likelihood on a grid around the LR limits (unimodality, Section 7.2)
say("\nCHECK: conventional profile log-likelihood on a grid (unimodality)")
for (q in Q) {
  w  <- CI[q, "lu"] - CI[q, "ll"]
  hs <- sort(c(seq(CI[q, "ll"] - 0.15 * w, CI[q, "lu"] + 0.15 * w, length.out = 21), CI[q, "est"]))
  pl <- vapply(hs, function(v) profile_at(pr, q, v)[["loglik"]], 0)
  k  <- which.max(pl)
  say(sprintf("  %-3s unimodal on %d points: %s", q, length(hs),
              all(diff(pl[1:k]) > 0) && all(diff(pl[k:length(pl)]) < 0)))
}

## ---- tables (tab-separated text; the LaTeX tables of the paper are made from these) -------
write_table(data.frame(class = CL, family = D, KS = GOF[, "KS"], p_KS = GOF[, "pKS"],
                       AD = GOF[, "AD"], p_AD = GOF[, "pAD"]), "tab_liver_gof.txt")
write_table(data.frame(alpha1 = fit$theta[1], beta1 = fit$theta[2], alpha2 = fit$theta[3],
                       beta2 = fit$theta[4], alpha3 = fit$theta[5], beta3 = fit$theta[6], VUS = VUS),
            "tab_liver_par.txt")
write_table(data.frame(rule = CRITS, c1 = CUT[CRITS, "c1"], c2 = CUT[CRITS, "c2"]), "tab_liver_cut.txt")
write_table(data.frame(rule = CRITS, p1 = CUT[CRITS, "p1"], p2 = CUT[CRITS, "p2"], p3 = CUT[CRITS, "p3"],
                       J = CUT[CRITS, "J"], MV = CUT[CRITS, "MV"], PM = CUT[CRITS, "PM"], H = CUT[CRITS, "H"],
                       rank_J = rk[CRITS, 1], rank_MV = rk[CRITS, 2], rank_PM = rk[CRITS, 3], rank_H = rk[CRITS, 4]),
            "tab_liver_pareto.txt")
write_table(data.frame(quantity = Q, estimate = CI[Q, "est"], se = CI[Q, "se"], wald_lower = CI[Q, "wl"],
                       wald_upper = CI[Q, "wu"], lr_lower = CI[Q, "ll"], lr_upper = CI[Q, "lu"]),
            "tab_liver_ci.txt")

## ---- figures ----------------------------------------------------------------------------------
## (a) fitted and empirical ROC surfaces
ps <- seq(1e-4, 1 - 1e-4, length.out = 30)
Zf <- outer(ps, ps, function(a, b) roc_s(a, b, M))
Fn <- stats::ecdf(xs[[2]])
Ze <- outer(ps, ps, function(a, b) {
  t1 <- stats::quantile(xs[[1]], a, names = FALSE, type = 6)
  t3 <- stats::quantile(xs[[3]], 1 - b, names = FALSE, type = 6)
  pmax(Fn(t3) - Fn(t1), 0) })
pp <- function(Z, main, bc) persp(ps, ps, Z, theta = 40, phi = 25, expand = 0.62, d = 3, col = "white",
  border = bc, lwd = 0.4, xlab = "p1", ylab = "p3", zlab = "p2", main = main,
  zlim = c(0, 1), ticktype = "detailed", nticks = 3, cex.axis = 0.75)
save_fig("Fig_surface_liver", {
  op <- par(mfrow = c(1, 2), mar = c(1.6, 1.8, 2.2, 0.8)); pp(Zf, "Fitted", DBLUE); pp(Ze, "Empirical", DRED); par(op)
}, width = 6.2, height = 3.4)

## (b) GLQ approximation against the number of nodes
nseq <- seq(2, 64, by = 2); vN <- vapply(nseq, function(N) vus_glq(M, N), 0); vref <- vus_glq(M, 1024L)
say(sprintf("\nGLQ: N = 256 gives %.10f, N = 1024 gives %.10f", VUS, vref))
save_fig("Fig_glq_liver", {
  op <- par(mar = c(4.2, 4.6, 1.0, 1.0), mgp = c(2.8, 0.7, 0))
  plot(nseq, vN, type = "b", pch = 19, cex = 0.7, lwd = 1.8, las = 1, bty = "l", col = DBLUE,
       xlab = expression("Number of nodes " * italic(N)), ylab = "VUS"); abline(h = vref, lty = 2, lwd = 1.6, col = DRED); par(op)
}, width = 4.6, height = 3.2)

## (c) profile log-likelihood functions
PC <- lapply(Q, function(q) profile_curve(pr, q)); names(PC) <- Q
xl <- list(VUS = "VUS", cH1 = expression(italic(c)[italic(H)[1]]), cH2 = expression(italic(c)[italic(H)[2]]))
save_fig("Fig_profile_liver", {
  op <- par(mfrow = c(1, 3), mar = c(4.2, 5.3, 1.0, 0.6), mgp = c(3.0, 0.7, 0))
  for (q in Q) {
    P  <- PC[[q]][PC[[q]]$converged & is.finite(PC[[q]]$h), ]
    sp <- stats::spline(P$h, P$loglik, n = 400, method = "natural")
    yl <- range(c(P$loglik, kap))
    plot(NA, xlim = range(sp$x), ylim = yl, xlab = xl[[q]], ylab = "", las = 1, bty = "l",
         yaxt = "n", xaxt = "n")
    if (q == "VUS") title(ylab = "Profile log-likelihood", line = 3.9)
    at <- pretty(yl); axis(2, at = at, labels = sprintf("%.1f", at), las = 1)
    xa <- CI[q, c("ll", "est", "lu")]
    axis(1, at = xa, labels = sprintf("%.4f", xa), cex.axis = 0.8)
    abline(v = CI[q, "est"], col = "black", lwd = 0.8)
    abline(v = CI[q, c("ll", "lu")], col = DRED, lty = 3, lwd = 2.2)
    abline(h = kap, col = DRED, lty = 2, lwd = 1.8)
    lines(sp$x, sp$y, col = DBLUE, lwd = 2.6)
  }
  par(op)
}, width = 6.15, height = 2.6)
for (q in Q) {
  P <- PC[[q]]; y <- P$loglik[order(P$h)]; k <- which.max(y)
  say(sprintf("\nPROFILE %s: %d of %d points converged; unimodal: %s", q, sum(P$converged), nrow(P),
              all(diff(y[1:k]) > 0) && all(diff(y[k:length(y)]) < 0)))
  say_table(P, digits = 6)
}
report_close()
