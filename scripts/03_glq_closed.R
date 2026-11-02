###############################################################################
## 03_glq_closed.R -- Section S3.4 (Supplementary Material): GLQ approximation against the closed forms
## Output: <out>/figures/GLQ_vs_Closed.pdf, GLQ_vs_Closed.eps, <out>/output/glq_closed_report.txt
###############################################################################
source(file.path(if (file.exists("R/load.R")) "." else "..", "R", "load.R"))

CASES <- list(
  list(fam = "W", a = 2, b = c(1, 2, 3)), list(fam = "W", a = 7, b = c(2, 3, 4)),
  list(fam = "E", a = 2, b = c(1, 2, 3)), list(fam = "E", a = 5, b = c(1, 1.5, 2)))
mk <- function(z) lapply(z$b, function(bk) list(dist = if (z$fam == "W") "W2" else "G2", par = c(z$a, bk)))
cf <- function(z) if (z$fam == "W") vus_weibull_cf(z$a, z$b) else vus_erlang_cf(z$a, z$b)

report_open("glq_closed")
for (z in CASES) {
  M <- mk(z); NS <- c(2, 4, 8, 16, 32, 64, 128, 256)
  v <- vapply(NS, function(N) vus_glq(M, N), 0)
  say(sprintf("%s: shape = %g, scale = (%s); closed form = %.12f", if (z$fam == "W") "Weibull" else "Erlang",
              z$a, paste(z$b, collapse = ", "), cf(z)))
  say_table(data.frame(N = NS, GLQ = v, abs_error = abs(v - cf(z))), digits = 12)
}
say(sprintf("\nexponential check (Erlang nu = 1 vs eq. S10): %.3e",
            vus_erlang_cf(1, c(1, 2, 3)) - vus_exp_cf(c(1, 2, 3))))
say(sprintf("exponential check (Weibull alpha = 1 vs eq. S10): %.3e",
            vus_weibull_cf(1, c(1, 2, 3)) - vus_exp_cf(c(1, 2, 3))))

draw <- function() {
  op <- par(mfrow = c(2, 2), mar = c(4.0, 5.4, 2.0, 0.8), mgp = c(3.6, 0.7, 0)); on.exit(par(op))
  nn <- seq(2, 40, by = 2); tag <- c("(a)", "(b)", "(c)", "(d)")
  for (m in seq_along(CASES)) {
    z <- CASES[[m]]; v <- vapply(nn, function(N) vus_glq(mk(z), N), 0); tv <- cf(z)
    plot(nn, v, type = "b", pch = 19, cex = 0.6, lwd = 1.6, las = 1, bty = "l", col = DBLUE,
         ylim = range(c(v, tv)), xlab = "", ylab = "VUS")
    title(xlab = expression("Number of nodes " * italic(N)), line = 2.4)
    abline(h = tv, lty = 2, lwd = 1.6, col = DRED)
    ttl <- if (z$fam == "W") bquote(.(tag[m]) ~ "Weibull:" ~ alpha == .(z$a) * "," ~ beta == (.(paste(z$b, collapse = ", "))))
           else bquote(.(tag[m]) ~ "Erlang:" ~ nu == .(z$a) * "," ~ beta == (.(paste(z$b, collapse = ", "))))
    mtext(ttl, side = 3, line = 0.5, adj = 0, cex = 0.85)
  }
}
save_fig("GLQ_vs_Closed", draw(), width = 6.2, height = 5.0)
report_close()
