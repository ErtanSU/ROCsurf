###############################################################################
## 02_fig_sim_cdf.R -- Figure S2 (Supplementary Material): CDFs of the three simulation models (Section 6)
## Output: <out>/figures/Fig_sim_cdf.pdf, Fig_sim_cdf.eps
###############################################################################
source(file.path(if (file.exists("R/load.R")) "." else "..", "R", "load.R"))
source(file.path(REPO, "R", "sim_models.R"))

LTY <- c(1, 2, 4)
lab <- function(m) sprintf("%s(%g, %g)", substr(m$dist, 1, 1), m$par[1], m$par[2])
XR  <- list(GGW = c(0, 3), WGW = c(0, 10), WWW = c(0, 5.5))
draw <- function() {
  op <- par(mfrow = c(1, 3), mar = c(7.0, 4.0, 2.0, 0.6), mgp = c(2.2, 0.7, 0)); on.exit(par(op))
  for (nm in names(SIM_MODELS)) {
    M <- SIM_MODELS[[nm]]; x <- seq(XR[[nm]][1], XR[[nm]][2], length.out = 600)
    plot(NA, xlim = XR[[nm]], ylim = c(0, 1), las = 1, bty = "l", xlab = expression(italic(x)),
         ylab = if (nm == "GGW") expression(italic(F)[italic(k)](italic(x))) else "", main = nm, font.main = 1)
    abline(h = c(0, 1), col = "grey85", lwd = 0.6)
    for (k in 1:3) lines(x, pclass(x, M[[k]]), col = PAL3[k], lty = LTY[k], lwd = 2.4)
    legend(grconvertX(0.5, "nfc", "user"), grconvertY(0.02, "nfc", "user"), xjust = 0.5, yjust = 0,
           legend = vapply(M, lab, ""), col = PAL3, lty = LTY, lwd = 2.4, bty = "n",
           seg.len = 2.2, cex = 0.85, xpd = NA)
  }
}
cat("written:", save_fig("Fig_sim_cdf", draw(), width = 6.2, height = 3.4), sep = "\n")
