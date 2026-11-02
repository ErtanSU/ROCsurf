###############################################################################
## 01_fig_schematic.R -- Figure 1: ordinal three-class classification rule
## The densities are for illustration only:
##   Class 1: Gamma(1.5, 0.2), Class 2: Gamma(8, 0.2), Class 3: Weibull(10, 2.8);
##   cut-off points c1 = 0.75, c2 = 2.50.
## Output: <out>/figures/fig_1.pdf, fig_1.eps
###############################################################################
source(file.path(if (file.exists("R/load.R")) "." else "..", "R", "load.R"))

f1 <- function(x) dgamma(x, shape = 1.5, scale = 0.2)
f2 <- function(x) dgamma(x, shape = 8.0, scale = 0.2)
f3 <- function(x) dweibull(x, shape = 10, scale = 2.8)
c1 <- 0.75; c2 <- 2.50; xmax <- 4.2
LTY <- c(1, 2, 4)

draw <- function() {
  x <- seq(0, xmax, length.out = 1500); y <- cbind(f1(x), f2(x), f3(x)); ymax <- max(y) * 1.22
  op <- par(mar = c(6.2, 4.4, 1.2, 1.0), xaxs = "i", yaxs = "i"); on.exit(par(op), add = TRUE)
  plot(NA, xlim = c(0, xmax), ylim = c(0, ymax), bty = "n", las = 1, xlab = "", ylab = "Density")
  rect(0, 0, c1, ymax, col = "#F2F2F2", border = NA); rect(c2, 0, xmax, ymax, col = "#F2F2F2", border = NA)
  for (k in 1:3) lines(x, y[, k], lwd = 2.8, lty = LTY[k], col = PAL3[k])
  top <- 0.86 * ymax
  segments(c(c1, c2), 0, c(c1, c2), top, lty = 2, lwd = 1.6)
  text(c(c1 / 2, (c1 + c2) / 2, (c2 + xmax) / 2), 0.965 * ymax,
       expression(italic(D) == 1, italic(D) == 2, italic(D) == 3))
  ay <- 0.90 * ymax
  arrows(c(0.06, c1 + 0.06, c2 + 0.06), ay, c(c1 - 0.06, c2 - 0.06, xmax - 0.06), ay,
         code = 3, length = 0.05, lwd = 1, col = "gray35")
  par(xpd = NA); text(c(c1, c2), par("usr")[3] - 0.09 * ymax, expression(italic(c)[1], italic(c)[2])); par(xpd = FALSE)
  mtext(expression("Marker value " * italic(m)), side = 1, line = 2.8)
  mtext(expression({italic(m) <= italic(c)[1]} %=>% {italic(D) == 1} * "," ~~~~
                   {{italic(c)[1] < italic(m)} <= italic(c)[2]} %=>% {italic(D) == 2} * "," ~~~~
                   {italic(m) > italic(c)[2]} %=>% {italic(D) == 3}), side = 1, line = 4.7)
  legend("topright", inset = c(0.02, 0.07),
         legend = expression("Class 1, " * italic(f)[1], "Class 2, " * italic(f)[2], "Class 3, " * italic(f)[3]),
         col = PAL3, lty = LTY, lwd = 2.8, bty = "n", seg.len = 2.4)
}
cat("written:", save_fig("fig_1", draw(), width = 6.0, height = 4.2), sep = "\n")
