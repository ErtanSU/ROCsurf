## The three simulation models of Section 6 (shape alpha, scale beta)
SIM_MODELS <- list(
  GGW = list(list(dist = "G2", par = c(2, 0.2)), list(dist = "G2", par = c(5, 0.2)),
             list(dist = "W2", par = c(4, 1.9))),
  WGW = list(list(dist = "W2", par = c(1, 2)),   list(dist = "G2", par = c(6, 0.8)),
             list(dist = "W2", par = c(8, 8))),
  WWW = list(list(dist = "W2", par = c(7, 2)),   list(dist = "W2", par = c(7, 3)),
             list(dist = "W2", par = c(7, 4))))
sim_dists <- function(M) vapply(M, `[[`, "", "dist")

## true values: VUS by the closed form when the three classes are Weibull with a
## common shape parameter (WWW, Section S3.1 of the Supplementary Material), otherwise by GLQ; c_H by
## grid search and local maximization at the true parameters
true_values <- function(M) {
  sh <- vapply(M, function(m) m$par[1], 0)
  common_w <- all(sim_dists(M) == "W2") && max(abs(sh - sh[1])) == 0
  vus <- if (common_w) vus_weibull_cf(sh[1], vapply(M, function(m) m$par[2], 0)) else vus_glq(M)
  cc <- cutoff(M, "H", n_grid = 400L)$c
  c(VUS = vus, cH1 = cc[1], cH2 = cc[2])
}
