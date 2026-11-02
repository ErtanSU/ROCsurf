###############################################################################
## 05_simulation.R -- Section 6: Monte Carlo study
##
## For each model (GGW, WGW, WWW) and n in (25, 50, 100, 250), with
## n1 = n2 = n3 = n: MLEs of VUS, c_H1, c_H2; 95% Wald and LR intervals.
##
## Settings (environment variables, all optional):
##   SIM_NREP     replications per cell            (default 5000)
##   SIM_N        sample sizes, comma separated    (default 25,50,100,250)
##   SIM_MODELS   models, comma separated          (default GGW,WGW,WWW)
##   SIM_WORKERS  parallel workers                 (default: cores - 1)
##   SIM_REDO     1: in finished cells, recompute the replications with a
##                non-converged LR limit (same random streams)
##
## Examples (R console, working directory = repository folder):
##   source("scripts/05_simulation.R")                                   # the paper: 5000 replications
##   Sys.setenv(SIM_NREP = 50, SIM_WORKERS = 2); source("scripts/05_simulation.R")   # quick trial
##
## Reproducibility: replication i of a cell always uses the i-th L'Ecuyer
## stream of that cell, so results do not depend on the number of workers, and
## a trial run gives exactly the first replications of the full run. The run
## can be interrupted and restarted; finished replications are read from
## <out>/output/sim/<model>_n<n>.rds (see R/utils.R for <out>).
##
## Output (<out>/output): sim/*.rds, tab_sim_bias_mse.txt (Table 1), tab_sim_cp.txt (Table 2),
##   tab_sim_al.txt (Table 3), tab_sim_counts.txt (excluded samples fit_fail; numbers of estimates nE,
##   of Wald intervals nW and of accepted LR intervals nL), sim_report.txt. A run with other settings
##   than those of the paper writes these tables and the report with the prefix "trial_".
###############################################################################
source(file.path(if (file.exists("R/load.R")) "." else "..", "R", "load.R"))
source(file.path(REPO, "R", "sim_models.R"))

envn <- function(k, d) { v <- Sys.getenv(k); if (nzchar(v)) as.numeric(strsplit(v, ",")[[1]]) else d }
N_REP   <- as.integer(envn("SIM_NREP", 5000))
N_VEC   <- as.integer(envn("SIM_N", c(25, 50, 100, 250)))
MODELS  <- if (nzchar(Sys.getenv("SIM_MODELS"))) strsplit(Sys.getenv("SIM_MODELS"), ",")[[1]] else names(SIM_MODELS)
WORKERS <- as.integer(envn("SIM_WORKERS", max(1L, parallel::detectCores() - 1L, na.rm = TRUE)))
REDO    <- Sys.getenv("SIM_REDO") == "1"     # recompute replications with non-converged LR limits
## tables and report of a run with other settings than those of the paper get the prefix "trial_",
## so that they do not overwrite the results of the paper
FULL    <- N_REP == 5000L && setequal(N_VEC, c(25, 50, 100, 250)) && setequal(MODELS, names(SIM_MODELS))
PFX     <- if (FULL) "" else "trial_"
LEVEL   <- 0.95
SEED    <- 20261024L
SIM_DIR <- file.path(OUT_DIR, "sim"); dir.create(SIM_DIR, FALSE, TRUE)
Q       <- c("VUS", "cH1", "cH2")
COLS    <- c(as.vector(outer(c("est_", "se_", "wl_", "wu_", "ll_", "lu_", "lc_"), Q, paste0)),
             "fit_ok", "sec")

## ---- one replication -------------------------------------------------------------------
one_rep <- function(M, n) {
  t0 <- proc.time()[3]
  out <- setNames(rep(NA_real_, length(COLS)), COLS)
  xs  <- lapply(M, function(m) rclass(n, m))
  fit <- tryCatch(fit_model(xs, sim_dists(M)), error = function(e) NULL)
  pr  <- if (!is.null(fit) && fit$converged) tryCatch(problem(fit, xs), error = function(e) NULL)
  ## fit_ok = 0: the computation of the MLEs did not converge (fit_model()$converged is FALSE, i.e. for some
  ## class the optimizer did not stop normally or the gradient of the log-likelihood at the estimate exceeds
  ## the tolerance), or the observed information is not positive definite, or c_H could not be computed;
  ## such samples are excluded from all the summaries (Section 6)
  out["fit_ok"] <- as.numeric(!is.null(pr) && pr$ok)
  if (out["fit_ok"] == 1) for (q in Q) {
    w <- tryCatch(wald_ci(pr, q, LEVEL), error = function(e) rep(NA_real_, 4))
    l <- tryCatch(lr_ci(pr, q, LEVEL), error = function(e) c(NA, NA, 0))
    out[paste0(c("est_", "se_", "wl_", "wu_"), q)] <- w
    out[paste0(c("ll_", "lu_", "lc_"), q)] <- l
  }
  out["sec"] <- proc.time()[3] - t0
  out
}
streams <- function(cell, nrep) {
  RNGkind("L'Ecuyer-CMRG"); set.seed(SEED + 1000L * cell)
  s <- .Random.seed; out <- vector("list", nrep)
  for (i in seq_len(nrep)) { out[[i]] <- s; s <- parallel::nextRNGStream(s) }
  out
}
run_one <- function(i, seeds, M, n) {
  RNGkind("L'Ecuyer-CMRG"); assign(".Random.seed", seeds[[i]], envir = globalenv())
  tryCatch(one_rep(M, n), error = function(e) setNames(rep(NA_real_, length(COLS)), COLS))
}

## ---- run (resumable) ------------------------------------------------------------------------
cl <- NULL
if (WORKERS > 1L) {
  cl <- parallel::makeCluster(WORKERS)
  parallel::clusterCall(cl, function(r) { setwd(r); source("R/load.R"); source("R/sim_models.R"); TRUE }, REPO)
  parallel::clusterExport(cl, c("one_rep", "COLS", "Q", "LEVEL"))
}
## progress line, rewritten in place after every finished replication
fmt_h <- function(sec) if (!is.finite(sec)) "--" else if (sec < 3600) sprintf("%.0f min", sec / 60) else
  sprintf("%.1f h", sec / 3600)
progress <- function(m, n, k, N, el, done_run, cells_left) {
  w <- 30L; f <- round(w * k / N)
  rate <- if (done_run > 0) el / done_run else NA_real_          # wall-clock seconds per replication
  cat(sprintf("\r%s n=%3d [%s%s] %5d/%d %5.1f%% | %5.2f s/rep | cell: %s left | all cells: about %s left   ",
              m, n, strrep("=", f), strrep(" ", w - f), k, N, 100 * k / N, rate,
              fmt_h((N - k) * rate), fmt_h(((N - k) + cells_left * N_REP) * rate)))
  utils::flush.console()
}

## one cell: replications are sent to the workers one at a time (dynamic load balancing);
## the finished replications are saved every SAVE_EVERY of them, in replication order
SAVE_EVERY <- 50L
t_all <- proc.time()[3]; done_run <- 0L
cells <- expand.grid(n = N_VEC, m = MODELS, stringsAsFactors = FALSE)
for (ic in seq_len(nrow(cells))) {
  m <- cells$m[ic]; n <- cells$n[ic]; cells_left <- nrow(cells) - ic
  cell <- match(m, names(SIM_MODELS)) * 10L + match(n, c(25, 50, 100, 250, n))
  f <- file.path(SIM_DIR, sprintf("%s_n%d.rds", m, n))
  R <- if (file.exists(f)) read_rds(f) else matrix(NA_real_, 0, length(COLS), dimnames = list(NULL, COLS))
  if (nrow(R) >= N_REP) {
    if (REDO) {        # recompute the replications with a non-converged LR limit (same streams)
      lc <- R[seq_len(N_REP), paste0("lc_", Q), drop = FALSE]
      bad <- which(R[seq_len(N_REP), "fit_ok"] %in% 1 & apply(is.na(lc) | lc != 1, 1, any))
      if (length(bad)) {
        seeds <- streams(cell, N_REP)
        res <- if (is.null(cl)) lapply(bad, run_one, seeds = seeds, M = SIM_MODELS[[m]], n = n)
               else parallel::parLapplyLB(cl, bad, run_one, seeds = seeds, M = SIM_MODELS[[m]], n = n)
        R[bad, ] <- do.call(rbind, res); save_rds(R, f)
        lc <- R[bad, paste0("lc_", Q), drop = FALSE]
        message(sprintf("%s n=%3d: %d replications recomputed, %d of them now with all LR limits converged",
                        m, n, length(bad), sum(apply(!is.na(lc) & lc == 1, 1, all))))
      } else message(sprintf("%s n=%3d: no replication to recompute", m, n))
    } else message(sprintf("%s n=%3d: already finished (%d replications)", m, n, nrow(R)))
    next
  }
  seeds <- streams(cell, N_REP)
  todo <- (nrow(R) + 1L):N_REP
  res <- vector("list", N_REP); k0 <- nrow(R); n_saved <- k0
  save_prefix <- function() {          # rows k0+1, ..., last contiguous finished replication
    j <- n_saved; while (j < N_REP && !is.null(res[[j + 1L]])) j <- j + 1L
    if (j > n_saved) { R <<- rbind(R, do.call(rbind, res[(n_saved + 1L):j])); n_saved <<- j; save_rds(R, f) }
  }
  n_fin <- 0L
  if (is.null(cl)) {
    for (i in todo) {
      res[[i]] <- run_one(i, seeds, SIM_MODELS[[m]], n); n_fin <- n_fin + 1L; done_run <- done_run + 1L
      if (n_fin %% SAVE_EVERY == 0L) save_prefix()
      progress(m, n, k0 + n_fin, N_REP, proc.time()[3] - t_all, done_run, cells_left)
    }
  } else {
    nxt <- 1L; busy <- 0L
    submit <- function(w) {
      i <- todo[nxt]; nxt <<- nxt + 1L; busy <<- busy + 1L
      parallel:::sendCall(cl[[w]], run_one, list(i, seeds, SIM_MODELS[[m]], n), tag = i)
    }
    for (w in seq_len(min(length(cl), length(todo)))) submit(w)
    while (busy > 0L) {
      r <- parallel:::recvOneResult(cl); busy <- busy - 1L
      res[[r$tag]] <- if (inherits(r$value, "try-error")) setNames(rep(NA_real_, length(COLS)), COLS) else r$value
      n_fin <- n_fin + 1L; done_run <- done_run + 1L
      if (nxt <= length(todo)) submit(r$node)
      if (n_fin %% SAVE_EVERY == 0L) save_prefix()
      progress(m, n, k0 + n_fin, N_REP, proc.time()[3] - t_all, done_run, cells_left)
    }
  }
  save_prefix(); cat("\n")
  message(sprintf("%s n=%3d: finished, %d replications (%s since start)", m, n, nrow(R),
                  fmt_h(proc.time()[3] - t_all)))
}
if (!is.null(cl)) parallel::stopCluster(cl)

## ---- summaries ---------------------------------------------------------------------------------
TV <- lapply(SIM_MODELS[MODELS], true_values)
S <- do.call(rbind, lapply(MODELS, function(m) do.call(rbind, lapply(N_VEC, function(n) {
  R <- read_rds(file.path(SIM_DIR, sprintf("%s_n%d.rds", m, n)))[seq_len(N_REP), , drop = FALSE]
  row <- data.frame(model = m, n = n, R = nrow(R), fit_fail = sum(R[, "fit_ok"] != 1 | is.na(R[, "fit_ok"])),
                    sec = mean(R[, "sec"], na.rm = TRUE))
  for (q in Q) {
    tv <- TV[[m]][[q]]; e <- R[, paste0("est_", q)]; ok <- is.finite(e)
    wl <- R[, paste0("wl_", q)]; wu <- R[, paste0("wu_", q)]
    ll <- R[, paste0("ll_", q)]; lu <- R[, paste0("lu_", q)]; lc <- R[, paste0("lc_", q)]
    okw <- is.finite(wl) & is.finite(wu); okl <- is.finite(ll) & is.finite(lu) & lc %in% 1
    row[[paste0("bias_", q)]] <- mean(e[ok]) - tv
    row[[paste0("mse_", q)]]  <- mean((e[ok] - tv)^2)
    row[[paste0("cpW_", q)]]  <- mean(wl[okw] <= tv & tv <= wu[okw])
    row[[paste0("cpL_", q)]]  <- mean(ll[okl] <= tv & tv <= lu[okl])
    row[[paste0("alW_", q)]]  <- mean(wu[okw] - wl[okw])
    row[[paste0("alL_", q)]]  <- mean(lu[okl] - ll[okl])
    row[[paste0("nE_", q)]]   <- sum(ok)
    row[[paste0("nW_", q)]]   <- sum(okw)
    row[[paste0("nL_", q)]]   <- sum(okl)
    row[[paste0("cpL0_", q)]] <- sum(okl & ll <= tv & tv <= lu, na.rm = TRUE) / sum(ok)   # failures = non-coverage
  }
  row
}))))

## ---- tables (tab-separated text; the LaTeX tables of the paper are made from these) -------
tabtxt <- function(file, cols) write_table(cbind(S[, c("model", "n", "R")], S[, cols]), paste0(PFX, file))
tabtxt("tab_sim_bias_mse.txt", paste0(rep(c("bias_", "mse_"), 3), rep(Q, each = 2)))
tabtxt("tab_sim_cp.txt",       paste0(rep(c("cpW_", "cpL_"), 3), rep(Q, each = 2)))
tabtxt("tab_sim_al.txt",       paste0(rep(c("alW_", "alL_"), 3), rep(Q, each = 2)))
tabtxt("tab_sim_counts.txt",   c("fit_fail", paste0("nE_", Q), paste0("nW_", Q), paste0("nL_", Q)))

## ---- report ----------------------------------------------------------------------------------------
report_open(paste0(PFX, "sim"))
say(sprintf("replications per cell = %d; n = %s; models = %s; workers = %d", N_REP,
            paste(N_VEC, collapse = ", "), paste(MODELS, collapse = ", "), WORKERS))
say("\nTRUE VALUES"); for (m in MODELS) say(sprintf("  %s: VUS = %.4f, c_H = (%.4f, %.4f)", m,
                                                    TV[[m]][["VUS"]], TV[[m]][["cH1"]], TV[[m]][["cH2"]]))
say("\nSUMMARY"); say_table(S[, c("model", "n", "R", "fit_fail", "sec")], digits = 3)
for (q in Q) {
  say(sprintf("\n%s: bias, MSE, CP (Wald, LR), AL (Wald, LR)", q))
  say_table(S[, c("model", "n", paste0(c("bias_", "mse_", "cpW_", "cpL_", "alW_", "alL_"), q))], digits = 4)
}
say("\nLR INTERVALS: converged / estimated, and LR coverage when non-converged intervals count as non-coverage")
for (k in seq_len(nrow(S))) say(sprintf("  %s n=%3d : %s", S$model[k], S$n[k], paste(vapply(Q, function(q)
  sprintf("%s %d/%d (CP0 %.4f)", q, S[k, paste0("nL_", q)], S[k, paste0("nE_", q)], S[k, paste0("cpL0_", q)]), ""),
  collapse = "; ")))
tot_e <- sum(unlist(S[, paste0("nE_", Q)])); tot_l <- sum(unlist(S[, paste0("nL_", Q)]))
say(sprintf("\nTOTAL: %d LR intervals attempted, %d converged, %d failed to converge; failed model fits: %d",
            tot_e, tot_l, tot_e - tot_l, sum(S$fit_fail)))
report_close()
