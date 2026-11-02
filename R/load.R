## Sources the whole code base. Every script starts with source("R/load.R").
## String collation is set to "C": sorting under a national locale (e.g. Turkish on Windows)
## is much slower and is not needed anywhere in the computations.
invisible(Sys.setlocale("LC_COLLATE", "C"))
local({
  ## folder of this file: working directory <repository> or <repository>/scripts,
  ## the file being sourced (source(".../R/load.R")), or the script run by Rscript
  ofile <- NULL
  for (i in rev(seq_len(sys.nframe()))) {
    f <- tryCatch(get("ofile", envir = sys.frame(i), inherits = FALSE), error = function(e) NULL)
    if (is.character(f) && length(f) == 1L && grepl("load\\.R$", f)) { ofile <- f; break }
  }
  a  <- commandArgs(trailingOnly = FALSE); fa <- sub("^--file=", "", a[grepl("^--file=", a)])
  cand <- c("R", file.path("..", "R"),
            if (!is.null(ofile)) dirname(ofile),
            if (length(fa) && !is.na(fa[1])) file.path(dirname(dirname(fa[1])), "R"))
  here <- cand[file.exists(file.path(cand, "utils.R"))][1]
  if (is.na(here))
    stop("R code not found: set the working directory to the folder that contains run_all.R, ",
         "e.g. setwd(\"path/to/repo\")")
  assign(".REPO_HINT", normalizePath(file.path(here, "..")), envir = globalenv())
  for (f in c("utils.R", "distributions.R", "roc.R", "inference.R"))
    sys.source(file.path(here, f), envir = globalenv())
})
