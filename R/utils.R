###############################################################################
## utils.R -- paths, packages, house palette, figure (PDF + EPS) and table (txt) writers, reporting
###############################################################################

## ---- folder layout -------------------------------------------------------------
## <repository>/      this code (R/, scripts/, data/, run_all.R)
## Results are written to <out>/figures and <out>/output, where <out> is PAPR14_OUT if set,
## else the folder that contains the repository if the manuscript papr14_ijb.tex is there,
## else <repository>/results.
find_repo_root <- function() {
  is_root <- function(p) file.exists(file.path(p, "R", "roc.R")) &&
                         file.exists(file.path(p, "R", "utils.R"))
  a  <- commandArgs(trailingOnly = FALSE)
  fa <- sub("^--file=", "", a[grepl("^--file=", a)])
  cand <- c(if (exists(".REPO_HINT", envir = globalenv())) get(".REPO_HINT", envir = globalenv()),
            if (length(fa)) dirname(normalizePath(fa[1], mustWork = FALSE)), getwd())
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) NULL)
    if (is.character(p) && length(p) == 1L && nzchar(p)) cand <- c(cand, dirname(p))
  }
  for (d in unique(cand)) {
    p <- d
    for (k in 1:6) {
      if (is_root(p)) return(normalizePath(p))
      pp <- dirname(p); if (identical(pp, p)) break; p <- pp
    }
  }
  stop("Repository root not found: set the working directory to the folder that contains run_all.R.")
}

REPO    <- find_repo_root()
## Output folder: the environment variable PAPR14_OUT if it is set; otherwise the folder
## that contains the repository, if the manuscript papr14_ijb.tex is there (layout of the
## authors' project, paper/repo); otherwise results/ inside the repository. Figures go to
## <folder>/figures, tables and reports to <folder>/output.
PAPER   <- if (nzchar(Sys.getenv("PAPR14_OUT"))) Sys.getenv("PAPR14_OUT") else
           if (file.exists(file.path(REPO, "..", "papr14_ijb.tex"))) file.path(REPO, "..") else
           file.path(REPO, "results")
dir.create(PAPER, showWarnings = FALSE, recursive = TRUE)
PAPER   <- normalizePath(PAPER, mustWork = FALSE)
FIG_DIR <- file.path(PAPER, "figures")
OUT_DIR <- file.path(PAPER, "output")
for (d in c(FIG_DIR, OUT_DIR)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

need <- function(pkgs) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss))
    stop("Missing R package(s): ", paste(miss, collapse = ", "),
         "\n  install.packages(c(", paste0('"', miss, '"', collapse = ", "), "))")
  invisible(TRUE)
}
need(c("numDeriv", "nloptr"))

## ---- house palette ----------------------------------------------------------------
DBLUE <- "#14387F"   # dark blue
DRED  <- "#8B0000"   # dark red
DGREY <- "#4A4A4A"   # dark grey
PAL3  <- c(DBLUE, DRED, DGREY)

## ---- files in folders whose path contains non-ASCII characters (e.g. "Drive'ım") --------
## On Windows, the pdf() and postscript() devices and gzip connections (saveRDS/readRDS) open
## files through the narrow-character C library, which fails for such paths when the locale
## code page differs from UTF-8. They are therefore opened by their bare (ASCII) file name
## from inside the target folder; setwd() itself handles any path.
in_dir <- function(dir, expr) { owd <- setwd(dir); on.exit(setwd(owd)); expr }
save_rds <- function(obj, f) in_dir(dirname(f), saveRDS(obj, basename(f)))
read_rds <- function(f) in_dir(dirname(f), readRDS(basename(f)))

## ---- figures: PDF and EPS written by R (no LaTeX needed) ---------------------------
## save_fig() draws the same plot twice, into <name>.pdf and <name>.eps in <out>/figures.
## Mathematical labels are plotmath expressions.
save_fig <- function(name, expr, width, height, dir = FIG_DIR) {
  e <- substitute(expr); env <- parent.frame()
  b_pdf <- paste0(name, ".pdf"); b_eps <- paste0(name, ".eps")
  in_dir(dir, {
    grDevices::pdf(b_pdf, width = width, height = height, family = "Times", useDingbats = FALSE)
    eval(e, env); invisible(grDevices::dev.off())
    grDevices::postscript(b_eps, width = width, height = height, family = "Times", horizontal = FALSE,
                          onefile = FALSE, paper = "special")
    eval(e, env); invisible(grDevices::dev.off())
  })
  invisible(file.path(dir, c(b_pdf, b_eps)))
}

## ---- output helpers -----------------------------------------------------------------
f4 <- function(x) sprintf("%.4f", x)
write_lines <- function(lines, file) { writeLines(lines, file.path(OUT_DIR, file)); invisible(file) }
## tables are written as tab-separated text (numbers with four decimals); the LaTeX tables of
## the paper are made from these files, outside R
write_table <- function(df, file) {
  out <- df
  for (j in seq_along(out)) if (is.double(out[[j]])) out[[j]] <- f4(out[[j]])
  utils::write.table(out, file.path(OUT_DIR, file), sep = "\t", quote = FALSE, row.names = FALSE)
  invisible(file)
}

.REPORT <- new.env()
## The report is kept in memory and the whole file is rewritten after each line, so that it is
## complete at any time and no line breaks are lost on synchronized drives (e.g. Google Drive).
.report_write <- function() writeLines(.REPORT$lines, .REPORT$file)
report_open <- function(name) {
  .REPORT$file  <- file.path(OUT_DIR, paste0(name, "_report.txt"))
  .REPORT$t0    <- proc.time()[3]
  .REPORT$lines <- c(name, paste("started:", format(Sys.time())),
                     sprintf("R %s, numDeriv %s, nloptr %s", getRversion(),
                             utils::packageVersion("numDeriv"), utils::packageVersion("nloptr")), "")
  .report_write()
}
say <- function(...) {
  txt <- paste0(...)
  cat(txt, "\n", sep = "")
  if (!is.null(.REPORT$file)) {
    .REPORT$lines <- c(.REPORT$lines, strsplit(txt, "\n", fixed = TRUE)[[1]])
    .report_write()
  }
}
say_table <- function(x, digits = 4) {
  txt <- utils::capture.output(print(x, digits = digits))
  say(paste(txt, collapse = "\n"))
}
report_close <- function() {
  say(sprintf("\nfinished: %s (%.1f s)", format(Sys.time()), proc.time()[3] - .REPORT$t0))
  .REPORT$file <- NULL
}
