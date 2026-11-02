###############################################################################
## 00_software_citations.R -- software versions and official citations
##
## Run once on the computer that produces the final results (the 5000-replication
## simulation):   Rscript scripts/00_software_citations.R
## Output: <out>/output/software_citations.txt
## The file records the R and package versions actually used and the citations
## recommended by the packages themselves, for checking reference.bib.
###############################################################################
pkgs <- c("numDeriv", "nloptr", "Rsolnp", "goftest")
source(file.path(if (file.exists("R/load.R")) "." else "..", "R", "load.R"))
out  <- file.path(OUT_DIR, "software_citations.txt")
sink(out)
cat("Created:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
cat("==== R ====\n"); cat(R.version.string, "\n\n")
print(citation(), bibtex = TRUE)
for (p in pkgs) {
  cat("\n\n==== package", p, "====\n")
  v <- tryCatch(as.character(packageVersion(p)), error = function(e) "NOT INSTALLED")
  cat("version:", v, "\n")
  if (v != "NOT INSTALLED") {
    d <- packageDescription(p)
    cat("Date/Publication:", if (is.null(d$`Date/Publication`)) "-" else d$`Date/Publication`, "\n")
    cat("Authors@R / Author:", if (is.null(d$Author)) "-" else gsub("\n", " ", d$Author), "\n\n")
    print(citation(p), bibtex = TRUE)
  }
}
cat("\n\n==== references given in the nloptr help page of the SLSQP algorithm ====\n")
h <- tryCatch(utils:::.getHelpFile(help("slsqp", package = "nloptr")), error = function(e) NULL)
if (!is.null(h)) tools::Rd2txt(h, options = list(underline_titles = FALSE))
cat("\n\n==== sessionInfo() ====\n")
for (p in pkgs) suppressWarnings(suppressMessages(requireNamespace(p, quietly = TRUE)))
print(sessionInfo())
sink()
cat("written: output/software_citations.txt\n")
