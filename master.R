#!/usr/bin/env Rscript
# =============================================================================
# master.R  --  top-level pipeline orchestrator for the ERCOT incentive study
# =============================================================================
# Runs the full replication pipeline end-to-end, in dependency order, invoking
# each R driver and each Stata do-file as its own clean process.
#
# USAGE (from the repository root):
#     Rscript master.R              # run the full pipeline (steps 01-06)
#     Rscript master.R 02 04        # run only the named steps (labels below)
#
#   R steps run with working directory ./R_files ; Stata steps run from the
#   repository root (the do-files derive their paths from c(pwd)).
#
# REQUIREMENTS:
#   * R 4.4.2 with the packages loaded by the R_files drivers (pacman handles).
#   * Stata/MP 17 with SSC packages estout + hettreatreg installed.
#   * Stata executable on PATH, or set env var STATA_EXE to its full path.
#
# STEP MAP (label -> engine -> script -> exhibits):
#   01  R     run_ercot_program.R                  Tables 1,3,4; Figs 1,2; stats
#   02  Stata 02_underbidding_data_summary.do      Table 2 + AR variations + lags
#   03  R     run_rls_underbidding_timeseries_model.R   Table 5
#   04  Stata 04_underbidding_matching.do          Table 6                 (~1.8 h)
#   05  Stata 05_underbidding_matching_robustness.do  App. F9 matching CSVs (~40 m)
#   06  R     ercot_appendix_robustness.R          Fig B1; F6-F8; F9 table; E4/E5
#
# NOTE: the 222-model variable-lags appendix figure (G2) is a multi-hour
#   deterministic robustness run and is NOT part of this default sequence; run
#   it separately when needed (see run_variable_lags_test() in regression_script.R).
# =============================================================================

## --- configuration ----------------------------------------------------------
repo  <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
rdir  <- file.path(repo, "R_files")
stata <- Sys.getenv("STATA_EXE", "C:/Program Files/Stata17/StataMP-64.exe")

if (!dir.exists(rdir))   stop("Run master.R from the repository root (missing ./R_files).")
if (!file.exists(stata)) stop("Stata executable not found; set STATA_EXE. Looked at: ", stata)

## --- step runners ------------------------------------------------------------
run_R <- function(script) {
  message("\n==== [01-06 R]    ", script, "   (", format(Sys.time()), ") ====")
  old <- setwd(rdir); on.exit(setwd(old))
  st <- system2("Rscript", c("--vanilla", shQuote(script)))
  if (!identical(st, 0L)) stop("R step failed: ", script, " (exit ", st, ")")
}

run_stata <- function(dofile) {
  message("\n==== [01-06 Stata] ", dofile, "   (", format(Sys.time()), ") ====")
  old <- setwd(repo); on.exit(setwd(old))
  st <- system2(stata, c("/e", "do", paste0("Stata/", dofile)))
  if (!identical(st, 0L)) stop("Stata step failed: ", dofile, " (exit ", st, ")")
}

## --- pipeline definition (dependency order) ---------------------------------
steps <- list(
  `01` = function() run_R("run_ercot_program.R"),
  `02` = function() run_stata("02_underbidding_data_summary.do"),
  `03` = function() run_R("run_rls_underbidding_timeseries_model.R"),
  `04` = function() run_stata("04_underbidding_matching.do"),
  `05` = function() run_stata("05_underbidding_matching_robustness.do"),
  `06` = function() run_R("ercot_appendix_robustness.R")
)

## --- select + execute --------------------------------------------------------
sel <- commandArgs(trailingOnly = TRUE)
if (length(sel) == 0L) sel <- names(steps)
unknown <- setdiff(sel, names(steps))
if (length(unknown)) stop("Unknown step label(s): ", paste(unknown, collapse = ", "))

message("ERCOT incentive pipeline -- start ", format(Sys.time()),
        "  (steps: ", paste(sel, collapse = " "), ")")
for (s in sel) steps[[s]]()
message("\nERCOT incentive pipeline -- complete ", format(Sys.time()))
