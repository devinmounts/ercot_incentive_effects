#!/usr/bin/env Rscript
# =============================================================================
# download_source_data.R  --  fetch the raw source data for the ERCOT study
# =============================================================================
# The FIRST step of the pipeline and the single lever for the analysis horizon:
# set the date range below, run this once to populate ./source_data, then run
# master.R.  Re-run with a wider range to EXTEND the horizon (e.g. through the
# end of the ORDC program, 2025) -- only the new files are fetched.
#
#   Rscript download_source_data.R                 # uses YEARS below
#   START_YEAR=2013 END_YEAR=2025 Rscript download_source_data.R   # override
#
# Design:
#   * Parameterized by START_YEAR / END_YEAR (env vars override the defaults).
#   * SKIP-EXISTING by default: never clobbers the canonical source_data; a
#     default run with the original range is a no-op, and extending only adds
#     the new years.  Set FORCE=1 to re-download.
#   * Auto-downloadable sources (EIA-860M, EIA-923, Henry Hub) are fully
#     implemented with verified URLs.  Sources that require a portal/API or a
#     custom extract (ERCOT MIS, NOAA weather, BLS, Berkeley Lab) are routed
#     through documented stubs that print exact acquisition instructions and
#     the expected target filenames, so the horizon can be completed manually.
#   * Downstream ingestion should discover files by pattern over this range
#     (see FLEXIBILIZATION NOTES at the bottom) so "less data -> smaller run".
# =============================================================================

## --- configuration ----------------------------------------------------------
START_YEAR <- as.integer(Sys.getenv("START_YEAR", "2013"))   # earliest source year (ERCOT prices/fuelmix begin 2013)
END_YEAR   <- as.integer(Sys.getenv("END_YEAR",   "2022"))   # <-- bump to 2025 to extend to program end
FORCE      <- nzchar(Sys.getenv("FORCE", ""))
SRC        <- "../source_data"                               # run from ./R_files (as the other drivers do)
EIA860M_FIRST <- as.Date("2015-07-01")                       # EIA-860M monthly series begins Jul 2015

months_lc <- c("january","february","march","april","may","june",
               "july","august","september","october","november","december")

## --- robust downloader (system curl; this machine's schannel needs --ssl-no-revoke) ---
dl <- function(url, dest) {
  if (file.exists(dest) && !FORCE) { message("  skip (exists): ", basename(dest)); return(invisible("skip")) }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  st <- suppressWarnings(system2("curl",
        c("-sS","-m","180","--ssl-no-revoke","-L","-o", shQuote(dest), shQuote(url)),
        stdout = TRUE, stderr = TRUE))
  ok <- file.exists(dest) && file.info(dest)$size > 1000
  message(ifelse(ok, "  ok:   ", "  FAIL: "), basename(dest),
          if (ok) sprintf("  (%.1f KB)", file.info(dest)$size/1024) else "")
  invisible(ifelse(ok, "ok", "fail"))
}

## --- 1. EIA-860M (monthly generator inventory) ------------------------------
download_eia860m <- function() {
  message("\n== EIA-860M (monthly) ==")
  for (y in START_YEAR:END_YEAR) for (mi in seq_along(months_lc)) {
    if (as.Date(sprintf("%d-%02d-01", y, mi)) < EIA860M_FIRST) next   # series starts Jul 2015
    f <- sprintf("%s_generator%d.xlsx", months_lc[mi], y)
    dl(sprintf("https://www.eia.gov/electricity/data/eia860m/archive/xls/%s", f),
       file.path(SRC, "EIA860M", f))
  }
}

## --- 2. EIA-923 (annual schedules; download zip, extract the Schedules file) -
download_eia923 <- function() {
  message("\n== EIA-923 (annual) ==")
  for (y in START_YEAR:END_YEAR) {
    if (!FORCE && length(list.files(file.path(SRC, "EIA923A"), pattern = as.character(y))) > 0) {
      message("  skip (year present): EIA-923 ", y); next                # avoid pulling the 21 MB zip
    }
    tmpz <- tempfile(fileext = ".zip")
    if (identical(dl(sprintf("https://www.eia.gov/electricity/data/eia923/archive/xls/f923_%d.zip", y),
                     tmpz), "ok")) {
      fl <- tryCatch(utils::unzip(tmpz, list = TRUE)$Name, error = function(e) character(0))
      sched <- grep("Schedules_2_3_4_5", fl, value = TRUE)[1]   # the generation schedules workbook
      if (!is.na(sched)) {
        target <- file.path(SRC, "EIA923A", basename(sched))
        if (file.exists(target) && !FORCE) { message("  skip (exists): ", basename(target)) }
        else { utils::unzip(tmpz, files = sched, exdir = tempdir(), junkpaths = TRUE)
               file.copy(file.path(tempdir(), basename(sched)), target, overwrite = TRUE)
               message("  extracted: ", basename(target)) }
      } else message("  WARN: no Schedules_2_3_4_5 workbook in f923_", y, ".zip")
      unlink(tmpz)
    }
  }
}

## --- 3. Henry Hub natural-gas daily spot (full series; horizon-automatic) ----
download_henry_hub <- function() {
  message("\n== Henry Hub spot (full daily series) ==")
  dl("https://www.eia.gov/dnav/ng/hist_xls/RNGWHHDd.xls",
     file.path(SRC, "Natural Gas Prices", "RNGWHHDd.xls"))
  message("  NOTE: ingestion currently reads a hand-exported CSV ",
          "(Henry_Hub_Natural_Gas_Spot_Price.csv); add a small reader for RNGWHHDd.xls ",
          "(sheet 'Data 1', skip 2; cols Date, Price) to make NG prices horizon-automatic.")
}

## --- 4. Manual-acquisition sources (portal/API/custom): instructions only ----
manual_sources <- function() {
  message("\n== Manual-acquisition sources (not auto-downloadable; fetch + drop into source_data) ==")
  msg <- function(name, where, target) message(sprintf("  - %-22s %s\n      -> %s", name, where, target))
  msg("ERCOT Fuel Mix",     "ERCOT MIS > Generation > Fuel Mix Report (IntGenByFuel, annual xls/xlsx)",
      "source_data/Generation/FuelMixReport_PreviousYears/IntGenByFuel<YEAR>.xls[x]")
  msg("ERCOT RTM Prices",   "ERCOT MIS > Historical RTM Load Zone & Hub Prices (rpt.00013061)",
      "source_data/Prices/rpt.00013061.0000000000000000.RTMLZHBSPP_<YEAR>.xlsx")
  msg("ERCOT ORDC/Reserves","ERCOT MIS > Historical RT ORDC & Reliability Deployment Price Adders & Reserves",
      "source_data/Reserves/RTM_ORDC_REL_DPLY_PRC_ADDR_RSRV_<YEAR>.xlsx")
  msg("ERCOT Peaker NM",    "ERCOT MIS > CDR Peaker Net Margin (PNMNP4790, daily csv.zip)",
      "source_data/Peaker Net Margin/.../cdr.*.PNMNP4790_*.zip")
  msg("ERCOT Load",         "ERCOT MIS > Backcasted/Actual Load Profiles (annual xlsx)",
      "source_data/Load/ERCOT Backcasted Load Profiles <YEAR>.xlsx")
  msg("NOAA weather",       "Custom NOAA station/zip extract (see energy_zip_weather*.xlsx); re-pull via NOAA API for the 8 study zips",
      "source_data/Weather/energy_zip_weather<DATE>.xlsx  (update the filename + the date filter in ingestion)")
  msg("BLS economic",       "BLS series (CPI, labor force, unemployment, rates) -- data.bls.gov / FRED",
      "source_data/Economic/*.csv,*.xlsx  (incl. bls_cpi_base_2022.12.csv for the deflator)")
  msg("Berkeley Lab LCOE",  "LBNL land-based wind & utility-scale solar reports (site blocks scripts; download in a browser)",
      "source_data/Berkeley Lab/*.xlsm")
  message("  RTC+B note: from 2025-12-05 ERCOT replaced the ORDC adder with ASDC; the post-2025 ",
          "Reserves/price-adder report format changes -- verify columns when extending past 2025.")
}

## --- run ---------------------------------------------------------------------
message("download_source_data.R  |  horizon ", START_YEAR, "-", END_YEAR,
        if (FORCE) "  (FORCE re-download)" else "  (skip-existing)")
download_eia860m()
download_eia923()
download_henry_hub()
manual_sources()
message("\nDone. Re-run with END_YEAR=2025 (env var) to extend; only missing files are fetched.")

# =============================================================================
# FLEXIBILIZATION NOTES (apply to the ingestion so the horizon is data-driven):
#   * ercot_preprocess_functions.R: replace the hard-coded per-year file VECTORS
#     for Generation (IntGenByFuel*), Prices (rpt.00013061*RTMLZHBSPP_*) and
#     Reserves (RTM_ORDC_*) with list.files(pattern=...) filtered to
#     START_YEAR:END_YEAR (Peaker Net Margin already uses list.files()).
#   * Weather: drop/relax `filter(date < '2023-09-01')`; key off max(data date).
#   * Deflator: keep the Dec-2022 CPI base (a fixed base is fine) but ensure
#     bls_cpi covers the full range.
#   * Panel/window: make the 2015-07-01 start and 2022-12 end PARAMETERS; the
#     panel end should default to max(observed month).
#   * Regression covariates: GENERATE the year_/month_ dummy lists from the data
#     present (e.g. year_<min..max>) instead of the hard-coded year_2016..2022,
#     so added years get their own fixed effects automatically.
# =============================================================================
