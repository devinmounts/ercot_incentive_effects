# ERCOT Scarcity Pricing Analysis

## Dependencies
* R 4.1.1
* Stata SE or greater (for execution from console with RStata)

## To Run
1. Clone repository
2. Set stata relative options in ercot_incentive_analysis.Rmd and relative file path in energy_time_2to1_full.do
3. Set boolean for appendix run (default to false). Appendix results take ~18hrs to complete, depending on RAM.


## New Run Plan
* Main Results:
  1. Run ./R_files/run_ercot_program.R
  2. Adjust absolute file path in ./Stata/cd_edata to {absolute_reference}/Data/ERCOT Compiled Data"
  3. Adjust absolute file path in ./Stata/cd_estats to {absolute_reference}/Tables/Summary Stats" 
  4. Run ./Stata/underbidding_data_summary.do
  5. Run ./Stata/underbidding_matching.do
* Appendix:
  1. Run ./R_files/appendix_robustness.R
  2. Run ./Stata/underbidding_matching_robustness.do
  3. Run ./R_files/regression_script.R format_underbidding_robustness_latex_table()
