# ERCOT Scarcity Pricing Analysis

## Dependencies
* R 4.1.1
* Stata SE or greater (for execution from console with RStata)

## To Run
* Main Results:
  1. Run ./R_files/run_ercot_program.R
  2. Adjust absolute file path in ./Stata/cd_edata to {absolute_reference}/Data/ERCOT Compiled Data"
  3. Adjust absolute file path in ./Stata/cd_estats to {absolute_reference}/Tables/Summary Stats" 
  4. Run ./Stata/underbidding_data_summary.do
  5. Run ./Stata/underbidding_matching.do
  6. Run ./R_files/run_rls_timeseries_underbidding_model.R
* Appendix:
  1. Run ./Stata/underbidding_matching_robustness.do
  2. Run ./R_files/ercot_appendix_robustness.R
