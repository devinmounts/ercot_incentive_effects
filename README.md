# ERCOT Scarcity Pricing Analysis

## Dependencies
* R 4.1.1
* Stata SE or greater (for execution from console with RStata)

## To Run
### Main Results:
  1. Run ./R_files/run_ercot_program.R
      * Creates necessary relative folders
      * Processes source data
      * Creates main body tables 1,3,4; figures 1 and 2; select stats in main body 
  2. Adjust absolute file path in ./Stata/cd_edata to {absolute_reference}/Data/ERCOT Compiled Data"
  3. Adjust absolute file path in ./Stata/cd_estats to {absolute_reference}/Tables/Summary Stats" 
  4. Run ./Stata/underbidding_data_summary.do
       * Creates main body table 2
  6. Run ./Stata/underbidding_matching.do
       * Create main body table 6
  7. Run ./R_files/run_rls_timeseries_underbidding_model.R
      * Creates main body table 5
### Appendix:
  1. Run ./Stata/underbidding_matching_robustness.do
  2. Run ./R_files/ercot_appendix_robustness.R
