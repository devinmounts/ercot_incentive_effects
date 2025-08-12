# ERCOT Scarcity Pricing Analysis

## Dependencies

* R 4.1.1
* Stata SE or greater (for execution from console with RStata)

## To Run

### Main Results:

1. Run ./R\_files/run\_ercot\_program.R

   * Creates necessary relative folders
   * Processes source data
   * Creates main body tables 1,3,4; figures 1 and 2; select stats in main body

2. Adjust absolute file path in ./Stata/cd\_edata.ado to {absolute\_reference}/Data/ERCOT Compiled Data"
3. Adjust absolute file path in ./Stata/cd\_estats.ado to {absolute\_reference}/Tables/Summary Stats"
4. Adjust absolute file path in ./Stata/cd\_underbid.ado to {absolute\_reference}/Tables/Regressions/underbidding"
5. Run ./Stata/underbidding\_data\_summary.do

   * Creates main body table 2

6. Run ./Stata/underbidding\_matching.do  -- Requires 1-4 hours

   * Create main body table 6

7. Run ./R\_files/run\_rls\_underbidding\_timeseries\_model.R

   * Creates main body table 5

### Appendix:

1. Adjsut absolute file path in ./Stata/cd\_underbid\_robustness.ado to {absolute\_reference}/Tables/Regressions/underbidding/robustness"
2. Run ./Stata/underbidding\_matching\_robustness.do
3. Run ./R\_files/ercot\_appendix\_robustness.R
