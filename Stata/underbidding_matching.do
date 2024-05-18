*********************************************************************************
****************************** Derivative Price Simulation **********************
** Written DM 6.8.21
** Update RC 6.9.21
** Update DM 6.21.21
** Update RC 6.28.21
** Convert RC 3.27.24
** Call:  energy_time test autocorrelation
*********************************************************************************

*********************************************************************************
** READ ME
** This proc cycles through greek parameters of interest 
** Save .ado file to working directory
** Call:
*********************************************************************************
// capture program drop 	energy_time2
// program define 			energy_time2

	******************************************************************
	************************* Matching *******************************
	******************************************************************
	local test_matching = 0
	clear all
	
	local dir =  "/`c(pwd)'"
	di "`dir'"
	cd "../Data/ERCOT Compiled Data"
	use underbidding_data_w_lags, clear
	cd "`dir'"
	
	drop temp_midpoint_sq
	if `test_matching'==1 {
		generate random = runiform()
		sort random
		generate insample = _n <= 5000
		drop if insample == 0
	}
	
	*********************** Match w/o autoregressive covariates *********
	*********************************************************************
	** Match - income - active_low (treatment) vs non_active (control)
	teffects nnmatch (price year-weather_wnds) (active), biasadj(year-weather_wnds) generate(matches) atet 

	** save and output match results
	matrix teresults = r(table)
	matrix teresults = teresults[1..4, 1]
	matrix teresults_xpose = teresults'
	matrix list teresults_xpose
	esttab matrix(teresults_xpose) using "../Tables/Regressions/underbidding/active_match_results.tex", replace ///
	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")

	** save and output match balance summary
	tebalance summarize
	matrix match_size = r(size)
	matrix match_balance = r(table)

	matlist match_size
	esttab matrix(match_size) using "../Tables/Regressions/underbidding/active_match_size.tex", replace


	matlist match_balance
	esttab matrix(match_balance) using "../Tables/Regressions/underbidding/active_match_balance.tex", replace

	drop matches*
