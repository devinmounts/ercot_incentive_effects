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
//capture program drop 	underbidding_matching
//program define 			underbidding_matching

	******************************************************************
	************************* Matching *******************************
	******************************************************************
	clear all
	
	*local dir =  "/`c(pwd)'"
	*di "`dir'"
	*cd "../Data/ERCOT Compiled Data"
	*cd_edata
	*use underbidding_data_w_lags, clear
	*cd "`dir'"
	*eststo clear
	
	*** drop columns
	*drop temp_midpoint_sq day
	
	*** Test
	local test_matching = 0
	if `test_matching'==1 {
		
		generate random = runiform()
		sort random
		generate insample = _n <= 5000
		drop if insample == 0
	}
	di "Done with test."
	
	********************************************************
	** Causal decomposition
	** Table 6
	********************************************************
	scalar cause = 1
	if cause == 1 {
		
		eststo clear
		
		** Reload
		cd_edata
		use underbidding_data_w_lags, clear
		
		** FE indicators loop
		foreach var of varlist year month day_of_week hour minute {
			
			quietly tabulate `var', generate(`var')
			drop `var'
			drop `var'1
			
		} //End loop
		di "Done with indicators loop."
		
		** Sort and drop
		order price year* month* day_of_week* hour* minute* int_tot_gen_gas_gw-weather_wnds active 
		keep price year* month* day_of_week* hour* minute* int_tot_gen_gas_gw-weather_wnds active 
		
		eststo reg0: reg price year* month* day_of_week* hour* minute* ///
		int_tot_gen_gas_gw-weather_wnds active 
				
		** Hetreg OLS - late treatment - higher income
		** Initial look at decomposition
		hettreatreg year2-weather_wnds, o(price) t(active) 
		** Generate confidence intervals
		bootstrap att = e(att) atu = e(atu) ate = e(ate), reps(100) seed(1101): hettreatreg year2-weather_wnds, o(price) t(active) 
	
		** Matching check
		** Match - income - active_low (treatment) vs non_active (control)
		** ATET
		teffects nnmatch (price year2-weather_wnds) (active), biasadj(year2-weather_wnds) generate(matches) atet 
		tebalance summarize
		drop matches*
		** ATE
		teffects nnmatch (price year2-weather_wnds) (active), biasadj(year2-weather_wnds) generate(matches) ate 
		tebalance summarize
		drop matches*
		
	
	} //End if
	di "Done with causal decomposition."
	
	*********************************************************************
	*********************** Match w/o autoregressive covariates *********
	*********************************************************************
	
	eststo clear
	
	** Reload
	cd_edata
	use underbidding_data_w_lags, clear

	
	** Match - income - active_low (treatment) vs non_active (control)
	teffects nnmatch (price year-day_of_week) (active), biasadj(year-day_of_week) generate(matches) atet 

	** save and output match results
	matrix teresults = r(table)
	matrix teresults = teresults[1..4, 1]
	matrix teresults_xpose = teresults'
	matrix list teresults_xpose

	cd_eunderbid
	esttab matrix(teresults_xpose) using "active_match_results.tex", replace ///
	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")
	
	esttab matrix(teresults_xpose) using "active_match_results.csv", replace ///
	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")

	** save and output match balance summary
	tebalance summarize
	matrix match_size = r(size)
	matrix match_balance = r(table)

	matlist match_size
	esttab matrix(match_size) using "active_match_size.tex", replace


	matlist match_balance
	esttab matrix(match_balance) using "active_match_balance.tex", replace

	drop matches*
	
//end
