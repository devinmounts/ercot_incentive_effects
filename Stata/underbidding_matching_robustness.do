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
****************************

	************************* Matching *******************************
	******************************************************************
	local test_matching = 0
	clear all
	
	local dir =  "/`c(pwd)'"
	di "`dir'"
	cd "../Data/ERCOT Compiled Data"
	use underbidding_data_w_lags, clear
	cd "`dir'"
	
	eststo all:  estpost sum price active total_pa int_tot_gen_gas_gw int_tot_gen_renewable_gw int_tot_gen_other_gw scarcity_measure rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent temp_midpoint_sq ng_price weather_wnds year month day_of_week hour minute  
	eststo active_off:  estpost sum price active total_pa int_tot_gen_gas_gw int_tot_gen_renewable_gw int_tot_gen_other_gw scarcity_measure rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent temp_midpoint_sq ng_price weather_wnds year month day_of_week hour minute  if active == 0 
	eststo active_on:  estpost sum price active total_pa  int_tot_gen_gas_gw int_tot_gen_renewable_gw int_tot_gen_other_gw scarcity_measure rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent temp_midpoint_sq ng_price weather_wnds year month day_of_week hour minute  if active == 1


	esttab all active_off active_on using "../Tables/Regressions/underbidding/robustness/covariate_means_by_incentive_state.csv", replace unstack mtitle("All Intervals" "Inactive Incentive" "Active Incentive") cells("mean(fmt(2))") label title(Covariate Means by Scarcity Incentive State)
			
	
	drop temp_midpoint_sq
	if `test_matching'==1 {
		generate random = runiform()
		sort random
		generate insample = _n <= 5000
		drop if insample == 0
	}



// 	*********************** Match w AR1 covariates *********
// 	*********************************************************************
// 	** Match - income - active_low (treatment) vs non_active (control)
//	
//	
// 	teffects nnmatch (price year-weather_wnds e_1) (active), biasadj(year-weather_wnds e_1) generate(matches) atet 
//	
// 	** save and output match results
// 	matrix teresults = r(table)
// 	matrix teresults = teresults[1..4, 1]
// 	matrix teresults_xpose = teresults'
// 	matrix list teresults_xpose
// 	esttab matrix(teresults_xpose) using "../Tables/Regressions/underbidding/robustness/active_match_results_ar1.tex", replace ///
// 	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")
//	
// 	esttab matrix(teresults_xpose) using "../Tables/Regressions/underbidding/robustness/active_match_results_ar1.csv", replace ///
// 	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")
//
// 	** save and output match balance summary
// 	tebalance summarize
// 	matrix match_size = r(size)
// 	matrix match_balance = r(table)
//
// 	matlist match_size
// 	esttab matrix(match_size) using "../Tables/Regressions/underbidding/robustness/active_match_size_ar1.tex", replace
// 	esttab matrix(match_size) using "../Tables/Regressions/underbidding/robustness/active_match_size_ar1.csv", replace
//
//
// 	matlist match_balance
// 	esttab matrix(match_balance) using "../Tables/Regressions/underbidding/active_match_balance_ar1.tex", replace
//
// 	drop matches*
//	
// 	*********************** Match w AR* covariates *********
// 	*********************************************************************
// 	** Match - income - active_low (treatment) vs non_active (control)
//	
//	
// 	teffects nnmatch (price year-weather_wnds e_* d_*) (active), biasadj(year-weather_wnds e_* d_*) generate(matches) atet 
//	
// 	** save and output match results
// 	matrix teresults = r(table)
// 	matrix teresults = teresults[1..4, 1]
// 	matrix teresults_xpose = teresults'
// 	matrix list teresults_xpose
// 	esttab matrix(teresults_xpose) using "../Tables/Regressions/underbidding/robustness/active_match_results_ar10.tex", replace ///
// 	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")
//	
// 	esttab matrix(teresults_xpose) using "../Tables/Regressions/underbidding/robustness/active_match_results_ar10.csv", replace ///
// 	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")
//
// 	** save and output match balance summary
// 	tebalance summarize
// 	matrix match_size = r(size)
// 	matrix match_balance = r(table)
//
// 	matlist match_size
// 	esttab matrix(match_size) using "../Tables/Regressions/underbidding/robustness/active_match_size_ar10.tex", replace
// 	esttab matrix(match_size) using "../Tables/Regressions/underbidding/robustness/active_match_size_ar10.csv", replace
//
//
// 	matlist match_balance
// 	esttab matrix(match_balance) using "../Tables/Regressions/underbidding/robustness/active_match_balance_ar10.tex", replace
//
// 	drop matches*
//	
// 	*****************************************************************
// 	*********************** No Uri **********************************
// 	*****************************************************************
	cd "../Data/ERCOT Compiled Data"
	use underbidding_data_w_lags, clear
	cd "`dir'"
	
	
	generate is_uri = (year == 2021 & month == 2 & (day >=10 & day <=17))
	drop if is_uri == 1
	
	********** Summary Stats *******************
	eststo all:  estpost sum price active total_pa int_tot_gen_gas_gw int_tot_gen_renewable_gw int_tot_gen_other_gw scarcity_measure rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent temp_midpoint_sq ng_price weather_wnds year month day_of_week hour minute  
	eststo active_off:  estpost sum price active total_pa int_tot_gen_gas_gw int_tot_gen_renewable_gw int_tot_gen_other_gw scarcity_measure rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent temp_midpoint_sq ng_price weather_wnds year month day_of_week hour minute  if active == 0 
	eststo active_on:  estpost sum price active total_pa  int_tot_gen_gas_gw int_tot_gen_renewable_gw int_tot_gen_other_gw scarcity_measure rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent temp_midpoint_sq ng_price weather_wnds year month day_of_week hour minute  if active == 1

	esttab all active_off active_on using "../Tables/Regressions/underbidding/robustness/covariate_means_by_incentive_state_no_uri.csv", replace unstack mtitle("All Intervals" "Inactive Incentive" "Active Incentive") cells("mean(fmt(2))") label title(Covariate Means by Scarcity Incentive State)
	
	******************* Basic Regressions *****************
	eststo reg1: reg price year##year month##month day_of_week##day_of_week hour##hour minute##minute ///
		int_tot_gen_gas_gw-weather_wnds active incentive 
	eststo reg2: reg price year##year month##month day_of_week##day_of_week hour##hour minute##minute ///
		int_tot_gen_gas_gw-weather_wnds active incentive e_1
	eststo reg3: reg price year##year month##month day_of_week##day_of_week hour##hour minute##minute ///
		int_tot_gen_gas_gw-weather_wnds active incentive e_1 d_1
	eststo reg4: reg price year##year month##month day_of_week##day_of_week hour##hour minute##minute ///
		int_tot_gen_gas_gw-weather_wnds active incentive e_1 e_2 e_3 e_4
	eststo reg5: reg price year##year month##month day_of_week##day_of_week hour##hour minute##minute ///
		int_tot_gen_gas_gw-weather_wnds active incentive e_1 e_2 e_3 e_4 e_5 d_1 d_2 d_3 d_4 d_5
	esttab reg1 reg2 reg3 reg4 reg5 using "../Tables/Regressions/underbidding/robustness/underbidding_w_autoregressive_variations_no_uri.csv", replace r2 aic bic scalar(F)	
		
	eststo clear
	
	if `test_matching'==1 {
		generate random = runiform()
		sort random
		generate insample = _n <= 5000
		drop if insample == 0
	}
	eststo clear
	

	*********************** Match w/o autoregressive covariates *********
	*********************************************************************
	*** drop columns
	drop temp_midpoint_sq day
	
	** Match - income - active_low (treatment) vs non_active (control)
	teffects nnmatch (price year-weather_wnds) (active), biasadj(year-weather_wnds) generate(matches) atet 

	** save and output match results
	matrix teresults = r(table)
	matrix teresults = teresults[1..4, 1]
	matrix teresults_xpose = teresults'
	matrix list teresults_xpose
	esttab matrix(teresults_xpose) using "../Tables/Regressions/underbidding/robustness/active_match_results_no_uri.tex", replace ///
	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")
	
	esttab matrix(teresults_xpose) using "../Tables/Regressions/underbidding/robustness/active_match_results_no_uri.csv", replace ///
	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")

	** save and output match balance summary
	tebalance summarize
	matrix match_size = r(size)
	matrix match_balance = r(table)

	matlist match_size
	esttab matrix(match_size) using "../Tables/Regressions/underbidding/robustness/active_match_size_no_uri.tex", replace
	esttab matrix(match_size) using "../Tables/Regressions/underbidding/robustness/active_match_size_no_uri.csv", replace


	matlist match_balance
	esttab matrix(match_balance) using "../Tables/Regressions/underbidding/robustness/active_match_balance_no_uri.tex", replace

	drop matches*
	
	
// 	*********************** Match w AR1 covariates **********************
// 	*********************************************************************
// 	** Match - income - active_low (treatment) vs non_active (control)
//	
//	
// 	teffects nnmatch (price year-weather_wnds e_1) (active), biasadj(year-weather_wnds e_1) generate(matches) atet 
//	
// 	** save and output match results
// 	matrix teresults = r(table)
// 	matrix teresults = teresults[1..4, 1]
// 	matrix teresults_xpose = teresults'
// 	matrix list teresults_xpose
// 	esttab matrix(teresults_xpose) using "../Tables/Regressions/underbidding/robustness/active_match_results_ar1_no_ur.tex", replace ///
// 	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")
//	
// 	esttab matrix(teresults_xpose) using "../Tables/Regressions/underbidding/robustness/active_match_results_ar1_no_ur.csv", replace ///
// 	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")
//
// 	** save and output match balance summary
// 	tebalance summarize
// 	matrix match_size = r(size)
// 	matrix match_balance = r(table)
//
// 	matlist match_size
// 	esttab matrix(match_size) using "../Tables/Regressions/underbidding/robustness/active_match_size_ar1_nor_uri.tex", replace
// 	esttab matrix(match_size) using "../Tables/Regressions/underbidding/robustness/active_match_size_ar1_nor_uri.csv", replace
//
//
// 	matlist match_balance
// 	esttab matrix(match_balance) using "../Tables/Regressions/underbidding/robustness/active_match_balance_ar1_no_uri.tex", replace
//
// 	drop matches*
//	
// 	*********************** Match w AR* covariates **********************
// 	*********************************************************************
// 	** Match - income - active_low (treatment) vs non_active (control)
//	
//	
// 	teffects nnmatch (price year-weather_wnds e_* d_*) (active), biasadj(year-weather_wnds e_* d_*) generate(matches) atet 
//	
// 	** save and output match results
// 	matrix teresults = r(table)
// 	matrix teresults = teresults[1..4, 1]
// 	matrix teresults_xpose = teresults'
// 	matrix list teresults_xpose
// 	esttab matrix(teresults_xpose) using "../Tables/Regressions/underbidding/robustness/active_match_results_ar10_no_uri.tex", replace ///
// 	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")
//	
// 	esttab matrix(teresults_xpose) using "../Tables/Regressions/underbidding/robustness/active_match_results_ar10_no_uri.csv", replace ///
// 	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")
//
// 	** save and output match balance summary
// 	tebalance summarize
// 	matrix match_size = r(size)
// 	matrix match_balance = r(table)
//
// 	matlist match_size
// 	esttab matrix(match_size) using "../Tables/Regressions/underbidding/robustness/active_match_size_ar10_no_uri.tex", replace
// 	esttab matrix(match_size) using "../Tables/Regressions/underbidding/robustness/active_match_size_ar10_no_uri.csv", replace
//
//
// 	matlist match_balance
// 	esttab matrix(match_balance) using "../Tables/Regressions/underbidding/robustness/active_match_balance_ar10_no_uri.tex", replace
//
// 	drop matches*
	
	****************************************************************
	************************** Away From Price Cap *****************
	****************************************************************
	cd "../Data/ERCOT Compiled Data"
	use underbidding_data_w_lags, clear
	cd "`dir'"
	
	drop if price+incentive > 7000
	
	********** Summary Stats *******************
	eststo all:  estpost sum price active total_pa int_tot_gen_gas_gw int_tot_gen_renewable_gw int_tot_gen_other_gw scarcity_measure rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent temp_midpoint_sq ng_price weather_wnds year month day_of_week hour minute  
	eststo active_off:  estpost sum price active total_pa int_tot_gen_gas_gw int_tot_gen_renewable_gw int_tot_gen_other_gw scarcity_measure rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent temp_midpoint_sq ng_price weather_wnds year month day_of_week hour minute  if active == 0 
	eststo active_on:  estpost sum price active total_pa  int_tot_gen_gas_gw int_tot_gen_renewable_gw int_tot_gen_other_gw scarcity_measure rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent temp_midpoint_sq ng_price weather_wnds year month day_of_week hour minute  if active == 1

	esttab all active_off active_on using "../Tables/Regressions/underbidding/robustness/covariate_means_by_incentive_price_cap_gap.csv", replace unstack mtitle("All Intervals" "Inactive Incentive" "Active Incentive") cells("mean(fmt(2))") label title(Covariate Means by Scarcity Incentive State)
	
	******************* Basic Regressions *****************
		eststo reg1: reg price year##year month##month day_of_week##day_of_week hour##hour minute##minute ///
		int_tot_gen_gas_gw-weather_wnds active incentive 
	eststo reg2: reg price year##year month##month day_of_week##day_of_week hour##hour minute##minute ///
		int_tot_gen_gas_gw-weather_wnds active incentive e_1
	eststo reg3: reg price year##year month##month day_of_week##day_of_week hour##hour minute##minute ///
		int_tot_gen_gas_gw-weather_wnds active incentive e_1 d_1
	eststo reg4: reg price year##year month##month day_of_week##day_of_week hour##hour minute##minute ///
		int_tot_gen_gas_gw-weather_wnds active incentive e_1 e_2 e_3 e_4
	eststo reg5: reg price year##year month##month day_of_week##day_of_week hour##hour minute##minute ///
		int_tot_gen_gas_gw-weather_wnds active incentive e_1 e_2 e_3 e_4 e_5 d_1 d_2 d_3 d_4 d_5

	esttab reg1 reg2 reg3 reg4 reg5 using "../Tables/Regressions/underbidding/robustness/underbidding_w_autoregressive_variations_price_cap_gap.csv", replace r2 aic bic scalar(F)	
	
	if `test_matching'==1 {
		generate random = runiform()
		sort random
		generate insample = _n <= 5000
		drop if insample == 0
	}
	
	
	*********************** Match w/o autoregressive covariates *********
	*********************************************************************
	*** drop columns
	drop temp_midpoint_sq day
	
	** Match - income - active_low (treatment) vs non_active (control)
	teffects nnmatch (price year-weather_wnds) (active), biasadj(year-weather_wnds) generate(matches) atet 

	** save and output match results
	matrix teresults = r(table)
	matrix teresults = teresults[1..4, 1]
	matrix teresults_xpose = teresults'
	matrix list teresults_xpose
	esttab matrix(teresults_xpose) using "../Tables/Regressions/underbidding/robustness/active_match_results_price_cap_gap.tex", replace ///
	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")
	
	esttab matrix(teresults_xpose) using "../Tables/Regressions/underbidding/robustness/active_match_results_price_cap_gap.csv", replace ///
	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")

	** save and output match balance summary
	tebalance summarize
	matrix match_size = r(size)
	matrix match_balance = r(table)

	matlist match_size
	esttab matrix(match_size) using "../Tables/Regressions/underbidding/robustness/active_match_size_price_cap_gap.tex", replace
	esttab matrix(match_size) using "../Tables/Regressions/underbidding/robustness/active_match_size_price_cap_gap.csv", replace


	matlist match_balance
	esttab matrix(match_balance) using "../Tables/Regressions/underbidding/robustness/active_match_balance_price_cap_gap.tex", replace

	drop matches*
//	
// 	*********************** Match w AR1 covariates **********************
// 	*********************************************************************
// 	** Match - income - active_low (treatment) vs non_active (control)
//	
//	
// 	teffects nnmatch (price year-weather_wnds e_1) (active), biasadj(year-weather_wnds e_1) generate(matches) atet 
//	
// 	** save and output match results
// 	matrix teresults = r(table)
// 	matrix teresults = teresults[1..4, 1]
// 	matrix teresults_xpose = teresults'
// 	matrix list teresults_xpose
// 	esttab matrix(teresults_xpose) using "../Tables/Regressions/underbidding/robustness/active_match_results_ar1_pcgap.tex", replace ///
// 	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")
//	
// 	esttab matrix(teresults_xpose) using "../Tables/Regressions/underbidding/robustness/active_match_results_ar1_pcgap.csv", replace ///
// 	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")
//
// 	** save and output match balance summary
// 	tebalance summarize
// 	matrix match_size = r(size)
// 	matrix match_balance = r(table)
//
// 	matlist match_size
// 	esttab matrix(match_size) using "../Tables/Regressions/underbidding/robustness/active_match_size_ar1_pcgap.tex", replace
// 	esttab matrix(match_size) using "../Tables/Regressions/underbidding/robustness/active_match_size_ar1_pcgap.csv", replace
//
//
// 	matlist match_balance
// 	esttab matrix(match_balance) using "../Tables/Regressions/underbidding/robustness/active_match_balance_ar1_pcgap.tex", replace
//
// 	drop matches*
	
	*********************** Match w AR* covariates **********************
	*********************************************************************
	** Match - income - active_low (treatment) vs non_active (control)
	
	
	teffects nnmatch (price year-weather_wnds e_* d_*) (active), biasadj(year-weather_wnds e_* d_*) generate(matches) atet 
	
	** save and output match results
	matrix teresults = r(table)
	matrix teresults = teresults[1..4, 1]
	matrix teresults_xpose = teresults'
	matrix list teresults_xpose
	esttab matrix(teresults_xpose) using "../Tables/Regressions/underbidding/robustness/active_match_results_ar10_pcgap.tex", replace ///
	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")
	
	esttab matrix(teresults_xpose) using "../Tables/Regressions/underbidding/robustness/active_match_results_ar10_pcgap.csv", replace ///
	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")

	** save and output match balance summary
	tebalance summarize
	matrix match_size = r(size)
	matrix match_balance = r(table)

	matlist match_size
	esttab matrix(match_size) using "../Tables/Regressions/underbidding/robustness/active_match_size_ar10_pcgap.tex", replace
	esttab matrix(match_size) using "../Tables/Regressions/underbidding/robustness/active_match_size_ar10_pcgap.csv", replace


	matlist match_balance
	esttab matrix(match_balance) using "../Tables/Regressions/underbidding/robustness/active_match_balance_ar10_pcgap.tex", replace

	drop matches*
		

	display "Done with underbidding robustness."

