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
	pwd
	display "Now running energy_time."
	local dir `c(pwd)'
	pwd
	*add .adopath to locate energy_price command
	*adopath++ "\\onid-fs.onid.oregonstate.edu\mountsd\Profile\Desktop\Stata"

	clear all
	** Setting variable to limit matching sample size for test purposes.
	local test_matching = 1
	** Define base folder
	pwd

	** Some basic settings
	local settings = 0
	if `settings'==1 {

		set cformat %9.3f
// 		** Settings
// 		set matsize 11000, permanently
// 		set maxvar 32767, permanently
// 		*set niceness 6
// 		set max_memory 80g, permanently
// 		set segmentsize 96m, permanently //for large memory computers
// 		set min_memory 0
// 		set more off, permanently
// 		set scrollbufsize 300000
// 		*set timeout1 600	//for updates with slow web connection
//		
// 		** Debug
// 		set rmsg on, permanently
		
		********************************
		** SSC INSTALL:  
		********************************
		*ssc install dm0018.pkg
		** Distinct (levelsof)
		*ssc install dm0042_2.pkg
				
	} // End if
	di "Done with default settings."

	** Load
	local inputs = 1
	if `inputs' == 1 {
	
		clear all
		cd "../Data/ERCOT Compiled Data"
		ls

		import delimited ercot_scarcity_pricing_matching_set.csv
		
			** Rename
		rename *, lower

		** Save
		save underbidding_data_for_regression, replace
		use underbidding_data_for_regression, clear
		
		
	} //End if
	di "Done with input read."

	***clean data
		** Load
	clear
	cd `myfolder'
	use underbidding_data_for_regression				

	** Keep
	*drop year*

	** Set seed (optional)
	set seed 1101
		
	** Initial output table
	eststo clear

	** Nulls
	drop if rtordpa=="NA"
	drop if hb_hubavg_energy_only=="NA"
	drop if hb_busavg_energy_only=="NA"
	drop if ng=="NA"
	drop if other=="NA"
	drop if renewables=="NA"
	// drop if delta_pnm=="NA"
	// drop if temp_midpoint=="NA"
	// drop if mean_ng_cap_gini_coef_hund=="NA"
	// drop if mean_wnd_cap_gini_coef_hund=="NA"

	destring total_pa rtorpa rtordpa hb_hubavg_energy_only hb_busavg_energy_only ng other renewables temp_midpoint, replace
	rename hb_busavg_energy_only price
	
	** generate new variables
	gen int_tot_gen_gas_gw = int_tot_gen_gas/1000
	gen int_tot_gen_renewable_gw = int_tot_gen_renewable/1000
	gen int_tot_gen_other_gw = int_tot_gen_other/1000

	gen rttotcap_gw = rttotcap/1000
	gen ng_gw = ng/1000
	gen other_gw = other/1000
	gen renewables_gw = renewables/1000
	** Add quadratic and cubic terms
	summarize temp_midpoint, meanonly
	gen temp_midpoint_cent = temp_midpoint - r(mean)
	gen temp_midpoint_sq = temp_midpoint_cent^2
	gen temp_midpoint_cube = temp_midpoint_cent^3
	gen active = (total_pa > 0)
	gen active_rtorpa = (rtorpa > 0)
	
	
	** Select columns
	keep price active active_rtorpa rtorpa rtordpa total_pa int_tot_gen_gas_gw int_tot_gen_renewable_gw int_tot_gen_other_gw scarcity_measure ///
	rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent temp_midpoint_sq ng_price weather_wnds ///
	year month day hour minute repeated_hour_flag

	order price active active_rtorpa rtorpa rtordpa  int_tot_gen_gas_gw int_tot_gen_renewable_gw int_tot_gen_other_gw scarcity_measure ///
	rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent temp_midpoint_sq ng_price weather_wnds ///
	year month day hour minute repeated_hour_flag

	
	** Run
	scalar greek = 1
	if greek == 1 {
		
		** Gen error
		scalar rhod = 1
		if rhod == 1 {
		    
			
			
			** Run order
// 			order *, alpha 
			order price year month hour minute repeated_hour_flag
			sort year month hour minute repeated_hour_flag
			
			** Temporary gen day
			local day = 0
			if `day'  == 1 {
					
				*gen n								= _n
				*gen rand 							= rnormal()
				*order n rand 
				*sort rand
				sort year month hour minute repeated_hour_flag//rand
				by year month hour minute repeated_hour_flag: gen day = _n
				*drop n rand
				
			} //end if 
			di "Done with the day."

			order price year month day hour minute repeated_hour_flag
			
			** Check dups
			duplicates list year month day hour repeated_hour_flag minute 
			duplicates list year month day hour repeated_hour_flag minute  price temp* ng* weather* ///
				other* renew* rt* int*
			duplicates list
			duplicates tag year month day hour repeated_hour_flag minute , generate(dups)
			
			** Email dups
			local dup = 0
			if `dup' == 1 {
				
				** Save
				cd "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Data/Generation"
				save temp_stage, replace 
				
				keep if dups==1
				save "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Data/Generation/int var dups.dta"
				
				** Reload
				cd "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Data/Generation"
				use temp_stage, clear 
				
			} //end if
			di "Done withe save."
			
			** Clean dups
			drop if dups == 1
			
			
			****************************************************************
// 			******************** Regressions from Thesis Version ***********
// 			****************************************************************
//			
// 			*** Generate summary table for 3 scarcity adder levels (all, low, high)
// 			eststo all:  estpost sum price active_rtorpa rtorpa rtordpa int_tot_gen_gas_gw int_tot_gen_renewable_gw int_tot_gen_other_gw scarcity_measure rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent temp_midpoint_sq ng_price weather_wnds year month hour minute  
// 			eststo active_off:  estpost sum price active_rtorpa rtorpa rtordpa int_tot_gen_gas_gw int_tot_gen_renewable_gw int_tot_gen_other_gw scarcity_measure rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent temp_midpoint_sq ng_price weather_wnds year month hour minute  if active_rtorpa == 0 
// 			eststo active_on:  estpost sum price active_rtorpa rtorpa rtordpa  int_tot_gen_gas_gw int_tot_gen_renewable_gw int_tot_gen_other_gw scarcity_measure rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent temp_midpoint_sq ng_price weather_wnds year month hour minute  if active_rtorpa == 1
//
// 			esttab all active_off active_on using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Summary Stats/scarcity_adder_covariate_means_acitve_rtorpa.tex", replace unstack mtitle("All Intervals" "Inactive Incentive" "Active Incentive") cells("mean(fmt(2))") label title(Covariate Means by Scarcity Incentive State)
//
// 			eststo clear
//
//			
// 			*** main regression thesis ***
// 			reg price active_rtorpa month##month year##year hour##hour minute##minute int_tot_gen_gas_gw ///
// 			int_tot_gen_renewable_gw int_tot_gen_other_gw rttotcap_gw scarcity_measure rtordpa rtorpa ///
// 			ng_gw other_gw renewables_gw  c.temp_midpoint_cent##c.temp_midpoint_cent ng_price weather_wnds
//
// 			** That 1-at-a-time-question from defense
// 			reg price active_rtorpa month##month year##year hour##hour minute##minute int_tot_gen_gas_gw ///
// 			int_tot_gen_renewable_gw int_tot_gen_other_gw rttotcap_gw scarcity_measure rtordpa ///
// 			ng_gw other_gw renewables_gw  c.temp_midpoint_cent##c.temp_midpoint_cent ng_price weather_wnds 
// 			reg price  month##month year##year hour##hour minute##minute int_tot_gen_gas_gw ///
// 			int_tot_gen_renewable_gw int_tot_gen_other_gw rttotcap_gw scarcity_measure rtordpa rtorpa ///
// 			ng_gw other_gw renewables_gw  c.temp_midpoint_cent##c.temp_midpoint_cent ng_price weather_wnds
			
			***************************************************************
			***************** Combined Incentive Regressions **************
			***************************************************************
			
			** Gen incentive
			gen incentive = total_pa
			drop rtordpa rtorpa active_rtorpa
				sum year if active > 0 & incentive == 0
				sum year if active == 0 & incentive > 0
			replace active = (incentive > 0)
			order active, last
			
			eststo all:  estpost sum price active total_pa int_tot_gen_gas_gw int_tot_gen_renewable_gw int_tot_gen_other_gw scarcity_measure rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent temp_midpoint_sq ng_price weather_wnds year month hour minute  
			eststo active_off:  estpost sum price active total_pa int_tot_gen_gas_gw int_tot_gen_renewable_gw int_tot_gen_other_gw scarcity_measure rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent temp_midpoint_sq ng_price weather_wnds year month hour minute  if active == 0 
			eststo active_on:  estpost sum price active total_pa  int_tot_gen_gas_gw int_tot_gen_renewable_gw int_tot_gen_other_gw scarcity_measure rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent temp_midpoint_sq ng_price weather_wnds year month hour minute  if active == 1

			esttab all active_off active_on using "../Tables/Summary Stats/scarcity_adder_covariate_means_acitve_1pa.tex", replace unstack mtitle("All Intervals" "Inactive Incentive" "Active Incentive") cells("mean(fmt(2))") label title(Covariate Means by Scarcity Incentive State)
			
			eststo clear
			** Run simple
			reg price year##year month##month day##day hour##hour ///
				int_tot_gen_gas_gw-weather_wnds active incentive 
				
			** That 1-at-a-time-question from defense
			reg price year##year month##month day##day hour##hour ///
				int_tot_gen_gas_gw-weather_wnds active 
			reg price year##year month##month day##day hour##hour ///
				int_tot_gen_gas_gw-weather_wnds incentive
				
			** Gen e
			local e = 1
			if `e' == 1 {
				
				predict yhat, xb
				gen e 								= price - yhat
				drop yhat
				
				** Save ordered set
				sort year month day hour repeated_hour_flag minute  
				gen period							= _n
				order period e
				** Save temp
				save temp_timeset, replace
				** Save errors
				keep period e 
				save period_e, replace
				** Reload data
				use temp_timeset, clear
				
				** 15 minute lags
				** Join loop e
				drop e
				local lags 					= 5
				foreach num of numlist 1/`lags' {
					
					** Lag
					replace period			= period - 1
					** Join
					joinby period using period_e, unmatched(master)
						tabulate _merge
						drop _merge
					** Rename 
					rename e e_`num'
					
				} //End loop
				di "Done with loop e."
				
				** Clean up
					sum period
				replace period					= period + `lags'
					sum period
					
				** Day lags
				** Join d (day lags)
				local dags 					= 5
				foreach num of numlist 1/`dags' {
					
					** Dag
					replace period			= period - 96
					
					** Join
					joinby period using period_e, unmatched(master)
						tabulate _merge
						drop _merge
					** Rename 
					rename e d_`num'
					
				} //End loop
				di "Done with loop d."
				
				** Clean up
				drop period
										
			} //end if
			di "Done with e d."
			

			** Run with e d
// 			generate is_uri = (year == 2021 & month == 2 & (day >=10 & day <=17))
// 			generate is_under_thresh = (price+incentive = 7000)
			
			reg price year##year month##month day##day hour##hour ///
				int_tot_gen_gas_gw-weather_wnds active incentive 
			reg price year##year month##month day##day hour##hour ///
				int_tot_gen_gas_gw-weather_wnds active incentive e_*
			reg price year##year month##month day##day hour##hour ///
				int_tot_gen_gas_gw-weather_wnds active incentive e_* d_*
			reg price year##year month##month day##day hour##hour ///
				int_tot_gen_gas_gw-weather_wnds active incentive e_1 
			reg price year##year month##month day##day hour##hour ///
				int_tot_gen_gas_gw-weather_wnds active incentive e_1 d_1
			
			
			eststo reg1: reg price year##year month##month day##day hour##hour ///
				int_tot_gen_gas_gw-weather_wnds active incentive 
			eststo reg2: reg price year##year month##month day##day hour##hour ///
				int_tot_gen_gas_gw-weather_wnds active incentive e_1
			eststo reg3: reg price year##year month##month day##day hour##hour ///
				int_tot_gen_gas_gw-weather_wnds active incentive e_1 d_1
			eststo reg4: reg price year##year month##month day##day hour##hour ///
				int_tot_gen_gas_gw-weather_wnds active incentive e_1 e_2 e_3 e_4
			eststo reg5: reg price year##year month##month day##day hour##hour ///
				int_tot_gen_gas_gw-weather_wnds active incentive e_1 e_2 e_3 e_4 d_1 d_2 d_3 d_4
			esttab reg1 reg2 reg3 reg4 reg5 using "Tables/Regressions/underbidding/autoregressive_underbidding.tex", replace r2 aic bic scalar(F)	
			
			eststo clear

		} //End if
		di "Done with rho section"
	
	} //End if
	di "Done with greeks."

	cd `myfolder'
	cd "../Data/ERCOT Compiled Data/"
	export delimited "underbidding_data_w_lags.csv", replace
	save underbidding_data_w_lags, replace
//	
// 	******************************************************************
// 	***************** Summary Stats and Regressions ******************
// 	***************** for outlier event exclusions *******************
// 	******************************************************************
// 	cd`myfolder'
// 	******************************************************************
// 	*********************** Exclude Storm Uri ************************
// 	******************************************************************
// 	cd "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Data/Generation"
// 	use underbidding_data_w_lags, clear
// 	generate is_uri = (year == 2021 & month == 2 & (day >=10 & day <=17))
// 	drop if is_uri == 1
//	
// 	eststo clear
// 	cd`myfolder'
//	
//
// 	eststo all:  estpost sum price active total_pa int_tot_gen_gas_gw int_tot_gen_renewable_gw ///
// 	int_tot_gen_other_gw scarcity_measure rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent ///
// 	temp_midpoint_sq ng_price weather_wnds year month hour minute  
// 	eststo active_off:  estpost sum price active total_pa int_tot_gen_gas_gw int_tot_gen_renewable_gw ///
// 	int_tot_gen_other_gw scarcity_measure rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent ///
// 	temp_midpoint_sq ng_price weather_wnds year month hour minute  if active == 0 
// 	eststo active_on:  estpost sum price active total_pa  int_tot_gen_gas_gw int_tot_gen_renewable_gw ///
// 	int_tot_gen_other_gw scarcity_measure rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent ///
// 	temp_midpoint_sq ng_price weather_wnds year month hour minute  if active == 1
//
// 	esttab all active_off active_on using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Summary Stats/scarcity_adder_covariate_means_no_uri.tex", replace ///
// 	unstack mtitle("All Intervals" "Inactive Incentive" "Active Incentive") cells("mean(fmt(2))") label title(Covariate Means by Scarcity Incentive State)
//	
// 	eststo reg1: reg price year##year month##month day##day hour##hour ///
// 		int_tot_gen_gas_gw-weather_wnds active incentive 
// 	eststo reg2: reg price year##year month##month day##day hour##hour ///
// 		int_tot_gen_gas_gw-weather_wnds active incentive e_1
// 	eststo reg3: reg price year##year month##month day##day hour##hour ///
// 		int_tot_gen_gas_gw-weather_wnds active incentive e_* d_*
// 	esttab reg1 reg2 reg3 using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/autoregressive_no_uri.tex", replace r2 aic bic scalar(F)	
//	
// 	eststo clear
//	
// 	******************************************************************
// 	****************** Exclude Near Price Cap ************************
// 	******************************************************************
//	
// 	cd "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Data/Generation"
// 	use underbidding_data_w_lags, clear
// 	drop if price+incentive > 7000
//	
// 	cd`myfolder'
//	
// 	eststo all:  estpost sum price active total_pa int_tot_gen_gas_gw int_tot_gen_renewable_gw ///
// 	int_tot_gen_other_gw scarcity_measure rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent ///
// 	temp_midpoint_sq ng_price weather_wnds year month hour minute  
// 	eststo active_off:  estpost sum price active total_pa int_tot_gen_gas_gw int_tot_gen_renewable_gw ///
// 	int_tot_gen_other_gw scarcity_measure rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent ///
// 	temp_midpoint_sq ng_price weather_wnds year month hour minute  if active == 0 
// 	eststo active_on:  estpost sum price active total_pa  int_tot_gen_gas_gw int_tot_gen_renewable_gw ///
// 	int_tot_gen_other_gw scarcity_measure rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent ///
// 	temp_midpoint_sq ng_price weather_wnds year month hour minute  if active == 1
//
// 	esttab all active_off active_on using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Summary Stats/scarcity_adder_covariate_means_price_cap_gap.tex", replace ///
// 	unstack mtitle("All Intervals" "Inactive Incentive" "Active Incentive") cells("mean(fmt(2))") label title(Covariate Means by Scarcity Incentive State)
//	
// 	eststo reg1: reg price year##year month##month day##day hour##hour ///
// 		int_tot_gen_gas_gw-weather_wnds active incentive 
// 	eststo reg2: reg price year##year month##month day##day hour##hour ///
// 		int_tot_gen_gas_gw-weather_wnds active incentive e_1
// 	eststo reg3: reg price year##year month##month day##day hour##hour ///
// 		int_tot_gen_gas_gw-weather_wnds active incentive e_* d_*
// 	esttab reg1 reg2 reg3 using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/autoregressive_price_cap_gap.tex", replace r2 aic bic scalar(F)	
// 	eststo clear
//	
// 	******************************************************************
// 	************************* Matching *******************************
// 	******************************************************************
// 	cd "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Data/Generation"
// 	use underbidding_data_w_lags, clear
//	
// 	drop temp_midpoint_sq
// 	if `test_matching'==1 {
// 		generate random = runiform()
// 		sort random
// 		generate insample = _n <= 5000
// 		drop if insample == 0
// 	}
// 	cd`myfolder'
// 	*********************** Match w/o autoregressive covariates *********
// 	*********************************************************************
// 	** Match - income - active_low (treatment) vs non_active (control)
// 	teffects nnmatch (price year-weather_wnds) (active), biasadj(year-weather_wnds) generate(matches) atet 
//
// 	** save and output match results
// 	matrix teresults = r(table)
// 	matrix teresults = teresults[1..4, 1]
// 	matrix teresults_xpose = teresults'
// 	matrix list teresults_xpose
// 	esttab matrix(teresults_xpose) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_results.tex", replace ///
// 	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")
//
// 	** save and output match balance summary
// 	tebalance summarize
// 	matrix match_size = r(size)
// 	matrix match_balance = r(table)
//
// 	matlist match_size
// 	esttab matrix(match_size) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_size.tex", replace
//
//
// 	matlist match_balance
// 	esttab matrix(match_balance) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_balance.tex", replace
//
// 	drop matches*
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
// 	esttab matrix(teresults_xpose) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_results_ar1.tex", replace ///
// 	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")
//
// 	** save and output match balance summary
// 	tebalance summarize
// 	matrix match_size = r(size)
// 	matrix match_balance = r(table)
//
// 	matlist match_size
// 	esttab matrix(match_size) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_size_ar1.tex", replace
//
//
// 	matlist match_balance
// 	esttab matrix(match_balance) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_balance_ar1.tex", replace
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
// 	esttab matrix(teresults_xpose) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_results_ar_all.tex", replace ///
// 	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")
//
// 	** save and output match balance summary
// 	tebalance summarize
// 	matrix match_size = r(size)
// 	matrix match_balance = r(table)
//
// 	matlist match_size
// 	esttab matrix(match_size) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_size_ar_all.tex", replace
//
//
// 	matlist match_balance
// 	esttab matrix(match_balance) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_balance_ar_all.tex", replace
//
// 	drop matches*
//	
// 	*****************************************************************
// 	*********************** No Uri **********************************
// 	*****************************************************************
// 	cd "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Data/Generation"
// 	use underbidding_data_w_lags, clear
// 	generate is_uri = (year == 2021 & month == 2 & (day >=10 & day <=17))
// 	drop if is_uri == 1
//	
// 	cd`myfolder'
// 	if `test_matching'==1 {
// 		generate random = runiform()
// 		sort random
// 		generate insample = _n <= 5000
// 		drop if insample == 0
// 	}
// 	eststo clear
//	
//
// 	*********************** Match w/o autoregressive covariates *********
// 	*********************************************************************
// 	drop temp_midpoint_sq
// 	** Match - income - active_low (treatment) vs non_active (control)
// 	teffects nnmatch (price year-weather_wnds) (active), biasadj(year-weather_wnds) generate(matches) atet 
//
// 	** save and output match results
// 	matrix teresults = r(table)
// 	matrix teresults = teresults[1..4, 1]
// 	matrix teresults_xpose = teresults'
// 	matrix list teresults_xpose
// 	esttab matrix(teresults_xpose) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_results_no_uri.tex", replace ///
// 	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")
//
// 	** save and output match balance summary
// 	tebalance summarize
// 	matrix match_size = r(size)
// 	matrix match_balance = r(table)
//
// 	matlist match_size
// 	esttab matrix(match_size) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_size_no_uri.tex", replace
//
//
// 	matlist match_balance
// 	esttab matrix(match_balance) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_balance_no_uri.tex", replace
//
// 	drop matches*
//	
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
// 	esttab matrix(teresults_xpose) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_results_ar1_no_ur.tex", replace ///
// 	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")
//
// 	** save and output match balance summary
// 	tebalance summarize
// 	matrix match_size = r(size)
// 	matrix match_balance = r(table)
//
// 	matlist match_size
// 	esttab matrix(match_size) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_size_ar1_nor_uri.tex", replace
//
//
// 	matlist match_balance
// 	esttab matrix(match_balance) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_balance_ar1_no_uri.tex", replace
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
// 	esttab matrix(teresults_xpose) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_results_ar_all_no_uri.tex", replace ///
// 	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")
//
// 	** save and output match balance summary
// 	tebalance summarize
// 	matrix match_size = r(size)
// 	matrix match_balance = r(table)
//
// 	matlist match_size
// 	esttab matrix(match_size) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_size_ar_all_no_uri.tex", replace
//
//
// 	matlist match_balance
// 	esttab matrix(match_balance) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_balance_ar_all_no_uri.tex", replace
//
// 	drop matches*
//	
// 	****************************************************************
// 	************************** Away From Price Cap *****************
// 	****************************************************************
// 	** Reload to return prior to Uri drop
// 	cd "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Data/Generation"
// 	use underbidding_data_w_lags, clear
// 	drop if price+incentive > 7000
// 	if `test_matching'==1 {
// 		generate random = runiform()
// 		sort random
// 		generate insample = _n <= 5000
// 		drop if insample == 0
// 	}
// 	cd`myfolder'
//	
//	
// 	*********************** Match w/o autoregressive covariates *********
// 	*********************************************************************
// 	drop temp_midpoint_sq
// 	** Match - income - active_low (treatment) vs non_active (control)
// 	teffects nnmatch (price year-weather_wnds) (active), biasadj(year-weather_wnds) generate(matches) atet 
//
// 	** save and output match results
// 	matrix teresults = r(table)
// 	matrix teresults = teresults[1..4, 1]
// 	matrix teresults_xpose = teresults'
// 	matrix list teresults_xpose
// 	esttab matrix(teresults_xpose) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_results_price_cap_gap.tex", replace ///
// 	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")
//
// 	** save and output match balance summary
// 	tebalance summarize
// 	matrix match_size = r(size)
// 	matrix match_balance = r(table)
//
// 	matlist match_size
// 	esttab matrix(match_size) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_size_price_cap_gap.tex", replace
//
//
// 	matlist match_balance
// 	esttab matrix(match_balance) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_balance_price_cap_gap.tex", replace
//
// 	drop matches*
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
// 	esttab matrix(teresults_xpose) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_results_ar1_pcgap.tex", replace ///
// 	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")
//
// 	** save and output match balance summary
// 	tebalance summarize
// 	matrix match_size = r(size)
// 	matrix match_balance = r(table)
//
// 	matlist match_size
// 	esttab matrix(match_size) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_size_ar1_pcgap.tex", replace
//
//
// 	matlist match_balance
// 	esttab matrix(match_balance) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_balance_ar1_pcgap.tex", replace
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
// 	esttab matrix(teresults_xpose) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_results_ar_all_pcgap.tex", replace ///
// 	cells("b(fmt(%5.3f)) se(fmt(%5.3f)) z(fmt(%5.3f)) pvalue(fmt(%5.3f))")
//
// 	** save and output match balance summary
// 	tebalance summarize
// 	matrix match_size = r(size)
// 	matrix match_balance = r(table)
//
// 	matlist match_size
// 	esttab matrix(match_size) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_size_ar_all_pcgap.tex", replace
//
//
// 	matlist match_balance
// 	esttab matrix(match_balance) using "Desktop/Graduate School/Thesis/ERCOT/EnergyOSU/Refactor/Images/Regressions/matching/active_match_balance_ar_all_pcgap.tex", replace
//
// 	drop matches*
//		
//
// 	display "Done with energy_time2."
//
//
// // end
// //
// // energy_time2
