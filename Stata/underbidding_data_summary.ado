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
capture program drop 	underbidding_data_summary
program define 			underbidding_data_summary

	clear all
	pwd
	display "Now running energy_time."
	

	*local dir = "C:\data\ercot_incentive_effects\"
	*di "`dir'"
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
		cd_edata
		*cd "../Data/ERCOT Compiled Data"
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
	*cd "`dir'"
	*cd "../Data/ERCOT Compiled Data"
	cd_edata
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
	year month day_of_week day hour minute repeated_hour_flag

	order price active active_rtorpa rtorpa rtordpa  int_tot_gen_gas_gw int_tot_gen_renewable_gw int_tot_gen_other_gw scarcity_measure ///
	rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent temp_midpoint_sq ng_price weather_wnds ///
	year month day_of_week day hour minute repeated_hour_flag

	
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
			

			order price year month day hour minute repeated_hour_flag
			
			
			** Gen incentive
			gen incentive = total_pa
			drop rtordpa rtorpa active_rtorpa
				sum year if active > 0 & incentive == 0
				sum year if active == 0 & incentive > 0
			replace active = (incentive > 0)
			order active, last
			
			eststo all:  estpost sum price active total_pa int_tot_gen_gas_gw int_tot_gen_renewable_gw int_tot_gen_other_gw scarcity_measure rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent temp_midpoint_sq ng_price weather_wnds year month day_of_week hour minute  
			eststo active_off:  estpost sum price active total_pa int_tot_gen_gas_gw int_tot_gen_renewable_gw int_tot_gen_other_gw scarcity_measure rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent temp_midpoint_sq ng_price weather_wnds year month day_of_week hour minute  if active == 0 
			eststo active_on:  estpost sum price active total_pa  int_tot_gen_gas_gw int_tot_gen_renewable_gw int_tot_gen_other_gw scarcity_measure rttotcap_gw  ng_gw renewables_gw other_gw temp_midpoint_cent temp_midpoint_sq ng_price weather_wnds year month day_of_week hour minute  if active == 1

			*cd "`dir'"
			cd_estats
			esttab all active_off active_on using "covariate_means_by_incentive_state.tex", replace unstack mtitle("All Intervals" "Inactive Incentive" "Active Incentive") cells("mean(fmt(2))") label title(Covariate Means by Scarcity Incentive State)
			
				
			** Print output
			esttab, nonumber se ar2 r2(a5) compress not nostar
			
			eststo clear
			** Run simple
			reg price year##year month##month day_of_week##day_of_week hour##hour minute##minute ///
				int_tot_gen_gas_gw-weather_wnds active incentive 

			
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
			cd_eunderbid
			esttab reg1 reg2 reg3 reg4 reg5 using "underbidding_w_autoregressive_variations.tex", replace r2 aic bic scalar(F)	
			
			esttab reg1 reg2 reg3 reg4 reg5 using "underbidding_w_autoregressive_variations.csv", replace r2 aic bic scalar(F)	
			
				
			** Print output
			esttab, nonumber se ar2 r2(a5) compress not nostar

		} //End if
		di "Done with rho section"
	
	} //End if
	di "Done with greeks."

	********************************************************
	** Save for downstream scripts
	********************************************************
	*cd "`dir'"
	*cd "../Data/ERCOT Compiled Data/"
	cd_edata
	export delimited "underbidding_data_w_lags.csv", replace
	save underbidding_data_w_lags, replace
	
end
