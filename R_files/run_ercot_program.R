#### ERCOT Energy Incentive
### necessary libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse', 'ggplot2', 'readxl', 'janitor', 'lubridate', 'geojsonio', 'broom', 'sp', 'zoo', 'fastDummies', 'stargazer', 'RStata', 'suncalc',
               'cowplot', 'olsrr', 'sf', 'MESS')

### necessary scripts
source('ercot_preprocess_functions.R')
source('eia_pre_process_functions.R')
source('statistic_functions.R')
source('regression_script.R')
source('figure_and_stat_functions.R')
source('variable_layers_regression_script.R')


create_necessary_file_folders <- function(){
  dir.create(file.path('../Data'))
  dir.create(file.path('../Data/Economic'))
  dir.create(file.path('../Data/EIA Compiled Data'))
  dir.create(file.path('../Data/ERCOT Compiled Data'))
  dir.create(file.path('../Data/Regressions'))
  dir.create(file.path('../Data/Regressions/Capacity Model'))
  dir.create(file.path('../Data/Regressions/Capacity Model/pre_model_data'))
  dir.create(file.path('../Data/Regressions/Capacity Model/robustness'))
  dir.create(file.path('../Data/Regressions/Capacity Model/robustness/variable_layers_regression'))
  dir.create(file.path('../Data/Weather'))
  
  dir.create(file.path('../Tables'))
  dir.create(file.path('../Tables/Regressions'))
  dir.create(file.path('../Tables/Regressions/Capacity Model'))
  dir.create(file.path('../Tables/Regressions/Capacity Model/Summary'))
  dir.create(file.path('../Tables/Regressions/Capacity Model/Latex'))
  dir.create(file.path('../Tables/Regressions/Capacity Model/robustness'))
  dir.create(file.path('../Tables/Regressions/Capacity Model/robustness/variable_layers_regression'))
  dir.create(file.path('../Tables/Regressions/underbidding'))
  dir.create(file.path('../Tables/Regressions/underbidding/robustness'))
  dir.create(file.path('../Tables/Summary Stats'))
  
  dir.create(file.path('../Figures'))
  

}

run_ercot_program <- function(RStataPath, RStataVersion, run_appendix=FALSE){
  print('creating target file folders')
  create_necessary_file_folders()
  
  #######################################################################
  #######################################################################
  ################ Load and Process Source Data #########################
  #######################################################################
  #######################################################################

  ############################################
  print('loading ERCOT data')
  create_generation_capacity_and_adder_data()
  gc()

  #### loads and processes EIA datasets ####
  #########################################
  print('loading EIA data')
  EIA_data <- load_EIA860M_and_EIA932A()

  df_EIA860M <- EIA_data$EIA860M
  df_EIA923A <- EIA_data$EIA923A
  rm(EIA_data)
  gc()

  write_csv(df_EIA860M, '../Data/EIA Compiled Data/EIA860M_compiled_data.csv')
  write_csv(df_EIA923A, '../Data/EIA Compiled Data/EIA923A_compiled_data.csv')

  #### Shape EIA data to represent chronological life of a generator. ####
  ########################################################################
  print('creating EIA plant timeline')
  create_plant_gen_id_phase_timeline_csv()
  gc()

  ################## Create Supporting Data Sets ###################
  ##################################################################
  print('creating controls data')
  load_peaker_net_margin()
  gc()

  ################### Monthly Data ###############################
  ################################################################
lag_months = 12 # default 12 months for main body, robustness data created in appendix section.
create_monthly_ercot_summary(lag_months)
gc()
create_pnm_monthly_summary(lag_months)
gc()
create_ng_hh_monthly_summary(lag_months)
gc()
create_monthly_weather_data(lag_months)
gc()
create_economic_controls_data(lag_months)
gc()
create_monthly_gini_summary(lag_months)
gc()
create_panel_totalquantity_specific_regressions_12mo_lag(lag_months)
gc()

####################### Interval Data #################################
#######################################################################
print('creating interval data')
create_daily_weather_data()
gc()
create_matching_dataset()
gc()

  #######################################################################
  #######################################################################
  ##################### Main Body Tables  ###############################
  #######################################################################
  #######################################################################

  ################################# Data Summaries ##########################
  ########################################################################

  ################### Table 1 #### - Entry and Exit of Operating Capacity
  #################################
  create_operating_pool_stats_latex()


  ###### Table 2 ##### (also process data to create autoregressive covariates and create summary stats table for appendix)
  ###############################################################
print('Generating Table two')
options("RStata.StataPath" = RStataPath)
options("RStata.StataVersion" = RStataVersion)
stata('../Stata/underbidding_data_summary.do')


################################### Results Section ############################
###############################################################################

####### Capacity Models #######################
####### Tables 3 & 4 ###########################
###############################################
######## also creates summary stats for capacity model for appendix #######
covar_versions <- c('full_controls')
# scenarios: rolling_3_month, exclude_winter_storm_uri, run_polynomial_weather
scenarios <- list(c(TRUE,FALSE, TRUE) # primary scenario; poly
              )

for(covar_version_input in covar_versions) {
  print(paste('starting models for covar version: ', covar_version_input, sep=''))

  for (i in seq_along(scenarios)) {

    rolling_3_month = scenarios[[i]][1]
    exclude_winter_storm_uri = scenarios[[i]][2]
    run_polynomial_weather = scenarios[[i]][3]

    run_regressions(covar_version_input, rolling_3_month,exclude_winter_storm_uri, run_polynomial_weather)
  }
}

######### Underbidding Models ###################
########## Table 5 #############################
###############################################
run_polynomial_weather=TRUE
run_rls_timeseries_underbidding_model(run_polynomial_weather)

########## Table 6 ##########################
############################################
options("RStata.StataPath" = RStataPath)
options("RStata.StataVersion" = RStataVersion)
stata('../Stata/underbidding_matching.do')

print('End of Main Body Tables')

#######################################################################
#######################################################################
##################### Main Body Figures ###############################
#######################################################################
#######################################################################
print('Begin Main Body Figures')
### Prints Figures 1 and 2: Comparison of Peak hour to Sunset Hour. Also prints averages of Sunset and Peak hour Times.
create_peak_daily_and_sunset_interval_realtime_cap_figures()
gc()

#######################################################################
#######################################################################
##################### Main Body Select Stats ##########################
#######################################################################
#######################################################################
print_n_generators_in_sample()

#### How many generators that enter the market show up in the applicant pool, and in how many phases of the applicant pool? ####
calculate_stats_on_presencence_in_applicant_pool()

#### How long does it take for a generator to move through the applicant pool and in to operation? ####
time_to_operation()

#### How much capacity that entered into the applicant pool was cancelled and never made it to operation? ####
calculate_cancelled_capacity()

#### If a plant goes into standby (ie. Not in operation by expected to return...), how often does it actually return? ####
create_stats_on_standby_movement()


  #######################################################################
  #######################################################################
  ################ Appendix Figures and Tables ##########################
  #######################################################################
  #######################################################################
if (run_appendix == TRUE){
    ### Summary Statistics for capacity model result from functions in main body section

    ### Profits of Major Firms and significance of scarcity adders
    print('Plotting Profit Margin of Major Firms')
    plot_profit_margin_select_firms()

    ### Full capacity model and applicant pool results tables produced in main body section

    #######################################################################################
    ################### Underbidding Autoregressive Robustness ############################
    ######################################################################################
    run_polynomial_weather=TRUE
    run_rls_ar1_timeseries_underbidding_model(run_polynomial_weather)
    run_rls_ar10_timeseries_underbidding_model(run_polynomial_weather)


    #######################################################################################
    ################### Capacity Model Robustness ############################
    ######################################################################################
    run_variable_lags_test()
    create_variable_lags_appendix_summary_and_figure()
    gc()

    run_variable_layers_regression_test()
    format_and_save_latex_output_of_variable_layers_regression()
    gc()

    #######################################################################################
    ######################## Underbidding Matching Robustness ############################
    ######################################################################################
    options("RStata.StataPath" = RStataPath)
    options("RStata.StataVersion" = RStataVersion)
    stata('../Stata/underbidding_matching_robustness.do')
    format_underbidding_robustness_latex_table()
}
  
  print('Program Complete')
}

