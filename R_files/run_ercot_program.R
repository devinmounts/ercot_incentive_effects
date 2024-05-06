#### ERCOT Energy Incentive
### necessary libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse', 'ggplot2', 'readxl', 'janitor', 'lubridate', 'geojsonio', 'broom', 'sp', 'zoo', 'fastDummies', 'stargazer', 'RStata')

### necessary scripts
source('ercot_preprocess_functions.R')
source('eia_pre_process_functions.R')
source('statistic_functions.R')


create_necessary_file_folders <- function(){
  dir.create(file.path('../Data'))
  dir.create(file.path('../Data/Economic'))
  dir.create(file.path('../Data/EIA Compiled Data'))
  dir.create(file.path('../Data/ERCOT Compiled Data'))
  dir.create(file.path('../Data/Regressions'))
  dir.create(file.path('../Data/Regressions/pre_model_data'))
  dir.create(file.path('../Data/Weather'))
  
  dir.create(file.path('../Tables'))
  dir.create(file.path('../Tables/Regressions'))
  dir.create(file.path('../Tables/Regressions/underbidding'))
  dir.create(file.path('../Tables/Summary Stats'))

}

run_ercot_program <- function(RStataPath, RStataVersion){
  print('creating target file folders')
  create_necessary_file_folders()
  
  #### loads and processes ERCOT datasets ####
  ############################################
  
  create_generation_capacity_and_adder_data()
  gc()

  # #### loads and processes EIA datasets ####
  # #########################################
  EIA_data <- load_EIA860M_and_EIA932A()

  df_EIA860M <- EIA_data$EIA860M
  df_EIA923A <- EIA_data$EIA923A
  rm(EIA_data)
  gc()

  write_csv(df_EIA860M, '../Data/EIA Compiled Data/EIA860M_compiled_data.csv')
  write_csv(df_EIA923A, '../Data/EIA Compiled Data/EIA923A_compiled_data.csv')

  #### Shape EIA data to represent chronological life of a generator. ####
  ########################################################################
  create_plant_gen_id_phase_timeline_csv()
  gc()

  ################## Create Supporting Data Sets ###################
  ##################################################################
  lag_months = 12 # default 12 months for main body, robustness data created in appendix section.
  load_peaker_net_margin()
  gc()
  
  ################### Monthly Data ###############################
  ################################################################
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
  create_daily_weather_data()
  gc()
  create_matching_dataset()
  gc()
  
  ################# Main Body Tables ###########################
  ##############################################################
  
  #################### Data Summaries ##########################
  #### Table 1 #### - Entry and Exit of Operating Capacity
  create_operating_pool_stats_latex()
  
  # ######### Stata Model for Underbidding, 
  options("RStata.StataPath" = RStataPath)
  options("RStata.StataVersion" = RStataVersion)
  stata('../Stata/energy_time_2to1_full.do')


}

