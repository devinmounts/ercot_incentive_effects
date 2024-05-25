
run_regressions <- function(covar_version_input, rolling_3_month_input, exclude_winter_storm_uri_input, run_polynomial_weather_input, variable_lags_test=FALSE, lag_months, months_to_lag){

  summary_file <- '../Data/Regressions/variable_layers_regression.csv'
  
  if (file.exists(summary_file)) {
    df_covars_of_interest <- read_csv(summary_file)
  } else {
    df_covars_of_interest <- data.frame(regression=character(),
                                        mean_rtorpa_coef=double(),
                                        mean_rtorpa_pvalue=character(),
                                        mean_rtordpa_coef=double(),
                                        mean_rtordpa_pvalue=character(),
                                        roll_mean_pnm_coef=double(),
                                        roll_mean_pnm_pvalue=character(),
                                        r_sq=double(),
                                        adj_r_sq=double(),
                                        obs=integer())
  }
  
  
  
  ## definitions of various regression types ##
  ### movers: a mover is a generator observation where the plant_gen_id first shows up in a specific phase, in this case phase 1 (foremost planning phase)
  ### entrants: an entrant is a generator observation where the plant_gen_id first shows up in any planning phase (1-6)
  ### operant: an operant is a generator observation where the plant_gen_id is defined as moving to phase 7 based on the recorded date in the data set (rather than first observation)
  ### quantity: quantity is equivalent to operant, plus accounts for net changes to rated capacity of a plant already in phase 7.
  
  #global variations
  run_polynomial_weather = run_polynomial_weather_input
  data_robot = F
  rolling_3_month = rolling_3_month_input
  exclude_winter_storm_uri = exclude_winter_storm_uri_input
  
  #covariate version: base_covars, year_fixed_effects, renewable_breakout, both
  covar_version <- covar_version_input
  
  
  
  if (covar_version == 'fixed_effects') {
    final_covariate_names <- c('2022-year', '2021-year', '2020-year', '2019-year', '2018-year', '2017-year',
                               'Incentive Payment - $/MW',
                               'Constant')
  }
  
  if (covar_version == 'fixed_and_seasonal_effects') {
    final_covariate_names <- c('2022-year', '2021-year', '2020-year', '2019-year', '2018-year', '2017-year', 
                               '12-month', '11-month', '10-month', '9-month', '8-month', '7-month', '6-month', '5-month', '3-month', '4-month', '2-month',
                               'Incentive Payment - $/MW', 
                               'Constant')
  }
    
    
  if (covar_version == 'fe_se_and_climatic') {
    final_covariate_names <- c('2022-year', '2021-year', '2020-year', '2019-year', '2018-year', '2017-year',
                               '12-month', '11-month', '10-month', '9-month', '8-month', '7-month', '6-month', '5-month', '3-month', '4-month', '2-month',
                               'Mean Temp. Midpoint - Yr. Roll', 'Mean Windspeed - Yr. Roll', 'Mean Temp. Midpoint Sq. - Yr. Roll',
                               'Incentive Payment - $/MW',
                               'Constant')
  }
  
  if (covar_version == 'fe_se_clim_and_economic') {
    final_covariate_names <- c('2022-year', '2021-year', '2020-year', '2019-year', '2018-year', '2017-year',
                               '12-month', '11-month', '10-month', '9-month', '8-month', '7-month', '6-month', '5-month', '3-month', '4-month', '2-month',
                               'Mean Temp. Midpoint - Yr. Roll',  'Mean Temp. Midpoint Sq. - Yr. Roll', 'Mean Windspeed - Yr. Roll',
                               'Labor Force Pop. (Millions)', 'Unemp. Rate', 'CPI', 'Interest Rate', 'Cost Wind Installation - $/MWh','Cost PV Installation - $/MWh','Natural Gas Price - $/MMBtu', 
                               'Incentive Payment - $/MW',
                               'Constant')
  }
  
  if (covar_version == 'fe_se_clim_econ_and_upmarket') {
    final_covariate_names <- c('2022-year', '2021-year', '2020-year', '2019-year', '2018-year', '2017-year',
                               '12-month', '11-month', '10-month', '9-month', '8-month', '7-month', '6-month', '5-month', '3-month', '4-month', '2-month',
                               'Mean Temp. Midpoint - Yr. Roll',  'Mean Temp. Midpoint Sq. - Yr. Roll', 'Mean Windspeed - Yr. Roll',  
                               'Labor Force Pop. (Millions)', 'Unemp. Rate', 'CPI', 'Interest Rate', 'Cost Wind Installation - $/MWh','Cost PV Installation - $/MWh', 'Natural Gas Price - $/MMBtu',
                               'Online Capacity - GW', 'Offline Capacity - GW', 'Capacity Utilization - %','Net System Lamda','Energy Only Price - $/MW',
                               'Incentive Payment - $/MW',
                               'Constant')
  }
  
  if (covar_version == 'fe_se_clim_econ_upmarket_and_downmarket') {
    final_covariate_names <- c('2022-year', '2021-year', '2020-year', '2019-year', '2018-year', '2017-year',
                               '12-month', '11-month', '10-month', '9-month', '8-month', '7-month', '6-month', '5-month', '3-month', '4-month', '2-month',
                               'Mean Temp. Midpoint - Yr. Roll',  'Mean Temp. Midpoint Sq. - Yr. Roll', 'Mean Windspeed - Yr. Roll',
                               'Labor Force Pop. (Millions)', 'Unemp. Rate', 'CPI', 'Interest Rate', 'Cost Wind Installation - $/MWh','Cost PV Installation - $/MWh', 'Natural Gas Price - $/MMBtu', 
                               'Online Capacity - GW', 'Offline Capacity - GW', 'Capacity Utilization - %','Net System Lamda','Energy Only Price - $/MW',
                               'Incentive Payment - $/MW', 'Change Peaker Net Margin - $/GW', 'NG GINI Index', 'Wind GINI Index',
                               'Renewable Appl. Pool - MW', 'NG Appl. Pool - MW ', 'Mean Status Phase - Renewables', 'Mean Status Phase - NG ',
                               'Constant')
  }
  
  
  
  folder = 'variable_layers_regression/'
  
  # regression types
  
  ng_monthly_totalquantity_12mo_lag = F
  renewable_monthly_totalquantity_12mo_lag = F

  
  # end regression types
  
  
  #variable to save final ng and renewable model comparisons
  # select_net_model_comparisons <- list()
  select_total_model_comparisons <- list()
  select_total_panel_model_comparisons <- list()
  # select_total_planning_model_comparisons <- list()
  
  select_struct_model_comparisons <- list()
  select_struct_panel_model_comparisons <- list()
  
  # select_poly_net_model_comparisons <- list()
  select_poly_total_model_comparisons <- list()
  select_poly_panel_model_comparisons <- list()
  select_poly_struct_model_comparisons <- list()
  select_poly_struct_panel_model_comparisons <- list()
  # select_poly_total_planning_model_comparisons <- list()
  
  

  
  regressions <- c( ng_monthly_totalquantity_12mo_lag, renewable_monthly_totalquantity_12mo_lag)
  
  regressions_string <- c('ng_monthly_totalquantity_12mo_lag', 'renewable_monthly_totalquantity_12mo_lag')
  
  for (i in seq_along(regressions)){
    print(regressions)
    regressions[i] = TRUE
    
    if (rolling_3_month) {
      file_type <- paste(regressions_string[i], '_3mo_roll', sep = '')
      if (exclude_winter_storm_uri) {
        file_type <- paste(file_type, '_no_uri', sep = '')
      }
      if (run_polynomial_weather) {
        file_type <- paste(file_type, '_poly_weather', sep = '')
      }
    }
    else {
      file_type <- paste(regressions_string[i], sep='')
      if (exclude_winter_storm_uri) {
        file_type <- paste(file_type, '_no_uri', sep = '')
      }
      if (run_polynomial_weather) {
        file_type <- paste(file_type, '_poly_weather', sep = '')
      }
    }
    
    
    output_file_path <- paste("../Images/Regressions/Tables/",folder, file_type, ".htm", sep = "")
    output_file_path_latex <- paste("../Images/Regressions/Tables/Latex/", folder, file_type, ".tex", sep = "")
    output_data_path <- paste('../Data/Regressions/',folder, file_type, '_regression_set.csv', sep="")
    decorrelated_output_data_path <- paste('../Data/Regressions/',folder, file_type, '_residuals.csv', sep="")
    

    if (regressions[1] == TRUE) {
      print(paste('running regressions w/ data for: ', regressions_string[i], 'monthtly aggregate'))
      ## monthly observation regression
      df_phase_1 <- read_csv('../Data/Regressions/pre_model_data/ng_panel_totalquantity_12mo_lag.csv')

      df_phase_1 <- df_phase_1 %>%
        mutate(log_pool_n_plant_gen_id_Renewables = 0,
               log_pool_n_plant_gen_id_Other = 0,
               log_pool_n_plant_gen_id = 0,
               log_pool_mean_mw_Renewables = 0,
               log_pool_mean_mw_Other = 0,
               log_pool_mean_mw = 0,
               log_pool_mean_status_phase_Renewables = 0,
               log_pool_mean_status_phase_Other = 0,
               log_pool_mean_status_phase = 0,
               log_pool_mean_weeks_in_phase_Renewables = 0,
               log_pool_mean_weeks_in_phase_Other = 0,
               log_pool_mean_weeks_in_phase = 0) %>%
        rename(new_mw_phs_1 = new_mw_phs_entrants)

      print(paste('natural gas data loaded, minimum date: ', min(df_phase_1$date)))


    }
    #
    if (regressions[2] == TRUE) {
      print(paste('running regressions w/ data for: ', regressions_string[i], 'monthtly aggregate'))
      ## monthly observation regression
      df_phase_1 <- read_csv('../Data/Regressions/pre_model_data/renewable_panel_totalquantity_12mo_lag.csv')

      df_phase_1 <- df_phase_1 %>%
        mutate(log_pool_n_plant_gen_id_Renewables = 0,
               log_pool_n_plant_gen_id_Other = 0,
               log_pool_n_plant_gen_id = 0,
               log_pool_mean_mw_Renewables = 0,
               log_pool_mean_mw_Other = 0,
               log_pool_mean_mw = 0,
               log_pool_mean_status_phase_Renewables = 0,
               log_pool_mean_status_phase_Other = 0,
               log_pool_mean_status_phase = 0,
               log_pool_mean_weeks_in_phase_Renewables = 0,
               log_pool_mean_weeks_in_phase_Other = 0,
               log_pool_mean_weeks_in_phase = 0) %>%
        rename(new_mw_phs_1 = new_mw_phs_entrants)

      print(paste('renewable data loaded, minimum date: ', min(df_phase_1$date)))

    }
    
    if (exclude_winter_storm_uri) {
      ## excludes Feb. 2021 observation
      df_phase_1 <- df_phase_1 %>% filter(!date %in% c(as.Date('2021-02-01')))
      
    }
    
    if (!exists('df_phase_1')) {
      print('setting conditional back to false')
      regressions[i] = FALSE
      print(regressions)
      next
    }
    
    print(paste('Is polynomial weather version', run_polynomial_weather, sep = ' '))
    
    if (rolling_3_month) {
      print('in 3 month roll')
      # 

      roll_covariates <- c('p_labf',
                           'p_uner',
                           'cpi',
                           'mean_shortr',
                           'lcoe_wind_dollars_mw',
                           'lcoe_sun_dollars_mw',
                           'mean_hh_price_MMBtu',
                           'mean_int_tot_spin_cap_mw',
                           'mean_rtoffcap',
                           'mean_total_gen_rtcap_ratio',
                           'mean_net_sys_lamda',
                           'mean_sys_lamda',
                           'mean_rtoffpa',
                           'mean_hb_busavg_energy_only',
                           'mean_rtorpa',
                           'mean_rtordpa',
                           'mean_total_pa',
                           'mean_net_rtorpa',
                           'mean_pnm',
                           'mean_delta_pnm',
                           'nameplate_cap_scarcity_measure',
                           'mean_ng_cap_gini_coef',
                           'mean_wnd_cap_gini_coef',
                           'strat_pool_total_mw_Renewables',
                           'strat_pool_total_mw_NG',
                           'strat_pool_mean_status_phase_Renewables',
                           'strat_pool_mean_status_phase_NG'
                           )
      
      df_phase_1 <- df_phase_1 %>%
        group_by(plant_gen_id) %>%
        arrange(plant_gen_id, date) %>%
        mutate_at(vars(roll_covariates),
                  ~ rollapplyr(.x, list(-seq(2)), mean, na.rm = TRUE, partial = FALSE, fill = NA)) %>%
        ungroup()
      
      df_phase_1 <-  df_phase_1 %>% filter_at(vars(roll_covariates),all_vars(!is.na(.)))
    
      
      if (grepl('monthly', output_file_path )) {
        #turn panel data into aggregated monthly observations
        df_phase_1 <- df_phase_1 %>%
          filter(status_phase %in% c(7,8)) %>%
          group_by(year, month) %>%
          mutate(new_mw_phs_1 = sum(new_mw_phs_1)) %>%
          ungroup() %>%
          select(-c(plant_gen_id:unfilled_net_summer_capacity_mw), -planned_capacity_mw) %>%
          distinct()
        
      } ## end block to create monthly capacity values
    
    } #end rolling 3 month block

    
    if (covar_version == 'fixed_effects') {
      covariates <- list(c('year_2017','year_2018','year_2019','year_2020','year_2021', 'year_2022'),
                         c('mean_total_pa'),
                         c('new_mw_phs_1')
                         # c('mean_rtfunccap')
                         )
      
    }

    if (covar_version == 'fixed_and_seasonal_effects') {
      covariates <- list(c('year_2017','year_2018','year_2019','year_2020','year_2021', 'year_2022'),
                      c('month_12','month_11','month_10','month_9','month_8','month_7','month_6','month_5','month_3', 'month_4','month_2'),
                      c('mean_total_pa'),
                      c('new_mw_phs_1'))
    }
    
    
    if (covar_version == 'fe_se_and_climatic') {
      covariates <- list(c('year_2017','year_2018','year_2019','year_2020','year_2021', 'year_2022'),
                     c('month_12','month_11','month_10','month_9','month_8','month_7','month_6','month_5','month_3', 'month_4','month_2'),
                     c('roll_temp_midpoint'), c('roll.weather_wnds'),
                     c('mean_total_pa'),
                     c('new_mw_phs_1'))
    }
    
    if (covar_version == 'fe_se_clim_and_economic') {
      covariates <- list(c('year_2017','year_2018','year_2019','year_2020','year_2021', 'year_2022'),
                      c('month_12','month_11','month_10','month_9','month_8','month_7','month_6','month_5','month_3', 'month_4','month_2'),
                      c('roll_temp_midpoint'), c('roll.weather_wnds'),
                      c('p_labf', 'p_uner', 'cpi','mean_shortr','lcoe_wind_dollars_mw','lcoe_sun_dollars_mw','mean_hh_price_MMBtu'),
                      c('mean_total_pa'),
                      c('new_mw_phs_1'))
    }
    
    if (covar_version == 'fe_se_clim_econ_and_upmarket') {
      covariates <- list(c('year_2017','year_2018','year_2019','year_2020','year_2021', 'year_2022'),
                      c('month_12','month_11','month_10','month_9','month_8','month_7','month_6','month_5','month_3', 'month_4','month_2'),
                      c('roll_temp_midpoint'), c('roll.weather_wnds'),
                      c('p_labf', 'p_uner', 'cpi','mean_shortr','lcoe_wind_dollars_mw','lcoe_sun_dollars_mw','mean_hh_price_MMBtu'),
                      c('mean_int_tot_spin_cap_mw'), c('mean_rtoffcap'), c('nameplate_cap_scarcity_measure'),c('mean_net_sys_lamda'),c('mean_hb_busavg_energy_only'),
                      c('mean_total_pa'),
                      c('new_mw_phs_1'))
    }
    
    if (covar_version == 'fe_se_clim_econ_upmarket_and_downmarket') {
      covariates <- list(c('year_2017','year_2018','year_2019','year_2020','year_2021', 'year_2022'),
                      c('month_12','month_11','month_10','month_9','month_8','month_7','month_6','month_5','month_3', 'month_4','month_2'),
                      c('roll_temp_midpoint'), c('roll.weather_wnds'),
                      c('p_labf', 'p_uner', 'cpi','mean_shortr','lcoe_wind_dollars_mw','lcoe_sun_dollars_mw','mean_hh_price_MMBtu'),
                      c('mean_int_tot_spin_cap_mw'), c('mean_rtoffcap'), c('nameplate_cap_scarcity_measure'),c('mean_net_sys_lamda'),c('mean_hb_busavg_energy_only'),
                      c('mean_total_pa'), c('mean_delta_pnm'), c('mean_ng_cap_gini_coef'), c('mean_wnd_cap_gini_coef'), 
                      c('strat_pool_total_mw_Renewables', 'strat_pool_total_mw_NG'), c('strat_pool_mean_status_phase_Renewables', 'strat_pool_mean_status_phase_NG'),
                      c('new_mw_phs_1'))
    }

    
   
    
    #change weather covariate to polynomial form, dependent on the covariate version.
    if (run_polynomial_weather) {
      print('this is a polynomial weather model')
      print(paste('covariate version:', covar_version))
      if (!covar_version %in% c('fixed_effects', 'fixed_and_seasonal_effects')) {
        covariates[[3]] <- c('roll_temp_midpoint', 'roll_temp_midpoint_sq')
        df_phase_1$roll_temp_midpoint_sq <- df_phase_1$roll_temp_midpoint**2
      }
      print('Post Polynomial Weather Covars')
      print(covariates)
      
    }
    
    print(paste('preparing to filter data due to variable lag. months to lag: ', lag_months, sep = ''))
    # print(min(df_phase_1$date))
    filter_date <- as.Date('2015-07-01') %m+% months(lag_months)
    
    print(filter_date)
    print(nrow(df_phase_1))
    print(min(df_phase_1$date))
    df_phase_1 <- df_phase_1 %>% filter(date >= filter_date)
    print(nrow(df_phase_1))
    print(min(df_phase_1$date))
    df_to_regress <- df_phase_1
   
    print(setdiff(unlist(covariates), names(df_phase_1))) 
    write_csv(df_to_regress[, c('date',unlist(covariates))], output_data_path)
    
    models <- list()
    saved_covariates <- NULL
    select_covariates <- NULL
    
    print('List Structural Covariates')
    print(covariates)
    
    for (i in seq_along(covariates)) {
      print(paste('starting loop ',i,sep = ' '))
      is_simultaneous = FALSE
      if (length(covariates[[i]])>1) {
        print(paste('this covaritate is simultaneous', covariates[[i]], sep=' '))
        is_simultaneous = TRUE
      } #end block to set simultaneous flag
      
      if (is_simultaneous) {
        print('simultaneous block')
        print(covariates[[i]])
        
        sim_covariates <- covariates[[i]]
        #identify whether the select polynomial covariates are either the simultaneous
        is_weather <- str_detect(sim_covariates, 'test')
        if (TRUE %in% is_weather & run_polynomial_weather) {
          polynomial_weather <- TRUE
        }
        else {
          polynomial_weather <- FALSE
        }
        print(paste('polynomial weather: ', polynomial_weather, sep = " "))
        for (j in sim_covariates) {
          
          dependent_var <- j
          print(paste('dependent var:', dependent_var))
          
          if (is.null(saved_covariates)) {
            print('saved covariates is null')
            print(saved_covariates)
            print('centering covariate around its mean')
            mean_centered_vector <- df_to_regress[[dependent_var]]-mean(df_to_regress[[dependent_var]])
            print('replacing covariate w mean centered')
            df_to_regress[,dependent_var] <- mean_centered_vector
          } # end null saved covariates block
          else {
            print('saved covariates')
            print(saved_covariates)
            f <- as.formula(paste(dependent_var,
                                  paste(saved_covariates, collapse = ' + '),
                                  sep = '~')
            )
            print(f)
            fit <- do.call("lm", list(formula = f, data = quote(df_to_regress)))
            print(paste('replacing dependent var. with residuals of', dependent_var, sep = ' '))
            residuals <- resid(fit)
            df_to_regress[,dependent_var] <- residuals
            models <- c(models, list(fit))
            
            if (polynomial_weather) { ## break out of polynomial weather so that temp isn't regressed twice and added to model list
              break
            }
            
          } # end else null saved covariates block
        } # end simultaneous covariates loop
        if (polynomial_weather) {
          print('creating specified polynomial weather features')
          df_to_regress$roll_temp_midpoint_sq <- df_to_regress$roll_temp_midpoint**2
          
          #creat same vars for ols data set as well
          df_phase_1$roll_temp_midpoint_sq <- df_phase_1$roll_temp_midpoint**2
          
          sim_covariates <- c('roll_temp_midpoint', 'roll_temp_midpoint_sq')
          covariates[[i]] <- c('roll_temp_midpoint', 'roll_temp_midpoint_sq')
        }
        saved_covariates <- c(saved_covariates, sim_covariates)
        print(paste('saved covariates:', saved_covariates, sep = " "))
      } # end simultaneous covariate block
      else{
        print('non simultaneuous block')
        print(covariates[[i]])
        
        dependent_var <- covariates[[i]]
        
        if (i == 1){
          print(dependent_var)
          print('centering first covariate around its mean')
          mean_centered_vector <- df_to_regress[[dependent_var]]-mean(df_to_regress[[dependent_var]])
          print('replacing first covariate w mean centered')
          df_to_regress[,dependent_var] <- mean_centered_vector
        } # end first covariate block (handles case where first covariate is not simultaneous)
        
        else if (i == length(covariates)) {
          print('final covariate residuals captured, running final model with all previous covariate residuals')
          print(paste('dependent var:', dependent_var))
          select_covariates <- unlist(covariates[1:i-1])
          print('select covariates:')
          print(select_covariates)
          f = as.formula(
            paste(dependent_var,
                  paste(select_covariates, collapse = '+'),
                  sep = '~')
          )
          print(f)
          fit <- do.call("lm", list(formula = f, data = quote(df_to_regress)))
          print('fitting ols model of final formula with original data set')
          fit2 <- do.call("lm", list(formula = f, data = quote(df_phase_1)))
          
          models <- c(models, list(fit), list(fit2))
          
          if (variable_lags_test) {
            print('inside variable lags print')
            print(output_file_path)
            print(grepl('ng_monthly', output_file_path))
            if (grepl('ng_monthly', output_file_path)) {
              segment = 'ng'
            }
            if (grepl('renewable_monthly', output_file_path)) {
              segment = 'renewable'
            }
            capture_output_for_variable_layers_regression(fit, fit2, i, segment, covar_version, run_polynomial_weather, exclude_winter_storm_uri, lag_months, months_to_lag)
            
          } ### end capture of statistics for variable lag test
        }
        else{
          print(paste('dependent var:', dependent_var))
          select_covariates <- unlist(covariates[1:i-1])
          print('select covariates:')
          print(select_covariates)
          f = as.formula(
            paste(dependent_var,
                  paste(select_covariates, collapse = '+'),
                  sep = '~')
          )
          print(f)
          fit <- do.call("lm", list(formula = f, data = quote(df_to_regress)))
          print(paste('replacing dependent var. with residuals of', dependent_var, sep = ' '))
          residuals <- resid(fit)
          df_to_regress[,dependent_var] <- residuals
          models <- c(models, list(fit))
          # saved_covariates <- c(select_covariates, dependent_var)
        }
        
        print('assigning dependent variable to saved covariates')
        saved_covariates <- c(select_covariates, dependent_var)
      } # end of non simultaneous block
      
    } # end of covariate loop
    

  
    print(paste('number of models created: ', length(models), sep = ''))
    
    
    print('setting conditional back to false')
    regressions[i] = FALSE
    print(regressions)
  }### end of regressions loop
  
  
}



run_variable_layers_regression_test <- function(reload_data=TRUE){
  
  variable_lags_test = TRUE
  reload_data = TRUE
  months_to_lag = c(6,12,24,36)
  iterations =  length(months_to_lag)
  
  summary_file <- '../Data/Regressions/Capacity Model/robustness/layered_model_statistics_roll_monthly_data.csv'
  if (file.exists(summary_file)) {
    #Delete file if it exists
    file.remove(summary_file)
  }
  
  for(i in 1:iterations){
    print(i)
    lag_months = months_to_lag[i]
    if (reload_data == TRUE){
      print('creating monthly ercot summary')
      create_monthly_ercot_summary(lag_months)
      print('creating pnm summary')
      create_pnm_monthly_summary(lag_months)
      print('creating henry hub summary')
      create_ng_hh_monthly_summary(lag_months)
      print('creating weather summary')
      create_monthly_weather_data(lag_months)
      print('creating economic controlls')
      create_economic_controls_data(lag_months)
      print('creating monthly gini summary')
      create_monthly_gini_summary(lag_months)
      gc()
      
      print('creating panel data')
      create_panel_totalquantity_specific_regressions_12mo_lag(lag_months)
      
    }
    
    covar_versions <- c('fixed_effects', 'fixed_and_seasonal_effects',
                        'fe_se_and_climatic', 'fe_se_clim_and_economic', 
                        'fe_se_clim_econ_and_upmarket', 'fe_se_clim_econ_upmarket_and_downmarket')

    # scenarios: rolling_3_month, exclude_winter_storm_uri, run_polynomial_weather
    scenarios <- list(c(TRUE,FALSE, TRUE), # yes roll, yes uri, yes poly
                      c(TRUE,TRUE, TRUE) # yes roll, no uri, yes poly
    )
    
    for(covar_version_input in covar_versions) {
      print(paste('starting models for covar version: ', covar_version_input, sep=''))
      
      for (i in seq_along(scenarios)) {
        
        rolling_3_month = scenarios[[i]][1]
        exclude_winter_storm_uri = scenarios[[i]][2]
        run_polynomial_weather = scenarios[[i]][3]
        
        run_regressions(covar_version_input, rolling_3_month,exclude_winter_storm_uri, run_polynomial_weather, variable_lags_test, lag_months, months_to_lag)
      }
    }
    gc()
  } # end of lag months loop
  
  format_and_save_latex_output_of_variable_layers_regression()
}




capture_output_for_variable_layers_regression <- function(fit, fit2, i, segment, covar_version, run_polynomial_weather, exclude_winter_storm_uri, lag_months, months_to_lag){
  
  df_ols_model_statistics <- glance(fit2)
  df_rls_model_statistics <- glance(fit)
  
  ### ols stats
  print('capture metrics from  ols fits')
  n_ols = nobs(fit2)
  r_sq_ols = df_ols_model_statistics$r.squared
  adj_r_sq_ols = df_ols_model_statistics$adj.r.squared
  f_stat_ols = df_ols_model_statistics$statistic
  aic_ols = df_ols_model_statistics$AIC
  bic_ols = df_ols_model_statistics$BIC
  fpe_ols = ols_fpe(fit2)
  
  print('capture metrics from rls fits')
  ### rls stats
  n_rls = nobs(fit)
  r_sq_rls = df_rls_model_statistics$r.squared
  adj_r_sq_rls = df_rls_model_statistics$adj.r.squared
  f_stat_rls = df_rls_model_statistics$statistic
  aic_rls = df_rls_model_statistics$AIC
  bic_rls = df_rls_model_statistics$BIC
  fpe_rls = ols_fpe(fit)
  
  #get model statistics
  # output[i,] <- c(segment,months_to_lag[i],r_sq,adj_r_sq,f_stat,aic,bic,fpe)
  print('preparing to pivot metrics from rls and ols fits')
  #get ols adder coefficients
  pa_coefs_ols <- coef(summary(fit2))[c("mean_total_pa"),c("Estimate", 'Pr(>|t|)')]
  df_pa_coefs_ols <- data.frame(covar='total_pa', coef=pa_coefs_ols['Estimate'], p_val=pa_coefs_ols['Pr(>|t|)'], row.names =1)

  
  #get rls adder coefficients
  pa_coefs_rls <- coef(summary(fit))[c("mean_total_pa"),c("Estimate", 'Pr(>|t|)')]
  df_pa_coefs_rls <- data.frame(covar='total_pa', coef=pa_coefs_ols['Estimate'], p_val=pa_coefs_ols['Pr(>|t|)'], row.names =1)
  

  print('captured metrics from rls and ols fits')
  if (segment == 'ng' & covar_version == 'fixed_effects' & run_polynomial_weather== TRUE & exclude_winter_storm_uri == FALSE & lag_months==min(months_to_lag)) {
    df_output <-data.frame(covar_version=covar_version,
                           polynomial_weather=run_polynomial_weather,
                           segment = segment,
                           exclude_winter_storm_uri= exclude_winter_storm_uri,
                           lag_months_model = lag_months,
                           n_ols=n_ols,
                           r_sq_ols=r_sq_ols,
                           adj_r_sq_ols=adj_r_sq_ols,
                           f_stat_ols=f_stat_ols,
                           aic_ols=aic_ols,
                           bic_ols=bic_ols,
                           fpe_ols=fpe_ols,
                           estimate_mean_total_pa_ols = df_pa_coefs_ols$coef[1],
                           p_val_mean_total_pa_ols = df_pa_coefs_ols$p_val[1],
                           n_rls=n_rls,
                           r_sq_rls=r_sq_rls,
                           adj_r_sq_rls=adj_r_sq_rls,
                           f_stat_rls=f_stat_rls,
                           aic_rls=aic_rls,
                           bic_rls=bic_rls,
                           fpe_rls=fpe_rls,
                           estimate_mean_total_pa_rls = df_pa_coefs_rls$coef[1],
                           p_val_mean_total_pa_rls = df_pa_coefs_rls$p_val[1])
    
    
    write_csv(df_output, '../Data/Regressions/Capacity Model/robustness/variable_layers_regression_summary.csv')
    
  } ### end print block for first iteration of model 
  else {
    row <-data.frame(covar_version,run_polynomial_weather,segment,exclude_winter_storm_uri,lag_months,n_ols,r_sq_ols,adj_r_sq_ols,f_stat_ols,aic_ols,bic_ols,fpe_ols,
                     df_pa_coefs_ols$coef, df_pa_coefs_ols$p_val,
                     n_rls,r_sq_rls,adj_r_sq_rls,f_stat_rls,aic_rls,bic_rls,fpe_rls,
                     df_pa_coefs_rls$coef, df_pa_coefs_rls$p_val
    )
    
    # writing row in the csv file 
    write.table(row, file = '../Data/Regressions/Capacity Model/robustness/variable_layers_regression_summary.csv', sep = ",", 
                append = TRUE, quote = FALSE, 
                col.names = FALSE, row.names = FALSE)
    
  } ## end print block for non-first iteration
  
  
} ## end of function to capture output for variable layers test

format_and_save_latex_output_of_variable_layers_regression <- function(){
  df_variable_layers_results <- read_csv('../Data/Regressions/robustness/variable_layers_regression_summary.csv')
  
  
  df_variable_layers_results <- df_variable_layers_results %>%
    mutate(star = case_when(p_val_mean_total_pa_ols <= .001~ '***',
                            p_val_mean_total_pa_ols <= .01~'**',
                            p_val_mean_total_pa_ols <= .05~ '*',
                            TRUE~''))
  
  logical_to_string <- function(x) ifelse(x==TRUE, "X", "")
  
  df_variable_results_formatted <- df_variable_layers_results %>% filter(segment =='ng', lag_months_model !=6) %>%
    mutate(fe = TRUE,
           se = if_else(covar_version == 'fixed_effects', FALSE, TRUE),
           climatic = if_else(covar_version %in% c('fixed_effects', 'fixed_and_seasonal_effects'), FALSE, TRUE),
           economic = if_else(covar_version %in% c('fixed_effects', 'fixed_and_seasonal_effects', 'fe_se_and_climatic'), FALSE, TRUE),
           upmarket = if_else(covar_version %in% c('fixed_effects', 'fixed_and_seasonal_effects', 'fe_se_and_climatic', 'fe_se_clim_and_economic' ), FALSE, TRUE),
           downmarket = if_else(covar_version %in% c('fixed_effects', 'fixed_and_seasonal_effects', 'fe_se_and_climatic', 'fe_se_clim_and_economic', 'fe_se_clim_econ_and_upmarket' ), FALSE, TRUE),
           lag_12mo = if_else(lag_months_model == 12, TRUE, FALSE),
           lag_24mo = if_else(lag_months_model == 24, TRUE, FALSE),
           lag_36mo = if_else(lag_months_model == 36, TRUE, FALSE),
           
    ) %>%
    mutate(incentive_effect=paste(round(estimate_mean_total_pa_ols,3), star, sep = '')) %>%
    arrange(exclude_winter_storm_uri, lag_months_model, fe, se, climatic, economic, upmarket, downmarket) %>%
    mutate_if(is.logical,logical_to_string) %>%
    mutate_if(is.numeric, round,digits=3)
  
  
  for (lag_months in unique(df_variable_results_formatted$lag_months_model)){
    df_lag_select <- df_variable_results_formatted %>% filter(lag_months_model == lag_months)
    lag_column = paste('lag_', lag_months, 'mo', sep='')
    df_lag_select_t <- data.frame(t(df_lag_select[-1]))
    names(df_lag_select_t) <- c('(1)','(2)','(3)','(4)','(5)','(6)','(7)','(8)','(9)','(10)','(11)','(12)')
    df_lag_select_t_rows <- df_lag_select_t[c('incentive_effect', 'exclude_winter_storm_uri',
                                              'fe', 'se', 'climatic', 'economic', 'upmarket', 'downmarket',
                                              'n_ols', 'r_sq_ols', 'aic_ols', 'bic_ols', 'f_stat_ols'),c(1,2,3,4,5,6,12)]
    rownames(df_lag_select_t_rows) <- c('Incentive', 'Exclude Storm Uri', 'Fixed Effects', 'Seasonal Effects', 'Climatic Eff.', 'Economic Eff.', 'Upstream Market Eff.', 'Downstream Market Eff.', 'Obs', 'R2', 'AIC', 'BIC', 'F Statistic')
    
    stargazer(df_lag_select_t_rows, summary = FALSE, type = 'latex', font.size = 'tiny', title=paste('Layered Covariates Incentive Effect: ', lag_months, 'month lag', sep = ''),  out = paste('../Tables/Regressions/Capacity Model/robustness/capacity_model_incentive_effect_layer_robustness_', lag_months, 'mo_lag.tex', sep = ''))
  } ## end of loop
  
}### end of save output function
