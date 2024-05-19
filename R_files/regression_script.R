
run_regressions <- function(covar_version_input, rolling_3_month_input, exclude_winter_storm_uri_input, run_polynomial_weather_input, variable_lags_test=FALSE, lag_months=12){

  summary_file <- '../Tables/Regressions/Capacity Model/Summary/policy_coeficients.csv'
  
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
  
  
  if (covar_version == 'full_controls') {
    final_covariate_names <- c('2022-year', '2021-year', '2020-year', '2019-year', '2018-year', '2017-year',
                               '12-month', '11-month', '10-month', '9-month', '8-month', '7-month', '6-month', '5-month', '3-month', '4-month', '2-month',
                               'Mean Temp. Midpoint - Yr. Roll', 'Mean Windspeed - Yr. Roll',
                               'Labor Force Pop. - Millions', 'Unemp. Rate', 'CPI', 'Interest Rate', 'Cost Wind Installation - $/MWh',
                               'Cost PV Installation - $/MWh', 'Natural Gas Price - $/MMBtu', 'Online Capacity - GW', 'Offline Capacity - GW',
                               'Capacity Utilization - %',
                               'Net System Lamda',
                               'Energy Only Price - $/MW', 'Incentive Payment - $/MW', 'Change Peaker Net Margin - $/GW', 'NG GINI Index', 'Wind GINI Index',
                               'Renewable Appl. Pool - MW', 'NG Appl. Pool - MW ', 'Mean Status Phase - Renewables', 'Mean Status Phase - NG ', 'Constant')
    
    final_covariate_names_poly_weather <- c('2022-year', '2021-year', '2020-year', '2019-year', '2018-year', '2017-year',
                                            '12-month', '11-month', '10-month', '9-month', '8-month', '7-month', '6-month', '5-month', '3-month', '4-month', '2-month',
                                            'Mean Temp. Midpoint - 12mo Mean', 'Mean Temp. Midpoint Sq. - 12mo Mean', 'Mean Windspeed - Yr. Roll',
                                            'Labor Force Pop. - Millions', 'Unemp. Rate', 'CPI', 'Interest Rate', 'Cost Wind Installation - $/MWh',
                                            'Cost PV Installation - $/MWh', 'Natural Gas Price - $/MMBtu', 'Online Capacity - GW', 'Offline Capacity - GW',
                                            'Capacity Utilization - %',
                                            'Net System Lamda',
                                            'Energy Only Price - $/MW', 'Incentive Payment - $/MW', 'Change Peaker Net Margin - $/GW', 'NG GINI Index', 'Wind GINI Index',
                                            'Renewable Appl. Pool - MW', 'NG Appl. Pool - MW ', 'Mean Status Phase - Renewables', 'Mean Status Phase - NG ', 'Constant', 'test'
                                            )
  }
  if (covar_version == 'year_fixed_effects') {
    final_covariate_names <- c('2022-year', '2021-year', '2020-year', '2019-year', '2018-year', '2017-year', 
                               'Mean Temp. Midpoint - Yr. Roll', 'Mean Windspeed - Yr. Roll',
                               'Labor Force Pop. - Millions', 'Unemp. Rate', 'CPI', 'Interest Rate', 'Cost Wind Installation - $/MWh',
                               'Cost PV Installation - $/MWh', 'Natural Gas Price - $/MMBtu', 'Online Capacity - GW', 'Offline Capacity - GW',
                               'Capacity Utilization - %',
                               'Net System Lamda',
                               'Energy Only Price - $/MW', 'Incentive Payment - $/MW',  'Change Peaker Net Margin - $/GW', 'NG GINI Index', 'Wind GINI Index',
                               'Renewable Appl. Pool - MW', 'NG Appl. Pool - MW ', 'Mean Status Phase - Renewables', 'Mean Status Phase - NG ', 'Constant')
    
    final_covariate_names_poly_weather <- c('2022-year', '2021-year', '2020-year', '2019-year', '2018-year', '2017-year',
                                            'Mean Temp. Midpoint - Yr. Roll', 'Mean Temp. Midpoint Sq. - Yr. Roll', 'Mean Windspeed - Yr. Roll',
                                            'Labor Force Pop. - Millions', 'Unemp. Rate', 'CPI', 'Interest Rate', 'Cost Wind Installation - $/MWh',
                                            'Cost PV Installation - $/MWh', 'Natural Gas Price - $/MMBtu', 'Online Capacity - GW', 'Offline Capacity - GW',
                                            'Capacity Utilization - %',
                                            'Net System Lamda',
                                            'Energy Only Price - $/MW', 'Incentive Payment - $/MW',  'Change Peaker Net Margin - $/GW', 'NG GINI Index', 'Wind GINI Index',
                                            'Renewable Appl. Pool - MW', 'NG Appl. Pool - MW ', 'Mean Status Phase - Renewables', 'Mean Status Phase - NG ', 'Constant')
  }
  
  if (covar_version == 'base_covars') {
    final_covariate_names <- c('Mean Temp. Midpoint - Yr. Roll', 'Mean Windspeed - Yr. Roll',
                               'Labor Force Pop. - Millions', 'Unemp. Rate', 'CPI', 'Interest Rate', 'Cost Wind Installation - $/MWh',
                               'Cost PV Installation - $/MWh', 'Natural Gas Price - $/MMBtu', 'Online Capacity - GW', 'Offline Capacity - GW',
                               'Capacity Utilization - %',
                               'Net System Lamda',
                               'Energy Only Price - $/MW', 'Incentive Payment - $/MW', 'Change Peaker Net Margin - $/GW', 'NG GINI Index', 'Wind GINI Index',
                               'Renewable Appl. Pool - MW', 'NG Appl. Pool - MW ', 'Mean Status Phase - Renewables', 'Mean Status Phase - NG ', 'Constant')
    
    final_covariate_names_poly_weather <- c('Mean Temp. Midpoint - Yr. Roll', 'Mean Temp. Midpoint Sq. - Yr. Roll', 'Mean Windspeed - Yr. Roll',
                                            'Labor Force Pop. - Millions', 'Unemp. Rate', 'CPI', 'Interest Rate', 'Cost Wind Installation - $/MWh',
                                            'Cost PV Installation - $/MWh', 'Natural Gas Price - $/MMBtu', 'Online Capacity - GW', 'Offline Capacity - GW',
                                            'Capacity Utilization - %',
                                            'Net System Lamda',
                                            'Energy Only Price - $/MW', 'Incentive Payment - $/MW', 'Change Peaker Net Margin - $/GW', 'NG GINI Index', 'Wind GINI Index',
                                            'Renewable Appl. Pool - MW', 'NG Appl. Pool - MW ', 'Mean Status Phase - Renewables', 'Mean Status Phase - NG ', 'Constant')
  }
  
  
  
  if (covar_version == 'year_fixed_effects') {
    folder <- 'year_fixed_effects/'
  } else if (covar_version == 'renewable_breakout') {
    folder <- 'renewable_breakout/'
  } else if (covar_version == 'full_controls') {
    folder <- 'full_controls/'
  } else {
    folder <- 'base_covars/'
    
  }
  
  dir.create(file.path(paste('../Tables/Regressions/Capacity Model/', covar_version, sep = '')))
  dir.create(file.path(paste('../Tables/Regressions/Capacity Model/Summary/', covar_version, sep = '')))
  dir.create(file.path(paste('../Tables/Regressions/Capacity Model/Summary/direct_and_total_effects/', covar_version, sep = '')))
  dir.create(file.path(paste('../Data/Regressions/Capacity Model/', covar_version, sep = '')))
  
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
  
  

  
  regressions <- c(ng_monthly_totalquantity_12mo_lag, renewable_monthly_totalquantity_12mo_lag)
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
    
    
    output_file_path <- paste("../Tables/Regressions/Capacity Model/",folder, file_type, ".htm", sep = "")
    output_summary_stats_path <- paste("../Tables/Summary Stats/", file_type, "_summary.tex", sep = "")
    output_file_path_latex <- paste("../Tables/Regressions/Capacity Model/", folder, file_type, ".tex", sep = "")
    output_data_path <- paste('../Data/Regressions/Capacity Model/',folder, file_type, '_regression_set.csv', sep="")
    decorrelated_output_data_path <- paste('../Data/Regressions/Capacity Model/',folder, file_type, '_residuals.csv', sep="")
    

    if (regressions[1] == TRUE) {
      print(paste('running regressions w/ data for: ', regressions_string[i], 'monthtly aggregate'))
      ## monthly observation regression
      df_phase_1 <- read_csv('../Data/Regressions/Capacity Model/pre_model_data/ng_panel_totalquantity_12mo_lag.csv')

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

      print(paste('natural gas data loaded,  with nrows: ', nrow(df_phase_1)))
      


    }
    #
    if (regressions[2] == TRUE) {
      print(paste('running regressions w/ data for: ', regressions_string[i], 'monthtly aggregate'))
      ## monthly observation regression
      df_phase_1 <- read_csv('../Data/Regressions/Capacity Model/pre_model_data/renewable_panel_totalquantity_12mo_lag.csv')

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

      print(paste('renewable data loaded, with nrows: ', nrow(df_phase_1)))
      
      
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
      print(paste('after roll filter with nrows: ', nrow(df_phase_1)))
      print(min(df_phase_1$date))
    
      
      if (grepl('monthly', output_file_path )) {
        #turn panel data into aggregated monthly observations
        df_phase_1 <- df_phase_1 %>%
          filter(status_phase %in% c(7,8)) %>%
          group_by(year, month) %>%
          mutate(new_mw_phs_1 = sum(new_mw_phs_1)) %>%
          ungroup() %>%
          select(-c(plant_gen_id:unfilled_net_summer_capacity_mw), -planned_capacity_mw) %>%
          distinct()
        
      }
    
    } # end of rolling window conditional
    
    
    
    covariates <- list(c('roll_temp_midpoint'), c('roll.weather_wnds'), c('p_labf', 'p_uner', 'cpi','mean_shortr',
                                                  'lcoe_wind_dollars_mw',
                                                  'lcoe_sun_dollars_mw',
                                                  'mean_hh_price_MMBtu'),
                       c('mean_int_tot_spin_cap_mw'), c('mean_rtoffcap'),
                       c('nameplate_cap_scarcity_measure'),
                       c('mean_net_sys_lamda'),
                       c('mean_hb_busavg_energy_only'),  c('mean_total_pa'), c('mean_delta_pnm'), c('mean_ng_cap_gini_coef'), c('mean_wnd_cap_gini_coef'),
                       c('strat_pool_total_mw_Renewables', 'strat_pool_total_mw_NG'), c('strat_pool_mean_status_phase_Renewables', 'strat_pool_mean_status_phase_NG'),
                       c('new_mw_phs_1')
    )
    
    
    if (covar_version == 'year_fixed_effects') {
      covariates <- list(c('year_2017','year_2018','year_2019','year_2020','year_2021', 'year_2022'),
                         c('roll_temp_midpoint'),  c('roll.weather_wnds'), c('p_labf', 'p_uner', 'cpi','mean_shortr',
                                                    'lcoe_wind_dollars_mw',
                                                    'lcoe_sun_dollars_mw',
                                                    'mean_hh_price_MMBtu'),
                         c('mean_int_tot_spin_cap_mw'), c('mean_rtoffcap'),
                         c('nameplate_cap_scarcity_measure'),
                         c('mean_net_sys_lamda'),
                         c('mean_hb_busavg_energy_only'),  c('mean_total_pa'),  c('mean_delta_pnm'), c('mean_ng_cap_gini_coef'), c('mean_wnd_cap_gini_coef'),
                         c('strat_pool_total_mw_Renewables', 'strat_pool_total_mw_NG'), c('strat_pool_mean_status_phase_Renewables', 'strat_pool_mean_status_phase_NG'),
                         c('new_mw_phs_1')
                         )
      
    }

    
    if (covar_version == 'full_controls') {
      covariates <- list(c('year_2022','year_2021','year_2020','year_2019','year_2018','year_2017'),
                         c('month_12','month_11','month_10','month_9','month_8','month_7','month_6','month_5','month_3', 'month_4','month_2'),
                         c('roll_temp_midpoint'), c('roll.weather_wnds'), c('p_labf', 'p_uner', 'cpi','mean_shortr',
                                                                            'lcoe_wind_dollars_mw',
                                                                            'lcoe_sun_dollars_mw',
                                                                            'mean_hh_price_MMBtu'),
                         c('mean_int_tot_spin_cap_mw'), c('mean_rtoffcap'),
                         c('nameplate_cap_scarcity_measure'),
                         c('mean_net_sys_lamda'),
                         c('mean_hb_busavg_energy_only'),  c('mean_total_pa'),  c('mean_delta_pnm'), c('mean_ng_cap_gini_coef'), c('mean_wnd_cap_gini_coef'),
                         c('strat_pool_total_mw_Renewables', 'strat_pool_total_mw_NG'), c('strat_pool_mean_status_phase_Renewables', 'strat_pool_mean_status_phase_NG'),
                         c('new_mw_phs_1')

      )
      
      
    }
    
    #change weather covariate to polynomial form, dependent on the covariate version.
    if (run_polynomial_weather) {
      print('this is a polynomial weather model')
      if (covar_version == 'full_controls') {
        covariates[[3]] <- c('roll_temp_midpoint', 'roll_temp_midpoint_sq')
        df_phase_1$roll_temp_midpoint_sq <- df_phase_1$roll_temp_midpoint**2
      } else if (covar_version == 'year_fixed_effects') {
        covariates[[2]] <- c('roll_temp_midpoint', 'roll_temp_midpoint_sq')
        df_phase_1$roll_temp_midpoint_sq <- df_phase_1$roll_temp_midpoint**2
      } else if (covar_version == 'base_covars'){
        covariates[[1]] <- c('roll_temp_midpoint', 'roll_temp_midpoint_sq')
        df_phase_1$roll_temp_midpoint_sq <- df_phase_1$roll_temp_midpoint**2
        print(covariates[[1]])
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
    
    ### create summary statistics ####
    if (lag_months == 12 & run_polynomial_weather==TRUE){
      stargazer(as.data.frame(df_to_regress[,unlist(covariates)]),
                out=output_summary_stats_path,
                covariate.labels = unlist(c(final_covariate_names_poly_weather[1:(length(final_covariate_names_poly_weather)-2)], 'Capacity - MW')),
                digits = 2
                )
    }
    
   
  
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
             capture_output_for_variable_lags_test(fit, fit2, i, segment, covar_version, run_polynomial_weather, lag_months)
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
    
    write_csv(df_to_regress[, unlist(covariates)], decorrelated_output_data_path)
    
  
    print(paste('number of models created: ', length(models), sep = ''))
    
    if (variable_lags_test == FALSE){ ### print final output for models if not running variable lags test
      #save rls and ols models of ng and renewable 12mo lag 3mo roll
      print('preparing to save direct and total effects models')
      if(((grepl('ng_monthly_totalquantity_12mo_lag_3mo_roll', output_file_path) & !(grepl('no_uri', output_file_path)) & !(grepl('poly_weather', output_file_path)))) | ((grepl('renewable_monthly_totalquantity_12mo_lag_3mo_roll', output_file_path) & !(grepl('no_uri', output_file_path)) & !(grepl('poly_weather', output_file_path))))) {
        print('saving select models')
        ## saving terminal models for OLS RLS compare
        select_total_model_comparisons <- c(select_total_model_comparisons, list(models[[length(models)-1]]), list(models[[length(models)]]))
        
        ## saving mid structure models for applicant pool to RLS terminal compare.
        if (grepl('ng_monthly_totalquantity', output_file_path)) {
          print('ng_monthly_totalquantity')
          print(output_file_path)
          if (covar_version == 'full_controls') {
            select_struct_model_comparisons <- c(select_struct_model_comparisons, list(models[[31]]), list(models[[length(models)-1]]))
          }
          else if (covar_version == 'base_covars') {
            select_struct_model_comparisons <- c(select_struct_model_comparisons, list(models[[19]]), list(models[[length(models)-1]]))
          }
          else if (covar_version == 'year_fixed_effects') {
            select_struct_model_comparisons <- c(select_struct_model_comparisons, list(models[[20]]), list(models[[length(models)-1]]))
          } 
          print(paste(length(select_total_model_comparisons), 'selected models now saved', sep = ' '))
        }
        if (grepl('renewable_monthly_totalquantity', output_file_path)) {
          print('renewable__monthly_totalquantity')
          print(output_file_path)
          if (covar_version == 'full_controls') {
            select_struct_model_comparisons <- c(select_struct_model_comparisons, list(models[[30]]), list(models[[length(models)-1]]))
          }
          else if (covar_version == 'base_covars') {
            select_struct_model_comparisons <- c(select_struct_model_comparisons, list(models[[18]]), list(models[[length(models)-1]]))
          }
          else if (covar_version == 'year_fixed_effects') {
            select_struct_model_comparisons <- c(select_struct_model_comparisons, list(models[[19]]), list(models[[length(models)-1]]))
          } 
          print(paste(length(select_total_model_comparisons), 'selected models now saved', sep = ' '))
        }
        
        
        print(paste(length(select_total_model_comparisons), 'selected models now saved', sep = ' '))
        if (length(select_total_model_comparisons) == 4) {
          comparison_file_tex = paste('../Tables/Regressions/Capacity Model/Summary/',covar_version,'/ng_renew_total_quantity_comparison.tex', sep = '')
          comparison_file_html = paste('../Tables/Regressions/Capacity Model/Summary/',covar_version,'/ng_renew_total_quantity_comparison.html', sep = '')
          
          
          stargazer(select_total_model_comparisons[[2]], #OLS NG
                    select_total_model_comparisons[[1]], #RLS NG
                    select_total_model_comparisons[[4]], #OLS Renew
                    select_total_model_comparisons[[3]], #RLS Renew
                    title="Decorrelated Total Quantity Regressions", align=TRUE,
                    omit.stat=c("LL","ser","f"), no.space=TRUE, header = F, report = 'vc*', float.env = 'sidewaystable', font.size = 'footnotesize' , type = 'latex',
                    out = comparison_file_tex,
                    star.cutoffs = c(0.05, 0.01, 0.001),
                    dep.var.labels=c("Total MW Operating"),
                    column.sep.width = "1pt",
                   # covariate.labels = final_covariate_names,
                    digits = 2
          )
          
          stargazer(select_total_model_comparisons[[2]], #OLS NG
                    select_total_model_comparisons[[1]], #RLS NG
                    select_total_model_comparisons[[4]], #OLS Renew
                    select_total_model_comparisons[[3]], #RLS Renew
                    title="Decorrelated Total Quantity Regressions", align=TRUE,
                    omit.stat=c("LL","ser","f"), no.space=TRUE, header = F, report = 'vc*', float.env = 'sidewaystable', font.size = 'footnotesize' , type = 'html',
                    out = comparison_file_html,
                    star.cutoffs = c(0.05, 0.01, 0.001),
                    dep.var.labels=c("Total MW Operatin"),
                    column.sep.width = "1pt",
                   # covariate.labels = final_covariate_names,
                    digits = 2
          )
          
          select_total_model_comparisons <- list()
        }
        
        print(paste(length(select_struct_model_comparisons), 'selected models now saved', sep = ' '))
        if (length(select_struct_model_comparisons) == 4) {
          
          comparison_file_tex = paste('../Tables/Regressions/Capacity Model/Summary/',covar_version,'/ng_renew_struct_quantity_comparison.tex', sep = '')
          comparison_file_html = paste('../Tables/Regressions/Capacity Model/Summary/',covar_version,'/ng_renew_struct_quantity_comparison.html', sep = '')
          
          stargazer(select_struct_model_comparisons[[1]],
                    select_struct_model_comparisons[[2]],
                    select_struct_model_comparisons[[3]],
                    select_struct_model_comparisons[[4]],
                    title="Structural Total Quantity Regressions", align=TRUE,
                    omit.stat=c("LL","ser","f"), no.space=TRUE, header = F, report = 'vc*', float.env = 'sidewaystable', font.size = 'footnotesize' , type = 'latex',
                    out =ng_renew_struct_quantity_comparison_tex,
                    star.cutoffs = c(0.05, 0.01, 0.001),
                    column.sep.width = "1pt",
                   # covariate.labels = final_covariate_names,
                    digits = 2
          )
          
          stargazer(select_struct_model_comparisons[[1]],
                    select_struct_model_comparisons[[2]],
                    select_struct_model_comparisons[[3]],
                    select_struct_model_comparisons[[4]],
                    title="Sturctural Total Quantity Regressions", align=TRUE,
                    omit.stat=c("LL","ser","f"), no.space=TRUE, header = F, report = 'vc*', float.env = 'sidewaystable', font.size = 'footnotesize' , type = 'html',
                    out =ng_renew_struct_quantity_comparison_html,
                    star.cutoffs = c(0.05, 0.01, 0.001),
                    column.sep.width = "1pt",
                   # covariate.labels = final_covariate_names,
                    digits = 2
          )
          
          select_struct_model_comparisons <- list()
        }
      }
      
      
      
      
      #### Polynomial versions 
      ### save rls and ols models of ng and renewable 12mo lag 3mo roll 
      if(((grepl('ng_monthly_totalquantity_12mo_lag_3mo_roll', output_file_path) & !(grepl('no_uri', output_file_path)) & (grepl('poly_weather', output_file_path)))) | ((grepl('renewable_monthly_totalquantity_12mo_lag_3mo_roll', output_file_path) & !(grepl('no_uri', output_file_path)) & (grepl('poly_weather', output_file_path))))) {
        print('saving select polynomial models')
        select_poly_total_model_comparisons <- c(select_poly_total_model_comparisons, list(models[[length(models)-1]]), list(models[[length(models)]]))
        
        if (grepl('ng_monthly_totalquantity', output_file_path)) {
          print('ng_monthly_totalquantity')
          print(output_file_path)
          if (covar_version == 'full_controls') {
            select_poly_struct_model_comparisons <- c(select_poly_struct_model_comparisons, list(models[[32]]), list(models[[length(models)-1]]))
          }
          else if (covar_version == 'base_covars') {
            select_poly_struct_model_comparisons <- c(select_poly_struct_model_comparisons, list(models[[19]]), list(models[[length(models)-1]]))
          }
          else if (covar_version == 'year_fixed_effects') {
            select_poly_struct_model_comparisons <- c(select_poly_struct_model_comparisons, list(models[[21]]), list(models[[length(models)-1]]))
          } 
          print(paste(length(select_poly_total_model_comparisons), 'selected models now saved', sep = ' '))
        }
        if (grepl('renewable_monthly_totalquantity', output_file_path)) {
          print('renewable_monthly_totalquantity')
          print(output_file_path)
          if (covar_version == 'full_controls') {
            select_poly_struct_model_comparisons <- c(select_poly_struct_model_comparisons, list(models[[31]]), list(models[[length(models)-1]]))
          }
           else if (covar_version == 'base_covars') {
            select_poly_struct_model_comparisons <- c(select_poly_struct_model_comparisons, list(models[[18]]), list(models[[length(models)-1]]))
          }
          else if (covar_version == 'year_fixed_effects') {
            select_poly_struct_model_comparisons <- c(select_poly_struct_model_comparisons, list(models[[20]]), list(models[[length(models)-1]]))
          } 
          print(paste(length(select_poly_total_model_comparisons), 'selected models now saved', sep = ' '))
        }
        
        print(paste(length(select_poly_total_model_comparisons), 'selected models now saved', sep = ' '))
        if (length(select_poly_total_model_comparisons) == 4) {
          comparison_file_tex = paste('../Tables/Regressions/Capacity Model/Summary/',covar_version,'/poly_ng_renew_total_quantity_comparison.tex', sep = '')
          comparison_file_html = paste('../Tables/Regressions/Capacity Model/Summary/',covar_version,'/poly_ng_renew_applicant_pool_comparison.tex', sep = '')
          
          stargazer(select_poly_total_model_comparisons[[2]],
                    select_poly_total_model_comparisons[[1]],
                    select_poly_total_model_comparisons[[4]],
                    select_poly_total_model_comparisons[[3]],
                    title="Decorrelated Total Quantity Regressions", align=TRUE,
                    omit.stat=c("LL","ser","f"), no.space=TRUE, header = F, report = 'vc*', float.env = 'sidewaystable', font.size = 'footnotesize' , type = 'latex',
                    out =comparison_file_tex,
                    star.cutoffs = c(0.05, 0.01, 0.001),
                    dep.var.labels=c("Total MW Operating"),
                    column.sep.width = "1pt",
                    covariate.labels = final_covariate_names_poly_weather,
                    digits = 2
          )
          
          stargazer(select_poly_total_model_comparisons[[2]],
                    select_poly_total_model_comparisons[[1]],
                    select_poly_total_model_comparisons[[4]],
                    select_poly_total_model_comparisons[[3]],
                    title="Decorrelated Total Quantity Regressions - Poly Weather", align=TRUE,
                    omit.stat=c("LL","ser","f"), no.space=TRUE, header = F, report = 'vc*', float.env = 'sidewaystable', font.size = 'footnotesize' , type = 'html',
                    out =comparison_file_html,
                    star.cutoffs = c(0.05, 0.01, 0.001),
                    dep.var.labels=c("Total MW Operating"),
                    column.sep.width = "1pt",
                    covariate.labels = final_covariate_names_poly_weather,
                    digits = 2
          )
          
          select_poly_total_model_comparisons <- list()
        }
        
        print(paste(length(select_poly_struct_model_comparisons), 'selected models now saved', sep = ' '))
        if (length(select_poly_struct_model_comparisons) == 4) {
          comparison_file_tex = paste('../Tables/Regressions/Capacity Model/Summary/',covar_version,'/poly_ng_renew_total_quantity_comparison.tex', sep = '')
          comparison_file_html = paste('../Tables/Regressions/Capacity Model/Summary/',covar_version,'/poly_ng_renew_applicant_pool_comparison.tex', sep = '')
          
          stargazer(select_poly_struct_model_comparisons[[1]],
                    select_poly_struct_model_comparisons[[2]],
                    select_poly_struct_model_comparisons[[3]],
                    select_poly_struct_model_comparisons[[4]],
                    title="Structural Total Quantity Regressions - Poly Weather", align=TRUE,
                    omit.stat=c("LL","ser","f"), no.space=TRUE, header = F, report = 'vc*', float.env = 'sidewaystable', font.size = 'footnotesize' , type = 'latex',
                    out =comparison_file_tex,
                    star.cutoffs = c(0.05, 0.01, 0.001),
                    column.sep.width = "1pt",
                    covariate.labels = final_covariate_names_poly_weather,
                    digits = 2
          )
          
          stargazer(select_poly_struct_model_comparisons[[1]],
                    select_poly_struct_model_comparisons[[2]],
                    select_poly_struct_model_comparisons[[3]],
                    select_poly_struct_model_comparisons[[4]],
                    title="Sturctural Total Quantity Regressions - Poly Weather", align=TRUE,
                    omit.stat=c("LL","ser","f"), no.space=TRUE, header = F, report = 'vc*', float.env = 'sidewaystable', font.size = 'footnotesize' , type = 'html',
                    out =comparison_file_html,
                    star.cutoffs = c(0.05, 0.01, 0.001),
                    column.sep.width = "1pt",
                    covariate.labels = final_covariate_names_poly_weather,
                    digits = 2
          )
          
          select_poly_struct_model_comparisons <- list()
          
        } ### end structural comparison prints
      } ### end polynomial version of comparison prints
      
      
    } ### end print statements when not running variable lags test
    
    
    print('setting conditional back to false')
    regressions[i] = FALSE
    print(regressions)
  }### end of regressions loop
  
  if (variable_lags_test == FALSE) {
    write_csv(df_covars_of_interest, summary_file)
  }
  
  
  
} ############## end of run regressions

update_format_of_policy_covars_csv <- function(){
  df_saved <- read_csv('../Tables/Regressions/Capacity Model/Summary/policy_coeficients.csv')
  
  df_saved %>%
    mutate(mean_rtordpa_sign_star = case_when(mean_rtordpa_pvalue <0.001 ~ '***',
                                              mean_rtordpa_pvalue <0.01 ~ '**',
                                              mean_rtordpa_pvalue <0.05 ~ '*',
                                              TRUE ~ ''),
           mean_rtorpa_sign_star = case_when(mean_rtorpa_pvalue <0.001 ~ '***',
                                             mean_rtorpa_pvalue <0.01 ~ '**',
                                             mean_rtorpa_pvalue <0.05 ~ '*',
                                             TRUE ~ ''),
           roll_mean_pnm_sign_star = case_when(roll_mean_pnm_pvalue <0.001 ~ '***',
                                               roll_mean_pnm_pvalue <0.01 ~ '**',
                                               roll_mean_pnm_pvalue <0.05 ~ '*',
                                               TRUE ~ '')) %>%
    select(regression, mean_rtorpa_coef, mean_rtorpa_pvalue, mean_rtorpa_sign_star, mean_rtordpa_coef, mean_rtordpa_pvalue, mean_rtordpa_sign_star, roll_mean_pnm_coef, roll_mean_pnm_pvalue, roll_mean_pnm_sign_star, r_sq, adj_r_sq, obs) %>%
    mutate_if(is.numeric, round, digits=4) %>%
    separate(regression, c('resource', 'category'), sep='_', remove=FALSE) %>%
    mutate(is_lag = if_else(grepl('12mo', regression), TRUE, FALSE),
           is_roll = if_else(grepl('3mo', regression), TRUE, FALSE),
           is_no_uri = if_else(grepl('uri', regression), TRUE, FALSE)) %>%
    write_csv('../Tables/Regressions/Capacity Model/Summary/policy_coef_w_sign.csv')
}

run_variable_lags_test <- function(){
  
  variable_lags_test = TRUE
  reload_data = FALSE
  months_to_lag = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,
    22,23,24,25,26,27,28,29,30,31,32,33,34,35,36
                    )
  iterations =  length(months_to_lag)
  
  summary_file <- '../Data/Regressions/lag_model_statistics_roll_monthly_data.csv'
  if (file.exists(summary_file)) {
    #Delete file if it exists
    file.remove(summary_file)
  }
  
  for(i in 1:iterations){
    print(i)
    lag_months = months_to_lag[i]
    if (reload_data) {
      
      
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
    }
    
    
    print('creating panel data')
    create_panel_totalquantity_specific_regressions_12mo_lag(lag_months)
    
    covar_versions <- c('base_covars',
                        'year_fixed_effects',
                        'full_controls'
    )
    # scenarios: rolling_3_month, exclude_winter_storm_uri, run_polynomial_weather
    scenarios <- list(#c(TRUE, TRUE, FALSE),
      # c(TRUE, TRUE, TRUE),
      c(TRUE,FALSE, FALSE), # primary scenario
      c(TRUE,FALSE, TRUE) # primary scenario; poly
      # c(FALSE, TRUE, FALSE)
      # c(FALSE, TRUE, TRUE),
      # c(FALSE, FALSE, FALSE),
      # c(FALSE, FALSE, TRUE)
    )
    
    for(covar_version_input in covar_versions) {
      print(paste('starting models for covar version: ', covar_version_input, sep=''))
      
      for (i in seq_along(scenarios)) {
        
        rolling_3_month = scenarios[[i]][1]
        exclude_winter_storm_uri = scenarios[[i]][2]
        run_polynomial_weather = scenarios[[i]][3]
        
        run_regressions(covar_version_input, rolling_3_month,exclude_winter_storm_uri, run_polynomial_weather, variable_lags_test, lag_months)
      }
    }
    gc()
  } # end of lag months loop
  
  create_variable_lags_appendix_summary_and_figure()
}


run_rls_treatment_model <- function(run_polynomial_weather=TRUE){
  
  covar_version = 'base_covars'
  active_low_threshold = 50
  df_matching_data <- read_csv( '../Data/ERCOT Compiled Data/ercot_scarcity_pricing_matching_set.csv')
  
  df_matching_data <- df_matching_data %>%
    dummy_cols(select_columns = c('year', 'month', 'hour', 'minute')) %>%
    mutate(active = ifelse(total_pa >0, 1, 0),
           rttotcap_gw = rttotcap/1000,
           int_tot_gen_gas_gw = int_tot_gen_gas/1000,
           int_tot_gen_renewable_gw = int_tot_gen_renewable/1000,
           int_tot_gen_other_gw = int_tot_gen_other/1000,
           ng_gw = NG/1000,
           renewables_gw = Renewables/1000,
           other_gw = Other/1000
           )
  
  
  
  df_matching_data <- df_matching_data %>% drop_na(rtordpa,hb_hubavg_energy_only,hb_busavg_energy_only,NG,Renewables,Other)
  
  
  covariates <- list(c('year_2022','year_2021','year_2020','year_2019','year_2018','year_2017', 'year_2016'),
                     c('month_12','month_11','month_10','month_9','month_8','month_7','month_6','month_5','month_4', 'month_3','month_2'),
                     c('hour_23', 'hour_22','hour_21', 'hour_20', 'hour_19','hour_18', 'hour_17', 'hour_16','hour_15', 'hour_14', 'hour_13','hour_12', 'hour_11', 'hour_10',
                       'hour_9','hour_8', 'hour_7', 'hour_6','hour_5', 'hour_4', 'hour_3','hour_2', 'hour_1'),
                     c('minute_45', 'minute_30','minute_15'),
                     c('ng_gw', 'renewables_gw', 'other_gw'),
                     c('temp_midpoint'),c('weather_wnds'),
                     c('ng_price'), c('int_tot_gen_gas_gw', 'int_tot_gen_renewable_gw','int_tot_gen_other_gw'),
                     c('rttotcap_gw'),
                     c('scarcity_measure'),
                     c('total_pa'),
                     c('active'),
                     c('hb_busavg_energy_only')
  )
  
  df_phase_1 <- df_matching_data %>%
    filter_at(vars(unlist(covariates)),all_vars(!is.na(.)))
  
  df_phase_1 <- df_phase_1 %>%
    mutate(temp_midpoint = temp_midpoint - mean(df_phase_1$temp_midpoint),
           temp_midpoint_sq = temp_midpoint**2)
  
  # write_csv(df_phase_1[,unlist(covariates)],'../Images/Regressions/rls_for_robin/data/underbidding_dataset.csv')
  final_covariate_names  <- c('2022-year', '2021-year', '2020-year', '2019-year', '2018-year', '2017-year', '2016-year',
                              '12-month', '11-month', '10-month', '9-month', '8-month', '7-month', '6-month', '5-month', '4-month', '3-month', '2-month',
                              '23-hour', '22-hour', '21-hour', '20-hour', '19-hour', '18-hour', '17-hour', '16-hour', '15-hour', '14-hour', '13-hour', '12-hour', '11-hour', '10-hour', '9-hour', '8-hour', '7-hour', '6-hour', '5-hour', '4-hour', '3-hour', '2-hour', '1-hour',
                              '45-minute', '30-minutes', '15-minute',
                              'Nat. Gas Capacity - GW', 'Renewable Capacity - GW', 'Other Capacity - GW',
                              'Midpoint Temp. ', 'Wind speed km/hr', 'Natural Gas Price - $/MMBtu',
                              'Supply Nat. Gas - GW', 'Supply Renewables - GW', 'Supply Other - GW',
                              'Reserve Capacity - GW', 'Capacity Utilization - %',
                              'Incentive Payment - $/MW', 'Incentive Active - Y/N',
                              'Constant')

  #change weather covariate to polynomial form, dependent on the covariate version.
  if (run_polynomial_weather) {
    print('this is a polynomial weather model')
    if (covar_version == 'base_covars'){
      covariates[[6]] <- c('temp_midpoint', 'temp_midpoint_sq')
      print(covariates[[1]])
      final_covariate_names  <- c('2022-year', '2021-year', '2020-year', '2019-year', '2018-year', '2017-year', '2016-year',
                                  '12-month', '11-month', '10-month', '9-month', '8-month', '7-month', '6-month', '5-month', '4-month', '3-month', '2-month',
                                  '23-hour', '22-hour', '21-hour', '20-hour', '19-hour', '18-hour', '17-hour', '16-hour', '15-hour', '14-hour', '13-hour', '12-hour', '11-hour', '10-hour', '9-hour', '8-hour', '7-hour', '6-hour', '5-hour', '4-hour', '3-hour', '2-hour', '1-hour',
                                  '45-minute', '30-minutes', '15-minute',
                                  'Nat. Gas Capacity - GW', 'Renewable Capacity - GW', 'Other Capacity - GW',
                                  'Midpoint Temp. ', 'Midpoint Temp. Sq.',  'Wind speed km/hr', 'Natural Gas Price - $/MMBtu',
                                  'Supply Nat. Gas - GW', 'Supply Renewables - GW', 'Supply Other - GW',
                                  'Reserve Capacity - GW', 'Capacity Utilization - %',
                                  'Incentive Payment - $/MW',  'Incentive Active - Y/N',
                                 'Constant')
    }
    print('Post Polynomial Weather Covars')
    print(covariates)
    
  }
  
  df_to_regress <- df_phase_1
  

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
          # print(summary(fit))
          print(paste('replacing dependent var. with residuals of', dependent_var, sep = ' '))
          residuals <- resid(fit)
          df_to_regress[,dependent_var] <- residuals
          models <- c(models, list(fit))
          
          if (polynomial_weather) {
            break
          }
          
        } # end else null saved covariates block
      } # end simultaneous covariates loop
      if (polynomial_weather) {
        print('creating specified polynomial weather features')
        df_to_regress$temp_midpoint_sq <- df_to_regress$temp_midpoint**2
        
        #creat same vars for ols data set as well
        df_phase_1$temp_midpoint_sq <- df_phase_1$temp_midpoint**2
        
        write_csv(df_to_regress,'../Data/ploy_test_resid.csv')
        write_csv(df_phase_1,'../Data/ploy_test_orig.csv')
        
        sim_covariates <- c('temp_midpoint', 'temp_midpoint_sq')
        covariates[[i]] <- c('temp_midpoint', 'temp_midpoint_sq')
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
        print(summary(fit))
        
        print('fitting ols model of final formula with original data set')
        fit2 <- do.call("lm", list(formula = f, data = quote(df_phase_1)))
        print(summary(fit2))
        
        models <- c(models, list(fit), list(fit2))
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
        # print(summary(fit))
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
  if (run_polynomial_weather == TRUE) {
    stargazer(models[[1]],
              models[[2]],
              models[[3]],
              models[[4]],
              models[[5]],
              models[[6]],
              models[[7]],
              models[[8]],
              models[[9]],
              models[[10]],
              models[[11]],
              models[[12]],
              models[[13]],
              models[[14]],
              models[[15]],
              models[[16]],
              models[[17]],
              models[[18]],
              models[[19]],
              models[[20]],
              models[[21]],
              models[[22]],
              models[[23]],
              models[[24]],
              models[[25]],
              models[[26]],
              models[[27]],
              models[[28]],
              models[[29]],
              models[[30]],
              models[[31]],
              models[[32]],
              models[[33]],
              models[[34]],
              models[[35]],
              models[[36]],
              models[[37]],
              models[[38]],
              models[[39]],
              models[[40]],
              models[[41]],
              models[[42]],
              models[[43]],
              models[[44]],
              models[[45]],
              models[[46]],
              models[[47]],
              models[[48]],
              models[[49]],
              models[[50]],
              models[[51]],
              models[[52]],
              models[[53]],
              title=paste("RLS Active Treament", sep = ' '), align=TRUE,
              omit.stat=c("LL","ser","f"), no.space=TRUE, header = F, report = 'vc*', float.env = 'sidewaystable', font.size = 'footnotesize' , type = 'html', out = '../Images/Regressions/matching/rls_matching_poly_weather.html',
              star.cutoffs = c(0.05, 0.01, 0.001), column.sep.width = "1pt", covariate.labels = final_covariate_names, digits = 2
              )
    
    stargazer(models[[52]],
              models[[53]],
              title=paste("RLS Active Treament", sep = ' '), align=TRUE,
              omit.stat=c("LL","ser","f"), no.space=TRUE, header = F, report = 'vc*', float.env = 'sidewaystable', font.size = 'footnotesize' , type = 'html', out = '../Images/Regressions/matching/rls__ols_compare_matching_poly_weather.html',
              star.cutoffs = c(0.05, 0.01, 0.001), column.sep.width = "1pt",covariate.labels = final_covariate_names, digits = 2
              )
    stargazer(models[[52]],
              models[[53]],
              title=paste("RLS Active Treament", sep = ' '), align=TRUE,
              omit.stat=c("LL","ser","f"), no.space=TRUE, header = F, report = 'vc*', float.env = 'sidewaystable', font.size = 'footnotesize' , type = 'latex', out = '../Images/Regressions/matching/rls__ols_compare_matching_poly_weather.tex',
              star.cutoffs = c(0.05, 0.01, 0.001), column.sep.width = "1pt",covariate.labels = final_covariate_names, digits = 2
    )
  } ### end poly weather print statements
  else {
    stargazer(models[[1]],
              models[[2]],
              models[[3]],
              models[[4]],
              models[[5]],
              models[[6]],
              models[[7]],
              models[[8]],
              models[[9]],
              models[[10]],
              models[[11]],
              models[[12]],
              models[[13]],
              models[[14]],
              models[[15]],
              models[[16]],
              models[[17]],
              models[[18]],
              models[[19]],
              models[[20]],
              models[[21]],
              models[[22]],
              models[[23]],
              models[[24]],
              models[[25]],
              models[[26]],
              models[[27]],
              models[[28]],
              models[[29]],
              models[[30]],
              models[[31]],
              models[[32]],
              models[[33]],
              models[[34]],
              models[[35]],
              models[[36]],
              models[[37]],
              models[[38]],
              models[[39]],
              models[[40]],
              models[[41]],
              models[[42]],
              models[[43]],
              models[[44]],
              models[[45]],
              models[[46]],
              models[[47]],
              models[[48]],
              models[[49]],
              models[[50]],
              models[[51]],
              models[[52]],
              title=paste("RLS Active Treament", sep = ' '), align=TRUE,
              omit.stat=c("LL","ser","f"), no.space=TRUE, header = F, report = 'vc*', float.env = 'sidewaystable', font.size = 'footnotesize' , type = 'html', out = '../Images/Regressions/matching/rls_matching.html',
              star.cutoffs = c(0.05, 0.01, 0.001), column.sep.width = "1pt", covariate.labels = final_covariate_names, digits = 2
              )
  } ### end no poly print statements
 
  
}## end RLS underbidding model

run_rls_timeseries_underbidding_model <- function(run_polynomial_weather=TRUE){
  
  covar_version = 'base_covars'
  df_timeseries_data <- read_csv( '../Data/ERCOT Compiled Data/underbidding_data_w_lags.csv')
  
  df_timeseries_data$date <- as.Date(paste(df_timeseries_data$year, df_timeseries_data$month, df_timeseries_data$day, sep = '-'), format="%Y-%m-%d")
  df_timeseries_data$day_of_week <- as.POSIXlt(df_timeseries_data$date)$wday
  df_timeseries_data <- df_timeseries_data %>%
    dummy_cols(select_columns = c('year', 'month', 'day_of_week', 'hour', 'minute'))
  
  
  
  covariates <- list(c('year_2022','year_2021','year_2020','year_2019','year_2018','year_2017', 'year_2016'),
                     c('month_12','month_11','month_10','month_9','month_8','month_7','month_6','month_5','month_4', 'month_3','month_2'),
                     c('day_of_week_1', 'day_of_week_2','day_of_week_3','day_of_week_4','day_of_week_5','day_of_week_6'),
                     c('hour_23', 'hour_22','hour_21', 'hour_20', 'hour_19','hour_18', 'hour_17', 'hour_16','hour_15', 'hour_14', 'hour_13','hour_12', 'hour_11', 'hour_10',
                       'hour_9','hour_8', 'hour_7', 'hour_6','hour_5', 'hour_4', 'hour_3','hour_2', 'hour_1'),
                     c('minute_45', 'minute_30','minute_15'),
                     c('ng_gw', 'renewables_gw', 'other_gw'),
                     c('temp_midpoint_cent'),c('weather_wnds'),
                     c('ng_price'),
                     c('int_tot_gen_gas_gw', 'int_tot_gen_renewable_gw','int_tot_gen_other_gw'),
                     c('rttotcap_gw'),
                     c('scarcity_measure'),
                     c('active'),
                     c('total_pa'),
                     c('price')
  )
  
  df_phase_1 <- df_timeseries_data %>%
    filter_at(vars(unlist(covariates)),all_vars(!is.na(.)))
  rm(df_timeseries_data)
  print('column names')
  print(names(df_phase_1))
  
  # write_csv(df_phase_1[,unlist(covariates)],'../Images/Regressions/rls_for_robin/data/underbidding_dataset.csv')
  final_covariate_names  <- c('2022-year', '2021-year', '2020-year', '2019-year', '2018-year', '2017-year', '2016-year',
                              '12-month', '11-month', '10-month', '9-month', '8-month', '7-month', '6-month', '5-month', '4-month', '3-month', '2-month',
                              'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday',
                              '23-hour', '22-hour', '21-hour', '20-hour', '19-hour', '18-hour', '17-hour', '16-hour', '15-hour', '14-hour', '13-hour', '12-hour', '11-hour', '10-hour', '9-hour', '8-hour', '7-hour', '6-hour', '5-hour', '4-hour', '3-hour', '2-hour', '1-hour',
                              '45-minute', '30-minutes', '15-minute',
                              'Nat. Gas Capacity - GW', 'Renewable Capacity - GW', 'Other Capacity - GW',
                              'Midpoint Temp. ', 'Wind speed km/hr', 'Natural Gas Price - $/MMBtu',
                              'Supply Nat. Gas - GW', 'Supply Renewables - GW', 'Supply Other - GW',
                              'Reserve Capacity - GW', 'Capacity Utilization - %',
                              'Incentive Active - Y/N','Incentive Payment - $/MW',
                              'Constant')
  
  #change weather covariate to polynomial form, dependent on the covariate version.
  if (run_polynomial_weather) {
    print('this is a polynomial weather model')
    if (covar_version == 'base_covars'){
      covariates[[7]] <- c('temp_midpoint_cent', 'temp_midpoint_sq')
      print(length(unlist(covariates)))
      final_covariate_names  <- c('2022-year', '2021-year', '2020-year', '2019-year', '2018-year', '2017-year', '2016-year',
                                  '12-month', '11-month', '10-month', '9-month', '8-month', '7-month', '6-month', '5-month', '4-month', '3-month', '2-month',
                                  'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday',
                                  '23-hour', '22-hour', '21-hour', '20-hour', '19-hour', '18-hour', '17-hour', '16-hour', '15-hour', '14-hour', '13-hour', '12-hour', '11-hour', '10-hour', '9-hour', '8-hour', '7-hour', '6-hour', '5-hour', '4-hour', '3-hour', '2-hour', '1-hour',
                                  '45-minute', '30-minutes', '15-minute',
                                  'Nat. Gas Capacity - GW', 'Renewable Capacity - GW', 'Other Capacity - GW',
                                  'Midpoint Temp. ', 'Midpoint Temp. Sq.',  'Wind speed km/hr', 'Natural Gas Price - $/MMBtu',
                                  'Supply Nat. Gas - GW', 'Supply Renewables - GW', 'Supply Other - GW',
                                  'Reserve Capacity - GW', 'Capacity Utilization - %',
                                  'Incentive Active - Y/N','Incentive Payment - $/MW'
                                  )
    }
    print('Post Polynomial Weather Covars')
    print(covariates)
    
  }
  
  df_to_regress <- df_phase_1
  
  
  
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
          # print(summary(fit))
          print(paste('replacing dependent var. with residuals of', dependent_var, sep = ' '))
          residuals <- resid(fit)
          df_to_regress[,dependent_var] <- residuals
          models <- c(models, list(fit))
          
          if (polynomial_weather) {
            break
          }
          
        } # end else null saved covariates block
      } # end simultaneous covariates loop
      if (polynomial_weather) {
        
        write_csv(df_to_regress,'../Data/ploy_test_resid.csv')
        write_csv(df_phase_1,'../Data/ploy_test_orig.csv')
        
        sim_covariates <- c('temp_midpoint_cent', 'temp_midpoint_sq')
        covariates[[i]] <- c('temp_midpoint_cent', 'temp_midpoint_sq')
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
        print(summary(fit))
        
        print('fitting ols model of final formula with original data set')
        fit2 <- do.call("lm", list(formula = f, data = quote(df_phase_1)))
        print(summary(fit2))
        
        models <- c(models, list(fit), list(fit2))
        rm(df_phase_1)
        rm(df_to_regress)
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
        # print(summary(fit))
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
  print(paste('run_poly_weather', run_polynomial_weather))
  if (run_polynomial_weather == TRUE) {
    print('inside')
    # stargazer(models[[1]],
    #           models[[2]],
    #           models[[3]],
    #           models[[4]],
    #           models[[5]],
    #           models[[6]],
    #           models[[7]],
    #           models[[8]],
    #           models[[9]],
    #           models[[10]],
    #           models[[11]],
    #           models[[12]],
    #           models[[13]],
    #           models[[14]],
    #           models[[15]],
    #           models[[16]],
    #           models[[17]],
    #           models[[18]],
    #           models[[19]],
    #           models[[20]],
    #           models[[21]],
    #           models[[22]],
    #           models[[23]],
    #           models[[24]],
    #           models[[25]],
    #           models[[26]],
    #           models[[27]],
    #           models[[28]],
    #           models[[29]],
    #           models[[30]],
    #           models[[31]],
    #           models[[32]],
    #           models[[33]],
    #           models[[34]],
    #           models[[35]],
    #           models[[36]],
    #           models[[37]],
    #           models[[38]],
    #           models[[39]],
    #           models[[40]],
    #           models[[41]],
    #           models[[42]],
    #           models[[43]],
    #           models[[44]],
    #           models[[45]],
    #           models[[46]],
    #           models[[47]],
    #           models[[48]],
    #           models[[49]],
    #           models[[50]],
    #           models[[51]],
    #           models[[52]],
    #           models[[53]],
    #           models[[54]],
    #           models[[55]],
    #           models[[56]],
    #           models[[57]],
    #           models[[58]],
    #           models[[59]],
    #           models[[60]],
    #           models[[61]],
    #           models[[62]],
    #           title=paste("RLS Active Treament", sep = ' '), align=TRUE,
    #           omit.stat=c("LL","ser","f"), no.space=TRUE, header = F, report = 'vc*', float.env = 'sidewaystable', font.size = 'footnotesize' , type = 'html', out = '../Images/Regressions/matching/rls_matching_poly_weather.html',
    #           star.cutoffs = c(0.05, 0.01, 0.001), column.sep.width = "1pt", covariate.labels = final_covariate_names, digits = 2
    # )
    # print('after full')
    
    
    stargazer(models[[length(models)]],
              models[[length(models)-1]],
              title=paste("RLS Active Treament", sep = ' '), align=TRUE,
              omit.stat=c("LL","ser","f"), no.space=TRUE, header = F, report = 'vc*', float.env = 'sidewaystable', font.size = 'footnotesize' , type = 'latex', out = '../Tables/Regressions/underbidding/gsls_ols_ts_matching_poly_weather.tex',
              star.cutoffs = c(0.05, 0.01, 0.001), column.sep.width = "1pt",covariate.labels = final_covariate_names,
              digits = 2
    )
    
    stargazer(models[[length(models)]],
              models[[length(models)-1]],
              title=paste("RLS Active Treament", sep = ' '), align=TRUE,
              omit.stat=c("LL","ser","f"), no.space=TRUE, header = F, report = 'vc*', float.env = 'sidewaystable', font.size = 'footnotesize' , type = 'html', out = '../Tables/Regressions/underbidding/gsls_ols_compare_ts_poly_weather.html',
              star.cutoffs = c(0.05, 0.01, 0.001), column.sep.width = "1pt",covariate.labels = final_covariate_names,
              digits = 2
    )
    
  } ### end poly weather print statements
  else {
    stargazer(models[[1]],
              models[[2]],
              models[[3]],
              models[[4]],
              models[[5]],
              models[[6]],
              models[[7]],
              models[[8]],
              models[[9]],
              models[[10]],
              models[[11]],
              models[[12]],
              models[[13]],
              models[[14]],
              models[[15]],
              models[[16]],
              models[[17]],
              models[[18]],
              models[[19]],
              models[[20]],
              models[[21]],
              models[[22]],
              models[[23]],
              models[[24]],
              models[[25]],
              models[[26]],
              models[[27]],
              models[[28]],
              models[[29]],
              models[[30]],
              models[[31]],
              models[[32]],
              models[[33]],
              models[[34]],
              models[[35]],
              models[[36]],
              models[[37]],
              models[[38]],
              models[[39]],
              models[[40]],
              models[[41]],
              models[[42]],
              models[[43]],
              models[[44]],
              models[[45]],
              models[[46]],
              models[[47]],
              models[[48]],
              models[[49]],
              models[[50]],
              models[[51]],
              models[[52]],
              models[[53]],
              models[[54]],
              models[[55]],
              models[[56]],
              models[[57]],
              models[[58]],
              models[[59]],
              models[[60]],
              models[[61]],
              models[[62]],
              title=paste("RLS Active Treament", sep = ' '), align=TRUE,
              omit.stat=c("LL","ser","f"), no.space=TRUE, header = F, report = 'vc*', float.env = 'sidewaystable', font.size = 'footnotesize' , type = 'html', out = '../Tables/Regressions/underbidding/underbidding_direct_and_total_effects.html',
              star.cutoffs = c(0.05, 0.01, 0.001), column.sep.width = "1pt", covariate.labels = final_covariate_names, digits = 2
    )
  } ### end no poly print statements
  
  
}## end RLS timeseries underbidding model


run_rls_ar1_timeseries_underbidding_model <- function(run_polynomial_weather=TRUE){
  
  covar_version = 'base_covars'
  df_timeseries_data <- read_csv( '../Data/Generation/underbidding_data_w_lags.csv')
  
  df_timeseries_data <- df_timeseries_data %>%
    dummy_cols(select_columns = c('year', 'month', 'day', 'hour', 'minute'))
  
  
  
  covariates <- list(c('year_2022','year_2021','year_2020','year_2019','year_2018','year_2017', 'year_2016'),
                     c('month_12','month_11','month_10','month_9','month_8','month_7','month_6','month_5','month_4', 'month_3','month_2'),
                     c('day_30','day_29', 'day_28','day_27', 'day_26','day_25', 'day_24','day_23', 'day_22','day_21', 'day_20', 'day_19','day_18', 'day_17', 'day_16','day_15', 'day_14', 'day_13','day_12', 'day_11', 'day_10',
                       'day_9','day_8', 'day_7', 'day_6','day_5', 'day_4', 'day_3','day_2', 'day_1'),
                     c('hour_23', 'hour_22','hour_21', 'hour_20', 'hour_19','hour_18', 'hour_17', 'hour_16','hour_15', 'hour_14', 'hour_13','hour_12', 'hour_11', 'hour_10',
                       'hour_9','hour_8', 'hour_7', 'hour_6','hour_5', 'hour_4', 'hour_3','hour_2', 'hour_1'),
                     c('minute_45', 'minute_30','minute_15'),
                     c('ng_gw', 'renewables_gw', 'other_gw'),
                     c('temp_midpoint_cent'),c('weather_wnds'),
                     c('ng_price'),
                     c('e_1'),
                     c('int_tot_gen_gas_gw', 'int_tot_gen_renewable_gw','int_tot_gen_other_gw'),
                     c('rttotcap_gw'),
                     c('scarcity_measure'),
                     c('active'),
                     c('total_pa'),
                     c('price')
  )
  
  df_phase_1 <- df_timeseries_data %>%
    filter_at(vars(unlist(covariates)),all_vars(!is.na(.)))
  rm(df_timeseries_data)
  print('column names')
  print(names(df_phase_1))
  
  # write_csv(df_phase_1[,unlist(covariates)],'../Images/Regressions/rls_for_robin/data/underbidding_dataset.csv')
  final_covariate_names  <- c('2022-year', '2021-year', '2020-year', '2019-year', '2018-year', '2017-year', '2016-year',
                              '12-month', '11-month', '10-month', '9-month', '8-month', '7-month', '6-month', '5-month', '4-month', '3-month', '2-month',
                              '30-Day','29-Day', '28-Day','27-Day', '26-Day','25-Day', '24-Day','23-Day', '22-Day','21-Day', '20-Day', '19-Day','18-Day', '17-Day', '16-Day','15-Day', '14-Day', '13-Day','12-Day', '11-Day', '10-Day',
                              '9-Day','8-Day', '7-Day', '6-Day','5-Day', '4-Day', '3-Day','2-Day', '1-Day',
                              '23-hour', '22-hour', '21-hour', '20-hour', '19-hour', '18-hour', '17-hour', '16-hour', '15-hour', '14-hour', '13-hour', '12-hour', '11-hour', '10-hour', '9-hour', '8-hour', '7-hour', '6-hour', '5-hour', '4-hour', '3-hour', '2-hour', '1-hour',
                              '45-minute', '30-minutes', '15-minute',
                              'Nat. Gas Capacity - GW', 'Renewable Capacity - GW', 'Other Capacity - GW',
                              'Midpoint Temp. ', 'Wind speed km/hr', 'Natural Gas Price - $/MMBtu',
                              'Supply Nat. Gas - GW', 'Supply Renewables - GW', 'Supply Other - GW',
                              'Reserve Capacity - GW', 'Capacity Utilization - %',
                              'Incentive Active - Y/N','Incentive Payment - $/MW',
                              'Constant')
  
  #change weather covariate to polynomial form, dependent on the covariate version.
  if (run_polynomial_weather) {
    print('this is a polynomial weather model')
    if (covar_version == 'base_covars'){
      covariates[[7]] <- c('temp_midpoint_cent', 'temp_midpoint_sq')
      print(length(unlist(covariates)))
      final_covariate_names  <- c('2022-year', '2021-year', '2020-year', '2019-year', '2018-year', '2017-year', '2016-year',
                                  '12-month', '11-month', '10-month', '9-month', '8-month', '7-month', '6-month', '5-month', '4-month', '3-month', '2-month',
                                  '30-Day','29-Day', '28-Day','27-Day', '26-Day','25-Day', '24-Day','23-Day', '22-Day','21-Day', '20-Day', '19-Day','18-Day', '17-Day', '16-Day','15-Day', '14-Day', '13-Day','12-Day', '11-Day', '10-Day',
                                  '9-Day','8-Day', '7-Day', '6-Day','5-Day', '4-Day', '3-Day','2-Day', '1-Day',
                                  '23-hour', '22-hour', '21-hour', '20-hour', '19-hour', '18-hour', '17-hour', '16-hour', '15-hour', '14-hour', '13-hour', '12-hour', '11-hour', '10-hour', '9-hour', '8-hour', '7-hour', '6-hour', '5-hour', '4-hour', '3-hour', '2-hour', '1-hour',
                                  '45-minute', '30-minutes', '15-minute',
                                  'Nat. Gas Capacity - GW', 'Renewable Capacity - GW', 'Other Capacity - GW',
                                  'Midpoint Temp. ', 'Midpoint Temp. Sq.',  'Wind speed km/hr', 'Natural Gas Price - $/MMBtu', '15min lag',
                                  'Supply Nat. Gas - GW', 'Supply Renewables - GW', 'Supply Other - GW',
                                  'Reserve Capacity - GW', 'Capacity Utilization - %',
                                  'Incentive Active - Y/N','Incentive Payment - $/MW'#,
                                  #'15min_lag', '30min_lag','45min_lag', '60min_lag', '75min_lag'
                                  #'24hr_lag', '48hr_lag','72hr_lag','96hr_lag','120hr_lag'#,
                                  # 'Constant'
      )
    }
    print('Post Polynomial Weather Covars')
    print(covariates)
    
  }
  
  df_to_regress <- df_phase_1
  
  
  
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
          # print(summary(fit))
          print(paste('replacing dependent var. with residuals of', dependent_var, sep = ' '))
          residuals <- resid(fit)
          df_to_regress[,dependent_var] <- residuals
          models <- c(models, list(fit))
          
          if (polynomial_weather) {
            break
          }
          
        } # end else null saved covariates block
      } # end simultaneous covariates loop
      if (polynomial_weather) {
        
        write_csv(df_to_regress,'../Data/ploy_test_resid.csv')
        write_csv(df_phase_1,'../Data/ploy_test_orig.csv')
        
        sim_covariates <- c('temp_midpoint_cent', 'temp_midpoint_sq')
        covariates[[i]] <- c('temp_midpoint_cent', 'temp_midpoint_sq')
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
        print(summary(fit))
        
        print('fitting ols model of final formula with original data set')
        fit2 <- do.call("lm", list(formula = f, data = quote(df_phase_1)))
        print(summary(fit2))
        
        models <- c(models, list(fit), list(fit2))
        rm(df_phase_1)
        rm(df_to_regress)
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
        # print(summary(fit))
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
  print(paste('run_poly_weather', run_polynomial_weather))
  if (run_polynomial_weather == TRUE) {
    print('inside')
    # stargazer(models[[1]],
    #           models[[2]],
    #           models[[3]],
    #           models[[4]],
    #           models[[5]],
    #           models[[6]],
    #           models[[7]],
    #           models[[8]],
    #           models[[9]],
    #           models[[10]],
    #           models[[11]],
    #           models[[12]],
    #           models[[13]],
    #           models[[14]],
    #           models[[15]],
    #           models[[16]],
    #           models[[17]],
    #           models[[18]],
    #           models[[19]],
    #           models[[20]],
    #           models[[21]],
    #           models[[22]],
    #           models[[23]],
    #           models[[24]],
    #           models[[25]],
    #           models[[26]],
    #           models[[27]],
    #           models[[28]],
    #           models[[29]],
    #           models[[30]],
    #           models[[31]],
    #           models[[32]],
    #           models[[33]],
    #           models[[34]],
    #           models[[35]],
    #           models[[36]],
    #           models[[37]],
    #           models[[38]],
    #           models[[39]],
    #           models[[40]],
    #           models[[41]],
    #           models[[42]],
    #           models[[43]],
    #           models[[44]],
    #           models[[45]],
    #           models[[46]],
    #           models[[47]],
    #           models[[48]],
    #           models[[49]],
    #           models[[50]],
    #           models[[51]],
    #           models[[52]],
    #           models[[53]],
    #           models[[54]],
    #           models[[55]],
    #           models[[56]],
    #           models[[57]],
    #           models[[58]],
    #           models[[59]],
    #           models[[60]],
    #           models[[61]],
    #           models[[62]],
    #           title=paste("RLS Active Treament", sep = ' '), align=TRUE,
    #           omit.stat=c("LL","ser","f"), no.space=TRUE, header = F, report = 'vc*', float.env = 'sidewaystable', font.size = 'footnotesize' , type = 'html', out = '../Images/Regressions/matching/rls_matching_poly_weather.html',
    #           star.cutoffs = c(0.05, 0.01, 0.001), column.sep.width = "1pt", covariate.labels = final_covariate_names, digits = 2
    # )
    # print('after full')
    
  
    stargazer(models[[84]],
              models[[83]],
              title=paste("RLS Active Treament", sep = ' '), align=TRUE,
              omit.stat=c("LL","ser","f"), no.space=TRUE, header = F, report = 'vc*', float.env = 'sidewaystable', font.size = 'footnotesize' , type = 'latex', out = '../Images/Regressions/matching/rls_ols_ts_matching_poly_weather_ar1.tex',
              star.cutoffs = c(0.05, 0.01, 0.001), column.sep.width = "1pt",covariate.labels = final_covariate_names,
              digits = 2
    )
    
   
  } ### end poly weather print statements
  else {
    stargazer(models[[1]],
              models[[2]],
              models[[3]],
              models[[4]],
              models[[5]],
              models[[6]],
              models[[7]],
              models[[8]],
              models[[9]],
              models[[10]],
              models[[11]],
              models[[12]],
              models[[13]],
              models[[14]],
              models[[15]],
              models[[16]],
              models[[17]],
              models[[18]],
              models[[19]],
              models[[20]],
              models[[21]],
              models[[22]],
              models[[23]],
              models[[24]],
              models[[25]],
              models[[26]],
              models[[27]],
              models[[28]],
              models[[29]],
              models[[30]],
              models[[31]],
              models[[32]],
              models[[33]],
              models[[34]],
              models[[35]],
              models[[36]],
              models[[37]],
              models[[38]],
              models[[39]],
              models[[40]],
              models[[41]],
              models[[42]],
              models[[43]],
              models[[44]],
              models[[45]],
              models[[46]],
              models[[47]],
              models[[48]],
              models[[49]],
              models[[50]],
              models[[51]],
              models[[52]],
              models[[53]],
              models[[54]],
              models[[55]],
              models[[56]],
              models[[57]],
              models[[58]],
              models[[59]],
              models[[60]],
              models[[61]],
              models[[62]],
              title=paste("RLS Active Treament", sep = ' '), align=TRUE,
              omit.stat=c("LL","ser","f"), no.space=TRUE, header = F, report = 'vc*', float.env = 'sidewaystable', font.size = 'footnotesize' , type = 'html', out = '../Images/Regressions/matching/rls_matching.html',
              star.cutoffs = c(0.05, 0.01, 0.001), column.sep.width = "1pt", covariate.labels = final_covariate_names, digits = 2
    )
  } ### end no poly print statements
  
  
}## end RLS AR1 timeseries underbidding model

run_rls_ar_all_timeseries_underbidding_model <- function(run_polynomial_weather=TRUE){
  
  covar_version = 'base_covars'
  df_timeseries_data <- read_csv( '../Data/Generation/underbidding_data_w_lags.csv')
  
  df_timeseries_data <- df_timeseries_data %>%
    dummy_cols(select_columns = c('year', 'month', 'day', 'hour', 'minute'))
  
  
  
  covariates <- list(c('year_2022','year_2021','year_2020','year_2019','year_2018','year_2017', 'year_2016'),
                     c('month_12','month_11','month_10','month_9','month_8','month_7','month_6','month_5','month_4', 'month_3','month_2'),
                     c('day_30','day_29', 'day_28','day_27', 'day_26','day_25', 'day_24','day_23', 'day_22','day_21', 'day_20', 'day_19','day_18', 'day_17', 'day_16','day_15', 'day_14', 'day_13','day_12', 'day_11', 'day_10',
                       'day_9','day_8', 'day_7', 'day_6','day_5', 'day_4', 'day_3','day_2', 'day_1'),
                     c('hour_23', 'hour_22','hour_21', 'hour_20', 'hour_19','hour_18', 'hour_17', 'hour_16','hour_15', 'hour_14', 'hour_13','hour_12', 'hour_11', 'hour_10',
                       'hour_9','hour_8', 'hour_7', 'hour_6','hour_5', 'hour_4', 'hour_3','hour_2', 'hour_1'),
                     c('minute_45', 'minute_30','minute_15'),
                     c('ng_gw', 'renewables_gw', 'other_gw'),
                     c('d_5'),c('d_4'),c('d_3'),c('d_2'),c('d_1'),
                     c('temp_midpoint_cent'),c('weather_wnds'),
                     c('ng_price'),
                     c('e_5'),c('e_4'),c('e_3'),c('e_2'),c('e_1'),
                     c('int_tot_gen_gas_gw', 'int_tot_gen_renewable_gw','int_tot_gen_other_gw'),
                     c('rttotcap_gw'),
                     c('scarcity_measure'),
                     c('active'),
                     c('total_pa'),
                     c('price')
  )
  
  df_phase_1 <- df_timeseries_data %>%
    filter_at(vars(unlist(covariates)),all_vars(!is.na(.)))
  rm(df_timeseries_data)
  print('column names')
  print(names(df_phase_1))
  
  # write_csv(df_phase_1[,unlist(covariates)],'../Images/Regressions/rls_for_robin/data/underbidding_dataset.csv')
  final_covariate_names  <- c('2022-year', '2021-year', '2020-year', '2019-year', '2018-year', '2017-year', '2016-year',
                              '12-month', '11-month', '10-month', '9-month', '8-month', '7-month', '6-month', '5-month', '4-month', '3-month', '2-month',
                              '30-Day','29-Day', '28-Day','27-Day', '26-Day','25-Day', '24-Day','23-Day', '22-Day','21-Day', '20-Day', '19-Day','18-Day', '17-Day', '16-Day','15-Day', '14-Day', '13-Day','12-Day', '11-Day', '10-Day',
                              '9-Day','8-Day', '7-Day', '6-Day','5-Day', '4-Day', '3-Day','2-Day', '1-Day',
                              '23-hour', '22-hour', '21-hour', '20-hour', '19-hour', '18-hour', '17-hour', '16-hour', '15-hour', '14-hour', '13-hour', '12-hour', '11-hour', '10-hour', '9-hour', '8-hour', '7-hour', '6-hour', '5-hour', '4-hour', '3-hour', '2-hour', '1-hour',
                              '45-minute', '30-minutes', '15-minute',
                              'Nat. Gas Capacity - GW', 'Renewable Capacity - GW', 'Other Capacity - GW',
                              'Midpoint Temp. ', 'Wind speed km/hr', 'Natural Gas Price - $/MMBtu',
                              'Supply Nat. Gas - GW', 'Supply Renewables - GW', 'Supply Other - GW',
                              'Reserve Capacity - GW', 'Capacity Utilization - %',
                              'Incentive Active - Y/N','Incentive Payment - $/MW',
                              'Constant')
  
  #change weather covariate to polynomial form, dependent on the covariate version.
  if (run_polynomial_weather) {
    print('this is a polynomial weather model')
    if (covar_version == 'base_covars'){
      covariates[[12]] <- c('temp_midpoint_cent', 'temp_midpoint_sq')
      print(length(unlist(covariates)))
      final_covariate_names  <- c('2022-year', '2021-year', '2020-year', '2019-year', '2018-year', '2017-year', '2016-year',
                                  '12-month', '11-month', '10-month', '9-month', '8-month', '7-month', '6-month', '5-month', '4-month', '3-month', '2-month',
                                  '30-Day','29-Day', '28-Day','27-Day', '26-Day','25-Day', '24-Day','23-Day', '22-Day','21-Day', '20-Day', '19-Day','18-Day', '17-Day', '16-Day','15-Day', '14-Day', '13-Day','12-Day', '11-Day', '10-Day',
                                  '9-Day','8-Day', '7-Day', '6-Day','5-Day', '4-Day', '3-Day','2-Day', '1-Day',
                                  '23-hour', '22-hour', '21-hour', '20-hour', '19-hour', '18-hour', '17-hour', '16-hour', '15-hour', '14-hour', '13-hour', '12-hour', '11-hour', '10-hour', '9-hour', '8-hour', '7-hour', '6-hour', '5-hour', '4-hour', '3-hour', '2-hour', '1-hour',
                                  '45-minute', '30-minutes', '15-minute',
                                  'Nat. Gas Capacity - GW', 'Renewable Capacity - GW', 'Other Capacity - GW'
                                  # '24hr_lag', '48hr_lag','72hr_lag','96hr_lag', '120hr_lag',
                                  # 'Midpoint Temp. ', 'Midpoint Temp. Sq.',  'Wind speed km/hr', 'Natural Gas Price - $/MMBtu',
                                  # '15min_lag', '30min_lag','45min_lag', '60min_lag', '75min_lag',
                                  # 'Supply Nat. Gas - GW', 'Supply Renewables - GW', 'Supply Other - GW',
                                  # 'Reserve Capacity - GW', 'Capacity Utilization - %',
                                  # 'Incentive Active - Y/N','Incentive Payment - $/MW'
      )
    }
    print('Post Polynomial Weather Covars')
    print(covariates)
    
  }
  
  df_to_regress <- df_phase_1
  
  
  
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
          # print(summary(fit))
          print(paste('replacing dependent var. with residuals of', dependent_var, sep = ' '))
          residuals <- resid(fit)
          df_to_regress[,dependent_var] <- residuals
          models <- c(models, list(fit))
          
          if (polynomial_weather) {
            break
          }
          
        } # end else null saved covariates block
      } # end simultaneous covariates loop
      if (polynomial_weather) {
        
        write_csv(df_to_regress,'../Data/ploy_test_resid.csv')
        write_csv(df_phase_1,'../Data/ploy_test_orig.csv')
        
        sim_covariates <- c('temp_midpoint_cent', 'temp_midpoint_sq')
        covariates[[i]] <- c('temp_midpoint_cent', 'temp_midpoint_sq')
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
        print(summary(fit))
        
        print('fitting ols model of final formula with original data set')
        fit2 <- do.call("lm", list(formula = f, data = quote(df_phase_1)))
        print(summary(fit2))
        
        models <- c(models, list(fit), list(fit2))
        rm(df_phase_1)
        rm(df_to_regress)
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
        # print(summary(fit))
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
  print(paste('run_poly_weather', run_polynomial_weather))
  if (run_polynomial_weather == TRUE) {
    print('inside')
    # stargazer(models[[1]],
    #           models[[2]],
    #           models[[3]],
    #           models[[4]],
    #           models[[5]],
    #           models[[6]],
    #           models[[7]],
    #           models[[8]],
    #           models[[9]],
    #           models[[10]],
    #           models[[11]],
    #           models[[12]],
    #           models[[13]],
    #           models[[14]],
    #           models[[15]],
    #           models[[16]],
    #           models[[17]],
    #           models[[18]],
    #           models[[19]],
    #           models[[20]],
    #           models[[21]],
    #           models[[22]],
    #           models[[23]],
    #           models[[24]],
    #           models[[25]],
    #           models[[26]],
    #           models[[27]],
    #           models[[28]],
    #           models[[29]],
    #           models[[30]],
    #           models[[31]],
    #           models[[32]],
    #           models[[33]],
    #           models[[34]],
    #           models[[35]],
    #           models[[36]],
    #           models[[37]],
    #           models[[38]],
    #           models[[39]],
    #           models[[40]],
    #           models[[41]],
    #           models[[42]],
    #           models[[43]],
    #           models[[44]],
    #           models[[45]],
    #           models[[46]],
    #           models[[47]],
    #           models[[48]],
    #           models[[49]],
    #           models[[50]],
    #           models[[51]],
    #           models[[52]],
    #           models[[53]],
    #           models[[54]],
    #           models[[55]],
    #           models[[56]],
    #           models[[57]],
    #           models[[58]],
    #           models[[59]],
    #           models[[60]],
    #           models[[61]],
    #           models[[62]],
    #           title=paste("RLS Active Treament", sep = ' '), align=TRUE,
    #           omit.stat=c("LL","ser","f"), no.space=TRUE, header = F, report = 'vc*', float.env = 'sidewaystable', font.size = 'footnotesize' , type = 'html', out = '../Images/Regressions/matching/rls_matching_poly_weather.html',
    #           star.cutoffs = c(0.05, 0.01, 0.001), column.sep.width = "1pt", covariate.labels = final_covariate_names, digits = 2
    # )
    # print('after full')
    
    
    stargazer(models[[93]],
              models[[92]],
              title=paste("RLS Active Treament", sep = ' '), align=TRUE,
              omit.stat=c("LL","ser","f"), no.space=TRUE, header = F, report = 'vc*', float.env = 'sidewaystable', font.size = 'footnotesize' , type = 'latex', out = '../Images/Regressions/matching/rls_ols_ts_matching_poly_weather_ar10.tex',
              star.cutoffs = c(0.05, 0.01, 0.001), column.sep.width = "1pt",#covariate.labels = final_covariate_names,
              digits = 2
    )
    
    
  } ### end poly weather print statements
  else {
    stargazer(models[[1]],
              models[[2]],
              models[[3]],
              models[[4]],
              models[[5]],
              models[[6]],
              models[[7]],
              models[[8]],
              models[[9]],
              models[[10]],
              models[[11]],
              models[[12]],
              models[[13]],
              models[[14]],
              models[[15]],
              models[[16]],
              models[[17]],
              models[[18]],
              models[[19]],
              models[[20]],
              models[[21]],
              models[[22]],
              models[[23]],
              models[[24]],
              models[[25]],
              models[[26]],
              models[[27]],
              models[[28]],
              models[[29]],
              models[[30]],
              models[[31]],
              models[[32]],
              models[[33]],
              models[[34]],
              models[[35]],
              models[[36]],
              models[[37]],
              models[[38]],
              models[[39]],
              models[[40]],
              models[[41]],
              models[[42]],
              models[[43]],
              models[[44]],
              models[[45]],
              models[[46]],
              models[[47]],
              models[[48]],
              models[[49]],
              models[[50]],
              models[[51]],
              models[[52]],
              models[[53]],
              models[[54]],
              models[[55]],
              models[[56]],
              models[[57]],
              models[[58]],
              models[[59]],
              models[[60]],
              models[[61]],
              models[[62]],
              title=paste("RLS Active Treament", sep = ' '), align=TRUE,
              omit.stat=c("LL","ser","f"), no.space=TRUE, header = F, report = 'vc*', float.env = 'sidewaystable', font.size = 'footnotesize' , type = 'html', out = '../Images/Regressions/matching/rls_matching.html',
              star.cutoffs = c(0.05, 0.01, 0.001), column.sep.width = "1pt", covariate.labels = final_covariate_names, digits = 2
    )
  } ### end no poly print statements
  
  
}## end RLS AR_all timeseries underbidding model


regress_active_price_adder <- function(){
  
  df_matching_data <- read_csv( '../Data/Generation/ercot_scarcity_pricing_matching_set.csv')
  
  df_matching_data <- df_matching_data %>% 
    mutate(active_low = if_else(rtorpa >0 & rtorpa <=.5,1,0),
           active_high = if_else(rtorpa >.5,1,0)
           ) %>%
    rename(price = hub_hubavg_energy_only)
  
  fit_matching <- lm(price~active_high + active_low + as.character(month) + as.character(year) + as.character(hour) + as.character(minute)
                     + int_tot_gen_gas + int_tot_gen_renewable + int_tot_gen_other
                     + rttotcap + rtordpa + rtorpa + scarcity_measure + ng + other + renewables +
                       I(c.temp_midpoint, 3))
  
  return(fit_matching)
  
  
}

#### code for reference does not execute as a function ####
tests_for_OLS_Monthly_to_panel_continuity <- function() {
  #generators for unit root tests
  df_EIA860M <- read_csv('../Data/EIA Compiled Data/EIA860M_compiled_data.csv')
  
  
  operating_generators <- df_EIA860M %>% filter(status_phase %in% c(7,8)) %>% distinct(plant_gen_id) %>% pull(plant_gen_id)
  df_tests <- df_EIA860M %>% 
    filter(balancing_authority_code == 'ERCO', energy_source_code_group %in% c('NG', 'Renewables'), plant_gen_id %in% operating_generators) %>%
    select(plant_gen_id, energy_source_code_group, net_summer_capacity_mw, year, month, status_phase) %>%
    mutate(date = as.Date(paste(year, month, '01', sep='-'))) %>%
    filter(date >= '2016-09-01') %>%
    sample_n(1000)
  
  
  
  
  df_ng <- read_csv('../Data/Regressions/pre_model_data/ng_panel_totalquantity_12mo_lag.csv')
  roll_covariates <- c('p_labf', 'p_uner', 'cpi','mean_shortr','lcoe_wind_dollars_mw','lcoe_sun_dollars_mw','mean_hh_price_MMBtu','mean_int_tot_spin_cap_mw','mean_rtoffcap','mean_net_sys_lamda', 'mean_sys_lamda', 'mean_rtoffpa','mean_hb_busavg_energy_only','mean_counterfeit_pa','mean_rtordpa','mean_net_rtorpa','mean_pnm','mean_ng_cap_gini_coef','mean_wnd_cap_gini_coef','strat_pool_total_mw_Renewables', 'strat_pool_total_mw_NG','strat_pool_mean_status_phase_Renewables', 'strat_pool_mean_status_phase_NG')
  
  
  df_ng <- df_ng %>% filter(date >= '2016-07-01')
  df_ng <- df_ng %>%
    group_by(plant_gen_id) %>%
    arrange(plant_gen_id, date) %>%
    mutate_at(vars(roll_covariates),
              ~ rollapplyr(.x, list(-seq(2)), mean, na.rm = TRUE, partial = FALSE, fill = NA)) %>%
    ungroup() %>%
    rename(new_mw_phs_1 = new_mw_phs_entrants)
  
  df_ng <-  df_ng %>% filter_at(vars(roll_covariates),all_vars(!is.na(.)))
  
  
  
  
  
  df_ng_panel <- df_ng
  length(unique(df_ng$date))
  length(unique(df_ng$plant_gen_id))
  df_ng_monthly <- df_ng %>%
    filter(status_phase %in% c(7,8)) %>%
    group_by(year, month) %>%
    mutate(new_mw_phs_1 = sum(new_mw_phs_1)) %>%
    ungroup() %>%
    select(-c(plant_gen_id:unfilled_net_summer_capacity_mw), -planned_capacity_mw) %>%
    distinct()
  
  
  
  df_ng_panel %>%
    select(date, plant_gen_id, new_mw_phs_1,  roll_covariates) %>%
    arrange(date, plant_gen_id)
  
  df_ng_monthly %>%
    select(date, new_mw_phs_1, roll_covariates)
  
  df_ng_panel %>%
    group_by(date) %>%
    summarise(mean_mw = mean(new_mw_phs_1),
              n_gen = length(unique(plant_gen_id))) %>%
    mutate(total_cap_at_mean = mean_mw*n_gen)
  
  df_ng_monthly %>% slice(rep(row_number(), length(unique(df_ng$plant_gen_id)))) %>%
    mutate(new_mw_phs_1 = new_mw_phs_1/length(unique(df_ng$plant_gen_id))) %>%
    select(date, new_mw_phs_1,  roll_covariates) %>%
    arrange(date)
  
  df_ng_monthly %>% slice(rep(row_number(), length(unique(df_ng$plant_gen_id)))) %>%
    mutate(new_mw_phs_1 = new_mw_phs_1/length(unique(df_ng$plant_gen_id))) %>%
    select(date, new_mw_phs_1,  roll_covariates) %>%
    arrange(date) %>%
    distinct(date, new_mw_phs_1)
  
  df_ng_monthly %>%
    group_by(date) %>%
    summarize(new_mw_phs_1 = sum(new_mw_phs_1))
  
  
  
  
  
  
  
  
  ##compare panel grouped by month with monthly panel for equivalency
  all.equal( df_ng_panel %>%
               group_by(date) %>%
               summarize(new_mw_phs_1 = sum(new_mw_phs_1)),
             
             df_ng_monthly %>%
               group_by(date) %>%
               summarize(new_mw_phs_1 = sum(new_mw_phs_1)))
  
  ##compare panel grouped by month with monthly panel repliicated and divided to create "mean generator" dataset
  all.equal( df_ng_panel %>%
               group_by(date) %>%
               summarize(new_mw_phs_1 = sum(new_mw_phs_1)),
             
             df_ng_monthly %>% slice(rep(row_number(), length(unique(df_ng$plant_gen_id)))) %>%
               mutate(new_mw_phs_1 = new_mw_phs_1/length(unique(df_ng$plant_gen_id))) %>%
               group_by(date) %>%
               summarize(new_mw_phs_1 = sum(new_mw_phs_1)))
  
  
  df_ng_panel %>%
    group_by(plant_gen_id) %>%
    tally() %>%
    arrange(desc(n))
  
  
  
  
  df_tests
  df_tests %>% filter(energy_source_code_group == 'NG') %>%
    left_join(df_ng_panel %>% select(date, plant_gen_id, energy_source_code_group, status_phase, new_mw_phs_1), by=c('plant_gen_id', 'date')) %>%
    write_csv('../Data/Regressions/unit_tests/ng_panel_mw_comparision.csv')
  
  
  
  
  
  
  df_renewable <- read_csv('../Data/Regressions/pre_model_data/renewable_panel_totalquantity_12mo_lag.csv')
  roll_covariates <- c('p_labf', 'p_uner', 'cpi','mean_shortr','lcoe_wind_dollars_mw','lcoe_sun_dollars_mw','mean_hh_price_MMBtu','mean_int_tot_spin_cap_mw','mean_rtoffcap','mean_net_sys_lamda', 'mean_sys_lamda', 'mean_rtoffpa','mean_hb_busavg_energy_only','mean_counterfeit_pa','mean_rtordpa','mean_net_rtorpa','mean_pnm','mean_ng_cap_gini_coef','mean_wnd_cap_gini_coef','strat_pool_total_mw_Renewables', 'strat_pool_total_mw_NG','strat_pool_mean_status_phase_Renewables', 'strat_pool_mean_status_phase_NG')
  
  df_renewable <- df_renewable %>% filter(date >= '2016-07-01')
  df_renewable <- df_renewable %>%
    group_by(plant_gen_id) %>%
    arrange(plant_gen_id, date) %>%
    mutate_at(vars(roll_covariates),
              ~ rollapplyr(.x, list(-seq(2)), mean, na.rm = TRUE, partial = FALSE, fill = NA)) %>%
    ungroup() %>%
    rename(new_mw_phs_1 = new_mw_phs_entrants)
  
  df_renewable <-  df_renewable %>% filter_at(vars(roll_covariates),all_vars(!is.na(.)))
  
  
  df_renewable_panel <- df_renewable
  length(unique(df_renewable_panel$date))
  length(unique(df_renewable_panel$plant_gen_id))
  df_renewable_monthly <- df_renewable %>%
    filter(status_phase %in% c(7,8)) %>%
    group_by(year, month) %>%
    mutate(new_mw_phs_1 = sum(new_mw_phs_1)) %>%
    ungroup() %>%
    select(-c(plant_gen_id:unfilled_net_summer_capacity_mw), -planned_capacity_mw) %>%
    distinct()
  
  ##compare panel grouped by month with monthly panel for equivalency
  all.equal(df_renewable_panel %>%
              group_by(date) %>%
              summarize(new_mw_phs_1 = sum(new_mw_phs_1)),
            
            df_renewable_monthly %>%
              group_by(date) %>%
              summarize(new_mw_phs_1 = sum(new_mw_phs_1)))
  
  ##compare panel grouped by month with monthly panel repliicated and divided to create "mean generator" dataset
  all.equal(df_renewable_panel %>%
              group_by(date) %>%
              summarize(new_mw_phs_1 = sum(new_mw_phs_1)),
            
            df_renewable_monthly %>% slice(rep(row_number(), length(unique(df_renewable$plant_gen_id)))) %>%
              mutate(new_mw_phs_1 = new_mw_phs_1/length(unique(df_renewable$plant_gen_id))) %>%
              group_by(date) %>%
              summarize(new_mw_phs_1 = sum(new_mw_phs_1)))
  
  df_renewable_panel %>%
    group_by(plant_gen_id) %>%
    tally() %>%
    arrange(desc(n))
  
  df_tests
  df_tests %>%  filter(energy_source_code_group == 'Renewables') %>%
    left_join(df_renewable_panel %>% select(date, plant_gen_id, energy_source_code_group, status_phase, new_mw_phs_1), by=c('plant_gen_id', 'date')) %>%
    write_csv('../Data/Regressions/unit_tests/renewable_panel_mw_comparision.csv')
  
  all.equal(df_tests %>% filter(status_phase == 7, date > '2015-07-01', energy_source_code_group == 'Renewables') %>% select(date, status_phase, net_summer_capacity_mw),
            df_renewable_panel %>% filter(status_phase == 7, date > '2015-07-01', plant_gen_id %in% c(unique(df_tests$plant_gen_id))) %>% select(date, status_phase, new_mw_phs_1))
  
  
  formula <- 'new_mw_phs_1 ~ year_2022 + year_2021 + year_2020 + year_2019 + 
    year_2018 + year_2017 + year_2016 + year_2015 + month_12 + 
    month_11 + month_10 + month_9 + month_8 + month_7 + month_6 + 
    month_5 + month_3 + month_2 + month_1 + roll_temp_midpoint + 
    p_labf + p_uner + cpi + mean_shortr + lcoe_wind_dollars_mw + 
    lcoe_sun_dollars_mw + mean_hh_price_MMBtu + mean_int_tot_spin_cap_mw + 
    mean_rtoffcap + mean_total_gen_rtcap_ratio + mean_net_sys_lamda + 
    mean_hb_busavg_energy_only + mean_rtorpa + mean_rtordpa + 
    mean_pnm + mean_ng_cap_gini_coef + mean_wnd_cap_gini_coef + 
    strat_pool_total_mw_Renewables + strat_pool_total_mw_NG + 
    strat_pool_mean_status_phase_Renewables + strat_pool_mean_status_phase_NG'
  
  # formula <- 'new_mw_phs_1 ~ year_2022 + year_2021 + year_2020 + year_2019 +
  #     year_2018 + year_2017 + year_2016 + year_2015 + month_12 +
  #     month_11 + month_10 + month_9 + month_8 + month_7 + month_6 +
  #     month_5 + month_3 + month_2 + month_1 + roll_temp_midpoint + 
  #     p_labf + p_uner + cpi + mean_shortr + lcoe_wind_dollars_mw + 
  #     lcoe_sun_dollars_mw + mean_hh_price_MMBtu + mean_int_tot_spin_cap_mw + 
  #     mean_rtoffcap + mean_total_gen_rtcap_ratio + mean_net_sys_lamda + 
  #     mean_hb_busavg_energy_only + mean_rtorpa + mean_rtordpa + 
  #     mean_pnm + mean_ng_cap_gini_coef + mean_wnd_cap_gini_coef '
  # 
  # formula <- 'new_mw_phs_1 ~ year_2022 + year_2021 + year_2020 + year_2019 +
  #     year_2018 + year_2017 + year_2016 + year_2015 + month_12 +
  #     month_11 + month_10 + month_9 + month_8 + month_7 + month_6 +
  #     month_5 + month_3 + month_2 + month_1 + roll_temp_midpoint + 
  #     p_labf + p_uner + cpi + mean_shortr + lcoe_wind_dollars_mw + 
  #     lcoe_sun_dollars_mw + mean_hh_price_MMBtu + mean_int_tot_spin_cap_mw + 
  #     mean_rtoffcap + mean_total_gen_rtcap_ratio + mean_net_sys_lamda + 
  #     mean_hb_busavg_energy_only'
  # 
  # formula <- 'new_mw_phs_1 ~ year_2022 + year_2021 + year_2020 + year_2019 +
  #     year_2018 + year_2017 + year_2016 + year_2015 + month_12 +
  #     month_11 + month_10 + month_9 + month_8 + month_7 + month_6 +
  #     month_5 + month_3 + month_2 + month_1 + roll_temp_midpoint + 
  #     p_labf + p_uner + cpi + mean_shortr + lcoe_wind_dollars_mw + 
  #     lcoe_sun_dollars_mw + mean_hh_price_MMBtu'
  # 
  # formula <- 'new_mw_phs_1 ~ year_2022 + year_2021 + year_2020 + year_2019 +
  #     year_2018 + year_2017 + year_2016 + year_2015 + month_12 +
  #     month_11 + month_10 + month_9 + month_8 + month_7 + month_6 +
  #     month_5 + month_3 + month_2 + month_1'
  # 
  
  fit_1 <- lm(formula,
              data=df_ng_monthly %>% slice(rep(row_number(), length(unique(df_ng$plant_gen_id)))) %>%
                mutate(new_mw_phs_1 = new_mw_phs_1/length(unique(df_ng$plant_gen_id))))
  
  fit_2 <- lm(formula,
              data=df_ng_monthly)
  
  fit_3 <- lm(formula,
              data=df_ng_panel)
  
  print(formula)
  fit_4 <- tobit(new_mw_phs_1 ~ year_2022 + year_2021 + year_2020 + year_2019 + 
                   year_2018 + year_2017 + year_2016 + year_2015 + month_12 + 
                   month_11 + month_10 + month_9 + month_8 + month_7 + month_6 + 
                   month_5 + month_3 + month_2 + month_1 + roll_temp_midpoint + 
                   p_labf + p_uner + cpi + mean_shortr + lcoe_wind_dollars_mw + 
                   lcoe_sun_dollars_mw + mean_hh_price_MMBtu + mean_int_tot_spin_cap_mw + 
                   mean_rtoffcap + mean_total_gen_rtcap_ratio + mean_net_sys_lamda + 
                   mean_hb_busavg_energy_only + mean_rtorpa + mean_rtordpa + 
                   mean_pnm + mean_ng_cap_gini_coef + mean_wnd_cap_gini_coef + 
                   strat_pool_total_mw_Renewables + strat_pool_total_mw_NG + 
                   strat_pool_mean_status_phase_Renewables + strat_pool_mean_status_phase_NG,
                 left = 0,
                 data = df_renewable_panel)
  
  fit_4
  summary(fit_1)
  summary(fit_2)
  summary(fit_3)
  summary(fit_4)
  
  # stargazer(fit_1, out='../Images/Regressions/ng_monthly_repeated_by_distinct_generator_count.html',
  #           star.cutoffs = c(0.05, 0.01, 0.001))
  # stargazer(fit_2, out='../Images/Regressions/ng_monthly.html',
  #         star.cutoffs = c(0.05, 0.01, 0.001))
  # 
  # stargazer(fit_3, out='../Images/Regressions/ng_panel.html',
  #         star.cutoffs = c(0.05, 0.01, 0.001))
  
  
  formula <- 'new_mw_phs_1 ~ year_2022 + year_2021 + year_2020 + year_2019 + 
    year_2018 + year_2017 + year_2016 + year_2015 + month_12 + 
    month_11 + month_10 + month_9 + month_8 + month_7 + month_6 + 
    month_5 + month_3 + month_2 + month_1 + roll_temp_midpoint + 
    p_labf + p_uner + cpi + mean_shortr + lcoe_wind_dollars_mw + 
    lcoe_sun_dollars_mw + mean_hh_price_MMBtu + mean_int_tot_spin_cap_mw + 
    mean_rtoffcap + mean_total_gen_rtcap_ratio + mean_net_sys_lamda + 
    mean_hb_busavg_energy_only + mean_rtorpa + mean_rtordpa + 
    mean_pnm + mean_ng_cap_gini_coef + mean_wnd_cap_gini_coef + 
    strat_pool_total_mw_Renewables + strat_pool_total_mw_NG + 
    strat_pool_mean_status_phase_Renewables + strat_pool_mean_status_phase_NG'
  
  # formula <- 'new_mw_phs_1 ~ year_2022 + year_2021 + year_2020 + year_2019 +
  #     year_2018 + year_2017 + year_2016 + year_2015 + month_12 +
  #     month_11 + month_10 + month_9 + month_8 + month_7 + month_6 +
  #     month_5 + month_3 + month_2 + month_1 + roll_temp_midpoint + 
  #     p_labf + p_uner + cpi + mean_shortr + lcoe_wind_dollars_mw + 
  #     lcoe_sun_dollars_mw + mean_hh_price_MMBtu + mean_int_tot_spin_cap_mw + 
  #     mean_rtoffcap + mean_total_gen_rtcap_ratio + mean_net_sys_lamda + 
  #     mean_hb_busavg_energy_only + mean_rtorpa + mean_rtordpa + 
  #     mean_pnm + mean_ng_cap_gini_coef + mean_wnd_cap_gini_coef '
  # 
  # formula <- 'new_mw_phs_1 ~ year_2022 + year_2021 + year_2020 + year_2019 +
  #     year_2018 + year_2017 + year_2016 + year_2015 + month_12 +
  #     month_11 + month_10 + month_9 + month_8 + month_7 + month_6 +
  #     month_5 + month_3 + month_2 + month_1 + roll_temp_midpoint + 
  #     p_labf + p_uner + cpi + mean_shortr + lcoe_wind_dollars_mw + 
  #     lcoe_sun_dollars_mw + mean_hh_price_MMBtu + mean_int_tot_spin_cap_mw + 
  #     mean_rtoffcap + mean_total_gen_rtcap_ratio + mean_net_sys_lamda + 
  #     mean_hb_busavg_energy_only'
  
  # formula <- 'new_mw_phs_1 ~ year_2022 + year_2021 + year_2020 + year_2019 +
  #     year_2018 + year_2017 + year_2016 + year_2015 + month_12 +
  #     month_11 + month_10 + month_9 + month_8 + month_7 + month_6 +
  #     month_5 + month_3 + month_2 + month_1 + roll_temp_midpoint + 
  #     p_labf + p_uner + cpi + mean_shortr + lcoe_wind_dollars_mw + 
  #     lcoe_sun_dollars_mw + mean_hh_price_MMBtu'
  
  # formula <- 'new_mw_phs_1 ~ year_2022 + year_2021 + year_2020 + year_2019 +
  #     year_2018 + year_2017 + year_2016 + year_2015 + month_12 +
  #     month_11 + month_10 + month_9 + month_8 + month_7 + month_6 +
  #     month_5 + month_3 + month_2 + month_1'
  # 
  # formula <- 'new_mw_phs_1 ~ year_2022 + year_2021 + year_2020 + year_2019 +
  #     year_2018 + year_2017 + year_2016 + year_2015'
  
  
  fit_4 <- lm(formula,
              data=df_renewable_monthly %>% slice(rep(row_number(), length(unique(df_renewable$plant_gen_id)))) %>%
                mutate(new_mw_phs_1 = new_mw_phs_1/length(unique(df_renewable$plant_gen_id))))
  
  fit_5 <- lm(formula,
              data=df_renewable_monthly)
  
  fit_6 <- lm(formula,
              data=df_renewable_panel)
  
  # summary(fit_4)
  summary(fit_5)
  # summary(fit_6)
  
  # stargazer(fit_4, out='../Images/Regressions/renewable_monthly_repeated_by_distinct_generator_count.html',
  #           star.cutoffs = c(0.05, 0.01, 0.001))
  # stargazer(fit_5, out='../Images/Regressions/renewable_monthly.html',
  #         star.cutoffs = c(0.05, 0.01, 0.001))
  # 
  # stargazer(fit_6, out='../Images/Regressions/renewable_panel.html',
  #         star.cutoffs = c(0.05, 0.01, 0.001))
  df_renewable_panel %>% write_csv('../Data/Regressions/pre_model_data/test_renewable_panel_12mo_lag_3mo_roll.csv')
  df_ng_panel %>% write_csv('../Data/Regressions/pre_model_data/test_ng_panel_12mo_lag_3mo_roll.csv')
  
  df_ng_panel %>%
    filter(date = '2017-01-01') %>%
    groupby(date) %>%
    summarize(total_mw = sum(new_mw_phs_entrants))
  
}

capture_output_for_variable_lags_test <- function(fit, fit2, i, segment, covar_version, run_polynomial_weather, lag_months){
  
  df_ols_model_statistics <- glance(fit2)
  df_rls_model_statistics <- glance(fit)
  
  ### ols stats
  print('caputring ols stats')
  n_ols = nobs(fit2)
  r_sq_ols = df_ols_model_statistics$r.squared
  adj_r_sq_ols = df_ols_model_statistics$adj.r.squared
  f_stat_ols = df_ols_model_statistics$statistic
  aic_ols = df_ols_model_statistics$AIC
  bic_ols = df_ols_model_statistics$BIC
  fpe_ols = ols_fpe(fit2)
  
  ### rls stats
  print('caputring rls stats')
  n_rls = nobs(fit)
  r_sq_rls = df_rls_model_statistics$r.squared
  adj_r_sq_rls = df_rls_model_statistics$adj.r.squared
  f_stat_rls = df_rls_model_statistics$statistic
  aic_rls = df_rls_model_statistics$AIC
  bic_rls = df_rls_model_statistics$BIC
  fpe_rls = ols_fpe(fit)
  
  #get model statistics
  # output[i,] <- c(segment,months_to_lag[i],r_sq,adj_r_sq,f_stat,aic,bic,fpe)
  
  #get ols adder coefficients
  pa_coefs_ols <- tidy(fit2) %>% filter(term == 'mean_total_pa')
  #get rls adder coefficients
  pa_coefs_rls <- tidy(fit) %>% filter(term == 'mean_total_pa')
  
  if (segment == 'ng' & covar_version == 'base_covars' & run_polynomial_weather== FALSE & lag_months==0) {
    df_output <-data.frame(covar_version=covar_version,
                           polynomial_weather=run_polynomial_weather,
                           segment = segment,
                           lag_months_model = lag_months,
                           n_ols=n_ols,
                           r_sq_ols=r_sq_ols,
                           adj_r_sq_ols=adj_r_sq_ols,
                           f_stat_ols=f_stat_ols,
                           aic_ols=aic_ols,
                           bic_ols=bic_ols,
                           fpe_ols=fpe_ols,
                           estimate_mean_total_pa_ols = pa_coefs_ols[1,'estimate'] %>% pull(estimate),
                           p_val_mean_total_pa_ols = pa_coefs_ols[1,'p.value'] %>% pull(p.value),
                           n_rls=n_rls,
                           r_sq_rls=r_sq_rls,
                           adj_r_sq_rls=adj_r_sq_rls,
                           f_stat_rls=f_stat_rls,
                           aic_rls=aic_rls,
                           bic_rls=bic_rls,
                           fpe_rls=fpe_rls,
                           estimate_mean_total_pa_rls = pa_coefs_rls[1,'estimate'] %>% pull(estimate),
                           p_val_mean_total_pa = pa_coefs_rls[1,'p.value'] %>% pull(p.value)
                          )
    
    
    write_csv(df_output, '../Data/Regressions/lag_model_statistics_roll_monthly_data.csv')
    
  } ### end print block for first iteration of model 
  else {
    row <-data.frame(covar_version,run_polynomial_weather,segment,lag_months,n_ols,r_sq_ols,adj_r_sq_ols,f_stat_ols,aic_ols,bic_ols,fpe_ols,
                     pa_coefs_ols[1,'estimate'] %>% pull(estimate), pa_coefs_ols[1,'p.value']  %>% pull(p.value),
                     n_rls,r_sq_rls,adj_r_sq_rls,f_stat_rls,aic_rls,bic_rls,fpe_rls,
                     pa_coefs_rls[1,'estimate'] %>% pull(estimate), pa_coefs_rls[1,'p.value']  %>% pull(p.value)
    )
    
    # writing row in the csv file 
    write.table(row, file = '../Data/Regressions/lag_model_statistics_roll_monthly_data.csv', sep = ",", 
                append = TRUE, quote = FALSE, 
                col.names = FALSE, row.names = FALSE)
    
  } ## end print block for non-first iteration
  
  
} ## end of function to capture output for variable lags test

create_variable_lags_appendix_summary_and_figure <- function(){
  df_lags <- read_csv('../Data/Regressions/lag_model_statistics_roll_monthly_data_1pa_0to36.csv')
  
  
  df_lags_review <- df_lags %>% mutate(is_significant_95 = ifelse(p_val_mean_total_pa_ols <=0.05,1,0),
                                       is_significant_99 = ifelse(p_val_mean_total_pa_ols <=0.01,1,0),
                                       is_significant_999 = ifelse(p_val_mean_total_pa_ols <=0.001,1,0),
                                       is_coef_positive = ifelse(estimate_mean_total_pa_ols >0,1,0))
  
  df_results_summary <- df_lags_review %>%
    group_by(segment) %>%
    summarize(n_model = n(),
              n_significant_95 = sum(is_significant_95),
              n_pos_significant_95 = sum(is_significant_95[is_coef_positive == 1]),
              n_significant_99 = sum(is_significant_99),
              n_pos_significant_99 = sum(is_significant_99[is_coef_positive == 1]),
              n_significant_999 = sum(is_significant_999),
              n_pos_significant_999 = sum(is_significant_999[is_coef_positive == 1]),
              share_sig_95 = n_significant_95/n_model,
              share_pos_sig_95 = n_pos_significant_95/n_model,
              share_neg_sig_95 = (n_significant_95-n_pos_significant_95)/n_model
    )
  print(df_results_summary)
  
  df_lags %>%
    mutate(segment = if_else(segment == 'ng', 'Natural Gas', 'Renewables'),
           hist_value = if_else(estimate_mean_total_pa_ols < 0, -1*p_val_mean_total_pa_ols, p_val_mean_total_pa_ols)) %>%
    ggplot(aes(x=hist_value)) +
    geom_histogram(aes(y=after_stat(density)), binwidth =.01, color='black', fill='white') +
    geom_vline(aes(xintercept = -.05, color='95%', linetype='95%')) +
    geom_vline(aes(xintercept = .05, color='95%', linetype='95%')) +
    geom_vline(aes(xintercept = -.01, color='99%', linetype='99%')) +
    geom_vline(aes(xintercept = .01, color='99%', linetype='99%')) +
    scale_color_manual(values = c("95%" = "blue", "99%" = "blue"), name='Confidence') +
    scale_linetype_manual(values = c("95%" = "solid", "99%" = "dashed"), name='Confidence') +
    geom_density(alpha=.2, fill="#FF6666") +
    xlab('p-value') +
    ylab('Density')+
    facet_wrap(~segment, ncol = 1) +
    theme(strip.text = element_text(
      size = 20),
      axis.title=element_text(size=16),
      legend.title=element_text(size=16),
      legend.text=element_text(size=14))
  ggsave('../Images/Figures/variable_lags_hist.png', width= 15)
}### end of variable_lags appendix figure