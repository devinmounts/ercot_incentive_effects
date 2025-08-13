##### packages 
if (!require("pacman")) install.packages("pacman")
library(pacman)

pacman::p_load('tidyverse', 'ggplot2', 'readxl', 'janitor', 'lubridate', 'geojsonio', 'broom', 'sp', 'zoo', 'fastDummies', 'stargazer', 'RStata', 'suncalc',
               'cowplot', 'olsrr', 'sf', 'MESS', 'pryr', 'plyr')

run_polynomial_weather <- TRUE

#######################################################################
#######################################################################
################ RLS Underbidding Timeseries Model ####################
######################     ( Table 5 )   ##############################
#######################################################################

covar_version = 'base_covars'
df_timeseries_data <- read_csv( '../Data/ERCOT Compiled Data/underbidding_data_w_lags.csv')

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
  
  stargazer(models[[length(models)]],
            models[[length(models)-1]],
            title=paste("RLS Active Treament", sep = ' '), align=TRUE,
            omit.stat=c("LL","ser","f"), no.space=TRUE, header = F, report = 'vc*', float.env = 'sidewaystable', font.size = 'footnotesize' , type = 'latex', out = '../Tables/Regressions/underbidding/gsls_ols_ts_matching_poly_weather.tex',
            star.cutoffs = c(0.05, 0.01, 0.001), column.sep.width = "1pt",covariate.labels = final_covariate_names,
            digits = 2
  )
  
  
  ### save direct effects to csv
  write.csv(tidy(models[[length(models)]]), "../Tables/Regressions/underbidding/robustness/gsls_ols_ts_matching_poly_weather.csv")
  
  
  
} ### end poly weather print statements

print("Program Complete")



