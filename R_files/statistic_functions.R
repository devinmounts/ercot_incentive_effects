#what is an exit, does it include out of service status types?
create_stats_on_standby_movement <- function() {
  df_plant_gen_id_phase_timeline <- read_csv('../Data/EIA Compiled Data/EIA860M_timeline.csv')
  
  standby_plants <- df_plant_gen_id_phase_timeline %>%
    filter(status_phase %in% c(9,10,11)) %>%
    distinct(plant_gen_id) %>%
    pull(unique(plant_gen_id))
  
  
  df_standby_mover_summary <- df_plant_gen_id_phase_timeline %>%
    filter(plant_gen_id %in% standby_plants) %>%
    group_by(plant_gen_id) %>%
    arrange(date) %>%
    mutate(lead_status_phase = lead(status_phase, n=1)) %>%
    relocate(lead_status_phase, .before = status_phase) %>%
    ungroup() %>%
    mutate(standby_move_type = case_when((lead_status_phase == 13 & status_phase == 9) ~ 'short_standby_to_retire',
                                         (lead_status_phase == 13 & status_phase == 10) ~ 'long_standby_to_retire',
                                         (lead_status_phase == 13 & status_phase == 11) ~ 'other_to_retire',
                                         (lead_status_phase == 7 & status_phase == 9) ~ 'short_standby_to_operating',
                                         (lead_status_phase == 7 & status_phase == 10) ~ 'long_standby_to_operating',
                                         (lead_status_phase == 7 & status_phase == 11) ~ 'other_to_operating')) %>%
    relocate(standby_move_type) %>%
    filter(!is.na(standby_move_type)) %>%
    arrange(plant_gen_id, date) %>%
    select(standby_move_type, plant_gen_id, energy_source_code_group, net_summer_capacity_mw, status_phase, lead_status_phase, weeks_in_phase) %>%
    group_by(standby_move_type, energy_source_code_group) %>%
    summarize(n = n(),
              unique_plant_gen_id = length(unique(plant_gen_id)),
              mean_weeks_before_move = mean(weeks_in_phase, na.rm=T),
              status_phase = unique(status_phase, na.rm=T),
              lead_status_phase = unique(lead_status_phase, na.rm=T)) %>%
    arrange(standby_move_type)
  
  share_short_standby_return_operating <- df_standby_mover_summary %>% filter(status_phase == 9 & lead_status_phase != 13) %>% pull(unique_plant_gen_id) %>% sum() / df_standby_mover_summary %>% filter(status_phase == 9) %>% pull(unique_plant_gen_id) %>% sum()
  share_long_standby_return_operating <- df_standby_mover_summary %>% filter(status_phase == 10 & lead_status_phase != 13) %>% pull(unique_plant_gen_id) %>% sum() / df_standby_mover_summary %>% filter(status_phase == 10) %>% pull(unique_plant_gen_id) %>% sum()
  share_other_return_operating <- df_standby_mover_summary %>% filter(status_phase == 11 & lead_status_phase != 13) %>% pull(unique_plant_gen_id) %>% sum() / df_standby_mover_summary %>% filter(status_phase == 11) %>% pull(unique_plant_gen_id) %>% sum()
  
  df_standby_mover_summary
  return(paste(round(share_short_standby_return_operating,2)*100, '% of short standby plants return to operating', 'and', round(share_long_standby_return_operating,2)*100, '% of short standby plants return to operating', 'and', round(share_other_return_operating,2)*100,'% of other plants return to operating'))
}

create_operating_pool_stats_latex <- function() {
  df_plant_gen_id_phase_timeline <- read_csv('../Data/EIA Compiled Data/EIA860M_timeline.csv')
  
  operating_plants_during_period <- df_plant_gen_id_phase_timeline %>% filter(status_phase %in% c(7,8,9)) %>% pull(unique(plant_gen_id))
  
  df_mover_categories <- df_plant_gen_id_phase_timeline %>%
    mutate(mover_category = case_when((recorded_first_operation_date > min(date) & last_appearance_phase %in% c(7,8,9))~'entrant',
                                      (recorded_first_operation_date > min(date) & last_appearance_phase > 9)~'entrant_exit',
                                      (recorded_first_operation_date <= min(date) & first_appearance_phase %in% c(7,8,9,10) & last_appearance_phase > 9)~'existing_exit',
                                      (recorded_first_operation_date <= min(date) & first_appearance_phase %in% c(7,8,9,10) & last_appearance_phase %in% c(7,8,9))~'existing_persists',
                                      (is.na(recorded_first_operation_date) & (first_appearance_phase < 7) | (first_appearance_phase == 12) & (last_appearance_phase %in% c(11,12)) | plant_gen_id == '58478_LEPA1')~'cancelled_postponed',
                                      is.na(recorded_first_operation_date) & last_appearance_phase < 7~'planned',
                                      first_appearance_phase == 13~'historically_retired',
                                      TRUE~'other')) %>%
    mutate(is_initial_generator = if_else(mover_category %in% c('existing_persists', 'existing_exit'), 1,0),
           is_entrant_generator = if_else(mover_category %in% c('entrant', 'entrant_exit'), 1,0),
           is_exit_generator = if_else(mover_category %in% c('existing_exit', 'entrant_exit'), 1,0),
           is_remaining_generator = if_else(mover_category %in% c('existing_persists', 'entrant'), 1,0)) %>%
    distinct(plant_gen_id, mover_category, recorded_first_operation_date, first_appearance, first_appearance_phase, last_appearance, last_appearance_phase, is_initial_generator, is_entrant_generator, is_exit_generator, is_remaining_generator) %>%
    left_join(df_plant_gen_id_phase_timeline %>% filter(date == first_appearance) %>%  select(plant_gen_id, energy_source_code_group, net_summer_capacity_mw, energy_source_code, weeks_since_first_appearance), by = 'plant_gen_id') #%>%
  
  write_csv(df_mover_categories, '../Data/EIA Compiled Data/EIA860M_generators_w_mover_categories.csv')
  
  #operating pool statistics
  df_summary_operation <- data_frame()
  seq_keys <- c('is_initial_generator', 'is_entrant_generator','is_exit_generator','is_remaining_generator')
  seq_category_names <- c('Initial Period', 'Entrants','Exits','Final Period')
  for (i in seq_along(seq_keys)) {
    filter_key <- seq_keys[i]
    category_name <- seq_category_names[i]
    print(filter_key)
    print(category_name)
    
    print('period stats')
    df_period_stats <- df_mover_categories %>%
      filter(!!sym(filter_key) == 1) %>%
      mutate(market_generators = length(unique(plant_gen_id, na.rm=T)),
             market_total_mw = sum(net_summer_capacity_mw, na.rm = T),
             market_mean_mw = mean(net_summer_capacity_mw, na.rm=T)
      ) %>%
      group_by(energy_source_code_group, market_generators, market_total_mw, market_mean_mw) %>%
      summarize(n_generators = length(unique(plant_gen_id)),
                mean_mw = mean(net_summer_capacity_mw, na.rm=T),
                total_mw = sum(net_summer_capacity_mw, na.rm=T),
                share_generators = n_generators/market_generators,
                share_mw = total_mw/market_total_mw,
                sequence_category = category_name) %>%
      ungroup() %>%
      distinct() %>%
      select(energy_source_code_group, n_generators:sequence_category) %>%
      ungroup() %>%
      pivot_longer(n_generators:share_mw, names_to = 'statistic') %>%
      arrange(match(statistic, c("n_generators", "share_generators", "mean_mw", 'total_mw','share_mw'))) %>%
      pivot_wider(id_cols = c(sequence_category:statistic), names_from = energy_source_code_group)
    
    print('total period stats')
    df_total_period_stats <- df_mover_categories %>%
      filter(!!sym(filter_key) == 1) %>%
      mutate(n_generators = length(unique(plant_gen_id)),
             mean_mw = mean(net_summer_capacity_mw, na.rm=T),
             total_mw = sum(net_summer_capacity_mw, na.rm=T),
             share_generators = n_generators/n_generators,
             share_mw = total_mw/total_mw,
             sequence_category = category_name,
             energy_source_code_group = 'Total'
      ) %>%
      distinct(n_generators, mean_mw, total_mw, share_generators, share_mw, sequence_category, energy_source_code_group) %>%
      pivot_longer(n_generators:share_mw, names_to = 'statistic') %>%
      arrange(match(statistic, c("n_generators", "share_generators", "mean_mw", 'total_mw','share_mw'))) %>%
      pivot_wider(id_cols = c(sequence_category:statistic), names_from = energy_source_code_group)
    
    # print(df_total_period_stats)
    
    df_period_stats <- df_period_stats %>% left_join(df_total_period_stats, by=c('statistic', 'sequence_category'))
    
    df_summary_operation <- rbind(df_summary_operation, df_period_stats)
  }
  # df_summary_operation$Total <- df_summary_operation$NG + df_summary_operation$Other + df_summary_operation$Renewables
  
  df_summary_operation <- df_summary_operation %>%
    mutate_if(is.numeric, round, digits=2) %>%
    mutate_if(is.numeric, as.character) %>%
    relocate(Renewables, .before = Other)
  
  # # #formatting for table
  for (row in which(grepl('share_mw', df_summary_operation$statistic))) {
    df_summary_operation[row,]$statistic <- 'Share Total MW'
    df_summary_operation[row,]$NG <- paste('(', as.character(df_summary_operation[row,]$NG), ')' )
    df_summary_operation[row,]$Renewables <- paste('(', as.character(df_summary_operation[row,]$Renewables), ')' )
    df_summary_operation[row,]$Other <- paste('(', as.character(df_summary_operation[row,]$Other), ')' )
    df_summary_operation[row,]$Total <- paste('(', as.character(df_summary_operation[row,]$Total), ')' )
  }
  
  for (row in which(grepl('share_gen', df_summary_operation$statistic))) {
    df_summary_operation[row,]$statistic <- 'Share Total Units'
    df_summary_operation[row,]$NG <- paste('(', as.character(df_summary_operation[row,]$NG), ')' )
    df_summary_operation[row,]$Renewables <- paste('(', as.character(df_summary_operation[row,]$Renewables), ')' )
    df_summary_operation[row,]$Other <- paste('(', as.character(df_summary_operation[row,]$Other), ')' )
    df_summary_operation[row,]$Total <- paste('(', as.character(df_summary_operation[row,]$Total), ')' )
  }
  #
  for (row in which(grepl('total_mw', df_summary_operation$statistic))) {
    df_summary_operation[row,]$statistic <- 'Total MW'
  }
  
  
  for (row in which(grepl('mean_mw', df_summary_operation$statistic))) {
    df_summary_operation[row,]$statistic <- 'Mean MW'
  }
  
  for (row in which(grepl('n_generators', df_summary_operation$statistic))) {
    df_summary_operation[row,]$statistic <- 'Generating Units'
  }
  
  
  stargazer(df_summary_operation,
            summary = F,
            title='Generator Summary Statistics',
            header = F,
            digits=2,
            rownames = F, 
            notes = 'Values in parenthesis denote share of total',
            font.size = 'small',
            out ='../Tables/Summary Stats/operating_generators_summary_statistics.tex')
  
  # stargazer(df_summary_operation, summary = F, title='Generator Summary Statistics', header = F, digits=2, rownames = F, notes = 'Values in parenthesis denote share of total', font.size = 'small',
  #           out = '../Tables/Summary Stats/generators_entry_exit_summary_statistics.tex')
}

time_to_operation <- function() {
  df_movers <- read_csv('../Data/EIA Compiled Data/EIA860M_generators_w_mover_categories.csv')
  df_time_to_operation <- df_movers %>%
    filter(grepl('entrant', mover_category) & first_appearance_phase %in% c(1)) %>%
    mutate(time_to_operation = recorded_first_operation_date - first_appearance) %>%
    group_by(energy_source_code_group) %>%
    summarize(time_to_operation = mean(time_to_operation),
              obs = length(unique(plant_gen_id))) 
  
  return(df_time_to_operation)
}

calculate_cancelled_capacity <- function() {
  
  df_plant_gen_id_phase_timeline <- read_csv('../Data/EIA Compiled Data/EIA860M_timeline.csv')
  
  df_plant_gen_id_phase_timeline %>%
    filter(first_appearance_phase < 7, date==last_appearance) %>%
    group_by(energy_source_code_group) %>%
    summarize(total_planned_generators = length(unique(plant_gen_id)),
              total_cancelled_generators = length(unique(plant_gen_id[status_phase == 12])),
              total_planned_mw = sum(net_summer_capacity_mw, na.rm = T),
              total_cancelled_mw = sum(net_summer_capacity_mw[status_phase == 12], na.rm = T),
              total_planned_generators_to_operation = length(unique(plant_gen_id[!is.na(recorded_first_operation_date)])),
              total_planned_mw_to_operation = sum(net_summer_capacity_mw[!is.na(recorded_first_operation_date)], na.rm = T),
              total_planned_generators_not_to_operation = length(unique(plant_gen_id[is.na(recorded_first_operation_date)])),
              total_planned_mw_not_to_operation = sum(net_summer_capacity_mw[is.na(recorded_first_operation_date)], na.rm = T),
              
    ) %>%
    mutate(ratio_gen_units_to_operation = total_planned_generators_to_operation/total_planned_generators,
           ratio_gen_units_not_to_operation = total_planned_generators_not_to_operation/total_planned_generators,
           ratio_gen_units_to_cancelled = total_cancelled_generators/total_planned_generators,
           ratio_mw_to_operation = total_planned_mw_to_operation/total_planned_mw,
           ratio_mw_not_to_operation = total_planned_mw_not_to_operation/total_planned_mw,
           ratio_mw_to_cancelled = total_cancelled_mw/total_planned_mw
    )
}

create_regression_summary_statistics <- function(){
  
  df_regression_data <- read_csv('../Data/Regressions/pre_model_data/ng_panel_totalquantity_12mo_lag.csv') 
  select_covariates <- c('new_mw_phs_entrants','roll_temp_midpoint', 'p_labf', 'p_uner', 'cpi','mean_shortr','lcoe_wind_dollars_mw','lcoe_sun_dollars_mw','mean_hh_price_MMBtu','mean_int_tot_spin_cap_mw', 'mean_rtoffcap', 'mean_rtoffpa', 'mean_hb_busavg_energy_only',  'mean_counterfeit_pa', 'mean_rtordpa', 'mean_pnm', 'mean_ng_cap_gini_coef', 'mean_wnd_cap_gini_coef', 'strat_pool_total_mw_Renewables', 'strat_pool_total_mw_NG', 'strat_pool_mean_status_phase_Renewables', 'strat_pool_mean_status_phase_NG')
  final_covariate_names <- c('MW Operating - NG','Mean Temp. Midpoint - Yr. Roll', 'Labor Force Pop. (Millions)', 'Unemp. Rate', 'CPI', 'Interest Rate', 'Cost Wind Installation - $/MWh',
                             'Cost PV Installation - $/MWh', 'Natural Gas Price - $/MMBtu', 'Online Capacity - GW', 'Offline Capacity - GW', 'Offline Reserves Incentive - $/MW',
                             'Energy Only Price - $/MW', 'Incentive Payment - $/MW', 'Compensation Adder - $/MW', 'Peaker Net Margin - $/GW', 'NG GINI Index', 'Wind GINI Index',
                             'Renewable Appl. Pool - MW', 'NG Appl. Pool - MW ', 'Mean Status Phase - Renewables', 'Mean Status Phase - NG ')
  
  stargazer(as.data.frame(df_regression_data) %>% filter(date > '2016-06-01') %>%
              select(select_covariates), type='latex', digits=2, omit.summary.stat = c("N", "p25", "p75"),
            covariate.labels = final_covariate_names,
            out = '../Images/Summary Stats/generators_entry_exit_summary_statistics.csv')
}

calculate_stats_on_presencence_in_applicant_pool <- function(){
  
  df_plant_gen_id_phase_timeline <- read_csv('../Data/EIA Compiled Data/EIA860M_timeline.csv')
  
  df_stats_applicant_pool_presence <- df_plant_gen_id_phase_timeline %>%
    filter(recorded_first_operation_date > '2015-07-01') %>%
    mutate(arrives_in_applicant_pool = if_else(first_appearance_phase < 7, 1,0),
           arrives_in_app_pool_stage_one = if_else(first_appearance_phase == 1, 1,0)) %>%
    group_by(plant_gen_id) %>%
    mutate(unique_status_phases = length(unique(status_phase[status_phase <7]))) %>%
    ungroup() %>%
    filter(date == recorded_first_operation_date) %>%
    group_by(energy_source_code_group) %>%
    summarize(n_gen_id_in_app_pool = length(unique(plant_gen_id[arrives_in_applicant_pool == 1])),
              n_gen_id_not_in_app_pool = length(unique(plant_gen_id[arrives_in_applicant_pool == 0])),
              share_n_gen_in_app_pool = n_gen_id_in_app_pool/(n_gen_id_in_app_pool+n_gen_id_not_in_app_pool),
              sum_mw_in_app_pool = sum(net_summer_capacity_mw[arrives_in_applicant_pool == 1], na.rm = T),
              sum_mw_not_in_app_pool = sum(net_summer_capacity_mw[arrives_in_applicant_pool == 0], na.rm = T),
              share_mw_in_app_pool = sum_mw_in_app_pool/(sum_mw_in_app_pool+sum_mw_not_in_app_pool),
              mean_phases_in_app_pool = mean(unique_status_phases[arrives_in_applicant_pool == 1], na.rm = T),
              mean_phases_not_in_app_pool = mean(unique_status_phases[arrives_in_applicant_pool == 0], na.rm = T),
              sum_mw_arrive_in_phase_1 = sum(net_summer_capacity_mw[arrives_in_app_pool_stage_one == 1], na.rm = T),
              sum_mw_arrive_not_in_phase_1 = sum(net_summer_capacity_mw[arrives_in_applicant_pool == 1], na.rm = T),
              share_mw_arriving_in_phase_1 = sum_mw_arrive_in_phase_1/(sum_mw_arrive_in_phase_1+sum_mw_arrive_not_in_phase_1),
              
    ) %>%
    select(energy_source_code_group, share_n_gen_in_app_pool, share_mw_in_app_pool, mean_phases_in_app_pool, share_mw_arriving_in_phase_1)
  
  rm(df_plant_gen_id_phase_timeline)
  return(df_stats_applicant_pool_presence)
}
