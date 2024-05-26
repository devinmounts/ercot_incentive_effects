##figures displaying realtime capacity and generations
create_peak_daily_and_sunset_interval_realtime_cap_figures <- function() {
  
  print('reading ERCOT data')
  df_gen_cap <- read.csv('../Data/ERCOT Compiled Data/gen_cap_and_adder_interval.csv')
  
  print('finding sunset hours for dates')
  start_date = "20130101"; end_date = "20230101"
  dates <- seq(ymd(start_date),ymd(end_date), by = "days")
  
  df_lubbock_sun <- expand.grid(delivery_date = dates) %>% 
    mutate(sunrise = getSunlightTimes(dates,33.5846,-101.8456,tz = 'America/Chicago')$sunrise,
           sunset = getSunlightTimes(dates,33.5846,-101.8456,tz = 'America/Chicago')$sunset,
           sunset_hour = hour(sunset))
  
  df_sunset_averages <- df_lubbock_sun %>%
    mutate(season = case_when(month(delivery_date) %in% c(6,7,8)~'summer',
                              month(delivery_date) %in% c(12,1,2)~'winter',
                              TRUE~'other')) %>%
    group_by(season) %>%
    summarize(avg_sunset_hour = mean(sunset_hour))
  
  print(df_sunset_averages)
  
  print('finding daily peak interval in ERCOT data')
  df_peak_int_by_day <- df_gen_cap %>%
    mutate(date = as.Date(paste(year(date), month(date), day(date), sep='-'), format='%Y-%m-%d')) %>%
    group_by(date) %>%
    filter(int_tot_gen_m_wh == max(int_tot_gen_m_wh))
  
  df_peak_averages <- df_peak_int_by_day %>%
    mutate(season = case_when(month(date) %in% c(6,7,8)~'summer',
                              month(date) %in% c(12,1,2)~'winter',
                              TRUE~'other')) %>%
    group_by(season) %>%
    summarize(avg_peak_hour = mean(hour))
  
  print(df_peak_averages)
  
  print('finding daily sunset peak interval in ERCOT data')
  df_hour_of_sunset_peak_int_by_day <- df_gen_cap %>%
    mutate(delivery_date = as.Date(delivery_date)) %>%
    left_join(df_lubbock_sun %>% select(delivery_date, sunset, sunset_hour), by=c('delivery_date')) %>%
    mutate(is_hour_of_sunset = if_else((sunset_hour == hour), 1,0)) %>%
    filter(is_hour_of_sunset == 1) %>%
    mutate(date = as.Date(paste(year(date), month(date), day(date), sep='-'), format='%Y-%m-%d')) %>%
    group_by(date) %>%
    filter(int_tot_gen_m_wh == max(int_tot_gen_m_wh)) %>%
    mutate(energy_source_code_group = case_when(fuel %in% c('gas', 'gas-cc') ~ 'NG',
                                                fuel %in% c('wind', 'solar') ~ 'Renewables',
                                                TRUE ~ 'Other')) 
  
  print('creating peak day by month data')
  df_peak_day_by_month <- df_gen_cap %>%
    mutate(date = as.Date(paste(year(date), month(date), day(date), sep='-'), format='%Y-%m-%d')) %>%
    group_by(year(date), month(date)) %>%
    filter(date == date[int_tot_gen_m_wh == max(int_tot_gen_m_wh)]) %>%
    mutate(energy_source_code_group = case_when(fuel %in% c('gas', 'gas-cc') ~ 'NG',
                                                fuel %in% c('wind', 'solar') ~ 'Renewables',
                                                TRUE ~ 'Other'))
  
  df_peak_day_by_month <- df_peak_day_by_month %>%
    mutate(delivery_date = as.Date(delivery_date)) %>%
    left_join(df_lubbock_sun %>% select(delivery_date, sunset, sunset_hour), by=c('delivery_date'))
  
  rm(df_gen_cap)
  gc()
  
  create_peak_and_sunset_reserves_figure_smooth(df_peak_int_by_day,df_hour_of_sunset_peak_int_by_day)
  create_peak_day_generation_by_fuel_w_select_summer_and_winter(df_peak_day_by_month)
  
}

create_peak_and_sunset_reserves_figure_smooth <- function(df_peak_int_by_day, df_hour_of_sunset_peak_int_by_day){
  
  df_peak_reserves_gen_ratio <- df_peak_int_by_day %>%
    filter(date >= '2014-06-01') %>%
    mutate(peak_reserves = (rtolcap+rtoffcap),
           peak_reserves_plus_goal = (rtolcap+rtoffcap) + 2000,
           peak_reserves_to_gen = peak_reserves/int_tot_gen_m_wh,
           peak_reserves_to_gen_plus_goal = peak_reserves_plus_goal/int_tot_gen_m_wh,
           peak_prc_to_gen = prc/int_tot_gen_m_wh,
           peak_share_renewables = int_tot_gen_renewable/int_tot_gen_m_wh,
           peak_ratio_ng_renewables = int_tot_gen_renewable/int_tot_gen_gas) %>%
    select(date, peak_reserves, peak_reserves_plus_goal, int_tot_gen_m_wh, peak_reserves_to_gen, peak_reserves_to_gen_plus_goal) %>%
    distinct()
  
  df_sunset_reserves_gen_ratio <- df_hour_of_sunset_peak_int_by_day %>%
    filter(date >= '2014-06-01') %>%
    mutate(sunset_reserves = (rtolcap+rtoffcap),
           sunset_reserves_plus_goal = (rtolcap+rtoffcap) + 2000,
           sunset_reserves_to_gen = sunset_reserves/int_tot_gen_m_wh,
           sunset_reserves_to_gen_plus_goal = sunset_reserves_plus_goal/int_tot_gen_m_wh,
           sunset_prc_to_gen = prc/int_tot_gen_m_wh,
           sunset_share_renewables = int_tot_gen_renewable/int_tot_gen_m_wh,
           sunset_ratio_ng_renewables = int_tot_gen_renewable/int_tot_gen_gas) %>%
    select(date,sunset_reserves, sunset_reserves_plus_goal, int_tot_gen_m_wh, sunset_reserves_to_gen, sunset_reserves_to_gen_plus_goal) %>%
    distinct()
  
  print('Setting span for smoothing')
  span = .02
  print(span)
  df_peak_reserves_gen_ratio_join <- df_peak_reserves_gen_ratio %>%
    left_join(df_sunset_reserves_gen_ratio, by='date') %>% 
    select(date, peak_reserves, peak_reserves_plus_goal, int_tot_gen_m_wh.x, peak_reserves_to_gen, peak_reserves_to_gen_plus_goal, sunset_reserves, sunset_reserves_plus_goal, int_tot_gen_m_wh.y, sunset_reserves_to_gen, sunset_reserves_to_gen_plus_goal) %>%
    distinct()
  
  smooth_ratio_plot <- df_peak_reserves_gen_ratio_join %>%
    ggplot(aes(x=date)) +
    geom_smooth(aes(y=peak_reserves_to_gen, linetype='Peak Hour'), se=T, color='black',
                # method = "gam", 
                # formula = y ~ s(x, bs = "cs", fx = TRUE, k = 8)
    ) +
    geom_smooth(aes(y=sunset_reserves_to_gen, linetype='Sunset Hour'), se=T, color='black',
                # method = "gam", 
                # formula = y ~ s(x, bs = "cs", fx = TRUE, k = 8)
    ) +
    theme_bw() +
    ggtitle('Reserves to Generation Ratio') +
    theme(plot.title = element_text(hjust = 0.5, size = 22),
          legend.text = element_text(size = 26),
          legend.title = element_text(size = 24),
          axis.text.x = element_text(size = 26),
          axis.text.y = element_text(size = 26),
          axis.title = element_text(size = 28),
          legend.position = "bottom") +
    xlab('Date') +
    ylab("Ratio") +
    labs(linetype = "") #+
  # geom_hline(aes(yintercept = 1.1, linetype='Sunset Goal')) +
  # geom_hline(aes(yintercept = .89, linetype='Peak Goal'))
  
  
  #get_data_for_goal
  smooth_goal_ratio_plot <- df_peak_reserves_gen_ratio_join %>%
    ggplot(aes(x=date)) +
    geom_smooth(aes(y=peak_reserves_to_gen_plus_goal, linetype='Peak Hour'), se=T, color='black') +
    geom_smooth(aes(y=sunset_reserves_to_gen_plus_goal, linetype='Sunset Hour'), se=T, color='black' )
  
  
  df_smooth_goal_ratio_plot <- ggplot_build(smooth_goal_ratio_plot)
  peak_goal <- df_smooth_goal_ratio_plot$data[[1]]$y[1]
  sunset_goal <- df_smooth_goal_ratio_plot$data[[2]]$y[1]
  
  #plot dual line smooth figure
  smooth_ratio_plot <- smooth_ratio_plot +
    geom_hline(aes(yintercept = peak_goal, linetype='Peak Goal')) +
    geom_hline(aes(yintercept = sunset_goal, linetype='Sunset Goal'))
  
  # ggsave(plot =smooth_ratio_plot, '../Figures/reserves_to_gen_ratio_singular_smooth.png', width = 20,  height = 10)
  
  
  smooth_ratio_plot_peak_only <- df_peak_reserves_gen_ratio_join %>%
    ggplot(aes(x=date)) +
    geom_smooth(aes(y=peak_reserves_to_gen, linetype='Peak Hour'), se=T, color='black',
                # method = "gam", 
                # formula = y ~ s(x, bs = "cs", fx = TRUE, k = 8)
    ) +
    theme_bw() +
    ggtitle('') +
    xlab('Date') +
    ylab("Reserves to Gen. Ratio") +
    theme(plot.title = element_text(hjust = 0.5, size = 22),
          legend.text = element_text(size = 28),
          legend.title = element_text(size = 30),
          axis.text.x = element_text(size = 30),
          axis.text.y = element_text(size = 30),
          axis.title = element_text(size = 32),
          legend.position = "bottom") +
    labs(linetype = "") +
    geom_hline(aes(yintercept = peak_goal, linetype='2GW Goal'))
  
  # ggsave(plot =smooth_ratio_plot_peak_only, '../Figures/reserves_to_gen_ratio_singular_smooth_peak_only.png', width = 20,  height = 10)
  
  smooth_ratio_plot_sunset_only <- df_peak_reserves_gen_ratio_join %>%
    ggplot(aes(x=date)) +
    geom_smooth(aes(y=sunset_reserves_to_gen, linetype='Sunset Hour'), se=T, color='black',
                # method = "gam", 
                # formula = y ~ s(x, bs = "cs", fx = TRUE, k = 8)
    ) +
    theme_bw() +
    ggtitle('') +
    xlab('Date') +
    ylab("Reserves to Gen. Ratio") +
    theme(plot.title = element_text(hjust = 0.5, size = 22),
          legend.text = element_text(size = 28),
          legend.title = element_text(size = 30),
          axis.text.x = element_text(size = 30),
          axis.text.y = element_text(size = 30),
          axis.title = element_text(size = 32),
          legend.position = "bottom") +
    labs(linetype = "") +
    geom_hline(aes(yintercept = sunset_goal, linetype='2GW Goal')) +
    expand_limits(y=c(NA, sunset_goal+.1))
  
  # ggsave(plot =smooth_ratio_plot_sunset_only, '../Figures/reserves_to_gen_ratio_singular_smooth_sunset_only.png', width = 20,  height = 10)
  
  
  
  grid_smooth <- cowplot::plot_grid(
    cowplot::plot_grid(
      smooth_ratio_plot_peak_only + theme(legend.position = 'none') ,
      ncol=1,
      labels = c('A')
    ),
    cowplot::plot_grid(
      cowplot::get_legend(smooth_ratio_plot_peak_only + theme(legend.position = "bottom")),
      nrow=1),
    cowplot::plot_grid(
      smooth_ratio_plot_sunset_only + theme(legend.position = 'none'),
      ncol=1,
      labels = c('B')
    ),
    cowplot::plot_grid(cowplot::get_legend(smooth_ratio_plot_sunset_only + theme(legend.position = "bottom")),
                       nrow=1),
    nrow=2,
    ncol=1,
    rel_heights = c(.4,.1,.4,.1)
  )
  
  grid_smooth <- cowplot::plot_grid(
    smooth_ratio_plot_peak_only,
    smooth_ratio_plot_sunset_only,
    ncol=1,
    labels = c('Panel A - Peak Hour', 'Panel B - Sunset Hour'),
    label_size = 26, hjust = 0, vjust = 1
  )
  
  save_plot(plot=grid_smooth, '../Figures/reserves_to_gen_ratio_singular_smooth_grid.png',  base_height = 15)
  
} 

create_peak_day_generation_by_fuel_w_select_summer_and_winter <- function(df_peak_day_by_month){
  
  print('creating full peak generation day figure')
  df_peak_day_by_month %>%
    filter(year %in% c(2016, 2022)) %>%
    select(year, month, date, hour, minute, int_tot_gen_gas, int_tot_gen_renewable, int_tot_gen_other, int_tot_gen_m_wh, sunset_hour) %>%
    distinct() %>%
    ggplot(aes(x=hour)) +
    geom_line(aes(y=int_tot_gen_gas, color='gen_gas')) +
    geom_line(aes(y=int_tot_gen_renewable, color='gen_renew')) +
    geom_line(aes(y=int_tot_gen_m_wh, linetype='total_gen')) +
    facet_wrap(~month*year) +
    geom_vline(aes(xintercept = sunset_hour), linetype=3)
  ggsave('../Figures/peak_generation_day_resource_segment.png', width = 20,  height = 10)
  
  print('creating select peak generation day figure')
  df_peak_day_by_month %>%
    filter(year %in% c(2022), month %in% c(2,9)) %>%
    select(year, month, date, hour, minute, int_tot_gen_gas, int_tot_gen_renewable, int_tot_gen_other, int_tot_gen_m_wh, sunset_hour) %>%
    distinct() %>%
    mutate(month = if_else(month == 2, 'February', 'September')) %>%
    ggplot(aes(x=hour)) +
    geom_line(aes(y=int_tot_gen_gas, linetype='Gas Gen.')) +
    geom_line(aes(y=int_tot_gen_renewable, linetype='Renew. Gen')) +
    geom_line(aes(y=int_tot_gen_m_wh, linetype='Total Gen.')) +
    facet_wrap(~month) +
    geom_vline(aes(xintercept = sunset_hour, linetype='Sunset')) +
    theme_bw() +
    # ggtitle('Generation by Resource Segment on Peak Day of Month: 2022' ) +
    theme(plot.title = element_text(hjust = 0.5, size = 22),
          legend.text = element_text(size = 26),
          legend.title = element_text(size = 24),
          axis.text.y = element_text(size = 26),
          axis.text.x = element_text(size = 26),
          # axis.title.y = element_blank(),
          # axis.title.x = element_blank(),
          axis.title = element_text(size = 28),
          strip.text.x = element_text(size = 18),
          legend.position = "bottom") +
    ylab('Real-time Generation (MW)') +
    xlab('Hour of Day') +
    labs(linetype = "")
  ggsave('../Figures/peak_generation_day_resource_segment_select.png', width = 20,  height = 10)
  
 
}

plot_profit_margin_select_firms <- function(){
  df_profits = read_excel('../source_data/Generator Profits/sec_filings_ng.xlsx')
  df_annual_pas = read_csv('../Data/ERCOT Compiled Data/ercot_pa_annaul_sum.csv')
  
  print(paste('Annual mean of Incentive:', mean(df_annual_pas[df_annual_pas$year >=2015,]$annual_rtorpa)))
  print(paste('Annual mean of Incentive payments:$', mean(df_annual_pas[df_annual_pas$year >=2015,]$total_pa_payments_bill), 'billion'))
  
  df_profits_and_pa <- df_profits %>%
    filter(revenues >0) %>%
    left_join(df_annual_pas, by='year') %>%
    mutate(annual_rtorpa_millions = annual_rtorpa/1000000,
           annual_rtordpa_millions = annual_rtordpa/10000000) %>%
    rename(Firm = firm,
           Year = year)
  
  ylim.prim <- c(-.6, .6)   # in this example, precipitation
  ylim.sec <- c(0, .5)    # in this example, temperature
  
  b <- diff(ylim.prim)/diff(ylim.sec)
  a <- b*(ylim.prim[1] - ylim.sec[1])
  
  
  ggplot(df_profits_and_pa, aes(x=Year, y=profit_margin, linetype=Firm)) +
    geom_line() +
    geom_line( aes(y=a + annual_rtorpa_millions*b, linetype='Incentive Payment', group = 1)) +
    geom_point( aes(y=a + annual_rtorpa_millions*b, linetype='Incentive Payment', group = 1), shape=1) +
    # geom_line( aes(y=a + annual_rtordpa_millions*b), linetype=2) +
    
    scale_y_continuous(
      
      # Features of the first axis
      name = "Profit Margin",
      
      # Add a second axis and specify its features
      sec.axis = sec_axis(~ (. - a)/b, name="Incentive Payment ($ millions)")
    ) +
    theme(#plot.title = element_text(hjust = 0.5, size = 22),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title = element_text(size = 22),
      legend.position = "bottom") +
    labs(linetype = "")
  
  # ggtitle('Profit Margins of Major Natural Gas Firms Operating in ERCOT')
  ggsave('../Figures/profit_margins_of_major_firms.png', width = 15)
  
  fit_1 <- lm(profit_margin~annual_rtorpa+annual_rtordpa,
              data=df_profits_and_pa)
  
  print(summary(fit_1))
  
}

#######################################################################
#######################################################################
################### Statistic Functions ##############################
######################################################################

print_n_generators_in_sample <- function(){
  df_EIA860M <- read_csv('../Data/EIA Compiled Data/EIA860M_compiled_data.csv', show_col_types = FALSE)
  n_generators_ids <- unique(df_EIA860M[df_EIA860M$balancing_authority_code == 'ERCO', 'plant_gen_id'][[1]])
  
  n_planned_and_operating_generators_ids <- unique(df_EIA860M[df_EIA860M$balancing_authority_code == 'ERCO' & df_EIA860M$record_category %in% c('planned','operating'), 'plant_gen_id'][[1]])
  
  df_sample_null_stats <- df_EIA860M %>%
    mutate(is_unfilled_ba_null = ifelse(is.na(unfilled_balancing_authority_code),1,0),
           is_pre_geocoords_unfilled_ba_null = ifelse(is.na(unfilled_pre_geocoord_ba_code),1,0),
           is_within_first_12_mo_reporting = ifelse(year <= 2016 & month <=6,1,0),
           is_planned_or_operating = ifelse( record_category %in% c('operating', 'planned'),1,0)
    ) %>%
    # filter(plant_state == 'TX') %>%
    summarize(n = n(),
              n_unfilled_ba = sum(is_unfilled_ba_null),
              n_precoords_unfilled_ba = sum(is_pre_geocoords_unfilled_ba_null),
              n_generators = length(unique(plant_gen_id)),
              n_generators_ercot = length(unique(plant_gen_id[balancing_authority_code == 'ERCO'])),
              n_generators_null_ba = length(unique(plant_gen_id[is_unfilled_ba_null == 1])),
              share_null_ba = n_generators_null_ba/n_generators,
              n_generators_null_ba_first_12mo = length(unique(plant_gen_id[is_unfilled_ba_null == 1 & is_within_first_12_mo_reporting == 1])),
              share_null_ba_first_12mo = n_generators_null_ba_first_12mo/n_generators_null_ba,
              n_generators_null_ba_pre_coords = length(unique(plant_gen_id[is_pre_geocoords_unfilled_ba_null == 1])),
              n_generators_null_ba_pre_coords_in_shape = length(unique(plant_gen_id[is_pre_geocoords_unfilled_ba_null == 1 & in_ercot_shape ==1 ])),
              n_generators_null_ba_pre_coords_operating = length(unique(plant_gen_id[is_pre_geocoords_unfilled_ba_null == 1 & in_ercot_shape ==1 & is_planned_or_operating ==1 ])),
              share_n_generators_null_ba_pre_coords_in_shape_operating = n_generators_null_ba_pre_coords_in_shape/n_generators_null_ba_pre_coords,
              n_generators_null_ba_pre_coords_operating_ng = length(unique(plant_gen_id[is_pre_geocoords_unfilled_ba_null == 1 & in_ercot_shape ==1 & is_planned_or_operating ==1 & energy_source_code_group == 'NG'])),
              n_generators_null_ba_pre_coords_operating_renew = length(unique(plant_gen_id[is_pre_geocoords_unfilled_ba_null == 1 & in_ercot_shape ==1 & is_planned_or_operating ==1 & energy_source_code_group == 'Renewables'])),
              n_generators_null_ba_pre_coords_operating_other = length(unique(plant_gen_id[is_pre_geocoords_unfilled_ba_null == 1 & in_ercot_shape ==1 & is_planned_or_operating ==1 & energy_source_code_group == 'Other']))
    )
  
  rm(df_EIA860M)
  print(paste(length(n_generators_ids), 'generators within ERCOT during analysis period', sep= ' '))
  print(paste(length(n_planned_and_operating_generators_ids), 'planned and operating generators within ERCOT during analysis period', sep= ' '))
  print(paste('Of the ', df_sample_null_stats$n_generators[1], ' generators in Texas during the analysis period ', round(df_sample_null_stats$share_null_ba[1],2)*100, '% have null balancing authority codes at some point in the record. ', round(df_sample_null_stats$share_null_ba_first_12mo[1],2)*100, '% of null values are concentrated in first 12 months of the data. ', df_sample_null_stats$n_generators_null_ba_pre_coords[1], ' generators remain with null balancing authority codes after filling with plant level values from future months, ', round(df_sample_null_stats$share_n_generators_null_ba_pre_coords_in_shape_operating[1],2)*100, '% of which are located within ERCOT\'s boundary. These ', df_sample_null_stats$n_generators_null_ba_pre_coords_in_shape[1], ' generators are assigned to ERCOT. This includes ', df_sample_null_stats$n_generators_null_ba_pre_coords_operating_ng[1], ', ', df_sample_null_stats$n_generators_null_ba_pre_coords_operating_renew[1], ', and ', df_sample_null_stats$n_generators_null_ba_pre_coords_operating_other[1], ' operating or applicant natural gas, renewable and other generators, respectively. The sample is then limited to the ', df_sample_null_stats$n_generators_ercot[1], ' generators assigned to the ERCOT balancing authority.'  ,sep=''))  
  
} ## end print of n generators in sample

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
    
    print(df_total_period_stats)
    
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
  
  
  # stargazer(df_summary_operation,
  #           summary = F,
  #           title='Generator Summary Statistics',
  #           header = F,
  #           digits=2,
  #           rownames = F, 
  #           notes = 'Values in parenthesis denote share of total',
  #           font.size = 'small',
  #           out ='../Images/Figures/operating_generators_summary_statistics.tex')
  
  stargazer(df_summary_operation, summary = F, title='Generator Summary Statistics', header = F, digits=2, rownames = F, notes = 'Values in parenthesis denote share of total', font.size = 'small',
            out = '../Tables/Summary Stats/generators_entry_exit_summary_statistics.csv')
  # return(stargazer(df_summary_operation, summary = F, title='Generator Summary Statistics', header = F, digits=2, rownames = F, notes = 'Values in parenthesis denote share of total', font.size = 'small'))
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

