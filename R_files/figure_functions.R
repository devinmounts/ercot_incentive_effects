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

