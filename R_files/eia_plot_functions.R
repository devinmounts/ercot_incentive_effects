
plot_EIA860M_locations <- function(df_EIA860M, sdf_nerc) {
  
  df_operating <- df_EIA860M %>% filter(record_category == 'operating')
  df_planned <- df_EIA860M %>% filter(record_category == 'planned')
  df_retired <- df_EIA860M %>% filter(record_category == 'retired')
  df_postponed <- df_EIA860M %>% filter(record_category == 'cancelled_postponed')
  
  ggplot() + 
    geom_polygon(data = urbnmapr::states %>% filter(state_abbv == 'TX'), mapping = aes(x = long, y = lat, group = group),
                 fill = "grey", color = "white") +
    geom_polygon(data = sdf_nerc, aes( x = long, y = lat, group = group), color="black", fill=NA,) +
    geom_point(data =df_operating %>% distinct(plant_id, latitude, longitude, balancing_authority_code, record_year), aes(x = longitude, y = latitude, fill=balancing_authority_code, color=balancing_authority_code), size = 1, 
               shape = 23) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    facet_wrap(~record_year) +
    ggtitle('Operating')
  ggsave('../Images/EIA Explore/operating_plants.png', width = 20,  height = 10)
  

  
  ggplot() + 
    geom_polygon(data = urbnmapr::states %>% filter(state_abbv == 'TX'), mapping = aes(x = long, y = lat, group = group),
                 fill = "grey", color = "white") +
    geom_polygon(data = sdf_nerc, aes( x = long, y = lat, group = group), color="black", fill=NA,) +
    geom_point(data =df_planned %>% distinct(plant_id, latitude, longitude, balancing_authority_code, record_year), aes(x = longitude, y = latitude, fill=balancing_authority_code, color=balancing_authority_code), size = 1, 
               shape = 23) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    facet_wrap(~record_year) +
    ggtitle('Planned')
  ggsave('../Images/EIA Explore/planned_plants.png', width = 20, height = 10)
  
  ggplot() + 
    geom_polygon(data = urbnmapr::states %>% filter(state_abbv == 'TX'), mapping = aes(x = long, y = lat, group = group),
                 fill = "grey", color = "white") +
    geom_polygon(data = sdf_nerc, aes( x = long, y = lat, group = group), color="black", fill=NA,) +
    geom_point(data =df_retired %>% distinct(plant_id, latitude, longitude, balancing_authority_code, record_year), aes(x = longitude, y = latitude, fill=balancing_authority_code, color=balancing_authority_code), size = 1, 
               shape = 23) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    facet_wrap(~record_year) +
    ggtitle('Retired')
  ggsave('../Images/EIA Explore/retired_plants.png', width = 20,  height = 10)
  
  ggplot() + 
    geom_polygon(data = urbnmapr::states %>% filter(state_abbv == 'TX'), mapping = aes(x = long, y = lat, group = group),
                 fill = "grey", color = "white") +
    geom_polygon(data = sdf_nerc, aes( x = long, y = lat, group = group), color="black", fill=NA,) +
    geom_point(data =df_postponed %>% distinct(plant_id, latitude, longitude, balancing_authority_code, record_year), aes(x = longitude, y = latitude, fill=balancing_authority_code, color=balancing_authority_code), size = 1, 
               shape = 23) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    facet_wrap(~record_year) +
    ggtitle('Postponed')
  ggsave('../Images/EIA Explore/postponed_plants.png', width = 20,  height = 10)
}


plot_initial_and_terminal_operating_plants_map <- function(df_EIA860M,sdf_nerc) {
  df_operating <- df_EIA860M %>% filter(record_category == 'operating')
  
  df_operating_initial <- df_operating %>% 
    filter(record_year == 2015 & record_month == 'december' & balancing_authority_code == 'ERCO' & plant_state == 'TX')
  
  df_operating_terminal <- df_operating %>% 
    filter(record_year == 2022 & record_month == 'december' & balancing_authority_code == 'ERCO' & plant_state == 'TX')
  
  
  
  ggplot() + 
    geom_polygon(data = urbnmapr::states %>% filter(state_abbv == 'TX'), mapping = aes(x = long, y = lat, group = group),
                 fill = "grey", color = "white") +
    geom_polygon(data = sdf_nerc, aes( x = long, y = lat, group = group), color="black", fill=NA,) +
    geom_point(data =df_operating_initial %>% mutate(`Capacity MW` = net_summer_capacity_mw,
                                                     `Fuel Source` = energy_source_code_group), aes(x = longitude, y = latitude, fill=`Fuel Source`, color=`Fuel Source`, size = `Capacity MW`), 
               shape = 23) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    ggtitle('ERCOT: Operating Generators by Fuel Source - 2015') +
    theme(plot.title = element_text(hjust = 0.5,
                                    size=22))
  ggsave('../Images/EIA Explore/initial_operating_plants_by_fuel.png', width = 20,  height = 10)
  
  ggplot() + 
    geom_polygon(data = urbnmapr::states %>% filter(state_abbv == 'TX'), mapping = aes(x = long, y = lat, group = group),
                 fill = "grey", color = "white") +
    geom_polygon(data = sdf_nerc, aes( x = long, y = lat, group = group), color="black", fill=NA,) +
    geom_point(data =df_operating_terminal %>% mutate(`Capacity MW` = net_summer_capacity_mw,
                                                      `Fuel Source` = energy_source_code_group), aes(x = longitude, y = latitude, fill=`Fuel Source`, color=`Fuel Source`, size = `Capacity MW`), 
               shape = 23) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    ggtitle('ERCOT: Operating Generators by Fuel Source - 2022') +
    theme(plot.title = element_text(hjust = 0.5,
                                    size=22))
  ggsave('../Images/EIA Explore/terminal_operating_plants_by_fuel.png', width = 20,  height = 10)
  
}

plot_null_balancing_authority <- function(df_EIA860M, sdf_nerc) {
  
  ggplot() + 
    geom_polygon(data = urbnmapr::states %>% filter(state_abbv == 'TX'), mapping = aes(x = long, y = lat, group = group),
                 fill = "grey", color = "white") +
    geom_polygon(data = sdf_nerc, aes( x = long, y = lat, group = group), color="black", fill=NA,) +
    geom_point(data =df_EIA860M %>% filter(is.na(balancing_authority_code)) %>% distinct(plant_id, latitude, longitude, in_ercot_shape, balancing_authority_code, record_category), aes(x = longitude, y = latitude, fill='green', color=as.character(in_ercot_shape)), size = 1, 
               shape = 23) +
    facet_wrap(~record_category) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    ggtitle('EIA860M Null Balancing Authority Plants: ERCOT Boundaries') +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave('../Images/EIA Explore/EIA860M_NULL_Balancing_Authority.png' , width = 20, height = 10)
}

plot_operating_cap_and_firm_count <- function(df_EIA860M) {
  df_EIA860M %>% left_join(df_EIA860M %>%
                             filter(record_category == 'operating', balancing_authority_code == 'ERCO') %>%
                             group_by(year, month, entity_id) %>%
                             summarize(entity_cap_mw = sum(net_summer_capacity_mw, na.rm=T)) %>%
                             left_join(df_EIA860M %>% 
                                         filter(record_category == 'operating', balancing_authority_code == 'ERCO') %>% group_by(year, month) %>% summarize(total_cap_mw = sum(net_summer_capacity_mw, na.rm = T)),
                                       by=c('year', 'month')) %>%
                             left_join(df_EIA860M %>% 
                                         filter(record_category == 'operating', balancing_authority_code == 'ERCO') %>% group_by(year, month) %>% summarize(n_entities = length(unique(entity_id))),
                                       by=c('year', 'month')),
                           by=c('year', 'month', 'entity_id')) %>%
    mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    filter(report_category == 'operating', balancing_authority_code == 'ERCO') %>%
    distinct(total_cap_mw, n_entities, date) %>%
    ggplot(aes(x=date)) +
    geom_line(aes(y=n_entities, color='N_Firms')) +
    geom_line(aes(y=total_cap_mw/1000, color='GW Capacity')) +
    ggtitle('EIA860M Total Operating Capacity & Firm Count in ERCOT') +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave('../Images/EIA Explore/EIA860_operating_capacity_and_firm_count.png', width = 20, height = 10)
}

plot_ercot_market_cap_lorenz_curve <- function(df_EIA860M) {
  df_EIA860M %>% left_join(df_EIA860M %>%
                             filter(record_category == 'operating', balancing_authority_code == 'ERCO') %>%
                             group_by(year, month, entity_id) %>%
                             summarize(entity_cap_mw = sum(net_summer_capacity_mw, na.rm=T)) %>%
                             left_join(df_EIA860M %>% 
                                         filter(record_category == 'operating', balancing_authority_code == 'ERCO') %>% group_by(year, month) %>% summarize(total_cap_mw = sum(net_summer_capacity_mw, na.rm = T)),
                                       by=c('year', 'month')) %>%
                             left_join(df_EIA860M %>% 
                                         filter(record_category == 'operating', balancing_authority_code == 'ERCO') %>% group_by(year, month) %>% summarize(n_entities = length(unique(entity_id))),
                                       by=c('year', 'month')),
                           by=c('year', 'month', 'entity_id')) %>%
    mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    filter(report_category == 'operating', balancing_authority_code == 'ERCO') %>%
    distinct(date, year, entity_id, entity_cap_mw, total_cap_mw, n_entities) %>%
    group_by(date) %>%
    arrange(date, entity_cap_mw) %>%
    mutate(cum_entities = row_number(),
           cum_cap_mw = cumsum(entity_cap_mw)) %>%
    mutate(percent_total_operators = cum_entities/n_entities,
           percent_total_cap = cum_cap_mw/total_cap_mw) %>%
    ggplot() +
    geom_line(aes(x=percent_total_operators, y=percent_total_cap, color=as.factor(year))) +
    geom_abline(intercept = 0, slope = 1) +
    ggtitle('EIA860M ERCOT Market Capacity Lorenz Curve') +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave('../Images/EIA Explore/ERCOT_market_cap_lorenz_curve.png', width = 20, height = 10)
}

plot_ercot_ng_cap_lorenz_curve <- function(df_EIA860M) {
  df_EIA860M %>% left_join(df_EIA860M %>%
                             filter(record_category == 'operating', balancing_authority_code == 'ERCO', energy_source_code == 'NG') %>%
                             group_by(year, entity_id) %>%
                             summarize(entity_cap_mw = sum(net_summer_capacity_mw, na.rm=T)) %>%
                             left_join(df_EIA860M %>% 
                                         filter(record_category == 'operating', balancing_authority_code == 'ERCO', energy_source_code == 'NG') %>% group_by(year) %>% summarize(total_cap_mw = sum(net_summer_capacity_mw, na.rm = T)),
                                       by=c('year')) %>%
                             left_join(df_EIA860M %>% 
                                         filter(record_category == 'operating', balancing_authority_code == 'ERCO', energy_source_code == 'NG') %>% group_by(year) %>% summarize(n_entities = length(unique(entity_id))),
                                       by=c('year')),
                           by=c('year', 'entity_id')) %>%
    mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    filter(report_category == 'operating', balancing_authority_code == 'ERCO', energy_source_code == 'NG') %>%
    distinct(year, entity_id, entity_cap_mw, total_cap_mw, n_entities) %>%
    group_by(year) %>%
    arrange(year, entity_cap_mw) %>%
    mutate(cum_entities = row_number(),
           cum_cap_mw = cumsum(entity_cap_mw)) %>%
    mutate(percent_total_operators = cum_entities/n_entities,
           percent_total_cap = cum_cap_mw/total_cap_mw) %>%
    ggplot() +
    geom_line(aes(x=percent_total_operators, y=percent_total_cap, color=as.factor(year))) +
    geom_abline(intercept = 0, slope = 1) +
    ggtitle('EIA860M ERCOT NG Capacity Lorenz Curve') +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave('../Images/EIA Explore/ERCOT_ng_cap_lorenz_curve.png', width = 20, height = 10)
}

plot_ercot_market_gen_lorenz_curve <- function(df_EIA923A) {
  df_EIA923A %>% 
    left_join(df_EIA923A %>%
                filter(balancing_authority_code == 'ERCO') %>%
                group_by(year, operator_id) %>%
                summarize(operator_gen_m_wh = sum(month_net_gen_m_wh, na.rm = T))  %>%
                left_join(df_EIA923A %>%
                            filter(balancing_authority_code == 'ERCO') %>% group_by(year) %>% summarize(total_gen_mw_h = sum(month_net_gen_m_wh, na.rm = T)),
                          by=c('year')) %>%
                left_join(df_EIA923A %>%
                            filter(balancing_authority_code == 'ERCO') %>% group_by(year) %>% summarize(n_operators = length(unique(operator_id))),
                          by=c('year')),
              by=c('year', 'operator_id')) %>%
    # mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    filter(balancing_authority_code == 'ERCO') %>%
    distinct(year, operator_id, operator_gen_m_wh, total_gen_mw_h, n_operators) %>%
    group_by(year) %>%
    arrange(year, operator_gen_m_wh) %>%
    mutate(cum_operators = row_number(),
           cum_gen_m_wh = cumsum(operator_gen_m_wh)) %>%
    mutate(percent_total_operators = cum_operators/n_operators,
           percent_total_gen = cum_gen_m_wh/total_gen_mw_h) %>%
    ggplot() +
    geom_line(aes(x=percent_total_operators, y=percent_total_gen, color=as.factor(year))) +
    geom_abline(intercept = 0, slope = 1) +
    ggtitle('EIA923A ERCOT Market Generation Lorenz Curve') +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave('../Images/EIA Explore/ERCOT_market_gen_lorenz_curve.png', width=20, height=10)
}

plot_ercot_ng_gen_lorenz_curve <- function(df_EIA923A) {
  df_EIA923A %>% 
    left_join(df_EIA923A %>%
                filter(balancing_authority_code == 'ERCO', reported_fuel_type_code == 'NG') %>%
                group_by(year, operator_id) %>%
                summarize(operator_gen_m_wh = sum(month_net_gen_m_wh, na.rm = T))  %>%
                left_join(df_EIA923A %>%
                            filter(balancing_authority_code == 'ERCO', reported_fuel_type_code == 'NG') %>% group_by(year) %>% summarize(total_gen_mw_h = sum(month_net_gen_m_wh, na.rm = T)),
                          by=c('year')) %>%
                left_join(df_EIA923A %>%
                            filter(balancing_authority_code == 'ERCO', reported_fuel_type_code == 'NG') %>% group_by(year) %>% summarize(n_operators = length(unique(operator_id))),
                          by=c('year')),
              by=c('year', 'operator_id')) %>%
    # mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    filter(balancing_authority_code == 'ERCO', reported_fuel_type_code == 'NG') %>%
    distinct(year, operator_id, operator_gen_m_wh, total_gen_mw_h, n_operators) %>%
    group_by(year) %>%
    arrange(year, operator_gen_m_wh) %>%
    mutate(cum_operators = row_number(),
           cum_gen_m_wh = cumsum(operator_gen_m_wh)) %>%
    mutate(percent_total_operators = cum_operators/n_operators,
           percent_total_gen = cum_gen_m_wh/total_gen_mw_h) %>%
    ggplot() +
    geom_line(aes(x=percent_total_operators, y=percent_total_gen, color=as.factor(year))) +
    geom_abline(intercept = 0, slope = 1) +
    ggtitle('EIA860M ERCOT NG Generation Lorenz Curve') +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave('../Images/EIA Explore/ERCOT_ng_gen_lorenz_curve.png', width=20, height=10)
}

plot_ercot_market_cap_gini_coef <- function(df_EIA860M) {
  df_EIA860M %>%
    filter(record_category == 'operating') %>%
    distinct(year, month, market_cap_gini_coef) %>%
    ggplot() +
    geom_line(aes(x=as.Date(paste(year, month, '01', sep = '-'), format = '%Y-%m-%d'), y=market_cap_gini_coef)) +
    ggtitle('EIA860M ERCOT Market Cap Monthly Gini Coeficient') +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab('date')
  ggsave('../Images/EIA Explore/ERCOT_market_cap_gini_coef.png', width = 20, height = 10)
}

plot_ercot_ng_cap_gini_coef <- function(df_EIA860M) {
  df_EIA860M %>%
    filter(record_category == 'operating') %>%
    ggplot() +
    geom_line(aes(x=as.Date(paste(year, month, '01', sep = '-'), format = '%Y-%m-%d'), y=ng_cap_gini_coef)) +
    ggtitle('EIA860M ERCOT NG Monthly Cap Gini Coeficient') +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab('date')
  ggsave('../Images/EIA Explore/ERCOT_ng_cap_gini_coef.png', width = 20, height = 10)
}

plot_ercot_market_gen_gini_coef <- function(df_EIA923A) {
  df_EIA923A %>%
    filter(balancing_authority_code == 'ERCO') %>%
    distinct(year, month, market_gen_gini_coef) %>%
    mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    ggplot() +
    geom_line(aes(x=date, y=market_gen_gini_coef)) +
    ggtitle('EIA923A ERCOT Market Monthly Gen Gini Coeficient') +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave('../Images/EIA Explore/ERCOT_market_gen_gini_coef.png', width = 20, height = 10)
}

plot_ercot_ng_gen_gini_coef <- function(df_EIA923A) {
  df_EIA923A %>%
    filter(balancing_authority_code == 'ERCO') %>%
    distinct(year, month, ng_gen_gini_coef) %>%
    mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    ggplot() +
    geom_line(aes(x=date, y=ng_gen_gini_coef)) +
    ggtitle('EIA923A ERCOT NG Monthly Gen Gini Coeficient') +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave('../Images/EIA Explore/ERCOT_ng_gen_gini_coef.png', width = 20, height = 10)
}


plot_ercot_monthly_gen_by_fuel <- function(df_EIA923A) {
  df_EIA923A %>% 
    filter(balancing_authority_code == 'ERCO') %>%
    group_by(year, month, reported_fuel_type_code) %>%
    summarize(month_gen_by_fuel = sum(month_net_gen_m_wh, na.rm = T)) %>%
    mutate(date = as.Date(paste(year, month, '01', sep = '-'))) %>%
    ggplot() +
    geom_line(aes(x=date, y=month_gen_by_fuel/1000, color=reported_fuel_type_code)) +
    ggtitle('EIA923A ERCOT Generation By Fuel') +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab('date') +
    ylab('Monthly Gen (GWH)') 
  ggsave('../Images/EIA Explore/ERCOT_monthly_gen_by_fuel.png', width = 20, height = 10)
}

plot_planned_mw_capacity_by_status_and_energy_source <- function(df_EIA860M) {
  
  df_EIA860M %>%
    filter(record_category == 'planned', balancing_authority_code == 'ERCO') %>%
    group_by(year, month, plant_id, status, energy_source_code) %>%
    mutate(plant_net_summer_capacity_mw = sum(net_summer_capacity_mw, na.rm = T)) %>%
    ungroup() %>%
    distinct(year, month, plant_id, energy_source_code, status, plant_net_summer_capacity_mw) %>%
    mutate(status_code = gsub("[\\(\\)]",          # Extract characters within parentheses
                              "",
                              regmatches(status,
                                         gregexpr("\\(.*?\\)",
                                                  status)))) %>%
    group_by(year, month, status, status_code, energy_source_code) %>%
    summarize(planned_capacity_mw = sum(plant_net_summer_capacity_mw, na.rm = T)) %>%
    ggplot() +
    geom_line(aes(x=as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d'), y=planned_capacity_mw, color=status)) +
    facet_wrap(~energy_source_code) +
    ggtitle('ERCOT Planned Capacity By Status and Energy Source') +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position="top") +
    xlab('date') 
  ggsave('../Images/EIA Explore/ERCOT_planned_mw_by_status_and_energy_source.png', width = 20, height = 10)
}

plot_planned_mw_capacity_by_energy_source <- function(df_EIA860M) {
  
  df_EIA860M %>%
    filter(record_category == 'planned', balancing_authority_code == 'ERCO') %>%
    group_by(year, month, plant_id, energy_source_code) %>%
    mutate(plant_net_summer_capacity_mw = sum(net_summer_capacity_mw, na.rm = T)) %>%
    ungroup() %>%
    distinct(year, month, plant_id, energy_source_code, plant_net_summer_capacity_mw) %>%
    group_by(year, month, energy_source_code) %>%
    summarize(planned_capacity_mw = sum(plant_net_summer_capacity_mw, na.rm = T)) %>%
    ggplot() +
    geom_line(aes(x=as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d'), y=planned_capacity_mw, color=energy_source_code)) +
    ggtitle('ERCOT Planned Capacity By Energy Source') +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab('date') 
  ggsave('../Images/EIA Explore/ERCOT_planned_mw_by_energy_source.png', width = 20, height = 10)
}

plot_cancelled_mw_capacity_by_energy_source <- function(df_EIA860M) {
  
  df_EIA860M %>%
    filter(record_category == 'cancelled_postponed', balancing_authority_code == 'ERCO') %>%
    group_by(year, month, plant_id, energy_source_code) %>%
    mutate(plant_net_summer_capacity_mw = sum(net_summer_capacity_mw, na.rm = T)) %>%
    ungroup() %>%
    distinct(year, month, plant_id, energy_source_code, plant_net_summer_capacity_mw) %>%
    group_by(year, month, energy_source_code) %>%
    summarize(planned_capacity_mw = sum(plant_net_summer_capacity_mw, na.rm = T)) %>%
    ggplot() +
    geom_line(aes(x=as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d'), y=planned_capacity_mw, color=energy_source_code)) +
    ggtitle('ERCOT Cancelled/Postponed Capacity By Energy Source') +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab('date') 
  ggsave('../Images/EIA Explore/ERCOT_cancelled_mw_by_energy_source.png', width = 20, height = 10)
}

plot_capacity_by_status <- function(df_EIA860M) {
  df_EIA860M %>%
    filter(balancing_authority_code == 'ERCO') %>%
    group_by(year, month, record_category) %>%
    summarize(total_capacity_mw = sum(net_summer_capacity_mw, na.rm=TRUE)) %>%
    ggplot() +
    geom_line(aes(x=as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d'), y=total_capacity_mw, color=record_category)) +
    ggtitle('ERCOT Capacity By Status') +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position="top") +
    xlab('date') 
  ggsave('../Images/EIA Explore/ERCOT_capacity_mw_by_status.png', width = 20, height = 10)
}

plot_capacity_by_status_energy_source <- function(df_EIA860M) {
  df_EIA860M %>%
    filter(balancing_authority_code == 'ERCO') %>%
    group_by(year, month, record_category, energy_source_code) %>%
    summarize(total_capacity_mw = sum(net_summer_capacity_mw, na.rm=TRUE)) %>%
    ggplot() +
    geom_line(aes(x=as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d'), y=total_capacity_mw, color=energy_source_code)) +
    facet_wrap(~record_category) +
    ggtitle('ERCOT Capacity By Status and Energy Source') +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position="top") +
    xlab('date') 
  ggsave('../Images/EIA Explore/ERCOT_capacity_mw_by_status_energy_source.png', width = 20, height = 10)
}

plot_ng_capacity_by_status <- function(df_EIA860M) {
  df_EIA860M %>%
    filter(balancing_authority_code == 'ERCO', energy_source_code == 'NG') %>%
    group_by(year, month, record_category) %>%
    summarize(total_capacity_mw = sum(net_summer_capacity_mw, na.rm=TRUE)) %>%
    ggplot() +
    geom_line(aes(x=as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d'), y=total_capacity_mw, color=record_category)) +
    ggtitle('ERCOT NG Capacity By Status') +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position="top") +
    xlab('date') 
  ggsave('../Images/EIA Explore/ERCOT_ng_capacity_mw_by_status.png', width = 20, height = 10)
}

plot_mean_mw_by_phase_timeline <- function(df_plant_gen_id_phase_timeline) {
  df_plant_gen_id_phase_timeline %>%
    arrange(plant_gen_id, date) %>%
    group_by(date, energy_source_code, status, status_phase) %>%
    summarize(n_plant_gen_ids = n(),
              total_mw = sum(net_summer_capacity_mw, na.rm=T),
              mean_mw = mean(net_summer_capacity_mw, na.rm=T),
              sd_mw = sd(net_summer_capacity_mw, na.rm=T)) %>%
    mutate(status_phase = if_else(status_phase >=10, paste('i', status_phase, sep = ''), as.character(status_phase)),
           status = paste(status_phase, status, sep = ' ')) %>%
    filter(energy_source_code %in% c('NG', 'WND', 'SUN')) %>%
    ggplot() +
    geom_line(aes(x=date, y = mean_mw, color= energy_source_code)) +
    # geom_errorbar(aes(x=date, ymin=mean_mw-sd_mw, ymax=mean_mw+sd_mw, color=energy_source_code), width=.2,
    # position=position_dodge(0.05)) +
    facet_wrap(~status, ncol=1) +
    ggtitle('EIA860M Mean MW by Status Phase') +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position="top") 
  ggsave('../Images/EIA Explore/mean_mw_by_phase.png', width=20, height=20)
}

plot_mw_new_operation <- function(df_plant_gen_id_phase_timeline){
  
  df_plant_gen_id_phase_timeline %>%
    group_by(date, energy_source_code, is_first_operation_record) %>%
    summarize(mw_new_op = sum(net_summer_capacity_mw, na.rm = T)) %>%
    filter(is_first_operation_record == 1, energy_source_code %in% c('NG', 'WND', 'SUN'), date > '2015-07-01') %>%
    ggplot(aes(x=date, y= mw_new_op, color=energy_source_code)) +
    geom_line() +
    geom_point() +
    ggtitle('Total MW of New Operation') +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position="top") 
  ggsave('../Images/EIA Explore/total_mw_new_operation.png', width=20, height=20)
}

plot_initial_and_terminal_operating_plants_map <- function(){
  
  df_EIA860M <-read.csv( '../Data/EIA Compiled Data/EIA860M_compiled_data.csv')
  sdf_nerc <- geojson_read('../Data/Geospatial/NERC_Regions.geojson', what='sp')
  sdf_nerc <- tidy(sdf_nerc)
  
  df_operating <- df_EIA860M %>% filter(record_category == 'operating')
  
  df_operating_initial <- df_operating %>% 
    filter(record_year == 2015 & record_month == 'december' & balancing_authority_code == 'ERCO' & plant_state == 'TX')
  
  df_operating_terminal <- df_operating %>% 
    filter(record_year == 2022 & record_month == 'december' & balancing_authority_code == 'ERCO' & plant_state == 'TX')
  
  
  
  ggplot() + 
    geom_polygon(data = urbnmapr::states %>% filter(state_abbv == 'TX'), mapping = aes(x = long, y = lat, group = group),
                 fill = "grey", color = "white") +
    geom_polygon(data = sdf_nerc, aes( x = long, y = lat, group = group), color="black", fill=NA,) +
    geom_point(data =df_operating_initial %>% mutate(`Capacity MW` = net_summer_capacity_mw,
                                                     `Fuel Source` = energy_source_code_group), aes(x = longitude, y = latitude, fill=`Fuel Source`, color=`Fuel Source`, size = `Capacity MW`), 
               shape = 23) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    ggtitle('ERCOT: Operating Generators by Fuel Source - 2015') +
    theme(plot.title = element_text(hjust = 0.5,
                                    size=22))
  ggsave('../Images/EIA Explore/initial_operating_plants_by_fuel.png', width = 20,  height = 10)
  
  ggplot() + 
    geom_polygon(data = urbnmapr::states %>% filter(state_abbv == 'TX'), mapping = aes(x = long, y = lat, group = group),
                 fill = "grey", color = "white") +
    geom_polygon(data = sdf_nerc, aes( x = long, y = lat, group = group), color="black", fill=NA,) +
    geom_point(data =df_operating_terminal %>% mutate(`Capacity MW` = net_summer_capacity_mw,
                                                      `Fuel Source` = energy_source_code_group), aes(x = longitude, y = latitude, fill=`Fuel Source`, color=`Fuel Source`, size = `Capacity MW`), 
               shape = 23) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    ggtitle('ERCOT: Operating Generators by Fuel Source - 2022') +
    theme(plot.title = element_text(hjust = 0.5,
                                    size=22))
  ggsave('../Images/EIA Explore/terminal_operating_plants_by_fuel.png', width = 20,  height = 10)
  
}

plot_profit_margin_select_firms <- function(){
  df_profits = read_excel('../Data/Generator Profits/sec_filings_ng.xlsx')
  df_annual_pas = read_csv('../Data/ERCOT Complied Data/ercot_pa_annaul_sum.csv')
  
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
  ggsave('../Images/Generator Profits/profit_margins.png', width = 15)
  
  fit_1 <- lm(profit_margin~annual_rtorpa+annual_rtordpa,
              data=df_profits_and_pa)
  
  print(summary(fit_1))
  
}
