library(data.table)

create_generation_capacity_and_adder_data <- function(){ 
  df_fuel_gen <- load_and_preprocess_ERCOT_gen_data()
  
  df_gen_cap <- merge_scarcity_pricing_and_reserves(df_fuel_gen)
  df_gen_cap <- merge_price_data(df_gen_cap)
  df_gen_cap <- calculate_interval_totals_for_fuel_gen(df_gen_cap)
  df_gen_cap <- adjust_dollars_to_base_2022_December(df_gen_cap)
  df_gen_cap <- calculate_cumulatives_and_means_for_figure_1(df_gen_cap)
  df_gen_cap <- df_gen_cap %>% filter(settlement_type == 'FINAL')
  write_csv(df_gen_cap, '../Data/ERCOT Compiled Data/gen_cap_and_adder_interval.csv')
}

load_and_preprocess_ERCOT_gen_data <- function() {
  #load (energy demand) data
  fuel_gen_files_1 <- c(
    '../source_data/Generation/FuelMixReport_PreviousYears/IntGenByFuel2013.xls',
    '../source_data/Generation/FuelMixReport_PreviousYears/IntGenByFuel2014.xls',
    '../source_data/Generation/FuelMixReport_PreviousYears/IntGenByFuel2015.xls',
    '../source_data/Generation/FuelMixReport_PreviousYears/IntGenByFuel2016.xlsx'
  )
  
  fuel_gen_files_2 <- c(
    '../source_data/Generation/FuelMixReport_PreviousYears/IntGenByFuel2017.xlsx',
    '../source_data/Generation/FuelMixReport_PreviousYears/IntGenByFuel2018.xlsx',
    '../source_data/Generation/FuelMixReport_PreviousYears/IntGenByFuel2019.xlsx',
    '../source_data/Generation/FuelMixReport_PreviousYears/IntGenByFuel2020.xlsx',
    '../source_data/Generation/FuelMixReport_PreviousYears/IntGenByFuel2021.xlsx',
    '../source_data/Generation/FuelMixReport_PreviousYears/IntGenByFuel2022.xlsx'
  )
  #instatiate data frame to recieve load data from various files
  df_fuel_gen_1 <- data.frame()
  df_fuel_gen_2 <- data.frame()
  
  #loop through files and sheet to create one data frame
  print("Starting to import Load files from group 1")
  for (file in fuel_gen_files_1) {
    
    print(paste('importing file: ', file, sep = " "))
    sheetnames <- excel_sheets(file)
    sheetnames <- sheetnames[!grepl(paste0(c('Chart', '20', 'Disclaimer', 'Summary'), collapse = "|"), sheetnames)]
    mylist <- lapply(sheetnames, function(x)
      read_excel(file,x, col_names = FALSE,skip = 1))
    # #col_names is TRUE by default so you can use this without anonymous function like
    # #mylist <- lapply(sheetnames, read_excel, path = path, skip = 1)
    # 
    # # name the dataframes
    # 
    # 
    names(mylist) <- sheetnames
    # 
    print('naming interval columns and adding 4 columns to non DST months, so all data has the same number of columns')
    for (sheet_num in c(1:length(sheetnames))){
      
      if (length(colnames(mylist[[sheet_num]])) == 98) {
        colnames(mylist[[sheet_num]]) <- c('Date_Fuel', 'Total', 1:96)
        mylist[[sheet_num]] <- mylist[[sheet_num]] %>%
          add_column(`97` = NA,
                     `98` = NA,
                     `99` = NA,
                     `100` = NA)
      }
      else {
        colnames(mylist[[sheet_num]]) <- c('Date_Fuel', 'Total', 1:100)
      }
      #
    }
    # 
    # #use Map to bind all the elements of the list into a dataframe
    my_list <- Map(cbind, mylist, Cluster = names(mylist))
    df_fuel_gen_individual <- do.call("rbind", my_list)
    
    df_fuel_gen_individual <- clean_names(df_fuel_gen_individual)
    
    
    df_fuel_gen_1 <- bind_rows(df_fuel_gen_1, df_fuel_gen_individual)
  }
  #separate data and fuel column
  print('Splitting data and fuel field into two')
  df_fuel_gen_1 <- df_fuel_gen_1 %>%
    separate(date_fuel, c('date', 'fuel'), sep='_') %>% 
    add_column(settlement_type = 'FINAL', .after = 'fuel' )  %>%
    mutate(date= as.Date(date,format = "%m/%d/%y"))
  
  
  print("Starting to import Load files from group 2")
  for (file in fuel_gen_files_2) {
    
    print(paste('importing file: ', file, sep = " "))
    sheetnames <- excel_sheets(file)
    sheetnames <- sheetnames[!grepl(paste0(c('Chart', 'Disclaimer', 'Summary'), collapse = "|"), sheetnames)]
    mylist <- lapply(sheetnames, function(x)
      read_excel(file,x, col_names = FALSE,skip = 1))
    # #col_names is TRUE by default so you can use this without anonymous function like
    # #mylist <- lapply(sheetnames, read_excel, path = path, skip = 1)
    # 
    # # name the dataframes
    # 
    # 
    names(mylist) <- sheetnames
    # 
    print('naming interval columns and adding 4 columns to non DST months, so all data has the same number of columns')
    for (sheet_num in c(1:length(sheetnames))){
      
      if (length(colnames(mylist[[sheet_num]])) == 100) {
        colnames(mylist[[sheet_num]]) <- c('Date', 'Fuel', 'Settlement_Type', 'Total', 1:96)
        mylist[[sheet_num]] <- mylist[[sheet_num]] %>%
          add_column(`97` = NA,
                     `98` = NA,
                     `99` = NA,
                     `100` = NA)
      }
      else {
        colnames(mylist[[sheet_num]]) <- c('Date', 'Fuel', 'Settlement_Type', 'Total', 1:100)
      }
      
      
    }
    # 
    # #use Map to bind all the elements of the list into a dataframe
    my_list <- Map(cbind, mylist, Cluster = names(mylist))
    df_fuel_gen_individual <- do.call("rbind", my_list)
    
    df_fuel_gen_individual <- clean_names(df_fuel_gen_individual)
    
    
    df_fuel_gen_2 <- bind_rows(df_fuel_gen_2, df_fuel_gen_individual)
  }
  print('Binding two gen data sets into one')
  df_fuel_gen <- bind_rows(df_fuel_gen_1, df_fuel_gen_2)
  rm(df_fuel_gen_1)
  rm(df_fuel_gen_2)
  
  
  df_fuel_gen <- df_fuel_gen %>%
    pivot_longer(x1:x100, names_to = 'interval', values_to = 'generation_m_wh') %>%
    mutate(interval = as.integer(sub('.','', interval)))
  
  print("Adding time relevant features...")
  df_fuel_gen <- df_fuel_gen %>%
    mutate(repeated_hour_flag = case_when(interval > 96 & !is.na(generation_m_wh) ~ 1,
                                          TRUE ~ 0))
  
  print('Altering intervals for daylight savings and marking with repeated_hour_flag')
  df_fuel_gen <- df_fuel_gen %>%
    mutate(interval = case_when(repeated_hour_flag == 1 & interval == 97 ~ 9,
                                repeated_hour_flag == 1 & interval == 98 ~ 10,
                                repeated_hour_flag == 1 & interval == 99 ~ 11,
                                repeated_hour_flag == 1 & interval == 100 ~ 12,
                                TRUE~as.double(interval)))
  
  print("Hour (accounting for daylight savings time)")
  df_interval_map <- data.frame(interval=seq(1, 100), hour=rep(seq(0,24), each=4), minute=rep(c(0,15,30,45), each = 1))
  
  df_fuel_gen <- df_fuel_gen %>%
    merge(df_interval_map, by='interval', how='left') %>%
    arrange(date, fuel, interval)
  
  print(paste('Hour intervals created to indicate minutes:', paste(unique(df_fuel_gen$minute), collapse = ' '), sep = ' '))
  
  print('Removing null records for intervals 97,98,99,100 on non DST days.')
  df_fuel_gen <- df_fuel_gen %>%
    filter(!(interval %in% c(97,98,99,100) & is.na(generation_m_wh)))
  print('Creating timestamps')
  df_fuel_gen <- df_fuel_gen %>%
    mutate(date_time = as.POSIXct(paste(date, paste(hour, minute, sep =  ':'), sep = ' ', format='%Y-%m-%d %H:%M'), tz='UTC'))
  
  print('pivoting wider')
  df_fuel_gen <- df_fuel_gen %>%
    select(date, interval, settlement_type, repeated_hour_flag, hour, minute, date_time, fuel, generation_m_wh) %>%
    pivot_wider(names_from = fuel, values_from = generation_m_wh) %>%
    clean_names()
  
  return(df_fuel_gen)
}### end of load and process generation data

#############################################
#############################################
merge_scarcity_pricing_and_reserves <- function(df_load, to_csv=FALSE) {
  print("Load and merge Scarcity Price adders and Reserves...")
  
  #load (energy demand) data
  scarcity_files <- c('../source_data/Reserves/Historical Real-Time Reserve ORDC Price Adder_2014.xlsx',
                      '../source_data/Reserves/RTM_ORDC_REL_DPLY_PRC_ADDR_RSRV_2015.xlsx',
                      '../source_data/Reserves/RTM_ORDC_REL_DPLY_PRC_ADDR_RSRV_2016.xlsx',
                      '../source_data/Reserves/RTM_ORDC_REL_DPLY_PRC_ADDR_RSRV_2017.xlsx',
                      '../source_data/Reserves/RTM_ORDC_REL_DPLY_PRC_ADDR_RSRV_2018.xlsx',
                      '../source_data/Reserves/RTM_ORDC_REL_DPLY_PRC_ADDR_RSRV_2019.xlsx',
                      '../source_data/Reserves/RTM_ORDC_REL_DPLY_PRC_ADDR_RSRV_2020.xlsx',
                      '../source_data/Reserves/RTM_ORDC_REL_DPLY_PRC_ADDR_RSRV_2021.xlsx',
                      '../source_data/Reserves/RTM_ORDC_REL_DPLY_PRC_ADDR_RSRV_2022.xlsx'
                      )
  
  #instatiate data frame to recieve load data from various files
  df_scarcity <- data.frame()
  
  #loop through files and sheet to create one data frame
  print("Starting to import Scarcity and Reserve files")
  for (file in scarcity_files) {
    
    print(paste('importing file: ', file, sep = " "))
    sheetnames <- excel_sheets(file)
    mylist <- lapply(sheetnames, function(x) 
      read_excel(file,x, col_names = TRUE,skip = 7))
    #col_names is TRUE by default so you can use this without anonymous function like
    #mylist <- lapply(sheetnames, read_excel, path = path, skip = 1)
    
    # name the dataframes
    
    names(mylist) <- sheetnames
    
    #use Map to bind all the elements of the list into a dataframe
    my_list <- Map(cbind, mylist, Cluster = names(mylist))
    
    df_scarcity_individual <- do.call("rbind", my_list)
    
    df_scarcity_individual <- clean_names(df_scarcity_individual)
    
    #df_scarcity_individual$sced_timestamp <- as.datetime(df_scarcity_individual$sced_timestamp)
    
    df_scarcity <- bind_rows(df_scarcity, df_scarcity_individual)
  }
  
  df_scarcity$repeated_hour_flag <- if_else(df_scarcity$repeated_hour_flag == 'Y', 1,0)
  
  #remove undesired columns from load data
  print('Importing of scarcity and reserve data complete')
  
  df_load <- df_load %>% ungroup()

  
  
  print('Merging load data and scarcity/reserves data by nearest timestamp')
  setDT(df_load)
  setDT(df_scarcity)
  indx <- df_scarcity[df_load,
                      on = c(repeated_hour_flag = "repeated_hour_flag",
                             sced_timestamp = "date_time"),
                      roll = 'nearest',
                      which = TRUE]

  df_load <- df_load[, sced_timestamp := df_scarcity[indx,sced_timestamp]]

  df_load <- df_load %>%
    left_join(df_scarcity, by=c('sced_timestamp', 'repeated_hour_flag'))
  
  print('Scarcity price adders and reserves data successfull merged')
  
  if (to_csv) {
    #write to csv
    print("Writing data to csv...")
    write_csv(df_load, '../Data/ERCOT Compiled Data/preprocessed_load.csv')
  }
  
  return(df_load)
}


load_and_preprocess_weather_data <- function() {
  #read weather file
  weather_file <- '../source_data/Weather/energy_zip_weather10292023.xlsx'
  
  df_weather_raw <- read_excel(weather_file)
  df_weather_raw <- df_weather_raw %>%
    mutate(dt = as.Date(paste(y,m,d, sep = "-"), format="%Y-%m-%d")) %>%
    rename(., date = dt) %>%
    filter(date < '2023-09-01')
  
  #subset weather file to target columns
  df_weather_target <- df_weather_raw[,c('zipcode', 'date', 'base_min', 'act_min', 'base_max', 'act_max', 'base_hum', 'act_hum', 'base_dew', 'act_dew')]
  
  #insert rows where there are missing dates group by zip
  df_weather_target <- df_weather_target %>%
    group_by(zipcode) %>%
    complete(date = seq(min(df_weather_raw$date), max(df_weather_raw$date), 1))
  
  #forward fill weather values
  df_weather_target <- df_weather_target %>%
    fill(base_min, act_min, base_max, act_max, base_hum, act_hum, base_dew, act_dew)
  
  
  df_hourly <- df_hourly %>%
    left_join(df_weather_target, by=c('zipcode', 'date'))  %>%
    mutate(temp_min = base_min + act_min,
           temp_max = base_max + act_max,
           hum = base_hum + act_hum,
           dew = base_dew + act_dew)

  

  df_weather_target <- df_weather_target %>%
    mutate(region = case_when(zipcode == 77061 ~'coast',
                              zipcode == 75083 ~ 'north_c',
                              zipcode == 75701 ~ 'east',
                              zipcode == 78214 ~ 'south_c',
                              zipcode == 78406 ~ 'southern',
                              zipcode == 79707 ~ 'far_west',
                              zipcode == 79602 ~ 'west',
                              zipcode == 76305 ~ 'north'))
  
  df_weather_target <- df_weather_target  %>%
    mutate(temp_min = base_min + act_min,
           temp_max = base_max + act_max,
           hum = base_hum + act_hum,
           dew = base_dew + act_dew)
  
  #store shaped raw data
  write_file <- '../Data/Weather/target_weather_data.csv'
  write_csv(df_weather_target, write_file)
  
} ### end of load and process weather data


###################################################
###################################################
merge_price_data <- function(df_gen_cap) {
  
  #load (energy demand) data
  price_files <- c(
    '../source_data/Prices/rpt.00013061.0000000000000000.RTMLZHBSPP_2013.xlsx',
    '../source_data/Prices/rpt.00013061.0000000000000000.RTMLZHBSPP_2014.xlsx',
    '../source_data/Prices/rpt.00013061.0000000000000000.RTMLZHBSPP_2015.xlsx',
    '../source_data/Prices/rpt.00013061.0000000000000000.RTMLZHBSPP_2016.xlsx',
    '../source_data/Prices/rpt.00013061.0000000000000000.RTMLZHBSPP_2017.xlsx',
    '../source_data/Prices/rpt.00013061.0000000000000000.RTMLZHBSPP_2018.xlsx',
    '../source_data/Prices/rpt.00013061.0000000000000000.RTMLZHBSPP_2019.xlsx',
    '../source_data/Prices/rpt.00013061.0000000000000000.RTMLZHBSPP_2020.xlsx',
    '../source_data/Prices/rpt.00013061.0000000000000000.RTMLZHBSPP_2021.xlsx',
    '../source_data/Prices/rpt.00013061.0000000000000000.RTMLZHBSPP_2022.xlsx'
  )
  
  
  #instatiate data frame to recieve load data from various files
  df_prices <- data.frame()
  
  #loop through files and sheet to create one data frame
  print("Starting to import Load files from group 1")
  for (file in price_files) {
    
    year = str_sub(file, -9, -6)
    print(year)
    print(paste('importing file: ', file, sep = " "))
    sheetnames <- excel_sheets(file)
    mylist <- lapply(sheetnames, function(x)
      read_excel(file,x, col_names = TRUE,skip = 0))
    # #col_names is TRUE by default so you can use this without anonymous function like
    # #mylist <- lapply(sheetnames, read_excel, path = path, skip = 1)
    # 
    # # name the dataframes
    # 
    # 
    names(mylist) <- paste(sheetnames, year, sep = "_")
    
    # 
    
    # 
    # #use Map to bind all the elements of the list into a dataframe
    my_list <- Map(cbind, mylist, Cluster = names(mylist))
    df_prices_individual <- do.call("rbind", my_list)
    
    df_prices_individual <- clean_names(df_prices_individual)
    
    
    df_prices <- bind_rows(df_prices, df_prices_individual)
  }
  
  print('Reducing prices data to only Bus and Hub Averages')
  df_prices <- df_prices %>%
    filter(settlement_point_name %in% c('HB_BUSAVG', 'HB_HUBAVG'))
  
  print('Pivoting price data wide and creating timestamp')
  df_prices <- df_prices %>%
    select(-settlement_point_type) %>%
    pivot_wider(names_from = settlement_point_name, values_from = settlement_point_price) %>%
    mutate(delivery_minute = plyr::mapvalues(delivery_interval, from = c(1,2,3,4), to=c(0,15,30,45)),
           delivery_date = as.Date(delivery_date, format='%m/%d/%Y'),
           delivery_hour = delivery_hour-1,
           repeated_hour_flag = if_else(repeated_hour_flag == "Y", 1,0)) %>%
    relocate(delivery_minute, .after = delivery_interval) %>%
    mutate(date_time = as.POSIXct(paste(delivery_date, paste(delivery_hour, delivery_minute, sep =  ':'), sep = ' ', format='%Y-%m-%d %H:%M'), tz='UTC')) %>%
    relocate(date_time, .before = delivery_date)
  
  df_prices <- clean_names(df_prices)
  
  df_gen_cap <- df_gen_cap %>%
    left_join(df_prices, by=c('date_time', 'repeated_hour_flag'))
  return(df_gen_cap)
} # end load and merge price data


#calculate market parameters for mu (mean) and sigma (standard deviation)
  # mean is calculated for each customer class (profile) and region month and 15min interval
      # ie for customer profile p in region r_p, during month 1 and interval 0 (first 15 minutes) the average load is x.
        # in other words, the average 15min load for this customer profile & region across all days days in month 1
  # sigma is calculated as the standard deviation of 15min load intervals across the entire market, ie. the market volatility
calculate_15min_mu_sigma <- function(df_ercot) {
  
  df_mu_sigma <- df_ercot %>%
    group_by(region, cust_class, date, interval) %>%
    summarize(mean_load_k_wh = mean(load_k_wh),
              sd_load_k_wh = sd(load_k_wh))
  
  return(df_mu_sigma)
}

calculate_market_sigma <- function(df_ercot) {
  
  market_sigma <- sd(df_ercot$load_k_wh)
  return(market_sigma)
  
}




calculate_interval_totals_for_fuel_gen <- function(df_gen_cap) {
  df_gen_cap <- df_gen_cap %>%
    mutate(int_tot_gen_m_wh = select(.,biomass:wind) %>% rowSums(na.rm=T),
           int_tot_gen_non_gas = select(.,c(biomass:wind, -gas, -gas_cc)) %>% rowSums(na.rm=T),
           int_tot_gen_gas = select(.,c(gas, gas_cc)) %>% rowSums(na.rm=T),
           int_tot_gen_renewable = select(.,c(wind, solar)) %>% rowSums(na.rm=T),
           int_tot_gen_other = select(.,c(biomass:wind, -gas, -gas_cc, -wind, -solar)) %>% rowSums(na.rm=T))
  df_gen_cap <- df_gen_cap %>%
    pivot_longer(cols=biomass:wind, names_to='fuel', values_to ='generation_m_wh') %>%
    mutate(int_tot_spin_cap_mw = int_tot_gen_m_wh + rtolcap) %>%
    relocate(c(int_tot_gen_m_wh:generation_m_wh, int_tot_spin_cap_mw), .after = minute)
  
  return(df_gen_cap)
}



load_peaker_net_margin <- function(){
  peaker_files <- list.files('../source_data/Peaker Net Margin/Peaker Net Margin_2010_2022May11/')
  peaker_files <- peaker_files[grepl('.csv.zip', peaker_files)]
  
  df_pnm <- data.frame()
  
  for(file in peaker_files) {
    file_year <- (str_sub(file, -33, -30))
    print(file)
    if (file_year != "2010"){
      file <- str_trim(file)
      file_path <- paste('../source_data/Peaker Net Margin/Peaker Net Margin_2010_2022May11/', file, sep = '')
      df_pnm_individual <- read_csv(file_path)
      df_pnm <- bind_rows(df_pnm, df_pnm_individual)
    }
    
  }
  
  write_csv(df_pnm, '../Data/ERCOT Compiled Data/compiled_zip_csv_pnm_files_2011_2022.csv')
} # end load peaker net margin

adjust_dollars_to_base_2022_December <- function(df_gen_cap) {
  
  print('adjusting dollars for inflaction to Dec. 2022')
  df_bls <- read_csv('../source_data/Economic/bls_cpi_base_2022.12.csv')
  df_gen_cap <- df_gen_cap %>%
    mutate(year = year(date_time),
           quarter  = quarters(date_time),
           month  = month(date_time)) %>%
    left_join(df_bls, by=c('year', 'month')) %>%
    mutate(rtorpa = rtorpa*cpi_base_2022,
           rtoffpa = rtoffpa*cpi_base_2022,
           rtordpa = rtordpa*cpi_base_2022,
           hb_busavg = hb_busavg*cpi_base_2022,
           hb_hubavg = hb_hubavg*cpi_base_2022
    )
  
  return(df_gen_cap)

} #### end adjust dollars 

calculate_cumulatives_and_means_for_figure_1 <- function(df_gen_cap) {
  
  
  #total rt capacity and energy only price (energy price minus 2 price adders: online reserves and reliability deployments)
  df_gen_cap <- df_gen_cap %>%
    mutate(rttotcap = select(., rtolcap, rtoffcap) %>% rowSums(na.rm=TRUE),
           rttotpa = select(., rtorpa, rtordpa) %>% rowSums(na.rm = TRUE),
           hb_busavg_energy_only = hb_busavg - rttotpa,
           hb_hubavg_energy_only = hb_hubavg - rttotpa)
  
  #cumulative price adders
  df_gen_cap <- df_gen_cap %>% 
    group_by(year, fuel) %>%
    mutate(annual_cum_rtorpa = cumsum(rtorpa),
           annual_cum_rtoffpa = cumsum(rtoffpa),
           annual_cum_rtordpa = cumsum(rtordpa),
           annual_cum_rttotpa = cumsum(rttotpa)) %>%
    ungroup() %>%
    group_by(fuel) %>%
    mutate(cum_rtorpa = cumsum(rtorpa),
           cum_rtoffpa = cumsum(rtoffpa),
           cum_rtordpa = cumsum(rtordpa),
           cum_rttotpa = cumsum(rttotpa)) %>%
    ungroup()
  
  #monthly means
  df_gen_cap <- df_gen_cap %>%
    group_by(year, month, fuel) %>%
    mutate(mean_mo_rtolcap = mean(rtolcap, na.rm=T),
           mean_mo_rtoffcap = mean(rtoffcap, na.rm=T),
           mean_mo_rttotcap = mean(rttotcap, na.rm=T),
           mean_mo_fuel_generation_m_wh = mean(generation_m_wh, na.rm=T),
           mean_mo_non_gas_gen_m_wh = mean(int_tot_gen_non_gas),
           mean_mo_total_gen_m_wh = mean(int_tot_gen_m_wh),
           mean_mo_hb_busavg_ene_only = mean(hb_busavg_energy_only, na.rm=T)) %>%
    ungroup()
  
  return(df_gen_cap)
} ### end calc. cumulatives and means for figure 1

plot_gen_by_fuel_mwh <- function(df_gen_cap) {
  df_gen_cap %>%
    group_by(year,month, fuel) %>%
    mutate(mean_gen_m_wh = mean(generation_m_wh, na.rm=T),
           mean_total_gen_m_wh = mean(int_tot_gen_m_wh, na.rm=T)) %>%
    ungroup() %>%
    ggplot(aes(x=date_time)) +
    geom_line(aes(y=mean_gen_m_wh, color=fuel)) +
    geom_line(aes(y=mean_total_gen_m_wh, color='total_gen_m_wh')) +
    scale_x_datetime(date_breaks = "years" , date_labels = "%Y") +
    ggtitle('ERCOT Monthly Mean Generation Fuel Mix (MWH)')
  
  ggsave('../Images/ERCOT_fuel_mix_mwh_month.png', width = 20,  height = 10)
}

plot_gen_by_fuel_percent <- function(df_gen_cap) {
  df_gen_cap %>%
    mutate(per_gen_m_wh = generation_m_wh/int_tot_gen_m_wh) %>%
    group_by(year, fuel) %>%
    mutate(mean_per_gen_m_wh = mean(per_gen_m_wh, na.rm=T)) %>%
    ungroup() %>%
    ggplot(aes(x=date_time)) +
    geom_line(aes(y=mean_per_gen_m_wh, color=fuel)) +
    scale_x_datetime(date_breaks = "years" , date_labels = "%Y") +
    ggtitle('ERCOT Monthly Mean Generation Fuel Mix')
  
  ggsave('../Images/ERCOT_fuel_mix_perc_month.png', width = 20,  height = 10)
}

plot_figure_1 <- function(df_gen_cap){
  df_gen_cap %>%
    filter(fuel %in% c('gas_cc') & date_time < '2021-01-01') %>%
    ggplot(aes(x=date_time)) +
    # geom_line(aes(y=cum_rtorpa/1000, color='cum_rtorpa')) +
    # geom_line(aes(y=cum_rtoffpa/1000, color='cum_rtoffpa')) +
    # geom_line(aes(y=cum_rtordpa/1000, color='cum_rtordpa')) +
    geom_line(aes(y=cum_rttotpa/100, color='Cum. ORDC Adders ($100\'s)')) +
    # geom_line(aes(y=mean_mo_generation_m_wh/10, color='mean_mo_gas_generation_m_wh')) +
    # geom_line(aes(y=mean_mo_non_gas_gen_m_wh*50/3000, color='mean_mo_non_gas_generation_m_wh')) +
    geom_line(aes(y=mean_mo_total_gen_m_wh, color='Total Generation (MWH)')) +
    # geom_line(aes(y=mean_mo_rtolcap*50/3000, color='mean_mo_rtolcap')) +
    # geom_line(aes(y=mean_mo_rtoffcap*50/3000, color='mean_mo_rtoffcap')) +
    geom_line(aes(y=mean_mo_rttotcap, color='Total Capacity (MW)')) +
    geom_line(aes(y=mean_mo_hb_busavg_ene_only, color='Energy Price (cents/MWH)')) +
    # scale_y_continuous(sec.axis = sec_axis(~ ./10, name = "Mean Mo Gen & Cap (MWH)" )) +
    labs(x="Date", y="") +
    ggtitle('ORDC Price Adders & Capacity and Gas-CC Gen') +
    scale_x_datetime(date_breaks = "years" , date_labels = "%Y")
  
  ggsave('../Images/ORDC_adders_RTM_cap_and_gen.png', width = 20,  height = 10)
}

plot_peaker_net_margin <- function() {
  df_pnm <- read_csv('../Data/Peaker Net Margin/compiles_zip_csv_pnm_files_2011_2022.csv')
  df_pnm <- df_pnm %>%
    mutate(date= as.Date(AsOfDate, format='%m/%d/%Y %H:%M:%S'),
           delta_pnm = if_else((PeakerNetMargin - lag(PeakerNetMargin)) >= 0, PeakerNetMargin - lag(PeakerNetMargin), 0),
           cum_delta_pnm = cumsum(ifelse(is.na(delta_pnm), 0, delta_pnm)) + delta_pnm*0) %>%
    filter(date <= '2021-01-01')
  
  df_pnm %>%
    ggplot(aes(x=date)) +
    geom_line(aes(y=PeakerNetMargin, color='Peaker Net Margin ($)')) +
    geom_line(aes(y=delta_pnm*10, color='Daily Peaker Net Margin ($ x10)')) +
    # geom_line(aes(y=cum_delta_pnm/10, color='Cumulative Peaker Net Margin ($/10)')) +
    # scale_y_continuous(
    #   name = "Peaker Net Margin ($)",
    #   sec.axis = sec_axis(~.*10, name="Cum. Peaker Net Margin ($)")
    # ) +
    ggtitle('ERCOT Peaker Net Margin: 2011-2021') +
    theme(plot.title = element_text(hjust = 0.5)) 
  ggsave('../Images/ERCOT Explore/peaker_net_margin.png', width=20, height=10)
}

create_monthly_ercot_summary <- function(df_gen_cap, lag_months=12) {
  
  df_gen_cap <- read_csv('../Data/ERCOT Compiled Data/gen_cap_and_adder_interval.csv')
  
  df_monthly_ercot_summary <- df_gen_cap %>%
    filter(settlement_type == 'FINAL') %>%
    distinct(date, 
             interval,
             int_tot_gen_m_wh,
             int_tot_gen_non_gas,
             int_tot_gen_gas,
             int_tot_gen_renewable,
             int_tot_spin_cap_mw,
             hb_busavg,
             hb_hubavg,
             hb_busavg_energy_only,
             hb_hubavg_energy_only,
             rtolcap,
             rtoffcap,
             rtorpa,
             rtoffpa,
             rtordpa,
             system_lamda,
             rttotcap) %>%
    mutate(total_pa = rtorpa+rtordpa,
           net_rtorpa = rtorpa - rtoffpa,
           counterfeit_subsidty = rtorpa - rtoffpa,
           total_gen_rtcap_ratio = int_tot_gen_m_wh/(rttotcap+int_tot_gen_m_wh),
           net_sys_lamda = system_lamda - hb_busavg_energy_only,
           rtfunccap = rtolcap+rtoffcap+int_tot_gen_m_wh,
           total_pa_payments = rtorpa*int_tot_gen_m_wh
           ) %>%
    group_by(year(date), month(date)) %>%
    summarize(mean_int_tot_gen_m_wh = mean(int_tot_gen_m_wh, na.rm=T),
              mean_int_tot_gen_non_gas = mean(int_tot_gen_non_gas, na.rm=T),
              mean_int_tot_gen_gas = mean(int_tot_gen_gas, na.rm=T),
              mean_int_tot_gen_renewable = mean(int_tot_gen_renewable, na.rm=T),
              mean_int_tot_spin_cap_mw = mean(int_tot_spin_cap_mw, na.rm=T),
              mean_hb_hubavg = mean(hb_hubavg, na.rm=T),
              mean_hb_busavg_energy_only = mean(hb_busavg_energy_only, na.rm=T),
              mean_rtolcap = mean(rtolcap, na.rm=T),
              mean_rtoffcap = mean(rtoffcap, na.rm=T),
              mean_rtfunccap = mean(rtfunccap, na.rm=TRUE),
              mean_rtorpa = mean(rtorpa, na.rm=T),
              mean_net_rtorpa = mean(net_rtorpa, na.rm=T),
              mean_rtoffpa = mean(rtoffpa, na.rm=T),
              mean_rtordpa = mean(rtordpa, na.rm=T),
              mean_total_pa = mean(total_pa, na.rm=T),
              mean_counterfeit_pa = mean(counterfeit_subsidty, na.rm=T),
              mean_sys_lamda = mean(system_lamda, na.rm=T),
              mean_net_sys_lamda = mean(net_sys_lamda, na.rm=T),
              mean_total_gen_rtcap_ratio = mean(total_gen_rtcap_ratio, na.rm=T),
              total_rtorpa = sum(rtorpa, na.rm = T),
              total_rtordpa = sum(rtordpa, na.rm = T),
              total_mw_under_pa = sum(int_tot_gen_m_wh[rtorpa >0], na.rm=T),
              total_pa_payments = sum(total_pa_payments, na.rm=T)
              ) %>%
    rename(year = `year(date)`, month = `month(date)`) %>%
    mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    relocate(date) %>%
    ungroup() %>%
    mutate(date_minus_month = as.Date(date) %m+% months(1),
           date_minus_year = as.Date(date) %m+% months(12),
           date_minus_lag = as.Date(date) %m+% months(lag_months),
           roll_mean_hb_busavg_ener_only = rollmean(mean_hb_busavg_energy_only, k=12, fill = NA, align='right'))
  
  write_csv(df_monthly_ercot_summary, '../Data/ERCOT Complied Data/ercot_gen_pa_price_monthly_stats.csv')
  
  df_annual_ercot_summary <- df_monthly_ercot_summary %>%
    group_by(year) %>%
    summarize(annual_rtorpa = sum(total_rtorpa, na.rm = T),
              annual_rtordpa = sum(total_rtordpa, na.rm = T),
              annual_mw_under_pa = sum(total_mw_under_pa, na.rm=T),
              total_pa_payments_bill = sum(total_pa_payments, na.rm=T)/100000000)
  
  write_csv(df_annual_ercot_summary, '../Data/ERCOT Complied Data/ercot_pa_annaul_sum.csv')
  rm(df_gen_cap)
}

create_pnm_monthly_summary <- function(lag_months=12) {
  
  df_pnm <- read_csv('../Data/ERCOT Compiled Data/compiled_zip_csv_pnm_files_2011_2022.csv')
  df_bls <- read_csv('../source_data/Economic/bls_cpi_base_2022.12.csv')
  
  df_pnm_summary <- df_pnm %>%
    mutate(date = as.Date(AsOfDate, format='%m/%d/%Y %H:%M:%S'),
           year = year(date),
           month = month(date)) %>%
    left_join(df_bls, by=c('year', 'month')) %>%
    mutate(PeakerNetMargin = PeakerNetMargin*cpi_base_2022,
           ) %>%
    mutate(delta_pnm = if_else((PeakerNetMargin - lag(PeakerNetMargin)) >= 0, PeakerNetMargin - lag(PeakerNetMargin), 0)) %>%
    group_by(year(date), month(date)) %>%
    summarize(mean_pnm = mean(PeakerNetMargin, na.rm=T),
              mean_delta_pnm = mean(delta_pnm, na.rm=T),
              sd_pnm = sd(PeakerNetMargin, na.rm=T),
              sum_pnm = sum(PeakerNetMargin, na.rm=T)) %>%
    rename(year = `year(date)`, month = `month(date)`) %>%
    mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    relocate(date) %>%
    ungroup() %>%
    mutate(date_minus_month = as.Date(date) %m+% months(1),
           date_minus_year = as.Date(date) %m+% months(12),
           date_minus_lag = as.Date(date) %m+% months(lag_months),
           roll_mean_pnm = rollmean(mean_pnm, k=12, fill = NA, align='right'))
    
  
  write_csv(df_pnm_summary, '../Data/ERCOT Compiled Data/pnm_monthly_summary.csv')
  rm(df_pnm_summary)
}

create_ng_hh_monthly_summary <- function(lag_months=12) {
  
  df_ng_prices <- read_csv('../source_data/Natural Gas Prices/Henry_Hub_Natural_Gas_Spot_Price.csv',skip = 4)
  df_bls <- read_csv('../source_data/Economic/bls_cpi_base_2022.12.csv')
  
  df_hh_montly_summary <- df_ng_prices %>%
    mutate(date = as.Date(Day, format='%m/%d/%Y')) %>%
    mutate(year = year(date),
           month = month(date)) %>%
    rename(price = `Henry Hub Natural Gas Spot Price Dollars per Million Btu`) %>%
    left_join(df_bls, by=c('year', 'month')) %>%
    mutate(price = price*cpi_base_2022) %>%
    group_by(year(date), month(date)) %>%
    summarize(mean_hh_price_MMBtu = mean(price, na.rm=T),
              sd_hh_price_MMBtu = sd(price, na.rm=T)) %>%
    rename(year = `year(date)`, month = `month(date)`) %>%
    mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    relocate(date)  %>%
    ungroup() %>%
    mutate(date_minus_month = as.Date(date) %m+% months(1),
           date_minus_year = as.Date(date) %m+% months(12),
           date_minus_lag = as.Date(date) %m+% months(lag_months),
           roll_mean_hh_price_MMBtu = rollmean(mean_hh_price_MMBtu, k=12, fill = NA, align='right'))
  
  write_csv(df_hh_montly_summary, '../Data/ERCOT Compiled Data/ng_hh_price_monthly_summary.csv')
  rm(df_hh_monthly_summary)
}

create_daily_weather_data <- function(){
  #read weather file
  weather_file <- '../source_data/Weather/energy_zip_weather10292023.xlsx'
  
  df_weather_raw <- read_excel(weather_file)
  df_weather_raw <- df_weather_raw %>%
    mutate(dt = as.Date(paste(y,m,d, sep = "-"), format="%Y-%m-%d")) %>%
    rename(., date = dt) %>%
    filter(date < '2023-09-01')
  
  #subset weather file to target columns
  for (col in grep('base', names(df_weather_raw))) {
    print(names(df_weather_raw)[col])
    var <- str_split(names(df_weather_raw)[col], '_')[[1]][2]
    base_var <- paste('base', var, sep = '_')
    act_var <- paste('act', var, sep = '_')
    new_col <- paste('weather', var, sep = '_')
    
    if (act_var %in% names(df_weather_raw)) {
      print(paste('creating new columns:', new_col, sep = ' '))
      df_weather_raw[, new_col] <- df_weather_raw[, base_var] + df_weather_raw[, act_var]
    }
  }
  
  df_weather_target <- df_weather_raw
  
  #insert rows where there are missing dates group by zip
  df_weather_target <- df_weather_target %>%
    group_by(zipcode) %>%
    complete(date = seq(min(df_weather_raw$date), max(df_weather_raw$date), 1)) %>%
    ungroup()
  
  #forward fill weather values
  df_weather_target <- df_weather_target %>%
    fill(names(.))# %>%
  
  
  df_weather_target <- df_weather_target  %>%
    mutate(week_number = isoweek(date))
  
  df_pre_study_mean <- df_weather_target  %>%
    mutate(is_pre_study_period = if_else(date < '2015-07-01', 1,0),
           week_number = isoweek(date)) %>%
    filter(is_pre_study_period == 1) %>%
    group_by(zipcode, week_number) %>%
    summarise(across(everything(), mean))
  
  
  df_demeaned_weather <- merge(df_weather_target, df_pre_study_mean,by=c("week_number", 'zipcode'))
  
  x_names <- names(df_demeaned_weather)[grepl("[.x]$",names(df_demeaned_weather))]
  x_names <- x_names[grepl('base|act|srf|weather',x_names)]
  y_names <- names(df_demeaned_weather)[grepl("[.y]$",names(df_demeaned_weather))]
  y_names <- y_names[grepl('base|act|srf|weather',y_names)]
  
  #demean weather using average by weeknumber of all observations prior to study period
  print('Demeaning weather data')
  df_demeaned_weather_subtract <- df_demeaned_weather[,x_names] - df_demeaned_weather[,y_names]
  names(df_demeaned_weather_subtract) <- gsub("\\.x$","",names(df_demeaned_weather_subtract))
  
  df_demeaned_weather <- cbind(df_demeaned_weather, df_demeaned_weather_subtract)
  
  #create midpoint (mean) temperature
  df_demeaned_weather$temp_midpoint <- rowMeans(df_demeaned_weather[,c('weather_max', 'weather_min')], na.rm=TRUE)

  df_demeaned_weather <- df_demeaned_weather %>%
  group_by(date.x)  %>%
  summarise(across(everything(), mean))
  
  print(max(df_demeaned_weather$date))
  #store shaped raw data
  write_file <- '../Data/Weather/daily_demeaned_weather_data.csv'
  write_csv(df_demeaned_weather, write_file)
  rm(df_demeaned_weather, df_weather_target, df_weather_raw)
  
}

create_monthly_weather_data <- function(lag_months=12) {
  #read weather file
  weather_file <- '../source_data/Weather/energy_zip_weather10292023.xlsx'
  
  df_weather_raw <- read_excel(weather_file)
  df_weather_raw <- df_weather_raw %>%
    mutate(dt = as.Date(paste(y,m,d, sep = "-"), format="%Y-%m-%d")) %>%
    rename(., date = dt) %>%
    filter(date < '2023-09-01')
  
  #subset weather file to target columns
  for (col in grep('base', names(df_weather_raw))) {
    print(names(df_weather_raw)[col])
    var <- str_split(names(df_weather_raw)[col], '_')[[1]][2]
    base_var <- paste('base', var, sep = '_')
    act_var <- paste('act', var, sep = '_')
    new_col <- paste('weather', var, sep = '_')
    
    if (act_var %in% names(df_weather_raw)) {
      print(paste('creating new columns:', new_col, sep = ' '))
      df_weather_raw[, new_col] <- df_weather_raw[, base_var] + df_weather_raw[, act_var]
    }
  }
  
  df_weather_target <- df_weather_raw
  
  #insert rows where there are missing dates group by zip
  df_weather_target <- df_weather_target %>%
    group_by(zipcode) %>%
    complete(date = seq(min(df_weather_raw$date), max(df_weather_raw$date), 1)) %>%
    ungroup()
  
  #forward fill weather values
  df_weather_target <- df_weather_target %>%
    fill(names(.))# %>%
  
  
  df_weather_target <- df_weather_target  %>%
    mutate(week_number = isoweek(date))
  
  df_pre_study_mean <- df_weather_target  %>%
    mutate(is_pre_study_period = if_else(date < '2015-07-01', 1,0),
           week_number = isoweek(date)) %>%
    filter(is_pre_study_period == 1) %>%
    group_by(zipcode, week_number) %>%
    summarise(across(everything(), mean))
  
  
  df_demeaned_weather <- merge(df_weather_target, df_pre_study_mean,by=c("week_number", 'zipcode'))
  
  x_names <- names(df_demeaned_weather)[grepl("[.x]$",names(df_demeaned_weather))]
  x_names <- x_names[grepl('base|act|srf|weather',x_names)]
  y_names <- names(df_demeaned_weather)[grepl("[.y]$",names(df_demeaned_weather))]
  y_names <- y_names[grepl('base|act|srf|weather',y_names)]
  
  #demean weather using average by weeknumber of all observations prior to study period
  print('Demeaning weather data')
  df_demeaned_weather_subtract <- df_demeaned_weather[,x_names] - df_demeaned_weather[,y_names]
  names(df_demeaned_weather_subtract) <- gsub("\\.x$","",names(df_demeaned_weather_subtract))
  
  df_demeaned_weather <- cbind(df_demeaned_weather, df_demeaned_weather_subtract)
  
  
  df_demeaned_weather <- df_demeaned_weather %>%
    group_by(y.x, m.x)  %>%
    summarise(across(everything(), mean)) %>%
    rename(year = y.x, month = m.x) %>%
    mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    relocate(date)  %>%
    ungroup() %>%
    mutate(date_minus_month = as.Date(date) %m+% months(1),
           date_minus_year = as.Date(date) %m+% months(12),
           date_minus_lag = as.Date(date) %m+% months(lag_months))
  
  num_cols <- unlist(lapply(df_demeaned_weather, is.numeric), use.names = FALSE)
  
  print('creating rolling weather features')
  df_demeaned_weather <- cbind(df_demeaned_weather, roll = rollmeanr(df_demeaned_weather[,num_cols], 12, fill = NA))
  
  
  #create midpoint (mean) temperature
  df_demeaned_weather$temp_midpoint <- rowMeans(df_demeaned_weather[,c('weather_max', 'weather_min')], na.rm=TRUE)
  df_demeaned_weather$roll_temp_midpoint <- rowMeans(df_demeaned_weather[,c('roll.weather_max', 'roll.weather_min')], na.rm=TRUE)
  
  #store shaped raw data
  write_file <- '../Data/Weather/monthly_summary_weather_data.csv'
  write_csv(df_demeaned_weather, write_file)
  rm(df_demeaned_weather, df_weather_target, df_weather_raw)
}


create_economic_controls_data <- function(lag_months=12) {
  
  #read and process economic controls 
  df_existhomes <- read_excel("../source_data/Economic/Updated STATA Controls 08-26-2022RB RC.xlsx", sheet = "existhomesales")
  df_newhomes <- read_excel("../source_data/Economic/Updated STATA Controls 08-26-2022RB RC.xlsx", sheet = "newhomesales")
  df_starts <- read_excel("../source_data/Economic/Updated STATA Controls 08-26-2022RB RC.xlsx", sheet = "starts")
  df_consconf <- read_excel("../source_data/Economic/Updated STATA Controls 08-26-2022RB RC.xlsx", sheet = "consconf")
  df_shortrates <- read_excel("../source_data/Economic/Updated STATA Controls 08-26-2022RB RC.xlsx", sheet = "shortrates")
  df_longrates <- read_excel("../source_data/Economic/Updated STATA Controls 08-26-2022RB RC.xlsx", sheet = "longrates")
  df_crude <- read_excel("../source_data/Economic/Updated STATA Controls 08-26-2022RB RC.xlsx", sheet = "crude")
  df_dies <- read_excel("../source_data/Economic/Updated STATA Controls 08-26-2022RB RC.xlsx", sheet = "dies")
  df_gas <- read_excel("../source_data/Economic/Updated STATA Controls 08-26-2022RB RC.xlsx", sheet = "gas")
  df_vix <- read_excel("../source_data/Economic/Updated STATA Controls 08-26-2022RB RC.xlsx", sheet = "vix")
  df_gld <- read_excel("../source_data/Economic/Updated STATA Controls 08-26-2022RB RC.xlsx", sheet = "gld")
  df_phm <- read_excel("../source_data/Economic/Updated STATA Controls 08-26-2022RB RC.xlsx", sheet = "phm")
  df_cpi <- read_excel("../source_data/Economic/Updated STATA Controls 08-26-2022RB RC.xlsx", sheet = "cpi")
  df_retsales <- read_excel("../source_data/Economic/Updated STATA Controls 08-26-2022RB RC.xlsx", sheet = "retsales")
  df_fixedmort <- read_excel("../source_data/Economic/Updated STATA Controls 08-26-2022RB RC.xlsx", sheet = "fixedmort")
  df_labor <- read_excel("../source_data/Economic/Updated STATA Controls 08-26-2022RB RC.xlsx", sheet = "tx_wages-unempl pivot")
  
  df_existhomes <- df_existhomes %>%
    filter(Census_Region == 3) %>% 
    mutate(date = as.Date(paste(Y, M, '01', sep='-'), format = '%Y-%m-%d'),
           date_minus_month = as.Date(date) %m+% months(1),
           date_minus_year = as.Date(date) %m+% months(12),
           date_minus_lag = as.Date(date) %m+% months(lag_months),
           roll_existing_home_sales_sa = rollmean(`Exisiting Home Sales - SA`, k=12, fill = NA, align = 'right')) %>%
    relocate(date)
  
  df_newhomes <- df_newhomes %>%
    filter(region_no == 3) %>%
    mutate(date = as.Date(paste(year, month, '01', sep='-'), format = '%Y-%m-%d'),
           date_minus_month = as.Date(date) %m+% months(1),
           date_minus_year = as.Date(date) %m+% months(12),
           date_minus_lag = as.Date(date) %m+% months(lag_months),
           roll_new_home_sales = rollmean(newhomesalesbyregion, k=12, fill = NA, align = 'right')) %>%
    relocate(date)
  
  df_starts <- df_starts %>%
    filter(region_no == 3) %>%
    mutate(date = as.Date(paste(Y, M, '01', sep='-'), format = '%Y-%m-%d'),
           date_minus_month = as.Date(date) %m+% months(1),
           date_minus_year = as.Date(date) %m+% months(12),
           date_minus_lag = as.Date(date) %m+% months(lag_months),
           roll_starts = rollmean(Starts, k=12, fill = NA, align = 'right'),
           roll_completions = rollmean(Completions, k=12, fill = NA, align = 'right')) %>%
    relocate(date)
  
  df_consconf <- df_consconf %>%
    mutate(date = as.Date(paste(Y, M, '01', sep='-'), format = '%Y-%m-%d'),
           date_minus_month = as.Date(date) %m+% months(1),
           date_minus_year = as.Date(date) %m+% months(12),
           date_minus_lag = as.Date(date) %m+% months(lag_months),
           roll_consconf_idx = rollmean(`Consumer Confidence Index`, k=12, fill = NA, align = 'right'),
           roll_pressituation_idx = rollmean(`Present Situation Index`, k=12, fill = NA, align = 'right'),
           roll_expect_idx = rollmean(`Expectations Index`, k=12, fill = NA, align = 'right')) %>%
    relocate(date)
  
  df_shortrates <- df_shortrates %>% group_by(year(Date), month(Date)) %>%
    summarize(mean_shortr = mean(`Adj Close`, na.rm=T)) %>%
    rename(year = `year(Date)`, month = `month(Date)`) %>%
    mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d'),
           date_minus_month = as.Date(date) %m+% months(1),
           date_minus_year = as.Date(date) %m+% months(12),
           date_minus_lag = as.Date(date) %m+% months(lag_months)) %>%
    relocate(date)  %>%
    ungroup() %>%
    mutate(roll_mean_shrtr = rollmean(mean_shortr, k=12, fill = NA, align = 'right'))
  
  df_longrates <- df_longrates %>% group_by(year(Date), month(Date)) %>%
    summarize(mean_longr = mean(`Adj Close`, na.rm=T)) %>%
    rename(year = `year(Date)`, month = `month(Date)`) %>%
    mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d'),
           date_minus_month = as.Date(date) %m+% months(1),
           date_minus_year = as.Date(date) %m+% months(12),
           date_minus_lag = as.Date(date) %m+% months(lag_months)) %>%
    relocate(date)  %>%
    ungroup() #%>%
    # mutate(roll_mean_longr = rollmean(mean_longr, k=12, fill = NA, align = 'right'))
  
  df_crude <- df_crude %>% group_by(year(Date), month(Date)) %>%
    summarize(mean_crude_price = mean(`Cushing, OK WTI Spot Price FOB (Dollars per Barrel)`, na.rm=T)) %>%
    rename(year = `year(Date)`, month = `month(Date)`) %>%
    mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d'),
           date_minus_month = as.Date(date) %m+% months(1),
           date_minus_year = as.Date(date) %m+% months(12),
           date_minus_lag = as.Date(date) %m+% months(lag_months)) %>%
    relocate(date)  %>%
    ungroup()# %>%
    # mutate(roll_mean_crude = rollmean(mean_crude_price, k=12, fill = NA, align = 'right'))
  
  df_dies <- df_dies %>% group_by(year(Date), month(Date)) %>%
    summarize(mean_dies_price = mean(`New York Harbor No 2 Diesel Low Sulfur Spot Price FOB (Cents per Gallon)`, na.rm=T)) %>%
    rename(year = `year(Date)`, month = `month(Date)`) %>%
    mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d'),
           date_minus_month = as.Date(date) %m+% months(1),
           date_minus_year = as.Date(date) %m+% months(12),
           date_minus_lag = as.Date(date) %m+% months(lag_months)) %>%
    relocate(date)  %>%
    ungroup() #%>%
    # mutate(roll_mean_dies = rollmean(mean_dies_price, k=12, fill = NA, align = 'right'))
  
  df_gas <- df_gas %>% group_by(year(Date), month(Date)) %>%
    summarize(mean_gas_price = mean(`New York Harbor No. 2 Heating Oil Spot Price FOB (Dollars per Gallon)`, na.rm=T)) %>%
    rename(year = `year(Date)`, month = `month(Date)`) %>%
    mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d'),
           date_minus_month = as.Date(date) %m+% months(1),
           date_minus_year = as.Date(date) %m+% months(12),
           date_minus_lag = as.Date(date) %m+% months(lag_months)) %>%
    relocate(date)  %>%
    ungroup() #%>%
    # mutate(roll_mean_gas = rollmean(mean_gas_price, k=12, fill = NA, align = 'right'))
  
  df_vix <- df_vix %>% group_by(year(Date), month(Date)) %>%
    summarize(mean_vix = mean(`Adj Close`, na.rm=T)) %>%
    rename(year = `year(Date)`, month = `month(Date)`) %>%
    mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d'),
           date_minus_month = as.Date(date) %m+% months(1),
           date_minus_year = as.Date(date) %m+% months(12),
           date_minus_lag = as.Date(date) %m+% months(lag_months)) %>%
    relocate(date)  %>%
    ungroup() #%>%
    # mutate(roll_mean_vix = rollmean(mean_vix, k=12, fill = NA, align = 'right'))
  
  df_gld <- df_gld %>% group_by(year(Date), month(Date)) %>%
    summarize(mean_gld = mean(`Adj Close`, na.rm=T)) %>%
    rename(year = `year(Date)`, month = `month(Date)`) %>%
    mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d'),
           date_minus_month = as.Date(date) %m+% months(1),
           date_minus_year = as.Date(date) %m+% months(12),
           date_minus_lag = as.Date(date) %m+% months(lag_months)) %>%
    relocate(date) %>%
    ungroup() #%>%
    # mutate(roll_mean_gld = rollmean(mean_gld, k=12, fill = NA, align = 'right'))
  
  df_cpi <- df_cpi %>%
    mutate(date = as.Date(paste(Y, M, '01', sep='-'), format = '%Y-%m-%d'),
           date_minus_month = as.Date(date) %m+% months(1),
           date_minus_year = as.Date(date) %m+% months(12),
           date_minus_lag = as.Date(date) %m+% months(lag_months)
           # roll_cpi = rollmean(`CPI - All areas -\r\n Seasonally Adjusted`, k=12, fill = NA, align = 'right')
           ) %>%
    rename(cpi = `CPI - All areas -\r\n Seasonally Adjusted`) %>%
    relocate(date)
  
  df_retsales <- df_retsales %>%
    mutate(date = as.Date(paste(Y, M, '01', sep='-'), format = '%Y-%m-%d'),
           date_minus_month = as.Date(date) %m+% months(1),
           date_minus_year = as.Date(date) %m+% months(12),
           date_minus_lag = as.Date(date) %m+% months(lag_months)
           # roll_retail_total_sa = rollmean(`Retail Total SA`, k=12, fill = NA, align = 'right'),
           # roll_retail_seasonal_sa = rollmean(`Seasonal Factors SA`, k=12, fill = NA, align = 'right')
    ) %>%
    relocate(date)
  
  df_fixedmort <- df_fixedmort  %>%
    mutate(date = as.Date(paste(Y, M, '01', sep='-'), format = '%Y-%m-%d'),
           date_minus_month = as.Date(date) %m+% months(1),
           date_minus_year = as.Date(date) %m+% months(12),
           date_minus_lag = as.Date(date) %m+% months(lag_months)
           # roll_fixedmort_rate = rollmean(Rate, k=12, fill = NA, align = 'right')
           ) %>%
    relocate(date)
  
  df_labor <- df_labor %>%
    filter(state == 'TX') %>%
    mutate(date = as.Date(paste(y, m, '01', sep='-'), format = '%Y-%m-%d'),
           date_minus_month = as.Date(date) %m+% months(1),
           date_minus_year = as.Date(date) %m+% months(12),
           date_minus_lag = as.Date(date) %m+% months(lag_months)
           # roll_mean_uner = rollmean(p_uner, k=12, fill = NA, align = 'right')
           ) %>%
    relocate(date)
  
  #source Berkeley Lab 
  ##- Wind Technologies Market Report (https://emp.lbl.gov/wind-technologies-market-report/) Sheet: Wind LCOE Over Time
  ##- Utility Scale Solar (https://emp.lbl.gov/utility-scale-solar) Sheet: LCOE of Utility-Scale PV* Data for 2014 missing for ERCOT, imputed as equivalent to lower 48 value.
  dates <- seq(as.Date('2014-01-01'), as.Date('2021-12-01'), by = '1 month')
  df_lcoe <- data.frame(date = as.Date(dates, format='%Y-%m-%d')) %>%
    mutate(year = year(date),
           month = month(date)) %>%
    left_join(data.frame(year=c(2014,2015,2016,2017,2018,2019,2020,2021),
                         lcoe_wind_dollars_mw=c(48,48,43,40,30,35,29,28),
                         lcoe_sun_dollars_mw=c(133,106,79,70,49,50,40,30))) %>%
    mutate(mean_lcoe_renew_mw = rowMeans(.[,c('lcoe_wind_dollars_mw', 'lcoe_sun_dollars_mw')], na.rm = T),
           date_minus_month = as.Date(date) %m+% months(1),
           date_minus_lag = as.Date(date) %m+% months(lag_months)) %>%
    select(-date)
  
  ### adjust lcoe dollars for inflation
  df_bls <- read_csv('../source_data/Economic/bls_cpi_base_2022.12.csv')
  df_lcoe <- df_lcoe %>%
    left_join(df_bls, by=c('year', 'month')) %>%
    mutate(lcoe_wind_dollars_mw = lcoe_wind_dollars_mw*cpi_base_2022,
           lcoe_sun_dollars_mw = lcoe_sun_dollars_mw*cpi_base_2022)
        
  print("joining economics data sets")
  df_economics <- df_existhomes %>%
    left_join(df_newhomes, by='date_minus_month') %>%
    left_join(df_starts, by='date_minus_month') %>%
    left_join(df_consconf, by='date_minus_month') %>%
    left_join(df_shortrates, by='date_minus_month') %>%
    left_join(df_longrates, by='date_minus_month') %>%
    left_join(df_crude, by='date_minus_month') %>%
    left_join(df_dies, by='date_minus_month') %>%
    left_join(df_gas, by='date_minus_month') %>%
    left_join(df_vix, by='date_minus_month') %>%
    left_join(df_gld, by='date_minus_month') %>%
    left_join(df_cpi, by='date_minus_month') %>%
    left_join(df_retsales, by='date_minus_month') %>%
    left_join(df_fixedmort, by='date_minus_month') %>%
    left_join(df_labor, by='date_minus_month') %>%
    left_join(df_lcoe, by='date_minus_month') %>%
    mutate(date_minus_year = as.Date(date) %m+% months(12),
           date_minus_lag = as.Date(date) %m+% months(lag_months))
  
  df_economics <- df_economics %>%
    mutate(p_labf_hund_thousands = p_labf/100000)
  
  print('cleaning names')
  df_economics <- df_economics[, !grepl('\\.y|\\.x', names(df_economics))] %>%
    relocate(date_minus_month, date_minus_year)
  
  num_cols <- unlist(lapply(df_economics, is.numeric), use.names = FALSE)  
  
  print('rolling values by twelve months')
  df_economics <- cbind(df_economics, roll = rollmeanr(df_economics[,num_cols], 12, fill = NA))
  
  write_csv(df_economics, '../Data/Economic/montly_economic_controls.csv')
  rm(df_economics, df_existhomes, df_newhomes, df_starts,df_consconf, df_shortrates, df_longrates, df_crude, df_dies, df_gas, df_vix,df_gld, df_cpi, df_retsales,df_fixedmort,df_labor,df_lcoe)
}

create_matching_dataset <- function(){
  
  df_gen_cap <- read.csv('../Data/ERCOT Compiled Data/gen_cap_and_adder_interval.csv')
  
  df_gen_cap <- df_gen_cap %>%
    filter(settlement_type == 'FINAL' & date > '2014-07-01') %>%
    mutate(total_pa = rtorpa+rtordpa) %>%
    distinct(date, interval, repeated_hour_flag, hour, minute,
             int_tot_gen_m_wh, int_tot_gen_gas, int_tot_gen_non_gas, int_tot_gen_renewable,
             int_tot_gen_other, int_tot_spin_cap_mw, prc, rtolcap, rtoffcap, rtorpa, rtordpa, system_lamda, rttotcap,
             total_pa, hb_busavg, hb_hubavg, hb_busavg_energy_only, hb_hubavg_energy_only) %>%
    mutate(scarcity_measure = (int_tot_gen_m_wh/(rttotcap+int_tot_gen_m_wh))*100,
           is_scarcity_pa_active = if_else(total_pa > 0, 1,0),
           year = year(date),
           month= month(date),
           day = day(date),
           date = as.Date(date))
  
  #create nameplate cap controls
  df_plant_gen_id_phase_timeline <- read_csv('../Data/EIA Compiled Data/EIA860M_timeline.csv')
  df_nameplate_capacity <- df_plant_gen_id_phase_timeline %>%
    filter(status_phase %in% c(7,8)) %>%
    group_by(date, energy_source_code_group) %>%
    summarize(nameplate_capacity = sum(net_summer_capacity_mw, na.rm=TRUE)) %>%
    pivot_wider(id_cols = date, names_from = energy_source_code_group, values_from=nameplate_capacity) %>%
    mutate(year = year(date),
           month = month(date))
  
  df_gen_cap <- df_gen_cap %>%
    left_join(df_nameplate_capacity,
              by=c('year', 'month'))
  
  rm(df_nameplate_capacity)
  
  #create economic controls
  df_economics <- read_csv('../Data/Economic/montly_economic_controls.csv')
  
  df_economics <- df_economics %>% select(date,p_labf, p_uner, cpi,mean_shortr,lcoe_wind_dollars_mw,lcoe_sun_dollars_mw) %>%
    mutate(year = year(date),
           month = month(date))
  
  df_gen_cap <- df_gen_cap %>% 
    left_join(df_economics, 
              by=c('year', 'month'))
  
  rm(df_economics)
  
  #create PNM controls
  df_pnm <- read_csv('../Data/ERCOT Compiled Data/compiled_zip_csv_pnm_files_2011_2022.csv')
  df_pnm <- df_pnm %>%
    mutate(date= as.Date(AsOfDate, format='%m/%d/%Y %H:%M:%S'),
           delta_pnm = if_else((PeakerNetMargin - lag(PeakerNetMargin)) >= 0, PeakerNetMargin - lag(PeakerNetMargin), 0),
           cum_delta_pnm = cumsum(ifelse(is.na(delta_pnm), 0, delta_pnm)) + delta_pnm*0) %>%
    mutate(date_only = as.Date(date))
  
  df_gen_cap <- df_gen_cap %>% 
    left_join(df_pnm, 
              by=c('date.x' = 'date_only'))
  
  rm(df_pnm)
  
  #create hh gas price controls
  df_ng_prices <- read_csv('../source_data/Natural Gas Prices/Henry_Hub_Natural_Gas_Spot_Price.csv',skip = 4)
  
  df_ng_prices <- df_ng_prices %>%
    mutate(date = as.Date(Day, format='%m/%d/%Y')) %>%
    rename(ng_price = `Henry Hub Natural Gas Spot Price Dollars per Million Btu`)
  
  df_gen_cap <- df_gen_cap %>% rename(date = date.x) %>% 
    left_join(df_ng_prices, 
              by=('date' = 'date')) %>%
    fill(ng_price, .direction = 'down')
  
  rm(df_ng_prices)
  
  #create weather controls
  df_weather <- read_csv('../Data/Weather/daily_demeaned_weather_data.csv')
  
  df_gen_cap <- df_gen_cap %>% 
    left_join(df_weather %>% select(date.x, temp_midpoint, weather_wnds, weather_max, weather_min) %>% rename(date = date.x), 
              by=('date.x' = 'date'))
  
  rm(df_weather)
  
  #create gini controls
  df_gini <- read_csv('../Data/EIA Compiled Data/gini_summary.csv') 
  df_gini <- df_gini%>% 
    mutate(year = year(date),
           month = month(date))
  
  df_gen_cap <- df_gen_cap %>% 
    left_join(df_gini, 
              by=c('year', 'month'))
  
  rm(df_gini)  
  write_csv(df_gen_cap, '../Data/ERCOT Compiled Data/ercot_scarcity_pricing_matching_set.csv')
  
}
