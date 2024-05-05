
load_EIA860M_and_EIA932A <- function(){
  
  df_EIA860M <- load_EIA860M()
  df_EIA923A <- load_EIA923A(df_EIA860M)
  
  EIA_data <- list('EIA860M' = df_EIA860M, 'EIA923A' = df_EIA923A)
  
  return(EIA_data)
}


load_EIA860M <- function(){
  #load (energy demand) data
  eia_m_files <- list.files('../source_data/EIA860M', full.names = T)
  
  print(eia_m_files)
  
  #instatiate data frame to recieve load data from various files
  df_operating <- data.frame()
  df_planned <- data.frame()
  df_retired <- data.frame()
  df_postponed <- data.frame()
  
  #loop through files and sheet to create one data frame
  print("Starting to import EIA860M files")
  for (file in eia_m_files) {
    
    year = str_sub(file, -9, -6)
    month = str_extract(file, "(?<=EIA860M/)(.*)(?=_generator)")
    print(year)
    print(month)
    print(paste('importing file: ', file, sep = " "))
    sheetnames <- excel_sheets(file)
    if (year %in% c(2021,2022)) {
      mylist <- lapply(sheetnames, function(x)
        read_excel(file,x, col_names = TRUE,skip = 2))
    }
    else {
      mylist <- lapply(sheetnames, function(x)
        read_excel(file,x, col_names = TRUE,skip = 1))
    }
    
    # #col_names is TRUE by default so you can use this without anonymous function like
    # #mylist <- lapply(sheetnames, read_excel, path = path, skip = 1)
    # 
    # # name the dataframes
    # 
    # 
    names(mylist) <- paste(sheetnames, year, month, sep = "_")
    
    # 
    
    # 
    # #use Map to bind all the elements of the list into a dataframe
    my_list <- Map(cbind, mylist, Cluster = names(mylist))
    
    print(1)
    print(do.call("rbind", my_list[1]) %>% clean_names() %>% names())
    df_operating_individual <- do.call("rbind", my_list[1]) %>% clean_names() %>% filter(plant_state %in% c('TX', 'OK', 'LA', 'AR', 'NM')) %>% mutate(record_year = as.integer(year),
                                                                                                                         record_month = month, 
                                                                                                                         record_category = 'operating')
    print(2)
    df_planned_individual <- do.call("rbind", my_list[2]) %>% clean_names() %>% filter(plant_state %in% c('TX', 'OK', 'LA', 'AR', 'NM')) %>% mutate(record_year = as.integer(year),
                                                                                                                       record_month = month,
                                                                                                                       record_category = 'planned')
    print(3)
    df_retired_individual <- do.call("rbind", my_list[3]) %>% clean_names() %>% filter(plant_state %in% c('TX', 'OK', 'LA', 'AR', 'NM')) %>% mutate(record_year = as.integer(year),
                                                                                                                       record_month = month,
                                                                                                                       record_category = 'retired')
    print(4)
    df_postponed_individual <- do.call("rbind", my_list[4]) %>% clean_names() %>% filter(plant_state %in% c('TX', 'OK', 'LA', 'AR', 'NM')) %>% mutate(record_year = as.integer(year),
                                                                                                                         record_month = month,
                                                                                                                         record_category = 'cancelled_postponed')
    
    df_operating_individual <- df_operating_individual %>% mutate(entity_id = as.double(entity_id))
    df_planned_individual <- df_planned_individual %>% mutate(entity_id = as.double(entity_id))
    df_retired_individual <- df_retired_individual %>% mutate(entity_id = as.double(entity_id))
    df_postponed_individual <- df_postponed_individual %>% mutate(entity_id = as.double(entity_id))
    
    df_operating <- bind_rows(df_operating, df_operating_individual) 
    df_planned <- bind_rows(df_planned, df_planned_individual)
    df_retired <- bind_rows(df_retired, df_retired_individual)
    df_postponed <- bind_rows(df_postponed, df_postponed_individual)
    
    
    
  }### end import of EIA files
  
  print('Assigning integers to string months')
  months_str <- c("april","august", "december", "february", "january", "july","june","march","may","november","october","september")
  months_int <- c(4,8,12,2,1,7,6,3,5,11,10,9)
  
  df_operating <- df_operating %>%
    mutate(record_month_int = as.integer(plyr::mapvalues(record_month, months_str, months_int)),
           year = as.integer(record_year)) %>%
    rename(month = record_month_int)
  
  df_planned <- df_planned %>%
    mutate(record_month_int = as.integer(plyr::mapvalues(record_month, months_str, months_int)),
           year = as.integer(record_year)) %>%
    rename(month = record_month_int)
  
  df_retired <- df_retired %>%
    mutate(record_month_int = as.integer(plyr::mapvalues(record_month, months_str, months_int)),
           year = as.integer(record_year)) %>%
    rename(month = record_month_int)
  
  df_postponed <- df_postponed %>%
    mutate(record_month_int = as.integer(plyr::mapvalues(record_month, months_str, months_int)),
           year = as.integer(record_year)) %>%
    rename(month = record_month_int)
  
  ##improve this logic as it is not specific to the observations that I've identified as data quality issues.
  print('Altering lat long of unique records where values fall outside of Texas')
  index_wrong_long_2016 <- which(df_planned$longitude >0)
  corrected_values_wrong_long_2016 <- df_planned[index_wrong_long_2016 ,c('longitude')]*(-1)
  
  corrected_values_wrong_long_2016 -> df_planned[index_wrong_long_2016 ,c('longitude')]
  
  index_wrong_lat_long_2019 <- which(df_planned$latitude > 45)
  
  corrected_values_wrong_long_as_lat_2019 <- df_planned[index_wrong_lat_long_2019,c('latitude')]*(-1)
  corrected_values_wrong_lat_as_long_2019 <- df_planned[index_wrong_lat_long_2019,c('longitude')]*(-1)
  corrected_values_wrong_long_as_lat_2019 -> df_planned[index_wrong_lat_long_2019,c('longitude')]
  corrected_values_wrong_lat_as_long_2019 -> df_planned[index_wrong_lat_long_2019,c('latitude')]
  
  index_wrong_long_2021 <- which(df_planned$longitude > -91.5)
  #city of pearson loacated at as 28.888451556135117, -99.09705861354483, altering longitude to locate longitude at same as city of pearson given county location of "Pearson"
  df_planned[index_wrong_long_2021,'longitude'] <- -99.09705861354483
  
  print('Filling NA values for balancing authority code at the plant_id level with same plant_id in other years')
  df_operating[df_operating == "NA"] <- NA
  df_planned[df_planned == "NA"] <- NA
  df_retired[df_retired == "NA"] <- NA
  df_postponed[df_postponed == "NA"] <- NA
  
  operating_plant_id_na_ba <- df_operating[which(is.na(df_operating$balancing_authority_code)),'plant_id']
  planned_plant_id_na_ba <- df_planned[which(is.na(df_planned$balancing_authority_code)),'plant_id']
  retired_plant_id_na_ba <- df_retired[which(is.na(df_retired$balancing_authority_code)),'plant_id']
  postponed_plant_id_na_ba <- df_postponed[which(is.na(df_postponed$balancing_authority_code)),'plant_id']
  
  df_ba_fill_operating <- df_operating %>% filter(plant_id %in% operating_plant_id_na_ba & !is.na(balancing_authority_code)) %>% distinct(plant_id, balancing_authority_code)
  df_ba_fill_planned <- df_planned %>% filter(plant_id %in% planned_plant_id_na_ba & !is.na(balancing_authority_code)) %>% distinct(plant_id, balancing_authority_code)
  df_ba_fill_retired <- df_retired %>% filter(plant_id %in% retired_plant_id_na_ba & !is.na(balancing_authority_code)) %>% distinct(plant_id, balancing_authority_code)
  df_ba_fill_postponed <- df_postponed %>% filter(plant_id %in% postponed_plant_id_na_ba & !is.na(balancing_authority_code)) %>% distinct(plant_id, balancing_authority_code)
  
  print(paste('Altering BA of', length(unique(operating_plant_id_na_ba)), 'operating plant ids with data from other year/months', sep = ' '))
  
  df_operating <- df_operating %>% left_join(df_ba_fill_operating, by='plant_id' ) %>% rename(balancing_authority_code = balancing_authority_code.x) %>% mutate(unfilled_balancing_authority_code = balancing_authority_code,
                                                                                                                                                                balancing_authority_code = if_else(is.na(balancing_authority_code), balancing_authority_code.y, balancing_authority_code),
                                                                                                                                                                is_filled_ba = if_else(is.na(balancing_authority_code), 1, 0)) %>% select(-balancing_authority_code.y)
  df_planned <- df_planned %>% left_join(df_ba_fill_planned, by='plant_id' ) %>% rename(balancing_authority_code = balancing_authority_code.x) %>% mutate(unfilled_balancing_authority_code = balancing_authority_code,
                                                                                                                                                          balancing_authority_code = if_else(is.na(balancing_authority_code), balancing_authority_code.y, balancing_authority_code),
                                                                                                                                                          is_filled_ba = if_else(is.na(balancing_authority_code), 1, 0)) %>% select(-balancing_authority_code.y)
  df_retired <- df_retired %>% left_join(df_ba_fill_retired, by='plant_id' ) %>% rename(balancing_authority_code = balancing_authority_code.x) %>% mutate(unfilled_balancing_authority_code = balancing_authority_code,
                                                                                                                                                          balancing_authority_code = if_else(is.na(balancing_authority_code), balancing_authority_code.y, balancing_authority_code),
                                                                                                                                                          is_filled_ba = if_else(is.na(balancing_authority_code), 1, 0)) %>% select(-balancing_authority_code.y)
  df_postponed <- df_postponed %>% left_join(df_ba_fill_postponed, by='plant_id' ) %>% rename(balancing_authority_code = balancing_authority_code.x) %>% mutate(unfilled_balancing_authority_code = balancing_authority_code,
                                                                                                                                                                balancing_authority_code = if_else(is.na(balancing_authority_code), balancing_authority_code.y, balancing_authority_code),
                                                                                                                                                                is_filled_ba = if_else(is.na(balancing_authority_code), 1, 0)) %>% select(-balancing_authority_code.y)
  
  df_operating <- df_operating %>% mutate(report_category = 'operating')
  df_planned <- df_planned %>% mutate(report_category = 'planned')
  df_retired <- df_retired %>% mutate(report_category = 'retired')
  df_postponed <- df_postponed %>% mutate(report_category = 'postponed')
  
  
  df_EIA860M <- bind_rows(df_operating,df_planned,df_retired,df_postponed)

  print('Check if point is within ERCOT region')
  sdf_ercot_poly <- geojson_read('../source_data/Geospatial/NERC_Regions.geojson', what='sp')
  sdf_ercot_poly_fortified <- tidy(sdf_ercot_poly)
  df_EIA860M <- df_EIA860M %>%
    mutate(in_ercot_shape = point.in.polygon(df_EIA860M$latitude, df_EIA860M$longitude, sdf_ercot_poly_fortified$lat, sdf_ercot_poly_fortified$long))
  
  
  print('Assigining NULL balancing Authority Codes to ERCOT if lat long falls within ERCOT polygon')
  n_plants_to_alter <- df_EIA860M %>%
    filter(is.na(balancing_authority_code),  in_ercot_shape == 1) %>%
    distinct(plant_id) %>%
    nrow()
  
  n_plants_to_alter_operating <- df_EIA860M %>%
    filter(is.na(balancing_authority_code),  in_ercot_shape == 1, record_category == 'operating') %>%
    distinct(plant_id) %>%
    nrow()
  
  write_csv(df_EIA860M %>%
              filter(is.na(balancing_authority_code),  in_ercot_shape == 1), '../Data/EIA Compiled Data/null_ba_ercot_shape.csv')
  print(paste('Altering ', n_plants_to_alter, 'plants from NULL BA to ERCO'))
  print(paste(round(n_plants_to_alter_operating/n_plants_to_alter,2)*100, '% altered plants reach operation ERCO:', n_plants_to_alter_operating, 'plants', sep = ' ' ))
  
  
  # print('standardizing operating year for plant_gen_id 64801_GEN3_NG_IC to 2020, observation for june 2021 reads 202')
  # df_EIA860M[which(df_EIA860M$plant_gen_id == '64801_GEN3_NG_IC'),'operating_year'] = 2020
  
  #create energy source code groups, unique generator id as plant_gen_id, and recorded first operation date and recorded retirement date
  df_EIA860M <- df_EIA860M %>%
    mutate(unfilled_pre_geocoord_ba_code = balancing_authority_code,
           balancing_authority_code = if_else(is.na(balancing_authority_code) & in_ercot_shape == 1, 'ERCO', balancing_authority_code),
           is_fill_ba_by_geocoords = if_else(is.na(balancing_authority_code) & in_ercot_shape == 1, 1, 0)) %>%
    mutate(energy_source_code_group = case_when(energy_source_code == 'NG' ~ 'NG',
                                                energy_source_code %in% c('WND', 'SUN') ~ 'Renewables',
                                                TRUE ~ 'Other')) %>%
    # mutate(plant_gen_id = paste(plant_id, generator_id, energy_source_code, prime_mover_code, sep = '_')) %>%
    mutate(plant_gen_id = paste(plant_id, generator_id, sep = '_')) %>%
    group_by(plant_gen_id) %>%
    mutate(recorded_first_operation_date = as.Date(paste(min(operating_year, na.rm = T), min(operating_month, na.rm = T), '01', sep = '-'), format='%Y-%m-%d'),
           recorded_first_retirement_date = as.Date(paste(min(retirement_year, na.rm = T), min(retirement_month, na.rm = T), '01', sep = '-'), format='%Y-%m-%d')) %>%
    ungroup()
  
  ### Standardize versions of ERCOT balancing authority code
  df_EIA860M <- df_EIA860M %>% mutate(balancing_authority_code = ifelse(balancing_authority_code == 'ERCOT', 'ERCO', balancing_authority_code))
  

  
  print('Calculate Market Capacity Gini')
  df_EIA860M <- calculate_market_cap_gini(df_EIA860M)
  
  print('Calculate Natural Gas Capacity Gini')
  df_EIA860M <- calculate_ng_cap_gini(df_EIA860M)
  
  print('Calculate Wind Capacity Gini')
  df_EIA860M <- calculate_wnd_cap_gini(df_EIA860M)

  print('Identify Gen. Entry and Exit by Record Category and Status')
  df_EIA860M <- identify_entry_and_exit_months_by_record_category_and_status(df_EIA860M)
  
  return(df_EIA860M)
  
}


load_EIA860A <- function(){
  eia_a_files <- c('../source_data/EIA860A/2___Plant_Y2013.xlsx',
                   '../source_data/EIA860A/2___Plant_Y2014.xlsx',
                   '../source_data/EIA860A/2___Plant_Y2015.xlsx',
                   '../source_data/EIA860A/2___Plant_Y2016.xlsx',
                   '../source_data/EIA860A/2___Plant_Y2017.xlsx',
                   '../source_data/EIA860A/2___Plant_Y2018.xlsx',
                   '../source_data/EIA860A/2___Plant_Y2019.xlsx',
                   '../source_data/EIA860A/2___Plant_Y2020.xlsx',
                   '../source_data/EIA860A/2___Plant_Y2021_Early_Release.xlsx'
  )
  
  df_EIA860A <- data.frame()
  
  print("Starting to import EIA860A files")
  for (file in eia_a_files) {
    
    if (grepl('Early_Release', file)) {
      year = str_sub(file, -23, -20)
    }
    else {
      year = str_sub(file, -9, -6)
    }
    
    print(year)
    if (year == 2021) {
      print(paste('importing file: ', file, sep = " "))
      sheetnames <- excel_sheets(file)
      mylist <- lapply(sheetnames, function(x)
        read_excel(file,x, col_names = TRUE,skip = 2))
      
      names(mylist) <- paste(sheetnames, year, sep = '_')
      #col_names is TRUE by default so you can use this without anonymous function like
      #mylist <- lapply(sheetnames, read_excel, path = path, skip = 1)
      
      # name the dataframes
      
      my_list <- Map(cbind, mylist, Cluster = names(mylist))
      
      #use Map to bind all the elements of the list into a dataframe
      
      
      
      df_EIA860A_individual <- do.call("rbind", my_list)
      
    }
    else {
      
      print(paste('importing file: ', file, sep = " "))
      sheetnames <- excel_sheets(file)
      mylist <- lapply(sheetnames, function(x)
        read_excel(file,x, col_names = TRUE,skip = 1))
      
      names(mylist) <- paste(sheetnames, year, sep = '_')
      #col_names is TRUE by default so you can use this without anonymous function like
      #mylist <- lapply(sheetnames, read_excel, path = path, skip = 1)
      
      # name the dataframes
      
      my_list <- Map(cbind, mylist, Cluster = names(mylist))
      
      #use Map to bind all the elements of the list into a dataframe
      
      
      
      df_EIA860A_individual <- do.call("rbind", my_list)
      
    }
    
    df_EIA860A <- bind_rows(df_EIA860A, df_EIA860A_individual)
  }  
  
  
  df_EIA860A <- clean_names(df_EIA860A)
  df_EIA860A <- df_EIA860A %>% filter(state == 'TX' )  %>% mutate(year = str_sub(cluster, -4,-1))
  
  print('Altering incorrect lat long values')
  index_wrong_lat_long <- which(df_EIA860A$longitude >-90 & df_EIA860A$plant_code == 62715)
  corrected_values_wrong_lat_as_long <- df_EIA860A[index_wrong_lat_long ,c('longitude')]*(-1)
  corrected_values_wrong_long_as_lat <- df_EIA860A[index_wrong_lat_long ,c('latitude')]*(-1)
  
  corrected_values_wrong_lat_as_long -> df_EIA860A[index_wrong_lat_long ,c('latitude')]
  corrected_values_wrong_long_as_lat -> df_EIA860A[index_wrong_lat_long ,c('longitude')]
  
  # Altering lat long of Motiva Plant 50973 to: 29.895464715785305, -93.95054632589161
  index_wrong_lat_long <- which(df_EIA860A$latitude > 37 & df_EIA860A$plant_code == 50973)
  df_EIA860A[index_wrong_lat_long ,c('longitude')] <- -93.95054632589161
  df_EIA860A[index_wrong_lat_long ,c('latitude')] <- 29.895464715785305
  
  
  return(df_EIA860A)
}
# 
# load_EIA860A_gen_historical <- function() {
#   eia_a_files <- c('../Data/EIA860A/3_1_Generator_Y2014.xlsx',
#                    '../Data/EIA860A/3_1_Generator_Y2013.xlsx',
#                    '../Data/EIA860A/GeneratorY2012.xlsx',
#                    '../Data/EIA860A/GeneratorY2011.xlsx',
#                    '../Data/EIA860A/GeneratorsY2010.xls',
#                    '../Data/EIA860A/GeneratorY09.xls',
#                    '../Data/EIA860A/GenY08.xls',
#                    '../Data/EIA860A/GenY07.xls',
#                    '../Data/EIA860A/GenY06.xls',
#                    '../Data/EIA860A/GenY05.xls'
#   )
#   
#   #instatiate data frame to recieve load data from various files
#   df_operating <- data.frame()
#   df_planned <- data.frame()
#   df_retired <- data.frame()
#   
#   print("Starting to import EIA860A files")
#   for (file in eia_a_files) {
#     
#     
#     year = gsub(".*[Y]([^.]+)[.xls].*", "\\1", file)[[1]]
#     # year = paste(year[1,2], collapse = '')
#     print(nchar(year))
#     if (nchar(year) !=4) {
#       year = paste('20', year, sep = '')
#     }
#     
#     
#     print(year)
#     print(paste('importing file: ', file, sep = " "))
#     
#     if (year %in% c('2012','2013','2014')) {
#       sheetnames <- excel_sheets(file)
#       
#       mylist <- lapply(sheetnames, function(x)
#         read_excel(file,x, col_names = TRUE,skip = 1))
#       
#       print(sheetnames)
#       # print(mylist)
#       names(mylist) <- paste(sheetnames, year, sep = "_")
#       
#       # 
#       
#       # 
#       # #use Map to bind all the elements of the list into a dataframe
#       my_list <- Map(cbind, mylist, Cluster = names(mylist))
#       # 
#       print(year)
#       print(1)
#       print(do.call("rbind", my_list[1]) %>% clean_names() %>% names())
#       df_operating_individual <- do.call("rbind", my_list[1]) %>% clean_names() %>% filter(state %in% c('TX', 'OK', 'LA', 'AR', 'NM')) %>% mutate(record_year = as.integer(year),
#                                                                                                                                                   record_category = 'operating')
#       # cols.num <- c("summer_capability","winter_capability", 'turbines', 'planned_uprates_net_summer_cap', 'planned_uprates_net_winter_cap', 'planned_uprates_month',
#       #               'planned_uprates_year', 'planned_derates_net_summer_cap', 'planned_derates_net_winter_cap', 'planned_derates_month', 'planned_derates_year',
#       #               'planned_repower_month','planned_repower_year', 'other_mod_month', 'other_mod_year', 'planned_retirement_month', 'planned_retirement_year')
#       # df_operating_individual[cols.num] <- sapply(df_operating_individual[cols.num],as.numeric)
#       df_operating <- bind_rows(df_operating, df_operating_individual)
#       
#       print(2)
#       print(do.call("rbind", my_list[2]) %>% clean_names() %>% names())
#       df_planned_individual <- do.call("rbind", my_list[2]) %>% clean_names() %>% filter(state %in% c('TX', 'OK', 'LA', 'AR', 'NM')) %>% mutate(record_year = as.integer(year),
#                                                                                                                                                 record_category = 'planned')
#       # cols.num <- c("summer_capability", 'winter_capability', 'turbines')
#       # df_planned_individual[cols.num] <- sapply(df_planned_individual[cols.num],as.numeric)
#       df_planned <- bind_rows(df_planned, df_planned_individual)
#       
#       print(3)
#       print(do.call("rbind", my_list[3]) %>% clean_names() %>% names())
#       df_retired_individual <- do.call("rbind", my_list[3]) %>% clean_names() %>% filter(state %in% c('TX', 'OK', 'LA', 'AR', 'NM')) %>% mutate(record_year = as.integer(year),
#                                                                                                                                                 record_category = 'retired_cancelled')
#       # cols.num <- c('planned_uprates_net_summer_cap')
#       # df_retired_individual[cols.num] <- sapply(df_retired_individual[cols.num],as.numeric)
#       
#       df_retired <- bind_rows(df_retired, df_retired_individual)
#       
#     }
#     
#     if (year %in% c('2011')) {
#       sheetnames <- excel_sheets(file)
#       
#       mylist <- lapply(sheetnames, function(x)
#         read_excel(file,x, col_names = TRUE,skip = 1))
#       
#       print(sheetnames)
#       # print(mylist)
#       names(mylist) <- paste(sheetnames, year, sep = "_")
#       
#       # 
#       
#       # 
#       # #use Map to bind all the elements of the list into a dataframe
#       my_list <- Map(cbind, mylist, Cluster = names(mylist))
#       # 
#       print(year)
#       print(1)
#       print(do.call("rbind", my_list[1]) %>% clean_names() %>% names())
#       df_operating_individual <- do.call("rbind", my_list[1]) %>% clean_names() %>% filter(state %in% c('TX', 'OK', 'LA', 'AR', 'NM')) %>% mutate(record_year = as.integer(year),
#                                                                                                                                                   record_category = 'operating')
#       cols.num <- c("summer_capability","winter_capability", 'turbines', 'planned_uprates_net_summer_cap', 'planned_uprates_net_winter_cap', 'planned_uprates_month',
#                     'planned_uprates_year', 'planned_derates_net_summer_cap', 'planned_derates_net_winter_cap', 'planned_derates_month', 'planned_derates_year',
#                     'planned_repower_month','planned_repower_year', 'other_mod_month', 'other_mod_year', 'planned_retirement_month', 'planned_retirement_year')
#       df_operating_individual[cols.num] <- sapply(df_operating_individual[cols.num],as.numeric)
#       df_operating <- bind_rows(df_operating, df_operating_individual)
#       
#       print(2)
#       print(do.call("rbind", my_list[2]) %>% clean_names() %>% names())
#       df_planned_individual <- do.call("rbind", my_list[2]) %>% clean_names() %>% filter(state %in% c('TX', 'OK', 'LA', 'AR', 'NM')) %>% mutate(record_year = as.integer(year),
#                                                                                                                                                 record_category = 'planned')
#       cols.num <- c("summer_capability", 'winter_capability', 'turbines')
#       df_planned_individual[cols.num] <- sapply(df_planned_individual[cols.num],as.numeric)
#       df_planned <- bind_rows(df_planned, df_planned_individual)
#       
#       print(3)
#       print(do.call("rbind", my_list[3]) %>% clean_names() %>% names())
#       df_retired_individual <- do.call("rbind", my_list[3]) %>% clean_names() %>% filter(state %in% c('TX', 'OK', 'LA', 'AR', 'NM')) %>% mutate(record_year = as.integer(year),
#                                                                                                                                                 record_category = 'retired_cancelled')
#       cols.num <- c("summer_capability","winter_capability", 'turbines', 'operating_month', 'operating_year', 'retirement_month', 'retirement_year', 'planned_derates_net_summer_cap',
#                     'planned_derates_net_winter_cap', 'planned_derates_month', 'planned_derates_year', 'planned_repower_month', 'planned_repower_year', 'other_mod_month',
#                     'other_mod_year', 'planned_uprates_net_summer_cap', 'planned_uprates_net_winter_cap', 'planned_uprates_month', 'planned_uprates_year'
#       )
#       df_retired_individual[cols.num] <- sapply(df_retired_individual[cols.num],as.numeric)
#       
#       df_retired <- bind_rows(df_retired, df_retired_individual)
#       
#     }
#     if (year %in% c('2009','2010')) {
#       sheetnames <- excel_sheets(file)
#       
#       mylist <- lapply(sheetnames, function(x)
#         read_excel(file,x, col_names = TRUE,skip = 0))
#       
#       print(sheetnames)
#       # print(mylist)
#       names(mylist) <- paste(sheetnames, year, sep = "_")
#       
#       # 
#       
#       # 
#       # #use Map to bind all the elements of the list into a dataframe
#       my_list <- Map(cbind, mylist, Cluster = names(mylist))
#       print(year)
#       print(1)
#       print(do.call("rbind", my_list[1]) %>% clean_names() %>% names())
#       df_operating_individual <- do.call("rbind", my_list[1]) %>% clean_names() %>% filter(state %in% c('TX', 'OK', 'LA', 'AR', 'NM')) %>% mutate(record_year = as.integer(year),
#                                                                                                                                                   record_category = 'operating')
#       cols.num <- c('planned_uprates_net_summer_cap', 'planned_uprates_net_winter_cap', 'planned_uprates_month', 'planned_uprates_year', 'planned_derates_net_summer_cap',
#                     'planned_derates_net_winter_cap', 'planned_derates_month', 'planned_derates_year', 'planned_repower_month', 'planned_repower_year', 'other_mod_month',
#                     'other_mod_year', 'planned_retirement_month', 'planned_retirement_year', 'summer_capability')
#       df_operating_individual[cols.num] <- sapply(df_operating_individual[cols.num],as.numeric)
#       df_operating <- bind_rows(df_operating, df_operating_individual)
#       
#       print(2)
#       print(do.call("rbind", my_list[2]) %>% clean_names() %>% names())
#       df_planned_individual <- do.call("rbind", my_list[2]) %>% clean_names() %>% filter(state %in% c('TX', 'OK', 'LA', 'AR', 'NM')) %>% mutate(record_year = as.integer(year),
#                                                                                                                                                 record_category = 'planned')
#       cols.num <- c("summer_capability", 'winter_capability', 'turbines')
#       df_planned_individual[cols.num] <- sapply(df_planned_individual[cols.num],as.numeric)
#       df_planned <- bind_rows(df_planned, df_planned_individual)
#       
#       print(3)
#       print(do.call("rbind", my_list[3]) %>% clean_names() %>% names())
#       df_retired_individual <- do.call("rbind", my_list[3]) %>% clean_names() %>% filter(state %in% c('TX', 'OK', 'LA', 'AR', 'NM')) %>% mutate(record_year = as.integer(year),
#                                                                                                                                                 record_category = 'retired_cancelled')
#       cols.num <- c("summer_capability", 'winter_capability', 'turbines', 'operating_month', 'operating_year', 'retirement_month', 'retirement_year', 'planned_derates_net_summer_cap',
#                     'planned_derates_net_winter_cap', 'planned_derates_month', 'planned_derates_year', 'planned_repower_month', 'planned_repower_year', 'other_mod_month',
#                     'other_mod_year', 'planned_uprates_net_summer_cap', 'planned_uprates_net_winter_cap', 'planned_uprates_month', 'planned_uprates_year')
#       df_retired_individual[cols.num] <- sapply(df_retired_individual[cols.num],as.numeric)
#       
#       df_retired <- bind_rows(df_retired, df_retired_individual)
#       
#       
#       
#       
#     }
#     if (year %in% c('2005','2006', '2007', '2008')) {
#       sheetnames <- excel_sheets(file)
#       
#       mylist <- lapply(sheetnames, function(x)
#         read_excel(file,x, col_names = TRUE,skip = 0))
#       
#       print(sheetnames)
#       # print(mylist)
#       names(mylist) <- paste(sheetnames, year, sep = "_")
#       
#       # 
#       
#       # 
#       # #use Map to bind all the elements of the list into a dataframe
#       my_list <- Map(cbind, mylist, Cluster = names(mylist))
#       # 
#       print(1)
#       print(do.call("rbind", my_list[1]) %>% clean_names() %>% names())
#       df_operating_individual <- do.call("rbind", my_list[1]) %>% clean_names() %>% filter(state %in% c('TX', 'OK', 'LA', 'AR', 'NM')) %>% mutate(record_year = as.integer(year),
#                                                                                                                                                   record_category = 'operating')
#       
#       # cols.num <- c("planned_uprates_month")
#       # df_operating_individual[cols.num] <- sapply(df_operating_individual[cols.num],as.numeric)
#       
#       df_operating <- bind_rows(df_operating, df_operating_individual)
#       
#     }
#     
#     
#   }
#   
#   df_EIA860_gen_historical <- bind_rows(df_operating,df_planned,df_retired)
#   df_EIA860_gen_historical <- df_EIA860_gen_historical %>%
#     # mutate(plant_gen_id = paste(plant_code, generator_id, energy_source_1, prime_mover, sep = '_')) %>%
#     mutate(plant_gen_id = paste(plant_code, generator_id, sep = '_')) %>%
#     relocate(plant_gen_id)
#   
#   write_csv(df_EIA860_gen_historical, '../Data/EIA860A/historical_plant_gen_id_appearances.csv')
# }

load_EIA923A <- function (df_EIA860M){
  eia_a_files <- c('../source_data/EIA923A/EIA923_Schedules_2_3_4_5_2013_Final_Revision.xlsx',
                   '../source_data/EIA923A/EIA923_Schedules_2_3_4_5_M_12_2014_Final_Revision.xlsx',
                   '../source_data/EIA923A/EIA923_Schedules_2_3_4_5_M_12_2015_Final_Revision.xlsx',
                   '../source_data/EIA923A/EIA923_Schedules_2_3_4_5_M_12_2016_Final_Revision.xlsx',
                   '../source_data/EIA923A/EIA923_Schedules_2_3_4_5_M_12_2017_Final_Revision.xlsx',
                   '../source_data/EIA923A/EIA923_Schedules_2_3_4_5_M_12_2018_Final_Revision.xlsx',
                   '../source_data/EIA923A/EIA923_Schedules_2_3_4_5_M_12_2019_Final_Revision.xlsx',
                   '../source_data/EIA923A/EIA923_Schedules_2_3_4_5_M_12_2020_Final_Revision.xlsx',
                   '../source_data/EIA923A/EIA923_Schedules_2_3_4_5_M_12_2021_Early_Release.xlsx'
                   
  )
  
  df_EIA923A <- data.frame()
  
  print("Starting to import EIA923A files")
  for (file in eia_a_files) {
    
    if (grepl('Early_Release', file)) {
      year = str_sub(file, -23, -20)
    }
    else {
      year = str_sub(file, -24, -21)
    }
    
    print(year)
    if (year == 2021) {
      print(paste('importing file: ', file, sep = " "))
      sheetnames <- excel_sheets(file)
      sheetnames <- sheetnames[grepl('Generation', sheetnames)]
      mylist <- lapply(sheetnames, function(x)
        read_excel(file,x, col_names = TRUE,skip = 6))
      
      names(mylist) <- paste(sheetnames, year, sep = '_')
      #col_names is TRUE by default so you can use this without anonymous function like
      #mylist <- lapply(sheetnames, read_excel, path = path, skip = 1)
      
      # name the dataframes
      
      my_list <- Map(cbind, mylist, Cluster = names(mylist))
      
      #use Map to bind all the elements of the list into a dataframe
      
      
      
      df_EIA923A_individual <- do.call("rbind", my_list)
      df_EIA923A_individual <- df_EIA923A_individual %>%
        mutate(`Nuclear Unit Id` = as.double(`Nuclear Unit Id`)) %>%
        clean_names()
      
      if ('combined_heat_and_power_plant' %in% names(df_EIA923A_individual)) {
        df_EIA923A_individual <- df_EIA923A_individual %>% rename(combined_heat_power_plant = combined_heat_and_power_plant)
      }
      
      if ('netgen_january' %in% names(df_EIA923A_individual)) {
        df_EIA923A_individual <- df_EIA923A_individual %>% rename(netgen_jan = netgen_january,
                                                                  netgen_feb = netgen_february,
                                                                  netgen_mar = netgen_march, 
                                                                  netgen_apr = netgen_april,
                                                                  netgen_may = netgen_may,
                                                                  netgen_jun = netgen_june,
                                                                  netgen_jul = netgen_july,
                                                                  netgen_aug = netgen_august,
                                                                  netgen_sep = netgen_september,
                                                                  netgen_oct = netgen_october,
                                                                  netgen_nov = netgen_november,
                                                                  netgen_dec = netgen_december)
      }
      df_EIA923A <- bind_rows(df_EIA923A, df_EIA923A_individual)
    }
    else if (year == 2020) {
      print(paste('importing file: ', file, sep = " "))
      sheetnames <- excel_sheets(file)
      sheetnames <- sheetnames[grepl('Generation', sheetnames)]
      print(sheetnames)
      mylist <- lapply(sheetnames, function(x)
        read_excel(file,x, col_names = TRUE,skip = 5))
      
      names(mylist) <- paste(sheetnames, year, sep = '_')
      #col_names is TRUE by default so you can use this without anonymous function like
      #mylist <- lapply(sheetnames, read_excel, path = path, skip = 1)
      
      # name the dataframes
      
      my_list <- Map(cbind, mylist, Cluster = names(mylist))
      
      #use Map to bind all the elements of the list into a dataframe
      df_EIA923A_individual <- do.call("rbind", my_list)
      df_EIA923A_individual <- df_EIA923A_individual %>%
        mutate(`Nuclear Unit Id` = as.double(`Nuclear Unit Id`)) %>%
        clean_names()
      
      if ('netgen_january' %in% names(df_EIA923A_individual)) {
        df_EIA923A_individual <- df_EIA923A_individual %>% rename(netgen_jan = netgen_january,
                                                                  netgen_feb = netgen_february,
                                                                  netgen_mar = netgen_march, 
                                                                  netgen_apr = netgen_april,
                                                                  netgen_may = netgen_may,
                                                                  netgen_jun = netgen_june,
                                                                  netgen_jul = netgen_july,
                                                                  netgen_aug = netgen_august,
                                                                  netgen_sep = netgen_september,
                                                                  netgen_oct = netgen_october,
                                                                  netgen_nov = netgen_november,
                                                                  netgen_dec = netgen_december)
      }
      
      if ('combined_heat_and_power_plant' %in% names(df_EIA923A_individual)) {
        df_EIA923A_individual <- df_EIA923A_individual %>% rename(combined_heat_power_plant = combined_heat_and_power_plant)
      }
      df_EIA923A <- bind_rows(df_EIA923A, df_EIA923A_individual)
    }
    else {
      print(paste('importing file: ', file, sep = " "))
      sheetnames <- excel_sheets(file)
      sheetnames <- sheetnames[grepl('Generation', sheetnames)]
      print(sheetnames)
      mylist <- lapply(sheetnames, function(x)
        read_excel(file,x, col_names = TRUE,skip = 5))
      
      names(mylist) <- paste(sheetnames, year, sep = '_')
      #col_names is TRUE by default so you can use this without anonymous function like
      #mylist <- lapply(sheetnames, read_excel, path = path, skip = 1)
      
      # name the dataframes
      
      my_list <- Map(cbind, mylist, Cluster = names(mylist))
      
      #use Map to bind all the elements of the list into a dataframe
      
      
      df_EIA923A_individual <- do.call("rbind", my_list)
      df_EIA923A_individual <- df_EIA923A_individual %>%
        mutate(`Nuclear Unit Id` = as.double(`Nuclear Unit Id`)) %>%
        clean_names()
      
      if ('state' %in% names(df_EIA923A_individual)) {
        df_EIA923A_individual <- df_EIA923A_individual %>% rename(plant_state = state)
      }
      
      if ('combined_heat_and_power_plant' %in% names(df_EIA923A_individual)) {
        df_EIA923A_individual <- df_EIA923A_individual %>% rename(combined_heat_power_plant = combined_heat_and_power_plant)
      }
      
      if ('netgen_january' %in% names(df_EIA923A_individual)) {
        df_EIA923A_individual <- df_EIA923A_individual %>% rename(netgen_jan = netgen_january,
                                                                  netgen_feb = netgen_february,
                                                                  netgen_mar = netgen_march, 
                                                                  netgen_apr = netgen_april,
                                                                  netgen_may = netgen_may,
                                                                  netgen_jun = netgen_june,
                                                                  netgen_jul = netgen_july,
                                                                  netgen_aug = netgen_august,
                                                                  netgen_sep = netgen_september,
                                                                  netgen_oct = netgen_october,
                                                                  netgen_nov = netgen_november,
                                                                  netgen_dec = netgen_december)
      }
      
      df_EIA923A <- bind_rows(df_EIA923A, df_EIA923A_individual)
    }  
  }
  
  cols_to_select <- c("plant_id", "combined_heat_power_plant",  "nuclear_unit_id",  "plant_name",
                      "operator_name", "operator_id", "plant_state", "census_region", "nerc_region",
                      "naics_code", "eia_sector_number", "sector_name","reported_prime_mover",
                      "reported_fuel_type_code", "aer_fuel_type_code", "physical_unit_label", 'year', 'cluster',
                      names(df_EIA923A)[grepl('net', names(df_EIA923A))]
  )
  
  df_EIA923A <- clean_names(df_EIA923A)
  df_EIA923A <- df_EIA923A  %>%  filter(plant_state == 'TX' | nerc_region == 'TRE') %>% mutate(year = as.integer(str_sub(cluster, -4,-1))) %>% select(cols_to_select)
  
  df_EIA923A <- df_EIA923A %>%
    pivot_longer(cols = c(netgen_jan:netgen_dec), names_to = 'month', values_to = 'month_net_gen_m_wh') %>%
    mutate(month = str_sub(month, -3,-1),
           month_net_gen_m_wh = as.integer(month_net_gen_m_wh)) %>%
    rename(annual_net_gen_m_wh = net_generation_megawatthours) %>%
    mutate(month = plyr::mapvalues(month, from = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'), to=c(1,2,3,4,5,6,7,8,9,10,11,12)),
           month = as.integer(month),
           report_category = 'operating',
           balancing_authority_code = if_else(nerc_region == 'TRE', 'ERCO', 'Other'))
  
  print('Fill null BA codes by referencing EIA860M')
  n_plants_w_null_ba <- df_EIA923A %>% filter(is.na(balancing_authority_code)) %>% distinct(plant_id) %>% nrow()
  print(paste(n_plants_w_null_ba, 'plants without a balancing authority',sep = ' '))
  
  df_null_ba_crosswalk <- df_EIA923A %>% 
    filter(is.na(nerc_region) & plant_id %in% unique(df_EIA860M$plant_id)) %>%
    left_join(df_EIA860M %>% distinct(plant_id, balancing_authority_code),
              by = 'plant_id') %>%
    distinct(plant_id, balancing_authority_code.y)
  
  df_EIA923A <- df_EIA923A %>% left_join(df_null_ba_crosswalk, by='plant_id') %>%
    mutate(balancing_authority_code = if_else(is.na(balancing_authority_code), balancing_authority_code.y, balancing_authority_code)) %>%
    select(-balancing_authority_code.y)
  
  n_plants_w_null_ba <- df_EIA923A %>% filter(is.na(balancing_authority_code)) %>% distinct(plant_id) %>% nrow()
  print(paste(n_plants_w_null_ba, 'plants without a balancing authority remaining',sep = ' '))
  
  print('Calculate Market Generation Gini Coef')
  df_EIA923A <- calculate_market_gen_gini(df_EIA923A)
  print('Calculate NG Generation Gini Coef')
  df_EIA923A <- calculate_ng_gen_gini(df_EIA923A)
  print('Calculate Wind Generation Gini Coef')
  df_EIA923A <- calculate_wnd_gen_gini(df_EIA923A)
  
  return(df_EIA923A)
}

# merge_EIM860M_w_EIM860A <- function(EIM_list, df_EIM860A) {
#   df_operating <- EIM_list$operating %>% left_join(df_EIM860A %>% distinct(plant_id, balancing_authority_code), by='plant_id')
#   df_planned <- EIM_list$planned %>% left_join(df_EIM860A %>% distinct(plant_id, balancing_authority_code), by='plant_id')
#   df_retired <- EIM_list$retired %>% left_join(df_EIM860A %>% distinct(plant_id, balancing_authority_code), by='plant_id')
#   df_postponed <- EIM_list$postponed %>% left_join(df_EIM860A %>% distinct(plant_id, balancing_authority_code), by='plant_id')
#   
#   return(list('operating' = df_operating, 'planned' = df_planned, 'retired' = df_retired, 'postponed' = df_postponed))
# }

calculate_market_cap_gini <- function(df_EIA860M) {
  df_market_cap_gini <- df_EIA860M %>% left_join(df_EIA860M %>%
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
    distinct(year, month, entity_id, entity_cap_mw, total_cap_mw, n_entities) %>%
    group_by(year, month) %>%
    arrange(year, month, entity_cap_mw) %>%
    mutate(cum_entities = row_number(),
           cum_cap_mw = cumsum(entity_cap_mw)) %>%
    mutate(percent_total_operators = cum_entities/n_entities,
           percent_total_cap = cum_cap_mw/total_cap_mw) %>%
    group_by(year, month) %>%
    mutate(auc =  MESS::auc(percent_total_operators, percent_total_cap, type='spline'), #calculate area under curve
           market_cap_gini_coef = 1-2*auc, #calculate gini as 1 − 2B due to the fact that A + B = 0.5 (since the axes scale from 0 to 1).
           date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    distinct(year, month, market_cap_gini_coef)
  
  df_EIA860M <- df_EIA860M %>%
    left_join(df_market_cap_gini,
              by=c('year', 'month')) 
  
  return(df_EIA860M)
}

calculate_ng_cap_gini <- function(df_EIA860M) {
  df_ng_cap_gini <- df_EIA860M %>% left_join(df_EIA860M %>%
                                               filter(record_category == 'operating', balancing_authority_code == 'ERCO', energy_source_code == 'NG') %>%
                                               group_by(year, month, entity_id) %>%
                                               summarize(entity_cap_mw = sum(net_summer_capacity_mw, na.rm=T)) %>%
                                               left_join(df_EIA860M %>% 
                                                           filter(record_category == 'operating', balancing_authority_code == 'ERCO', energy_source_code == 'NG') %>% group_by(year, month) %>% summarize(total_cap_mw = sum(net_summer_capacity_mw, na.rm = T)),
                                                         by=c('year', 'month')) %>%
                                               left_join(df_EIA860M %>% 
                                                           filter(record_category == 'operating', balancing_authority_code == 'ERCO', energy_source_code == 'NG') %>% group_by(year, month) %>% summarize(n_entities = length(unique(entity_id))),
                                                         by=c('year', 'month')),
                                             by=c('year', 'month', 'entity_id')) %>%
    mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    filter(report_category == 'operating', balancing_authority_code == 'ERCO', energy_source_code == 'NG') %>%
    distinct(year, month, entity_id, entity_cap_mw, total_cap_mw, n_entities) %>%
    group_by(year, month) %>%
    arrange(year, month, entity_cap_mw) %>%
    mutate(cum_entities = row_number(),
           cum_cap_mw = cumsum(entity_cap_mw)) %>%
    mutate(percent_total_operators = cum_entities/n_entities,
           percent_total_cap = cum_cap_mw/total_cap_mw) %>%
    group_by(year, month) %>%
    mutate(auc =  MESS::auc(percent_total_operators, percent_total_cap, type='spline'), #calculate area under curve
           ng_cap_gini_coef = 1-2*auc, #calculate gini as 1 − 2B due to the fact that A + B = 0.5 (since the axes scale from 0 to 1).
           date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    distinct(year, month, ng_cap_gini_coef)
  
  df_EIA860M <- df_EIA860M %>%
    left_join(df_ng_cap_gini,
              by=c('year', 'month')) 
  
  return(df_EIA860M)
}

calculate_wnd_cap_gini <- function(df_EIA860M) {
  df_wnd_cap_gini <- df_EIA860M %>% left_join(df_EIA860M %>%
                                               filter(record_category == 'operating', balancing_authority_code == 'ERCO', energy_source_code == 'WND') %>%
                                               group_by(year, month, entity_id) %>%
                                               summarize(entity_cap_mw = sum(net_summer_capacity_mw, na.rm=T)) %>%
                                               left_join(df_EIA860M %>% 
                                                           filter(record_category == 'operating', balancing_authority_code == 'ERCO', energy_source_code == 'WND') %>% group_by(year, month) %>% summarize(total_cap_mw = sum(net_summer_capacity_mw, na.rm = T)),
                                                         by=c('year', 'month')) %>%
                                               left_join(df_EIA860M %>% 
                                                           filter(record_category == 'operating', balancing_authority_code == 'ERCO', energy_source_code == 'WND') %>% group_by(year, month) %>% summarize(n_entities = length(unique(entity_id))),
                                                         by=c('year', 'month')),
                                             by=c('year', 'month', 'entity_id')) %>%
    mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    filter(report_category == 'operating', balancing_authority_code == 'ERCO', energy_source_code == 'WND') %>%
    distinct(year, month, entity_id, entity_cap_mw, total_cap_mw, n_entities) %>%
    group_by(year, month) %>%
    arrange(year, month, entity_cap_mw) %>%
    mutate(cum_entities = row_number(),
           cum_cap_mw = cumsum(entity_cap_mw)) %>%
    mutate(percent_total_operators = cum_entities/n_entities,
           percent_total_cap = cum_cap_mw/total_cap_mw) %>%
    group_by(year, month) %>%
    mutate(auc =  MESS::auc(percent_total_operators, percent_total_cap, type='spline'), #calculate area under curve
           wnd_cap_gini_coef = 1-2*auc, #calculate gini as 1 − 2B due to the fact that A + B = 0.5 (since the axes scale from 0 to 1).
           date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    distinct(year, month, wnd_cap_gini_coef)
  
  df_EIA860M <- df_EIA860M %>%
    left_join(df_wnd_cap_gini,
              by=c('year', 'month')) 
  
  return(df_EIA860M)
}

calculate_market_gen_gini <- function(df_EIA923A) {
  df_market_gen_gini <- df_EIA923A %>% left_join(df_EIA923A %>%
                                                   filter(balancing_authority_code == 'ERCO') %>%
                                                   group_by(year, month, operator_id) %>%
                                                   summarize(operator_gen_m_wh = sum(month_net_gen_m_wh, na.rm=T)) %>%
                                                   left_join(df_EIA923A %>% 
                                                               filter(balancing_authority_code == 'ERCO') %>% group_by(year, month) %>% summarize(total_gen_m_wh = sum(month_net_gen_m_wh, na.rm = T)),
                                                             by=c('year', 'month')) %>%
                                                   left_join(df_EIA923A %>% 
                                                               filter(balancing_authority_code == 'ERCO') %>% group_by(year, month) %>% summarize(n_operators = length(unique(operator_id))),
                                                             by=c('year', 'month')),
                                                 by=c('year', 'month', 'operator_id')) %>%
    mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    filter(balancing_authority_code == 'ERCO') %>%
    distinct(year, month, operator_id, operator_gen_m_wh, total_gen_m_wh, n_operators) %>%
    group_by(year, month) %>%
    arrange(year, month, operator_gen_m_wh) %>%
    mutate(cum_entities = row_number(),
           cum_cap_mw = cumsum(operator_gen_m_wh)) %>%
    mutate(percent_total_operators = cum_entities/n_operators,
           percent_total_gen = cum_cap_mw/total_gen_m_wh) %>%
    group_by(year, month) %>%
    mutate(auc =  MESS::auc(percent_total_operators, percent_total_gen, type='spline'), #calculate area under curve
           market_gen_gini_coef = 1-2*auc, #calculate gini as 1 − 2B due to the fact that A + B = 0.5 (since the axes scale from 0 to 1).
           date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    distinct(year, month, market_gen_gini_coef)
  
  df_EIA923A <- df_EIA923A %>%
    left_join(df_market_gen_gini,
              by=c('year', 'month'))
  
  return(df_EIA923A)
}

calculate_ng_gen_gini <- function(df_EIA923A) {
  df_ng_gen_gini <- df_EIA923A %>% left_join(df_EIA923A %>%
                                               filter(balancing_authority_code == 'ERCO', reported_fuel_type_code == 'NG') %>%
                                               group_by(year, month, operator_id) %>%
                                               summarize(operator_gen_m_wh = sum(month_net_gen_m_wh, na.rm=T)) %>%
                                               left_join(df_EIA923A %>% 
                                                           filter(balancing_authority_code == 'ERCO', reported_fuel_type_code == 'NG') %>% group_by(year, month) %>% summarize(total_gen_m_wh = sum(month_net_gen_m_wh, na.rm = T)),
                                                         by=c('year', 'month')) %>%
                                               left_join(df_EIA923A %>% 
                                                           filter(balancing_authority_code == 'ERCO', reported_fuel_type_code == 'NG') %>% group_by(year, month) %>% summarize(n_operators = length(unique(operator_id))),
                                                         by=c('year', 'month')),
                                             by=c('year', 'month', 'operator_id')) %>%
    mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    filter(balancing_authority_code == 'ERCO', reported_fuel_type_code == 'NG') %>%
    distinct(year, month, operator_id, operator_gen_m_wh, total_gen_m_wh, n_operators) %>%
    group_by(year, month) %>%
    arrange(year, month, operator_gen_m_wh) %>%
    mutate(cum_entities = row_number(),
           cum_cap_mw = cumsum(operator_gen_m_wh)) %>%
    mutate(percent_total_operators = cum_entities/n_operators,
           percent_total_gen = cum_cap_mw/total_gen_m_wh) %>%
    group_by(year, month) %>%
    mutate(auc =  MESS::auc(percent_total_operators, percent_total_gen, type='spline'), #calculate area under curve
           ng_gen_gini_coef = 1-2*auc, #calculate gini as 1 − 2B due to the fact that A + B = 0.5 (since the axes scale from 0 to 1).
           date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    distinct(year, month, ng_gen_gini_coef)
  
  df_EIA923A <- df_EIA923A %>%
    left_join(df_ng_gen_gini,
              by=c('year', 'month'))
  
  return(df_EIA923A)
}

calculate_wnd_gen_gini <- function(df_EIA923A) {
  df_wnd_gen_gini <- df_EIA923A %>% left_join(df_EIA923A %>%
                                               filter(balancing_authority_code == 'ERCO', reported_fuel_type_code == 'WND') %>%
                                               group_by(year, month, operator_id) %>%
                                               summarize(operator_gen_m_wh = sum(month_net_gen_m_wh, na.rm=T)) %>%
                                               left_join(df_EIA923A %>% 
                                                           filter(balancing_authority_code == 'ERCO', reported_fuel_type_code == 'WND') %>% group_by(year, month) %>% summarize(total_gen_m_wh = sum(month_net_gen_m_wh, na.rm = T)),
                                                         by=c('year', 'month')) %>%
                                               left_join(df_EIA923A %>% 
                                                           filter(balancing_authority_code == 'ERCO', reported_fuel_type_code == 'WND') %>% group_by(year, month) %>% summarize(n_operators = length(unique(operator_id))),
                                                         by=c('year', 'month')),
                                             by=c('year', 'month', 'operator_id')) %>%
    mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    filter(balancing_authority_code == 'ERCO', reported_fuel_type_code == 'WND') %>%
    distinct(year, month, operator_id, operator_gen_m_wh, total_gen_m_wh, n_operators) %>%
    group_by(year, month) %>%
    arrange(year, month, operator_gen_m_wh) %>%
    mutate(cum_entities = row_number(),
           cum_cap_mw = cumsum(operator_gen_m_wh)) %>%
    mutate(percent_total_operators = cum_entities/n_operators,
           percent_total_gen = cum_cap_mw/total_gen_m_wh) %>%
    group_by(year, month) %>%
    mutate(auc =  MESS::auc(percent_total_operators, percent_total_gen, type='spline'), #calculate area under curve
           wnd_gen_gini_coef = 1-2*auc, #calculate gini as 1 − 2B due to  A + B = 0.5 (since the axes scale from 0 to 1).
           date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    distinct(year, month, wnd_gen_gini_coef)
  
  df_EIA923A <- df_EIA923A %>%
    left_join(df_wnd_gen_gini, 
              by=c('year', 'month'))
  
  return(df_EIA923A)
}

identify_entry_and_exit_months_by_record_category_and_status <- function(df_EIA860M) {
  
  df_EIA860M <- df_EIA860M %>%
    mutate(status = if_else(is.na(status), paste('(',record_category,')', sep=''), status),
           status_code = if_else(!is.na(status),gsub("[\\(\\)]",          # Extract characters within parentheses
                                                                 "",
                                                                 regmatches(status,
                                                                            gregexpr("\\(.*?\\)",
                                                                                     status))),NA_character_)) %>%
    mutate(plant_gen_id = paste(plant_id, generator_id, sep = '_'),
           # plant_gen_id = paste(plant_id, generator_id, energy_source_code, prime_mover_code, sep = '_'),
           status_phase = as.integer(plyr::mapvalues(status_code,
                                                     from=c('P','T','L','U','V','TS','OP','SB','OA','OS', 'OT', 'cancelled_postponed', 'retired', NULL),
                                                     to=c(1,2,3,4,5,6,7,8,9,10,11,12,13,NULL))))
  
  df_entry_by_status <- df_EIA860M %>%
    mutate(status_code = if_else(!is.na(status),gsub("[\\(\\)]",          # Extract characters within parentheses
                                                     "",
                                                     regmatches(status,
                                                                gregexpr("\\(.*?\\)",
                                                                         status))),NA_character_)) %>%
    group_by(status_code) %>%
    arrange(year, month) %>%
    mutate(plant_gen_id = paste(plant_id, generator_id, sep = '_'),
           # plant_gen_id = paste(plant_id, generator_id, energy_source_code, prime_mover_code, sep = '_'),
           ) %>%
    slice(match(unique(plant_gen_id), plant_gen_id)) %>%
    distinct(plant_gen_id, year, month, status_code, net_summer_capacity_mw) %>%
    mutate(is_entry_month = 1)
  
  df_exit_by_status <- df_EIA860M %>% 
    mutate(status_code = if_else(!is.na(status),gsub("[\\(\\)]",          # Extract characters within parentheses
                                                     "",
                                                     regmatches(status,
                                                                gregexpr("\\(.*?\\)",
                                                                         status))),NA_character_)) %>%
    group_by(status_code) %>%
    arrange(desc(year), desc(month)) %>%
    mutate(plant_gen_id = paste(plant_id, generator_id, sep = '_'),
           # plant_gen_id = paste(plant_id, generator_id, energy_source_code, prime_mover_code, sep = '_'),
           ) %>%
    slice(match(unique(plant_gen_id), plant_gen_id)) %>%
    distinct(plant_gen_id, year, month, status_code, net_summer_capacity_mw) %>%
    mutate(is_exit_month = 1)
  
  df_entry_exit_by_status <- rbind(df_entry_by_status, df_exit_by_status)
  
  df_EIA860M <- df_EIA860M %>%
    left_join(df_entry_exit_by_status,
              by=c('plant_gen_id', 'year', 'month', 'status_code', 'net_summer_capacity_mw'))
  
  return(df_EIA860M)
} 


calculate_stats_duration_planned_to_operating_market_capacity <- function(df_EIA860M) {
  
  df_stats <- df_EIA860M %>%
    filter(balancing_authority_code == 'ERCO') %>%
    group_by(record_category) %>%
    arrange(year, month) %>%
    mutate(plant_gen_id = paste(plant_id, generator_id, sep = '_'),
           # plant_gen_id = paste(plant_id, generator_id, energy_source_code, prime_mover_code, sep = '_')
           ) %>%
    slice(match(unique(plant_gen_id), plant_gen_id)) %>%
    select(record_category, year, month, plant_id, plant_name, plant_gen_id, net_summer_capacity_mw) %>%
    arrange(plant_gen_id, year, month) %>% 
    mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    select(-c(year, month, plant_id)) %>%
    pivot_wider(names_from = record_category, values_from = c(date)) %>%
    mutate(planned_operating_days_diff = as.numeric(difftime(operating, planned, units='days')),
           planned_cancelled_days_diff = as.numeric(difftime(cancelled_postponed, planned, units='days'))) %>%
    filter(!is.na(planned_operating_days_diff)) %>%
    select(planned_operating_days_diff) %>%
    mutate(mean_planned_op_days_diff = mean(planned_operating_days_diff),
           std_planned_op_days_diff = sd(planned_operating_days_diff),
           min_planned_op_days_diff = min(planned_operating_days_diff),
           max_planned_op_days_diff = max(planned_operating_days_diff)) %>%
    distinct(mean_planned_op_days_diff, std_planned_op_days_diff, min_planned_op_days_diff, max_planned_op_days_diff)
  
  return(df_stats)
}

calculate_stats_duration_planned_to_operating_ng_capacity <- function(df_EIA860M) {
  
  
  
  df_stats <- df_EIA860M %>%
    filter(balancing_authority_code == 'ERCO') %>%
    group_by(record_category) %>%
    arrange(year, month) %>%
    mutate(plant_gen_id = paste(plant_id, generator_id, sep = '_'),
           # plant_gen_id = paste(plant_id, generator_id, energy_source_code, prime_mover_code, sep = '_')
           ) %>%
    slice(match(unique(plant_gen_id), plant_gen_id)) %>%
    select(record_category, year, month, plant_id, plant_name, plant_gen_id, net_summer_capacity_mw) %>%
    arrange(plant_gen_id, year, month) %>% 
    mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    select(-c(year, month, plant_id)) %>%
    pivot_wider(names_from = record_category, values_from = c(date)) %>%
    mutate(planned_operating_days_diff = as.numeric(difftime(operating, planned, units='days')),
           planned_cancelled_days_diff = as.numeric(difftime(cancelled_postponed, planned, units='days'))) %>%
    filter(!is.na(planned_operating_days_diff), grepl('_NG_', plant_gen_id)) %>%
    select(planned_operating_days_diff) %>%
    mutate(mean_planned_op_days_diff = mean(planned_operating_days_diff),
           std_planned_op_days_diff = sd(planned_operating_days_diff),
           min_planned_op_days_diff = min(planned_operating_days_diff),
           max_planned_op_days_diff = max(planned_operating_days_diff)) %>%
    distinct(mean_planned_op_days_diff, std_planned_op_days_diff, min_planned_op_days_diff, max_planned_op_days_diff)
  
  return(df_stats)
}

calculate_stats_duration_planned_to_operating_wnd_capacity <- function(df_EIA860M) {
  
  df_stats <- df_EIA860M %>%
    filter(balancing_authority_code == 'ERCO') %>%
    group_by(record_category) %>%
    arrange(year, month) %>%
    mutate(plant_gen_id = paste(plant_id, generator_id, sep = '_'),
           # plant_gen_id = paste(plant_id, generator_id, energy_source_code, prime_mover_code, sep = '_')
           ) %>%
    slice(match(unique(plant_gen_id), plant_gen_id)) %>%
    select(record_category, year, month, plant_id, plant_name, plant_gen_id, net_summer_capacity_mw) %>%
    arrange(plant_gen_id, year, month) %>% 
    mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')) %>%
    select(-c(year, month, plant_id)) %>%
    pivot_wider(names_from = record_category, values_from = c(date)) %>%
    mutate(planned_operating_days_diff = as.numeric(difftime(operating, planned, units='days')),
           planned_cancelled_days_diff = as.numeric(difftime(cancelled_postponed, planned, units='days'))) %>%
    filter(!is.na(planned_operating_days_diff), grepl('_WND_', plant_gen_id)) %>%
    select(planned_operating_days_diff) %>%
    mutate(mean_planned_op_days_diff = mean(planned_operating_days_diff),
           std_planned_op_days_diff = sd(planned_operating_days_diff),
           min_planned_op_days_diff = min(planned_operating_days_diff),
           max_planned_op_days_diff = max(planned_operating_days_diff)) %>%
    distinct(mean_planned_op_days_diff, std_planned_op_days_diff, min_planned_op_days_diff, max_planned_op_days_diff)
  
  return(df_stats)
}



create_EIA_operating_cap_gen_summaries_market_and_ng <- function(df_EIA860M, df_EIA923A) {
  
  df_ERCOT_EIA_market_operating_gen_cap_summary <- df_EIA860M %>%
    filter(balancing_authority_code == 'ERCO', record_category == 'operating') %>%
    group_by(year, month) %>%
    summarize(capacity_mw = sum(net_summer_capacity_mw, na.rm = TRUE),
              cap_gini = mean(market_cap_gini_coef, na.rm=T)) %>%
    left_join(df_EIA923A %>% filter(balancing_authority_code == 'ERCO') %>%
                group_by(year, month) %>%
                summarize(generation_m_wh = sum(month_net_gen_m_wh, na.rm=T),
                          gen_gini = mean(market_gen_gini_coef, na.rm=T)) %>%
                mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')))
  
  write_csv(df_ERCOT_EIA_market_operating_gen_cap_summary, '../Data/Figure Summaries/ERCOT_EIA_market_gen_cap_summary.csv')
  
  df_ERCOT_EIA_ng_operating_gen_cap_summary <- df_EIA860M %>%
    filter(balancing_authority_code == 'ERCO', record_category == 'operating', energy_source_code == 'NG') %>%
    group_by(year, month) %>%
    summarize(capacity_mw = sum(net_summer_capacity_mw, na.rm = TRUE),
              cap_gini = mean(market_cap_gini_coef, na.rm=T)) %>%
    left_join(df_EIA923A %>% filter(balancing_authority_code == 'ERCO', reported_fuel_type_code == 'NG') %>%
                group_by(year, month) %>%
                summarize(generation_m_wh = sum(month_net_gen_m_wh, na.rm=T),
                          gen_gini = mean(market_gen_gini_coef, na.rm=T)) %>%
                mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')))
  
  write_csv(df_ERCOT_EIA_ng_operating_gen_cap_summary, '../Data/Figure Summaries/ERCOT_EIA_ng_gen_cap_summary.csv')
  
  df_ERCOT_EIA_wnd_operating_gen_cap_summary <- df_EIA860M %>%
    filter(balancing_authority_code == 'ERCO', record_category == 'operating', energy_source_code == 'WND') %>%
    group_by(year, month) %>%
    summarize(capacity_mw = sum(net_summer_capacity_mw, na.rm = TRUE),
              cap_gini = mean(wnd_cap_gini_coef, na.rm=T)) %>%
    left_join(df_EIA923A %>% filter(balancing_authority_code == 'ERCO', reported_fuel_type_code == 'WND') %>%
                group_by(year, month) %>%
                summarize(generation_m_wh = sum(month_net_gen_m_wh, na.rm=T),
                          gen_gini = mean(wnd_gen_gini_coef, na.rm=T)) %>%
                mutate(date = as.Date(paste(year, month, '01', sep = '-'), format='%Y-%m-%d')))
  
  write_csv(df_ERCOT_EIA_wnd_operating_gen_cap_summary, '../Data/Figure Summaries/ERCOT_EIA_wnd_gen_cap_summary.csv')
}


display_compare_plants_EIA_860M_and_EIA923A_table <- function(df_EIA860M, df_EIA923A) {
  
  df_EIA860M <- df_EIA860M %>%
    group_by(year, month, plant_id) %>%
    mutate(plant_capacity_mw = sum(net_summer_capacity_mw, na.rm = T)) %>%
    ungroup()
  
  print('display plant ids that are not present in both EIA 860M and EIA 923 data sets')
  df_EIA923A %>% filter(balancing_authority_code == 'ERCO') %>% distinct(plant_id, balancing_authority_code, year, plant_name) %>%
    merge((df_EIA860M %>% filter(record_category == 'operating' & balancing_authority_code == 'ERCO') %>% distinct(plant_id, balancing_authority_code, record_year, plant_capacity_mw, energy_source_code, plant_name)),
          by.x=c('plant_id', 'balancing_authority_code', 'year'),
          by.y=c('plant_id', 'balancing_authority_code', 'record_year'), all.x=TRUE, all.y = T) %>%
    rename(plant_name_923A = plant_name.x,
           plant_name_860M = plant_name.y) %>%
    group_by(year) %>%
    filter(!complete.cases(plant_name_923A)) %>%
    arrange(year)
}


unnest_dt <- function(tbl, col) {
  tbl <- as.data.table(tbl)
  col <- ensyms(col)
  clnms <- syms(setdiff(colnames(tbl), as.character(col)))
  tbl <- as.data.table(tbl)
  tbl <- eval(
    expr(tbl[, as.character(unlist(!!!col)), by = list(!!!clnms)])
  )
  colnames(tbl) <- c(as.character(clnms), as.character(col))
  return (tbl)
}


create_monthly_gini_summary <- function(lag_months=12) {
  
  df_EIA <- read_csv('../Data/EIA Compiled Data/EIA860M_compiled_data.csv')
  
  df_EIA_gini_summary <- df_EIA %>%
    select(year, month, market_cap_gini_coef, ng_cap_gini_coef, wnd_cap_gini_coef) %>%
    mutate(date = as.Date(paste(year, month, '01', sep='-'), format = '%Y-%m-%d')) %>%
    distinct() %>%
    group_by(date) %>%
    summarize(mean_market_cap_gini_coef = mean(market_cap_gini_coef, na.rm=T),
              mean_ng_cap_gini_coef = mean(ng_cap_gini_coef, na.rm=T),
              mean_wnd_cap_gini_coef = mean(wnd_cap_gini_coef, na.rm=T)) %>%
    mutate(date_minus_month = as.Date(date) %m+% months(1),
           date_minus_year = as.Date(date) %m+% months(12),
           date_minus_lag = as.Date(date) %m+% months(lag_months),
           mean_ng_cap_gini_coef_hund = mean_ng_cap_gini_coef*100,
           mean_wnd_cap_gini_coef_hund = mean_wnd_cap_gini_coef*100)
  
  write_csv(df_EIA_gini_summary, '../Data/EIA Compiled Data/gini_summary.csv') 
}

create_plant_gen_id_phase_timeline_csv <- function(df_EIA860M) {
  
  df_EIA860M <- read_csv('../Data/EIA Compiled Data/EIA860M_compiled_data.csv')  
  remove_pre_eval_period_plant_gen_ids <- FALSE
  
  
  df_plant_gen_id_phase_timeline <- df_EIA860M %>%
    filter(balancing_authority_code == 'ERCO') %>%
    group_by(plant_gen_id, plant_id, generator_id, energy_source_code_group, energy_source_code, prime_mover_code, status_phase, net_summer_capacity_mw) %>%
    mutate(year_month = paste(year,month, sep = '-')) %>%
    arrange(status_phase, year, month) %>%
    pivot_wider(id_cols = c(plant_gen_id, plant_id, generator_id, energy_source_code_group, energy_source_code, prime_mover_code, net_summer_capacity_mw),
                names_from = status_phase,
                values_from = year_month,
                values_fn = list) %>%
    pivot_longer(cols = -c(plant_gen_id, plant_id, generator_id, energy_source_code_group, energy_source_code, prime_mover_code, net_summer_capacity_mw),
                 names_to = 'status_phase',
                 values_to = 'year_months')
  
  df_plant_gen_id_phase_timeline <- unnest_dt(df_plant_gen_id_phase_timeline, year_months)
  df_plant_gen_id_phase_timeline <- df_plant_gen_id_phase_timeline %>%
    mutate(year_months = as.Date(paste(year_months, '01', sep = '-'), format='%Y-%m-%d'),
           status_phase = as.numeric(status_phase)) %>%
    left_join(df_EIA860M %>% distinct(status, status_phase), by='status_phase') %>%
    rename(date = year_months) %>%
    distinct() %>%
    group_by(plant_gen_id) %>%
    mutate(first_appearance = min(date),
           operation_date = min(date[status_phase == 7]),
           retire_date = min(date[status_phase == 13]),
           last_appearance = max(date),
           first_appearance_phase = status_phase[date==first_appearance],
           last_appearance_phase = status_phase[date==last_appearance],
           first_appearance_energy_source_code_group = energy_source_code_group[date==first_appearance],
           last_appearance_energy_source_code_group = energy_source_code_group[date==last_appearance],
           first_appearance_net_summer_cap = net_summer_capacity_mw[date==first_appearance],
           last_appearance_net_summer_cap = net_summer_capacity_mw[date==last_appearance]) %>%
    ungroup() %>%
    group_by(plant_gen_id, status_phase) %>%
    mutate(first_date_in_phase = min(date),
           last_date_in_phase = max(date)) %>%
    mutate(weeks_in_phase = round(as.numeric(difftime(last_date_in_phase, first_date_in_phase, unit='weeks')),2),
           moves_into_operation = if_else(7 %in% status_phase, 1,0),
           is_first_operation_record = if_else(status_phase == 7 & first_date_in_phase == date,1,0),
           weeks_since_first_appearance = as.numeric(difftime(date, first_appearance, unit='weeks')),
           weeks_to_operation = as.numeric(difftime(operation_date, first_appearance, unit='weeks'))) %>%
    ungroup()
  
  df_postponed_to_planning <- df_plant_gen_id_phase_timeline %>%
    filter(status_phase <=6 ) %>%
    group_by(plant_gen_id) %>%
    summarize(min_planning_date = min(date), 
              max_planning_date = max(date)) %>%
    left_join(df_plant_gen_id_phase_timeline %>%
                filter(status_phase == 12) %>%
                group_by(plant_gen_id) %>%
                summarize(min_postponed_date = min(date), 
                          max_postponed_date = max(date)),
              by='plant_gen_id') %>%
    mutate(planning_to_postponed = if_else(min_planning_date < min_postponed_date, 1,0),
           postponed_to_planning = if_else(max_planning_date > max_postponed_date, 1,0),
           postponed_to_planning_date = if_else(postponed_to_planning ==1, as.Date(max_postponed_date) %m+% months(1), max_postponed_date),
           planning_to_postponed_and_back = if_else(planning_to_postponed == 1 & postponed_to_planning ==1, 1,0)) %>%
    select(plant_gen_id, postponed_to_planning_date) %>%
    filter(!is.na.POSIXlt(postponed_to_planning_date)) %>%
    rename(date = postponed_to_planning_date) %>%
    mutate(moves_back_to_planning = 1)
  
  
  df_plant_gen_id_phase_timeline <- df_plant_gen_id_phase_timeline %>%
    left_join(df_postponed_to_planning,
              by=c('plant_gen_id', 'date')) %>%
    mutate(moves_back_to_planning = if_else(moves_back_to_planning == 1, 1, 0))
  
  #merge in recorded first operation date
  df_first_operating_data <- df_EIA860M[complete.cases(df_EIA860M$recorded_first_operation_date),] %>%
    distinct(plant_gen_id, recorded_first_operation_date)
  
  df_plant_gen_id_phase_timeline <- df_plant_gen_id_phase_timeline %>%
    left_join(df_first_operating_data, by='plant_gen_id')
  
  if (remove_pre_eval_period_plant_gen_ids == TRUE) {
    print('Removing plant_gen_ids whos first appearance is present in 2014')
    
    df_EIA860_gen_historical <- read_csv('../Data/EIA860A/historical_plant_gen_id_appearances.csv')
    
    operating_first_appearances <- df_plant_gen_id_phase_timeline %>%
      filter(date > '2015-07-01' & first_appearance == date, status_phase == 7) %>%
      pull(plant_gen_id)
    
    pre_eval_period_appearance_plant_gen_id <- df_EIA860_gen_historical %>%
      filter(plant_gen_id %in% operating_first_appearances) %>%
      pull(plant_gen_id)
    
    print(paste('count of plant_gen_ids excluded due to first appearance present in historical data: ', length(pre_eval_period_appearance_plant_gen_id)))
    df_plant_gen_id_phase_timeline <- df_plant_gen_id_phase_timeline %>%
      filter(!(plant_gen_id %in% pre_eval_period_appearance_plant_gen_id))
  }
  
  #include rerates
  # create data frame that captures changes in rated capacity
  df_rerates <- df_plant_gen_id_phase_timeline %>%
    group_by(plant_gen_id) %>%
    arrange(plant_gen_id, date) %>%
    mutate(lag_net_summer_capacity = if_else(row_number() == 1, net_summer_capacity_mw,  lag(net_summer_capacity_mw, 1)),
           rerate_change = net_summer_capacity_mw - lag_net_summer_capacity) %>%
    select(plant_gen_id, date, lag_net_summer_capacity, rerate_change) %>%
    arrange(plant_gen_id, date) %>%
    ungroup()
  
  print('Joining in rerates')
  df_plant_gen_id_phase_timeline <- df_plant_gen_id_phase_timeline %>%
    left_join(df_rerates,
              by=c('plant_gen_id', 'date')) %>%
    ungroup()
  
  
  ###calculate plant specific values of entry, rerates, and status change capacity in mw
  df_plant_gen_id_phase_timeline <- df_plant_gen_id_phase_timeline %>%
    group_by(plant_gen_id) %>%
    arrange(date) %>%
    mutate(previous_month_status_phase = lag(status_phase, 1)) %>%
    mutate(net_summer_operating_capacity_status_change_mw = case_when((status_phase > previous_month_status_phase)
                                                                      & (status_phase == 7 | previous_month_status_phase == 7)
                                                                      & (date > recorded_first_operation_date) ~ -net_summer_capacity_mw,
                                                                      (status_phase < previous_month_status_phase)
                                                                      & (status_phase == 7 | previous_month_status_phase == 7)
                                                                      & (date > recorded_first_operation_date) ~ net_summer_capacity_mw,
                                                                      TRUE ~ 0),
           net_summer_operating_capacity_new_entry_mw = if_else(date == recorded_first_operation_date, net_summer_capacity_mw, 0),
           net_summer_operating_capacity_rerate_mw = if_else((status_phase == previous_month_status_phase)
                                                             & (net_summer_capacity_mw != lag_net_summer_capacity)
                                                             & (date > recorded_first_operation_date), net_summer_capacity_mw - lag_net_summer_capacity, 0)) %>%
    ungroup() %>%
    distinct()
  
    #alter data quality issue for observation: plant_gen_id:  64801_GEN3_NG_IC. Recorded first operation date is 2020-08-01 in one month and 
  
    write_csv(df_plant_gen_id_phase_timeline, '../Data/EIA Compiled Data/EIA860M_timeline.csv')

}


create_panel_totalquantity_specific_regressions_12mo_lag <- function(lag_months=12) {
  
  market_segments <- c('ng'
                       , 'renewable'
                       #'market', 'nonng', 'wind', 'solar', 'other'\
  )
  
  df_EIA860M <- read_csv('../Data/EIA Compiled Data/EIA860M_compiled_data.csv')
  df_plant_gen_id_phase_timeline <- read_csv('../Data/EIA Compiled Data/EIA860M_timeline.csv')
  
  min_date = as.Date('2015-07-01', format='%Y-%m-%d')
  # ma  x_date = as.Date('2021-12-01', format='%Y-%m-%d')
  max_date = as.Date('2022-12-01', format='%Y-%m-%d')
  df_date_range <- data.frame(date=as.Date(seq(min_date,max_date,by="month"), format="%Y-%m-01"))
  
  #create_panel_data
  operating_generators <- df_plant_gen_id_phase_timeline %>% filter(status_phase %in% c(7,8)) %>% distinct(plant_gen_id) %>% pull(plant_gen_id)
  df_panel_plant_gen <- merge(df_date_range, df_plant_gen_id_phase_timeline %>% select(plant_gen_id) %>% distinct())
  
  df_panel_plant_gen <- df_panel_plant_gen %>%
    left_join(df_plant_gen_id_phase_timeline %>% 
                select(plant_gen_id, recorded_first_operation_date, first_appearance, first_appearance_phase, last_appearance, last_appearance_phase, first_appearance_energy_source_code_group, last_appearance_energy_source_code_group, first_appearance_net_summer_cap, last_appearance_net_summer_cap) %>%
                distinct(),
              by=c('plant_gen_id')) %>%
    left_join(df_plant_gen_id_phase_timeline %>% select(plant_gen_id, date, energy_source_code, energy_source_code_group, status_phase, net_summer_capacity_mw),
              by=c('plant_gen_id', 'date')) %>%
    mutate(unfilled_status_phase = status_phase,
           unfilled_energy_source_code = energy_source_code,
           unfilled_energy_source_code_group = energy_source_code_group,
           unfilled_net_summer_capacity_mw = net_summer_capacity_mw) %>%
    mutate(energy_source_code_group = if_else(is.na(energy_source_code_group) , first_appearance_energy_source_code_group, energy_source_code_group)) %>%
    mutate(energy_source_code_group = if_else(last_appearance_energy_source_code_group == 'NG' & first_appearance_energy_source_code_group != 'NG' , last_appearance_energy_source_code_group, energy_source_code_group)) %>%
    mutate(energy_source_code_group = if_else(first_appearance_energy_source_code_group == 'NG' & last_appearance_energy_source_code_group != 'NG' , first_appearance_energy_source_code_group, energy_source_code_group)) %>%
    mutate(status_phase = if_else((date >= recorded_first_operation_date) & !is.na(recorded_first_operation_date) & (status_phase < 7), 7, status_phase)) %>% #alter status_phase to align w/ recorded first_operation_date
    mutate(status_phase = if_else((date < first_appearance) & (first_appearance_phase < 7), 0, status_phase)) %>% #alter status_phase to align w/ planning appearances (do not back-project record in planning longer than observed)
    mutate(operating_capacity_mw = if_else((date >= recorded_first_operation_date) & (status_phase %in% c(7,8)), net_summer_capacity_mw,0), ## define operating capacity based on recorded operation date and status phase
           planned_capacity_mw = if_else((date < recorded_first_operation_date) & (date >= first_appearance), net_summer_capacity_mw, 0),
           planned_capacity_mw = if_else(is.na(recorded_first_operation_date) & (date >= first_appearance) & (status_phase < 7), net_summer_capacity_mw, 0)
    ) ## define planned capacity based on recorded operation date, first_appearance, and status phase (2nd condition handles null operation date)
  
  
  print('reading in other pertinent data sets for regression; monthly summaries of: 15min ercot data, peaker net margin, henry hub ng prices')
  df_monthly_gen_summary <- read_csv('../Data/ERCOT Compiled Data/ercot_gen_pa_price_monthly_stats.csv')
  df_pnm_summary <- read_csv('../Data/Peaker Net Margin/pnm_monthly_summary.csv')
  df_hh_ng_monthly_summary <- read_csv('../Data/Natural Gas Prices/ng_hh_price_monthly_summary.csv')
  df_weather <- read_csv('../Data/Temperature/montly_summary_weather_data.csv')
  df_economics <- read_csv('../Data/Economic/montly_economic_controls.csv')
  df_gini <- read_csv('../Data/EIA Compiled Data/gini_summary.csv') 
  
  
  new_column = paste('new_mw_phs_', 'entrants', sep='')
  
  df_strategic_pool_statistics <- df_panel_plant_gen %>%
    filter(status_phase %in% c(1,2,3,4,5,6)) %>%
    group_by(date) %>%
    summarize(strat_pool_n_plant_gen_id = n(),
              strat_pool_mean_status_phase = mean(status_phase, na.rm=T),
              strat_pool_mean_mw = mean(planned_capacity_mw, na.rm=T),
              strat_pool_total_mw = sum(planned_capacity_mw, na.rm=T)
    )  %>%
    mutate(date_minus_month = as.Date(date) %m+% months(1),
           date_minus_year = as.Date(date) %m+% months(12),
           date_minus_lag = as.Date(date) %m+% months(lag_months)
           )
  
  df_renew_breakout_strategic_pool_statistics <- df_panel_plant_gen %>%
    filter(status_phase %in% c(1,2,3,4,5,6)) %>%
    group_by(date, energy_source_code_group) %>%
    summarize(strat_pool_n_plant_gen_id = n(),
              strat_pool_mean_status_phase = mean(status_phase, na.rm=T),
              strat_pool_mean_mw = mean(planned_capacity_mw, na.rm=T),
              strat_pool_total_mw = sum(planned_capacity_mw, na.rm=T)
    )  %>%
    mutate(date_minus_month = as.Date(date) %m+% months(1),
           date_minus_year = as.Date(date) %m+% months(12),
           date_minus_lag = as.Date(date) %m+% months(lag_months)) %>%
    pivot_wider(id_cols = date_minus_lag, names_from = energy_source_code_group, values_from = c(strat_pool_n_plant_gen_id:strat_pool_total_mw)) %>%
    replace(is.na(.), 0)
  
  #### monthly total nameplate capacity
  print('Joining in total nameplate capacity')
  df_panel_plant_gen <- df_panel_plant_gen %>%
    left_join(df_panel_plant_gen %>% group_by(date) %>% summarize(month_total_nameplate_mw = sum(net_summer_capacity_mw, na.rm=TRUE)),
              by=c('date'))
  
  for (i in seq_along(market_segments)) {
    print(paste('creating ', lag_months, ' mo lag regression data for market segment: ', market_segments[i], sep = ''))
    
    print('Finding Total Capacity in MW by month')
    if (market_segments[i] == 'ng') {
      df_ng_new_in_phase <- df_panel_plant_gen %>%
        filter(energy_source_code_group == 'NG' & plant_gen_id %in% operating_generators) %>%
        rename(!!new_column := operating_capacity_mw)
    }
    if (market_segments[i] == 'renewable') {
      df_ng_new_in_phase <- df_panel_plant_gen %>%
        filter(energy_source_code_group == 'Renewables' & plant_gen_id %in% operating_generators) %>%
        rename(!!new_column := operating_capacity_mw)
    }
    if (market_segments[i] == 'market') {
      df_ng_new_in_phase <- df_panel_plant_gen %>%
        filter(status_phase %in% c(7,8))
        rename(!!new_column := operating_capacity_mw)
    }
    if (market_segments[i] == 'nonng') {
      df_ng_new_in_phase <- df_panel_plant_gen %>%
        filter(energy_source_code_group != 'NG') %>%
        rename(!!new_column := operating_capacity_mw)
    }
    if (market_segments[i] == 'wind') {
      df_ng_new_in_phase <- df_panel_plant_gen %>%
        filter(energy_source_code == 'WND') %>%
        rename(!!new_column := operating_capacity_mw)
    }
    if (market_segments[i] == 'solar') {
      df_ng_new_in_phase <- df_panel_plant_gen %>%
        filter(energy_source_code == 'SUN') %>%
        rename(!!new_column := operating_capacity_mw)
    }
    if (market_segments[i] == 'other') {
      df_ng_new_in_phase <- df_panel_plant_gen %>%
        filter(energy_source_code_group == 'Other') %>%
        rename(!!new_column := operating_capacity_mw)
    }
    
    print('Joining datasets')
    df_ng_regression <- df_ng_new_in_phase %>%
      # left_join(df_logistic_pool_statistics, by=c('date' = 'date_minus_month')) %>%
      left_join(df_strategic_pool_statistics, by=c('date' = 'date_minus_lag')) %>%
      # left_join(df_renew_breakout_logistic_pool_statistics, by=c('date' = 'date_minus_month')) %>%
      left_join(df_renew_breakout_strategic_pool_statistics, by=c('date' = 'date_minus_lag')) %>%
      left_join(df_monthly_gen_summary %>% select(-date), by=c('date'='date_minus_lag')) %>%
      left_join(df_pnm_summary %>% select(-date), by=c('date'='date_minus_lag')) %>%
      left_join(df_hh_ng_monthly_summary  %>% select(-date), by=c('date'='date_minus_lag')) %>%
      left_join(df_weather %>% select(-date, -year, -month), by=c('date'='date_minus_lag')) %>%
      left_join(df_economics, by = c('date'='date_minus_lag')) %>%
      left_join(df_gini, by = c('date'='date_minus_lag')) %>%
      mutate_if(is.numeric , replace_na, replace = 0) %>%
      mutate(year = year(date),
             month = month(date)) %>%
      dummy_cols(select_columns = c('year', 'month'))
    
    df_ng_regression <- df_ng_regression[, !grepl('sd|\\.y|\\.x', names(df_ng_regression))]
    print('subsetting rows')
    
    ### create monthly nameplate scarcity measure
    df_ng_regression <- df_ng_regression %>% mutate(nameplate_cap_scarcity_measure = mean_int_tot_gen_m_wh/(mean_int_tot_gen_m_wh+month_total_nameplate_mw))
    
    
    ## scaling units
    df_ng_regression <- df_ng_regression %>%
      mutate(p_labf = p_labf/1000000, #population to millions
             mean_int_tot_spin_cap_mw = mean_int_tot_spin_cap_mw/1000, #online cap to GW
             mean_rtoffcap = mean_rtoffcap/1000, #offline cap to GW
             mean_pnm = mean_pnm/1000, #pnm to $/GW
             mean_delta_pnm = mean_delta_pnm/1000, #pnm to $/GW
             mean_ng_cap_gini_coef = mean_ng_cap_gini_coef*100,
             mean_wnd_cap_gini_coef = mean_wnd_cap_gini_coef*100,
             nameplate_cap_scarcity_measure = nameplate_cap_scarcity_measure*100,
             mean_total_gen_rtcap_ratio = mean_total_gen_rtcap_ratio*100
             # strat_pool_total_mw_Renewables = strat_pool_total_mw_Renewables/1000, #appl pool to GW
             # strat_pool_total_mw_NG = strat_pool_total_mw_NG/1000 #appl pool to GW
             )
    
    
    
    print(paste('writing data set to csv for total changes: ', market_segments[i]), sep='')
    file_to_output <- paste('../Data/Regressions/pre_model_data/', market_segments[i], '_panel_totalquantity_12mo_lag.csv', sep='')
    write_csv(df_ng_regression, file_to_output)
  }
  print('12mo lag data sets for total capacity complete')
}


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
