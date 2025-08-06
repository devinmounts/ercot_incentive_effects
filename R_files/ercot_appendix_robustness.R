##### packages 
if (!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse', 'ggplot2', 'readxl', 'janitor', 'lubridate', 'geojsonio', 'broom', 'sp', 'zoo', 'fastDummies', 'stargazer', 'RStata', 'suncalc',
               'cowplot', 'olsrr', 'sf', 'MESS', 'dplyr')


### necessary scripts
source('ercot_preprocess_functions.R')
source('eia_pre_process_functions.R')
source('statistic_functions.R')
source('regression_script.R')
source('figure_and_stat_functions.R')
source('variable_layers_regression_script.R')

# #######################################################################
# #######################################################################
# ################ Appendix Figures and Tables ##########################
# #######################################################################
# #######################################################################
# 
# ### Summary Statistics for capacity model result from functions in main body section
# 
# ### Profits of Major Firms and significance of scarcity adders
# print('Plotting Profit Margin of Major Firms')
# plot_profit_margin_select_firms()
# 
# ### Full capacity model and applicant pool results tables produced in main body section
# 
# #######################################################################################
# ################### Underbidding Autoregressive Robustness ############################
# ######################################################################################
# run_polynomial_weather=TRUE
# print('running rls ar1 underbidding model')
# run_rls_ar1_timeseries_underbidding_model(run_polynomial_weather)
# print('running rls ar10 underbidding model')
# run_rls_ar10_timeseries_underbidding_model(run_polynomial_weather)
# 
# 
# #######################################################################################
# ################### Capacity Model Robustness ############################
# ######################################################################################
# print('running variable lags test')
# run_variable_lags_test()
# print('create variable lags appendix and summary figure')
# create_variable_lags_appendix_summary_and_figure()
# gc()

print('run variable layers regression test')
run_variable_layers_regression_test()
print('format and save latex output of variable layers regression')
format_and_save_latex_output_of_variable_layers_regression()
gc()

#######################################################################################
######################## Underbidding Matching Robustness ############################
######################################################################################
# options("RStata.StataPath" = RStataPath)
# options("RStata.StataVersion" = RStataVersion)
# stata('../Stata/underbidding_matching_robustness.do')
format_underbidding_robustness_latex_table()