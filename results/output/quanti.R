library(dplyr)
library(tidyverse)
source(file =  "./rcode/visualisation/nested_loop_plot.R")
summary <-readRDS(file = "./results/summaries/summary.Rds")
summary <- summary[order(-linear, R_squared, skewness),]
use_sampling_strats <- c("random", "uniform", "extremes")
select_summary(summary, 
               use_size_valdata = 0.1, 
               use_method = "inadm_reg_cal",
               use_sampling_strats = use_sampling_strats) %>%
  as.data.frame() %>%
  #filter(
    #random.R_squared %in% c(0.2, 0.4, 0.6, 0.8) & 
           #random.skewness %in% c(1.5, 3.0) &
          #random.linear %in% c(0, 1)) %>%
  select(extremes.mse_mcse) %>%
  max()

# abstract
sum_df_cc_40 <-
  select_summary(
    summary,
    use_size_valdata = 0.4,
    use_method = "complete_case",
    use_sampling_strats = use_sampling_strats
  ) %>%
  as.data.frame()
# mean uniform mse cc
(sum_df_cc_40 %>%
  pull(random.mse) %>%
  mean() - 
  sum_df_cc_40 %>%
  pull(uniform.mse) %>%
  mean()) / 
  sum_df_cc_40 %>%
  pull(random.mse) %>%
  mean() * 100
# mean extremes mse cc
(sum_df_cc_40 %>%
    pull(random.mse) %>%
    mean() - 
    sum_df_cc_40 %>%
    pull(extremes.mse) %>%
    mean()) / 
  sum_df_cc_40 %>%
  pull(random.mse) %>%
  mean() * 100

# vrc
sum_df_vrc_40 <-
  select_summary(
    summary,
    use_size_valdata = 0.4,
    use_method = "inadm_reg_cal",
    use_sampling_strats = use_sampling_strats
  ) %>%
  as.data.frame()
# mean uniform mse cc
(sum_df_vrc_40 %>%
    pull(random.mse) %>%
    mean() - 
    sum_df_vrc_40 %>%
    pull(uniform.mse) %>%
    mean()) / 
  sum_df_vrc_40%>%
  pull(random.mse) %>%
  mean() * 100
# mean extremes mse cc
(sum_df_vrc_40 %>%
    pull(random.mse) %>%
    mean() - 
    sum_df_vrc_40 %>%
    pull(extremes.mse) %>%
    mean()) / 
  sum_df_vrc_40 %>%
  pull(random.mse) %>%
  mean() * 100

# abstract
sum_df_cc_10 <-
  select_summary(
    summary,
    use_size_valdata = 0.1,
    use_method = "complete_case",
    use_sampling_strats = use_sampling_strats
  ) %>%
  as.data.frame()
# mean uniform mse cc
(sum_df_cc_10 %>%
    pull(random.mse) %>%
    mean() - 
    sum_df_cc_10 %>%
    pull(uniform.mse) %>%
    mean()) / 
  sum_df_cc_10 %>%
  pull(random.mse) %>%
  mean() * 100
# mean extremes mse cc
(sum_df_cc_10 %>%
    pull(random.mse) %>%
    mean() - 
    sum_df_cc_10 %>%
    pull(extremes.mse) %>%
    mean()) / 
  sum_df_cc_10 %>%
  pull(random.mse) %>%
  mean() * 100

# vrc
sum_df_vrc_10 <-
  select_summary(
    summary,
    use_size_valdata = 0.1,
    use_method = "inadm_reg_cal",
    use_sampling_strats = use_sampling_strats
  ) %>%
  as.data.frame()
# mean uniform mse cc
(sum_df_vrc_10 %>%
    pull(random.mse) %>%
    mean() - 
    sum_df_vrc_10 %>%
    pull(uniform.mse) %>%
    mean()) / 
  sum_df_vrc_10%>%
  pull(random.mse) %>%
  mean() * 100
# mean extremes mse cc
(sum_df_vrc_10 %>%
    pull(random.mse) %>%
    mean() - 
    sum_df_vrc_10 %>%
    pull(extremes.mse) %>%
    mean()) / 
  sum_df_vrc_10 %>%
  pull(random.mse) %>%
  mean() * 100

sum_df_vrc_40 %>%
  select(random.perc_bias, uniform.perc_bias, extremes.perc_bias) %>%
  apply(., MARGIN = 2, FUN = range)
sum_df_vrc_40 %>%
  select(random.cover, uniform.cover, extremes.cover) %>%
  apply(., MARGIN = 2, FUN = range)
sum_df_vrc_10 %>%
  select(random.perc_bias, uniform.perc_bias, extremes.perc_bias) %>%
  apply(., MARGIN = 2, FUN = range)
sum_df_vrc_10 %>%
  select(random.cover, uniform.cover, extremes.cover) %>%
  apply(., MARGIN = 2, FUN = range)

# mcse
sum_df_cc_40 %>%
  select(random.bias_mcse, uniform.bias_mcse, extremes.bias_mcse) %>%
  apply(., MARGIN = 2, FUN = max) %>% round(., digits = 5) # < 0.001
sum_df_cc_40 %>%
  select(random.cover_mcse, uniform.cover_mcse, extremes.cover_mcse) %>%
  apply(., MARGIN = 2, FUN = max) %>% round(., digits = 4) # < 0.0005
sum_df_cc_40 %>%
  select(random.mse_mcse, uniform.mse_mcse, extremes.mse_mcse) %>%
  apply(., MARGIN = 2, FUN = max) %>% round(., digits = 6) # < 0.0001

  
        