#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
## Simulation study
##
## Sampling strategy: uniform
## nested loop plots
## lindanab4@gmail.com - 20200506
#############################################################

##############################
# 0 - Load librairies ----
##############################
source(file =  "./rcode/visualisation/nested_loop_plot.R")
summary <-readRDS(file = "./results/summaries/summary.Rds")
summary <- summary[order(-linear, R_squared, skewness),]
use_methods <- c("complete_case", "reg_cal", 
                 "efficient_reg_cal", "inadm_reg_cal")

##############################
# 1 - Percentage bias ----
##############################
png(paste0("./results/figures", "/perc_bias_uniform.png"),
    width = 4, height = 4, units = 'in', res = 100)
create_nlp_method(summary = summary,
                  stats = "perc_bias",
                  ref = 0,
                  limits = c(-20, 10),
                  xlab = "2 x 4 x 3 = 24 ordered scenarios",
                  ylab = "percentage bias (%)",
                  use_size_valdata = 0.4,
                  use_sampling_strat = "uniform", 
                  use_methods = use_methods,
                  y_axis_at = c(-20, 0, 10),
                  y_axis_labels = c("-20", "0", "10", ""))
dev.off()

##############################
# 2 - MSE ----
##############################
png(paste0("./results/figures", "/mse_uniform.png"),
    width = 4, height = 4, units = 'in', res = 100)
create_nlp_method(summary = summary,
                  stats = "mse",
                  ref = 0,
                  limits = c(-0.05, 0.25),
                  xlab = "2 x 4 x 3 = 24 ordered scenarios",
                  ylab = "mean squared error",
                  use_size_valdata = 0.4,
                  use_sampling_strat = "uniform", 
                  use_methods = use_methods,
                  y_axis_at = c(-0.05, 0, 0.25),
                  y_axis_labels = c("-0.05", "0", "0.25", ""))
dev.off()

##############################
# 3 - coverage ----
##############################
png(paste0("./results/figures", "/cover_uniform.png"),
    width = 4, height = 4, units = 'in', res = 100)
create_nlp_method(summary = summary,
                  stats = "cover",
                  ref = 0.95,
                  limits = c(0.8, 1),
                  xlab = "2 x 4 x 3 = 24 ordered scenarios",
                  ylab = "coverage",
                  use_size_valdata = 0.4,
                  use_sampling_strat = "uniform", 
                  use_methods = use_methods,
                  y_axis_at = c(0.8, 0.95, 1),
                  y_axis_labels = c("0.8", "0.95", "1", ""))
dev.off()
