#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
## Simulation study
##
## eAppendix
## nested loop plots
## lindanab4@gmail.com - 20200528
#############################################################

##############################
# 0 - Load librairies ----
##############################
source(file =  "./rcode/visualisation/nested_loop_plot.R")
summary <-readRDS(file = "./results/summaries/summary.Rds")
summary <- summary[order(-linear, R_squared, skewness),]

##############################
# 1 - MSE reg_cal size_valdata = 0.10
##############################
use_methods <- "reg_cal"
png(paste0("./results/figures", "/mse_rc_size_valdata_10_random.png"),
    width = 4, height = 4, units = 'in', res = 100)
create_nlp_method(summary = summary,
                  stats = "mse",
                  ref = 0,
                  limits = c(-10, 100),
                  xlab = "2 x 4 x 3 = 24 ordered scenarios",
                  ylab = "mean squared error",
                  use_size_valdata = 0.1,
                  use_sampling_strat = "random", 
                  use_methods = use_methods,
                  legend = T,
                  y_axis_at = c(-10, 0, 50, 100),
                  y_axis_labels = c("", "0", "50", "100", ""),
                  legend_text = c("Regression calibration"),
                  lty = c(2)
                  )
dev.off()

png(paste0("./results/figures", "/mse_rc_size_valdata_10_extremes.png"),
    width = 4, height = 4, units = 'in', res = 100)
create_nlp_method(summary = summary,
                  stats = "mse",
                  ref = 0,
                  limits = c(-10, 100),
                  xlab = "2 x 4 x 3 = 24 ordered scenarios",
                  ylab = "mean squared error",
                  use_size_valdata = 0.1,
                  use_sampling_strat = "extremes",
                  use_methods = use_methods,
                  legend = T,
                  y_axis_at = c(-10, 0, 50, 100),
                  y_axis_labels = c("", "0", "50", "100", ""),
                  legend_text = c("Regression calibration"),
                  lty = c(2)
)
dev.off()

##############################
# 1b - MSE reg_cal size_valdata = 0.25
##############################
use_methods <- "reg_cal"
png(paste0("./results/figures", "/mse_rc_size_valdata_25_random.png"),
    width = 4, height = 4, units = 'in', res = 100)
create_nlp_method(summary = summary,
                  stats = "mse",
                  ref = 0,
                  limits = c(-0.1, 1),
                  xlab = "2 x 4 x 3 = 24 ordered scenarios",
                  ylab = "mean squared error",
                  use_size_valdata = 0.25,
                  use_sampling_strat = "random", 
                  use_methods = use_methods,
                  legend = T,
                  y_axis_at = c(-0.1, 0, 0.5, 1),
                  y_axis_labels = c("", "0", "0.5", "1", ""),
                  legend_text = c("Regression calibration"),
                  lty = c(2)
)
dev.off()

png(paste0("./results/figures", "/mse_rc_size_valdata_25_extremes.png"),
    width = 4, height = 4, units = 'in', res = 100)
create_nlp_method(summary = summary,
                  stats = "mse",
                  ref = 0,
                  limits = c(-0.1, 1),
                  xlab = "2 x 4 x 3 = 24 ordered scenarios",
                  ylab = "mean squared error",
                  use_size_valdata = 0.1,
                  use_sampling_strat = "extremes",
                  use_methods = use_methods,
                  legend = T,
                  y_axis_at = c(-0.1, 0, 0.5, 1),
                  y_axis_labels = c("", "0", "0.5", "1", ""),
                  legend_text = c("Regression calibration"),
                  lty = c(2))
dev.off()

##############################
# 2 - MSE cc/irc/erc (misc) size_valdata = 0.10
##############################
use_methods <- c("complete_case", "efficient_reg_cal", "inadm_reg_cal")
png(paste0("./results/figures", "/mse_misc_size_valdata_10_random.png"),
    width = 4, height = 4, units = 'in', res = 100)
create_nlp_method(summary = summary,
                  stats = "mse",
                  ref = 0,
                  limits = c(-0.005, 0.02),
                  xlab = "2 x 4 x 3 = 24 ordered scenarios",
                  ylab = "mean squared error",
                  use_size_valdata = 0.1,
                  use_sampling_strat = "random", 
                  use_methods = use_methods,
                  y_axis_at = c(-0.005, 0, 0.02),
                  y_axis_labels = c("-0.005", "0", "0.02", ""),
                  legend_text = c("Internal validation sample restricted",
                                  "Efficient regression calibration (RC)", 
                                  "Inadmissible RC"),
                  lty = c(1, 3, 4))
dev.off()

png(paste0("./results/figures", "/mse_misc_size_valdata_10_extremes.png"),
    width = 4, height = 4, units = 'in', res = 100)
create_nlp_method(summary = summary,
                  stats = "mse",
                  ref = 0,
                  limits = c(-0.005, 0.02),
                  xlab = "2 x 4 x 3 = 24 ordered scenarios",
                  ylab = "mean squared error",
                  use_size_valdata = 0.1,
                  use_sampling_strat = "extremes", 
                  use_methods = use_methods,
                  y_axis_at = c(-0.005, 0, 0.02),
                  y_axis_labels = c("-0.005", "0", "0.02", ""),
                  legend_text = c("Internal validation sample restricted",
                                  "Efficient regression calibration (RC)", 
                                  "Inadmissible RC"),
                  lty = c(1, 3, 4))
dev.off()

##############################
# 2b - MSE cc/irc/erc (misc) size_valdata = 0.25
##############################
use_methods <- c("complete_case", "efficient_reg_cal", "inadm_reg_cal")
png(paste0("./results/figures", "/mse_misc_size_valdata_25_random.png"),
    width = 4, height = 4, units = 'in', res = 100)
create_nlp_method(summary = summary,
                  stats = "mse",
                  ref = 0,
                  limits = c(-0.005, 0.02),
                  xlab = "2 x 4 x 3 = 24 ordered scenarios",
                  ylab = "mean squared error",
                  use_size_valdata = 0.25,
                  use_sampling_strat = "random", 
                  use_methods = use_methods,
                  y_axis_at = c(-0.005, 0, 0.02),
                  y_axis_labels = c("-0.005", "0", "0.02", ""),
                  legend_text = c("Internal validation sample restricted",
                                  "Efficient regression calibration (RC)", 
                                  "Inadmissible RC"),
                  lty = c(1, 3, 4))
dev.off()

png(paste0("./results/figures", "/mse_misc_size_valdata_25_extremes.png"),
    width = 4, height = 4, units = 'in', res = 100)
create_nlp_method(summary = summary,
                  stats = "mse",
                  ref = 0,
                  limits = c(-0.005, 0.02),
                  xlab = "2 x 4 x 3 = 24 ordered scenarios",
                  ylab = "mean squared error",
                  use_size_valdata = 0.25,
                  use_sampling_strat = "extremes", 
                  use_methods = use_methods,
                  y_axis_at = c(-0.005, 0, 0.02),
                  y_axis_labels = c("-0.005", "0", "0.02", ""),
                  legend_text = c("Internal validation sample restricted",
                                  "Efficient regression calibration (RC)", 
                                  "Inadmissible RC"),
                  lty = c(1, 3, 4))
dev.off()
