#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
## Simulation study
##
## Method: ivs restricted analysis
## nested loop plots
## lindanab4@gmail.com - 20200929
#############################################################

##############################
# 0 - Load librairies ----
##############################
source(file =  "./rcode/visualisation/nested_loop_plot.R")
summary <-readRDS(file = "./results/summaries/summary.Rds")
summary <- summary[order(-linear, R_squared, skewness),]
use_method <- "complete_case"
use_sampling_strats <- c("random", "uniform", "extremes")

##############################
# 1 - Percentage bias ----
##############################
png(paste0("./results/figures", "/perc_bias_cc.png"),
    width = 4, height = 4, units = 'in', res = 250)
create_nlp(
  summary = summary,
  stats = "perc_bias",
  ref = 0,
  limits = c(-10, 2),
  xlab = "2 x 4 x 3 = 24 ordered scenarios",
  ylab = "percentage bias (%)",
  use_size_valdata = 0.4,
  use_sampling_strats = use_sampling_strats,
  use_method = use_method,
  y_axis_at = c(-10, 0, 2),
  y_axis_labels = c("-10", "0", "2", "")
)
dev.off()

##############################
# 2 - MSE ----
##############################
pdf(paste0("./results/figures", "/mse_cc.pdf"),
    width = 8, height = 3.5)
create_nlp(
  summary = summary,
  stats = "mse",
  ref = -1,
  limits = c(0, 0.003),
  xlab = "2 x 4 x 3 = 24 ordered scenarios",
  ylab = "mean squared error",
  use_size_valdata = 0.4,
  use_sampling_strats = use_sampling_strats,
  legend = T,
  use_method = use_method,
  y_axis_at = c(0, 0.003),
  y_axis_labels = c("0", "0.003", "")
)

dev.off()

##############################
# 3 - coverage ----
##############################
png(paste0("./results/figures", "/cover_cc.png"),
    width = 4, height = 4, units = 'in', res = 250)
create_nlp(
  summary = summary,
  stats = "cover",
  ref = 0.95,
  limits = c(0.85, 1),
  xlab = "2 x 4 x 3 = 24 ordered scenarios",
  ylab = "coverage",
  use_size_valdata = 0.4,
  use_sampling_strats = use_sampling_strats,
  use_method = use_method,
  y_axis_at = c(0.8, 0.85, 1),
  y_axis_labels = c("0.8", "0.85", "1", "")
)
dev.off()

## Size valdata = 0.25
##############################
# 1 - Percentage bias ----
##############################
png(paste0("./results/figures", "/perc_bias_cc_010.png"),
    width = 4, height = 4, units = 'in', res = 250)
create_nlp(
  summary = summary,
  stats = "perc_bias",
  ref = 0,
  limits = c(-15, 2),
  xlab = "2 x 4 x 3 = 24 ordered scenarios",
  ylab = "percentage bias (%)",
  use_size_valdata = 0.1,
  use_sampling_strats = use_sampling_strats,
  use_method = use_method,
  y_axis_at = c(-15, 0, 2),
  y_axis_labels = c("-15", "0", "2", "")
)
dev.off()

##############################
# 2 - MSE ----
##############################
pdf(paste0("./results/figures", "/mse_cc_10.pdf"),
    width = 8, height = 3.5)
create_nlp(
  summary = summary,
  stats = "mse",
  ref = -1,
  limits = c(0, 0.015),
  digits = 4,
  xlab = "2 x 4 x 3 = 24 ordered scenarios",
  ylab = "mean squared error",
  use_size_valdata = 0.1,
  use_sampling_strats = use_sampling_strats,
  legend = T,
  use_method = use_method,
  y_axis_at = c(0, 0.015),
  y_axis_labels = c("0", "0.015", "")
)
dev.off()

##############################
# 3 - coverage ----
##############################
png(paste0("./results/figures", "/cover_cc_010.png"),
    width = 4, height = 4, units = 'in', res = 250)
create_nlp(
  summary = summary,
  stats = "cover",
  ref = 0.95,
  limits = c(0.85, 1),
  xlab = "2 x 4 x 3 = 24 ordered scenarios",
  ylab = "coverage",
  use_size_valdata = 0.1,
  use_sampling_strats = use_sampling_strats,
  use_method = use_method,
  y_axis_at = c(0.8, 0.85, 1),
  y_axis_labels = c("0.8", "0.85", "1", "")
)
dev.off()
