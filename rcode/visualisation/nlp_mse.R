#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
## Simulation study
##
## Mean squared error nested loop plots
## lindanab4@gmail.com - 20200428
#############################################################

##############################
# 0 - Load librairies ----
##############################
source(file =  "./rcode/visualisation/nested_loop_plot.R")
summary <-readRDS(file = "./results/summaries/summary.Rds")
summary <- summary[order(-linear, R_squared, skewness),]

##############################
# 1 - Naive ----
##############################
# png(paste0("./results/figures", "/mse_size_valdata_40_naive.png"),
#     width = 4, height = 4, units = 'in', res = 500)
# create_nlp(summary = summary,
#            stats = "mse",
#            ref = 0,
#            limits = c(-100, 100),
#            xlab = "2 x 4 x 3 = 24 ordered scenarios",
#            ylab = "mean squared error",
#            use_size_valdata = 0.4,
#            use_method = "naive", 
#            use_sampling_strats = c("random", "uniform", "extremes")[1],
#            legend = F)
# dev.off()

##############################
# 2 - Complete case ----
##############################
png(paste0("./results/figures", "/mse_size_valdata_40_cc.png"),
    width = 4, height = 4, units = 'in', res = 500)
create_nlp(summary = summary,
           stats = "mse",
           ref = 0,
           limits = c(-0.01, 0.05),
           xlab = "2 x 4 x 3 = 24 ordered scenarios",
           ylab = "mean squared error",
           use_size_valdata = 0.4,
           use_method = "complete_case", 
           use_sampling_strats = c("random", "uniform", "extremes"))
dev.off()

##############################
# 3 - Regression calibration ----
##############################
png(paste0("./results/figures", "/mse_size_valdata_40_rc.png"),
    width = 4, height = 4, units = 'in', res = 500)
create_nlp(summary = summary,
           stats = "mse",
           ref = 0, 
           limits = c(-0.01, 0.05),
           xlab = "2 x 4 x 3 = 24 ordered scenarios",
           ylab = "mean squared error",
           use_size_valdata = 0.4,
           use_method = "reg_cal", 
           use_sampling_strats = c("random", "uniform", "extremes"))
dev.off()

##############################
# 4 - Efficient Regression calibration 
##############################
png(paste0("./results/figures", "/mse_size_valdata_40_erc.png"),
    width = 4, height = 4, units = 'in', res = 500)
create_nlp(summary = summary,
           stats = "mse",
           ref = 0,
           limits = c(-0.01, 0.05),
           xlab = "2 x 4 x 3 = 24 ordered scenarios",
           ylab = "mean squared error",
           use_size_valdata = 0.4,
           use_method = "efficient_reg_cal", 
           use_sampling_strats = c("random", "uniform", "extremes"))
dev.off()

##############################
# 5 - Inadmissible Regression calibration 
##############################
png(paste0("./results/figures", "/mse_size_valdata_40_irc.png"),
    width = 4, height = 4, units = 'in', res = 500)
create_nlp(summary = summary,
           stats = "mse",
           ref = 0,
           limits = c(-0.01, 0.05),
           xlab = "2 x 4 x 3 = 24 ordered scenarios",
           ylab = "mean squared error",
           use_size_valdata = 0.4,
           use_method = "inadm_reg_cal", 
           use_sampling_strats = c("random", "uniform", "extremes"))
dev.off()
