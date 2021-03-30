#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
## Simulation study
##
## nested loop plots new style
## lindanab4@gmail.com - 20201111
#############################################################

##############################
# 0 - Load librairies ----
##############################
source(file =  "./rcode/visualisation/nlp_new_style.R")
summary <-readRDS(file = "./results/summaries/summary.Rds")
summary <- summary[order(-linear, skewness, R_squared),]

##############################
# 1 - IVS restricted ---------
##############################
use_method <- "complete_case"
limits1 = c(0.00, 0.003)
limits2 = c(0, 0.015)
pdf(paste0("./results/figures", "/nlpnew_mse_ivs.pdf"),
    width = 8, height = 5)
create_nlp_new_style(summary,
                     use_method,
                     limits1,
                     limits2)
dev.off()

##############################
# 2 - Val reg cal ------------
##############################
use_method <- "inadm_reg_cal"
limits1 = c(0.00, 0.003)
limits2 = c(0, 0.015)
pdf(paste0("./results/figures", "/nlpnew_mse_vrc.pdf"),
    width = 8, height = 5)
create_nlp_new_style(summary,
                     use_method,
                     limits1,
                     limits2)
dev.off()


##############################
# 3 - Efficient reg cal ------
##############################
# plot in online supplement
use_method <- "efficient_reg_cal"
limits1 = c(0.00, 0.003)
limits2 = c(0, 0.015)
limits3 = c(0, 0.005)
pdf(paste0("./results/figures", "/nlpnew_mse_supp_erc.pdf"),
    width = 8, height = 7.5)
create_nlp_new_style(summary,
                     use_method,
                     limits1,
                     limits2,
                     size_valdata_25 = T,
                     limits3)
dev.off()

##############################
# 3b -  ef reg cal -----------
##############################
use_method <- "efficient_reg_cal"
limits1 = c(0.00, 0.003)
limits2 = c(0, 0.015)
pdf(paste0("./results/figures", "/nlpnew_mse_erc.pdf"),
    width = 8, height = 5)
create_nlp_new_style(summary,
                     use_method,
                     limits1,
                     limits2)
dev.off()

##############################
# 4 -  st reg cal ------------
##############################
use_method <- "reg_cal"
use_sampling_strats <- c("random")
limits1 = c(-0.25, 0.25)
limits2 = c(-10, 100)
pdf(paste0("./results/figures", "/nlpnew_mse_rc.pdf"),
    width = 8, height = 5)
create_nlp_new_style(summary,
                     use_method,
                     limits1,
                     limits2)
dev.off()
