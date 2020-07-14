#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
## Simulation study
##
## Tabularise summaries mse
## lindanab4@gmail.com - 20200425
#############################################################

##############################
# 0 - Load librairies + source code 
##############################
library(xtable)
source(file = "./rcode/dgm/sim_scen.R")
caption <- "Values for the parameters variedin the simulation study in a full factorial design"

s <- unique(datagen_scenarios()$skewness)
lambda <- round(calc_lambda(unique(datagen_scenarios()$skewness)), 1)
r <- unique(datagen_scenarios()$R_squared)[-1]
tau <- round(calc_tau(unique(datagen_scenarios()$R_squared)[-1]), 1)

table <- data.frame(matrix(nrow = 3, ncol = 2))
colnames(table) <- c("Parameter", "Value")
table$Parameter <- c("Linear", "R-squared ($tau$)", "Skewness ($lambda$)")
table[1, "Value"] <- "yes/no"
table[2, "Value"] <- toString(paste0(r, " (", tau, ")"))
table[3, "Value"] <- toString(paste0(s, " (", lambda, ")"))

# Create TeX table
table_xtable <- print(xtable(table, 
                             caption = caption,
                             digits = 0), 
                      include.rownames = FALSE)
file_con <- file("./results/tables/table_datagenscen.txt")
writeLines(table_xtable, file_con)
close(file_con)
