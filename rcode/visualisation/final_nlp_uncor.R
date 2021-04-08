#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
## Simulation study
##
## Method: validation regression calibration
## nested loop plots
## lindanab4@gmail.com - 20200929
#############################################################

##############################
# 0 - Load librairies ----
##############################
library(extrafont)
source(file =  "./rcode/visualisation/nlp_select_summary.R")
summary <-readRDS(file = "./results/summaries/summary.Rds")
summary <- summary[order(-linear, skewness, R_squared),]
use_method <- "naive"
use_sampling_strats <- c("random")
sub_summary <- select_summary(summary, 
                              use_size_valdata = 0.1, 
                              use_method = use_method,
                              use_sampling_strats = use_sampling_strats)


##############################
# 1 - Helper functions ----
##############################
make_plot_percbias_uncor <- function(asp) {
  scen_range <- 1:12 # no of scens
  stats <- "perc_bias"
  ylab <- "Percentage Bias, %"
  limits <- c(-100, 0)
  plot(
    0,
    type = "n",
    xaxt = "n",
    yaxt = "n",
    yaxs = "i",
    xaxs = "i",
    frame.plot = F,
    ann = F,
    xlim = c(0, max(scen_range)),
    ylim = limits,
    asp = asp
  )
  for (i in c(1, 0)) {
    with (sub_summary$random[linear == i, ], lines(
      c(0, scen_range),
      c(get(stats)[1], get(stats)),
      type = "S",
      lty = ifelse(i == 1, 1, 2),
      asp = asp
    ))
  }
  axis(
    1,
    at = c(scen_range - 0.5),
    las = 2,
    labels = FALSE,
    asp = asp
  )
  segments(0,-100, 0.5,-100)
  segments(11.5,-100, 12,-100)
  text(x = c(scen_range - 0.5), 
       y = par("usr")[3] - 6,
       labels = c(
         expression(paste(R ^ 2, " = 0.2, S = 0.1")),
         expression(paste(R ^ 2, " = 0.4, S = 0.1")),
         expression(paste(R ^ 2, " = 0.6, S = 0.1")),
         expression(paste(R ^ 2, " = 0.8, S = 0.1")),
         expression(paste(R ^ 2, " = 0.2, S = 1.5")),
         expression(paste(R ^ 2, " = 0.4, S = 1.5")),
         expression(paste(R ^ 2, " = 0.6, S = 1.5")),
         expression(paste(R ^ 2, " = 0.8, S = 1.5")),
         expression(paste(R ^ 2, " = 0.2, S = 3.0")),
         expression(paste(R ^ 2, " = 0.4, S = 3.0")),
         expression(paste(R ^ 2, " = 0.6, S = 3.0")),
         expression(paste(R ^ 2, " = 0.8, S = 3.0"))
       ), 
       srt = 45, 
       adj = 1)
  mtext("Scenario Parameters",
        side = 1, 
        line = 6.25)
  axis(
    2,
    at = c(-100,-75,-50,-25, 0),
    labels = c(
      expression(-100),
      expression(-75),
      expression(-50),
      expression(-25),
      0
    ),
    las = 1
  )
  mtext(ylab,
        side = 2,
        line = 4)
}

##############################
# 2 - Save plot ----
##############################
pdf(paste0("./results/figures", "/percbias_uncor.pdf"),
    width = 2.8, height = 2.8, family = "Arial",
    pointsize = 8)
#layout(matrix(c(1,0,0,0), 2, 2, byrow = TRUE))
par(
  mar = c(7.5, 6, 2.5, 4),
  xpd = NA,
  family = "Arial",
  cex = 0.83
)
make_plot_percbias_uncor(asp = 12/100)
dev.off()

