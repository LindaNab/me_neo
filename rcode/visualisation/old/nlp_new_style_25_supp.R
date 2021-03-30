#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## Create nested loop plot new style for 25%
## lindanab4@gmail.com - 20201123
#############################################################

##############################
# 0 - Load librairies ----
##############################
source(file =  "./rcode/dgm/sim_scen.R")
library(data.table)
summary <-readRDS(file = "./results/summaries/summary.Rds")
summary <- summary[order(-linear, skewness, R_squared),]

##############################
# 1 - Helper functions ----
##############################
# Parameters used to loop through different scenarios
get_labels_x_axis <- function(){
  levels <-
    expand.grid(unique(datagen_scenarios()$R_squared)[-c(1, 6)],
                unique(datagen_scenarios()$skewness)[-2])
  colnames(levels) <- c("R_squared", "Skewness")
  labels <- list()
  labels[[1]] <- expression(paste(R^2, " = 0.2, S = 0.1"))
  labels[[2]] <- expression(paste(R^2, " = 0.4, S = 0.1"))
  labels[[3]] <- expression(paste(R^2, " = 0.6, S = 0.1"))
  labels[[4]] <- expression(paste(R^2, " = 0.8, S = 0.1"))
  
  labels[[5]] <- expression(paste(R^2, " = 0.2, S = 1.5"))
  labels[[6]] <- expression(paste(R^2, " = 0.4, S = 1.5"))
  labels[[7]] <- expression(paste(R^2, " = 0.6, S = 1.5"))
  labels[[8]] <- expression(paste(R^2, " = 0.8, S = 1.5"))
  
  labels[[9]] <- expression(paste(R^2, " = 0.2, S = 3.0"))
  labels[[10]] <- expression(paste(R^2, " = 0.4, S = 3.0"))
  labels[[11]] <- expression(paste(R^2, " = 0.6, S = 3.0"))
  labels[[12]] <- expression(paste(R^2, " = 0.8, S = 3.0"))
  
  labels
}

# creates a list of summaries, for use_sampling_strats 
# using the selected use_size_valdata and use_method
select_summary <- function(summary, 
                           use_size_valdata, 
                           use_method, 
                           use_sampling_strats) {
  summary <- subset(
    summary,
    size_valdata == use_size_valdata &
      method == use_method &
      (R_squared == 0.2 | R_squared == 0.4 | R_squared == 0.6 | R_squared == 0.8) &
      (skewness == 0.1 | skewness == 1.5 | skewness == 3)
  )
  select_sampling_strat <- function(summary, use_sampling_strat){
    subset <- subset(summary,
                     sampling_strat == use_sampling_strat)
  }
  out <- lapply(as.list(use_sampling_strats),
                FUN = select_sampling_strat,
                summary = summary)
  names(out) <- use_sampling_strats
  out
}

# the code of this function is not wonderful, though it works ;-)
create_nlp_new_style_25 <- function(summary,
                                    use_method,
                                    limits1) {
  #dev.new(width = 8, height = 4, noRStudioGD = TRUE)
  #### internal validation sample of 25%
  use_sampling_strats = c("random", "uniform", "extremes")
  # get summary for the method and sampling strats (all)
  sub_summary <- select_summary(summary, 
                                use_size_valdata = 0.25, 
                                use_method = use_method,
                                use_sampling_strats = use_sampling_strats)
  labels_x_axis <- get_labels_x_axis()
  scen_range <- 1:length(labels_x_axis) # no of scens
  stats = "mse"
  ylab = "mean squared error"
  
  par(mfrow = c(1, 2), 
      mgp = c(1, 0.25, 0), 
      mar = c(4, 4, 4, 0.5),
      yaxs = "i",
      xaxs = "i",
      xpd = TRUE) # allows to put legend outside the margins
  # init plot
  plot(0,
       type = "n",
       xaxt = "n",
       yaxt = "n",
       frame.plot = F,
       ann = F,
       xlim = c(0, max(scen_range)),
       ylim = c(limits1[1], limits1[2])
  )
  # lines with mean squared error
  for (i in seq_along(sub_summary)) {
    with (sub_summary[[i]][linear == 1,], lines( # linear setting
      c(0, scen_range),
      c(get(stats)[1], get(stats)),
      type = "S",
      lty = i
    ))
  }
  # x axis
  axis(1,
       at = c(0, scen_range),
       labels = F,
       tck = - 0.01,
       cex.axis = 0.75
  ) # y axis
  axis(2, 
       at = limits1,
       tck = - 0.01,
       cex.axis = 0.75
  )
  # text y axis
  mtext(ylab, 
        side = 2, 
        line = 1,
        cex = 0.75)
  # panel text row 1
  mtext("sample size = 25%",
        side = 2, 
        line = 2.3)
  legend(
    x = 0.1,
    y = 0.0055,
    legend = c("random"),
    lty = 1,
    lwd = 1.75,
    bty = "n",
    horiz = T,
    cex = 0.85,
    text.width = 1.5,
    seg.len = 2
  )
  legend(
    x = 5.5,
    y = 0.0055,
    legend = c("stratified random"),
    lty = 2,
    lwd = 1.75,
    bty = "n",
    horiz = T,
    cex = 0.85,
    seg.len = 2
  )
  # panel text column 1
  mtext("Linear measurement error model", 
        side = 3, 
        line = 2,
        cex = 1)
  for (i in 1:length(labels_x_axis)) {
    text(
      x = i - 0.5,
      y = par("usr")[3] - 0.0002 ,
      labels = labels_x_axis[[i]],
      xpd = NA,
      srt = 30,
      adj = 1,
      cex = 0.75
    )
  }
  # non linear
  par(mar = c(4, 0.5, 4, 4)) # mirrors the above
  # init plot
  plot(0,
       type = "n",
       xaxt = "n",
       yaxt = "n",
       frame.plot = F,
       ann = F,
       xlim = c(0, max(scen_range)),
       ylim = c(limits1[1], limits1[2])
  )
  for (i in seq_along(sub_summary)) {
    with (sub_summary[[i]][linear == 0,], lines( # non-linear
      c(0, scen_range),
      c(get(stats)[1], get(stats)),
      type = "S",
      lty = i
    ))
  }
  # x axis
  axis(1,
       at = c(0, scen_range),
       labels = F,
       tck = - 0.01,
       cex.axis = 0.75
  ) 
  legend(
    x = 0.1,
    y = 0.0055,
    legend = c("extremes"),
    lty = 3,
    lwd = 1.75,
    bty = "n",
    horiz = T,
    cex = 0.85,
    seg.len = 2
  )
  # panel text column 2 
  mtext("Non-linear measurement error model", 
        side = 3, 
        line = 2,
        cex = 1)
  for (i in 1:length(labels_x_axis)) {
    text(
      x = i - 0.5,
      y = par("usr")[3] - 0.0002,
      labels = labels_x_axis[[i]],
      xpd = NA,
      srt = 30,
      adj = 1,
      cex = 0.75
    )
  }
}

##############################
# 2 - Create plots        ----
##############################
use_method <- "complete_case"
limits1 = c(0, 0.005)
pdf(paste0("./results/figures", "/nlpnew_mse_ivs_25.pdf"),
    width = 8, height = 3.5)
create_nlp_new_style_25(summary,
                        use_method,
                        limits1)
dev.off()
  




use_method <- "inadm_reg_cal"
limits1 = c(0, 0.005)
pdf(paste0("./results/figures", "/nlpnew_mse_vrc_25.pdf"),
    width = 8, height = 3.5)
create_nlp_new_style_25(summary,
                        use_method,
                        limits1)
dev.off()
