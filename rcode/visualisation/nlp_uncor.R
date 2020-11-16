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
source(file =  "./rcode/visualisation/nlp_new_style.R")
summary <-readRDS(file = "./results/summaries/summary.Rds")
summary <- summary[order(-linear, skewness, R_squared),]
use_method <- "naive"
use_sampling_strats <- c("random")
sub_summary <- select_summary(summary, 
                              use_size_valdata = 0.1, 
                              use_method = use_method,
                              use_sampling_strats = use_sampling_strats)

##############################
# 1 - Percentage bias ----
##############################
pdf(paste0("./results/figures", "/percbias_uncor.pdf"),
    width = 8, height = 3.5)
labels_x_axis <- get_labels_x_axis()
scen_range <- 1:length(labels_x_axis) # no of scens
stats <- "perc_bias"
ylab <- "Percentage bias (%)"
limits <- c(-100, 0)
par(mfrow = c(1, 2), 
    mgp = c(1, 0.25, 0), 
    mar = c(4, 4, 2, 0.5),
    yaxs = "i",
    xaxs = "i",
    xpd = TRUE) # allows to put legend outside the margins
plot(0,
     type = "n",
     xaxt = "n",
     yaxt = "n",
     frame.plot = F,
     ann = F,
     xlim = c(0, max(scen_range)),
     ylim = c(limits[1], limits[2])
)
for (i in seq_along(sub_summary)) {
  with (sub_summary[[i]][linear == 1,], lines(
    c(0, scen_range),
    c(get(stats)[1], get(stats)),
    type = "S",
    lty = i
  ))
}
axis(1,
     at = c(0, scen_range),
     labels = F,
     tck = - 0.01,
     cex.axis = 0.75
)
# labels x axis 
for (i in 1:length(labels_x_axis)) {
  text(
    x = i-0.5,
    y = par("usr")[3] - 2,
    labels = labels_x_axis[[i]],
    xpd = NA,
    srt = 30,
    adj = 1,
    cex = 0.75
  )
}
axis(2, 
     at = limits,
     tck = - 0.01,
     cex.axis = 0.75
)
mtext(ylab, 
      side = 2, 
      line = 1,
      cex = 0.75)
# panel text column 1
mtext("Linear measurement error model", 
      side = 3, 
      line = 1,
      cex = 1)
# non linear
par(mar = c(4, 0.5, 2, 4)) # mirrors the above margins
# init plot
plot(0,
     type = "n",
     xaxt = "n",
     yaxt = "n",
     frame.plot = F,
     ann = F,
     xlim = c(0, max(scen_range)),
     ylim = c(limits[1], limits[2])
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
# labels x axis 
for (i in 1:length(labels_x_axis)) {
  text(
    x = i-0.5,
    y = par("usr")[3] - 2,
    labels = labels_x_axis[[i]],
    xpd = NA,
    srt = 30,
    adj = 1,
    cex = 0.75
  )
}
# panel text column 2 
mtext("Non-linear measurement error model", 
      side = 3, 
      line = 1,
      cex = 1)
dev.off()



