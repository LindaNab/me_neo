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
summary <- readRDS(file = "./results/summaries/summary.Rds")
summary <- summary[order(-linear, skewness, R_squared),]
use_sampling_strats = c("random", "uniform", "extremes")

##############################
# 1 - Percentage bias ----
##############################
make_plot_mse <- function(use_linear,
                          use_method,
                          use_size_valdata,
                          limits,
                          txt,
                          asp,
                          legend = 1,
                          legend_where = "bottomleft",
                          box_legend = TRUE,
                          y_axis_tcks,
                          y_axis_labels,
                          adj_x_label) {
  # get summary for the method and sampling strats (all)
  sub_summary <- select_summary(summary, 
                                use_size_valdata = use_size_valdata, 
                                use_method = use_method,
                                use_sampling_strats = use_sampling_strats)
  scen_range <- 1:12 # no of scens
  stats <- "mse"
  ylab <- "Mean Squared Error"
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
  lty = c(1, 2, 1)
  col = c("black", "black", "grey")
  for (i in seq_along(sub_summary)) {
    with (sub_summary[[i]][linear == use_linear,], lines(
      c(0, scen_range),
      c(get(stats)[1], get(stats)),
      type = "S",
      lty = lty[i],
      col = col[i],
      asp = asp
    ))
  }
  axis(
    1,
    at = c(scen_range - 0.5),
    las = 2,
    labels = FALSE
  )
  segments(0, 0, 0.5, 0)
  segments(11.5, 0, 12, 0)
  text(x = c(scen_range - 0.5), 
       y = par("usr")[3] - adj_x_label,
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
  mtext("Scenario parameters",
        side = 1, 
        line = 6.25)
  axis(
    2,
    at = y_axis_tcks,
    labels = y_axis_labels,
    las = 1
  )
  mtext(ylab,
        side = 2,
        line = 4
  )
  di <- dev.size("in")
  x <- grconvertX(c(0, di[1]), from = "in", to = "user")
  y <- grconvertY(c(0, di[2]), from = "in", to = "user")
  fig <- par("fig")
  x <- x[1] + (x[2] - x[1]) * fig[1:2]
  y <- y[1] + (y[2] - y[1]) * fig[3:4]
  x <- x[1] + strwidth(txt)
  y <- y[2] - strheight(txt) * 1.5
  text(x, y, txt)
  if (legend == TRUE) {
    legend(
      legend = c("Random", "Stratified Random", "Extremes"),
      legend_where,
      col = col,
      lty = lty,
      bty = ifelse(box_legend == TRUE, "o", "n"),
      title = expression(underline(Sampling ~ Strategy))
    )
  }
}
make_canvas_mse <- function(use_method, 
                            legend,
                            legend_where = "bottomleft",
                            box_legend = TRUE) {
  make_plot_mse(
    use_linear = 1,
    use_method = use_method,
    use_size_valdata = 0.4,
    limits = c(0, 0.003),
    txt = "A)",
    asp = 12 / 0.003,
    legend = ifelse(legend == 1, TRUE, FALSE),
    legend_where = legend_where,
    box_legend = box_legend,
    y_axis_tcks = c(0, 0.0015, 0.003),
    y_axis_labels = c("0.0000",
                      "0.0015",
                      "0.0030"),
    adj_x_label = 0.003 * 0.06
  )
  make_plot_mse(
    use_linear = 0,
    use_method = use_method,
    use_size_valdata = 0.4,
    c(0, 0.003),
    txt = "B)",
    asp = 12 / 0.003,
    ifelse(legend == 2, TRUE, FALSE),
    legend_where = legend_where,
    box_legend = box_legend,
    y_axis_tcks = c(0, 0.0015, 0.003),
    y_axis_labels = c("0.0000",
                      "0.0015",
                      "0.0030"),
    adj_x_label = 0.003 * 0.06
  )
  make_plot_mse(
    use_linear = 1,
    use_method = use_method,
    use_size_valdata = 0.1,
    limits = c(0, 0.015),
    txt = "C)",
    asp = 12 / 0.015,
    ifelse(legend == 3, TRUE, FALSE),
    legend_where = legend_where,
    box_legend = box_legend,
    y_axis_tcks = c(0, 0.0075, 0.015),
    y_axis_labels = c("0.0000",
                      "0.0075",
                      "0.0150"),
    adj_x_label = 0.015 * 0.06
  )
  make_plot_mse(
    use_linear = 0,
    use_method = use_method,
    use_size_valdata = 0.1,
    limits = c(0, 0.015),
    txt = "D)",
    asp = 12 / 0.015,
    ifelse(legend == 4, TRUE, FALSE),
    legend_where = legend_where,
    box_legend = box_legend,
    y_axis_tcks = c(0, 0.0075, 0.015),
    y_axis_labels = c("0.0000",
                      "0.0075",
                      "0.0150"),
    adj_x_label = 0.015 * 0.06
  )
}
make_canvas_mse_appendix <- function(use_method,
                                     legend,
                                     legend_where = "bottomleft",
                                     box_legend = FALSE) {
  make_plot_mse(
    use_linear = 1,
    use_method = use_method,
    use_size_valdata = 0.25,
    limits = c(0, 0.005),
    txt = "A)",
    asp = 12 / 0.005,
    legend = ifelse(legend == 1, TRUE, FALSE),
    legend_where = legend_where,
    box_legend = box_legend,
    y_axis_tcks = c(0, 0.0025, 0.005),
    y_axis_labels = c("0.0000",
                      "0.0025",
                      "0.0050"),
    adj_x_label = 0.005 * 0.06
  )
  make_plot_mse(
    use_linear = 0,
    use_method = use_method,
    use_size_valdata = 0.25,
    c(0, 0.005),
    txt = "B)",
    asp = 12 / 0.005,
    legend = ifelse(legend == 2, TRUE, FALSE),
    legend_where = legend_where,
    box_legend = box_legend,
    y_axis_tcks = c(0, 0.0025, 0.005),
    y_axis_labels = c("0.0000",
                      "0.0025",
                      "0.0050"),
    adj_x_label = 0.005 * 0.06
  )
}

##############################
# 2 - Internal validation restricted
##############################
use_method <- "complete_case"
# linear 40%
pdf(paste0("./results/figures", "/mse_ivr_l40.pdf"),
    width = 2.8, height = 2.8, family = "Arial",
    pointsize = 8)
par(
  mar = c(7.5, 6, 2.5, 4),
  xpd = NA,
  family = "Arial",
  cex = 0.83
)
make_plot_mse(
  use_linear = 1,
  use_method = use_method,
  use_size_valdata = 0.4,
  limits = c(0, 0.003),
  txt = "A)",
  asp = 12 / 0.003,
  legend = TRUE,
  legend_where = "bottomleft",
  box_legend = TRUE,
  y_axis_tcks = c(0, 0.0015, 0.003),
  y_axis_labels = c("0.0000",
                    "0.0015",
                    "0.0030"),
  adj_x_label = 0.003 * 0.06
)
dev.off()
# non-linear 40%
pdf(paste0("./results/figures", "/mse_ivr_nl40.pdf"),
    width = 2.8, height = 2.8, family = "Arial",
    pointsize = 8)
par(
  mar = c(7.5, 6, 2.5, 4),
  xpd = NA,
  family = "Arial",
  cex = 0.83
)
make_plot_mse(
  use_linear = 0,
  use_method = use_method,
  use_size_valdata = 0.4,
  limits = c(0, 0.003),
  txt = "B)",
  asp = 12 / 0.003,
  legend = FALSE,
  y_axis_tcks = c(0, 0.0015, 0.003),
  y_axis_labels = c("0.0000",
                    "0.0015",
                    "0.0030"),
  adj_x_label = 0.003 * 0.06
)
dev.off()
# non-linear 40%
pdf(paste0("./results/figures", "/mse_ivr_nl40.pdf"),
    width = 2.8, height = 2.8, family = "Arial",
    pointsize = 8)
par(
  mar = c(7.5, 6, 2.5, 4),
  xpd = NA,
  family = "Arial",
  cex = 0.83
)
make_plot_mse(
  use_linear = 0,
  use_method = use_method,
  use_size_valdata = 0.4,
  limits = c(0, 0.003),
  txt = "B)",
  asp = 12 / 0.003,
  legend = FALSE,
  y_axis_tcks = c(0, 0.0015, 0.003),
  y_axis_labels = c("0.0000",
                    "0.0015",
                    "0.0030"),
  adj_x_label = 0.003 * 0.06
)
dev.off()
# linear 10%
pdf(paste0("./results/figures", "/mse_ivr_l10.pdf"),
    width = 2.8, height = 2.8, family = "Arial",
    pointsize = 8)
par(
  mar = c(7.5, 6, 2.5, 4),
  xpd = NA,
  family = "Arial",
  cex = 0.83
)
make_plot_mse(
  use_linear = 1,
  use_method = use_method,
  use_size_valdata = 0.1,
  limits = c(0, 0.015),
  txt = "C)",
  asp = 12 / 0.015,
  legend = FALSE,
  y_axis_tcks = c(0, 0.0075, 0.015),
  y_axis_labels = c("0.0000",
                    "0.0075",
                    "0.0150"),
  adj_x_label = 0.015 * 0.06
)
dev.off()
# non-linear 10%
pdf(paste0("./results/figures", "/mse_ivr_nl10.pdf"),
    width = 2.8, height = 2.8, family = "Arial",
    pointsize = 8)
par(
  mar = c(7.5, 6, 2.5, 4),
  xpd = NA,
  family = "Arial",
  cex = 0.83
)
make_plot_mse(
  use_linear = 0,
  use_method = use_method,
  use_size_valdata = 0.1,
  limits = c(0, 0.015),
  txt = "D)",
  asp = 12 / 0.015,
  legend = FALSE,
  y_axis_tcks = c(0, 0.0075, 0.015),
  y_axis_labels = c("0.0000",
                    "0.0075",
                    "0.0150"),
  adj_x_label = 0.015 * 0.06
)
dev.off()

# appendix
# linear 25%
pdf(paste0("./results/figures", "/mse_ivr_l25.pdf"),
    width = 2.8, height = 2.8, family = "Arial",
    pointsize = 8)
par(
  mar = c(7.5, 6, 2.5, 4),
  xpd = NA,
  family = "Arial",
  cex = 0.83
)
make_plot_mse(
  use_linear = 1,
  use_method = use_method,
  use_size_valdata = 0.25,
  limits = c(0, 0.005),
  txt = "A)",
  asp = 12 / 0.005,
  legend = TRUE,
  legend_where = "bottomleft",
  box_legend = TRUE,
  y_axis_tcks = c(0, 0.0025, 0.005),
  y_axis_labels = c("0.0000",
                    "0.0025",
                    "0.0050"),
  adj_x_label = 0.005 * 0.06
)
dev.off()
# non-linear 25%
pdf(paste0("./results/figures", "/mse_ivr_nl25.pdf"),
    width = 2.8, height = 2.8, family = "Arial",
    pointsize = 8)
par(
  mar = c(7.5, 6, 2.5, 4),
  xpd = NA,
  family = "Arial",
  cex = 0.83
)
make_plot_mse(
  use_linear = 0,
  use_method = use_method,
  use_size_valdata = 0.25,
  limits = c(0, 0.005),
  txt = "B)",
  asp = 12 / 0.005,
  legend = FALSE,
  y_axis_tcks = c(0, 0.0025, 0.005),
  y_axis_labels = c("0.0000",
                    "0.0025",
                    "0.0050"),
  adj_x_label = 0.005 * 0.06
)
dev.off()

##############################
# 3 - Validation regression calibration ----
##############################
use_method <- "inadm_reg_cal"
# linear 40%
pdf(paste0("./results/figures", "/mse_vrc_l40.pdf"),
    width = 2.8, height = 2.8, family = "Arial",
    pointsize = 8)
par(
  mar = c(7.5, 6, 2.5, 4),
  xpd = NA,
  family = "Arial",
  cex = 0.83
)
make_plot_mse(
  use_linear = 1,
  use_method = use_method,
  use_size_valdata = 0.4,
  limits = c(0, 0.003),
  txt = "A)",
  asp = 12 / 0.003,
  legend = TRUE,
  legend_where = "bottomleft",
  box_legend = TRUE,
  y_axis_tcks = c(0, 0.0015, 0.003),
  y_axis_labels = c("0.0000",
                    "0.0015",
                    "0.0030"),
  adj_x_label = 0.003 * 0.06
)
dev.off()
# non-linear 40%
pdf(paste0("./results/figures", "/mse_vrc_nl40.pdf"),
    width = 2.8, height = 2.8, family = "Arial",
    pointsize = 8)
par(
  mar = c(7.5, 6, 2.5, 4),
  xpd = NA,
  family = "Arial",
  cex = 0.83
)
make_plot_mse(
  use_linear = 0,
  use_method = use_method,
  use_size_valdata = 0.4,
  limits = c(0, 0.003),
  txt = "B)",
  asp = 12 / 0.003,
  legend = FALSE,
  y_axis_tcks = c(0, 0.0015, 0.003),
  y_axis_labels = c("0.0000",
                    "0.0015",
                    "0.0030"),
  adj_x_label = 0.003 * 0.06
)
dev.off()
# non-linear 40%
pdf(paste0("./results/figures", "/mse_vrc_nl40.pdf"),
    width = 2.8, height = 2.8, family = "Arial",
    pointsize = 8)
par(
  mar = c(7.5, 6, 2.5, 4),
  xpd = NA,
  family = "Arial",
  cex = 0.83
)
make_plot_mse(
  use_linear = 0,
  use_method = use_method,
  use_size_valdata = 0.4,
  limits = c(0, 0.003),
  txt = "B)",
  asp = 12 / 0.003,
  legend = FALSE,
  y_axis_tcks = c(0, 0.0015, 0.003),
  y_axis_labels = c("0.0000",
                    "0.0015",
                    "0.0030"),
  adj_x_label = 0.003 * 0.06
)
dev.off()
# linear 10%
pdf(paste0("./results/figures", "/mse_vrc_l10.pdf"),
    width = 2.8, height = 2.8, family = "Arial",
    pointsize = 8)
par(
  mar = c(7.5, 6, 2.5, 4),
  xpd = NA,
  family = "Arial",
  cex = 0.83
)
make_plot_mse(
  use_linear = 1,
  use_method = use_method,
  use_size_valdata = 0.1,
  limits = c(0, 0.015),
  txt = "C)",
  asp = 12 / 0.015,
  legend = FALSE,
  y_axis_tcks = c(0, 0.0075, 0.015),
  y_axis_labels = c("0.0000",
                    "0.0075",
                    "0.0150"),
  adj_x_label = 0.015 * 0.06
)
dev.off()
# non-linear 10%
pdf(paste0("./results/figures", "/mse_vrc_nl10.pdf"),
    width = 2.8, height = 2.8, family = "Arial",
    pointsize = 8)
par(
  mar = c(7.5, 6, 2.5, 4),
  xpd = NA,
  family = "Arial",
  cex = 0.83
)
make_plot_mse(
  use_linear = 0,
  use_method = use_method,
  use_size_valdata = 0.1,
  limits = c(0, 0.015),
  txt = "D)",
  asp = 12 / 0.015,
  legend = FALSE,
  y_axis_tcks = c(0, 0.0075, 0.015),
  y_axis_labels = c("0.0000",
                    "0.0075",
                    "0.0150"),
  adj_x_label = 0.015 * 0.06
)
dev.off()

# appendix
# linear 25%
pdf(paste0("./results/figures", "/mse_vrc_l25.pdf"),
    width = 2.8, height = 2.8, family = "Arial",
    pointsize = 8)
par(
  mar = c(7.5, 6, 2.5, 4),
  xpd = NA,
  family = "Arial",
  cex = 0.83
)
make_plot_mse(
  use_linear = 1,
  use_method = use_method,
  use_size_valdata = 0.25,
  limits = c(0, 0.005),
  txt = "A)",
  asp = 12 / 0.005,
  legend = TRUE,
  legend_where = "topleft",
  box_legend = FALSE,
  y_axis_tcks = c(0, 0.0025, 0.005),
  y_axis_labels = c("0.0000",
                    "0.0025",
                    "0.0050"),
  adj_x_label = 0.005 * 0.06
)
dev.off()
# non-linear 25%
pdf(paste0("./results/figures", "/mse_vrc_nl25.pdf"),
    width = 2.8, height = 2.8, family = "Arial",
    pointsize = 8)
par(
  mar = c(7.5, 6, 2.5, 4),
  xpd = NA,
  family = "Arial",
  cex = 0.83
)
make_plot_mse(
  use_linear = 0,
  use_method = use_method,
  use_size_valdata = 0.25,
  limits = c(0, 0.005),
  txt = "B)",
  asp = 12 / 0.005,
  legend = FALSE,
  y_axis_tcks = c(0, 0.0025, 0.005),
  y_axis_labels = c("0.0000",
                    "0.0025",
                    "0.0050"),
  adj_x_label = 0.005 * 0.06
)
dev.off()

##############################
# 4 - Efficient regression calibration ----
##############################
# appendix // 40% and 10% 
use_method = "efficient_reg_cal"
# linear 40%
pdf(paste0("./results/figures", "/mse_erc_l40.pdf"),
    width = 2.8, height = 2.8, family = "Arial",
    pointsize = 8)
par(
  mar = c(7.5, 6, 2.5, 4),
  xpd = NA,
  family = "Arial",
  cex = 0.83
)
make_plot_mse(
  use_linear = 1,
  use_method = use_method,
  use_size_valdata = 0.4,
  limits = c(0, 0.003),
  txt = "A)",
  asp = 12 / 0.003,
  legend = TRUE,
  legend_where = "bottomleft",
  box_legend = TRUE,
  y_axis_tcks = c(0, 0.0015, 0.003),
  y_axis_labels = c("0.0000",
                    "0.0015",
                    "0.0030"),
  adj_x_label = 0.003 * 0.06
)
dev.off()
# non-linear 40%
pdf(paste0("./results/figures", "/mse_erc_nl40.pdf"),
    width = 2.8, height = 2.8, family = "Arial",
    pointsize = 8)
par(
  mar = c(7.5, 6, 2.5, 4),
  xpd = NA,
  family = "Arial",
  cex = 0.83
)
make_plot_mse(
  use_linear = 0,
  use_method = use_method,
  use_size_valdata = 0.4,
  limits = c(0, 0.003),
  txt = "B)",
  asp = 12 / 0.003,
  legend = FALSE,
  y_axis_tcks = c(0, 0.0015, 0.003),
  y_axis_labels = c("0.0000",
                    "0.0015",
                    "0.0030"),
  adj_x_label = 0.003 * 0.06
)
dev.off()
# non-linear 40%
pdf(paste0("./results/figures", "/mse_erc_nl40.pdf"),
    width = 2.8, height = 2.8, family = "Arial",
    pointsize = 8)
par(
  mar = c(7.5, 6, 2.5, 4),
  xpd = NA,
  family = "Arial",
  cex = 0.83
)
make_plot_mse(
  use_linear = 0,
  use_method = use_method,
  use_size_valdata = 0.4,
  limits = c(0, 0.003),
  txt = "B)",
  asp = 12 / 0.003,
  legend = FALSE,
  y_axis_tcks = c(0, 0.0015, 0.003),
  y_axis_labels = c("0.0000",
                    "0.0015",
                    "0.0030"),
  adj_x_label = 0.003 * 0.06
)
dev.off()
# linear 10%
pdf(paste0("./results/figures", "/mse_erc_l10.pdf"),
    width = 2.8, height = 2.8, family = "Arial",
    pointsize = 8)
par(
  mar = c(7.5, 6, 2.5, 4),
  xpd = NA,
  family = "Arial",
  cex = 0.83
)
make_plot_mse(
  use_linear = 1,
  use_method = use_method,
  use_size_valdata = 0.1,
  limits = c(0, 0.015),
  txt = "C)",
  asp = 12 / 0.015,
  legend = FALSE,
  y_axis_tcks = c(0, 0.0075, 0.015),
  y_axis_labels = c("0.0000",
                    "0.0075",
                    "0.0150"),
  adj_x_label = 0.015 * 0.06
)
dev.off()
# non-linear 10%
pdf(paste0("./results/figures", "/mse_erc_nl10.pdf"),
    width = 2.8, height = 2.8, family = "Arial",
    pointsize = 8)
par(
  mar = c(7.5, 6, 2.5, 4),
  xpd = NA,
  family = "Arial",
  cex = 0.83
)
make_plot_mse(
  use_linear = 0,
  use_method = use_method,
  use_size_valdata = 0.1,
  limits = c(0, 0.015),
  txt = "D)",
  asp = 12 / 0.015,
  legend = FALSE,
  y_axis_tcks = c(0, 0.0075, 0.015),
  y_axis_labels = c("0.0000",
                    "0.0075",
                    "0.0150"),
  adj_x_label = 0.015 * 0.06
)
dev.off()

# appendix
# linear 25%
pdf(paste0("./results/figures", "/mse_erc_l25.pdf"),
    width = 2.8, height = 2.8, family = "Arial",
    pointsize = 8)
par(
  mar = c(7.5, 6, 2.5, 4),
  xpd = NA,
  family = "Arial",
  cex = 0.83
)
make_plot_mse(
  use_linear = 1,
  use_method = use_method,
  use_size_valdata = 0.25,
  limits = c(0, 0.005),
  txt = "A)",
  asp = 12 / 0.005,
  legend = TRUE,
  legend_where = "topleft",
  box_legend = FALSE,
  y_axis_tcks = c(0, 0.0025, 0.005),
  y_axis_labels = c("0.0000",
                    "0.0025",
                    "0.0050"),
  adj_x_label = 0.005 * 0.06
)
dev.off()
# non-linear 25%
pdf(paste0("./results/figures", "/mse_erc_nl25.pdf"),
    width = 2.8, height = 2.8, family = "Arial",
    pointsize = 8)
par(
  mar = c(7.5, 6, 2.5, 4),
  xpd = NA,
  family = "Arial",
  cex = 0.83
)
make_plot_mse(
  use_linear = 0,
  use_method = use_method,
  use_size_valdata = 0.25,
  limits = c(0, 0.005),
  txt = "B)",
  asp = 12 / 0.005,
  legend = FALSE,
  y_axis_tcks = c(0, 0.0025, 0.005),
  y_axis_labels = c("0.0000",
                    "0.0025",
                    "0.0050"),
  adj_x_label = 0.005 * 0.06
)
dev.off()
