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
                          y_axis_labels) {
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
    )
  )
  segments(0, 0, 0.5, 0)
  segments(11.5, 0, 12, 0)
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
      legend = c("Random", "Str. Random", "Extremes"),
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
                      "0.0030")
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
                      "0.0030")
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
                      "0.0150")
  )
  make_plot_mse(
    use_linear = 0,
    use_method = use_method,
    use_size_valdata = 0.1,
    limits = c(0, 0.015),
    txt = "D)",
    asp = 12 / 0.015,
    ifelse(legend == 1, TRUE, FALSE),
    legend_where = legend_where,
    box_legend = box_legend,
    y_axis_tcks = c(0, 0.0075, 0.015),
    y_axis_labels = c("0.0000",
                      "0.0075",
                      "0.0150")
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
    limits = c(0, 0.004),
    txt = "A)",
    asp = 12 / 0.004,
    legend = ifelse(legend == 1, TRUE, FALSE),
    legend_where = legend_where,
    box_legend = box_legend,
    y_axis_tcks = c(0, 0.002, 0.004),
    y_axis_labels = c("0.000",
                      "0.002",
                      "0.004")
  )
  make_plot_mse(
    use_linear = 0,
    use_method = use_method,
    use_size_valdata = 0.25,
    c(0, 0.004),
    txt = "B)",
    asp = 12 / 0.004,
    legend = ifelse(legend == 2, TRUE, FALSE),
    legend_where = legend_where,
    box_legend = box_legend,
    y_axis_tcks = c(0, 0.002, 0.004),
    y_axis_labels = c("0.000",
                      "0.002",
                      "0.004")
  )
}

##############################
# 2 - Internal validation restricted
##############################
use_method <- "complete_case"
pdf(paste0("./results/figures", "/mse_ivr.pdf"),
    width = 5, height = 5, family = "Arial",
    pointsize = 8)
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
par(
  mar = c(8, 6, 2.5, 4.5),
  xpd = NA,
  family = "Arial"
)
make_canvas_mse(use_method, legend = 2)
dev.off()
# appendix
pdf(paste0("./results/figures", "/app_mse_ivr_25.pdf"),
    width = 5, height = 5, family = "Arial",
    pointsize = 8)
layout(matrix(c(1,2,0,0), 2, 2, byrow = TRUE))
par(
  mar = c(8, 6, 2.5, 4.5),
  xpd = NA,
  family = "Arial"
)
make_canvas_mse_appendix(use_method, legend = 2, legend_where = "bottomleft")
dev.off()

##############################
# 3 - Validation regression calibration ----
##############################
use_method <- "inadm_reg_cal"
pdf(paste0("./results/figures", "/mse_vrc.pdf"),
    width = 5, height = 5, family = "Arial",
    pointsize = 8)
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
par(
  mar = c(8, 6, 2.5, 4.5),
  xpd = NA,
  family = "Arial"
)
make_canvas_mse(use_method, legend = 2)
dev.off()
# appendix
pdf(paste0("./results/figures", "/app_mse_vrc_25.pdf"),
    width = 5, height = 5, family = "Arial",
    pointsize = 8)
layout(matrix(c(1,2,0,0), 2, 2, byrow = TRUE))
par(
  mar = c(8, 6, 2.5, 4.5),
  xpd = NA,
  family = "Arial"
)
make_canvas_mse_appendix(use_method, legend = 2, legend_where = "bottomleft")
dev.off()

##############################
# 4 - Efficient regression calibration ----
##############################
# appendix // 40% and 10% 
use_method = "efficient_reg_cal"
pdf(paste0("./results/figures", "/app_mse_erc_4010.pdf"),
    width = 5, height = 5, family = "Arial",
    pointsize = 8)
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
par(
  mar = c(8, 6, 2.5, 4.5),
  xpd = NA,
  family = "Arial"
)
make_canvas_mse(use_method, legend = 2, box_legend = TRUE)
dev.off()
# appendix // 25%
pdf(paste0("./results/figures", "/app_mse_erc_25.pdf"),
    width = 5, height = 5, family = "Arial",
    pointsize = 8)
layout(matrix(c(1,2,0,0), 2, 2, byrow = TRUE))
par(
  mar = c(8, 6, 2.5, 4.5),
  xpd = NA,
  family = "Arial"
)
make_canvas_mse_appendix(use_method, legend = 2, legend_where = "bottomleft")
dev.off()
