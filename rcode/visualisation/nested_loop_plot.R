#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## Create nested loop plot
## lindanab4@gmail.com - 20200421
#############################################################

##############################
# 0 - Load librairies ----
##############################
source(file =  "./rcode/dgm/sim_scen.R")
library(data.table)

##############################
# 1 - Helper functions ----
##############################
# Parameters used to loop through different scenarios
# variable $name is used in the nested loop plot to indicate the levels
get_by <- function() {
  by <- list(
    "levels" = list("linear" = unique(datagen_scenarios()$linear),
                    "R_squared" = unique(datagen_scenarios()$R_squared)[-c(1,6)],
                    "skewness" = unique(datagen_scenarios()$skewness)[-2]),
    "names" = list("linear" = "Linear: yes, no",
                   "R_squared" = "R-squared: 0.2, 0.4, 0.6, 0.8",
                   "skewness" = "Skewness: 0.1, 1.5, 3.0")
  )
  by
}
# limits is fe c(-100, 100) if stats used is percentage bias
get_placement <- function(by, limits, digits = 2) {
  delta <- diff(limits) / 10
  placement <- vector(mode = "list", length = length(by$levels))
  for (i in seq_along(placement)) {
    if (i == 1) {
      placement[[i]] <- c(
        round(limits[2], digits = digits) + delta,
        round(limits[2], digits = digits) + (7 / 4) * delta,
        round(limits[2], digits = digits) + 2 * delta
      )
    }
    else {
      placement[[i]] <- c(placement[[i - 1]][2] + delta,
                          placement[[i - 1]][2] + (7 / 4) * delta,
                          placement[[i - 1]][2] + 2 * delta)
    }
  }
  placement
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
# creates a list of summaries, for use_methods
# using the selected use_size_valdata and use_sampling strat
select_summary_method <- function(summary, 
                                  use_size_valdata, 
                                  use_sampling_strat,
                                  use_methods){
  summary <- subset(
    summary,
    size_valdata == use_size_valdata &
      sampling_strat == use_sampling_strat &
      (R_squared == 0.2 | R_squared == 0.4 | R_squared == 0.6 | R_squared == 0.8) &
      (skewness == 0.1 | skewness == 1.5 | skewness == 3)
  )
  select_method <- function(summary, use_method){
    subset <- subset(summary,
                     method == use_method)
  }
  out <- lapply(as.list(use_methods),
                FUN = select_method,
                summary = summary)
  names(out) <- use_methods
  out
}
# Function that creates a nested loop plot, using the defined limits, eg
# c(-100, 100) if stats is percentage bias
create_nlp <- function(summary, 
                       stats, 
                       ref,
                       limits, 
                       xlab, 
                       ylab,
                       use_size_valdata,
                       use_method, 
                       use_sampling_strats,
                       legend = T, 
                       y_axis_at = c(limits[1], 0, limits[2], placement[[length(placement)]][3]),
                       y_axis_labels = F) {
  sub_summary <- select_summary(summary, 
                                use_size_valdata = use_size_valdata, 
                                use_method = use_method,
                                use_sampling_strats = use_sampling_strats)
  placement <- get_placement(get_by(), limits = limits)
  n_scen <- NROW(expand.grid(get_by()$levels))
  scen_range <- 1:n_scen
  par(mgp = c(0, 0.25, 0), mar = c(3.5, 3.5, 1, 1))
  plot(0,
    type = "n",
    xaxt = "n",
    yaxt = "n",
    frame.plot = F,
    ann = F,
    xlim = c(0, n_scen),
    ylim = c(limits[1], placement[[length(placement)]][3])
  )
  axis(1,
    at = c(0, n_scen / 4, 2 * n_scen / 4, 3 * n_scen / 4, n_scen),
    tck = - 0.01,
    cex.axis = 0.75
  )
  mtext(xlab,
    side = 1,
    line = 1.5,
    cex = 1
  )
  axis(2, 
    at = c(y_axis_at, placement[[length(placement)]][3]),
    labels = y_axis_labels,
    tck = - 0.01,
    cex.axis = 0.75
  )
  mtext(ylab, 
        side = 2, 
        line = 1.5)
  segments(0, ref, 24, ref, col = "grey")
  if (legend == T){
    legend("bottomright",
           legend = use_sampling_strats,
           lty = 1:length(use_sampling_strats),
           bty = "n",
           horiz = F,
           cex = 0.5,
           x.intersp = 0.25,
           seg.len = 2)
    }
  for (i in seq_along(sub_summary)) {
    with (sub_summary[[i]], lines(
      c(0, scen_range),
      c(get(stats)[1], get(stats)),
      type = "S",
      lty = i
    ))
  }
  get_levels <- function(){
    n_levels <- length(get_by()$levels)
    levels_length <- lengths(get_by()$levels)
    levels <- vector(mode = "list", length = length(get_by()$levels))
    names(levels) <- names(get_by()$levels)
    j <- 1
    for (i in seq_along(levels)) {
      levels[[i]]$x <- seq(from = 0,
                           to = n_scen,
                           length.out = levels_length[i] * j + 1)
      levels[[i]]$y <- c(rep(seq(from = placement[[n_levels - i + 1]][1],
                            to = placement[[n_levels - i + 1]][2],
                            length.out = levels_length[i]), j),
                         placement[[n_levels - i + 1]][2])
      j <- j * levels_length[i]
    }
    levels
  }
  for (i in seq_along(get_levels())){ # plot steps of levels
    lines(get_levels()[[i]][[1]], get_levels()[[i]][[2]], 
          type = "s", 
          col = "grey")
  }
  for (i in seq_along(get_by()$names)){ # names of step levels
    text(0,
         placement[[length(get_by()$names) + 1 - i]][3], 
         get_by()$names[i], 
         adj = c(0, 0), 
         cex = 0.5)
  }
}
create_nlp_method <- function(summary, 
                              stats, 
                              ref,
                              limits, 
                              xlab, 
                              ylab,
                              use_size_valdata,
                              use_sampling_strat,
                              use_methods,
                              legend = T,
                              y_axis_at = c(limits[1], 0, limits[2], placement[[length(placement)]][3]),
                              y_axis_labels = F,
                              legend_text = c("Internal validation sample restricted",
                                              "Regression calibration (RC)",
                                              "Efficient RC",
                                              "Inadmissible RC"),
                              lty = 1:length(use_methods),
                              digits_plcmnt = 2) {
  sub_summary <- select_summary_method(
    summary,
    use_size_valdata = use_size_valdata,
    use_sampling_strat = use_sampling_strat,
    use_method = use_methods
  )
  placement <- get_placement(get_by(), limits = limits, digits = digits_plcmnt)
  n_scen <- NROW(expand.grid(get_by()$levels))
  scen_range <- 1:n_scen
  par(mgp = c(0, 0.25, 0), mar = c(4, 3, 1, 0.5))
  plot(0,
       type = "n",
       xaxt = "n",
       yaxt = "n",
       frame.plot = F,
       ann = F,
       xlim = c(0, n_scen),
       ylim = c(limits[1], placement[[length(placement)]][3])
  )
  axis(1,
       at = c(0, n_scen / 4, 2 * n_scen / 4, 3 * n_scen / 4, n_scen),
       tck = - 0.01,
       cex.axis = 0.75
  )
  mtext(xlab,
        side = 1,
        line = 1.5,
        cex = 1
  )
  axis(2, 
       at = c(y_axis_at, placement[[length(placement)]][3]),
       labels = y_axis_labels,
       tck = - 0.01,
       cex.axis = 0.75
  )
  mtext(ylab, 
        side = 2, 
        line = 1.5,
        cex = 1
  )
  segments(0, ref, 24, ref, col = "grey")
  if (legend == T){
    legend("bottomleft",
           legend = legend_text,
           lty = lty,
           bty = "n",
           horiz = F,
           ncol = 2,
           cex = 0.5,
           x.intersp = 0.25,
           seg.len = 2)
  }
  for (i in seq_along(sub_summary)) {
    with (sub_summary[[i]], lines(
      c(0, scen_range),
      c(get(stats)[1], get(stats)),
      type = "S",
      lty = lty[i]
    ))
  }
  get_levels <- function(){
    n_levels <- length(get_by()$levels)
    levels_length <- lengths(get_by()$levels)
    levels <- vector(mode = "list", length = length(get_by()$levels))
    names(levels) <- names(get_by()$levels)
    j <- 1
    for (i in seq_along(levels)) {
      levels[[i]]$x <- seq(from = 0,
                           to = n_scen,
                           length.out = levels_length[i] * j + 1)
      levels[[i]]$y <- c(rep(seq(from = placement[[n_levels - i + 1]][1],
                                 to = placement[[n_levels - i + 1]][2],
                                 length.out = levels_length[i]), j),
                         placement[[n_levels - i + 1]][2])
      j <- j * levels_length[i]
    }
    levels
  }
  for (i in seq_along(get_levels())){ # plot steps of levels
    lines(get_levels()[[i]][[1]], get_levels()[[i]][[2]], 
          type = "s", 
          col = "grey")
  }
  for (i in seq_along(get_by()$names)){ # names of step levels
    text(0,
         placement[[length(get_by()$names) + 1 - i]][3], 
         get_by()$names[i], 
         adj = c(0, 0), 
         cex = 0.5)
  }
}
