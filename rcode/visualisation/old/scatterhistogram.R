#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction 
## Simulation study
##
## Scatter histogram
## lindanab4@gmail.com - 202005012
#############################################################

##############################
# 0 - Load librairies
##############################

############################## 
# 1 - Workhorse scatterhistogram
##############################
create_scatterhist <- function(data, 
                               uniform = F,
                               bins){
  zones <- matrix(c(2, 0, 1, 3), ncol = 2, byrow = TRUE)
  layout(zones, widths = c(4/5, 1/5), heights = c(1/5, 4/5))
  par(mar = c(3, 3, 1, 1))
  plot(0,
       type = "n",
       xaxt = "n",
       yaxt = "n",
       frame.plot = F,
       ann = F,
       xlim = c(-1.5, 5.5),
       ylim = c(-3, 3.5)
  )
  axis(1,
       at = c(-1.5, 2, 5.5),
       tck = - 0.01,
       cex.axis = 0.75
  )
  mtext("Visceral adipose tissue",
        side = 1,
        line = 2,
        cex = 1
  )
  axis(2, 
       at = c(-3, 0.25, 3.5),
       tck = - 0.01,
       cex.axis = 0.75
  )
  mtext("Waist circumference", 
        side = 2, 
        line = 2)
  abline(h = c(bins$lower_bound, bins$upper_bound[10]),
         col = "lightgrey")
  with (data[data$in_valdata == 0, ], 
        points(VAT, WC, 
               col = "lightgrey", 
               pch = 16))
  with (data[data$in_valdata == 1, ], 
        points(VAT, WC,
               pch = 16))
  xhist <- with (data[data$in_valdata == 1, ],
                 hist(VAT, plot = FALSE))
  yhist <- with (data[data$in_valdata == 1, ],
                 hist(WC, 
                      breaks = c(bins$lower_bound, bins$upper_bound[10]),
                      plot = FALSE))
  top = max(c(xhist$counts, yhist$counts))
  par(mar = c(0,3,1,1))
  barplot(xhist$counts, axes = FALSE, ylim = c(0, top), space = 0)
  par(mar=c(3,0,1,1))
  if (uniform == T){
    barplot(bins$n_valdata_bin, 
            axes = FALSE, 
            xlim = c(0, top),
            space = 0, horiz = T)
  } else
    barplot(yhist$count,
            axes = FALSE, 
            xlim = c(0, top),
            space = 0, horiz = T)
}