#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## 1 - Helper functions
## lindanab4@gmail.com - 20200221
#############################################################

##############################
# 0 - Load librairies + source code 
##############################

############################## 
# 1 - Helper Functions ----
##############################
# creates a data.frame that is used to sample uniformly. Data is divided in
# n_bins bins with equal distance between lower and upper bound use_variable
# in that bin. It counts the number of subjects in the data (n_data) included
# in that bin and the number of subjects that will be selected in val_data
# between the bounds of that bin.
create_bins <- function(n_bins, n_each_bin, data, use_variable){
  min_var <- min(data[use_variable])
  max_var <- max(data[use_variable])
  # create bounds of the bins using min and max
  bounds_bins <- seq(from = min_var, to = max_var, 
                     length.out = ( n_bins + 1 ))
  bounds_bins <- cbind(c(NA, bounds_bins), 
                       c(bounds_bins, NA))[2:( n_bins + 1 ),]
  # create data.frame with the bounds of a bin and number of subjects in data 
  # in that bin and the number of subjects that will be included in the 
  # validation sub sample from that bind (using the lower and upper bound)
  bins <- data.frame("lower_bound" = bounds_bins[,1],
                     "upper_bound" = bounds_bins[,2], 
                     "n_data" = rep(NA_integer_, n_bins),
                     "n_valdata" = rep(NA_integer_, n_bins))
  # count the number of subjects in one bin in the data
  bins["n_data"] <- apply(bounds_bins, 1, 
                          FUN = count_n_data, 
                          data = data,
                          use_variable = use_variable)
  # fill column n_valdata in bins containing the number of subjects that will
  # be sampled between the bounds of that bin
  bins <- fill_n_valdata(bins, n_each_bin, n_valdata)
  bins
}
# counts the number of subjects in the data between the bounds of that bin 
# (n_data)
count_n_data <- function(bounds_bin, data, use_variable){
  condition <- data[use_variable] < bounds_bin[2] & 
    data[use_variable] >= bounds_bin[1] 
  NROW(data[condition,])
}
# counts the number of subjects that should be sampled in the validation data 
# between the bounds of the bin (n_valdata) and fills data.frame 'bins' with 
# that number.
fill_n_valdata <- function(bins, n_each_bin, n_valdata){
  # n_valdata is equal to n_data if n_data is smaller than the required size
  # of each bin (too few subjects between those bounds)
  # if n_data is equal or greater than the required size of each bin, then
  # the n_valdata is equal to n_data
  while (any(is.na(bins["n_valdata"]))){ # stop if n_valdata is filled completely
    n_data_too_small <- bins["n_data"] < n_each_bin
    n_valdata_empty <- is.na(bins["n_valdata"])
    if(any(rows <- n_data_too_small & n_valdata_empty)){
      bins[rows, "n_valdata"] <- bins[rows, "n_data"]
    }
    else {
      bins[n_valdata_empty, "n_valdata"] <- size_bins 
    }
    n_each_bin <- update_n_each_bin(n_each_bin, bins, n_valdata)
  }
  bins
}
# after filling n_valdata the bins containing less subjects than the required
# size of each bin (i.e., n_each_bin), n_each_bin is updated (increased) so that 
# in the end, the total number of subjects in the validation data equals its 
# desired size
update_n_each_bin <- function(n_each_bin, bins, n_valdata){
  n_valdata_now <- n_valdata - sum(bins["n_valdata"], na.rm = T)
  n_bins_not_filled <- length(which (is.na(bins["n_valdata"])))
  round(n_valdata_now / n_bins_not_filled)
}
# selects the row numbers of the subjects that will be included in the 
# validation sample sampled at random
select_subjects_uniform <- function(bin, data, use_variable){
  in_bin <- data[use_variable] >= bin[["lower_bound"]] & 
    data[use_variable] < bin[["upper_bound"]]
  n_valdata_bin <- bin[["n_valdata"]]
  sample(which (in_bin), n_valdata_bin)
}

############################## 
# 2 - Work horse that adds column 'in_valdata' to data that indicates if a 
# subject is included in the validation data or not.
##############################
select_valdata <- function(data, 
                           use_variable = NA,
                           size_valdata,
                           sampling_strat){
  # total number of subjects in data
  n <- NROW(data)
  # desired  number of subjects in valdata
  n_valdata <- ceiling(n * size_valdata)
  if (sampling_strat == "Random"){
    data$in_valdata <- sample(c(rep(0, n - n_valdata), rep(1, n_valdata)),
                              size = n, replace = FALSE)
  }
  if (sampling_strat == "Uniform" & !is.na(use_variable)){
    # to samply uniformly, data is dividided in a number of bins, with equal
    # distance between 'use_variable' within these bins
    n_bins <- 10
    n_each_bin <- round(n_valdata / n_bins)
    bins <- create_bins(n_bins, n_each_bin, data, use_variable)
    
    # indicate whether subject is included in validation sample (1) or not (0)
    data$in_valdata <- rep(0, n)
    # get rownumbers of subjects that are included in validation sample
    rownumbers <- unlist(
      apply(bins, 1, 
            FUN = select_subjects_uniform, 
            data = data, 
            use_variable = use_variable)
      )
    # change those subjects 0 to 1
    data$in_valdata[rownumbers] <- 1
  }
  if (sampling_strat == "Extremes" & !is.na(use_variable)){
    # order the observations and select the subjects in the extremes
    rownumbers <- c(order(data[use_variable])[1:(n_valdata/2)],
                    order(data[use_variable])[(n - n_valdata / 2 + 1):n])
    data$valdata_present <- rep(0, n)
    data[rownumbers,]$valdata_present <- 1
  }
  return(data)
}
