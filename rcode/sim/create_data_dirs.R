#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## Create directories in which sim output will be stored
## lindanab4@gmail.com - 20200305
#############################################################

##############################
# 0 - Load librairies ----
##############################

##############################
# 1 - Helper functions ----
##############################
# create directory if it doesn't exist
create_dir <- function(name){
  if(!dir.exists(name)){
    dir.create(name)
  }
}
# format path of directory of levels
format_dir_path <- function(data_dir, prefix_dir, dir_name){
  char <- as.vector(sapply(data_dir, 
                           function(x) paste0(x, prefix_dir, "_", dir_name),
                           USE.NAMES = F))
}
##############################
# 2 - Create directories in data_dir directory (fe: ./data/output)
##############################
create_data_dirs <- function(data_dir = "./data/output/", levels){
  dir_paths <- list_data_dirs(data_dir, levels)
  invisible(sapply(unlist(dir_paths), create_dir))
}
list_data_dirs <- function(data_dir = "./data/output/", levels){
  dir_paths <- vector("list", length(levels))
  for (i in 1:length(levels)){
    dir_paths[[i]] <- format_dir_path(
      data_dir = data_dir,
      prefix_dir = names(levels)[i],
      dir_name = levels[[i]]
    )
    data_dir <- paste0(dir_paths[[i]], "/")
  }
  dir_paths
}

# count_number_of_dirs <- function(levels){
#   i <- 2
#   n_dirs <- lengths(levels)[1]
#   mult <- n_dirs
#   while(i <= length(levels)){
#     n_dirs <- n_dirs + mult * lengths(levels)[i]
#     mult <- mult * lengths(levels)[i]
#     i <- i + 1
#   }
#   n_dirs
# }