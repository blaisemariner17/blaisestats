#' Return mean, standard dev, and standard error of a data set with grouping of the variable of interest
#'
#' This function takes in a dataframe in long format to calculate the mean,
#' standard dev, and standard error of the data based on the labeling of the
#' groups in the data frame. Many biological applications have multiple
#' genotypes and drug treatments within a single data frame, this function
#' is albe to handle these different dependent group variable to produce
#' their associated basic statistics.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
data_summary <- function(in_data, varname, groupnames){
  summary_func <- function(x, column_var){
    c(mean = mean(x[[column_var]], na.rm=TRUE),
      sd = sd(x[[column_var]], na.rm=TRUE),
      se = sd(x[[column_var]], na.rm=TRUE) / sqrt(length(x[[column_var]]))
    )
  }
  data_sum<-plyr::ddply(in_data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- plyr::rename(data_sum, c("mean" = varname))
  return(data_sum)
}
