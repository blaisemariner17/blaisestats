% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_summary.R
\name{data_summary}
\alias{data_summary}
\title{Return mean, standard dev, and standard error of a data set with grouping of the variable of interest}
\usage{
data_summary(in_data, varname, groupnames)
}
\arguments{
\item{infile}{Path to the input file}
}
\value{
A matrix of the infile
}
\description{
This function takes in a dataframe in long format to calculate the mean,
standard dev, and standard error of the data based on the labeling of the
groups in the data frame. Many biological applications have multiple
genotypes and drug treatments within a single data frame, this function
is albe to handle these different dependent group variable to produce
their associated basic statistics.
}
