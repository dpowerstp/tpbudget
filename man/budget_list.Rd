% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_prep.R
\name{budget_list}
\alias{budget_list}
\title{Create List of Grouped Datasets
Groups base-processed Tyler Expenditures or Revenue dataset by different combinations of columns of interest, summing year columns by groups and calculating changes for a given fiscal-year. Returns list of grouped-dataframes}
\usage{
budget_list(df, curr_fy = 2022, .missingcols = F)
}
\arguments{
\item{df}{Tyler Budget Extract report expenditures or revenue dataset}

\item{curr_fy}{Current fiscal year}

\item{.missingcols}{Whether to print names of grouped-columns that are not present in the dataframe}
}
\value{
List of datasets, grouped by different combinations of group columns
}
\description{
Create List of Grouped Datasets
Groups base-processed Tyler Expenditures or Revenue dataset by different combinations of columns of interest, summing year columns by groups and calculating changes for a given fiscal-year. Returns list of grouped-dataframes
}
