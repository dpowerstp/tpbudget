% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_prep.R
\name{pull_year_cols}
\alias{pull_year_cols}
\title{Pull Year Column Names
Pulls names of year columns from minimally or non-minimally Tyler Extract Report custom dataset, with year column names following standard format of ####_#### or diff(year number).}
\usage{
pull_year_cols(df, nochng = NULL)
}
\arguments{
\item{df}{Minimally-processed Tyler Extract report custom dataset. Year columns in dataset follow a standard naming convention of ####_#### or diff(year number).}

\item{nochng}{Whether to include columns measuring changes from one year to another or differences between years. If null, returns all columns; if true, omits change columns; if false, returns just change columns}
}
\value{
Character vector of year columns in dataset
}
\description{
Pull Year Column Names
Pulls names of year columns from minimally or non-minimally Tyler Extract Report custom dataset, with year column names following standard format of ####_#### or diff(year number).
}
