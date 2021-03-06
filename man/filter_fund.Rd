% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fund_prep.R
\name{filter_fund}
\alias{filter_fund}
\title{Filter fund}
\usage{
filter_fund(df, fundac, fundaccol = fund_acronym)
}
\arguments{
\item{df}{Dataframe}

\item{fundac}{Acronym of fund filtering dataframe to}

\item{fundaccol}{Data-masked name of column filtering containing fund acronyms; default fund_acronym}
}
\value{
Dataframe filtered to one fund
}
\description{
Filters dataframe to given fund
}
