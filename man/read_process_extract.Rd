% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_prep.R
\name{read_process_extract}
\alias{read_process_extract}
\title{Read in and Process Tyler Report Extract
Reads in unprocessed Tyler Budget Extract Report Custom file, and performs base-processing on the file to pull expenditure data or revenue data.}
\usage{
read_process_extract(
  filepath = "./data/Budget Extract Report Custom.xlsx",
  exp = F
)
}
\arguments{
\item{filepath}{Filepath to unprocessed Tyler Budget Extract Report Custom file}

\item{exp}{Exp indicates whether want to return revenue dataset (F) or expenditure dataset (T; default). Unprocessed Tyler Budget Extract Report Custom is structured as a revenue dataset on top of an expenditure dataset}
}
\value{
Base-processed Tyler Budget Extract report custom file with standard column names
}
\description{
Read in and Process Tyler Report Extract
Reads in unprocessed Tyler Budget Extract Report Custom file, and performs base-processing on the file to pull expenditure data or revenue data.
}
