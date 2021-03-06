% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_preparation.R
\name{budget_rename}
\alias{budget_rename}
\title{Rename budget columns}
\usage{
budget_rename(df_rename)
}
\arguments{
\item{df_rename}{Processed Tyler Budget Extract report preparing for visualization}
}
\value{
Dataframe with renamed columns for visualization in table or graphs
}
\description{
Renames budget columns in preparation for visualization in tables or visualizations, including applying year_transform and tpbudget::df_rename_yeartoyear functions
}
