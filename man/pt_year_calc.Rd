% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pivot_tables.R
\name{pt_year_calc}
\alias{pt_year_calc}
\title{Add year columns to pivot tables}
\usage{
pt_year_calc(pt, colname)
}
\arguments{
\item{pt}{Pivot table to add column to, created by pivottabler::PivotTable$new()}

\item{colname}{String name of column to add year column to}
}
\value{
Pivot table with year column added to it
}
\description{
Adds year columns to a pivot table, summing year columns and renaming the year column for representation in the table
}
