% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flex_tables.R
\name{pt_render_div}
\alias{pt_render_div}
\title{Render Pivot Division Expenditures Overall}
\usage{
pt_render_div(divcode, bud_dept_list, fy = 2023, deptadjust = F)
}
\arguments{
\item{divcode}{Division code}

\item{bud_dept_list}{List of grouped budget expenditures}

\item{fy}{Fiscal year for this budget book; default 2023}

\item{deptadjust}{Whether including adotped budget information and comparisons for sharing of table with departments; default F, so adopted budget information excluded}
}
\value{
Rendered pivot table of departmental expenditures by division
}
\description{
Render pivottable of division expenditures overall
}
