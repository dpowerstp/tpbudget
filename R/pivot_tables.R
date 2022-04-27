#' Add year columns to pivot tables
#'
#' Adds year columns to a pivot table, summing year columns and renaming the year column for representation in the table
#'
#' @param pt Pivot table to add column to, created by pivottabler::PivotTable$new()
#' @param colname String name of column to add year column to
#'
#' @return Pivot table with year column added to it
#' @export
#'
#' @examples
pt_year_calc <- function(pt, colname){

  exp <- paste0("sum(", colname, ", na.rm = T)")
  # ,(exp)
  pt$defineCalculation(calculationName=colname, summariseExpression = exp, format = list(digits = 0, nsmall = 0, big.mark=",", scientific = F), caption = tpbudget::year_transform(colname), noDataCaption = " - ")

}

#' Add change or difference columns to pivot table
#'
#' Adds change or difference in year columns to pivot table, summing total difference/change columns and recalculating/rounding percentage columns
#'
#' @param pt Pivot table to add change or difference columns to, created by pivottabler::PivotTable$new()
#' @param colname Name of change or difference column
#' @param proj Name of projected year column
#' @param current Name of current year column
#' @param adpt Name of adapted budget column; default NULL. Used to provide additional information to Departments for Departmental pivot tables
#'
#' @return Pivot table with change or difference column addded to it
#' @export
#'
#' @examples
pt_chng_calc <- function(pt, colname, proj, current, adpt = NULL){
  if (grepl("tot", colname)){
    exp <- paste0("sum(", colname, ", na.rm = T)")

    pt$defineCalculation(calculationName=colname, summariseExpression = exp, format = list(digits = 0, nsmall = 0, big.mark=",", scientific = F), caption = tpbudget::rename_yeartoyear(colname), noDataCaption = " - ")

  }

  else if (grepl("pct", colname) & grepl("chng", colname)){

    # browser()

    exp <- paste0("(values$", current, " - values$", proj, ") * 100 / values$", proj)

    #
    pt$defineCalculation(calculationName=colname, calculationExpression = exp, type = "calculation", basedOn = c(current, proj), format = "%.0f %%", caption = tpbudget::rename_yeartoyear(colname), noDataCaption = " - ")

  }

  else if (!is.null(adpt) & grepl("pct", colname) & grepl("diff", colname)){

    # browser()

    exp <- paste0("(values$", proj, " - values$", adpt, ") / values$", adpt)

    #
    pt$defineCalculation(calculationName=colname, calculationExpression = exp, type = "calculation", basedOn = c(adpt, proj), format = "%.0f %%", caption = tpbudget::rename_yeartoyear(colname), noDataCaption = " - ")

  }

}

#' Funcction to calculate totals for each year
#'
#' @param pt pivot table created by pivottabler::PivotTable$new()
#' @param cols String vector of year columns to calculate totals for
#'
#' @return Pivot table with year columns all added in
#' @export
#'
#' @examples
pt_calc_tot <- function(pt, cols){
  # browser()

  cols <- sort(cols)

  purrr::walk(cols, ~{
    tpbudget::pt_year_calc(pt = pt, colname = .x)
  })
}

#' Add all change or difference columns to pivot table
#'
#' Function to add all change or difference columns in budget dataframe to pivot table created by pivotTabler::PivotTable$new()
#'
#' @param pt pivot table created by pivottabler::PivotTable$new()
#' @param cols String vector of change or difference columns in budget dataframe being visualized in pivot table
#' @param proj Name of projected year column
#' @param current Name of current FY column
#' @param adpt Name of adapted budget column; default NULL. Used to provide additional information to Departments for Departmental pivot tables. Default NULL, so excluded
#'
#' @return Budget pivot table with change and difference columns added in
#' @export
#'
#' @examples
pt_year_calc_loop <- function(pt, cols, proj, current, adpt = NULL){

  purrr::walk(cols, ~{
    tpbudget::pt_chng_calc(pt = pt, colname = .x, current = current, proj = proj, adpt = adpt)
  })

}


#' Add row or column groups
#'
#' Function to add row or column groups to budget pivot table
#'
#' @param pt Pivot table created by pivottabler::PivotTable$new()
#' @param cols Columns to add as group to row or column in pivot table; string vector
#' @param colgrp Whether to add columns as groups in columns of pivot table, or as rows of pivot table; default T, for columns
#'
#' @return Pivot table with columns added in as rows or columns
#' @export
#'
#' @examples
pt_grps <- function(pt, cols, colgrp = T){

  purrr::walk(cols, ~ {

    if (colgrp) {
      pt$addColumnDataGroups(.x)

    }
    else{
      pt$addRowDataGroups(.x)
    }
  })
}


#' Standard Pivot Table
#'
#' Create standard pivot table for visualizing budget data
#'
#' @param df Dataframe visualized in pivot table
#' @param rowvec Rows to visualize in pivot table; default null, so no groups appear in rows added
#' @param colvec Columns to visualize in pivot table; default null, so no groups appear in columns
#' @param calcrow whether to put year columns in rows of pivot table; default F, so years appear at top of pivot table
#' @param calccoltop whether to put calculation groups in columns; default F, so don't appear in columns
#' @param deptadjust Whether to generate pivot table with differences for adapted budget, for sharing with departments. Default false
#'
#' @return Pivot table of budget data
#' @export
#'
#' @examples
pt_createstandard <- function(df, rowvec = NULL, colvec = NULL, calcrow = F, calccoltop = F, deptadjust = F){

  # browser()

  df <- dplyr::ungroup(df)

  yrs <- tpbudget::pull_year_cols(df)

  rowany <- function(x) rowSums(x, na.rm = T) > 0

  df <- df %>%
    dplyr::filter(rowany(
      dplyr::across(
        .cols = yrs,
        .fns = ~ .x != 0
      )
    ))

  # browser()
  pt <- pivottabler::PivotTable$new()

  pt$addData(df)

  # %>%
  #   grep("_pct", ., invert = T, value = T)
  #

  nonchng_cols <- grep("(_tot$)|(_pct$)", yrs, invert = T, value = T)
  chng_cols <- grep("(_tot$)|(_pct$)", yrs, invert = F, value = T)

  tpbudget::pt_calc_tot(pt, nonchng_cols)

  if (length(chng_cols) > 0){
    proj <- grep("project$", nonchng_cols, value = T)
    current <- grep("prelim$", nonchng_cols, value = T)

    adpt <- NULL

    if (deptadjust){
      adpt <- grep("total.budget$", nonchng_cols, value = T)
    }

    tpbudget::pt_year_calc_loop(pt = pt, cols = chng_cols, proj = proj, current = current, adpt = adpt)

  }

  prop_chng <- grep("(prelim)|(diff)|(chng)", yrs)

  if (!is.null(rowvec)){
    tpbudget::pt_grps(pt = pt, rowvec, F)

  }

  if (!is.null(colvec)){
    tpbudget::pt_grps(pt = pt, cols = colvec)
  }

  if (calcrow){
    pt$addRowCalculationGroups()
  }

  if (calccoltop){
    pt$addColumnCalculationGroups()
  }

  pt
}

#' Save budget pivot table
#'
#' Saves budget pivot table in Excel workbook
#'
#' @param pt Pivot table to save
#' @param filename Filename of Excel document, don't include .xlsx
#' @param deptadjust Whetherto save pivot tables to folder for sharing adopted budget information with departments; default false
#'
#' @return Outputted Excel document with pivot table
#' @export
#'
#' @examples
pt_save <- function(pt, filename, deptadjust = F){

  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb, filename)

  pt$evaluatePivot()

  pt$writeToExcelWorksheet(wb = wb, wsName = filename, topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)

  # excel_format_pt(wb, filename)

  subdir <- ifelse(deptadjust, "dept_tables", "dept_pivots")

  tpbudget::dir_budg(subdir)

  openxlsx::saveWorkbook(wb, file = paste0("./data/output/", subdir, "/", filename, ".xlsx"), filename, overwrite = T)

}


#' Style budget pivot table
#'
#' Applies prescribed style to budget pivot table
#'
#' @param pt Budget pivot table to style
#' @param df Budget dataframe pivot table representing
#' @param rowvec Row-groups of budget pivot table, represented as string vector
#' @param colvec Column-groups of budget pivot table, represented as string vector
#'
#' @return Pivot table with standard styling for budget book
#' @export
#'
#' @examples
pt_style <- function(pt, df, rowvec = NULL, colvec = NULL){

  pt$evaluatePivot()

  # default_table_theme <- list(
  #   fontName = "Arial",
  #   fontsize = "0.75em",
  # )

  yrs <- tpbudget::pull_year_cols(df)

  prop_chng <- grep("(prelim)|(diff)|(chng)", yrs)

  if (length(prop_chng) > 0){

    col_pos <- seq(length(yrs) - length(prop_chng) + 1, length(yrs), by = 1)

    pt$setStyling(columnNumbers = col_pos, declarations=list("background-color" = "#F2F2F2"))

  }

  pt

}

#' Create and Render Budget Pivot
#'
#' Creates and renders budget pivot table in one step
#'
#' @param df Dataframe to represent in budget pivot table
#' @param rowvec Columns of dataframe to add as row-groups to budget pivot table; default NULL, for none
#' @param colvec Columns of dataframe to add as column-groups to budget pivot table; default NULL, for none
#' @param calcrow Whether to display year columns as rows of budget pivot table; default F
#' @param .ptsavename Filename of outputted Excel document that contains pivot table; default NULL, in which case pivot table not saved as Excel doc
#' @param deptadjust Whether you're generating pivot tables to share with departments that include comparisons with the adopted budget book; default F, so no adjustment made
#'
#' @return Rendered pivot table of budget data
#' @export
#'
#' @examples
pt_rendercreate <- function(df, rowvec = NULL, colvec = NULL, calcrow = F, .ptsavename = NULL, deptadjust = F){
  # browser()

  pt <- tpbudget::pt_createstandard(df = df, rowvec = rowvec, colvec = colvec, calcrow = calcrow, deptadjust = deptadjust)


  if (is.character(.ptsavename)){
    tpbudget::pt_save(pt, .ptsavename, deptadjust = deptadjust)
  }

  # browser()
  tpbudget::pt_style(pt = pt, df = df)


  pt$renderPivot(exportOptions=list(exportNegInfAs=" - ", exportPosInfAs=" - ",
                                    exportNAAs=" - ", exportNaNAs=" - "))
}


