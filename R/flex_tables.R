#' Process budget basic table
#'
#' Prepares budget pivot table converted to basic table for visual representation in flextable; intermediate function used within pt_word_table
#'
#' @param bt Budget basic table
#' @param colvec Column-groups of budget basic table
#'
#' @return Basic table with values reformatted consistent with budget book formatting
#' @export
#'
#' @examples
process_bt <- function(bt, colvec){

  # browser()

  # pull names of header row
  header <- bt$cells$getRowValues(rowNumber = 1+length(colvec))

  # identify position of yr and change columns
  # loop through year and change columns, formatting them appropriately
  purrr::walk2(grep("(FY)|(Change)", header), grep("(FY)|(Change)", header, value = T), ~ {

    # pull values from each year column formatted as string
    yrvals <- bt$cells$getColumnValues(columnNumber = .x, formattedValue = T)

    # substitute for missing values
    yrvals <- gsub("(NaN)|(Inf)|(NA)|(-Inf)", " - ", yrvals, ignore.case = T)

    # turn negative numbers into parentheses
    yrvals <- case_when(grepl("^-[1-9]", yrvals) ~ gsub("-", "(", yrvals) %>%
                          paste0(., ")"),
                        T ~ yrvals)

    # if it's a percentage column - adjust for that
    if (grepl("%", .y)){
      yrvals <- gsub(" %", "%", yrvals)

      yrvals <- gsub(" - %", " - ", yrvals)

    }

    bt$cells$setColumn(columnNumber = .x, rawValues = yrvals)

  })


  # else{
  #   yrvals <- case_when(grepl(" - ", yrvals) ~ yrvals,
  #                       grepl("^\\([1-9]", yrvals) ~ paste0("$", yrvals),
  #                       grepl("^[0-9]", yrvals) ~ paste0("$", yrvals))
  # }

  bt

}



#' Create Standard Flextable in Word
#'
#' Creates standard budget flextable for output to Word documents
#'
#' @param df Budget dataframe to represent in flextable
#' @param rowvec Columns of budget dataframe to add as groups in flextable; default NULL, for none
#' @param colvec Columns of budget dataframe to add as columns in flextable; default NULL, for none
#' @param calcrow Whether to represent year columns in rows of flextable; default F, for not
#' @param .ptsavename Name of file to save pivot table as Excel document; default NULL, so not saved
#' @param .caption Caption of flextable; default NULL, so no caption added
#' @param deptadjust Whether including adotped budget information and comparisons for sharing of table with departments; default F, so adopted budget information excluded
#'
#' @return Flextable of budget data styled in standard format
#' @export
#'
#' @examples
pt_word_table <- function(df, rowvec = NULL, colvec = NULL, calcrow = F, .ptsavename = NULL, .caption = NULL, deptadjust = F){
  # browser()

  # flextable default = - replacing na
  flextable::set_flextable_defaults(nan_str = " - ")

  # create pivot
  pt <- tpbudget::pt_createstandard(df = df, rowvec = rowvec, colvec = colvec, calcrow = calcrow, deptadjust = deptadjust)

  # save pivot if want to
  if (is.character(.ptsavename)){
    tpbudget::pt_save(pt, .ptsavename, deptadjust = deptadjust)
  }

  # style pivot
  tpbudget::pt_style(pt = pt, df = df)

  # turn pivot into flextable
  basic_table <- pt$asBasicTable()

  # convert table to basic table - format values in basic table
  basic_table <- tpbudget::process_bt(basic_table, colvec = colvec)

  # convert basic table to flextable
  flex <- basic_table$asFlexTable()

  # set width of columns
  flex <- flextable::set_table_properties(flex, width = 0.5)

  # pull dataset
  flex_df <- flex$body$dataset

  # identify year columns position
  yrpos <- grep("(FY)|(\\$)", x = flex_df[1 + length(colvec), ])
  pct_pos <- grep("\\%", x = flex_df[1 + length(colvec), ])
  chng_pos <- grep("Change", flex_df[1 + length(colvec), ])


  # print(yrpos)
  # print(pct_pos)

  # if no columns or rows provided as argument - still supply value
  if (is.null(colvec)){
    colvec <- 1
  }

  if (is.null(rowvec)){
    rowvec <- 1
  }

  # identify rows with total in them
  rows_color <- purrr::map(1:length(rowvec), ~ grep("^Total$", flex_df[, .x])) %>% unlist() %>% unique()

  # format flextable
  flex <- flex %>%
    flextable::fontsize(x = .,
                        size = 7,
                        part = "all") %>%
    flextable::italic(part = "all") %>%
    flextable::bold(part = "header") %>%
    # set_caption("General Fund Summary") %>%
    flextable::autofit(add_w = 0,
            add_h = 0,
            part = "all") %>%
    # flextable::border_remove() %>%
    flextable::bg(i = 1:length(colvec), bg = "#004990") %>%
    flextable::color(i = 1:length(colvec), color = "white") %>%
    flextable::bg(i = rows_color, bg = "#a8d5e5") %>%
    flextable::bold(i = rows_color) %>%
    flextable::width(j = 1, width = 1) %>%
    flextable::width(j = chng_pos, width = 0.78)

  # flextable::colformat_char(j = yrpos, prefix = "$", i = (1:length(colvec)+1):nrow(flex_df))
  # flextable::compose(j = 5, i = 4, value = as_paragraph("( ", as_chunk(gsub("-", "", 'Change (%)')), ")"))
  # flextable::colformat_char(j = pct_pos, suffix = "%", i = (1:length(colvec)+1):nrow(flex_df))
  # colformat_num(j = yrpos, prefix = "$") %>%
  # colformat_num(j = pct_pos, prefix = "%")

  # browser()

  if (length(rowvec > 1)) {
    # format identifying columns of flextable to be narrower
    flex <- flex %>%
      flextable::width(j = 2:length(rowvec), width = 1)
  }

  # if (!is.null(.caption)){
  #   flex <- flex %>%
  #     flextable::set_caption(.caption, style = "Heading 4")
  # }

  flex <- flex %>%
    flextable::fit_to_width(max_width = 10)


  flex

}

#' Render budget table depending on doc type
#'
#' Renders either a pivottabler pivot-table or flextable pivot-table based on type of document; the former in Latex documents, the latter otherwise
#'
#' @param df Budget dataframe to represent in budget table
#' @param rowvec String column-names of the dataframe to add as row-groups to the pivot table; default NULL, for none
#' @param colvec String columnn-names of the dataframe to add as column-groups to the pivot table; default NULL, for none
#' @param calcrow Whether to display year columns of budget dataframe as rows of the pivot table; default F, so shown as columns
#' @param .ptsavename Filename to save pivot table as in Excel document; default NULL, so not saved
#' @param .caption Caption for pivot table; default NULL, so no caption
#' @param deptadjust Whether including adotped budget information and comparisons for sharing of table with departments; default F, so adopted budget information excluded
#'
#' @return pivottabler pivot-table of budget data or flextable pivot-table of budget data
#' @export
#'
#' @examples
pt_outputtype <- function(df, rowvec = NULL, colvec = NULL, calcrow = F, .ptsavename = NULL, .caption = NULL, deptadjust = F){
  if (knitr::is_latex_output()){
    tpbudget::pt_rendercreate(df = df, rowvec = rowvec, colvec = colvec, calcrow = calcrow, .ptsavename = .ptsavename, deptadjust = deptadjust)
  }

  else{
    tpbudget::pt_word_table(df = df, rowvec = rowvec, colvec = colvec, calcrow = calcrow, .ptsavename = .ptsavename, .caption = .caption, deptadjust = deptadjust)
  }
}

#' Create and Render Department Overall Pivot
#'
#' Generate standard pivot table for department expenditures overall
#'
#' @param dept_code Code of department creating table for
#' @param dept_adjust Whether including adotped budget information and comparisons for sharing of table with departments; default F, so adopted budget information excluded
#' @param bud_dept_list List of grouped budget expenditures
#' @param fy Fiscal year for this budget book
#'
#' @return Flextable or pivottabler pivot-table of budget data
#' @export
#'
#' @examples
pt_render_dept_exp_ov <- function(dept_code, bud_dept_list, fy = 2023, dept_adjust = F){
  # browser()

  tpbudget::pt_outputtype(tpbudget::gen_dept_exp_ov(dept_code = dept_code, bud_dept_list = bud_dept_list, fy = fy), rowvec = c("department_desc"), .ptsavename = paste0(dept_code, " department overall"), .caption = paste0(tpbudget::dept_pull(dept_code), " Department Expenditures Overall"), dept_adjust  = dept_adjust)

}


#' Create and Render Department Revenue Pivot
#'
#' Creates/renders departmental revenue for given department in pivot table
#'
#' @param dept_code Code of department to show revenue for
#' @param bud_extract_rev Budget extract revenue file
#' @param fy Fiscal year for this budget
#' @param dept_adjust Whether including adotped budget information and comparisons for sharing of table with departments; default F, so adopted budget information excluded
#'
#' @return Rendered pivot table of departmental revenues by source
#' @export
#'
#' @examples
pt_render_dept_overall <- function(dept_code, bud_extract_rev, fy = 2023, dept_adjust){
  # browser()

  pt_outputtype(tpbudget::gen_dept_summary_rev(dept_code = dept_code, bud_extract_rev = bud_extract_rev, fy = fy), rowvec = c("department_desc", "budget_cat_overall_desc"), .ptsavename = paste0(dept_code, " department overall"), .caption = paste0(dept_pull(dept_code), " Department Overall"), dept_adjust = dept_adjust)

}


#' Pivot Table of Departmental Expenditures by Division
#'
#' Create and render pivot table of departmental expenditures by division
#'
#' @param dept_code Code of department creating table for
#' @param dept_adjust Whether including adotped budget information and comparisons for sharing of table with departments; default F, so adopted budget information excluded
#' @param bud_dept_list List of grouped budget expenditures
#' @param fy Fiscal year for this budget book
#'
#' @return Pivot table of departmental expenditures by divisions
#' @export
#'
#' @examples
pt_render_dept_divcosts <- function(dept_code, bud_dept_list, fy = 2023, dept_adjust = F){
  tpbudget::pt_outputtype(
    tpbudget::gen_dept_summary_div(
      dept_code = dept_code,
      bud_dept_list = bud_dept_list,
      fy = fy),
    rowvec = "division_desc",
    .ptsavename = paste0(dept_code, " department by division"),
    .caption = paste0(dept_pull(dept_code), " Expenditures by Division"),
    dept_adjust = dept_adjust
    )
}


#' Pivot table of Departmental Expenditures by Personnel/Operating
#'
#' Generate pivot table of departmental expenditures by personnel/operating type
#'
#' @param deptcode Department code
#' @param dept_adjust Whether including adotped budget information and comparisons for sharing of table with departments; default F, so adopted budget information excluded
#' @param bud_dept_list List of grouped budget expenditures
#' @param fy Fiscal year for this budget book
#'
#' @return Pivot table of departmental expenditures by personnel/operating type
#' @export
#'
#' @examples
pt_render_dept_perstype <- function(deptcode, bud_dept_list, fy = 2023, dept_adjust = F){
  ptbudget::pt_outputtype(
    tpbudget::gen_dept_summary_perstype(
      deptcode,
      bud_dept_list = bud_dept_list,
      fy = fy),
    c("perstype", "budget_cat_overall_desc"),
    .ptsavename = paste0(deptcode, " department by operating"),
    .caption = paste0(dept_pull(deptcode), " Expenditures by Type"),
    dept_adjust = dept_adjust)
}


#' Render Pivot Division Expenditures Overall
#'
#' Render pivottable of division expenditures overall
#'
#' @param divcode Division code
#' @param dept_adjust Whether including adotped budget information and comparisons for sharing of table with departments; default F, so adopted budget information excluded
#' @param bud_dept_list List of grouped budget expenditures
#' @param fy Fiscal year for this budget book; default 2023
#'
#' @return Rendered pivot table of departmental expenditures by division
#' @export
#'
#' @examples
pt_render_div <- function(divcode, bud_dept_list, fy = 2023, dept_adjust = F){
  tpbudget::pt_outputtype(
    tpbudget::div_overall(
      divcode = divcode,
      bud_dept_list = bud_dept_list,
      fy = fy), rowvec = c("division_desc"), .ptsavename = paste0(divcode, " division overall"), .caption = paste0(div_pull(divcode), " Expenditures Overall"),
    dept_adjust = dept_adjust)
}


#' Render Pivot Division Expenses by Personnel/operating
#'
#' Creates/renders pivot table of division expenses by personnel/operating type
#'
#' @param divcode Division code
#' @param dept_adjust Whether including adotped budget information and comparisons for sharing of table with departments; default F, so adopted budget information excluded
#' @param bud_dept_list List of grouped budget expenditures
#' @param fy Fiscal year for this budget book; default 2023
#' @return
#' @export
#'
#' @examples
pt_render_divperstype <- function(divcode, bud_dept_list, fy = 2023, dept_adjust = F){
  # browser()
  tpbudget::pt_outputtype(
    df = tpbudget::div_perstype(
      divcode = divcode,
      bud_dept_list = bud_dept_list,
      fy = fy),
    rowvec = c("perstype", "budget_cat_overall_desc"),
    .ptsavename = paste0(divcode, " operating"),
    .caption = paste0(div_pull(divcode), " Expenditures by Type"),
    dept_adjust = dept_adjust)

}



