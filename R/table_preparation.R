#' Print Overall Department Expenditures
#' Prints a standard table name for Departmental  expenditures overall tables
#'
#' @param dept_code The Tyler code of the department
#' @param .inctable Whether to include table number as prefix; true or false
#' @param .tabnum Number of table in document; E.g., table 12
#'
#' @return String of Overall Departmental Expenditures table name
#' @export
#'
#' @examples{
#' print_dept_exp_overall(1000)
#'
#' print_dept_exp_overall(3000, F)
#'
#' print_dept_exp_overall(3000, T, 12)
#'
#'
#' }
print_dept_exp_overall <- function(dept_code, .inctable = T, .tabnum = "XX"){

  rel_dept <- dept_list %>%
    dplyr::filter(department_num == dept_code) %>%
    dplyr::pull(department_desc)

  if (length(rel_dept == 0)) {
    stop("Error; department code not in department list; check code again")
  }

  output <- "{rel_dept} Department Expenditures Overall"

  if (!is.logical(.inctable)){
    stop("Error; .inctable must be of type logical (true or false")
  }

  output <- ifelse(.inctable, paste0("Table ", tabnum, ":", output), output)

  glue::glue(output)
}

#' Print Departmental Revenue
#' Prints a standard table name for Departmental  revenue tables
#'
#' @param dept_code The Tyler code of the department
#' @param .inctable Whether to include table number as prefix; true or false
#' @param .tabnum Number of table in document; E.g., table 12
#'
#' @return String of Overall Departmental Revenues table name
#' @export
#'
#' @examples{
#' print_dept_overall(1000)
#'
#' print_dept_overall("3000", F)
#'
#' print_dept_overall(3000, T, 12)
#'
#'
#' }
print_dept_overall <- function(dept_code, .inctable = T, .tabnum = "XX"){

  rel_dept <- dept_list %>%
    dplyr::filter(department_num == dept_code) %>%
    dplyr::pull(department_desc)

  if (length(rel_dept == 0)) {
    stop("Error; department code not in department list; check code again")
  }

  output <- "{rel_dept} Department Revenue"

  if (!is.logical(.inctable)){
    stop("Error; .inctable must be of type logical (true or false")
  }

  output <- ifelse(.inctable, paste0("Table ", tabnum, ":", output), output)


  glue::glue(output)
}

#' Print Departmental Expenditures by Divsion
#' Prints a standard name for departmental tables on expenditures by division
#'
#' @param dept_code The Tyler code of the department
#' @param .inctable Whether to include table number as prefix; true or false
#' @param .tabnum Number of table in document; E.g., table 12
#'
#' @return String of Overall Departmental Expenditures by Division table name
#' @export
#'
#' @examples{
#' print_dept_divcosts(1000)
#'
#' print_dept_divcosts("3000", F)
#'
#' print_dept_divcosts(3000, T, 12)
#'
#'
#' }
print_dept_divcosts <- function(dept_code, .inctable = T, .tabnum = "XX"){
  rel_dept <- dept_list %>%
    dplyr::filter(department_num == dept_code) %>%
    dplyr::pull(department_desc)


  if (length(rel_dept == 0)) {
    stop("Error; department code not in department list; check code again")
  }

  output <- "{rel_dept} Expenditures by Division"

  if (!is.logical(.inctable)){
    stop("Error; .inctable must be of type logical (true or false")
  }

  output <- ifelse(.inctable, paste0("Table ", tabnum, ":", output), output)

  glue::glue(output)

}


#' Print Departmental Expenditures by Type
#' Prints a standard name for departmental tables on expenditures by type
#'
#' @param dept_code The Tyler code of the department
#' @param .inctable Whether to include table number as prefix; true or false
#' @param .tabnum Number of table in document; E.g., table 12
#'
#' @return String of Overall Departmental Expenditures by type table name
#' @export
#'
#' @examples{
#' print_dept_perstype(1000)
#'
#' print_dept_perstype("3000", F)
#'
#' print_dept_perstype(3000, T, 12)
#'
#'
#' }
print_dept_perstype <- function(dept_code, .inctable = T, .tabnum = "XX"){
  rel_dept <- dept_list %>%
    dplyr::filter(department_num == dept_code) %>%
    dplyr::pull(department_desc)


  if (length(rel_dept == 0)) {
    stop("Error; department code not in department list; check code again")
  }

  output <- "{rel_dept} Expenditures by Type"

  if (!is.logical(.inctable)){
    stop("Error; .inctable must be of type logical (true or false")
  }

  output <- ifelse(.inctable, paste0("Table ", tabnum, ":", output), output)

  glue::glue(output)
}

#' Print Division Expenditures Overall
#' Prints a standard name for division tables on expenditures overall
#'
#' @param dept_code The Tyler code of the division
#' @param .inctable Whether to include table number as prefix; true or false
#' @param .tabnum Number of table in document; E.g., table 12
#'
#' @return String of Overall Division Expenditures table name
#' @export
#'
#' @examples{
#' print_div_overall(1100)
#'
#' print_div_overall("3200", F)
#'
#' print_dept_exp_overall(3200, T, 12)
#'
#'
#' }
print_div_overall <- function(div_code, .inctable = T, .tabnum = "XX"){

  rel_div <- div_list %>%
    filter(division_num == div_code) %>%
    pull(division_desc)

  if (length(rel_div == 0)) {
    stop("Error; division code not in division list; check code again")
  }

  output <- "{rel_div} Expenditures by Type"

  if (!is.logical(.inctable)){
    stop("Error; .inctable must be of type logical (true or false")
  }

  output <- ifelse(.inctable, paste0("Table ", tabnum, ":", output), output)

  glue::glue(output)

  glue("Table XX: {rel_div} Division Overall")

}


#' Print Division Expenditures by Type
#' Prints a standard name for division tables on expenditures by type
#'
#' @param dept_code The Tyler code of the division
#' @param .inctable Whether to include table number as prefix; true or false
#' @param .tabnum Number of table in document; E.g., table 12
#'
#' @return String of Overall Division Expenditures table name
#' @export
#'
#' @examples{
#' print_div_perstype(1100)
#'
#' print_div_perstype("3200", F)
#'
#' print_div_perstype(3200, T, 12)
#'
#'
#' }
print_div_perstype <- function(div_code, .inctable = T, .tabnum = "XX"){
  rel_div <- div_list %>%
    dplyr::filter(division_num == div_code) %>%
    dplyr::pull(division_desc)

  if (length(rel_div == 0)) {
    stop("Error; division code not in division list; check code again")
  }

  output <- "Table XX: {rel_div} Expenditures by Type"

  if (!is.logical(.inctable)){
    stop("Error; .inctable must be of type logical (true or false")
  }

  output <- ifelse(.inctable, paste0("Table ", tabnum, ":", output), output)

  glue::glue(output)

}

#' Print Performance Table Name
#' Prints standard name for performance/workload measure table
#'
#' @param deptdivname Name of division/department as string
#' @param .inctable Whether to include table number as prefix; true or false
#' @param .tabnum Number of table in document; E.g., table 12
#'
#' @return
#' @export
#'
#' @examples{
#' print_performance("Legal Services")
#' print_performance("General Government")
#'
#' }
print_performance <- function(deptdivname, .inctable = T, .tabnum = "XX"){
  glue::glue("Table XX: {deptdivname} Performance/Workload Measures")
}

#' Rename budget columns
#' Renames budget columns in preparation for visualization in tables or visualizations, including applying year_transform and tpbudget::df_rename_yeartoyear functions
#'
#'
#' @param df_rename Processed Tyler Budget Extract report preparing for visualization
#'
#' @return Dataframe with renamed columns for visualization in table or graphs
#' @export
#'
#' @examples
budget_rename <- function(df_rename){

  # print(colnames(df_rename))

  check_type <- function(string){
    case_when(string == "perstype" ~ "Expenditure type",
              string == "accountnum" ~ "Account",
              T ~ string)
  }

  # if ( %in% colnames(df_rename)){
  #   df_rename <- df_rename %>%
  #     rename( = perstype)
  # }

  yrcols <- tpbudget::pull_year_cols(df_rename) %>%
    grep("(chng)|(diff)", x = ., value = T, invert = T)

  df_rename%>%
    dplyr::rename_all(~ check_type(.x)) %>%
    dplyr::rename_with(.fn = ~ tbudget::year_transform(.x), yrcols) %>%
    tpbudget::df_rename_yeartoyear() %>%
    dplyr::rename_all(stringr::str_to_title) %>%
    dplyr::rename_all(~ paste0(.x, "  "))

}

#' Identify years too early
#'
#' Pulls year columns that should be omitted from budget tables because they are before the range of years
#'
#' @param fy Fiscal year of this budget book
#' @param tbltype Options include "recent" (to include only preceding 3 fiscal years) and "operating" (to include preceding 4 fiscal years)
#' @param bud_extract_rev A budget extract dataframe to pull year columns from
#'
#' @return String vector of year columns to exclude from dataframe
#' @export
#'
#' @examples
find_yrstooearly <- function(fy, bud_extract_df, tbltype = "recent"){


  if (!tbltype %in% c("recent", "operating")){
    stop("Parameter tbltype must be one of 'recent' or 'operating'")
  }

  # both these included regardless start or end
  tfminusone <- fy - 1
  tfminustwo <- fy - 2

  if (tbltype == "recent"){
    # ok to start with this year - but not end with it
    tflaststart <- paste0("_", fy - 3)

    # grep match
    matchyrs <- glue::glue("({fy})|({tfminusone})|({tfminustwo})|({tflaststart})")

  }

  if (tbltype == "operating"){
    # extra years included in operating tables
    tfminusthree <- fy - 3
    tfminuslaststart <- paste0("_", fy - 4)

    matchyrs <- glue::glue("({fy})|({tfminusone})|({tfminustwo})|({tfminusthree})|({tflaststart})")

  }

  yrs_too_early <- c(grep(matchyrs, tpbudget::pull_year_cols(bud_extract_df), value = T, invert = T),
                     grep("(cert)|(ytd)", tpbudget::pull_year_cols(bud_extract_df), value = T, invert = T)) %>%
    unique()

  yrs_too_early
}


#' Pull Department name from department code
#'
#' Pulls department name from supplied department code
#'
#' @param dc Department code
#'
#' @return Department name
#' @export
#'
#' @examples
dept_pull <- function(dc){
  dept_name <- tpbudget::dept_list %>%
    dplyr::filter(department_num == dc) %>%
    dplyr::pull(department_desc)

  dept_name

}


#' Pull Division name from Division code
#'
#' Pulls Division name from supplied Division code
#'
#' @param dc Division code
#'
#' @return Division name
#' @export
#'
#' @examples
div_pull <- function(dc){
  tpbudget::div_list %>%
    dplyr::filter(division_num == dc) %>%
    dplyr::pull(division_desc)
}




