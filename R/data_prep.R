#' Separate budget columns
#' Creates a number column and description column from standard Tyler Extract report column that's a combination of a number and description. E.g., Departments are contained in the "department" column and represented like 1000 Department Name; this creates a department_num column of 1000, and a 'department_desc" column of "Department Name."
#'
#'
#' @param df Minimally-processed Tyler Extract report dataframe
#' @param colname String name of column with a numeric part and string part separated by a space
#'
#' @return Dataframe with original column, and column for number-part of original column and string part
#' @export
#'
#' @examples{
#' c <- data.frame(sepcolumn = c("52 Cats", "35 Dogs", "42 Fish"))
#'
#' sep_budget(c, "sepcolumn")
#' }
#'
sep_budget <- function(df, colname){
  colnum <- paste0(colname, "_num")
  coldesc <- paste0(colname, "_desc")

  # look behind for number, ahead for characters, only do it once
  df %>%
    separate(col = !!sym(colname), into = c(colnum, coldesc), sep = "(?<=[0-9]) (?=[A-Z])", extra = "merge", remove = F)
}



#' Pull Year Column Names
#' Pulls names of year columns from minimally or non-minimally Tyler Extract Report custom dataset, with year column names following standard format of ####_#### or diff(year number).
#'
#' @param df Minimally-processed Tyler Extract report custom dataset. Year columns in dataset follow a standard naming convention of ####_#### or diff(year number).
#' @param nochng Whether to include columns measuring changes from one year to another or differences between years. If null, returns all columns; if true, omits change columns; if false, returns just change columns
#'
#' @return Character vector of year columns in dataset
#' @export
#'
#' @examples{
#'
#' df <- data.frame(account = c("a", "b", "c"), yr2021_2022.total.budget" = c(1, 2, 4), "yr2021_2022.ytd.activity.through.april" = c(4, 2, 1), "diff_adpt2022_prop2022" = c(2, 34, 1), "chng_2022_2023_tot" = c(1, 2, 4), yr21_22 = c(1, 2, 4))
#'
#' # pulls all cols but account and yr21_22
#' pull_year_cols(df)
#'
#' # pulls all cols but account, yr21_22, and chng_2022_2023_tot/diff_adpt2022_prop2022
#' pull_year_cols(df, T)
#'
#' # Returns just chng_2022_2023_tot/diff_adpt2022_prop2022
#' pull_year_cols(df, F)
#'
#' }
pull_year_cols <- function(df, nochng = NULL){


  yrs <- grep("([0-9]{4}_[0-9]{4})|(diff[0-9])", colnames(df), value = T)

  if (!is.null(nochng) & !is.logical(nochng)){
    stop("Parameter nochng must be true, false, or NULL")
  }

  if (is.null(nochng)){
    return(yrs)
  }

  if (nochng == T){
    yrs <- grep("(chng)|(diff)", yrs, value = T, invert = T)
  }

  if (nochng == F){
    yrs <- grep("(chng)|(diff)", yrs, value = T, invert = F)
  }

  if (length(yrs) == 0){
    warning("No year columns in dataset, or year columns renamed to non-standard format")
  }

  yrs
}


#' Factorize Revenue/Expenditure Column
#' Turns revenue/expenditure column of Tyler exract report dataframe into factor in order of revenue, then expense
#'
#' @param df Tyler Extract report dataframe with a column categorizing accounts as "Revenue" or "Expense"
#' @param colname Data-masked name of column categorizing accounts as "Revenue" or "Expense"
#'
#' @return
#' @export
#'
#' @examples{
#' df <- data.frame(account.type = c("Revenue", "Expense", "Revenue"), account_desc = c("Taxes", "Maintenance", "Parking fees"))
#'
#' revenue_factorize(df)
#'
#' df <- data.frame(revcol = c("Revenue", "Expense", "Revenue"), account_desc = c("Taxes", "Maintenance", "Parking fees"))
#'
#' revenue_factorize(df, revcol)
#'
#' }
revenue_factorize <- function(df, colname = account.type){

  output <- df %>%
    dplyr::mutate({{colname}} = factor({{colname}}, c("Revenue", "Expense"), c("Revenue", "Expense")))

  if (any(is.na(output %>%
                dplyr::pull({{colname}})))){
    warning("Some missing values returned in factorized column")
  }

}

#' Factorize Description Columns
#' Turns descriptive columns separated by tpbudget::sep_budget into factor columns ordered by the number of the column. For instance, turns "department_desc" column describing departments into factor column in order of department_num (the department number associated with the description; e.g., 1000, 2000, 3000, 4000). Useful for representing the descriptive component of the column in tables/visualizations in a logical order.
#'
#' @param df A Tyler Extract Report dataframe, likely processed by sep_budget
#' @param basename The string-name of the base column that's also been separated into numeric and descriptive columns. For instance, "department" would have it's own column as well as a "department_desc" column with the text-name of the department and a "department_num" column with the number of the department. Function assumes that base column is still present in dataframe, consisting of number/description combination (e.g., "1000 General Government", "40 Personnel").
#' @param .suffix Suffix of the column with the descriptive portion of the basename; default "_desc." Assumes name of descriptive-column is combination of basename and .suffix.
#'
#' @return A dataframe with a descriptive column as a factor
#' @export
#'
#' @examples{
#' a <- data.frame("department" = c("1000 General Government", "2000 Police", "3000 Public Works"))
#'
#' # now has a department_num and department_desc column
#' a <- tpbudget::sep_budget(a, "department")
#'
#' factorize_desc(a, "department")
#'
#' # works without numeric column so long as basename column is structured in "number text" format
#' factorize_desc(dplyr::select(a, -department_num), "department")
#'
#' a_rename <- dplyr::rename(a, "departmentdescriptive" = "department_desc")
#'
#' # change suffix to match
#' factorize_desc(a, "department", "descriptive")
#'
#' }
factorize_desc <- function(df, basename, .suffix = "_desc"){

  if (!all(c(basename, paste0(basename, .suffix)) %in% colnames(df))){
    stop("Columns for basename and basename/.suffix combination not in dataframe; check column names")
  }

  df_order <- df %>%
    dplyr::ungroup %>%
    dplyr::arrange(!!sym(basename))

  vec_order <- df[[paste0(basename, .suffix)]]

  df_order %>%
    dplyr::mutate(!!sym(paste0(basename, .suffix)) := factor(!!sym(paste0(basename, .suffix)), vec_order, vec_order))

}



budget_group <- function(df, colsgrp){

  # browser()

  # pull year columns from dataframe
  yrcols <- pull_year_cols(df)

  # get names of columns to group by
  grpcols <- map(colsgrp, ~ {
    grp_num <- paste0(.x, "_num")
    grp_desc <- paste0(.x, "_desc")

    return(c(.x, grp_num, grp_desc))
  }) %>%
    unlist()

  # remove if not in df (ie if didn't separate then only one column name)
  grpcols <- grpcols[grpcols %in% colnames(df)]

  # calculate total costs by grouped columns
  df %>%
    dplyr::group_by(across(all_of(grpcols))) %>%
    dplyr::summarise(dplyr::across(.cols = yrcols, .fns = ~ round(sum(.x, na.rm = T), digits = 2)))
}

