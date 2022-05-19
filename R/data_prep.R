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
    tidyr::separate(col = !!dplyr::sym(colname), into = c(colnum, coldesc), sep = "(?<=[0-9]) (?=[A-Z])", extra = "merge", remove = F)
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
#' df <- data.frame(account.type = c("Revenue", "Expense", "Revenue"),
#' account_desc = c("Taxes", "Maintenance", "Parking fees"))
#'
#' revenue_factorize(df)
#'
#' df <- data.frame(revcol = c("Revenue", "Expense", "Revenue"),
#' account_desc = c("Taxes", "Maintenance", "Parking fees"))
#'
#' revenue_factorize(df, revcol)
#'
#' }
revenue_factorize <- function(df, colname = account.type){

  output <- df %>%
    dplyr::mutate({{colname}} := factor({{colname}}, c("Revenue", "Expense"), c("Revenue", "Expense")))

  if (any(is.na(output %>%
                dplyr::pull({{colname}})))){
    warning("Some missing values returned in factorized column")
  }

}

#' Factorize Description Columns
#'
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
#' factorize_desc(a_rename, "department", "descriptive")
#'
#' }
factorize_desc <- function(df, basename, .suffix = "_desc"){

  if (!all(c(basename, paste0(basename, .suffix)) %in% colnames(df))){
    stop("Columns for basename and basename/.suffix combination not in dataframe; check column names")
  }

  df_order <- df %>%
    dplyr::ungroup() %>%
    dplyr::arrange(!!dplyr::sym(basename))

  vec_order <- df[[paste0(basename, .suffix)]]

  df_order %>%
    dplyr::mutate(!!dplyr::sym(paste0(basename, .suffix)) := factor(!!dplyr::sym(paste0(basename, .suffix)), vec_order, vec_order))

}

#' Group Budget Data
#'
#' Groups Tyler budget extract report budget data by provided set of columns, summing year columns in the dataset. Year columns in the data should have standard names that will be picked up by tpbudget::pull_year_cols()
#'
#' @param df Tyler Extract Report budget dataset
#' @param colsgrp String vector of columns to group the Tyler Extract Report by. For columns that have been separated by tpbudget::sep_budget, only need to identify the basename of the column. For instance, if there are columns for accountnum, accountnum_desc, and accountnum_num, only need to provide "accountnum" to still keep the "accountnum_desc" and "accountnum_num" columns in the group. Note that if there are "change" or "difference" year-columns in the dataset, percentages will still be summed and these should be recalculated
#'
#' @return A dataframe grouped by the provided colsgroup vector, with year-columns summed by the grouped columns
#' @export
#'
#' @examples{
#'
#' df <- data.frame(account = c("a", "b", "c"), department = c("1000 General Government", "1000 General Government", "1000 General Government"), division = c("1100 City Manager", "1100 City Manager", "1200 Legal Services"), yr2021_2022.total.budget = c(1, 2, 4), "yr2021_2022.ytd.activity.through.april" = c(4, 2, 1), "diff_adpt2022_prop2022" = c(2, 34, 1), "chng_2022_2023_tot" = c(1, 2, 4), yr21_22 = c(1, 2, 4))
#'
#' budget_group(df = df, colsgrp = "department")
#'
#' # "department_desc" and "department_num" cols still included
#' budget_group(df = sep_budget(df, "department), "department")
#'
#' budget_group(df = df, c("department", "division"))
#'
#' }
budget_group <- function(df, colsgrp){

  # pull year columns from dataframe
  yrcols <- tpbudget::pull_year_cols(df)

  if (length(yrcols) == 0){
    stop("No year columns recognized by pull_year_cols; check if they have standard names")

  }

  if (!all(colsgrp) %in% colnames(df)){
    stop("Not all columns in colsgrp appear in dataframe")
  }

  if (any(grepl("pct", yrcols))){
    warning("Percent columns identified in year columns; are summed across groups and will likely need to be recalculated")
  }

  # get names of columns to group by
  grpcols <- purrr::map(colsgrp, ~ {
    grp_num <- paste0(.x, "_num")
    grp_desc <- paste0(.x, "_desc")

    return(c(.x, grp_num, grp_desc))
  }) %>%
    unlist()

  # remove if not in df (ie if didn't separate then only one column name)
  grpcols <- grpcols[grpcols %in% colnames(df)]

  # calculate total costs by grouped columns
  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grpcols))) %>%
    dplyr::summarise(dplyr::across(.cols = yrcols, .fns = ~ round(sum(.x, na.rm = T), digits = 2)))
}


#' One to one merge check
#'
#' Checks if number of rows increased in the joined dataframe compared to one the dataframe that was left-joined to after what was supposed to be a one-to-one merge
#'
#' @param df1 The merged dataframe
#' @param df2 The dataframe that was left-joined to
#' @param brows Whether to enter browser for debugging if there's an error; default F
#'
#' @return Nothing; stops if number of rows increased in joined dataframe and enters browser
#' @export
#'
#' @examples
oneone_check <- function(df1, df2, brows = F){
  if (!nrow(df1) == nrow(df2)){

    if (brows){
      browser()
    }

    comp <- df2 %>%
      dplyr::group_by(account.number, accountnum) %>%
      dplyr::mutate(freq = dplyr::n())

    stop(glue::glue("Number of rows increased in output dataset after merge. There are {nrow(df1)} rows in DF1, and {nrow(df2)} rows in DF2"))
  }

}

#' Base-process Tyler Report Extract
#' Base-processing of unprocessed Tyler report extract dataset. Standardizes column names  for other functions, associates Tyler categories with dataset, makes sure merged successfully
#'
#' @param df Unprocessed Tyler-report extract
#'
#' @return Base-processed Tyler report extract
#' @export
#'
#' @examples
base_process_extract <- function(df){

  # browser()

  output <- df %>%
    dplyr::rename_all(tolower) %>%
    dplyr::rename_all(.funs = ~ gsub("-", "_", .x) %>%
                        gsub("#", "num", x = .) %>%
                        gsub("^([0-9])", "yr\\1", .)) %>%
    dplyr::left_join(tpbudget::tyler_funds, by = c("fund" = "fund_num"))

  # check to confirm one to one merge tyler funds and df
  tpbudget::oneone_check(df, output)

  output
}

#' Read in and Process Tyler Report Extract
#' Reads in unprocessed Tyler Budget Extract Report Custom file, and performs base-processing on the file to pull expenditure data or revenue data.
#'
#' @param filepath Filepath to unprocessed Tyler Budget Extract Report Custom file
#' @param exp Exp indicates whether want to return revenue dataset (F) or expenditure dataset (T; default). Unprocessed Tyler Budget Extract Report Custom is structured as a revenue dataset on top of an expenditure dataset
#'
#' @return Base-processed Tyler Budget Extract report custom file with standard column names
#' @export
#'
#' @examples
read_process_extract <- function(filepath = "./data/Budget Extract Report Custom.xlsx", exp = F){

  # read in data
  df <- openxlsx::read.xlsx(filepath)

  # find row with colnames in it; dataset is structured as an revenue dataset with column names in the first row, and then a expenditure dataset with column names in a later row
  rowvals <- is.na(df$X17)


  pos_new_table <- match(F, rowvals)

  # for budget cateogry overall
  # budgetcat <- function(df){
  #
  #   quickfunct <- function(df, string, cat){
  #     df %>%
  #       mutate({{string}} := case_when(is.na({{string}}) ~ {{cat}},
  #                                      T ~ {{string}}))
  #   }
  #
  #   df %>%
  #     quickfunct(budget_cat_overall, accountnum) %>%
  #     quickfunct(budget_cat_overall_desc, accountnum_desc) %>%
  #     quickfunct(budget_cat_overall_num, accountnum_num)
  # }


  if (!exp){
    df <- df %>%
      dplyr::select(-c(X17, X18)) %>%
      tpbudget::base_process_extract()  %>%
      tpbudget::sep_budget("category") %>%
      tpbudget::sep_budget("accountnum")  %>%
      dplyr::slice_head(n = pos_new_table-1) %>%
      dplyr::left_join(tpbudget::rev_budget_cat) %>%
      dplyr::mutate(department = dplyr::case_when(is.na(department) ~ "9000 Non-departmental",
                                    T ~ department),
             budget_cat_overall_desc = dplyr::case_when(is.na(budget_cat_overall_desc) ~ accountnum_desc,
                                                 T ~ budget_cat_overall_desc)) %>%
      tpbudget::sep_budget("department")
    # mutate(department = revdeptcode(account.name)) %>%
    # sep_budget("department")
    # left_join(budget_cat_df, by = c("accountnum_num")) %>%
    # budgetcat()

  }

  else if (exp){

    df <- openxlsx::read.xlsx(filepath, startRow = pos_new_table+1) %>%
      tpbudget::base_process_extract() %>%
      tpbudget::sep_budget("department") %>%
      tpbudget::sep_budget("division") %>%
      tpbudget::sep_budget("category") %>%
      tpbudget::sep_budget("accountnum") %>%
      tpbudget::recode_personnel_other() %>%
      dplyr::left_join(tpbudget::table_accounts_complete)
    # %>%
    #   left_join(budget_cat_df, by = c("accountnum_num")) %>%
    #   budgetcat()
  }

  yr_rev <- tpbudget::pull_year_cols(df)

  df %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(yr_rev), .fns = as.numeric)) %>%
    dplyr::mutate(fund_recode = dplyr::case_when(grepl("General Fu", fund_desc) ~ fund_desc,
                                   T ~ "All Other Funds"),
           fund_recode = factor(fund_recode, c("General Fund", "All Other Funds"), c("General Fund", "All Other Funds")))
}


#' Recode expenditures as personnel, operating, or non-departmental
#' Code expenditures as perosonnel, operating, or non-departmental types
#'
#' @param df Ungrouped Tyler-extract report, ideally processed by base_report_extract
#' @param category_num_col Data-masked column identifying the category number; default category_num
#' @param division_col Data-masked column identifying the full division; defaults division
#' @param accountnum_desc_col Data-masked column identifying the account number; defaults accountnum_desc
#' @param div_desc Data-masked column identifying description of division; defaults division_desc
#'
#' @return Dataframe with perstype column of factor type categorizing expenditures as "Personnel" or "Other Operating Expenses"
#' @export
#'
#' @examples
recode_personnel_other <- function(df, category_num_col = category_num, division_col = division, div_desc = division_desc, accountnum_desc_col = accountnum_desc){


  df <- df %>%
    dplyr::mutate(perstype = dplyr::case_when({{category_num_col}} == "40" ~ "Personnel",
                                              grepl("^9000", {{division_col}}) ~ {{accountnum_desc_col}},
                                              grepl("^[8-9][0-9]", {{division_col}}) ~ {{div_desc}},
                                              T ~ "Other Operating Expenses"))

  persunique <- grep("(Personnel)|(Other Operating Expenses)", df[["perstype"]] %>% unique, value = T, invert = T)

  persorder <- c("Personnel", "Other Operating Expenses")

  df %>%
    dplyr::mutate(perstype = factor(perstype, persorder, persorder))

}


#' Calculate Change and Differences in Years
#'
#' From a base-processed Tyler Extract Report dataset, calculates change in year-to-year values for a given fiscal year, and differences between adopted and projected expenditures for a given fiscal year
#'
#' @param df Tyler Extract Report dataset processed by base_report_extract()
#' @param curr_fy Current fiscal year
#'
#' @return Dataframe with columns for
#' @export
#'
#' @examples
calc_yearcol <- function(df, curr_fy = 2022){

  if (!is.numeric(curr_fy)){
    return("Error; current fiscal year is not numeric")
  }

  # pull year columns from dataframe
  df_years <- tpbudget::pull_year_cols(df)

  # pull current fiscal year column, previous, and next
  curr_years <- grep(paste0("^yr", curr_fy - 1, "_", curr_fy), df_years, value = T)
  act_year_last <- grep(pattern = paste0("^yr", curr_fy - 2, "_", curr_fy - 1), df_years, value = T)
  next_year <- grep(paste0("^yr", curr_fy, "_", curr_fy + 1), df_years, value = T)


  # if (proposed){
  #   prelim_next <-
  # }

  # browser()
  # identify actual and projected in current year
  curr_years_adopt <- grep("total.budget$", curr_years, value = T)

  curr_years_proj <- grep("project", curr_years, value = T)

  message(glue::glue("Identifying current year adopted/adjusted as {{curr_years_adopt}}, current year projected as {{curr_years_proj}}, last year as {{act_year_last}}, and next year as {{next_year}}"))

  # should now have 4 year columns - if pulling multiple, return error
  if (any(purrr::map_lgl(c(curr_years_adopt, curr_years_proj, act_year_last, next_year), ~ {
    check <- length(.x) != 1
    if (check) print(x)

    check
  }))){
    stop("Error; multiple column-names returned in pattern match")
  }

  # creae column names for change in budget to next year and difference in projected vs. actual expenditures
  chng_tot <- paste0("chng_", curr_fy, "_", curr_fy+1, "_tot")
  chng_pct <- paste0("chng_", curr_fy, "_", curr_fy+1, "_pct")

  diff_proj_adpt_tot <- paste0("diff_adpt", curr_fy, "_prop", curr_fy, "_tot")
  diff_proj_adpt_pct <- paste0("diff_adpt", curr_fy, "_prop", curr_fy, "_pct")

  message(glue::glue("Identifying change in projected to proposed expenditures as {{chng_tot}}, and change in adjusted to projected expenditures as {{diff_proj_adpt_tot}}"))

  # diff prelim project
  # diff_prelim_projlast_tot <- paste0("_projlast", curr_fy, "_projlast", curr_fy, "_tot")
  # diff_prelim_projlast_pct <- paste0("_projlast", curr_fy, "_projlast", curr_fy, "_pct")

  # calculate change values
  df %>%
    dplyr::mutate(
      !!dplyr::sym(chng_tot) := !!dplyr::sym(next_year) - !!dplyr::sym(curr_years_proj),
      !!dplyr::sym(chng_pct) := !!dplyr::sym(chng_tot) / !!dplyr::sym(curr_years_proj),
      !!dplyr::sym(diff_proj_adpt_tot) := !!dplyr::sym(curr_years_proj) - !!dplyr::sym(curr_years_adopt),
      !!dplyr::sym(diff_proj_adpt_pct) := !!dplyr::sym(diff_proj_adpt_tot) / !!dplyr::sym(curr_years_adopt)
      # !!dplyr::sym(diff_prelim_projlast_tot) := !!dplyr::sym(next_year) - !!dplyr::sym(curr_years_proj),
      # !!dplyr::sym(diff_prelim_projlast_pct) := tpfuncts::pct_round(!!dplyr::sym(chng_tot), !!sym(curr_years_proj)),


    )

}


#' Create List of Grouped Datasets
#' Groups base-processed Tyler Expenditures or Revenue dataset by different combinations of columns of interest, summing year columns by groups and calculating changes for a given fiscal-year. Returns list of grouped-dataframes
#'
#' @param df Tyler Budget Extract report expenditures or revenue dataset
#' @param curr_fy Current fiscal year
#' @param .missingcols Whether to print names of grouped-columns that are not present in the dataframe
#'
#' @return List of datasets, grouped by different combinations of group columns
#' @export
#'
#' @examples
budget_list <- function(df, curr_fy = 2022, .missingcols = F){
  # browser()

  # columns to group data by
  cols_grp_by <- c("fund", "department", "division", "category", "accountnum", "tablecat")
  cols_df <- colnames(df)

  # if not in dataframe - filter out - is this redundant with checkpres
  cols_grp_by <- cols_grp_by[cols_grp_by %in% cols_df]

  # check to make sure columns present
  bud_grp_checkpres <- function(df, colsgrp){
    if (all(colsgrp %in% colnames(df))){
      output_df <- tpbudget::budget_group(df = df, colsgrp = colsgrp) %>%
        tpbudget::calc_yearcol(curr_fy = curr_fy)

      output_df

      # list(paste0(colsgrp, collapse = "_") = output_df)
    }
    else{
      return(T)
    }
  }

  # for each set of columns in list of columns to group by - group the dataset by that, and assign names to the list of the columns grouped by
  map_bud_grp_check <- function(colsgrp){
    # print(colsgrp)
    #
    # browser()
    #
    list_dfs <- purrr::map(colsgrp, ~ bud_grp_checkpres(df, .x))

    names(list_dfs) <- purrr::map_chr(colsgrp, ~ paste0(.x, collapse = "_"))

    list_dfs
  }


  output_dfs <- map_bud_grp_check(
    list(
      c("fund"),
      c("fund", "perstype"),
      c("fund", "account.type"),
      c("fund", "accountnum"),
      c("fund", "account.type", "accountnum"),
      c("fund", "account.type", "category",  "accountnum"),
      c("fund", "category", "accountnum"),
      c("fund", "account.type", "category"),
      c("fund", "category"),
      c("account.type"),
      c("accountnum"),
      c("account.type", "accountnum"),
      c("account.type", "category",  "accountnum"),
      c("category", "accountnum"),
      c("account.type", "category"),
      c("category"),

      c("fund", "department"),

      c("fund", "department", "perstype"),
      c("fund", "department", "division", "perstype"),
      c("fund", "department", "perstype", "accountnum"),
      c("fund", "department", "division", "perstype", "accountnum"),


      c("department", "fund"),
      c("fund", "department", "account.type"),
      c("fund", "account.type", "department"),
      c("fund", "department", "accountnum"),
      c("fund", "accountnum", "department"),
      c("fund", "department", "account.type", "accountnum"),
      c("fund", "department", "account.type", "category",  "accountnum"),
      c("fund", "department", "category", "accountnum"),
      c("fund", "department", "account.type", "category"),
      c("fund", "department", "category"),
      c("fund", "category", "department"),

      c("department", "account.type"),
      c("department", "accountnum"),
      c("department", "account.type", "accountnum"),
      c("department", "account.type", "category",  "accountnum"),
      c("department", "category", "accountnum"),
      c("department", "account.type", "category"),
      c("department", "category"),
      c("department"),
      c("category", "department"),

      c("division"),
      c("department", "division"),


      c("fund", "department", "division"),
      c("department", "division", "fund"),
      c("fund", "department", "division", "account.type"),
      c("fund", "account.type", "department", "division"),
      c("fund", "department", "division", "accountnum"),
      c("fund", "accountnum", "department", "division"),
      c("fund", "department", "division", "account.type", "accountnum"),
      c("fund", "department", "division", "account.type", "category",  "accountnum"),
      c("fund", "department", "division", "category", "accountnum"),
      c("fund", "department", "division", "account.type", "category"),
      c("fund", "department", "division", "category"),
      c("department", "division", "account.type"),
      c("department", "division", "accountnum"),
      c("department", "division", "account.type", "accountnum"),
      c("department", "division", "account.type", "category",  "accountnum"),
      c("department", "division", "category", "accountnum"),
      c("department", "division", "account.type", "category"),
      c("department", "division", "category"),
      c("category", "department", "division"),
      c("fund", "category", "department", "division"),

      c("fund", "budget_cat_overall"),
      c("fund", "department", "budget_cat_overall"),
      c("fund", "department", "perstype", "budget_cat_overall"),

      c("fund", "department", "division", "budget_cat_overall"),
      c("fund", "budget_cat_overall"),
      c("fund", "department", "division", "budget_cat_overall", "accountnum"),
      c("fund", "department", "budget_cat_overall", "accountnum"),
      c("fund", "department", "perstype", "budget_cat_overall", "accountnum"),
      c("fund", "department",  "division", "perstype", "budget_cat_overall"),
      c("fund", "department",  "division", "perstype", "budget_cat_overall", "accountnum")

    )
  )

  # filter out items from list that are not datasets
  vec_log <- purrr::map_lgl(output_dfs, ~ !is.logical(.x))

  if (.missingcols) {
    message("Nmaes of grouping columns that are not present in dataframe:")
    print(names(output_dfs[!vec_log]))
  }

  # filter out null responses (indicating columns weren't present)
  output_dfs[vec_log]

}


#' Rename Year Columns
#' Renames year columns to match budget book descriptions, for representation in visualizations and tables
#'
#' @param yrcol String or string-vector associated with year column names from tpbudget::base_process_extract()
#'
#' @return String representation of year column
#' @export
#'
#' @examples{
#'
#' year_transform("yr2021_2022.total.budget")
#'
#' year_transform(c("yr2021_2022.ytd.activity.through.april", "yr2021_2022.total.budget"))
#'
#' }
year_transform <- function(yrcol){
  # extract year, attach fy to
  year_val <- stringr::str_extract(string = yrcol, pattern = "_[0-9]{4}(?<=.)") %>%
    gsub("_20", "FY", .)

  # quick wrapper around grepl to shorten
  check_str <- function(pattern){
    grepl(pattern = pattern, x = yrcol, ignore.case = T)
  }

  # rename year columns to correspond to budget book
  type_check <- dplyr::case_when(
    check_str("ytd") ~ "Year to date",
    check_str("cert") ~ "Certified",
    check_str("project") ~ "Projected",
    check_str("prelim") ~ "Proposed",
    check_str("activity") ~ "Actual",
    check_str("budget") ~ "Adjusted"
  )

  paste0(type_check, " ", year_val)

}

#' Rename year-change columns
#' Rename columns representing changes from one year to another year, and difference columns from adopted to projected to names for representation in tables and visualizations
#'
#' @param vec Character vector of change and difference column names
#'
#' @return Vector of renamed columns
#' @export
#'
#' @examples
rename_yeartoyear <- function (vec) {

  vals <- dplyr::case_when(grepl("chng.*tot", vec) ~ "Change ($) Proj FY22-FY23 ",
                           grepl("chng.*pct", vec) ~ "Change (%) Proj FY22-FY23 ",
                           grepl("diff.*tot", vec) ~ "Difference adjusted and projected ($)",
                           grepl("diff.*pct", vec) ~ "Difference adjusted and projected (%)")

  if (any(is.na(vals))){
    browser()
    stop("Error; some values in vec not recognized")
  }

  vals
}


#' Rename year-to-year columns in dataframe
#' Renames year-to-year change and difference between adopted and adjusted columns in a dataframe.
#'
#' @param df Dataframe with standard year to year change and difference between adopted and adjusted columns.
#'
#' @return Dataframe with year-to-year and difference columns renamed
#' @export
#'
#' @examples
df_rename_yeartoyear <- function(df){

  chng_cols <- grep("chng_[0-9]{4}", colnames(df), value = T)

  diff_cols <- grep("diff_adpt[0-9]{4}", colnames(df), value = T)

  if (length(chng_cols) == 0 & length(diff_cols) == 0){
    stop("Error; no year-to-year change and difference columns identified")

  }

  df %>%
    dplyr::rename_with(~ tpbudget::rename_yeartoyear(.x), c(chng_cols, diff_cols))

}


#' Generate departmental revenue table
#'
#' Generates departmental revenue table for given department
#'
#' @param dept_code Department code
#' @param bud_extract_rev Base budget revenue dataset
#' @param fy Fiscal year; default 2023
#'
#' @return Dataframe of departmental revenues for a given department
#' @export
#'
#' @examples
gen_dept_summary_rev <- function(dept_code, bud_extract_rev, fy = 2023){

  # browser()

  yrs_too_early <- tpbudget::find_yrstooearly(fy = fy, bud_extract_df = bud_extract_rev, "recent")

  yrs <- tpbudget::pull_year_cols(bud_extract_rev)

  op_exp <- bud_extract_rev %>%
    dplyr::mutate(dplyr::across(yrs, ~ -.x)) %>%
    tpbudget::budget_group(colsgrp = c("fund", "department", "budget_cat_overall_desc")) %>%
    tpbudget::calc_yearcol %>%
    dplyr::filter(department_num == dept_code) %>%
    dplyr::select(-yrs_too_early)

  op_exp

  # browser()

}

#' Generate departmental expenses by division dataframe
#'
#' Generates departmental expenses by division dataframe for visualization in tables, grouping base processed-expenditures dataframe by department and division
#'
#' @param dept_code Department code to generate table for
#' @param exp_df Base process Tyler expenditures dataframe
#' @param fy Fiscal year preparing budget book for
#'
#' @return Budget expenditures dataframe with factorized description field, and without too-early year columns
#' @export
#'
#' @examples
gen_dept_summary_div <- function(dept_code, exp_df, fy = 2023){

  # group by department and division
  grpdf <- tpbudget::budget_group(df = exp_df, colsgrp = c("department", "division")) %>%
    tpbudget::calc_yearcol(curr_fy = fy)

  yrs_too_early_div <- tpbudget::find_yrstooearly(fy = fy, bud_extract_df = grpdf, "recent")

  grpdf %>%
    dplyr::filter(department_num == dept_code) %>%
    tpbudget::factorize_desc("division") %>%
    dplyr::select(-yrs_too_early_div)
}



#' Generate Departmental Expenditures Overall
#'
#' Generates departmental expenditures overall dataframe for visualization in tables
#'
#' @param dept_code Department code of Department to prepare dataframe for
#' @param exp_df Base process Tyler expenditures dataframe
#' @param fy Fiscal year preparing budget book for
#'
#' @return
#' @export
#'
#' @examples
gen_dept_exp_ov <- function(dept_code, exp_df, fy = 2023){

  grpdf <- tpbudget::budget_group(df = exp_df, colsgrp = c("fund", "department")) %>%
    tpbudget::calc_yearcol(curr_fy = fy)


  yrs_too_early_div <- tpbudget::find_yrstooearly(fy = fy, bud_extract_df = grpdf, "recent")

  grpdf %>%
    dplyr::filter(department_num == dept_code) %>%
    dplyr::select(-yrs_too_early_div)
}


#' Generate Division Expenditures Overall dataframe
#'
#' Prepares dataframe on division expenditures overall for visualization in table
#'
#' @param divcode Division code
#' @param exp_df Base process Tyler expenditures dataframe
#' @param fy Fiscal year preparing budget book for; default 2023
#'
#' @return Dataframe with division expenditures overall
#' @export
#'
#' @examples
div_overall <- function(divcode, exp_df, fy = 2023){

  grpdf <- tpbudget::budget_group(df = exp_df, colsgrp = c("department", "division")) %>%
    tpbudget::calc_yearcol(curr_fy = fy)

  yrs_too_early_div <- tpbudget::find_yrstooearly(fy = fy, bud_extract_df = grpdf, "recent")

  grpdf %>%
    dplyr::filter(division_num == divcode) %>%
    dplyr::select(-yrs_too_early_div)
}


#' Generate Department Expenditures by Personnel or Operating Dataframe
#'
#' Prepares dataframe of Departmental expenditures by operating/personnel expenditures for visualization in table
#'
#' @param dept_code Department code
#' @param exp_df Base process Tyler expenditures dataframe
#' @param fy Fiscal year preparing budget book for; default 2023
#'
#' @return Dataframe of departmental expenditures by personnel/operating expenditures for visualization in tables
#' @export
#'
#' @examples
gen_dept_summary_perstype <- function(dept_code, exp_df, fy){

  grpdf <- tpbudget::budget_group(df = exp_df, colsgrp = c("fund", "department", "perstype", "budget_cat_overall")) %>%
    tpbudget::calc_yearcol(curr_fy = fy)

  yrs_too_early_deptperstype <- tpbudget::find_yrstooearly(fy = fy, bud_extract_df = grpdf, "recent")

  grpdf %>%
    dplyr::filter(department_num == dept_code) %>%
    tpbudget::factorize_desc("budget_cat_overall") %>%
    dplyr::select(-yrs_too_early_deptperstype)
}



#' Generate Division Expenditures by Personnel/Operating
#'
#' Prepares dataframe of division expenditures by personnel/operating type for visualization in tables
#'
#' @param divcode Division Code
#' @param exp_df Base process Tyler expenditures dataframe
#' @param fy Fiscal year preparing budget book for; default 2023
#'
#' @return Dataframe of division expenses by personnel/operating
#' @export
#'
#' @examples
div_perstype <- function(divcode, exp_df, fy = 2023){

  grpdf <- tpbudget::budget_group(df = exp_df, colsgrp = c("fund", "department", "division", "perstype", "budget_cat_overall")) %>%
    tpbudget::calc_yearcol(curr_fy = fy)

  yrs_too_early_div <- tpbudget::find_yrstooearly(fy = fy, bud_extract_df = grpdf, "recent")

  df <- grpdf %>%
    dplyr::filter(division_num == divcode) %>%
    dplyr::select(-yrs_too_early_div) %>%
    tpbudget::factorize_desc("budget_cat_overall")

  df
}



