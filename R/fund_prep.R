#' Generate Combined Statement of Revenue
#'
#' Generates dataframe for "Combined Statement of Revenue, Expenditures, & Change in Fund Balances - All Governmental Funds" table in budget book.
#'
#' @param df_rev Base-processed budget revenue data.
#' @param df_exp Base-processed budget expenditure data.
#'
#' @return Combined dataframe of fund-summarized revenues and expenditures.
#' @export
#'
#' @examples
df_rev_exp_chngfund <- function(df_rev, df_exp){

  yrcols <- tpbudget::pull_year_cols(df_rev)
  # browser()
  if (!all(yrcols == pull_year_cols(df_exp))){
    stop("Error; mismatch in year columns between dataframes")
  }

  # calculate total revenue by account type and fund
  fy_rev <- df_rev %>%
    dplyr::group_by(account.type, fund) %>%
    dplyr::summarise(dplyr::across(.cols = yrcols, .fns = ~ round(sum(.x, na.rm = T), digits = 2)))

  # calculate total expenditures by account type and fnd
  fy_exp <- df_exp %>%
    dplyr::group_by(account.type, fund) %>%
    dplyr::summarise(dplyr::across(.cols = yrcols, .fns = ~ round(sum(.x, na.rm = T), digits = 2)))


  # bind two together
  rbind(fy_rev, fy_exp)

}

#' Calculate Overall Revenue
#'
#' Calculates overall revenue in budget-revenue dataset and change by year
#'
#' @param df_rev Revenue dataframe
#' @param pivot Whether to pivot dataframe into longer format
#'
#' @return Grouped revenue dataframe
#' @export
#'
#' @examples
df_budg_rec_table <- function(df_rev, pivot = F){
  yrcols <- tpbudget::pull_year_cols(df_rev)

  grped_df <- df_rev %>%
    dplyr::group_by() %>%
    dplyr::summarize(dplyr::across(.cols = yrcols, .fns = ~ round(sum(.x, na.rm = T), digits = 2))) %>%
    tpbudget::calc_yearcol()

  if (pivot){
    grped_df <- grped_df %>%
      tidyr::pivot_longer(cols = colnames(grped_df))
  }
  grped_df

}



#' Title
#'
#' function to generate base df for changes to proposed/adopted budget table
#'
#' @param df_exp Base-processed expendiures dataset
#' @param fund_ac Fund-acronym to filter dataframe to; default "GF" for general fund
#' @param .currfy Fiscal-year for the concluding budget (not the fiscal year for the budget being prepared); default 2022
#' @param filternot0 Whether to filter dataframe to rows where the difference between the adopted and proposed budget is not 0; default false
#'
#' @return Dataframe summarized to fund-level expenditures and revenues
#' @export
#'
#' @examples
df_propadpt_chng <- function(df_exp, fund_ac = "GF", filternot0 = F, .currfy = 2022){

  yrcols <- pull_year_cols(df_exp)

  # pull funds in dataset
  uq_funds <- unique(df_exp[["fund_acronym"]])

  if (!fund_ac %in% uq_funds){
    stop(glue::glue("Selected fund type acronym not found in base expenditures dataframe. Fund type must be one of {uq_funds} for this dataframe"))
  }

  processed <- df_exp %>%
    dplyr::filter(fund_acronym == fund_ac) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(fund_desc, department_desc, account.name) %>%
    dplyr::summarise(dplyr::across(.cols = yrcols, .fns = ~ round(sum(.x, na.rm = T), digits = 2))) %>%
    tpbudget::calc_yearcol(curr_fy = .currfy)
  # filter to changes

  if (filternot0){

    yrdiff <- glue::glue("diff_adpt{.currfy}_prop{.currfy}_tot")

    processed <- processed %>%
      dplyr::filter(!!dplyr::sym(yrdiff) != 0)
  }

  processed

}


#' Filter fund
#'
#' Filters dataframe to given fund
#'
#' @param df Dataframe
#' @param fundac Acronym of fund filtering dataframe to
#' @param fundaccol Data-masked name of column filtering containing fund acronyms; default fund_acronym
#'
#' @return Dataframe filtered to one fund
#' @export
#'
#' @examples
filter_fund <- function(df, fundac, fundaccol = fund_acronym){
  df %>%
    dplyr::filter({{fundaccol}} == fundac)
}



#' All Operating Funds Dataframe
#'
#' Function to make operating dataframe for visualization of all operating funds, merging budget and revenue data into one file
#'
#' @param df_rev Base-revenue dataframe; bud_extract_rev
#' @param df_exp Base-expenditures dataframe; bud_extract_dept
#' @param .fundcol Column corresponding to recoded fund values; default fund_recode
#'
#' @return
#' @export
#'
#' @examples
df_all_op_funds <- function(df_rev, df_exp, .fundcol = fund_recode){
  yrcols <- tpbudget::pull_year_cols(df_rev)

  if (!all(yrcols == tpbudget::pull_year_cols(df_exp))){
    stop("Error; mismatch in year columns between dataframes")
  }

  op_fund_rev <- df_rev %>%
    tpbudget::factorize_desc("category") %>%
    dplyr::group_by({{.fundcol}}, account.type, category_desc) %>%
    dplyr::summarise(dplyr::across(.cols = yrcols, .fns = ~ round(sum(.x, na.rm = T), digits = 2)))

  op_fund_expense <- df_exp %>%
    tpbudget::factorize_desc(basename = "department") %>%
    dplyr::group_by({{.fundcol}}, account.type, department_desc) %>%
    dplyr::summarise(dplyr::across(.cols = yrcols, .fns = ~ round(sum(.x, na.rm = T), digits = 2))) %>%
    dplyr::rename(category_desc = department_desc)

  op_fund_combo <- rbind(op_fund_rev, op_fund_expense) %>%
    tpbudget::calc_yearcol()

}


#' All Operating Funds Table
#'
#' Generate all operating funds dataframe with combined revenues and expenditures for a fund-filtered to
#'
#' @param fundac Acronym of fund to filter data to.
#' @param fundcol Data-masked name of fund column to supply df_all_op_funds function; default fund_recode
#' @param rev_df Base-processed revenue dataframe
#' @param exp_df Base-processed expenditures dataframe
#' @param fy Fiscal year of this budget book; default 2023
#' @param ... Optional additional arguments to find_yrstooearly, filter_fund, and revenue_factorize functions.
#'
#' @return Dataframe of all operating funds with revenue and expenditures in one dataframe, filtered to one fund
#' @export
#'
#' @examples
df_fundtable <- function(rev_df, exp_df, fundac, fy = 2023, fundcol = fund_recode, ...){

  yrs_too_early <- tpbudget::find_yrstooearly(fy = fy, exp_df, tbltype = "operating")

  tpbudget::df_all_op_funds(df_rev = tpbudget::filter_fund(exp_df, fundac = fundac, ...),
                           df_exp = tpbudget::filter_fund(exp_df, fundac = fundac, ...),
                           .fundcol = {{ fundcol}}) %>%
    dplyr::ungroup %>%
    dplyr::select(-yrs_too_early) %>%
    tpbudget::revenue_factorize(...)
}



