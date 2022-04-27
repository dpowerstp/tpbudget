#' Budget accounts from 2019 to 2023
#'
#' Names and numbers used by each Department and Division for all accounts used in the FY 2019 to FY 2023 budget. Each row is a unique combination of the account.name, department, and division.
#'
#' @format A data frame with 1,042 rows and 7 variables:
#' \describe{
#'   \item{account.name}{Account name in Tyler}
#'   \item{accountnum}{Combined account number and account description; distinct from account.name in some instances}
#'   \item{accountnum_desc}{Description of account}
#'   \item{accountnum_num}{Number of account}
#'   \item{department}{A department that uses the account}
#'   \item{division}{A division that uses the account}
#'   \item{accountnum_desc_small}{Small-case accountnum_desc}
#' }
#' @source {Tyler budgeting system}
"accounts_2019_2023"


#' Budget categories documented by Finance in 2021
#'
#' Names and numbers of budget account categories used by finance department in FY 2021 budget, documented in PDF.
#'
#' @format A data frame with 147 rows and 4 variables:
#' \describe{
#'   \item{budget_cat_overall_num }{Account name in Tyler}
#'   \item{budget_cat_overall_desc}{Combined account number and account description; distinct from account.name in some instances}
#'   \item{accountnum_num}{An account number associated with }
#'   \item{budget_cat_overall}{A category used in the City's budget tables}
#' }
#' @source {Finance Department}
"budget_cat_df"


#' Budget categories documented by Finance in 2021
#'
#' Names and numbers of budget account categories used by finance department in FY 2021 budget, documented in PDF. Each row is a unique accountnum_num. Note: should use table_accounts_complete for full-association of budget categories with account names and numbers.
#'
#' @format A data frame with 147 rows and 4 variables:
#' \describe{
#'   \item{budget_cat_overall_num }{Number of budget category used in City tables. Some of these are currently unassigned}
#'   \item{budget_cat_overall_desc}{Description of budget category used in City tables}
#'   \item{accountnum_num}{Account number associated with budget categories}
#'   \item{budget_cat_overall}{Combination of name and number associated with budget categories used in City tables}
#' }
#' @source {Finance Department}
"budget_cat_df"


#' Department List
#'
#' List of all Departments and their Department-number in Tyler. Each row is a unique department.
#'
#' @format A data frame with 12 rows and 3 variables:
#' \describe{
#'   \item{department}{Department name and number}
#'   \item{department_desc}{Department description}
#'   \item{department_num}{Department number}
#' }
#' @source {Tyler budgeting system}
"dept_list"


#' Complete Table Accounts
#'
#' Dataframe of all budget accounts and their associated categories.
#'
#' @format A data frame with 331 rows and 6 variables:
#' \describe{
#'   \item{account.name }{Account name in Tyler}
#'   \item{accountnum }{Combined account number and account description; distinct from account.name in some instances}
#'   \item{accountnum_num }{An account number associated with a budget category}
#'   \item{budget_cat_overall}{A category used in the City's budget tables}
#'   \item{budget_cat_overall_desc}{Description of budget category used in City tables}
#'   \item{budget_cat_overall_num }{Number of budget category used in City tables}
#' }
#' @source {City staff}
"table_accounts_complete"



#' Division List
#'
#' List of all Divisions and their Division-number in Tyler. Each row is a unique division.
#'
#' @format A data frame with 49 rows and 3 variables:
#' \describe{
#'   \item{division}{division name and number}
#'   \item{division_desc}{division description}
#'   \item{division_num}{division number}
#' }
#' @source {Tyler budgeting system}
"div_list"

