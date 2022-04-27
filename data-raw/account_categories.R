## code to prepare `account_categories` dataset goes here - unique list of account names and their budget categories from FY 2019 to 2023

# recode account categories based on categorizations in previous year budget books from finance
recode_account_category <- function(string){
  qg <- function(matchstring){
    string == matchstring
  }

  case_when(
    qg("40011") ~ "Wages",
    qg("40016") ~ "Wages",
    qg("40031") ~ "Overtime--Training",
    qg("40032") ~ "Overtime--Holiday",
    qg("40034") ~ "Overtime--Court",
    qg("40035") ~ "Overtime--Staff Shortage",
    qg("40180") ~ "K-9 Allowance",
    qg("40190") ~ "Car & Clothing Allowance",
    qg("40191") ~ "Car & Clothing Allowance",
    qg("40540") ~ "Employee Recognition",
    qg("40600") ~ "Workers Compensation",
    qg("52250") ~ "Supplies",
    qg("52900") ~ "Computer Expenditures",
    qg("53400") ~ "Repairs & Maintenance",
    qg("61050") ~ "Services and Charges",
    qg("62015") ~ "Communications",
    qg("66025") ~ "Services and Charges",
    qg("70006") ~ "Council Priorities",
    qg("70007") ~ "Special Events & Programs",
    qg("70010") ~ "General Contigency",
    qg("70090") ~ "Recruitment",
    qg("71004") ~ "Special Events & Programs",
    qg("71650") ~ "Office Expenditures",
    qg("56000") ~ "Supplies",
    qg("70011") ~ "Covid-19 Emergency Assistance Fund",
    qg("70019") ~ "Community Festival & 4th of July",
    qg("71350") ~ "Office Expenditures",
    qg("71600") ~ "Partnership Program",
    qg("72000") ~ "Housing Expenditure",
    qg("73750") ~ "Tax Rebate & Supplemental Tax",
    qg("80001") ~ "Capital Outlay",
    T ~ NA_character_

    # qg("") ~ "Services and Charges"
  )
}

# using account names finance categorized (tpbudget::finance_categories) - standardize account names and categories, and trim whitespace
fin_cat <- tpbudget::finance_categories %>%
  dplyr::mutate(accountnum_desc = trimws(accountnum_desc),
                accountnum_desc = dplyr::case_when(
                  accountnum_desc == "Office Suplies" ~ "Office Supplies",
                  accountnum_desc == "Recognition Non Cash" ~ "Recognition-Noncash",
                  accountnum_desc == "Refuse Disp.fees (t.f.)" ~ "Refuse Disposal Service",
                  accountnum_desc == "City Code-Contract" ~ "City Code-contract",
                  accountnum_desc == "Labor/Employment Services" ~ "Labor/employment Services",
                  accountnum_desc == "Conference" ~ "Conferences & Conventions",
                  accountnum_desc == "Intern Salaries" ~ "Intern Salary",
                  accountnum_desc == "Media Printing" ~ "Media",
                  accountnum_desc == "Refuse Disposal Service" ~ "Refuse Disp.fees (t.f.)",
                  accountnum_desc == "Write off /Bad Debt Expense" ~ "Bad Debt Expense",
                  accountnum_desc == "Emissions Tests/Inspections" ~ "Emissions Tests/Inspectio",
                  accountnum_desc == "Computer Inventory Exp" ~ "Computer Supplies",
                  grepl("Salaries.*part", accountnum_desc, ignore.case = T) ~ "Salaries-part Time",
                  T ~ accountnum_desc
                ),
                budget_cat_overall_desc = trimws(budget_cat_overall_desc),
                budget_cat_overall_desc = dplyr::case_when(
                  grepl("Contracts", accountnum_desc, ignore.case = T) ~ "Services and Charges",
                  budget_cat_overall_desc == "Communication" ~ "Communications",
                  budget_cat_overall_desc == "Overtime -Holiday" ~ 'Overtime--Holiday',
                  budget_cat_overall_desc == "Overtime -Court" ~ "Overtime--Court",
                  budget_cat_overall_desc == "Overtime -Training" ~ "Overtime--Training",
                  budget_cat_overall_desc == "Special Events & Programs:" ~ "Special Events & Programs",
                  budget_cat_overall_desc == "Car and Clothing Allowances" ~ "Car & Clothing Allowance",
                  # grepl("Dues & Other", budget_cat_overall_desc) ~ "Conferences, Trainings, Dues & Other",
                  grepl("(Conferences, Training)|(Conferenecs)", budget_cat_overall_desc) ~ "Conferences, Training, & Dues",
                  grepl("Repairs and Mainten", budget_cat_overall_desc) ~ "Repairs & Maintenance",
                  grepl("Suppllies and Charges", budget_cat_overall_desc) ~ "Supplies",
                  grepl("Copying", accountnum_desc) ~ "Supplies",
                  grepl("Media", accountnum_desc) ~ "Services and Charges",
                  T ~ budget_cat_overall_desc
                )) %>%
  dplyr::arrange(budget_cat_overall_desc, accountnum_desc) %>%
  dplyr::distinct()

## use sample budget data from previous years to get unique list of account names and departments associated with - from 2019 to 2023
# bud_extract_dept_prelim <- readRDS("./bud_extract_dept_prelim.rds")

# yrs_inc <- grep("(_201[5-8])|(cert)|(ytd)", pull_year_cols(bud_extract_dept_prelim), invert = T, value = T)

# # select all account numbers, names, and their departments/divisions
# accounts_2019_2023 <-bud_extract_dept_prelim %>%
#   select(account.name, accountnum, accountnum_desc, accountnum_num, department, division, all_of(yrs_inc)) %>%
#   distinct() %>%
#   select(-yrs_inc) %>%
#   mutate(accountnum_desc_small = tolower(accountnum_desc))

# usethis::use_data(accounts_2019_2023)

# join accounts to finance's data by account name - joindesc check whether join worked
fin_cat_join_data <- fin_cat %>%
  dplyr::mutate(accountnum_desc_small = tolower(accountnum_desc)) %>%
  dplyr::rename(accountnum_ron = accountnum_desc) %>%
  dplyr::full_join(accountinfo) %>%
  dplyr::mutate(joindesc = accountnum_desc == accountnum_ron) %>%
  dplyr::select(budget_cat_overall_desc, account.name, accountnum_desc, accountnum_ron, accountnum_desc_small, joindesc, accountnum, accountnum_num, department, division) %>%
  arrange(accountnum_ron)

# recode budget categories based on account number if didn't get assigned - keep account number as category if didn't recode
fin_cat_join_data_recode <- fin_cat_join_data %>%
  dplyr::mutate(budget_cat_overall_desc = dplyr::case_when(is.na(budget_cat_overall_desc) ~ recode_account_category(accountnum_num),
                                                           T ~ budget_cat_overall_desc),
                budget_cat_overall_desc = dplyr::case_when(is.na(budget_cat_overall_desc) ~ accountnum_desc,
                                                           T ~ budget_cat_overall_desc))


# gather divisions and departments into one place for each account
fin_cat_join_new <-fin_cat_join_data_recode %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(names_from = c(department), values_from = c(department)) %>%
  tidyr::unite(col = "departments", grep("[0-9]{4}", colnames(.), value = F), sep = ", ", na.rm = T, remove = T) %>%
  tidyr::pivot_wider(names_from = c(division), values_from = c(division), names_repair = "unique") %>%
  tidyr::unite(col = "divisions", grep("[0-9]{4}", colnames(.), value = F), sep = ", ", na.rm = T, remove = T) %>%
  dplyr::distinct() %>%
  dplyr::select(-NA...10, -NA...41)

# identify just list of accounts matched up against categories
account_categories <- fin_cat_join_new %>%
  select(account.name, accountnum, accountnum_num, budget_cat_overall_desc) %>%
  distinct() %>%
  arrange(budget_cat_overall_desc) %>%
  filter(!is.na(accountnum))

usethis::use_data(account_categories, overwrite = TRUE)
