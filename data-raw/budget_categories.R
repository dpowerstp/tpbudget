## code to prepare `budget_categories` dataset goes here

# assign codes to budget categories based on 2021 PDF from finance
budget_categories <- list(
  "Wages" = list("40" = c("")),
  "Fringe Benefits" = list("41" = c("")),
  "Overtime" = list("42" = c("")),
  "Employee Recognition" = list("43" = c("")),
  "Contractual Labor" = list("44" = c("")),
  "Supplies" = list("50" = c(
    "51000", "52000", "52005", "52007", "52100", "52250", "52700", "52350", "52450", "52510", "52550", "52600", "52650", "52800", "53100", "53250", "53300", "53460", "53500", "54050", "55000", "56000")),
  "Uniforms" = list("51" = c(
    52150, 52300
  )),
  "Computer Expenditures" = list("52" = c("52006", "52900")),
  "Vehicle Fuel" = list("53" = c(52400, 52410)),
  "Repairs & Maintenance" = list("54" = c(53050, 53060, 53080, 53090, 53150, 53350, 53400)),
  "Services and Charges" = list("60" = c(
    "61001", "61005",  "61015", "61044", "61020", "61021", "61022", "61023", "61025", "61026", "61027",
    "61028", "61029", "61039", "61040", "61042", "61045", "61049", "61050", "62010", "62020", "63000", "63005", "64005", "64010",
    "64011", "64015", "65005", "65010", "65015", "65020", "66005", "66010", "66015", "66016", "66020", "66025", "66030", "68005",
    "68011", "69100", "69150", "69160", "69200")),
  "Communications" = list("61" = c("62005", "62006", "62007", "62008", "62015", "68010")),
  "Utilities" = list("62" = c("67110", "67130", "67140", "67200", "67300")),
  "Office Expenditures" = list("70" = c("70001", "70005", "70009", "70010", "1110-70015", "71000", "71450")),
  "Conferences, Training, & Dues" = list("71" = c("70017", "70020", "70025", "70035", "70050", "70051", "70053", "70054", "70056", "70060", "70065", "70075", "70085", "70086", "70095", "70096", "73500")),
  "Recruitment" = list("72" = c("70090")),
  "Special Events & Programs" = list("73" = c(70002, 70006, 70007, 70013, 70014, 70015, 70016, 70018, 70019, 70040, 70052, 71004, 71005, 71006, 71007, 71009, 71100, 71110, 71150, 71200, 71500, 71600, 71650, 71700, 72000, 73750, "5400-70015")))

# quick function to turn list into dataframe
create_df <- function(list_pos, budget_list){
  # print(list_pos)
  val <- budget_list[[list_pos]][[1]]

  df <- data.table::data.table(
    budget_cat_overall_num = names(budget_list[[list_pos]]),
    budget_cat_overall_desc = names(budget_list[list_pos]),
    accountnum_num = as.character(val)
  )

  df

}

budget_cat_df <- purrr::map_dfr(1:length(budget_categories), ~ create_df(.x, budget_categories)) %>%
  dplyr::mutate(budget_cat_overall = paste0(budget_cat_overall_num, " ", budget_cat_overall_desc))

usethis::use_data(budget_cat_df, overwrite = TRUE)
