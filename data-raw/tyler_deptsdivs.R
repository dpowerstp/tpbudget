## code to prepare `tyler_deptsdivs` dataset goes here

# departments and divisions in tlyer
tyler_deptsdivs <- data.table::data.table(
  div_num = c(
    paste0("11", 1:7, "0"),
    paste0("2", 1:6, "00"),
    paste0("3", 1:9, "00"),
    paste0("4", 1:8, "00"),
    paste0("5", 4:8, "00"),
    "6000",
    "7000",
    "7200",
    "8000",
    "9000",
    "9100",
    "9200"
  ),
  div_name = c(
    "Legislative Division",
    "City Manager's Office",
    "Finance Division",
    "Legal Division",
    "Information Technology",
    "Human Resources",
    "City Clerk",
    "Office of Chief",
    "Communications",
    "Operation/Patrol",
    "Criminal Investigations",
    "Administration Svc",
    "Neighborhood Svc",
    "Administration",
    "Building Maintenance",
    "Equipment Maintenance",
    "Right of Way",
    "Solid Waste",
    "Sustainable",
    "Vegetation Management",
    "Urban Forest",
    "City Engineer",
    "Administration",
    "Youth Outreach",
    "Recreation Center",
    "Community Programs",
    "Athletic Fields/ Facilities",
    "Camps",
    "Before/After School Prog",
    "Community Center",
    "Planning & Development",
    "HCD Administration",
    "Economic Development",
    "Arts & Humanities",
    "Housing & Comm Service",
    "Media/Communications",
    "Library",
    "Computer Learning Center",
    "Debt Service",
    "General Government-Non-departmental",
    "Capital Expenditures",
    "General Fund Transfers to other funds"
  )
) %>% dplyr::mutate(
  dept_num = dplyr::case_when(
    substr(div_num, 1, 1) < 9 ~ paste0(substr(div_num, 1, 1), "000"),
    T ~ div_num
  ),
  dept_name = dplyr::case_when(
    dept_num == "1000" ~ "General Government",
    dept_num == "2000" ~ "Police",
    dept_num == "3000" ~ "Public Works",
    dept_num == "4000" ~ "Recreation",
    dept_num == "5000" ~ "Housing & Community Development",
    dept_num == "6000" ~ "Communications/Media",
    dept_num == "7000" ~ "Library",
    dept_num == "8000" ~ "Debt Service",
    dept_num == "9000" ~ "General Government Non-departmental",
    dept_num == "9100" ~ "Capital Expenditures",
    dept_num == "9200" ~ "General Fund Transfers to other funds"
  )
)

usethis::use_data(tyler_deptsdivs, overwrite = TRUE)
