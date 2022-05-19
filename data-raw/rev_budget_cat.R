## code to prepare `rev_budget_cat` dataset goes here


# list of revenue codes corresponding to each department
rev_budget_cat <- list(
  "1000 General Government" =
    list("Investment Earnings" =  c("36100", "36110", "36120", "36130", "36140", "36150"),
         "Passport Services" = 36870),
  "2000 Police" =
    list("Police Protection (state)" = 33100,
         "Police Rebate" = 33230,
         "In Lieu of Police" = 33250,
         "In Lieu of Crossing Guard" = 33290,
         "Summons & Forfeitures"=35200,
         "Public Parking Facilities" = 34400,
         "Parking Permits"= 32600,
         "Admin. Fees-parking" = 35100,
         "Municipal Infractions" = 35300),
  "3000 Public Works" = c(
    "Highway" = 31200,
    "In Lieu Of Roads Maint." = 33260,
    "Waste Coll.& Disp.charges" = 34500,
    "Recyclable Sales" = 36820,
    "Mulch Sales" = 36850,
    "Special Trash Pickup" = 36890,
    "Driveway Permits" = 32200,
    "Tree Permits" = 32500,
    "Tree Fund" = 36160),
  "4000 Recreation" =
    list(
      "Takoma/Langley Rec. Agmt." = 33330,
      "Program Service Charges" = c(
        "34600",
        "34610",
        34620,
        34640,
        34650,
        34660,
        34670,
        34680,
        34690,
        34710,
        34720,
        34730,
        34740
      )
    ),
  "5000 Community Development" =
    list(
      "Commerical Inspections" = 34210,
      "Inspection Fees" = 34200
    ),
  "6000 Communication / Media" =
    list(
      "Cable Franchise Fees" = 33500,
      "Cable-Operating" = 33510
    ),
  "7000 Library" = c(
    "Library Aid" = 33210,
    "Library Fines & Fees" = 34700
  )
)


# turn list into df
rev_budget_categories <- purrr::map2_dfr(rev_budget_cat, names(rev_budget_cat), function(deptlist, deptname){

  dept_df <- purrr::map2_dfr(deptlist, names(deptlist), function(accountnumvec, budgetcategory){

    # browser()
    data.frame("accountnum_num" = as.character(accountnumvec),
               "budget_cat_overall_desc" = rep(budgetcategory, length(accountnumvec)))
  })

  dept_df %>%
    mutate(department = deptname)

})

usethis::use_data(rev_budget_categories, overwrite = TRUE)
