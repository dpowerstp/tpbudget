## code to prepare `tyler_funds` dataset goes here

tyler_funds <- data.frame(
  fund_num =
    c("0001", paste0("00", 1:7, "0")),
  fund_desc =
    c("General Fund",
      "Special Revenue Fund",
      "Rehabilitation Loan Fund",
      "Stormwater Management Fund",
      "Debt Service Accounts",
      "Community Center Fund (Construction)",
      "Speed Camera Fund",
      "ARPA-Federal Fund"),
  fund_acronym =
    c("GF",
      "SRF",
      "Rehab Loan",
      "SW",
      "",
      "CCF",
      "SCF",
      "ARPAF")
)

usethis::use_data(tyler_funds, overwrite = TRUE)
