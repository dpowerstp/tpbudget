##### functions for creating flextables of performance measure tables

#' Rename Performance Table Columns
#'
#' Renames columns in standard department performance table Excel file
#'
#' @param perfdata Performance table dataframe
#'
#' @return Performance dataframe with renamed columns
#' @export
#'
#' @examples
perf_rename <- function(perfdata) {
  perfdata %>%
    dplyr::rename_with(.fn = ~ "Measurement", .cols = 1) %>%
    dplyr::rename_with(
      .fn = ~ dplyr::case_when(
        grepl("2[0-1]$", .x) ~ paste0("Actual ", .x),
        grepl("22$", .x) ~ paste0("Estimated ", .x),
        grepl("23$", .x) ~ paste0("Projected ", .x),
        T ~ .x
      ))
}




#' Transform Numeric Performance Measures
#'
#' Formats numeric performance measure columns for representation in tables.
#'
#' @param df Performance measure dataframe.
#' @param colname Name of performance measure column to format as number; string.
#' @param .numdec Number of decimal places; default 2.
#' @param .percsub Default NULL. Perf_read looks for the word "percent" in the measurement name to determine whether to round the row to 2 decimal points and multiply it by 100. In some cases "percent" does not appear in the measurement name for percentage-based metrics. In these cases, .percsub accepts a logical vector of T/F corresponding to the number of rows in the performance table, with true indicating it is a performance measure and F indicating it is not.
#'
#' @return Formatted dataframe with formatted performance measure column
#' @export
#'
#' @examples
transform_num <- function(df, colname, .numdec = 2, .percsub = NULL){

  df <- df %>%
    dplyr::mutate(percrow = grepl("percent", Measurement, ignore.case = T))

  if (!is.null(.percsub)){

    if (!is.logical(.percsub) | length(.percsub) != nrow(df)){
      stop(".percsub must be a logical vector of equal legnth to the number of rows in the performance table")
    }

    df[["percrow"]] <- .percsub
  }

  # format percentage cols as percentages
  df <- df %>%
    dplyr::mutate(dplyr::across(.fn = ~ as.character(.x))) %>%
    dplyr::mutate(newcol := as.numeric(!!dplyr::sym(colname)),
                  newcol = dplyr::case_when(percrow == T ~ newcol * 100,
                                            T ~ newcol))

  # if newcolumn created by function contains values - comma separate, and add dollar signs/percentage signs, and replace NA
  if (!all(is.na(df[["newcol"]]))){

    df <- df %>%
      dplyr::mutate(newcol = round(newcol, .numdec) %>%
                      format(., big.mark = ","),
                    newcol = as.character(newcol))

    df <- df %>%
      dplyr::mutate(newcol = gsub(pattern = "\\.0{1,}$", "", newcol, perl = T),
                    newcol = dplyr::case_when(percrow == T & !grepl("NA", newcol) ~
                                                # gsub("0{1,}$", "", newcol) %>%
                                                paste0(newcol, "%"),
                                              T ~ newcol),
                    newcol = trimws(newcol))

    # browser()
    df <- df %>%
      dplyr::mutate(!!dplyr::sym(colname) := dplyr::case_when(newcol != "NA" ~ newcol,
                                                              T ~ !!dplyr::sym(colname)))
  }

  df %>%
    dplyr::select(-c(percrow, newcol))
}


#' Read Performance Data
#'
#' Reads and formats standard Departmental performance tables. Assumes performance tables saved in Workbook organized by Department titled "perf_departmentnumber.xlsx", and that each sheet of the table corresponds to one division.
#'
#' @param dept Department number of Department reading performance data on.
#' @param sheet Name of sheet containing performance data reading in.
#' @param .numdec Number of decimal places; default 2.
#' @param .percsub Default NULL. Perf_read looks for the word "percent" in the measurement name to determine whether to round the row to 2 decimal points and multiply it by 100. In some cases "percent" does not appear in the measurement name for percentage-based metrics. In these cases, .percsub accepts a logical vector of T/F corresponding to the number of rows in the performance table, with true indicating it is a performance measure and F indicating it is not.
#' @param dirpath Directory performance file is stored in.
#'
#' @return Performance table formatted for visualization.
#' @export
#'
#' @examples
perf_read <- function(dept, sheet, .numdec = 2, .percsub = NULL, dirpath = "./data/performance/"){

  # read in performance table and rename
  df <- openxlsx::read.xlsx(glue::glue("{dirpath}perf_{dept}.xlsx"), sheet = sheet) %>%
    tpbudget::perf_rename

  # apply number transform to all columns
  purrr::walk(colnames(df), ~ {
    df <<- tpbudget::transform_num(df, .x, .numdec = .numdec, .percsub = .percsub)
  })

  df

}


#' Performance Data Flextable
#'
#' Create flextable out of performance measure dataframe. Ideally, should format performance measure dataframe with perf_read() beforehand.
#'
#' @param df Performance measure dataframe
#'
#' @return Flextable of performance measure dataframe
#' @export
#'
#' @examples
flex_performancedata <- function(df){
  flextable::flextable(df) %>%
    flextable::border_inner(border = officer::fp_border(color = "gray", width = 0.5)) %>%
    flextable::border_outer(border = officer::fp_border(color = "gray", width = 0.5)) %>%
    flextable::font(fontname = "arial") %>%
    flextable::italic(part = "all") %>%
    flextable::bg(j = 1, bg = "#f2f2f2") %>%
    flextable::bold(i = 1, part = "header") %>%
    flextable::bg(part = "header", bg = "#004990") %>%
    flextable::color(part = "header", color = "white") %>%
    flextable::fontsize(size = 9, part = "all") %>%
    flextable::autofit() %>%
    flextable::width(width = 1.75, j = 1) %>%
    flextable::fit_to_width(max_width = 10)

}


