#' Budget Book Pie Charts
#'
#' Create pie charts for budget book. Change the input dataframe (ideally, from the bud_list options) to change what's visualized.
#'
#' @param namecol Name of column to supply to label pie chart values. E.g., "accountnum" if pie chart is showing expenditures for all accounts. Input data-masked column name.
#' @param yrcol Year-column to visualize data for in pie chart as data-masked column.
#' @param df_fund Dataframe; e.g., bud_list$fund. Changing the
#'
#' @return
#' @export
#'
#' @examples
plot_bud_pies <- function(df_fund, namecol, yrcol) {

  quo_val <- rlang::enquo(yrcol)
  quo_name <- rlang::enquo(namecol)
  df <- df %>%
    dplyr::ungroup %>%
    dplyr::mutate(dplyr::across(.cols = where(is.numeric), abs)) %>%
    dplyr::filter({{yrcol}} != 0)

  plotly::plot_ly(data = df,
          type = "pie",
          values = quo_val,
          labels = quo_name,
          textinfo = "label+percent",
          textposition= "inside")
}


