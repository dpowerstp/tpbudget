
#' Standard knit settings
#' Sets default knitting options for RMarkdown budget documents
#'
#' @return Default chunk options for RMarkdown
#' @export
#'
#' @examples
standard_knitr <- function(){
  knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, ft.align = "left")
}
