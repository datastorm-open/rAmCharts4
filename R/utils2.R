#' Add dataFields information
#'
#' @param value A \code{character} name of value field.
#' @param category A \code{character} name of category field.
#' @param valueY A \code{character} name of Y field value.
#' @param categoryX A \code{character} name of categoryX field.
#' @param ... Arguments defined in \url{https://www.amcharts.com/docs/v4/}.
#'
#' @return A \code{list}.
#' @export
dataFields <- function(value = NULL, category = NULL,
                       valueY = NULL, categoryX = NULL,
                       ...){
  dropNulls(list(
    "value" = value,
    "category" = category,
    "valueY" = valueY,
    "categoryX" = categoryX,
    ... = ...))
}


