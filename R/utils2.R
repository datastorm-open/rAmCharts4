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

#' Add series information
#'
#' @param type A \code{character} type of serie.
#' @param stroke A \code{character} color, hexa.
#' @param strokeWidth A \code{interger} size.
#' @param ... Arguments defined in \url{https://www.amcharts.com/docs/v4/}.
#'
#'
#' @examples
#' \dontrun{
#' amCharts4(data = data.frame(tag = c("My", "Best", "Word"),
#'                             weight = c(100, 200, 300)),
#'           series = series(type = "WordCloudSeries",
#'                           dataFields = dataFields(
#'                             word = "tag",
#'                             value = "weight")),
#'           type = "WordCloud")
#' }
#'
#' @return A \code{list}.
#' @export
series <- function(type = NULL, stroke = NULL,
               strokeWidth = NULL,
               ...){
  list(dropNulls(list(
    "type" = type,
    "stroke" = stroke,
    "strokeWidth" = strokeWidth,
    ... = ...)))
}



