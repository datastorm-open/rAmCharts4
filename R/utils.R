# dropNulls
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}


`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

#' Utility function to create rAmCharts4 parameters JSON
#'
#' @param x A \code{rAmCharts4} \code{htmlwidget} object.
#' @param name Slot's name to edit
#' @param ... Arguments for the slot
#'
#' @return A \code{rAmCharts4} \code{htmlwidget} object.
#'
#' @importFrom utils modifyList
#'
#' @noRd
.merge_opt <- function(x, name, ...) {

  if(!any(class(x) %in% c("rAmCharts4", "amCharts4Proxy"))){
    stop("x must be a rAmCharts4 or a amCharts4Proxy object")
  }

  if (is.null(x$x$opts[[name]])) {
    x$x$opts[[name]] <- list(...)
  } else {
    x$x$opts[[name]] <- utils::modifyList(x = x$x$opts[[name]], val = list(...), keep.null = TRUE)
  }

  return(x)
}

#' Utility function to create rAmCharts4 parameters JSON
#'
#' @param x A \code{rAmCharts4} \code{htmlwidget} object.
#' @param name Slot's name to edit
#' @param l List of arguments for the slot
#'
#' @return A \code{rAmCharts4} \code{htmlwidget} object.
#'
#' @noRd
.merge_opt2 <- function(x, name, l) {

  if (is.null(x$x$opts[[name]])) {
    x$x$opts[[name]] <- l
  } else {
    x$x$opts[[name]] <- utils::modifyList(x = x$x$opts[[name]], val = l, keep.null = TRUE)
  }

  return(x)
}

#' Add data parameters
#'
#' @param x A \code{rAmCharts4} \code{htmlwidget} object.
#' @param ... Arguments defined in \url{https://www.amcharts.com/docs/v4/}.
#'
#' @return A \code{rAmCharts4} \code{htmlwidget} object.
#' @export
#'
#' @name amc_data
amc_data <- function(x, data) {
  x$x$opts$data <- data
  x
}

#' Add series parameters
#'
#' @param x A \code{rAmCharts4} \code{htmlwidget} object.
#' @param type \code{character} type of granh, can be PieChart, XYChart,
#'  RadarChart, SankeyDiagram, TreeMap, ChordDiagram, SlicedChart...
#' @param stroke A \code{character} color, hexa.
#' @param strokeWidth A \code{interger} size.
#' @param ... Arguments defined in \url{https://www.amcharts.com/docs/v4/}.
#'
#' @return A \code{rAmCharts4} \code{htmlwidget} object.
#' @export
#'
#' @name amc_series
amc_series <- function(x, type = "PieChart", ...) {

  if(is.null(x$x$opts$series)){
    x$x$opts$series <- list(list(...))
  } else {
    x$x$opts$series[[length(x$x$opts$series)]] <- list(...)
  }
  x
}

#' Add xAxes parameters
#'
#' @param x A \code{rAmCharts4} \code{htmlwidget} object.
#' @param ... Arguments defined in \url{https://www.amcharts.com/docs/v4/}.
#'
#' @return A \code{rAmCharts4} \code{htmlwidget} object.
#' @export
#'
#' @name amc_xAxes
amc_xAxes <- function(x, ...) {

  if(is.null(x$x$opts$xAxes)){
    x$x$opts$xAxes <- list(list(...))
  } else {
    x$x$opts$xAxes[[length(x$x$opts$xAxes)]] <- list(...)
  }
  x
}

#' Add yAxes parameters
#'
#' @param x A \code{rAmCharts4} \code{htmlwidget} object.
#' @param ... Arguments defined in \url{https://www.amcharts.com/docs/v4/}.
#'
#' @return A \code{rAmCharts4} \code{htmlwidget} object.
#' @export
#'
#' @name amc_yAxes
amc_yAxes <- function(x, ...) {

  if(is.null(x$x$opts$yAxes)){
    x$x$opts$yAxes <- list(list(...))
  } else {
    x$x$opts$yAxes[[length(x$x$opts$yAxes)]] <- list(...)
  }
  x
}

#' Add legend parameters
#'
#' @param x A \code{rAmCharts4} \code{htmlwidget} object.
#' @param ... Arguments defined in \url{https://www.amcharts.com/docs/v4/}.
#'
#' @return A \code{rAmCharts4} \code{htmlwidget} object.
#' @export
#'
#' @name amc_legend
amc_legend <- function(x, ...) {

  .merge_opt(x, "legend", ...)

}

#' Add responsive parameters
#'
#' @param x A \code{rAmCharts4} \code{htmlwidget} object.
#' @param ... Arguments defined in \url{https://www.amcharts.com/docs/v4/}.
#'
#' @return A \code{rAmCharts4} \code{htmlwidget} object.
#' @export
#'
#' @name amc_responsive
amc_responsive <- function(x, ...) {

  .merge_opt(x, "responsive", ...)

}

#' Add cursor parameters
#'
#' @param x A \code{rAmCharts4} \code{htmlwidget} object.
#' @param ... Arguments defined in \url{https://www.amcharts.com/docs/v4/}.
#'
#' @return A \code{rAmCharts4} \code{htmlwidget} object.
#' @export
#'
#' @name amc_cursor
amc_cursor <- function(x, ...) {

  .merge_opt(x, "cursor", ...)

}
