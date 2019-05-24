#' @import htmlwidgets
#' @export
amCharts4 <- function(..., type, width = NULL, height = NULL) {

  x <- list(opts = list(...), type = type)

  # create the widget
  htmlwidgets::createWidget("rAmCharts4", x, width = width, height = height)
}

#' @export
amPieChart <- function(..., width = NULL, height = NULL){
  amCharts4(..., type = "PieChart", width = NULL, height = NULL)
}

#' @export
amXYChart <- function(..., width = NULL, height = NULL){
  amCharts4(..., type = "XYChart", width = NULL, height = NULL)
}

#' @export
amRadarChart <- function(..., width = NULL, height = NULL){
  amCharts4(..., type = "RadarChart", width = NULL, height = NULL)
}

#' @export
amSankeyDiagram <- function(..., width = NULL, height = NULL){
  amCharts4(..., type = "SankeyDiagram", width = NULL, height = NULL)
}

#' @export
amTreeMap <- function(..., width = NULL, height = NULL){
  amCharts4(..., type = "TreeMap", width = NULL, height = NULL)
}

#' @export
amChordDiagram <- function(..., width = NULL, height = NULL){
  amCharts4(..., type = "ChordDiagram", width = NULL, height = NULL)
}

#' @export
amSlicedChart <- function(..., width = NULL, height = NULL){
  amCharts4(..., type = "SlicedChart", width = NULL, height = NULL)
}

#' @export
#'
#' @examples
#' \dontrun{
#'    amGaugeChart(xAxes = list(list(type = "ValueAxis", min = 0, max = 100)))
#' }
#'
amGaugeChart <- function(..., width = NULL, height = NULL){
  amCharts4(..., type = "GaugeChart", width = NULL, height = NULL)
}


#' Shiny bindings for rAmCharts4
#'
#' Output and render functions for using rAmCharts4 within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a rAmCharts4
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @param shinyId single-element character vector indicating the output ID of the
#'   chart to modify (if invoked from a Shiny module, the namespace will be added
#'   automatically)
#' @param data A \code{data.frame}.
#' @param session the Shiny session object to which the chart belongs; usually the
#'   default value will suffice
#'
#' @name rAmCharts4-shiny
#'
#' @export
#' @importFrom htmlwidgets shinyWidgetOutput shinyRenderWidget
amCharts4Output <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'rAmCharts4', width, height, package = 'rAmCharts4')
}

#' @rdname rAmCharts4-shiny
#' @export
renderAmCharts4 <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, amCharts4Output, env, quoted = TRUE)
}


#' @rdname rAmCharts4-shiny
#' @export
amCharts4Proxy <- function(shinyId, data = NULL, session = shiny::getDefaultReactiveDomain()) {

  if (is.null(session)) {
    stop("amCharts4Proxy must be called from the server function of a Shiny app")
  }

  if (!is.null(session$ns) && nzchar(session$ns(NULL)) && substring(shinyId, 1, nchar(session$ns(""))) != session$ns("")) {
    shinyId <- session$ns(shinyId)
  }

  structure(
    list(
      session = session,
      id = shinyId,
      x = structure(
        list(data = data)
      )
    ),
    class = "amCharts4_Proxy"
  )
}