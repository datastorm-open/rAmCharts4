% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/amCharts.R
\name{amSunburst}
\alias{amSunburst}
\title{Sun burst}
\usage{
amSunburst(..., width = NULL, height = NULL)
}
\description{
Sun burst
}
\examples{
\dontrun{

   amSunburst(data = list(list(name = "f", value = 1), list( name = "s", value = 2)),
              dataFields =list(
                value = "value",
                name = "name"))


data = list(list(name = "First",
                            children = list(list(name = "A1" ,value = 100),
                                            list(name = "A2" ,value = 60),
                                            list(name = "A3" ,value = 30)
                            )),
                       list(
                         name = "Second",
                         children = list(list(name = "B1" ,value = 135),
                                         list(name = "B2" ,value = 98),
                                         list(name = "B3" ,value = 56)
                         )),
                       list(
                         name = "Third",
                         children = list(list(name = "C1" ,value = 335),
                                         list(name = "C2" ,value = 148),
                                         list(name = "C3" ,value = 126),
                                         list(name = "C4" ,value = 26)
                         )),
                       list(
                         name = "Fourth",
                         children = list(list(name = "D1" ,value = 415),
                                         list(name = "D2" ,value = 148),
                                         list(name = "D3" ,value = 89),
                                         list(name = "D4" ,value = 64),
                                         list(name = "D4" ,value = 16)
                         )),
                       list(
                         name = "Fifth",
                         children = list(list(name = "E1" ,
                                              children = list(
                                                list(name = "EE1" ,value = 415),
                                                list(name = "EE1" ,value = 256)
                                              )),
                                         list(name = "E2" ,value = 148)
                         ))
)
amSunburst(data = data,
dataFields =list(
  value = "value",
  name = "name",
  children = "children"))

}

}
