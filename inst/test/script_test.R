require(magrittr)
require(rAmCharts4)

amCharts4(data = data.frame(country = c("Lithuania", "Ireland"), litres = c(10, 20)), 
          type = "PieChart",
          series = list(list(type = "PieSeries", 
                             dataFields = list("value" = "litres",
                                               "category" = "country"))),
          responsive = list(enabled = TRUE))



data = data.frame(country = c("Lithuania", "Ireland"), litres = c(10, 20))
amPieChart() %>% amc_data(data) %>%
  amc_series(type = "PieSeries", 
                  dataFields = list("value" = "litres",
                                    "category" = "country")) %>%
  amc_legend()

amPieChart(data = data)  %>%
  amc_series(type = "PieSeries", 
             dataFields = list("value" = "litres",
                               "category" = "country")) %>%
  amc_legend()


amXYChart(data = data)  %>%
  amc_xAxes(type = "CategoryAxis",
            dataFields = list(category = "country", title = list(text = "Countries"))
  ) %>%
  amc_yAxes(type = "ValueAxis") %>%
  amc_series(type = "ColumnSeries", 
             dataFields = list("valueY" = "litres",
                               "categoryX" = "country")) %>%
  amc_legend() %>% amc_cursor()