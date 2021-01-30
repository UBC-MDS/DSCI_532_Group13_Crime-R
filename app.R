library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(tidyverse)
library(stringr)

data_raw <- read.csv("data/raw/ucr_crime_1975_2015.csv")

crime_list <- list('Homicide', 'Rape', 'Larceny', 'Violent')
sum_list <- list('homs_sum', 'rape_sum', 'rob_sum', 'violent_crime')
rate_list <- list('homs_per_100k', 'rape_per_100k', 'rob_per_100k', 'violent_per_100k')

data_processing <- function(data){
  data <- data %>%
    mutate(Abbreviation = substr(ORI, 1, 2))
  states <- read.csv('data/raw/states.csv')
  data %>%
    left_join(states, by = "Abbreviation") %>%
    select(-c(source, url, ORI, months_reported))
}

data_crime <- data_processing(data_raw)
state_list <- unique(data_crime %>%
                       select(State) %>%
                       drop_na() %>%
                       pull(State))

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

custom_css <- function() {
  css <- list()
  
  # Input parameter box
  css$dd <- list(
    "border" = "2px solid black",
    "border-radius" = "10px",
    "background-color" = "light grey"
  )
  
  # Footnote text
  css$sources <- list("font-size" = "xx-small")
  
  css$filter_panel <- list("border" = "10px solid black")
  
  # Return CSS
  css
}

css <- custom_css()

app$layout(
  dbcContainer(
    list(
      htmlH1("Crime In United States", style = list('color'= 'red', 'fontSize'= '50px', 'fontFamily' = "Times New Roman", 'paddingTop'= '10px', 'textAlign' = 'center')),
      htmlBr(),
      dbcRow(
        list(
          dbcCol(
            id = 'filter-panel',
            style = css$filter_panel,
            md = 4,
            list(
              htmlBr(),
              dbcLabel("STATE"),
              dccDropdown(
                id = 'state',
                style = css$box,
                options = list(list(label = 'Colorado', value = 'Colorado'),
                               list(label = 'Texas', value = 'Texas'),
                               list(label = 'Georgia', value = 'Georgia')),
                multi = TRUE,
                value = c('Colorado')
              ),
              htmlBr(),
              htmlBr(),
              dbcLabel("CRIME"),
              dccDropdown(
                id = 'crime',
                style = css$box,
                options = list(list(label = 'Homicide', value = 'Homicide'),
                               list(label = 'Larceny', value = 'Larceny'),
                               list(label = 'Rape', value = 'Rape'),
                               list(label = 'Violent', value = 'Violent')),
                multi = TRUE,
                value = c('Homicide', 'Larceny', 'Rape', 'Violent')
              ),
              htmlBr(),
              htmlBr(),
              dbcLabel("YEAR RANGE"),
              dccRangeSlider(
                id = 'year_range',
                min = 1975,
                max = 2015,
                step = 1,
                marks = list(
                  "1975" = "1975",
                  "1985" = "1985",
                  "1995" = "1995",
                  "2005" = "2005",
                  "2015" = "2015"
                ),
                value = list(1975, 1995)
              ),
              htmlBr(),
              htmlBr(),
              dbcLabel("METRIC"),
              dccRadioItems(
                id = "metric",
                options = list(
                  list(label = "Rate ", value = "rate"),
                  list(label = "Number", value = "number"))
                )
            )
          ),
          dbcCol(
            id = 'trend_chart',
            md = 9,
            list(
              dccGraph(id='trend_chart_1'),
              dccGraph(id='trend_chart_2')
            )
            
          )
          
        )
      )
    )
  )
)

app$callback(
  output('trend_chart_1', 'figure'),
  list(input('state', 'value'),
       input('crime', 'value'),
       input('year_range', 'value'),
       input('metric', 'value')),
  function(state, crime, year_range, metric){
    trend <- data_crime %>%
      ggplot(aes(x = year, y = homs_sum)) +
      geom_line(stat = 'summary', fun = sum)
    
    ggplotly(trend)
  })

app$run_server()