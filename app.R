library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(tidyverse)
library(stringr)
library(purrr)

data_raw <- read.csv("data/raw/ucr_crime_1975_2015.csv")

crime_list <- c('Homicide', 'Rape', 'Larceny', 'Violent')
sum_list <- c('homs_sum', 'rape_sum', 'rob_sum', 'violent_crime')
rate_list <- c('homs_per_100k', 'rape_per_100k', 'rob_per_100k', 'violent_per_100k')
crime_cols <- tibble(crime_name = crime_list,
                     `Crime Count` = sum_list,
                     `Crime Rate` = rate_list)

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

map_data <- function(map_df, state_name, crime_name, year_) {
  map_df %>% filter(State %in% state_name) %>%
    filter(between(year, year_[1], year_[2])) %>%
    select(append(crime_name, c("year", "Abbreviation", "State"))) %>%
    mutate(crime_sum = rowSums(.[1:length(crime_name)])) %>% 
    mutate(hover = paste0(State, " ", round(crime_sum, 2)))
}

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
            md = 3,
            list(
              htmlBr(),
              dbcLabel("STATE"),
              dccDropdown(
                id = 'state',
                style = css$box,
                options = map(state_list, function(col) list(label = col, value = col)),
                multi = TRUE,
                value = state_list
              ),
              htmlBr(),
              htmlBr(),
              dbcLabel("CRIME"),
              dccDropdown(
                id = 'crime',
                style = css$box,
                options = map(crime_list, function(col) list(label = col, value = col)),
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
                value = list(1975, 2015)
              ),
              htmlBr(),
              htmlBr(),
              dbcLabel("METRIC"),
              dccRadioItems(
                id = "metric",
                options = list(
                  list(label = "Rate", value = "Crime Rate"),
                  list(label = "Number", value = "Crime Count")),
                value = "Crime Rate"
                )
            )
          ),
          dbcCol(
            md = 9,
            list(
              dccGraph(id='geo_chart'),
              dccGraph(id='trend_chart')
            )
            
          )
          
        )
      )
    )
  )
)

app$callback(
  output('trend_chart', 'figure'),
  list(input('state', 'value'),
       input('crime', 'value'),
       input('year_range', 'value'),
       input('metric', 'value')),
  function(state, crime, year_range, metric){
    
    selected_crime_cols <- crime_cols[c('crime_name', metric)] %>%
      filter(crime_name %in% crime) %>%
      pull(-crime_name)
    
    trend_df <- data_crime %>%
      select(year, State, selected_crime_cols)
    
    colnames(trend_df) <- append(c("year", "State"), unlist(crime))
    
    trend <- trend_df %>%
      filter(State %in% state) %>%
      filter(between(year, year_range[1], year_range[2])) %>%
      pivot_longer(cols = unlist(crime), names_to = "crime_type", values_to = "crime_metric") %>%
      mutate(crime_type = factor(crime_type, levels = crime)) %>%
      group_by(year, State, crime_type) %>%
      summarise(sum_crime = sum(crime_metric)) %>%
      ggplot(aes(x = year, y = sum_crime, color = crime_type)) +
      geom_line(stat = 'summary', fun = sum) +
      labs(x = "Year", y = metric, color = "Crime Type")
    
    ggplotly(trend)
  })

app$callback(
  output('geo_chart', 'figure'),
  list(input('state', 'value'),
       input('crime', 'value'),
       input('year_range', 'value'),
       input('metric', 'value')),
  function(state, crime, year_range, metric){
    
    selected_crime_cols <- crime_cols[c('crime_name', metric)] %>%
      filter(crime_name %in% crime) %>%
      pull(-crime_name)
    
    map_fnl_data <- map_data(data_crime, state,
                             crime_name = selected_crime_cols,
                             year_ = year_range)
    
    plot_geo(map_fnl_data, 
             locationmode = 'USA-states') %>% 
      add_trace(locations = ~Abbreviation,
                z = ~crime_sum, name = "Crime Count",
                zmin = min(map_fnl_data$crime_sum),
                zmax = max(map_fnl_data$crime_sum),
                text = ~hover,
                hoverinfo = 'text'
      ) %>% 
      layout(geo = list(scope = 'usa')) %>%
      colorbar(title = metric)

  })

app$run_server()