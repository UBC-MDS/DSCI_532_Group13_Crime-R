library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)

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
  
  css$filter_panel <- list("border" = "10px solid black", )
  
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
          )
        )
      )
    )
  )
)



app$run_server()