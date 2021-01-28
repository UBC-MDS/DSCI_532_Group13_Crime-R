library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
  dbcContainer(
    list(
      htmlH1("Crime Dashboard"),
      dbcRow(
        list(
          dbcCol(
            id = 'filter-panel',
            md = 4,
            list(
              htmlBr(),
              dbcLabel("STATE"),
              dccDropdown(
                id = 'state',
                options = list(list(label = 'Colorado', value = 'Colorado'),
                               list(label = 'Texas', value = 'Texas'),
                               list(label = 'Georgia', value = 'Georgia')),
                multi = TRUE,
                value = c('Colorado')
              ),
              htmlBr(),
              dbcLabel("CRIME"),
              dccDropdown(
                id = 'crime',
                options = list(list(label = 'Homicide', value = 'Homicide'),
                               list(label = 'Larceny', value = 'Larceny'))
              ),
              htmlBr(),
              htmlBr(),
              dbcLabel("YEAR RANGE"),
              dccSlider(
                id = 'year_range',
                min = 1975,
                max = 2015,
                step = 10,
                marks = c(1975, 1985, 1995, 2005, 2015)
              ),
              htmlBr(),
              dbcLabel("METRIC"),
              dccRadioItems(
                id = "metric",
                options = list(
                  list(label = "Rate", value = "rate"),
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