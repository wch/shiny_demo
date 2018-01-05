library(shiny)
library(shinythemes)
library(dplyr)
library(DT)
library(ggplot2)
library(ggrepel)

all_data <- readRDS("all.rds")
all_data <- all_data %>%
  # This package has a way earlier date than all others
  filter(Package != "hpower") %>%
  group_by(Package) %>%
  arrange(desc(date))

date_min <- min(all_data$date)
date_max <- max(all_data$date)

packages_at_date <- function(target_date) {
  all_data %>%
    filter(date <= target_date) %>%
    slice(1)
}

# Compute by using cumsum
# Talk about precompute for hours
# Fairly expensive operation - takes ~7s for n=20
compute_count_by_date <- function(n = 20) {
  dates <- seq(date_min, date_max, length.out = n)
  counts <- vapply(dates,
    function(date) packages_at_date(date) %>% nrow(),
    0L
  )

  data.frame(date = dates, n = counts)
}
count_by_date <- compute_count_by_date()



ui <- navbarPage(theme = shinytheme("paper"),
  "Packages on CRAN",
  tabPanel("Timeline",
    sliderInput("date", "Date",
      date_min, date_max, date_max,
      width = "100%"),
    plotOutput("cran_timeline", height = "160px"),
    checkboxInput("cran_timeline_log", "Log-10 scale", FALSE),
    h3(
      "Number of packages:",
      textOutput("info_n_packages", inline = TRUE)
    ),
    DT::dataTableOutput("table")
  ),
  tabPanel("Package info",
    textInput("package", "Package Name", placeholder = "ggplot2, Rcpp, shiny, ..."),
    plotOutput("package_timeline", height = "120px")
  )

)

server <- function(input, output) {
  all_at_date <- reactive({
    packages_at_date(input$date)
  })

  output$cran_timeline <- renderPlot({
    if (input$cran_timeline_log) {
      count_by_date$n <- log10(count_by_date$n)
    }
    par(mar = c(2,2,1.5,0))
    plot(count_by_date, n ~ date, type = "l")
    abline(v = input$date, col = "#ffcccc")
  })

  output$table <- renderDataTable({
    all_at_date() %>%
      mutate(Date = as.character(date)) %>%
      select(Package, Version, Date)
  })

  output$info_n_packages <- renderText({
    all_at_date() %>% nrow()
  })


  output$package_timeline <- renderPlot({
    dat <- all_data %>% filter(Package == input$package)
    ggplot(dat, aes(date)) + geom_point(y=1, color = "red") +
      scale_y_continuous(name = NULL, limits = c(0, 2), breaks = NULL) +
      scale_x_date(name = NULL, date_minor_breaks = "1 month") +
      geom_text_repel(y = 1, color = "grey60", aes(label = Version),
        point.padding = 1, min.segment.length = 0) +
      ggtitle("Release dates") +
      theme_bw()
  })
}

shinyApp(ui, server)
