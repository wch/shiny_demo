library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(DT)
library(purrr)
library(ggplot2)
library(ggrepel)

source("../utils.R")
source("../pkg_info.R")

all_data <- readRDS("packages.rds")
all_data <- all_data %>%
  # This package has a way earlier date than all others
  filter(Package != "hpower") %>%
  # These will always be "Package" and the same as `Author` -- removing them leaves us with a neat 16 rows
  select(-Type, -`Authors@R`) %>%
  select(
    Package, Title, Version, URL,
    Description, Maintainer, Author, Collate,
    License, NeedsCompilation, BugReports, Repository,
    MD5sum, Packaged, `Date/Publication`, date
  ) %>%
  group_by(Package) %>%
  arrange(desc(date))

all_deps <- readRDS("deps.rds")

date_min <- min(all_data$date)
date_max <- max(all_data$date)


deps_summary <- all_deps %>%
  group_by(Package, Version, type) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  spread(type, n) %>%
  replace_na(list(Depends = 0L, Imports = 0L, Suggests = 0L))


packages_at_date <- function(target_date) {
  all_data %>%
    filter(date <= target_date) %>%
    slice(1)
}

# Compute by using cumsum
# Talk about precompute for hours
# Fairly expensive operation - takes ~7s for n=20
compute_count_by_date <- function(n = 4) {
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
    includeCSS("../pkg_info.css"),
    sliderInput("date", "Date",
      date_min, date_max, date_max,
      width = "100%"),
    plotOutput("cran_timeline", height = "160px"),
    checkboxInput("cran_timeline_log", "Log-10 scale", FALSE),
    h3(
      "Number of packages:",
      textOutput("info_n_packages", inline = TRUE)
    ),
    DT::dataTableOutput("table"),
    actionButton("refresh", "Refresh data")
  ),
  tabPanel("Package info",
    div(
      textInput("package", "Package Name", placeholder = "ggplot2, Rcpp, shiny, ..."),
      style="display:inline-block"
    ),
    uiOutput("package_version_selector", style = "display:inline-block"),
    uiOutput("package_info"),
    plotOutput("package_timeline", height = "120px"),
    plotOutput("package_deps_timeline", height = "120px"),
    tableOutput("package_versions_table")
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

  observeEvent(input$refresh, {
    download_crandb()
    crandb_file_to_df()
  })


  selected_package_data <- reactive({
    all_data %>% filter(Package == input$package)
  })

  output$package_version_selector <- renderUI({
    versions <- selected_package_data()$Version
    selectInput("package_version", "Version", versions, selectize = FALSE, width = "150px")
  })

  output$package_info <- renderUI({
    dat <- selected_package_data() %>% filter(Version == input$package_version)
    if (nrow(dat) != 1)
      return(invisible())
    # str(transpose(dat)[[1]])
    pkg_info_table(dat)
  })

  output$package_timeline <- renderPlot({
    dat <- selected_package_data()

    ggplot(dat, aes(date)) + geom_point(y=1, color = "red") +
      scale_y_continuous(name = NULL, limits = c(0, 2), breaks = NULL) +
      scale_x_date(name = NULL, date_minor_breaks = "1 month") +
      geom_text_repel(y = 1, color = "grey60", aes(label = Version),
        point.padding = 1, min.segment.length = 0) +
      ggtitle("Releases") +
      theme_bw()
  })

  output$package_deps_timeline <- renderPlot({
    dat <- gather(deps_summary, type, n, Depends:Suggests) %>%
      filter(Package == input$package) %>%
      left_join(all_data, by = c("Package", "Version")) %>%
      mutate(type = factor(type, levels = c("Suggests", "Imports", "Depends")))

    ggplot(dat, aes(x = date, y = n, fill = type)) + geom_area()

  })

  output$package_versions_table <- renderTable({
    selected_package_data() %>%
      ungroup() %>%
      left_join(deps_summary, by = c("Package", "Version")) %>%
      mutate(date = as.character(date)) %>%
      select(Version, Maintainer, License, date, Depends, Imports, Suggests) %>%
      arrange(desc(date))
  }, spacing = "xs", hover = TRUE)


}

shinyApp(ui, server)

