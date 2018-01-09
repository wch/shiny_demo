library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggrepel)

source("utils.R")

all_data <- readRDS("packages.rds")
all_data <- all_data %>%
  # This package has a way earlier date than all others
  filter(Package != "hpower") %>%
  arrange(desc(date))

date_min <- min(all_data$date)
date_max <- max(all_data$date)


# Get packages available on CRAN at a particular date
packages_at_date <- function(target_date) {
  all_data %>%
    filter(date <= target_date) %>%
    group_by(Package) %>%
    slice(1)
}

compute_count_by_date <- function(n = 25) {
  dates <- seq(date_min, date_max, length.out = n)
  counts <- vapply(dates,
    function(date) packages_at_date(date) %>% nrow(),
    0L
  )

  data.frame(date = dates, n = counts)
}
count_by_date <- compute_count_by_date()


# Dependencies data
all_deps <- readRDS("deps.rds")

deps_summary <- all_deps %>%
  group_by(Package, Version, type) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  spread(type, n) %>%
  replace_na(list(Depends = 0L, Imports = 0L, Suggests = 0L))


info_panel <- function(title = "", content, class = "default") {
  panel_class <- if (!is.null(class)) paste0("panel-", class)

  div(class = "col-sm-3",
    div(class = paste("panel", panel_class),
      div(class = "panel-heading",
        div(class = "panel-title", title)
      ),
      div(class = "panel-body",
        content
      )
    )
  )
}


ui <- navbarPage(theme = shinytheme("paper"),
  "CRAN explorer",
  tabPanel("Timeline",
    tags$head(tags$style(HTML("body { overflow-y: scroll; }"))),
    sliderInput("date", "Date",
      date_min, date_max, date_max,
      width = "100%"),
    plotOutput("cran_timeline", height = "160px"),
    checkboxInput("cran_timeline_log", "Log-10 scale", FALSE),
    div(class = "row",
      info_panel(
        "On this day",
        h3(textOutput("info_date")),
        class = "primary"
      ),
      info_panel(
        "Packages on CRAN",
        h3(textOutput("info_n_packages"))
      ),
      info_panel(
        "Packages released",
        h3(textOutput("info_n_packages_day"))
      ),
      info_panel(
        "New packages",
        h3(textOutput("info_n_new_packages_day"))
      )
    ),
    actionButton("refresh", "Refresh data")
  ),
  tabPanel("Package info",
    div(
      textInput("package", "Package Name", placeholder = "ggplot2, Rcpp, shiny, ..."),
      style="display:inline-block"
    ),
    uiOutput("package_version_selector", style = "display:inline-block"),
    uiOutput("package_info"),
    plotOutput("package_timeline", height = "240px"),
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

  output$info_date <- renderText({
    format(input$date, "%Y/%m/%d")
  })

  output$info_n_packages <- renderText({
    all_at_date() %>% nrow()
  })

  output$info_n_packages_day <- renderText({
    all_data %>%
      filter(date == input$date) %>%
      nrow()
  })

  output$info_n_new_packages_day <- renderText({
    all_data %>%
      filter(date <= input$date) %>%
      group_by(Package) %>%
      filter(any(date == input$date)) %>%
      summarise(total_releases = n()) %>%
      filter(total_releases == 1) %>%
      nrow()
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
    if (is.null(input$package_version) || input$package_version == "")
      return()

    dat <- selected_package_data() %>% filter(Version == input$package_version)
    if (nrow(dat) != 1)
      return()

    deps <- dat %>% left_join(all_deps, by = c("Package", "Version"))


    wellPanel(
      p(tags$b("Title: "), dat$Title),
      p(tags$b("Description: "), dat$Description),
      p(tags$b("Maintainer: "), dat$Maintainer),
      p(tags$b("License: "), dat$License),
      if (!is.na(dat$BugReports))
        p(tags$b("Bug Reports: "), a(href = dat$BugReports, dat$BugReports, target = "_blank")),
      if (!is.na(dat$URL))
        p(tags$b("URL: "), a(href = dat$URL, dat$URL, target = "_blank")),
      p(tags$b("Date: "), dat$date),
      p(tags$b("Depends: "),
        filter(deps, type == "Depends") %>% pull(name) %>% paste(collapse = ", ")),
      p(tags$b("Imports: "),
        filter(deps, type == "Imports") %>% pull(name) %>% paste(collapse = ", ")),
      p(tags$b("Suggests: "),
        filter(deps, type == "Suggests") %>% pull(name) %>% paste(collapse = ", ")),
      p(tags$b("MD5sum: "), dat$MD5sum)
    )
  })

  output$package_timeline <- renderPlot({
    dat <- selected_package_data()
    if (nrow(dat) == 0)
      return()

    deps <- gather(deps_summary, type, n, Depends:Suggests) %>%
      filter(Package == input$package) %>%
      left_join(all_data, by = c("Package", "Version")) %>%
      mutate(type = factor(type, levels = c("Suggests", "Imports", "Depends")))

    y_max <- deps %>%
      group_by(Package, Version) %>%
      summarise(n = sum(n)) %>%
      pull(n) %>%
      max()
    y_lims <- c(-0.5*y_max, y_max)
    y_breaks <- pretty(c(0, y_max), n = 3)

  # Create data set for step plot (we would use geom_step here, but it doesn't
  # fill area under curve)
  deps_step <- deps %>%
    group_by(type) %>%
    arrange(type, date)

  deps_step <- bind_rows(
      deps_step,
      mutate(
        deps_step,
        date = date - 0.01,
        n = lag(n, default = NA)
      )
    ) %>%
    drop_na(n)

    ggplot(deps_step, aes(x = date)) +
      geom_area(aes(y = n, fill = type), position = "stack", alpha = 0.4) +
      geom_hline(yintercept = 0, size = 0.4) +
      geom_line(aes(y = n, group = type), position = "stack", size = .2) +
      geom_point(data = dat, y = 0, size = 1, color = "red") +
      geom_text_repel(data = dat, y = 0, angle = 90, color = "grey60",
        ylim = c(NA, 0), aes(label = Version),
        point.padding = 1, min.segment.length = 0) +
      scale_fill_brewer(palette = "Blues") +
      scale_y_continuous(name = NULL, limits = y_lims, breaks = y_breaks, expand = c(0.05, 0)) +
      scale_x_date(name = NULL, date_minor_breaks = "1 month", expand = c(0.02, 0)) +
      guides(fill = guide_legend(title = NULL)) +
      ggtitle("Releases and dependencies over time") +
      theme_bw() +
      theme(legend.position = "bottom")
  })

  output$package_versions_table <- renderTable({
    dat <- selected_package_data()
    if (nrow(dat) == 0)
      return()

    dat %>%
      ungroup() %>%
      left_join(deps_summary, by = c("Package", "Version")) %>%
      mutate(date = as.character(date)) %>%
      select(Version, Maintainer, License, date, Depends, Imports, Suggests) %>%
      arrange(desc(date))
  }, spacing = "xs", hover = TRUE)


}

shinyApp(ui, server)

