library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(ggplot2)
library(ggrepel)

source("utils.R")
source("plot_cache.R")

all_data <- reactiveVal(read_rds("packages.rds"))

date_min <- reactive( min(all_data()$date) )
date_max <- reactive( max(all_data()$date) )

packages_summary_by_date <- reactive({
  # Creates a tibble with columns:
  # * date - ascending dates from date_min() to date_max()
  # * n - total number of packages available as of that date
  # * new - number of packages that were published for the first time

  df <- all_data() %>%
    group_by(Package) %>%
    summarise(date = min(date)) %>%
    group_by(date) %>%
    tally() %>%
    arrange(date)

  all_dates <- data.frame(
    date = seq(date_min(), date_max(), 1)
  )

  df %>%
    right_join(all_dates, "date") %>%
    mutate(n = ifelse(!is.na(n), n, 0)) %>%
    rename(new = n) %>%
    mutate(n = cumsum(new)) %>%
    arrange(date) %>%
    select(date, n, new)
})

# Get packages available on CRAN at a particular date
packages_available_at_date <- function(target_date) {
  packages_summary_by_date() %>%
    filter(date == target_date) %>%
    pull(n)
}

compute_count_by_date <- function(n = 25) {
  df <- packages_summary_by_date()
  indices <- seq(1, nrow(df), length.out = n)
  df[indices,]
}
count_by_date <- reactive( compute_count_by_date() )


# Dependencies data
all_deps <- reactiveVal(read_rds("deps.rds"))

deps_summary <- reactive({
  all_deps() %>%
    group_by(Package, Version, type) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    spread(type, n) %>%
    replace_na(list(Depends = 0L, Imports = 0L, Suggests = 0L))
})


plot_width <- 800
plot_height <- 240
plot_retina <- 2

plot_cache <- plotCache("package_timeline", all_deps(),
  width = plot_width * plot_retina, height = plot_height * plot_retina,
  res = 72 * plot_retina,
  function(package, dat) {
    if (nrow(dat) == 0)
      return()

    deps <- gather(deps_summary(), type, n, Depends:Suggests) %>%
      filter(Package == package) %>%
      left_join(all_data(), by = c("Package", "Version")) %>%
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

    p <- ggplot(deps_step, aes(x = date)) +
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
    print(p)
  }
)


# =============================================================================
# UI
# =============================================================================
info_panel <- function(title = "", content, class = "default") {
  panel_class <- if (!is.null(class)) paste0("panel-", class)

  div(class = "col-sm-4",
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
  id = "tabs",
  tabPanel("Overview",
    tags$head(tags$style(HTML("body { overflow-y: scroll; } #package_timeline { max-width: 100%; }"))),
    uiOutput("date_slider_ui"),
    plotOutput("cran_timeline", height = "160px"),
    div(
      checkboxInput("cran_timeline_log", "Log-10 scale", FALSE),
      style = "display:inline-block"
    ),
    actionButton("refresh", "Refresh data",
      style = "display:inline-block; float:right;"
    ),
    div(class = "panel",
      div(class = "panel-heading",
        style = "font-weight: bold; font-size: xx-large;",
        textOutput("info_date")
      ),
      div(class = "panel-body",
        div(class = "row",
          info_panel(
            "Available packages",
            h3(textOutput("info_n_packages"))
          ),
          info_panel(
            "Packages released",
            h3(textOutput("info_n_packages_day"))
          ),
          info_panel(
            "New packages",
            h3(textOutput("info_n_new_packages_day"))
          ),
          div(class = "col-sm-12",
            h4("Packages released on this day"),
            tableOutput("info_released_packages_table")
          )
        )
      )
    )
  ),
  tabPanel("Package info",
    div(
      textInput("package", "Package Name", placeholder = "ggplot2, Rcpp, shiny, ..."),
      style="display:inline-block"
    ),
    uiOutput("package_version_selector", style = "display:inline-block"),
    uiOutput("package_info"),
    imageOutput("package_timeline", width = plot_width, height = "auto"),
    tableOutput("package_versions_table")
  )
)


# =============================================================================
# Server
# =============================================================================
server <- function(input, output) {

  # Overview tab ==============================================================

  packages_released_on_date <- reactive({
    req(input$date)

    released_packages <- all_data() %>% filter(date == input$date) %>% pull(Package)

    all_data() %>%
      filter(date <= input$date) %>%
      filter(Package %in% released_packages) %>%
      group_by(Package) %>%
      summarise(
        Version = first(Version),
        total_releases = n()
      ) %>%
      ungroup()
  })

  output$date_slider_ui <- renderUI({
    sliderInput("date", "Date", date_min(), date_max(), date_max(), width = "100%")
  })

  output$cran_timeline <- renderPlot({
    req(input$date)
    counts <- count_by_date() %>% select(date, n)

    if (input$cran_timeline_log) {
      counts$n <- log10(counts$n)
    }
    par(mar = c(2,2,1.5,0))
    plot(counts, n ~ date, type = "l")
    abline(v = input$date, col = "#ffcccc")
  })

  output$info_date <- renderText({
    req(input$date)
    format(input$date, "%Y/%m/%d")
  })

  output$info_n_packages <- renderText({
    req(input$date)
    packages_available_at_date(input$date)
  })

  output$info_n_packages_day <- renderText({
    packages_released_on_date() %>%
      nrow()
  })

  output$info_n_new_packages_day <- renderText({
    packages_released_on_date() %>%
      filter(total_releases == 1) %>%
      nrow()
  })

  output$info_released_packages_table <- renderTable({
    packages_released_on_date() %>%
      mutate(" " = ifelse(total_releases == 1, "New", "")) %>%
      rename(`Total Releases` = total_releases)
  })

  observeEvent(input$refresh, {
    old_all_data  <- all_data()
    old_all_deps <- all_deps()

    # Set these to NULL immediately to invalidate downstream reactives and have
    # corresponding UI elements gray out.
    all_data(NULL)
    all_deps(NULL)

    tryCatch(
      {
        # Update the data. Note that this does not save over the existing .rds
        # files, so the update will not persist across runs of the app.
        json <- download_crandb()
        crandb_data <- process_crandb_json(json)

        # Writing to these reactive vals will trigger invalidations.
        all_data(crandb_data$packages)
        all_deps(crandb_data$deps)
      },
      error = function(e) {
        # If error occurs, just restore the original data.
        all_data(old_all_data)
        all_deps(old_all_deps)
      }
    )
  })


  # Selected package tab ======================================================

  selected_package_data <- reactive({
    all_data() %>% filter(Package == input$package)
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

    deps <- dat %>% left_join(all_deps(), by = c("Package", "Version"))

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

  output$package_timeline <- renderImage({
    path <- plot_cache(input$package, selected_package_data())
    list(
      src = path,
      width = "100%",
      height = "auto"
    )
  }, deleteFile = FALSE)

  output$package_versions_table <- renderTable({
    dat <- selected_package_data()
    if (nrow(dat) == 0)
      return()

    dat %>%
      ungroup() %>%
      left_join(deps_summary(), by = c("Package", "Version")) %>%
      mutate(date = as.character(date)) %>%
      select(Version, Maintainer, License, date, Depends, Imports, Suggests) %>%
      arrange(desc(date))
  }, spacing = "xs", hover = TRUE)

}


shinyApp(ui, server)

