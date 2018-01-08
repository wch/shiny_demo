# Download all CRAN package data from crandb and save as gzipped JSON file
dowload_crandb <- function() {
  library(jsonlite)
  library(magrittr)
  library(readr)

  skip_lines <- function(text, head = 1e6, tail = 1e6) {
  	text <- strsplit(text, "\n")[[1]]
  	tail <- min(tail, max(0, length(text) - head))
  	skip_text <- if (length(text) > head + tail) {
  		paste("\n... not showing", length(text) - head - tail, "lines ...\n")
  	} else {
  		character()
  	}
      c(head(text, head), skip_text, tail(text, tail)) %>%
  		paste(collapse = "\n")
  }

  DB <- function(api, head = 1e6, tail = head) {
    paste0("http://crandb.r-pkg.org", "/", api) %>%
      httr::GET() %>%
    	httr::content(as = "text", encoding = "UTF-8") %>%
    	jsonlite::prettify()
  }

  all_data_json <- DB("/-/all")
  all_data_json <- prettify(all_data_json, indent = 2)
  write_file(unclass(all_data_json), "all.json.gz")
}


# Convert gzipped JSON file to data frame. Returns the data frame.
process_crandb_file <- function() {
  library(dplyr)
  library(jsonlite)
  library(purrr)
  library(readr)

  all_data_json <- read_file("all.json.gz")
  all_data_raw <- fromJSON(all_data_json)

  # Returns an atomic vector
  extract_col <- function(x, name) {
    vecs <- lapply(x, function(obj) {
      val <- obj[[name]]
      if (is.null(val)) {
        NA
      } else {
        val
      }
    })

    do.call(c, vecs)
  }

  # For Depends and Suggests columns - returns a data frame
  extract_dep_col <- function(x, name) {
    vecs <- lapply(x, function(obj) {
      val <- obj[[name]]
      data.frame(name = names(val), version = as.character(val), stringsAsFactors = FALSE)
    })
    vecs
  }

  extract_list_col <- function(x, name) {
    lapply(x, function(obj) {
      obj[[name]]
    })
  }

  extract_package_info <- function(dat) {
    tibble(
      Package            = extract_col(dat, "Package"),
      Type               = extract_col(dat, "Type"),
      Title              = extract_col(dat, "Title"),
      Version            = extract_col(dat, "Version"),
      `Authors@R`        = extract_col(dat, "Authors@R"),
      Maintainer         = extract_col(dat, "Maintainer"),
      Description        = extract_col(dat, "Description"),
      License            = extract_col(dat, "License"),
      Depends            = extract_list_col(dat, "Depends"),
      Imports            = extract_list_col(dat, "Imports"),
      Suggests           = extract_list_col(dat, "Suggests"),
      NeedsCompilation   = recode(extract_col(dat, "NeedsCompilation"), yes = TRUE, no = FALSE, .missing = FALSE, .default = FALSE),
      URL                = extract_col(dat, "URL"),
      BugReports         = extract_col(dat, "BugReports"),
      Collate            = extract_col(dat, "Collate"),
      Author             = extract_col(dat, "Author"),
      Repository         = extract_col(dat, "Repository"),
      MD5sum             = extract_col(dat, "MD5sum"),
      Packaged           = extract_col(dat, "Packaged"),
      `Date/Publication` = extract_col(dat, "Date/Publication"),
      # `Date` is supplied by the author, and they often don't update it.
      # Date               = extract_col(dat, "Date"),
      # `date` is the actual date that the package is on CRAN (I think)
      date               = as.Date(extract_col(dat, "date"))
    )
  }

  all_data <- all_data_raw %>% map("versions") %>% flatten() %>% unname()
  all_data <- all_data %>% extract_package_info()

  # Data cleaning
  all_data <- all_data %>%
    filter(!is.na(Version))


  # =================== Create table of dependency data =======================
  create_dep_list <- function(Package, Version, pkg_deps) {
    # Remove R dependency
    pkg_deps[["R"]] <- NULL
    n_deps <- length(pkg_deps)

    list(
      Package = rep(Package, n_deps),
      Version = rep(Version, n_deps),
      name    = names(pkg_deps),
      ver     = as.character(pkg_deps)
    )
  }

  depends <- mapply(
    create_dep_list,
    all_data$Package, all_data$Version, all_data$Depends,
    SIMPLIFY = FALSE
  )
  depends <- depends %>% transpose() %>% map(flatten_chr) %>% as_tibble()
  depends$type <- "Depends"

  imports <- mapply(
    create_dep_list,
    all_data$Package, all_data$Version, all_data$Imports,
    SIMPLIFY = FALSE
  )
  imports <- imports %>% transpose() %>% map(flatten_chr) %>% as_tibble()
  imports$type <- "Imports"

  suggests <- mapply(
    create_dep_list,
    all_data$Package, all_data$Version, all_data$Suggests,
    SIMPLIFY = FALSE
  )
  suggests <- suggests %>% transpose() %>% map(flatten_chr) %>% as_tibble()
  suggests$type <- "Suggests"

  all_deps <- bind_rows(depends, imports, suggests)


  # Remove Depends, Imports, and Suggests columns from all_data since the data
  # has been extracted into its own table.
  all_data$Depends <- NULL
  all_data$Imports <- NULL
  all_data$Suggests <- NULL

  # =================================================
  list(
    # raw = all_data_raw,
    packages = all_data,
    deps = all_deps
  )
}
