# Download all CRAN package data from crandb and return as a JSON string.
# `outfile`: If non-NULL, save the JSON to the specfied filename.
# `limit`: Limit the number of packages to download information about.
download_crandb <- function(limit = NULL, outfile = NULL) {
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

  cat("downloading... ")
  limit_str <- if (!is.null(limit)) paste0("?limit=", limit)
  all_data_json <- DB(paste0("/-/all", limit_str))
  cat("prettifying... ")
  all_data_json <- prettify(all_data_json, indent = 2)

  if (!is.null(outfile)) {
    cat("writing... ")
    write_file(unclass(all_data_json), outfile)
  }

  cat("done.\n")
  all_data_json
}


# Convert gzipped JSON file to data frame. Returns the data frame.
process_crandb_json <- function(json = NULL, filename = NULL) {
  library(dplyr)
  library(jsonlite)
  library(purrr)
  library(readr)

  if (!xor( is.null(json), is.null(filename))) {
    stop("Need either json or filename, but not both.")
  }

  if (!is.null(filename)) {
    cat("reading JSON from file... ")
    json <- read_file(filename)
  }

  cat("converting JSON to R list... ")
  all_data_raw <- fromJSON(json)

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

  cat("Converting to data frame... ")
  all_data <- all_data_raw %>% map("versions") %>% purrr::flatten() %>% unname()
  all_data <- all_data %>% extract_package_info()

  cat("done.\n")
  # Data cleaning
  all_data <- all_data %>%
    filter(
      !is.na(Version),
      # This package has a way earlier date than all others
      Package != "hpower"
    ) %>%
    arrange(desc(date))


  # =================== Create table of dependency data =======================
  cat("Processing dependencies... ")
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
  cat("done.\n")

  # =================================================
  list(
    # raw = all_data_raw,
    packages = all_data,
    deps = all_deps
  )
}
