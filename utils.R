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
crandb_file_to_df <- function() {
  library(dplyr)
  library(jsonlite)
  library(purrr)
  library(readr)

  all_data_json <- read_file("all.json.gz")
  all_data <- fromJSON(all_data_json)

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

  extract_package_info <- function(dat) {
    tibble(
      Package            = extract_col(dat, "Package"),
      Version            = extract_col(dat, "Version"),
      Maintainer         = extract_col(dat, "Maintainer"),
      License            = extract_col(dat, "License"),
      Depends            = extract_dep_col(dat, "Depends"),
      Suggests           = extract_dep_col(dat, "Suggests"),
      NeedsCompilation   = recode(extract_col(dat, "NeedsCompilation"), yes = TRUE, no = FALSE, .missing = FALSE),
      Packaged           = extract_col(dat, "Packaged"),
      # `Date` is supplied by the author, and they often don't update it.
      # Date               = extract_col(dat, "Date"),
      # `date` is the actual date that the package is on CRAN (I think)
      date               = as.Date(extract_col(dat, "date"))
    )
  }

  all_data <- all_data %>% map("versions") %>% flatten() %>% unname()
  all_data <- all_data %>% extract_package_info()

  # Data cleaning
  all_data <- all_data %>%
    filter(!is.na(Version))

  all_data
}
