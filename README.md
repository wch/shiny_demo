CRAN explorer demo application
==============================

To run the application:

```R
library(shiny)
runApp("cran_explorer")
```


The data is stored in the cran_explorer/ directory in two files: packages.csv and deps.csv. To update these files, run this from the top-level directory:

```R
source("cran_explorer/utils.R")
json <- download_crandb()
crandb_data <- process_crandb_json(json)
write_csv(crandb_data$packages, "cran_explorer/packages.csv")
write_csv(crandb_data$deps, "cran_explorer/deps.csv")
```

In the application, there is a "Refresh Data" button. It will download new data, but it will *not* save it over the .rds files. This is to make it easier to demonstrate that the update really occurs -- the saved .rds files should be from an earlier day than the demonstration. (In the future, it might be useful to make it possible to save to disk.)


## Saving raw JSON data

The JSON data downloaded from crandb is about 140 MB, and is by default not saved to disk -- only the processed data is saved, as tibbles in .rds files. If you want to save the raw JSON for other forms of processing, you can do the following:

```R
# Save the raw JSON
json <- download_crandb(outfile = "all.json")

# Save gzipped JSON
json <- download_crandb(outfile = "all.json.gz")

# Download only the first 1000 packages from crandb
json <- download_crandb(outfile = "all.json.gz", limit = 1000)
```

The `process_crandb_json()` function can take the name of a JSON (or gzipped JSON) file to process:

```R
crandb_data <- process_crandb_json(filename = "all.json.gz")
```
