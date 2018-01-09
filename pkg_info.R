itemHTML <- function(desc) {
  div(class = "item_desc", if (is.na(desc)) "unavailable" else desc)
}

contruct_row <- function(item) {
  column(3, h4(class = "item_name", item))
}

pkg_info_row <- function(row, dat) {
  items <- dat[1, row:(row + 3)]
  div(class = "row-container", 
    tagList(
      fluidRow(
        lapply(names(items), contruct_row)
      ),
      lapply(1:4, function(slot) {
        column(3,
          itemHTML(items[[1, slot]])
        )
      })
    )
  )
}

pkg_info_table <- function(dat) {
  div(id = "pkg_info",
    h3("Package Info"),
    lapply(c(1,5,9,13), pkg_info_row, dat)
  )
}