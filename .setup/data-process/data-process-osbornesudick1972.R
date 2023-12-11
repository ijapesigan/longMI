#' Data Process - `data/osbornesudick1972.rda`
#'
DataProcessOsborneSudick1972 <- function(overwrite = FALSE) {
  root <- rprojroot::is_rstudio_project
  data_folder <- root$find_file(
    "data"
  )
  if (!dir.exists(data_folder)) {
    dir.create(
      data_folder,
      recursive = TRUE
    )
  }
  osbornesudick1972_rda <- file.path(
    data_folder,
    "osbornesudick1972.rda"
  )
  if (!file.exists(osbornesudick1972_rda)) {
    write <- TRUE
  } else {
    if (overwrite) {
      write <- TRUE
    } else {
      write <- FALSE
    }
  }
  if (write) {
    osbornesudick1972 <- read.csv(
      root$find_file(
        ".setup",
        "data-raw",
        "osbornesudick1972.txt"
      )
    )
    save(
      osbornesudick1972,
      file = root$find_file(
        "data",
        "osbornesudick1972.rda"
      ),
      compress = FALSE
    )
  }
}
DataProcessOsborneSudick1972(overwrite = TRUE)
rm(DataProcessOsborneSudick1972)
