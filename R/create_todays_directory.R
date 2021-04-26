CreateOutputDirectory <- function(dirname_vec, add_today_folder = TRUE) {

  # set directory names
  if (add_today_folder) {
    dirname_vec <- c(dirname_vec, gsub('-', '', Sys.Date()))
  }

  dirname_i <- NULL
  for (i in 1:length(dirname_vec)) {

    # create full path
    if (is.null(dirname_i)) {
      dirname_i <- dirname_vec[[i]]
    } else {
      dirname_i <- file.path(dirname_i, dirname_vec[[i]])
    }

    # create directory
    dir.create(dirname_i, showWarnings = FALSE)

  }

  return(dirname_i)

}
