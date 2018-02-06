dhs_read_data_dta <- function(file, keep) {
  data <- haven::read_dta(file)
  names(data) <- tolower(names(data))
  data <- data[,keep]
  return(data)
}


