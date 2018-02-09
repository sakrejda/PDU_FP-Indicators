

individualRecode <- function(surveyRecode) {
  countryCode <- substr(surveyRecode, 1, 2)
  roundRelease <- substr(surveyRecode, 3, 4)
  ir <- paste0(countryCode, "ir", roundRelease)
  return(ir)
}

individualRecodeFile <- function(surveyRecode) {
  individualRecode <- individualRecode(surveyRecode)
  irFile <- paste0(toupper(individualRecode), "FL.DTA")
  return(irFile)
}

DHSReadDTA <- function(dataPath, surveyCode, keep) {
  file <- file.path(dataPath, individualRecodeFile(surveyCode))
  if (!file.exists(file)) 
    return(NULL)
  data <- haven::read_dta(file)
  names(data) <- tolower(names(data))
  data <- data[,intersect(keep, names(data))]
  return(data)
}


