reject_na <- function(x) ifelse(is.na(x), FALSE, x)

must_contain <- function(data, s, target) {
  if (isTRUE(all(s %in% names(data)))) {
    return(TRUE)
  } else {
    s <- s[!(s %in% names(data))]
    msg <- paste0("Data is missing columns required",
      " to calculate target: '", target, "'.\n")
    msg <- paste0(msg, "Missing columns are:\n")
    msg <- paste0(msg, paste0("  ", s, collapse="\n"))
    stop(msg)
  } 
}

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

#' Generate Time since Last Birth
#' b3.01 = Date of last birth (CMC)
#' v008 = Date of Interview (CMC)
#
#' DHS April Update: tsinceb now calculated using v222 which is based on century 
#' day codes in DHS7
#'
#' @param data data frame 
#' @return input data frame with a `tsinceb` column added
insertTimeSinceBirth <- function(data) {
  must_contain(data, 'v000', 'time since birth')
  if (all(data$v000 < 7)) {
    must_contain(data, c('v008', 'b3.01'), 'time since birth')
    data$tsinceb = data$v008 - data$b3.01
  } else {
    must_contain(data, 'v222', 'time since birth')
    data$tsinceb = data$v222
  }
  return(data)
}


#' Generate Time since Last Period in Months
#' From DHS recode manual:
#' V215: Time since last menstrual period as reported by respondent
#'       The first digit gives the units in which the response
#'       was given by the respondent: 1 - Days ago, 2 - Weeks ago,
#'       3 - Months ago, 4 - Years ago, 9 - special answers.
#'       The last two digits give the time since the last period
#'       in those units.  If the last two digits contain a number
#'       greater than 90 the response is not a count of time units
#'       but a special code.
insertTimeSincePeriod <- function(data) {
  response_type <- substr(data$v215, 1, 1)
  time <- substr(data$v215, 2, 3)
  miscoded <- nchar(data$v215) != 3
  special <- time > 90 | miscoded | response_type == 9
  in_days <- ((response_type == 1) & !special) %>% reject_na()
  in_weeks <- ((response_type == 2) & !special) %>% reject_na()
  in_months <- ((response_type == 3) & !special) %>% reject_na()
  in_years <- ((response_type == 3) & !special) %>% reject_na()
  data$tsincep <- NA
  data$tsincep[in_days] <- data$v215[in_days] / 30
  data$tsincep[in_weeks] <- data$v215[in_weeks] / 4.3
  data$tsincep[in_months] <- data$v215[in_months] 
  data$tsincep[in_years] <- data$v215[in_years] * 12
  return(data)
} 

#' Calculate an indicator for women who are pregnant or
#' post-partum amenorrheic (PPA).  The indicator is either
#' NA or 1 (TRUE) it is NEVER 0 (FALSE).
#' 
#' Source columns are:
#' Initialize Pregnant/Postpartum Amenorrheic women
#'   v213 = Currently Pregnant:
#'     0 - No/Unsure, 1 - Yes
#'   m6.1 = Duration of PPA after Birth of last child in month:
#'     96 = Period Not Returned, 
#'     97 = Inconsitent data
#'     98 = Don't know
#' 
#' @param data data frame
#' @return input data frame with a 'pregPPA' 
#'         column added.
insertPPPA <- function(data, surveyCode) {
  if (surveyCode == 'ke03') {  ## Added based on dta-file labels
    data$v213 <- NA
    data$v213[!is.na(data$v215) & data$v215 == 994] <- 1
  }
  
  data$pregPPA <- NA
  data$pregPPA[data$v213 == 1] <- 1
  if ('m6.1' %in% colnames(data)) {
    data$pregPPA[data$m6.1 == 96] <- 1
  }
  
  # For women with missing data or "Period not Returned" 
  # as date of last menstrual, we use secondary data to
  # derive those classified as PPA.
  if (all(c('m6.1', 'tsinceb') %in% colnames(data))) {
    data$pregPPA[
      (is.na(data$m6.1) | data$m6.1==97 | data$m6.1==99) & 
      (data$tsincep > data$tsinceb) & 
       data$tsinceb < 60 & 
      !is.na(data$tsincep) & !is.na(data$tsinceb)
    ] <- 1
  
    # "Before Last Birth" as time since last period in the last 5 years
    data$pregPPA[
      (is.na(data$m6.1) | data$m6.1 == 97 | data$m6.1 == 99) & 
       data$v215 == 995 & 
       data$tsinceb < 60 & 
      !is.na(data$tsinceb)
    ] <- 1
  }
  return(data)
}

#' Calculate whether the last pregnancy was wanted at the time
#'
#' Uses either an IR question or a maternity history question
#' (in that order).
#'
#' @param data data frame
#' @return input data frame with a 'pregPPA' 
#'         column added.
insertWantedLast <- function(data, surveyID) {
  if (surveyID %in% c('ke42', 'ke52'))
    data[['m10.1']] <- data[['m10_1']]

  must_contain(data, c('v225', 'v000', 'm10.1'), 'wantedlast')
  # Classification of wantedness of current / last birth
  data$wantedlast <- NA
  data$wantedlast <- data$v225    ## Based on the current pregnancy
  
  # Based on maternity history
  indexWL <- 
    (is.na(data$wantedlast) | data$wantedlast==9) &  # not yet calculated
    (data$v213 != 1 | is.na(data$v213))              # currently pregnant or don't know
  data$wantedlast[indexWL] <- data$m10.1[indexWL]    # answer from maternity survey
  rm(indexWL)
  
  # Special Survey 
  if (surveyID %in% c("ci35", "md21", "ni22")) {
    data$wantedlast <- mapvalues(data$wantedlast, from = c(4,8), to = c(2,2))
  }
  
  if (surveyID == "ni22") {
    data$wantedlast[data$v000 == 4] <- 1
  }
  
  return(data) 
}























