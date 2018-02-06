
#' Compute marital status categories
#'
#' Binary marital status categories are either:
#' a) Married / in-union; or
#' b) Unmarried / not-in-union
#' These are placed in the 'mstatusBinary' variable.
#' 
#' This function also defines a second 'mstatus' variable
#' that is:
#' a) 0 = never married
#' b) 2 = ...
#' c) 9 = ...
#' d) NA = NA _or_ married
#' 
#' @param ir.data the data frame we are adding the variables to.
#' @return ir.data with additional variables.
defineMaritalCategories <- function(ir.data){
  ir.data$mstatus <- ifelse(ir.data$v502 == 1, NA, ir.data$v502)

  ir.data$mstatusBinary <- NA
  ir.data$mstatusBinary[ir.data$v502 == 1] <- 1
  ir.data$mstatusBinary[ir.data$v502 == 9] <- 9
  ir.data$mstatusBinary[ir.data$v502 != 1 & ir.data$v502 != 9] <-2   #Remove Unmarried categories if formerly-married exists but not never-in-unin

  return(ir.data)
}

#' This function also defines an 'agegroup' variable that is
#' just v013 used directly
#' 
#' @param ir.data the data frame we are adding the variables to.
#' @return ir.data with additional variables.
addAgeGroup <- function(ir.data) ir.data %>% dplyr::mutate(agegroup = ir.data$v013)

#' Sources a separate file to create a list of labels.  Then applies
#' the list to convert implicit factors into explicit R factor type
#' with complete labels.
#' @param data a data frame to define the factors in.
#' @param label_file a regular R file that creates a list named 'label_list'
#'        This list must have named elements, one per variable to transform
#'        into a factor.  Each element is a list with two elements: 1) a
#'        vector of levels for the factor; and 2) a character vector of 
#'        corresponding labels.  Check DHS_Labels.R for an example.
labelVariables <- function(data, label_file = 'DHS_Labels.R') {
  data <- defineMaritalCategories(data) %>% addAgeGroup()
  e <- new.env()
  soure(label_file, local=e)
  label_these <- names(e$label_list)

  for (variable in label_these ) {
    data[[variable]] <- factor(x=data[[variable]], 
      levels = e$label_list[[variable]][['levels']],
      labels = e$label_list[[variable]][['labels']])
  }
  return (data)
}

#' Calculate variables related to unmet need, marital status, sexual activity and 
#' age group
#' 
#' @param ir.data data frame to add the variables to.
calculateNewVariables <- function(ir.data) ir.data %>% 
  defineMaritalCategories() %>% addAgeGroup() %>% labelVariables()
 

#' Compute contraceptive method categories used in
#' FPET modeling:
Categorization <- function(data) {
  data <- defineMaritalCategories(data)

  data$anymethod <- ifelse(data$method %in% c(1,2), 1, 0) 
  data$tradmethod <- ifelse(data$method == 2, 1, 0)
  data$modernmethod <- ifelse(data$method == 1, 1, 0)
  data$unmettot <- data$unmettot
  data$maritalstatus <- ifelse(data$mstatusBinary == 1, 1, 0)
  
  return (data)
}

#' Contraceptive modern and traditional method classification function
#' relies on translation table to map original survey-specific codes
#' to generic codes and the method classification to classify methods
#' into more general categories (modern, traditional, other, notusing, 
#' and exclude.
contraceptiveClassification <- function(data, methodClasses, translationTable){
  
  modern <- methodClasses[['modern']]
  traditional <- methodClasses[['traditional']] 
  other <- methodClasses[['other']]
  notusing <- methodClasses[['notusing']]
  exclude <- methodClasses[['exclude']]
  
  # Harmonized variables
  original <- as.numeric(gsub("=.*$", "", translationTable[ , dosurvey$Individual.Recode]))
  harmony <- translationTable[ ,"harmonised"]
  harmonised_LAB <- as.character(translationTable[ , "label"])
  harmonised_LAB <- sub("^\\s+", "", harmonised_LAB) #Trim off trailing spaces for subcategories of methods in translation table
  
  data$methodSpecific <- plyr::mapvalues(data$v312, from = original, to = harmony)
  data$methodSpecific[which(is.na(data$v312))] <- NA
  
  # Apply classification
  data$method <- NA
  data$method[which(data$methodSpecific %in% notusing)] <- 0
  data$method[which(data$methodSpecific %in% modern)] <- 1
  data$method[which(data$methodSpecific %in% traditional)] <- 2
  data$method[which(data$methodSpecific %in% other)] <- 2         
  data$method[which(data$methodSpecific %in% exclude)] <- 9
  
  data$methodSpecific_LAB <- plyr::mapvalues(x = data$methodSpecific, 
    from = harmony, to = harmonised_LAB)
  
  return (data)
}

