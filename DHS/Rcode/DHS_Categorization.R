## R scripts to harmonise family planning variable and estimate family planning indicators by marital status and age from DHS micro-data files
# 1. 'DHS_Translate.R' Translates relevant variables across surveys and stores harmonised variable names and codes as R data sets. Based on IPUMS-DHS (https://www.idhsdata.org/idhs/).
# 2. 'DHS_Categorization.R' Computes marital status and contraceptive use variables
# 3. 'DHS_GenerateUnmet.R' Computes unmet need variable based on DHS code [http://dhsprogram.com/topics/unmet-need.cfm]
# 4. 'DHS_output_FP-Indicators.R' Outputs table of family planning indicators by marital status and age
## Author: United Nations Population Division (Ching Yee Lin, Philipp Ueffing, Stephen Kisambira and Aisha Dasgupta)
## Project: Making family planning count 
# [http://www.un.org/en/development/desa/population/projects/making-family-planning-count/index.shtml]
# [http://www.un.org/en/development/desa/population/theme/family-planning/index.shtml]
## DHS micro data sets need to be downloaded from the DHS program website [https://dhsprogram.com/]

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
labelVariables <- function(data, label_file = 'DHS_Labels.R'){
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
 

# Compute contraceptive method categories - for calculation of standard error
Categorization_SE <- function(ir.data){
  ir.data <- defineMaritalCategories(ir.data)

  ir.data$anymethod <- ifelse(ir.data$method %in% c(1,2), 1, 0) 
  ir.data$tradmethod <- ifelse(ir.data$method == 2, 1, 0)
  ir.data$modernmethod <- ifelse(ir.data$method == 1, 1, 0)
  ir.data$unmettot <- ir.data$unmettot
  ir.data$maritalstatus <- ifelse(ir.data$mstatusBinary == 1, 1, 0)
  
  return (ir.data)
}
