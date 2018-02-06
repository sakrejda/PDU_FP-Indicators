
#############LIBRARIES##############
library(stats)
library(dplyr)
#library(reshape2)  <- this is really outdated, what is it even used for?
library(tidyr)
library(tools)
####################################

#  rm(list = ls())  <- if your code needs this to run you're skating on thin ice.

# these are utility functions
source("./Rcode/DHS_config.R")
source("./Rcode/DHS_readData.R")
source("./Rcode/DHS_methodClassification.R")
source("./Rcode/DHS_generateUnmet.R")
source("./Rcode/DHS_categorization.R")
source("./Rcode/DHS_translate.R")
source("./Rcode/DHS_transform.R")
source("./Rcode/DHS_output.R")


#####DIRECTORIES AND FILE LISTS#####
#Load in DHS inventory list for standard surveys and individual record is available
DHSMaster <- read.csv(DHSMasterFile, header=TRUE, 
  stringsAsFactors=FALSE, na.strings=c("..", "NA", "", " "))

DHSMaster <- DHSMaster %>% dplyr::filter(!is.na(Survey.code), !is.na(Individual.Recode), !is.na(Recode),
  (Type!="MIS" & Type=="Standard DHS" | Type=="Interim DHS" | Type=="Continuous DHS"), 
  Survey.code != "br21")


#List of translated surveys
translated.list <- file_path_sans_ext(dir("./Translated_RDataFiles",pattern=".RData"))
#Retain the translated surveys that exist in DHSMaster
working.list <- translated.list[translated.list %in% DHSMaster$Individual.Recode | translated.list %in% DHSMaster$Survey.code]
translationTable <- read.csv(translationTableFile, header=TRUE, stringsAsFactors=FALSE)
file.list <- FileSetting()

####################################

# Writes one .RData file per survey...
translateSurveys(translationTable, outputPath, dataPath)

for(SurveyID in working.list) {
  SurveyRDataFile <- file.path(output_dir, paste(SurveyID, ".RData"))
  print (SurveyID)
  
  SurveyInfo <- subset(DHSMaster, Survey.code == SurveyID)
  load(SurveyRDataFile)
  
  if(SurveyID %in% specialSurveys){
    keepColumnRegex <- paste(specialSurveyColumns, sep='|')
  } else {
    keepColumnRegex <- paste(ordinarySurveyColumns, sep='|')
  }
# FIXME: Processes those .RData files I guess.... where did ir.data come 
# from here???  Did I delete loading it?
  ir.data <- ir.data %>% dplyr::select(dplyr::matches(keepColumnRegex))
  
 # FIXME: Unmet() function should check for these and dump NA's if not available. 
  if(requiredUnmetVar %in% names(ir.data)) {
    ir.data <- Unmet(ir.data)
  } else {
    ir.data$unmettot <- NA
    ir.data$unmet <- NA
    ir.data$sexact <-NA
    ir.data$specific_unmet<- NA
  }

  #Categorize Marital Status, Age 
  ir.data <- calculateNewVariables(ir.data)
  
  #Calculate CP Estimates
  tTYPE <- CP_OUTPUT("Both")
  tMETH <- CP_METH("Both")
  
  tTYPE$totalDemand <- rowSums(tTYPE[,c("cpAny","Unmet")],na.rm=T)
 
  
  # FIXME: RESOLVE: Does this Publish/Exclude stuff matter to us?
 
  #WMCUMA = Indicator of whether sample size of unmarried for 15-49 < 10
  ## <10 = Exclude
  ## >=10 = Publish
  tTYPE$`WCUMA_nAny_totalAge_<10` <- NA
  
  if(any(tTYPE$Universe == "Ever married" | !"Unmarried/Not-in-union" %in% tTYPE$mstatus)){
    if(any(tTYPE$nAny_unweighted[which(tTYPE$mstatus == "Formerly married" & tTYPE$AGE5YEAR_LAB == "[Total]")]<10)){
      tTYPE$`WCUMA_nAny_totalAge_<10`[which(tTYPE$mstatus=="Formerly married" | tTYPE$mstatus=="Never marred" | tTYPE$mstats == "AllWomen")] <- "Exculde"
    }
  }else if(any(tTYPE$nAny_unweighted[which(tTYPE$mstatus=="Unmarried/Not-in-union" & tTYPE$agegroup=="[Total]")] < 10)){
    tTYPE$`WCUMA_nAny_totalAge_<10`[which(tTYPE$mstatus == "Unmarried/Not-in-union" | tTYPE$mstatus == "Formerly in-union" | tTYPE$mstatus == "Neverin-union" | tTYPE$mstats == "AllWomen")] <- "Exclude"
  }
  tTYPE$`WCUMA_nAny_totalAge_<10`[which(is.na(tTYPE$`WCUMA_nAny_totalAge_<10`))] <- "Publish"
  
  #WCUMA_n<50 = Indicator of whether sample size of any age and marital group < 50
  ## <50 = Exclude
  ## >=50 = Publish
  tTYPE$`WCUMA_n<50` <- NA
  tTYPE$`WCUMA_n<50`[which(tTYPE$n_unweighted <50)] <- "Exclude"
  tTYPE$`WCUMA_n<50`[which(tTYPE$n_unweighted >=50)] <- "Publish"
  
  #WCUMA_totalDemand<5 = Indicator of whether total demand of any age and marital group < 5
  ## <5 = Exclude
  ## >=5 = Publish
  tTYPE$`WCUMA_totalDemand<5` <- NA
  tTYPE$`WCUMA_totalDemand<5`[which(tTYPE$totalDemand <5)] <- "Exclude"
  tTYPE$`WCUMA_totalDemand<5`[which(tTYPE$totalDemand >=5)] <- "Publish"
  
  Output(tTYPE,tMETH)
}

Transform(file.list)
