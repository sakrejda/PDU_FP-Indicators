
#############LIBRARIES##############
library(stats) # <- this IS needed for Rscript-driven runs.
library(dplyr)
library(tidyr)

#library(reshape2)  <- this is really outdated, what is it even used for?
#library(tools) ## <- why?
####################################

# these are utility functions

scripts <- c( "config.R", "utilities.R", "generateUnmet.R", 
  "categorization.R", "transform.R", "output.R")

script_paths <- file.path(PDUCodePath, scripts)

for (script in script_paths) source(script)

source_files <- dir(path='../source', pattern='\\.ZIP', full.names=TRUE, ignore.case=TRUE)
files_on_women <- pdhs::filter_file_names(source_files, latest=TRUE, country="KE", dataset="IR", 
  format = pdhs::get_file_format_code("Stata"))

pdhs::filtered_unzip(files_on_women, 'dta', dataPath)

dtaOnly <- dir(path=dataPath, pattern='^KEIR..FL\\.DTA', full.names=TRUE)
surveyCodes <- paste0(substr(basename(dtaOnly), 1,2), substr(basename(dtaOnly), 5,6)) %>%
  tolower

####################################

# Reclassify based on translation table and Write one .RData file per survey
# to the output directory.

for (surveyCode in surveyCodes) {
  outputFile <- file.path(outputPath, paste0(surveyCode, ".rds"))
  if (file.exists(outputFile)) {
    msg <- paste0("skipping '", surveyCode, "' due to file '", 
      outputFile, "' being present.")
    print(msg)
    next()
  }
  irData <- DHSReadDTA(dataPath, surveyCode, varToKeep)
  if (is.null(irData)) {
    msg <- paste0("input file for survey '", surveyCode, "', at path: '",
      dataPath, "', is not available. Skipping.")
    print(msg)
    next()
  }
  msg <- paste0("input file for survey '", surveyCode, "' found and output",
    " file not present, processing.")
  print(msg)
  reclassifiedData <- contraceptiveClassification(irData, methodClassification, 
    translationTable, surveyCode) %>% Unmet(surveyCode, specialSurveys)
 
  saveRDS(reclassifiedData, file = outputFile)
}


stop("This is as far as we need to go.")


for(SurveyID in translatedSurveysInMaster) {
  SurveyRDataFile <- file.path(output_dir, paste(SurveyID, ".RData"))
  print (SurveyID)
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

FileSetting() %>% Transform()
