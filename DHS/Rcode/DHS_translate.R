
translateSurveys <- function(translationTable, outputPath, dataPath) {
  # Variables needed for calcuation of contraceptive prevalence and unmet need (To speed up working with RData files)
  varToKeep<-c("v000", "v020", "v007", "v502", "v605", "v312", "v215", "v213", "m6.1", "v225", "v527", "v528", "v005", "v013", "b3_01",
               "v536", "v512", "v302", "v375a", "v376", "s607d", "s607c", "v3a08d", "v602", "v001", "v002", "v222", "m10_1", "v008")
  Translated <- dir(outputPath, pattern = ".RData$") %>% gsub("\\.RData$", "", x=.)
  Untranslated <- subset(DHSMaster, !(Survey.code %in% Translated))
  if (nrow(Untranslated) > 0) {
    for (i in 1:nrow(Untranslated)) {
      SurveyID <- Untranslated[i,"Survey.code"]
      print (SurveyID)
      
      dosurvey <- subset(Untranslated, Survey.code == SurveyID)
      dofile <- file.path(dataPath, toupper(paste(dosurvey$Individual.Recode), "FL.DTA", sep=""))
      ir.data <- dhs_read_data_dta(dofile, keep = varToKeep)
      ir.data <- contraceptiveClassification(ir.data, methodClasses = methodClasses, translationTable = translationTable)
      
      save(ir.data, file = paste0("Translated_RDataFiles/",SurveyID,".RData"))
    }
  }
  print("All surveys are translated")
}

