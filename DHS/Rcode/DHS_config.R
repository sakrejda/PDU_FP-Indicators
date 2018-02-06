DHSMasterFile <- "DHSMaster.csv"

## List of special surveys need special unmet need calculation
## K: Term "special" is ambiguous, how are we defining a "special" survey?
specialSurveys <- c("br31", "kh61", "co60", "ci35", "ga41", "co60", "ci35", "ga41",
                    "gu34", "gu41" , "ht31" , "ia23" , "ia42", "ia52" , "jo42" , "kk42" ,
                    "ls60", "md21" , "mv50" , "mr42" , "ma43", "np51" , "ni22" , "tz3a" ,
                    "tz41", "tz60" , "tr31" , "tr4a" , "ug33", "ye21") 

specialSurveyColumns <- c("v000", "v001", "v002", "v003", "v005", "v007", "v008", 
  "v011", "v012", "v013", "v015", "v016", "v020", "v021", "v022", "v023", "v024", 
  "v025", "v213", "v215", "v222", "v225", "v302", "v3a08d", "v302a", "v312", "v313", 
  "v375a", "v376", "v512", "v525", "v528", "v529", "v536", "v502", "v602", "v605", 
  "b3.01", "m6.1", "m10.1", "s313", "s309b", "s607c", "s607d", 
  "method", "methodSpecific", "weights")

ordinarySurveyColumns <- c("v000", "v001", "v002", "v003", "v005", "V007", "v008", 
  "v011", "v012", "v013", "v015", "v016", "v020", "v021", "v022", "v023", "v024", 
  "v025", "v213", "v215", "v222", "v225", "v302", "v3a08d", "v302a", "v312", "v313", 
  "v375a", "v376", "v512", "v525", "V527", "v528", "v529", "v536", "v501", "v502", 
  "v602", "v605", "v613", "v614", "b3.01", "m6.1", "m10.1", "s313", 
  "awfactt", "awfactu", "awfactr", "awfacte", "awfactw", "method", "methodSpecific", 
  "weights")

translationTable <- "TranslationTables/DHS_VariableTranslation_v312.csv"
outputPath <- "output"
dataPath <- "data"

