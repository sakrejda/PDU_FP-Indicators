
DHSMasterFile <- "./dhs/DHSMaster_CC0711.csv"
PDUCodePath <- "./PDU_FP-Indicators/DHS/Rcode"
translationTableFile <- "./PDU_FP-Indicators/DHS/TranslationTables/DHS_VariableTranslation_v312.csv"
outputPath <- "kenya-output"
dataPath <- "kenya-dhs"

if (!dir.exists(outputPath))
  dir.create(outputPath)

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

#Calculate Unmet Estimates
## v213: Currently Pregnant
## v225: Wantedness of Current Pregnancy
## v312: Current CP Method
## m10_1: Wantedness of Last Child
## m6_1: PPA Duration
## v605: Wantedness of Future Child
## b3_01: Date of last Birth
## v008: Date of Interview

# FIXME: The "Unmet" function seems to require more than this so... huh?
requiredUnmetVar <- c("v213","v225","v312","m10.1","m6.1","v605","b3.01","v008")

# Load in DHS inventory list for standard surveys and individual record is available
# Only surveys that appear in this file are processed b/c we need survey metadata
# for processing.
DHSMaster <- read.csv(DHSMasterFile, header=TRUE, 
    stringsAsFactors=FALSE, na.strings=c("..", "NA", "", " ")) %>% 
  dplyr::filter(!is.na(Survey.code), !is.na(Individual.Recode), !is.na(Recode),
    (Type!="MIS" & Type=="Standard DHS" | Type=="Interim DHS" | Type=="Continuous DHS"), 
    Survey.code != "br21")

#' This is where survey-specific values for contraceptive type are recoded more
#' generally.
translationTable <- read.csv(translationTableFile, header=TRUE, stringsAsFactors=FALSE)

# Variables needed for calcuation of contraceptive prevalence and unmet 
# need (To speed up working with RData files)
varToKeep<-c("v000", "v020", "v007", "v502", "v605", "v312", "v215", "v213", 
  "m6.1", "v225", "v527", "v528", "v005", "v013", "b3_01", "v536", "v512", 
  "v302", "v375a", "v376", "s607d", "s607c", "v3a08d", "v602", "v001", "v002", 
  "v222", "m10_1", "v008")


# How the new contraceptive classes (from translation tables) relate
# to the few categories we use for modeling.
methodClassification <- list(
  modern = c(100:108, 110:112, 120:122, 130:136, 140, 150, 160),
  traditional = c(141, 200, 210:217, 220, 230:236),
  other = c(300:304),
  notusing = c(999), 
  exclude = c(997, 998)
)
  
