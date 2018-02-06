

# PDU_FP-Indicators
Family planning indicators by marital status and age from MICS and DHS micro-data sets

This repository contains tools for harmonising relevant variables and calculating family planning indicators by marital status and age from MICS and DHS micro-data sets. These tools were developed as part of the 'Making Family Planning Count' project by the UN Population Division and financed by the Bill and Melinda Gates Foundation. This work is licensed under the Creative Commons Attribution-NonCommercial 4.0 International License (CC BY-NC 4.0). If you detect a bug or have a suggestion please notify us using the Issues tab on github.

Note: MICS (http://mics.unicef.org/) and DHS (https://www.dhsprogram.com/) micro-data sets need to be obtained from the providers in order to use the tools.

## Original file header: 

Should restore this once the shuffle is done.

 R scripts to harmonise family planning variable and estimate family planning indicators by marital status and age from DHS micro-data files
1. 'DHS_Translate.R' Translates relevant variables across surveys and stores harmonised variable names and codes as R data sets. Based on IPUMS-DHS (https://www.idhsdata.org/idhs/).
2. 'DHS_Categorization.R' Computes marital status and contraceptive use variables
3. 'DHS_GenerateUnmet.R' Computes unmet need variable based on DHS code [http://dhsprogram.com/topics/unmet-need.cfm]
4. 'DHS_output_FP-Indicators.R' Outputs table of family planning indicators by marital status and age
 Author: United Nations Population Division (Ching Yee Lin, Philipp Ueffing, Stephen Kisambira and Aisha Dasgupta)
 Project: Making family planning count 
[http://www.un.org/en/development/desa/population/projects/making-family-planning-count/index.shtml]
[http://www.un.org/en/development/desa/population/theme/family-planning/index.shtml]
 DHS micro data sets need to be downloaded from the DHS program website [https://dhsprogram.com/]

