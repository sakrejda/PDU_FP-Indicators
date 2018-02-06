label_list = list(
  method = list(
    description = "contraceptive use method",
    levels = c(0,1,2,9),
    labels = c("Not_using_any_method", "Using_modern_method", "Using_traditional_method", "Unknown")
  ),
  unmetneed = list(
    description = "unmet need for contraceptive methods, detailed codes",
    levels = c(1,2,3,4,7,9,97,98,99), 
    labels = c("unmet_need_for_spacing", "unmet_need_for_limiting", "using_for_spacing",
               "using_for_limiting", "no_unmet_need", "infecund_or_menopausal", 
               "not_sexually_active",
               "unmarried_EM_sample_or_no_data", "unmet need missing")
  ),
  unmettot = list(
    description = "unmet need for contraceptive methods, binary classification",
    levels = c(0,1), 
    labels = c("No_unmet_need", "Unmet_need" )
  ),
  specific_unmet = list(
    description = "unmet need for contraceptive methods, ternary classification",
    levels = c(0,1,2),
    labels = c("No_Unmet_Need","UnmetNeed_for_Spacing","UnmetNeed_for_Limiting")
  ),
  sexact = list(
    description = "sexual activity status",
    levels = c(1,2,3),
    labels = c("Sexually_active", "Sexually_inactive", "Never_had_sex")
  ),
  mstatus = list(
    description = "past marital status",
    levels = c(0,2,9),
    labels = c("Never married", "Formerly married", "Marital Status, Missing")
  ),
  mstatusBinary = list(
    description = "current marital status, binary classification",
    levels = c(1,2,9),
    labels = c("Married/In-union", "Unmarried/Not-in-union", "Marital Status, Missing")
  ),
  agegroup = list(
    description = "age bracket",
    levels = c(1,2,3,4,5,6,7), 
    labels = c("[15-19]", "[20-24]", "[25-29]", "[30-34]", "[35-39]", "[40-44]","[45-49]")
  )
)
