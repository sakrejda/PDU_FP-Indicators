
Unmet <- function(data, surveyID, specialSurveys) {
  data$unmet <- NA
  
  # This whole if-clause is reasons why unmet need is NA.  Could just
  # use these to filter out the surveys ahead of time (?)
  if(surveyID %in% specialSurveys){
    # Apparently in 'special' surveys there are reasons why 
    # the 'wantedlast' should be NA (therefore also why 
    # 'unmet' should be NA.
    index1 <- (is.na(data$unmet) & data$v502!=1 & 
      (data$v020==1 | substr(data$v000,3,3)=="2" | 
       data$v000=="MA4" | 
       data$v000=="TR2" | 
      (data$v000=="CI3" & data$v007==94) | 
       data$v000=="HT3" | 
       data$v000=="IA5" | 
       data$v000=="NP5")
    )
    data$unmet[index1] <- mapvalues(data$unmet[index1], from=NA, to=98)
  } else {
    # Set unmet need to NA for unmarried women if survey only 
    # included ever-married women or only collected necessary data 
    # for married women
    if (all(c('v502', 'v020') %in% colnames(data)))
      data$unmet[which(data$v502 != 1 & data$v020 == 1)] <- 98
  }
 
  ##############################################################################  
  # GROUP 1: CONTRACEPTIVE USERS 
  #
  # Using to limit if wants no more, sterilized or declared infecud
  #  v312 = Current contraceptive method
  #   0 - Not using, 
  #   6 - Female Sterilization, 
  #   7 - Male Sterilization, 
  #  13 - Lactational Amenorrhea
  #
  #  v605 = Desire for more children
  #   1 - Wants within 2 years, 
  #   5 - Wants no more, 
  #   6 - Sterilized, 
  #   7 - Declared Infecud
  #
  data$unmet[
    is.na(data$unmet) & # not yet calculated
    data$v312 != 0 &    # using contraception
    data$v605 >= 5 &    # wants no more, sterilized or infecund
    data$v605 <= 7      # but not.... (?)
  ] <- 4                   # this is the definition of limiting (unmet == 4)
  
  # Using to space = All other contraceptive users
  data$unmet[
     is.na(data$unmet) & # not yet calculated
    (data$v312 != 0 | is.na(data$v312)) # using, or don't know if using FIXME: HUH?
  ] <- 3                    # this is the definitio of spacing
  
  ############################################################################  
  # GROUP 2: PREGNANT or POSTPARTUM AMENORRHEIC (PPA) WOMEN 
  #
  data <- insertTimeSinceBirth(data)
  data <- insertTimeSincePeriod(data)
  data <- insertPPPA(data, surveyID) # pregnant or post-partum amenorheic
  
  # Select only women who are pregnant or PPA <24Months
  data$pregPPA24 <- NA
  data$pregPPA24[data$pregPPA == 1 & data$tsinceb < 24] <- 1
 
  data <- insertWantedLast(data, surveyID) 
  
  data$unmet[
    is.na(data$unmet)    &  # don't know if it's unmet need
    data$pregPPA24 == 1  &  # pregnant or was within 2 yrs.
    data$wantedlast == 1    # wanted last/current pregnancy
  ] <- 7  # No unmet need.
  
  # Unmet Need for Spacing
  # If wanted current pregnant/ last birth later
  data$unmet[
    is.na(data$unmet)    &  # don't know if it's unmet need
    data$pregPPA24 == 1  &  # pregnant within 2 years
    data$wantedlast == 2    # wanted but later 
  ] <- 1   # Unmet need for spacing.
  
  # Unmet Need for Limiting
  # If wanted current/last birth not at all
  data$unmet[
    is.na(data$unmet)    & # don't know if it's unmet need yet
    data$pregPPA24 == 1  & # pregnant or PPA for 2yrs
    data$wantedlast == 3   # did not want last pregnancy
  ] <- 2   # Unmet need unmet need for limiting.
  
  #Missing data on watedness of current/last birth
  data$unmet[
    is.na(data$unmet) &   # don't know if it's unmet need yet
    data$pregPPA24 == 1 &   # # pregenant or PPA for 2 years
    (is.na(data$wantedlast) | data$wantedlast==9)  # don't know if was wanted
  ] <- 99  # code for NA.
  
  # Determine Sexually Active women in Last 30 Days
  # No Unmet Need for unmarried women who are not sexually active
  data$sexact <- NA
  
  # Special Survey
  # Sexually Active Defined as 1 month
  if (surveyID == "bf21") {
    data$sexact[data$v527 >= 0 & data$v527 < 300] <- 1 
    data$sexact[data$v527 > 300] <- 2
  } else {
    data$sexact[data$v528 >= 0 & data$v528 <= 30] <- 1
    if (surveyID %in% specialSurveys) {
      data$sexact[is.na(data$sexact) & data$v528 == 95] <- 1
    } else {
      data$sexact[data$v528 > 30] <- 2
      data$sexact[data$v536 == 0] <- 3
    }
  }

  # No Unmet need
  data$unmet[
    is.na(data$unmet) &                           # don't know yet
   (data$v502 != 1 | is.na(data$v502)) &       # ever-married (or lived w/ partner
   (data$sexact != 1 | is.na(data$sexact))     # not known sexually active
  ] <- 97    # Code for no unmet need FIXME: 7 is also a code for this... (????)


  ##############################################################################
  # GROUP 3: DETERMINE FECUNDITY (Boxes refer to Figure 2 flowchart in report)    

  # Box 1 - Applicable only to Currently Married Women #
  # Married 5+ years ago, no children in past 5 years, never used contraception
  # Excluding pregnant and PPA <24 Months
  data$infec <- NA   ## Infecund, will be either 1 (infecund) or NA (don't know)
  
  if (surveyID == "kh61") {
    data$infec[
      data$v502==1 & 
      data$v512>=5 & 
     !is.na(data$v512) & 
     (data$tsinceb>59 | is.na(data$tsinceb)) & 
      data$s313==0 & 
     (data$pregPPA24!=1 | is.na(data$pregPPA24)) & 
     (data$v007==2010 | data$v007==2011)
    ] <- 1   # Infecund
  } else if (surveyID == "tz60") {
    data$infec[
      data$v502 == 1 & 
      data$v512 >= 5 & 
     !is.na(data$v512) & 
     (data$tsinceb > 59 | is.na(data$tsinceb)) & 
      data$s309b == 0 & 
     (data$pregPPA24 != 1 | is.na(data$pregPPA24)) & 
     (data$v007 == 2009 | data$v007 == 2010)
    ] <- 1   # Infecund
  } else {
    # DHS Update April 2017: checks for v000 now look for "7"
    data$infec[
      !substr(data$v000,3,3) %in% c("6","7") & 
       data$v502==1 & 
       data$v512>=5 & 
      !is.na(data$v512) & 
      (data$tsinceb>59 | is.na(data$tsinceb)) & 
       data$v302==0 &
      (data$pregPPA24!=1 | is.na(data$pregPPA24))
    ] <- 1   # Infecund
    
    # Pakistan round 6 DHS does still have v302 as name and not v302a 
    # (Not specified in DHS code because differently coded)
    if(surveyID == "pk61") {
      data$v302a <- data$v302
    }
    
    data$infec[
      substr(data$v000,3,3) %in% c("6","7") & 
      data$v502 == 1 & 
      data$v512>=5 & 
     !is.na(data$v512) & 
      (data$tsinceb>59 | is.na(data$tsinceb)) & 
       data$v302a==0 &
      (data$pregPPA24!=1 | is.na(data$pregPPA24))
    ] <- 1   # Infecund
    # FIXME: The rampant use of alternative codes for NA is killing  
    #        this code.
  }
  
  # Box 2 
  # Declare infecund on future desires for children
  data$infec[data$v605 == 7] <- 1
  
  # Box 3 
  # Menopausal/Hysterectomy as the reason of not using contraception
  
  ####Special Surveys####
  if (surveyID %in% c("br31","gu34","gu41")) {
    data$infec[
      is.na(data$infec) & 
     (data$v375a==23 | data$v375a==28)
    ] <- 1 
  } else if (surveyID == "ci35") {
    data$infec[
      is.na(data$infec) & 
      data$v007 == 94 & 
      data$v376 == 23
    ] <- 1
  } else if (surveyID == "ga41") {
    data$infec[
      is.na(data$infec) & 
      data$s607d==1
    ] <- 1
  } else if (surveyID == "ht31") {
    data$infec[
      is.na(data$infec) & 
      data$v376 == 23
    ] <- 1
  } else if(surveyID == "jo42") {
    data$infec[
      is.na(data$infec) & 
     (data$v376 == 23 | data$v376 == 24)
    ] <- 1
  } else if (surveyID %in% c("kk42","tz3a")) {
    data$infec[
      is.na(data$infec) & 
     (data$v007 == 99 & data$s607d == 1)
    ] <- 1
  } else if (surveyID == "mv50") {
    data$infec[
      is.na(data$infec) & 
      data$v376 == 23
    ] <- 1
  } else if (surveyID == "mr42") {
    data$infec[
      is.na(data$infec) & 
      data$s607c == 1
    ] <- 1
  } else if (surveyID == "tr4a") {
    data$infec[
      is.na(data$infec) & 
      data$v375a == 23
    ] <- 1
  } else {
    # DHS IV+ Surveys
    data$infec[
      substr(data$v000, 3, 3) %in% c("4","5","6","7") & 
      data$v3a08d == 1
    ] <- 1
    
    # DHS III Surveys
    data$infec[
      substr(data$v000, 3, 3) %in% c("3","T") & 
      data$v375a == 23
    ] <- 1
    
    # DHS II Surveys
    # Reason not using contraception does not exists in DHSII
    # Use Reason Not Intending to use In Future
    data$infec[
      substr(data$v000, 3, 3) == "2" & 
      data$v376 == 14
    ]<-1
  }
  
  # Box 4 
  # Time since Last Period >=6 Months and not PPA
  data$infec[
    data$tsincep>=6 & 
   !is.na(data$tsincep) & 
   (is.na(data$pregPPA) | data$pregPPA!=1)
  ] <- 1   
  
  # Box 5 
  # Menopausal/Hysterectomy on Time Since Last Period
  if (surveyID %in% c("tr41", "ug33", "ye21")) {
    data$infec[
      is.na(data$infec) & 
      data$v215 == 993
    ] <- 1
  } else {
    data$infec[data$v215 == 994] <- 1
  }
  
  # Never Menstruated on Time Since Last birth, unless had a birth in the last 5 years
  data$infec[
    data$v215 == 996 & 
   (data$tsinceb > 59 | is.na(data$tsinceb))
  ] <- 1
  
  # Box 6
  # Time Since Last Birth >=60 Months and last period was before last birth
  data$infec[
    data$v215 == 995 & 
    data$tsinceb>=60 & !is.na(data$tsinceb)
  ] <- 1
  
  # Never had Birth, but last period reported as before last birth
  # Assume code should have been 994/996
  data$infec[
    data$v215==995 & 
    is.na(data$tsinceb) 
  ] <- 1
  
  # Exclude pregnant & PPA < 24 months
  data$infec[data$pregPPA24  ==1 ] <- NA

  # Infecund women have no unmet need for contraception
  data$unmet[is.na(data$unmet) & data$infec == 1] <- 9
 
  ##############################################################################
  # GROUP 4: NOT INFECUND WOMEN 

  # Wants within 2 years.
  if (surveyID == "ia23") {
    data$unmet[
      is.na(data$unmet) & 
      data$v602 == 6
    ] <- 7
  } else if (surveyID == "ia42") {
    data$unmet[
      is.na(data$unmet) & data$v605==9
    ] <-7
  } else {
    data$unmet[
      is.na(data$unmet) & data$v605==1
    ] <- 7
  }

  # wants in 2+ years, wants undecided timing, or unsure if wants
  if (surveyID == "ls60") {
    data$v605[is.na(data$v605)] <- 4
  }
  data$unmet[
    is.na(data$unmet) & 
    data$v605 >= 2 & 
    data$v605 <= 4 
  ] <- 1

  # wants no more
  data$unmet[
    is.na(data$unmet) & 
    data$v605 == 5
  ] <-2

  data$unmet[is.na(data$unmet)] <- 99   # This could just stay NA... :/
  
  # Special Surveys
  if (surveyID == "tr4a") {   ##FIXME: WHAT??
    data <- subset(data, data$v001 %% 2 == data$v002 %% 2)
  }
  
  # Total Unmet Need
  data$specific_unmet <- data$unmet
  data$specific_unmet[
    !data$specific_unmet %in% c(1,2)   ## Apparently these two categories are 'specific unmet'
  ] <- 0
  
  data$unmettot <- NA
  data$unmettot[  data$unmet %in% c(1,2) ] <- 1
  data$unmettot[!(data$unmet %in% c(1,2))] <- 0
  
  # Kenya 2014 is not included in DHS list of special surveys, but has long and short questionnaire.
  # Only short questionnaire included questions on fertility preferences. Therefore limit universe.
  # We use desire for children variable and exclude missings from universe
  # FIXME: what is a Universe?
  if (surveyID == "ke70") {
    for (i in c("unmet","unmettot","specific_unmet")) {
      data[,i][is.na(data$v605)]<-NA
    }
  }
  
  return (data)
}
