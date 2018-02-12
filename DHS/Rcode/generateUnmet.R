
Unmet <- function(ir.data, surveyID, specialSurveys) {
  ir.data$unmet <- NA
  
  # This whole if-clause is reasons why unmet need is NA.  Could just
  # use these to filter out the surveys ahead of time (?)
  if(SurveyID %in% specialSurveys){
    # Apparently in 'special' surveys there are reasons why 
    # the 'wantedlast' should be NA (therefore also why 
    # 'unmet' should be NA.
    index1 <- (is.na(ir.data$unmet) & ir.data$v502!=1 & 
      (ir.data$v020==1 | substr(ir.data$v000,3,3)=="2" | 
       ir.data$v000=="MA4" | 
       ir.data$v000=="TR2" | 
      (ir.data$v000=="CI3" & ir.data$v007==94) | 
       ir.data$v000=="HT3" | 
       ir.data$v000=="IA5" | 
       ir.data$v000=="NP5")
    )
    ir.data$unmet[index1] <- mapvalues(ir.data$unmet[index1], from=NA, to=98)
  } else {
    # Set unmet need to NA for unmarried women if survey only 
    # included ever-married women or only collected necessary ir.data 
    # for married women
    ir.data$unmet[which(ir.data$v502 != 1 & ir.data$v020 == 1)] <- 98
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
  ir.data$unmet[
    is.na(ir.data$unmet) & # not yet calculated
    ir.data$v312 != 0 &    # using contraception
    ir.data$v605 >= 5 &    # wants no more, sterilized or infecund
    ir.data$v605 <= 7      # but not.... (?)
  ] <- 4                   # this is the definition of limiting (unmet == 4)
  
  # Using to space = All other contraceptive users
  ir.data$unmet[
     is.na(ir.data$unmet) & # not yet calculated
    (ir.data$v312 != 0 | is.na(ir.data$v312)) # using, or don't know if using FIXME: HUH?
  ] <- 3                    # this is the definitio of spacing
  
  ############################################################################  
  # GROUP 2: PREGNANT or POSTPARTUM AMENORRHEIC (PPA) WOMEN 
  #
  ir.data <- insertTimeSinceBirth(data)
  ir.data <- insertTimeSincePeriod(data)
  ir.data <- insertPPPA(data) # pregnant or post-partum amenorheic
  
  # Select only women who are pregnant or PPA <24Months
  ir.data$pregPPA24 <- NA
  ir.data$pregPPA24[ir.data$pregPPA == 1 & ir.data$tsinceb < 24] <- 1
 
  ir.data <- insertWantedLast(data) 
  
  ir.data$unmet[
    is.na(ir.data$unmet)    &  # don't know if it's unmet need
    ir.data$pregPPA24 == 1  &  # pregnant or was within 2 yrs.
    ir.data$wantedlast == 1    # wanted last/current pregnancy
  ] <- 7  # No unmet need.
  
  # Unmet Need for Spacing
  # If wanted current pregnant/ last birth later
  ir.data$unmet[
    is.na(ir.data$unmet)    &  # don't know if it's unmet need
    ir.data$pregPPA24 == 1  &  # pregnant within 2 years
    ir.data$wantedlast == 2    # wanted but later 
  ] <- 1   # Unmet need for spacing.
  
  # Unmet Need for Limiting
  # If wanted current/last birth not at all
  ir.data$unmet[
    is.na(ir.data$unmet)    & # don't know if it's unmet need yet
    ir.data$pregPPA24 == 1  & # pregnant or PPA for 2yrs
    ir.data$wantedlast == 3   # did not want last pregnancy
  ] <- 2   # Unmet need unmet need for limiting.
  
  #Missing ir.data on watedness of current/last birth
  ir.data$unmet[
    is.na(ir.data$unmet) &   # don't know if it's unmet need yet
    ir.data$pregPPA24 == 1 &   # # pregenant or PPA for 2 years
    (is.na(ir.data$wantedlast) | ir.data$wantedlast==9)  # don't know if was wanted
  ] <- 99  # code for NA.
  
  # Determine Sexually Active women in Last 30 Days
  # No Unmet Need for unmarried women who are not sexually active
  ir.data$sexact <- NA
  
  # Special Survey
  # Sexually Active Defined as 1 month
  if (SurveyID == "bf21") {
    ir.data$sexact[ir.data$v527 >= 0 & ir.data$v527 < 300] <- 1 
    ir.data$sexact[ir.data$v527 > 300] <- 2
  } else {
    ir.data$sexact[ir.data$v528 >= 0 & ir.data$v528 <= 30] <- 1
    if (SurveyID %in% specialSurveys) {
      ir.data$sexact[is.na(ir.data$sexact) & ir.data$v528==95] <- 1
    } else {
      ir.data$sexact[ir.data$v528 > 30] <- 2
      ir.data$sexact[ir.data$v536 == 0] <- 3
    }
  }

  # No Unmet need
  ir.data$unmet[
    is.na(ir.data$unmet) &                           # don't know yet
   (ir.data$v502 != 1 | is.na(ir.data$v502)) &       # ever-married (or lived w/ partner
   (ir.data$sexact != 1 | is.na(ir.data$sexact))     # not known sexually active
  ] <- 97    # Code for no unmet need FIXME: 7 is also a code for this... (????)


  ##############################################################################
  # GROUP 3: DETERMINE FECUNDITY (Boxes refer to Figure 2 flowchart in report)    

  # Box 1 - Applicable only to Currently Married Women #
  # Married 5+ years ago, no children in past 5 years, never used contraception
  # Excluding pregnant and PPA <24 Months
  ir.data$infec <- NA   ## Infecund, will be either 1 (infecund) or NA (don't know)
  
  if (SurveyID == "kh61") {
    ir.data$infec[
      ir.data$v502==1 & 
      ir.data$v512>=5 & 
     !is.na(ir.data$v512) & 
     (ir.data$tsinceb>59 | is.na(ir.data$tsinceb)) & 
      ir.data$s313==0 & 
     (ir.data$pregPPA24!=1 | is.na(ir.data$pregPPA24)) & 
     (ir.data$v007==2010 | ir.data$v007==2011)
    ] <- 1   # Infecund
  } else if (SurveyID == "tz60") {
    ir.data$infec[
      ir.data$v502 == 1 & 
      ir.data$v512 >= 5 & 
     !is.na(ir.data$v512) & 
     (ir.data$tsinceb > 59 | is.na(ir.data$tsinceb)) & 
      ir.data$s309b == 0 & 
     (ir.data$pregPPA24 != 1 | is.na(ir.data$pregPPA24)) & 
     (ir.data$v007 == 2009 | ir.data$v007 == 2010)
    ] <- 1   # Infecund
  } else {
    # DHS Update April 2017: checks for v000 now look for "7"
    ir.data$infec[
      !substr(ir.data$v000,3,3) %in% c("6","7") & 
       ir.data$v502==1 & 
       ir.data$v512>=5 & 
      !is.na(ir.data$v512) & 
      (ir.data$tsinceb>59 | is.na(ir.data$tsinceb)) & 
       ir.data$v302==0 &
      (ir.data$pregPPA24!=1 | is.na(ir.data$pregPPA24))
    ] <- 1   # Infecund
    
    # Pakistan round 6 DHS does still have v302 as name and not v302a 
    # (Not specified in DHS code because differently coded)
    if(SurveyID == "pk61") {
      ir.data$v302a <- ir.data$v302
    }
    
    ir.data$infec[
      substr(ir.data$v000,3,3) %in% c("6","7") & 
      ir.data$v502 == 1 & 
      ir.data$v512>=5 & 
     !is.na(ir.data$v512) & 
      (ir.data$tsinceb>59 | is.na(ir.data$tsinceb)) & 
       ir.data$v302a==0 &
      (ir.data$pregPPA24!=1 | is.na(ir.data$pregPPA24))
    ] <- 1   # Infecund
    # FIXME: The rampant use of alternative codes for NA is killing  
    #        this code.
  }
  
  # Box 2 
  # Declare infecund on future desires for children
  ir.data$infec[ir.data$v605 == 7] <- 1
  
  # Box 3 
  # Menopausal/Hysterectomy as the reason of not using contraception
  
  ####Special Surveys####
  if (SurveyID %in% c("br31","gu34","gu41")) {
    ir.data$infec[
      is.na(ir.data$infec) & 
     (ir.data$v375a==23 | ir.data$v375a==28)
    ] <- 1 
  } else if (SurveyID == "ci35") {
    ir.data$infec[
      is.na(ir.data$infec) & 
      ir.data$v007 == 94 & 
      ir.data$v376 == 23
    ] <- 1
  } else if (SurveyID == "ga41") {
    ir.data$infec[
      is.na(ir.data$infec) & 
      ir.data$s607d==1
    ] <- 1
  } else if (SurveyID == "ht31") {
    ir.data$infec[
      is.na(ir.data$infec) & 
      ir.data$v376 == 23
    ] <- 1
  } else if(SurveyID == "jo42") {
    ir.data$infec[
      is.na(ir.data$infec) & 
     (ir.data$v376 == 23 | ir.data$v376 == 24)
    ] <- 1
  } else if (SurveyID %in% c("kk42","tz3a")) {
    ir.data$infec[
      is.na(ir.data$infec) & 
     (ir.data$v007 == 99 & ir.data$s607d == 1)
    ] <- 1
  } else if (SurveyID == "mv50") {
    ir.data$infec[
      is.na(ir.data$infec) & 
      ir.data$v376 == 23
    ] <- 1
  } else if (SurveyID == "mr42") {
    ir.data$infec[
      is.na(ir.data$infec) & 
      ir.data$s607c == 1
    ] <- 1
  } else if (SurveyID == "tr4a") {
    ir.data$infec[
      is.na(ir.data$infec) & 
      ir.data$v375a == 23
    ] <- 1
  } else {
    # DHS IV+ Surveys
    ir.data$infec[
      substr(ir.data$v000, 3, 3) %in% c("4","5","6","7") & 
      ir.data$v3a08d == 1
    ] <- 1
    
    # DHS III Surveys
    ir.data$infec[
      substr(ir.data$v000, 3, 3) %in% c("3","T") & 
      ir.data$v375a == 23
    ] <- 1
    
    # DHS II Surveys
    # Reason not using contraception does not exists in DHSII
    # Use Reason Not Intending to use In Future
    ir.data$infec[
      substr(ir.data$v000, 3, 3) == "2" & 
      ir.data$v376 == 14
    ]<-1
  }
  
  # Box 4 
  # Time since Last Period >=6 Months and not PPA
  ir.data$infec[
    ir.data$tsincep>=6 & 
   !is.na(ir.data$tsincep) & 
   (is.na(ir.data$pregPPA) | ir.data$pregPPA!=1)
  ] <- 1   
  
  # Box 5 
  # Menopausal/Hysterectomy on Time Since Last Period
  if (SurveyID %in% c("tr41", "ug33", "ye21")) {
    ir.data$infec[
      is.na(ir.data$infec) & 
      ir.data$v215 == 993
    ] <- 1
  } else {
    ir.data$infec[ir.data$v215 == 994] <- 1
  }
  
  # Never Menstruated on Time Since Last birth, unless had a birth in the last 5 years
  ir.data$infec[
    ir.data$v215 == 996 & 
   (ir.data$tsinceb > 59 | is.na(ir.data$tsinceb))
  ] <- 1
  
  # Box 6
  # Time Since Last Birth >=60 Months and last period was before last birth
  ir.data$infec[
    ir.data$v215 == 995 & 
    ir.data$tsinceb>=60 & !is.na(ir.data$tsinceb)
  ] <- 1
  
  # Never had Birth, but last period reported as before last birth
  # Assume code should have been 994/996
  ir.data$infec[
    ir.data$v215==995 & 
    is.na(ir.data$tsinceb) 
  ] <- 1
  
  # Exclude pregnant & PPA < 24 months
  ir.data$infec[ir.data$pregPPA24  ==1 ] <- NA

  # Infecund women have no unmet need for contraception
  ir.data$unmet[is.na(ir.data$unmet) & ir.data$infec == 1] <- 9
 
  ##############################################################################
  # GROUP 4: NOT INFECUND WOMEN 

  # Wants within 2 years.
  if (SurveyID == "ia23") {
    ir.data$unmet[
      is.na(ir.data$unmet) & 
      ir.data$v602 == 6
    ] <- 7
  } else if (SurveyID == "ia42") {
    ir.data$unmet[
      is.na(ir.data$unmet) & ir.data$v605==9
    ] <-7
  } else {
    ir.data$unmet[
      is.na(ir.data$unmet) & ir.data$v605==1
    ] <- 7
  }

  # wants in 2+ years, wants undecided timing, or unsure if wants
  if (SurveyID == "ls60") {
    ir.data$v605[is.na(ir.data$v605)] <- 4
  }
  ir.data$unmet[
    is.na(ir.data$unmet) & 
    ir.data$v605 >= 2 & 
    ir.data$v605 <= 4 
  ] <- 1

  # wants no more
  ir.data$unmet[
    is.na(ir.data$unmet) & 
    ir.data$v605 == 5
  ] <-2

  ir.data$unmet[is.na(ir.data$unmet)] <- 99   # This could just stay NA... :/
  
  # Special Surveys
  if (SurveyID == "tr4a") {   ##FIXME: WHAT??
    ir.data <- subset(ir.data, ir.data$v001 %% 2 == ir.data$v002 %% 2)
  }
  
  # Total Unmet Need
  ir.data$specific_unmet <- ir.data$unmet
  ir.data$specific_unmet[
    !ir.data$specific_unmet %in% c(1,2)   ## Apparently these two categories are 'specific unmet'
  ] <- 0
  
  ir.data$unmettot <- NA
  ir.data$unmettot[  ir.data$unmet %in% c(1,2) ] <- 1
  ir.data$unmettot[!(ir.data$unmet %in% c(1,2))] <- 0
  
  # Kenya 2014 is not included in DHS list of special surveys, but has long and short questionnaire.
  # Only short questionnaire included questions on fertility preferences. Therefore limit universe.
  # We use desire for children variable and exclude missings from universe
  # FIXME: what is a Universe?
  if (SurveyID == "ke70") {
    for (i in c("unmet","unmettot","specific_unmet")) {
      ir.data[,i][is.na(ir.data$v605)]<-NA
    }
  }
  
  return (ir.data)
}
