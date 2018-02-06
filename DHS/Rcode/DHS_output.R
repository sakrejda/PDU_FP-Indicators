#' Combo of file name stubs and ... logic for checking...
#' 1) If the file exists, 2) remove it if it is empty, 
#' 3) rename it to subdir if it is not empty
FileSetting <- function(){
  fileStubs <- c("DHS_CPTYPE_by_MARSTAT_WIDE",
                 "DHS_CPTYPE_by_MARSTAT_LONG",
                 "DHS_CPMETHOD_by_MARSTAT_LONG",
                 "DHS_CPMETHOD_by_MARSTAT_WIDE")
  fileList <- vector(mode='character', length=0)
  ## Check existence of output file
  for (f in fileStubs) {
    fileName <- paste0(f, ".csv")
    fileExists <- file.exists(fileName)
    fileEmpty <- isTRUE(file.size(fileName) == 0)
    oldFileCTime <- file.info(fileName)$ctime
    oldFileTag <- gsub(' ', '_', oldFileCTime)
    oldFileDir <- 'prior'
    oldFileTarget <- file.path(oldFileDir, paste0(f, "_", oldFileTag, ".csv"))
    if(fileExists) {
      if (fileEmpty) {
        file.remove(fileName)
      } else {
        file.rename(fileName, oldFileTarget)
      }
    }
    fileList <- c(fileList, fileName)
  }
  
  ## Create output file 
  file.create(fileList)
  return(fileList)
}

CP_OUTPUT <- function(choice) {
  #Remove observations when 
  ## 1. marital status is missing
  ### NA retained for married women in mstatusBinary
  ## 2. Method is NA/missing
  ## 3. Agegroup is NA
  ir.data <- filter(ir.data, (mstatus != 9 | is.na(mstatus)) ,method!="Unknown",!is.na(method),!is.na(agegroup))
  if(choice == "Both"){
    cp_MW <- CP_OUTPUT("MW")
    cp_UMW <- CP_OUTPUT("UMW")
    
    final <- rbind(cp_MW,cp_UMW)
    
    tTYPE <- ResetRecords(final)
    
    return(tTYPE)
    
  }else if(choice == "MW"){
    cp<-CrossTab(T,"mstatusBinary","method",df,T)
    samplesize <- CrossTab(F,"mstatusBinary","method",df,T)
    umn <- CrossTab(T,"mstatusBinary","unmettot",df,T)
  }else if(choice == "UMW"){
    cp<-CrossTab(T,"mstatus","method",df,F)
    samplesize<-CrossTab(F,"mstatus","method",df,F)
    umn <- CrossTab(T,"mstatus","unmettot",df,F)
  }
  
  samplesize$n_unweighted <- rowSums(samplesize[,c("Not_using_any_method","Using_modern_method","Using_traditional_method")])
  samplesize$nAny_unweighted <- rowSums(samplesize[,c("Using_modern_method","Using_traditional_method")])
  samplesize <- samplesize[,c("mstatus","agegroup","n_unweighted","nAny_unweighted")]
  
  cp <- full_join(cp,samplesize)
  final <- full_join(cp, umn, by= c("mstatus", "agegroup")) %>%
    mutate(cpModern = Using_modern_method / (Not_using_any_method + Using_modern_method + Using_traditional_method) * 100,
           cpTraditional = Using_traditional_method / (Not_using_any_method + Using_modern_method + Using_traditional_method) *100,
           cpAny = (Using_modern_method + Using_traditional_method) / (Not_using_any_method + Using_modern_method + Using_traditional_method) *100,
           Unmet = Unmet_need / (Unmet_need + No_unmet_need) * 100,
           `Demand satisfied by modern` = ifelse(!is.na(Unmet),(Using_modern_method) / (Using_modern_method + Using_traditional_method + Unmet_need) * 100,NA),
           country = SurveyInfo$CountryName.UN,
           iso = SurveyInfo$LocID,
           catalogID = SurveyInfo$CatalogID,
           Universe = SurveyInfo$Sample.Type.Female,
           DateToday = Sys.Date(),
           Phase = SurveyInfo$Phase,
           SampleType = SurveyInfo$Type,
           surveyShort = SurveyInfo$ShortName,
           survey = SurveyInfo$SurveyName,
           Startyear = SurveyInfo$StartYear,
           Endyear = SurveyInfo$EndYear,
           StartDate = SurveyInfo$StartDate,
           EndDate = SurveyInfo$EndDate,
           RefDate = SurveyInfo$RefDate
    ) 
  
  tTYPE <- select(final, country : Universe, mstatus,agegroup,Not_using_any_method:agegroup, No_unmet_need:`Demand satisfied by modern`, DateToday : RefDate, n_unweighted,nAny_unweighted)
  return(tTYPE)
}

CP_METH <- function(choice){
  AllWomen <- sum
  if(choice == "Both"){
    cpMETH_MW<-CP_METH("MW")
    if(nrow(as.data.frame(xtabs(weights ~ mstatus + methodspecific_lab,ir.data)))>0){
      cpMETH_UMW<-CP_METH("UMW")
      cpMETH <- rbind(cpMETH_MW,cpMETH_UMW)
    }else{
      cpMETH <- cpMETH_MW
    }
    return(cpMETH)
  }else{
    if(choice=="MW"){
      cpMETH_All <- as.data.frame(addmargins(xtabs(weights ~ mstatusBinary + methodspecific_lab,ir.data),1,FUN=AllWomen)) %>%
        mutate(agegroup ="[Total]")
      cpMETH_Age <- as.data.frame(addmargins(xtabs(weights ~ mstatusBinary + methodspecific_lab+agegroup,ir.data),1,FUN=AllWomen))
      
      umnAll <- as.data.frame(addmargins(xtabs(weights~mstatusBinary + specific_unmet, ir.data),1,FUN=AllWomen))%>%
        mutate(agegroup = "[Total]")
      
      umnAge <- as.data.frame(addmargins(xtabs(weights~mstatusBinary + specific_unmet + agegroup,ir.data),1,FUN=AllWomen))
      
    }else if (choice == "UMW"){
      cpMETH_All <- as.data.frame(xtabs(weights ~ mstatus + methodspecific_lab,ir.data)) %>%
        mutate(agegroup = "[Total]")
      cpMETH_Age <- as.data.frame(xtabs(weights ~ mstatus + methodspecific_lab + agegroup, ir.data))
      
      umnAll <- as.data.frame(xtabs(weights~mstatus + specific_unmet, ir.data))%>%
        mutate(agegroup = "[Total]")
      
      umnAge <- as.data.frame(xtabs(weights~mstatus + specific_unmet + agegroup,ir.data))
    }
    
    if(nrow(cpMETH_All) >0 & nrow(cpMETH_Age)>0){
      umn <- full_join(umnAll,umnAge)
      names(umn)[which(names(umn)=="specific_unmet")] <- "methodspecific_lab"
      cpMETH <- full_join(cpMETH_All,cpMETH_Age)
      cpMETH <- full_join(cpMETH,umn)
      if("mstatusBinary" %in% colnames(cpMETH)){
        colnames(cpMETH)[colnames(cpMETH)=="mstatusBinary"] <- "mstatus"
      }
      cpMETH <- cpMETH %>%
        arrange(mstatus,agegroup)%>%
        mutate(country = SurveyInfo$CountryName.UN,
               iso = SurveyInfo$LocID,
               catalogID = SurveyInfo$CatalogID,
               Universe = SurveyInfo$Sample.Type.Female,
               surveyShort = SurveyInfo$ShortName,
               survey = SurveyInfo$SurveyName,
               Startyear = SurveyInfo$StartYear,
               Endyear = SurveyInfo$EndYear,
               StartDate = SurveyInfo$StartDate,
               EndDate = SurveyInfo$EndDate,
               RefDate = SurveyInfo$RefDate
        )
      return (cpMETH)
    }else{
      return (NULL)
    }
  }
  
}

ResetRecords <- function(t){
  mstat.list <- c("Formerly married","Never married")
  group.list <- c("AllWomen","Unmarried/Not-in-union")
  
  for(m in mstat.list){
    #Adjust for the new surveys that do not ask CP questions to unmarried women
    if(t$Using_modern_method[which(t$mstatus==m & t$agegroup == "[Total]")]==0 & 
       t$Using_traditional_method[which(t$mstatus==m & t$agegroup == "[Total]")]==0){
      t$cpAny[which(t$mstatus==m)]<-NA
      t$cpModern[which(t$mstatus==m)]<-NA
      t$cpTraditional[which(t$mstatus==m)]<-NA
      t$Unmet[which(t$mstatus==m)]<-NA
      t$`Demand satisfied by modern`[which(t$mstatus==m)]<-NA
      
      for(g in group.list){
        t$cpAny[which(t$mstatus==g)]<-NA
        t$cpModern[which(t$mstatus==g)]<-NA
        t$cpTraditional[which(t$mstatus==g)]<-NA
        t$Unmet[which(t$mstatus==g)]<-NA
        t$`Demand satisfied by modern`[which(t$mstatus==g)]<-NA
      }
    }
    if(t$Unmet_need[which(t$mstatus==m &t$agegroup=="[Total]")] ==0){
      t$Unmet[which(t$mstatus==m)]<-NA
      t$`Demand satisfied by modern`[which(t$mstatus==m)]<-NA
      
      for(g in group.list){
        t$Unmet[which(t$mstatus==g)]<-NA
        t$`Demand satisfied by modern`[which(t$mstatus==g)]<-NA
      }
    }
  }
  
  #Adjust for Ever married sample
  if(all(t$Universe=="Ever Married")){
    t <- subset(t, subset=t$mstatus!="Unmarried/Not-in-union")
    for(r in 13:17){
      t[(which(t$mstatus=="AllWomen")),r]<-NA
      if(r<12){
        if(all(t[(which(t$mstatus=="Never married")),r]==0)){
          t[(which(t$mstatus=="Never married")),r] <- NA
        }
      }
    }
  }
  
  #Adjust for DHS-I
  t$Unmet[which(t$Phase == "DHS-I")]<-NA
  t$No_unmet_need[which(t$Phase == "DHS-I")]<-NA
  t$Unmet_need[which(t$Phase == "DHS-I")]<-NA
  
  return(t)
}

Output <- function(tTYPE,tMETH){
  if (file.size(file.list[1]) == 0){
    # if the csv output file is empty append the computed values to the output file but output the column names first, that is,in the first row
    write.table(tTYPE, file = file.list[1], append = TRUE, quote = TRUE, sep = ",", row.names = FALSE, col.names = TRUE)
  } else {
    # if the csv output file already has observations in it append the results to the output file without displaying column names each time data is outputted
    write.table(tTYPE, file = file.list[1], append = TRUE, quote = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
  }
  
  #Output for tMETH
  if (file.size(file.list[3]) == 0){
    # if the csv output file is empty append the computed values to the output file but output the column names first, that is,in the first row
    write.table(tMETH, file = file.list[3], quote = TRUE, append = TRUE, sep = ",", row.names = FALSE, col.names = TRUE)
  } else {
    # if the csv output file already has observations in it append the results to the output file without displaying column names each time data is outputted
    write.table(tMETH, file = file.list[3], quote = TRUE, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
  }
}
