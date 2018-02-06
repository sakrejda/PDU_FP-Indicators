

Transform <- function(file.list) { ## I think file.list here is fromt he SettingFile function or similar.
  df <- read.csv(file.list[1])
  
  tt <- df %>%
    gather(Indicator, Data.Value, cpModern:`Demand.satisfied.by.modern`)%>%
    filter(mstatus != "Marital Status, Missing")
  
  write.table(tt, file.list[2] ,quote=TRUE,sep=",",row.names=F)
  
  #tMETH
  tLong <- read.csv(file = file.list[3])
  colList <- c("Pill","Daily pill","Monthly pill","IUD","Norplant/Implants",
               "Condom","Female Condom","Female Sterilization","Male Sterilization",
               "Patch","Ring","Injections","Injection (3 monthly)","Injection (monthly)",
               "Diaphragm/Foam/Jelly","Diaphragm","Diaphragm/Foam","Diaphragm/Jelly",
               "Foam or Jelly","Foaming tablets","Vaginal methods","Lactational amenorrhea (LAM)",
               "Prolonged breastfeeding","Emergency contraception",
               "Other modern method","Abstinence or periodic abstinence","Periodic abstinence",
               "Cycle Beads/Standard days method","Abstinence","Mucus method","Temperature",
               "Other Rhythm/Calendar/Periodic Abstinence",
               "Natural family planning, unspecified","Withdrawal","Other traditional/folkloric",
               "Herbs/Plants","Gris-Gris/Amulet","Astrology","Strings","Massage","Douche",
               "OTHER METHOD, UNSPECIFIED","Other specific method 1","Other specific method 2",
               "Other specific method 3","Other specific method 4","modernUser", 
               "traditionalUser", "totalUser", "NotUsing","No_Unmet_Need","UnmetNeed_for_Spacing",
               "UnmetNeed_for_Limiting")
  
  #filters out surveys from tLong which did not have method specific variable or other problems
  requiredColumns <- c("Married/In-union", "Unmarried/Not-in-union", "AllWomen", "Formerly in-union", "Neverin-union", "Unmarried") 
  tWide <- tLong %>% filter(mstatus %in% requiredColumns) %>% 
    reshape2::dcast(catalogID + iso + survey + surveyShort + country + Startyear +Endyear + mstatus + agegroup ~ methodspecific_lab, value.var = "Freq")
  
  for (j in colList[!colList %in% names(tWide)]) {
    tWide[j] <- NA
  }
  
  tWide <- tWide[c(names(tWide)[!names(tWide) %in% colList], colList)]
  
  tWide <- tWide %>%  ### Isn't this list of columns somewhere else already? Why repeat here?
    mutate(modernUser = rowSums(cbind(Pill, `Daily pill`, `Monthly pill`, IUD, `Norplant/Implants`, Condom, `Female Condom`, `Female Sterilization`,
                                      `Male Sterilization`,Patch, Ring, Injections, `Injection (3 monthly)`,`Injection (monthly)`,`Diaphragm/Foam/Jelly`,
                                      Diaphragm, `Diaphragm/Foam`, `Diaphragm/Jelly`, `Foam or Jelly`, `Foaming tablets`,`Vaginal methods`, `Lactational amenorrhea (LAM)`,
                                      `Emergency contraception`,`Other modern method`),na.rm=T),
           traditionalUser = rowSums(cbind(`Abstinence or periodic abstinence`,`Periodic abstinence`,`Cycle Beads/Standard days method`, Abstinence,
                                           `Mucus method`, Temperature,`Other Rhythm/Calendar/Periodic Abstinence`, `Natural family planning, unspecified`,
                                           Withdrawal,`Other traditional/folkloric`,`Herbs/Plants`,`Gris-Gris/Amulet`,Astrology,Strings,Massage,Douche,`Prolonged breastfeeding`,`OTHER METHOD, UNSPECIFIED`),na.rm=T),
           totalUser = modernUser+traditionalUser,
           NotUsing = `Not using`,
           totalN = rowSums(cbind(modernUser, traditionalUser, NotUsing), na.rm = TRUE)
    )
  
  for (j in colList) {
    colTitle <- paste("CP", j, sep = "_")
    if(j %in% c("No_Unmet_Need","UnmetNeed_for_Spacing","UnmetNeed_for_Limiting")){
      tWide[colTitle] <- tWide[j] / rowSums(tWide[,c("No_Unmet_Need","UnmetNeed_for_Spacing","UnmetNeed_for_Limiting")],na.rm=T)*100
    }else{
      tWide[colTitle] <- tWide[j] / rowSums(tWide[, c("modernUser", "traditionalUser", "NotUsing")], na.rm = TRUE) * 100
    }
  }
  
  write.table(tWide, file = file.list[4], quote = TRUE, sep = ",", row.names = FALSE, col.names = TRUE)
  
}




