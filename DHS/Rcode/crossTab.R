
CrossTab <- function(VarWeight,VarMarital, VarMethod,df,Formula){
  require(reshape2) # There it is... 
  AllWomen <- sum
  if (isTRUE(Formula)) {
    if (isTRUE(VarWeight)) {
      tab_All <- as.data.frame(addmargins(xtabs(weights~get(VarMarital)+get(VarMethod), ir.data),1,FUN=AllWomen)) %>%
        dcast(get.VarMarital.~get.VarMethod.,value.var="Freq") %>%
        mutate(agegroup = "[Total]")
      tab_Age <- as.data.frame(addmargins(xtabs(weights~get(VarMarital)+agegroup+get(VarMethod),ir.data),1,FUN=AllWomen)) %>%
        dcast(get.VarMarital.+agegroup~get.VarMethod.,value.var="Freq")
    } else {
      tab_All <- as.data.frame(addmargins(xtabs(~get(VarMarital)+get(VarMethod),ir.data),1,FUN=AllWomen)) %>%
        dcast(get.VarMarital.~get.VarMethod.,value.var="Freq") %>%
        mutate(agegroup="[Total]")
      tab_Age <- as.data.frame(addmargins(xtabs(~get(VarMarital)+agegroup+get(VarMethod),ir.data),1, FUN=AllWomen)) %>%
        dcast(get.VarMarital.+agegroup~get.VarMethod.,value.var="Freq")
    }
  } else {
    if (isTRUE(VarWeight)) {
      tab_All <- as.data.frame(xtabs(weights~get(VarMarital)+get(VarMethod),ir.data))%>%
        dcast(get.VarMarital.~get.VarMethod.,value.var="Freq")%>%
        mutate(agegroup = "[Total]")
      tab_Age <- as.data.frame(xtabs(weights~get(VarMarital)+agegroup+get(VarMethod),ir.data))%>%
        dcast(get.VarMarital.+agegroup ~ get.VarMethod.,value.var="Freq")
    } else {
      tab_All <- as.data.frame(xtabs(~get(VarMarital)+get(VarMethod),ir.data))%>%
        dcast(get.VarMarital.~get.VarMethod.,value.var="Freq")%>%
        mutate(agegroup = "[Total]")
      tab_Age <- as.data.frame(xtabs(~get(VarMarital)+agegroup+get(VarMethod),ir.data))%>%
        dcast(get.VarMarital.+agegroup ~ get.VarMethod.,value.var="Freq")
    }
  }
  
  names(tab_All)[which(names(tab_All)=="get.VarMarital.")] <- "mstatus"
  names(tab_Age)[which(names(tab_Age)=="get.VarMarital.")] <- "mstatus"
  
  tab <- full_join(tab_All,tab_Age)
  
  return(tab)
}
