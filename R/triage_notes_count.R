
triage_notes_count<-function(data){
  triage_notes=data%>%
    select(Triage_Notes,C_BioSense_ID)%>%
    filter(is.na(Triage_Notes)==FALSE)%>%
    group_by(C_BioSense_ID)%>%
    count(Triage_Notes)%>%
    arrange(Triage_Notes)%>%
    select(Triage_Notes,n,C_BioSense_ID)
  
  return(triage_notes)
}
