##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for.
##Trigger_Event

trigger_event_perc<-function(data){
  Trigger_Event=data%>%
    select(C_BioSense_ID, Trigger_Event)%>%
    count(Trigger_Event)%>%
    transmute(Trigger_Event,count=n,percentage=round(100*n/sum(n),2))
  return(
    Trigger_Event
  )
}
