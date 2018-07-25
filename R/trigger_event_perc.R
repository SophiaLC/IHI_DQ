##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for.
##Trigger_Event
##number_visits=n_groups(group_by(data,C_BioSense_ID)), this number of visits will be computed in write_report function
trigger_event_perc<-function(data){
  Trigger_Event=data%>%
    select(C_BioSense_ID, Trigger_Event)%>%
    distinct(C_BioSense_ID,Trigger_Event,.keep_all=TRUE)%>%
    count(Trigger_Event)%>%
    transmute(Trigger_Event,count=n,percentage=round(100*n/number_visits,2))
  return(
    Trigger_Event
  )
}
