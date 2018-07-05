#' Visit-Arrival Lag
#'
#' @description 
#' Calculates the average time lag between when: a patient arrives and the first record for their visit is received by the NSSP BioSense Platform.
#' A summary is returned at the facility level.
#' 
#' @param data The raw data from BioSense on which you will do these summaries.
#' @param names A table of Facility_Name and C_Biosense_Facility_ID which taken from the MFT using the `pull_data` function.
#' @param offset The number of hours you wish to offset Arrived_Date_Time (which is in UTC). The offset value is how far off your local time zone is from UTC. 
#' For example, the Central Time Zone would set this to 5 or 6, depending on if it is daylight savings or not. This value should be an integer. 
#' @return A data frame with three columns: facility name, facility ID, and average lag between patient visit and first record arrival.
#' @import dplyr
#' @import tidyr
#' @export
va_lag <- function(data) {
    LagTime=data %>% 
      select(C_Biosense_Facility_ID, Arrived_Date_Time, C_Visit_Date_Time, Message_Date_Time, Recorded_Date_Time)%>% 
      mutate(Arrived=as.POSIXct(Arrived_Date_Time,format="%Y-%m-%d %H:%M:%S"),
             Visit=as.POSIXct(C_Visit_Date_Time,format="%Y-%m-%d %H:%M:%S"),
             Message=as.POSIXct(Message_Date_Time,format="%Y-%m-%d %H:%M:%S"),
             Record=as.POSIXct(Recorded_Date_Time,format="%Y-%m-%d %H:%M:%S")
      )
    
    Time_Diff=LagTime%>%
      transmute(C_Biosense_Facility_ID,
                lag_Record_Visit=as.numeric(difftime(Record,Visit,units="hours")),
                lag_Message_Record=as.numeric(difftime(Message,Record,units="hours")),
                lag_Arrival_Message=as.numeric(difftime(Arrived,Message,units="hours")),
                lag_Arrival_Visit=as.numeric(difftime(Arrived,Visit,units="hours"))         
      )
    
   return(
     Lag_Summary=Time_Diff %>%
       group_by(C_Biosense_Facility_ID)%>%
       summarise(Record_Visit=round(mean(lag_Record_Visit,na.rm=TRUE),2)),
                 Message_Record=round(mean(lag_Message_Record,na.rm=TRUE),2),
                 Arrival_Message=round(mean(lag_Arrival_Message,na.rm=TRUE),2),
                 Arrival_Visit=round(mean(lag_Arrival_Visit,na.rm=TRUE),2)     
   )
}
