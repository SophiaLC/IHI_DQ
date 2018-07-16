##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 


## for variable Race_Description
Race_Description_perc<-function(data){
  Race_Description=data%>%
    select(C_BioSense_ID,Race_Description)%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Race_Description)%>%
    transmute(Race_Description,count=n,percentage=n/sum(n))
  
  return(
    Race_Description
  )
  
}


## Race_Code
Race_Code_perc<-function(data){
  Race_Code=data%>%
    select(C_BioSense_ID,Race_Code)%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Race_Code)%>%
    transmute(Race_Code,count=n,percentage=n/sum(n))
  
  return(
    Race_Code
  )
  
}



## Ethnicity_Code
Ethnicity_Code_perc<-function(data){
  Ethnicity_Code=data%>%
    select(C_BioSense_ID,Ethnicity_Code)%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Ethnicity_Code)%>%
    transmute(Ethnicity_Code,count=n,percentage=n/sum(n))
  
  return(
    Ethnicity_Code
  )
  
}



## Ethnicity_Description
Ethnicity_Description_perc<-function(data){
  Ethnicity_Description=data%>%
    select(C_BioSense_ID,Ethnicity_Description)%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Ethnicity_Description)%>%
    transmute(Ethnicity_Description,count=n,percentage=n/sum(n))
  
  return(
    Ethnicity_Description
  )
  
}


##city
City_perc<-function(data){
  City=data%>%
    select(C_BioSense_ID, Patient_City)%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Patient_City)%>%
    transmute(Patient_City,count=n,percentage=n/sum(n))
  return(
    City
  )
}


## State
State_perc<-function(data){
  State=data%>%
    select(C_BioSense_ID, Patient_State)%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Patient_State)%>%
    transmute(Patient_State,count=n,percentage=n/sum(n))
  return(
    State
  )
}


##country
Country_perc<-function(data){
  Country=data%>%
    select(C_BioSense_ID, Patient_Country)%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Patient_Country)%>%
    transmute(Patient_Country,count=n,percentage=n/sum(n))
  return(
    Country
  )
}


##county
County_perc<-function(data){
  County=data%>%
    select(C_BioSense_ID, C_Patient_County)%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(C_Patient_County)%>%
    transmute(C_Patient_County,count=n,percentage=n/sum(n))
  return(
    County
  )
}


## smoking status code
Smoking_Status_Code_perc<-function(data){
  Smoking_Status_Code=data%>%
    select(C_BioSense_ID, Smoking_Status_Code)%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Smoking_Status_Code)%>%
    transmute(Smoking_Status_Code,count=n,percentage=n/sum(n))
  return(
    Smoking_Status_Code
  )
}



## smoking status description
Smoking_Status_Description_perc<-function(data){
  Smoking_Status_Description=data%>%
    select(C_BioSense_ID, Smoking_Status_Description)%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Smoking_Status_Description)%>%
    transmute(Smoking_Status_Description,count=n,percentage=n/sum(n))
  return(
    Smoking_Status_Description
  )
}



## discharge disposition
Discharge_Disposition_perc<-function(data){
  Discharge_Disposition=data%>%
    select(C_BioSense_ID, Discharge_Disposition)%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Discharge_Disposition)%>%
    transmute(Discharge_Disposition,count=n,percentage=n/sum(n))
  return(
    Discharge_Disposition
  )
}


## Insurance_Company_ID
Insurance_Company_ID_perc<-function(data){
  Insurance_Company_ID=data%>%
    select(C_BioSense_ID, Insurance_Company_ID)%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Insurance_Company_ID)%>%
    transmute(Insurance_Company_ID,count=n,percentage=n/sum(n))
  return(
    Insurance_Company_ID
  )
}



## C_MFT_Patient_Class
C_MFT_Patient_Class_perc<-function(data){
  C_MFT_Patient_Class=data%>%
    select(C_BioSense_ID, C_MFT_Patient_Class)%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(C_MFT_Patient_Class)%>%
    transmute(C_MFT_Patient_Class,count=n,percentage=n/sum(n))
  return(
    C_MFT_Patient_Class
  )
}


##Trigger_Event

Trigger_Event_perc<-function(data){
  Trigger_Event=data%>%
    select(C_BioSense_ID, Trigger_Event)%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Trigger_Event)%>%
    transmute(Trigger_Event,count=n,percentage=n/sum(n))
  return(
    Trigger_Event
  )
}



##Chief_Complaint_Text
## with issues
Chief_Complaint_Text_perc<-function(data){
  Chief_Complaint_Text=data%>%
    select(C_BioSense_ID, Chief_Complaint_Text)%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Chief_Complaint_Text)%>%
    transmute(Chief_Complaint_Text,count=n,percentage=n/sum(n))
  return(
    Chief_Complaint_Text
  )
}



## Admit_Reason_Description
Admit_Reason_Description_perc<-function(data){
  Admit_Reason_Description=data%>%
    select(C_BioSense_ID, Admit_Reason_Description)%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Admit_Reason_Description)%>%
    transmute(Admit_Reason_Description,count=n,percentage=n/sum(n))
  return(
    Admit_Reason_Description
  )
}


## Clinical_Impression
Clinical_Impression_perc<-function(data){
  Clinical_Impression=data%>%
    select(C_BioSense_ID, Clinical_Impression)%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Clinical_Impression)%>%
    transmute(Clinical_Impression,count=n,percentage=n/sum(n))
  return(
    Clinical_Impression
  )
}


## Triage_Notes
Triage_Notes_perc<-function(data){
  Triage_Notes=data%>%
    select(C_BioSense_ID, Triage_Notes)%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Triage_Notes)%>%
    transmute(Triage_Notes,count=n,percentage=n/sum(n))
  return(
    Triage_Notes
  )
}



## Facility_Type_Code
Facility_Type_Code_perc<-function(data){
  Facility_Type_Code=data%>%
    select(C_BioSense_ID, Facility_Type_Code)%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Facility_Type_Code)%>%
    transmute(Facility_Type_Code,count=n,percentage=n/sum(n))
  return(
    Facility_Type_Code
  )
}


##Facility_Type_Description
Facility_Type_Description_perc<-function(data){
  Facility_Type_Description=data%>%
    select(C_BioSense_ID, Facility_Type_Description)%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Facility_Type_Description)%>%
    transmute(Facility_Type_Description,count=n,percentage=n/sum(n))
  return(
    Facility_Type_Description
  )
}



##Diagnosis_Code
Diagnosis_Code_perc<-function(data){
  Diagnosis_Code=data%>%
    select(C_BioSense_ID, Diagnosis_Code)%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Diagnosis_Code)%>%
    transmute(Diagnosis_Code,count=n,percentage=n/sum(n))
  return(
    Diagnosis_Code
  )
}



## Diagnosis_Type
Diagnosis_Type_perc<-function(data){
  Diagnosis_Type=data%>%
    select(C_BioSense_ID, Diagnosis_Type)%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Diagnosis_Type)%>%
    transmute(Diagnosis_Type,count=n,percentage=n/sum(n))
  return(
    Diagnosis_Type
  )
}




## C_Patient_Age_Years
Age_Group_perc<-function(data){
  Age_Group=data%>%
    select(C_BioSense_ID,C_Patient_Age_Years)%>%
    mutate(Age_Group=case_when(C_Patient_Age_Years<= 4 ~"0-4",
                               C_Patient_Age_Years<=17 ~"5-17",
                               C_Patient_Age_Years<=44 ~"18-44",
                               C_Patient_Age_Years<=64 ~"45-64",
                               C_Patient_Age_Years>=65 ~"65+") )%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Age_Group)%>%
    transmute(Age_Group,count=n, percentage=n/sum(n))
  
  return(
    Age_Group
  )
  
}
