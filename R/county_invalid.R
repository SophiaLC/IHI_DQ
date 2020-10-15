#' Getting Invalid Examples and Summaries for C_Patient_County
#'
#' This function will generate two data frames: first, a frame to be used later to extract invalid examples from;
#' second, a frame that contains facility-level summaries for counts and percentages of cases with invalid C_Patient_County.
#' 
#' The valid values were taken from the `PHVS_Country_ISO_3166-1_V1.xls` file from 
#' Public Health Information Network Vocabulary Access and Distribution System value sets
#' (https://phinvads.cdc.gov/vads/ViewValueSet.action?oid=2.16.840.1.114222.4.11.829). The package will
#' be updated as the CDC provides new or different codes that are considered valid or invalid.
#' 
#' You can view the concept codes that are considered valid by calling `data("county")`.
#' 
#' @param data The raw data on which you will do the invalid C_Patient_County check.
#' @return A list of two data frames: examples and summary for C_Patient_County.
#' @import dplyr
#' @export
county_invalid <- function(data) {
  
  # get valid values for county
  data("county", envir=environment())
  
  valid_county_values <- county %>% # take data
    select(Concept.Code) %>% # the variable we want is called concept code
    filter(!is.na(Concept.Code)) %>% # get rid of any nas
    c() %>% # turn this into a vector
    unlist() %>% # unlist them from the concept name object 
    unname() %>% # unname stuff
    toupper() # uppercase everything
  
  # generate example file
  county_examples <- data %>% # take data
    select(c(C_Facility_ID, C_Visit_ID, C_Patient_County)) %>%  # taking just the variables we need
    mutate(C_Patient_County=toupper(as.character(C_Patient_County)), # make as character and uppercase
           Invalid_Patient_County=case_when(
             is.na(C_Patient_County) ~ NA, # if field is na, then invalid remains na
             C_Patient_County %in% valid_county_values ~ FALSE, # if field includes one of the allowed values, then invalid is false
             !C_Patient_County %in% valid_county_values ~ TRUE # if field does not include allowed value, then invalid is true
           ))
  
  # generating summary data
  county_summary <- county_examples %>% # take data
    group_by(v) %>% # group by patient visit
    mutate(Any_Invalid_Patient_County=case_when(
      all(is.na(Invalid_Patient_County)) ~ NA, # if all checks na, then case is na
      sum(Invalid_Patient_County, na.rm=TRUE) == 0 ~ FALSE, # if no checks true, then false
      TRUE ~ TRUE # otherwise, true
    )) %>% 
    slice(1) %>% # take one observation per patient visit
    ungroup() %>% # explicitly ungroup
    group_by(C_Facility_ID) %>% # group by facility
    summarise(C_Patient_County.Percent=round(mean(Any_Invalid_Patient_County, na.rm=TRUE)*100,2), # percent
              C_Patient_County.Count=sum(Any_Invalid_Patient_County, na.rm=TRUE)) # count
  
  return(
    list(county_examples=county_examples,
         county_summary=county_summary)
  )  
}
