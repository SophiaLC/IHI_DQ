  
   if (nexamples > 0) {
    # get list of invalid examples data frames
    # DO NOT CHANGE THE ORDER OF THIS LIST
    invalid_examples <- list(admit_source_invalid(data)[[1]], # 1
                             age_invalid(data)[[1]], # 2
                             any_e_invalid(data)[[1]], # 3
                             blood_pressure_invalid(data)[[1]], # 4
                             cc_ar_invalid(data)[[1]], # 5
                             country_invalid(data)[[1]], # 6
                             death_invalid(data)[[1]], # 7
                             diagnosis_type_invalid(data)[[1]], # 8
                             discharge_disposition_invalid(data)[[1]], # 9
                             ethnicity_invalid(data)[[1]], # 10
                             facility_type_invalid(data)[[1]], # 11
                             fpid_mrn_invalid(data)[[1]], # 12
                             gender_invalid(data)[[1]], # 13
                             height_invalid(data)[[1]], # 14
                             patient_class_invalid(data)[[1]], # 15
                             pulseox_invalid(data)[[1]], # 16
                             race_invalid(data)[[1]], # 17
                             smoking_status_invalid(data)[[1]], # 18
                             state_invalid(data)[[1]], # 19
                             temperature_invalid(data)[[1]], # 20
                             weight_invalid(data)[[1]], # 21
                             zip_invalid(data)[[1]]) # 22

      inv_examples <- examples_invalids(facility, invalid_examples) # get examples of invalids from this facility
      null_examples <- examples_nulls(facility, data) # get examples of nulls from this faciltiy
      # join with other relevant fields
      inv_examples <- inv_examples %>% # take examples
        left_join(., select(data, c(C_BioSense_ID, C_Visit_Date, C_Visit_Date_Time, First_Patient_ID,
                                    C_Unique_Patient_ID, Medical_Record_Number, Visit_ID, Admit_Date_Time,
                                    Recorded_Date_Time, Message_Date_Time, Create_Raw_Date_Time,
                                    Message_Type, Trigger_Event, Message_Structure, Message_Control_ID)),
                  by="C_BioSense_ID") %>% # join with all these fields, for every record of that visit
        rename(Invalid_Field=Field) %>% # make it clearer that that field is the one that is invalid
        group_by(Invalid_Field) %>% # group by type of field
        slice(1:nexamples) # get nexamples
      # do the same for nulls
      null_examples <- null_examples %>%
        left_join(., select(data, c(C_BioSense_ID, C_Visit_Date, C_Visit_Date_Time, First_Patient_ID,
                                    C_Unique_Patient_ID, Medical_Record_Number, Visit_ID, Admit_Date_Time,
                                    Recorded_Date_Time, Message_Date_Time, Create_Raw_Date_Time,
                                    Message_Type, Trigger_Event, Message_Structure, Message_Control_ID)),
                  by="C_BioSense_ID") %>% # join with all these fields, for every record of that visit
        group_by(Null_Field) %>% # group by type of field
        slice(1:nexamples) # get nexamples

      # write to excel workbook
      wb <- createWorkbook()
      # sheet 1: invalids
      sheet1 <- addWorksheet(wb, "Invalids")
      writeDataTable(wb, sheet1, inv_examples, firstColumn=TRUE, bandedRows=TRUE)
      setColWidths(wb, sheet1, 1:ncol(inv_examples), "auto")
      freezePane(wb, sheet1, firstActiveRow=2, firstActiveCol=4)
      # sheet2: nulls
      sheet2 <- addWorksheet(wb, "Nulls")
      writeDataTable(wb, sheet2, null_examples, firstColumn=TRUE, bandedRows=TRUE)
      setColWidths(wb, sheet2, 1:ncol(null_examples), "auto")
      freezePane(wb, sheet2, firstActiveRow=2, firstActiveCol=3)
      # write sheet
      filename <- str_replace_all(name, "[^[a-zA-z\\s0-9]]", "") %>% # get rid of punctuation from faciltiy name
        str_replace_all("[\\s]", "_") # replace spaces with underscores
      saveWorkbook(wb, paste0(directory, "/", filename, "_Examples.xlsx"), overwrite=TRUE)
  }
}
