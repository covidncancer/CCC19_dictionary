###Code to create derived variables
setwd("~/Box Sync/CCC19 data")

#Required libraries
require("dplyr")

#Create an object that is a copy of the original data
ccc19x <- foo

#Define the desired suffix for the save function
# suffix <- 'data with derived variables for analysis'
 suffix <- 'data with derived variables for site QA'
#suffix <- 'data with derived variables for central QA'
# suffix <- 'data with derived variables for appeal'

#Create a table to log the variables as they are created
var.log <- data.frame(name = character(),
                      timestamp = character(),
                      values = character(),
                      stringsAsFactors = F)

##DERIVED VARIABLES to recode:
{
  #########
  #Outcomes
  #########
  {
    
    ###########################
    #O1. Binary death indicator
    ###########################
    ccc19x$der_deadbinary <- NA
    
    #Alive on primary form
    ccc19x$der_deadbinary[which(ccc19x$current_status %in% 1:8 |
                                  ccc19x$current_status_retro %in% c("1", "1b") | 
                                  ccc19x$current_status_v2 %in% c("1", "1b", "2")) ] <- 0
    
    #Dead on primary form (overwriting allowed)
    ccc19x$der_deadbinary[which(ccc19x$current_status_retro == 3 | 
                                  ccc19x$current_status_v2 == 3 | 
                                  ccc19x$mortality == 0 |
                                  ccc19x$mortality_90 == 0 |
                                  ccc19x$mortality_180 == 0 |
                                  ccc19x$current_status == 9)] <- 1
    
    #Unknown on primary form (don't overwrite)
    ccc19x$der_deadbinary[which(ccc19x$current_status_retro == 99 &
                                  is.na(ccc19x$der_deadbinary))] <- 99
    
    #Alive on followup form
    ccc19x$der_deadbinary[which(ccc19x$covid_19_status_fu %in% c('1', '1b', '2') | 
                        ccc19x$fu_reason %in% 1:2)] <- 0
    
    #Dead on followup form (overwriting allowed)
    ccc19x$der_deadbinary[which(ccc19x$covid_19_status_fu ==3 | 
                        ccc19x$d30_vital_status == 1 |
                        ccc19x$d90_vital_status == 1 |
                        ccc19x$d180_vital_status == 1 |
                        ccc19x$d365_vital_status == 1 |
                        ccc19x$current_status_fu == 9 |
                        ccc19x$fu_reason == 3)] <- 1
    
    #Unknown on followup form (don't overwrite)
    ccc19x$der_deadbinary[which(ccc19x$covid_19_status_fu == 99 &
                                  is.na(ccc19x$der_deadbinary))] <- 99
    
    #Reconcile death status for each patient
    temp <- unique(ccc19x$record_id)
    for(i in 1:length(temp))
    {
      temp.ref1 <- which(ccc19x$record_id == temp[i] & ccc19x$redcap_repeat_instrument == '')
      temp1 <- ccc19x$der_deadbinary[temp.ref1]
      
      temp.ref2 <- which(ccc19x$record_id == temp[i] & ccc19x$redcap_repeat_instrument == 'followup')
      if(length(temp.ref2) > 0)
      {
        temp2 <- ccc19x$der_deadbinary[temp.ref2]
        
        #If there is a follow-up but the baseline is missing, change baseline to unknown
        if(is.na(temp1)) temp1 <- 99
        
        temp1 <- temp1[!is.na(temp1)]
        temp2 <- temp2[!is.na(temp2)]
        
        temp.ref <- c(temp.ref1, temp.ref2)
        
        #Screen for false positives ("resuscitations")
        if(length(temp1) > 0)
        {
          if(temp1 == 1 & any(temp2 == 0) & !any(temp2 == 1)) 
          {
            temp1 <- 3
            ccc19x$der_deadbinary[temp.ref] <- 3 
          }
        } 
        
        if(temp1 != 3)
        {
          temp2 <- c(temp1, temp2)
          {
            if(any(temp2 == 1)) ccc19x$der_deadbinary[temp.ref] <- 1
            if(!any(temp2 == 1) & any(temp2 == 0)) ccc19x$der_deadbinary[temp.ref] <- 0
            if(!any(temp2 == 1) & !any(temp2 == 0) & any(temp2 == 99)) ccc19x$der_deadbinary[temp.ref] <- 99
          }
        }
      }
    }
    
    #Factor
    ccc19x$der_deadbinary <- as.factor(ccc19x$der_deadbinary)
    
    temp <- summary(ccc19x$der_deadbinary[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_deadbinary',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ####################
    #O2. Hospitalization
    ####################
    ccc19x$der_hosp <- NA
    
    #Initial form
    
    #No
    ccc19x$der_hosp[which(ccc19x$hosp_status == 0| 
                            ccc19x$current_status %in% c(1,3)| #Outpatient or ER - new COVID-19 diagnosis
                            ccc19x$worst_status_clinical %in% c(0:3)|
                            ccc19x$worst_complications_severity___0 == 1)] <- 0
    
    #Yes
    ccc19x$der_hosp[which(ccc19x$hosp_status %in% c(1:88) | 
                            ccc19x$current_status %in% c(5:8)|
                            ccc19x$c19_anticoag_reason___3 == 1| #Can only be true if patient was hospitalized
                            ccc19x$worst_status_clinical %in% c(5:8)|
                            ccc19x$labs == '2a'| #Labs drawn at time of hospitalization
                            ccc19x$current_status_clinical %in% c(4:8))] <- 1
    
    #Interventions that could only happen in a hospital
    ccc19x$der_hosp[which(ccc19x$resp_failure_tx %in% 2:6)] <- 1
    
    #Unknown
    ccc19x$der_hosp[which((ccc19x$hosp_status == 99 |
                            ccc19x$worst_status_clinical == 99) & 
                           is.na(ccc19x$der_hosp))] <- 99
    
    #followup forms
    
    #No
    ccc19x$der_hosp[which(ccc19x$hosp_status_fu == 0)] <- 0
    
    #Yes
    ccc19x$der_hosp[which(ccc19x$fu_reason == 1 | 
                            ccc19x$hosp_status_fu %in% c(1:3) | 
                            ccc19x$current_status_fu %in% c(5:8) |
                            ccc19x$current_status_clinical_fu %in% c(4:8) |
                            ccc19x$who_ordinal_scale %in% 3:7|
                            ccc19x$c19_anticoag_reason_fu___3 == 1) #Can only be true if hospitalized
    ] <- 1
    
    #Interventions that could only happen in a hospital
    ccc19x$der_hosp[which(ccc19x$resp_failure_tx_fu %in% 2:6)] <- 1
    
    #Unknown
    ccc19x$der_hosp[which((ccc19x$hosp_status_fu == 99 |
                            ccc19x$current_status_fu == 99 |
                            ccc19x$current_status_clinical_fu == 99) & 
                           is.na(ccc19x$der_hosp))] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_hosp[temp.ref]
      temp2 <- ccc19x$der_hosp[temp.ref][2:length(temp.ref)]
      temp2 <- temp2[!is.na(temp2)]
      if(length(temp[!is.na(temp)]) > 0)
      {
        if(any(temp[!is.na(temp)] == 1)) ccc19x$der_hosp[temp.ref] <- 1 else
        if(length(temp[2:length(temp)][!is.na(temp[2:length(temp)])]) > 0)
        {
          if((is.na(temp[1])|temp[1] == 0) & all(temp2 == 0) & !any(temp[!is.na(temp)] == 1)) ccc19x$der_hosp[temp.ref] <- 0
          if((is.na(temp[1])|temp[1] == 0) & any(temp2 == 99) & !any(temp[!is.na(temp)] == 1)) ccc19x$der_hosp[temp.ref] <- 99
        }
      }
    }
    
    #Factor
    ccc19x$der_hosp <- as.factor(ccc19x$der_hosp)
    
    temp <- summary(ccc19x$der_hosp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_hosp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #O2a. Hospitalization based on baseline form only
    ccc19x$der_hosp_bl <- NA
    
    #Initial form
    
    #No
    ccc19x$der_hosp_bl[which(ccc19x$hosp_status == 0| 
                               ccc19x$current_status %in% c(1,3)| #Outpatient or ER - new COVID-19 diagnosis
                               ccc19x$worst_status_clinical %in% c(0:3)|
                               ccc19x$worst_complications_severity___0 == 1)] <- 0
    
    #Yes
    ccc19x$der_hosp_bl[which(ccc19x$hosp_status %in% c(1:88) | 
                               ccc19x$current_status %in% c(5:8)|
                               ccc19x$c19_anticoag_reason___3 == 1| #Can only be true if patient was hospitalized
                               ccc19x$worst_status_clinical %in% c(5:8)| 
                               ccc19x$labs == '2a'| #Labs drawn at time of hospitalization
                               ccc19x$current_status_clinical %in% c(4:8))] <- 1
    
    #Interventions that could only happen in a hospital
    ccc19x$der_hosp_bl[which(ccc19x$resp_failure_tx %in% 2:6)] <- 1
    
    #Unknown
    ccc19x$der_hosp_bl[which((ccc19x$hosp_status == 99 |
                                ccc19x$worst_status_clinical == 99) & 
                               is.na(ccc19x$der_hosp_bl))] <- 99
    
    #Factor
    ccc19x$der_hosp_bl <- as.factor(ccc19x$der_hosp_bl)
    
    temp <- summary(ccc19x$der_hosp_bl[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_hosp_bl',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #O2b. Hospitalization within first 30 days
    ccc19x$der_hosp_30 <- NA
    {
      #Definite - diagnosis-to-hospital interval is provided and less than or equal to 30
      temp.ref <- which(ccc19x$dx_hosp_interval <= 30)
      ccc19x$der_hosp_30[temp.ref] <- 'Definite'
      
      #Definitely not - diagnosis-to-hospital interval is provided and greater than 30 (and less than 9999)
      temp.ref <- which(ccc19x$dx_hosp_interval > 30 & ccc19x$dx_hosp_interval < 9999)
      ccc19x$der_hosp_30[temp.ref] <- 'Definitely not'
      
      #Definitely not - der_hosp is 0
      temp.ref <- which(ccc19x$der_hosp == 0 & is.na(ccc19x$der_hosp_30))
      ccc19x$der_hosp_30[temp.ref] <- 'Definitely not'
      
      #Definite - baseline form has hospitalization and interval from diagnosis to reporting is 4 weeks or less
      temp.ref <- which(ccc19x$der_hosp_bl == 1 & ccc19x$covid_19_dx_interval %in% 1:3 &
                          is.na(ccc19x$der_hosp_30))
      ccc19x$der_hosp_30[temp.ref] <- 'Definite'
      
      #Definite - follow-up form has hospitalization as reason for f/u and within 30 days
      temp.ref <- which(ccc19x$fu_reason == 1 & 
                          ((ccc19x$fu_weeks == 'OTH' & ccc19x$timing_of_report_weeks <= 4)|
                             ccc19x$fu_weeks == 30) &
                          is.na(ccc19x$der_hosp_30))
      ccc19x$der_hosp_30[ccc19x$record_id %in% ccc19x$record_id[temp.ref]] <- 'Definite'
      
      #Definitely not - baseline form doesn't have hospitalization, f/u does but is >30 days
      temp <- ccc19x$record_id[which(ccc19x$fu_reason == 1 & 
                                       ((ccc19x$fu_weeks == 'OTH' & ccc19x$timing_of_report_weeks > 4)|
                                          ccc19x$fu_weeks %in% c(90,180,365)))]
      temp.ref <- which(ccc19x$record_id %in% temp &
                          ccc19x$der_hosp_bl == 0 &
                          is.na(ccc19x$der_hosp_30))
      ccc19x$der_hosp_30[temp.ref] <- 'Definitely not'
      
      #Unknown hospitalization status
      temp.ref <- which(ccc19x$der_hosp == 99 & is.na(ccc19x$der_hosp_30))
      ccc19x$der_hosp_30[temp.ref] <- 'Unknown'
      
      #The remainder are possibly hospitalized within 30 days
      temp.ref <- which(ccc19x$der_hosp == 1 & is.na(ccc19x$der_hosp_30))
      ccc19x$der_hosp_30[temp.ref] <- 'Possible'
      
      #Factor
      ccc19x$der_hosp_30 <- as.factor(ccc19x$der_hosp_30)
      
      temp <- summary(ccc19x$der_hosp_30[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_hosp_30',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
    }
    
    #O2c. Hospitalization within first 14 days
    ccc19x$der_hosp_14 <- NA
    
    #Definite - diagnosis-to-hospital interval is provided and less than or equal to 14
    temp.ref <- which(ccc19x$dx_hosp_interval <= 14)
    ccc19x$der_hosp_14[temp.ref] <- 'Definite'
    
    #Definitely not - diagnosis-to-hospital interval is provided and greater than 14 (and less than 9999)
    temp.ref <- which(ccc19x$dx_hosp_interval > 14 & ccc19x$dx_hosp_interval < 9999)
    ccc19x$der_hosp_14[temp.ref] <- 'Definitely not'
    
    #Definitely not - der_hosp is 0
    temp.ref <- which(ccc19x$der_hosp == 0 & is.na(ccc19x$der_hosp_14))
    ccc19x$der_hosp_14[temp.ref] <- 'Definitely not'
    
    #Definite - baseline form has hospitalization and interval from diagnosis to reporting is 2 weeks or less
    temp.ref <- which(ccc19x$der_hosp_bl == 1 & ccc19x$covid_19_dx_interval %in% 1:2 &
                        is.na(ccc19x$der_hosp_14))
    ccc19x$der_hosp_14[temp.ref] <- 'Definite'
    
    #Definite - follow-up form has hospitalization as reason for f/u and within 14 days
    temp.ref <- which(ccc19x$fu_reason == 1 & 
                        ccc19x$fu_weeks == 'OTH' & 
                        ccc19x$timing_of_report_weeks <= 2 &
                        is.na(ccc19x$der_hosp_14))
    ccc19x$der_hosp_14[ccc19x$record_id %in% ccc19x$record_id[temp.ref]] <- 'Definite'
    
    #Definitely not - baseline form doesn't have hospitalization, f/u does but is >14 days
    temp <- ccc19x$record_id[which(ccc19x$fu_reason == 1 & 
                                     ((ccc19x$fu_weeks == 'OTH' & ccc19x$timing_of_report_weeks > 2)|
                                        ccc19x$fu_weeks %in% c(30,90,180,365)))]
    temp.ref <- which(ccc19x$record_id %in% temp &
                        ccc19x$der_hosp_bl == 0 &
                        is.na(ccc19x$der_hosp_14))
    ccc19x$der_hosp_14[temp.ref] <- 'Definitely not'
    
    #Unknown timing and not otherwise already calculated
    temp.ref <- which(ccc19x$dx_hosp_interval == 9999 & is.na(ccc19x$der_hosp_14))
    ccc19x$der_hosp_14[temp.ref] <- 'Unknown'
    
    #Unknown hospitalization status
    temp.ref <- which(ccc19x$der_hosp == 99 & is.na(ccc19x$der_hosp_14))
    ccc19x$der_hosp_14[temp.ref] <- 'Unknown'
    
    #The remainder are possibly hospitalized within 14 days
    temp.ref <- which(ccc19x$der_hosp == 1 & is.na(ccc19x$der_hosp_14))
    ccc19x$der_hosp_14[temp.ref] <- 'Possible'
    
    #Factor
    ccc19x$der_hosp_14 <- as.factor(ccc19x$der_hosp_14)
    
    temp <- summary(ccc19x$der_hosp_14[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_hosp_14',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #O2d. Hospitalization with attribution
    ccc19x$der_hosp_attrib <- NA
    
    #Baseline form hospitalizations
    temp.ref <- which(ccc19x$der_hosp_bl == 1 & ccc19x$redcap_repeat_instrument == '')
    
    temp.ref2 <- which(ccc19x$dx_hosp_interval[temp.ref] < -7|
                         ((ccc19x$c19_workup_why_2___1[temp.ref] == 0 & ccc19x$symptoms___84387000[temp.ref] == 1) & 
                            (ccc19x$c19_workup_why_2___2[temp.ref] == 1|ccc19x$c19_workup_why_2___3[temp.ref] == 1|ccc19x$c19_workup_why_2___4[temp.ref] == 1|ccc19x$c19_workup_why_2___5[temp.ref] == 1)))
    ccc19x$der_hosp_attrib[temp.ref][temp.ref2] <- 'Unrelated'
    
    temp.ref2 <- which(((ccc19x$dx_hosp_interval[temp.ref] >= -7 & ccc19x$dx_hosp_interval[temp.ref] < 0)|
                         (ccc19x$dx_hosp_interval[temp.ref] > 14 & ccc19x$dx_hosp_interval[temp.ref] < 9999)|
                         (ccc19x$dx_hosp_interval[temp.ref] == 0 & ccc19x$c19_workup_why_2___1[temp.ref] == 0)) &
                         is.na(ccc19x$der_hosp_attrib[temp.ref]))
    ccc19x$der_hosp_attrib[temp.ref][temp.ref2] <- 'Possibly related'
    
    temp.ref2 <- which(ccc19x$dx_hosp_interval[temp.ref] >= 0 & ccc19x$dx_hosp_interval[temp.ref] <= 14 &
                         ccc19x$c19_workup_why_2___1[temp.ref] == 1 &
                         is.na(ccc19x$der_hosp_attrib[temp.ref]))
    ccc19x$der_hosp_attrib[temp.ref][temp.ref2] <- 'Definitely related'
    
    temp.ref2 <- which(ccc19x$dx_hosp_interval[temp.ref] >= 0 & ccc19x$dx_hosp_interval[temp.ref] <= 14 &
                         is.na(ccc19x$der_hosp_attrib[temp.ref]))
    ccc19x$der_hosp_attrib[temp.ref][temp.ref2] <- 'Possibly related'
    
    #Follow-up form hospitalizations
    temp.ref <- which(ccc19x$der_hosp_bl %in% c(0,99) & ccc19x$der_hosp == 1
                      & ccc19x$redcap_repeat_instrument == '')
    temp <- ccc19x$record_id[temp.ref]
    for(i in 1:length(temp))
    {
      temp.ref2 <- which(ccc19x$record_id == temp[i] & ccc19x$redcap_repeat_instrument == 'followup')
      temp2 <- ccc19x$admission_reason_fu[temp.ref2]
      temp2 <- temp2[!is.na(temp2)]
      if(length(temp2) > 0)
        if(any(temp2 == 1)) ccc19x$der_hosp_attrib[temp.ref[i]] <- 'Definitely related' else
          if(any(temp2 == 2)) ccc19x$der_hosp_attrib[temp.ref[i]] <- 'Possibly related' else
            if(any(temp2 == 3)) ccc19x$der_hosp_attrib[temp.ref[i]] <- 'Unrelated' else
              ccc19x$der_hosp_attrib[temp.ref[i]] <- 'Unknown'
    }
    
    #Initial severity-based
    temp.ref <- which(ccc19x$severity_of_covid_19_v2 %in% 2:3 & ccc19x$der_hosp_bl == 1 & is.na(ccc19x$der_hosp_attrib))
    ccc19x$der_hosp_attrib[temp.ref] <- 'Definitely related'
      
    #Time anchor-based
    temp.ref <- which(ccc19x$covid_19_dx_interval %in% 1:2 & ccc19x$der_hosp_bl == 1 & is.na(ccc19x$der_hosp_attrib))
    ccc19x$der_hosp_attrib[temp.ref] <- 'Definitely related'
    
    temp.ref <- which(ccc19x$covid_19_dx_interval %in% 3:10 & ccc19x$der_hosp_bl == 1 & is.na(ccc19x$der_hosp_attrib))
    ccc19x$der_hosp_attrib[temp.ref] <- 'Possibly related'
    
    temp.ref <- which(ccc19x$der_hosp == 1 & ccc19x$redcap_repeat_instrument == '' & is.na(ccc19x$der_hosp_attrib))
    temp <- ccc19x$record_id[temp.ref]
    for(i in 1:length(temp))
    {
      temp.ref2 <- which(ccc19x$record_id == temp[i] & ccc19x$redcap_repeat_instrument == 'followup' &
                           !is.na(ccc19x$timing_of_report_weeks) & ccc19x$fu_reason == 1)
      if(length(temp.ref2) > 0)
      {
        temp2 <- ccc19x$timing_of_report_weeks[temp.ref2]
        if(any(temp2 <= 2)) ccc19x$der_hosp_attrib[temp.ref[i]] <- 'Definitely related' else
          ccc19x$der_hosp_attrib[temp.ref[i]] <- 'Possibly related'
      } else ccc19x$der_hosp_attrib[temp.ref[i]] <- 'Possibly related'
    }
    
    #Factor
    ccc19x$der_hosp_attrib <- as.factor(ccc19x$der_hosp_attrib)
    
    temp <- summary(ccc19x$der_hosp_attrib[which(ccc19x$redcap_repeat_instrument == '' & ccc19x$der_hosp == 1)])
    temp.var.log <- data.frame(name = 'der_hosp_attrib',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    "ICU"
    #O3. Derived variable indicating time in the ICU (ever/never)
    ccc19x$der_ICU <- NA
    
    #Initial form
    
    #No
    ccc19x$der_ICU[which((ccc19x$hosp_status %in% c(0:1) | 
                            ccc19x$current_status %in% c(1,3) |
                            ccc19x$worst_status_clinical %in% 0:6 |
                            ccc19x$worst_complications_severity___0 == 1) &
                           is.na(ccc19x$der_ICU))] <- 0
    
    #Yes
    ccc19x$der_ICU[which(ccc19x$hosp_status %in% c(2:3) | 
                           ccc19x$current_status %in% c(7,8) | 
                           ccc19x$worst_status_clinical %in% c("7","8")| 
                           ccc19x$current_status_clinical %in% c("7","8"))] <- 1
    
    #Unknown
    ccc19x$der_ICU[which((ccc19x$hosp_status %in% 88:99 |
                            ccc19x$worst_status_clinical == 99) & 
                           is.na(ccc19x$der_ICU))] <- 99
    
    #followup forms
    
    #No
    ccc19x$der_ICU[which(ccc19x$hosp_status_fu %in% c(0:1) &
                           is.na(ccc19x$der_ICU))] <- 0
    
    #Yes
    ccc19x$der_ICU[which(ccc19x$hosp_status_fu %in% c(2:3) | 
                           ccc19x$current_status_fu %in% c(7,8) | 
                           ccc19x$current_status_clinical_fu %in% c("7","8"))] <- 1
    
    #Unknown
    ccc19x$der_ICU[which((ccc19x$hosp_status_fu == 99 |
                            ccc19x$current_status_fu == 99 |
                            ccc19x$current_status_clinical_fu == 99) & 
                           is.na(ccc19x$der_ICU))] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_ICU[temp.ref]
      temp2 <- ccc19x$der_ICU[temp.ref][2:length(temp.ref)]
      temp2 <- temp2[!is.na(temp2)]
      if(length(temp[!is.na(temp)]) > 0)
      {
        if(any(temp[!is.na(temp)] == 1)) ccc19x$der_ICU[temp.ref] <- 1 else
        if(length(temp[2:length(temp)][!is.na(temp[2:length(temp)])]) > 0)
        {
          if((is.na(temp[1])|temp[1] == 0) & all(temp2 == 0) & !any(temp[!is.na(temp)] == 1)) ccc19x$der_ICU[temp.ref] <- 0
          if((is.na(temp[1])|temp[1] == 0) & any(temp2 == 99) & !any(temp[!is.na(temp)] == 1)) ccc19x$der_ICU[temp.ref] <- 99
        }
      }
    }
    
    #Factor
    ccc19x$der_ICU <- as.factor(ccc19x$der_ICU)
    
    temp <- summary(ccc19x$der_ICU[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ICU',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #O3a. Direct admission to ICU at baseline
    ccc19x$der_ICU_direct <- ccc19x$der_ICU
    ccc19x$der_ICU_direct[which(ccc19x$hosp_status != 3 & ccc19x$der_ICU_direct == 1)] <- 0
    
    temp <- summary(ccc19x$der_ICU_direct[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ICU_direct',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    "mv"
    #O4. derived variable indicating if patients were intubated or not
    ccc19x$der_mv <- NA
    
    #Yes
    
    #Baseline
    ccc19x$der_mv[which(ccc19x$resp_failure_tx ==6 | 
                                 ccc19x$current_status_clinical == 8 | 
                                 ccc19x$worst_status_clinical == 8)] <- 1
    
    #Follow-up
    ccc19x$der_mv[which(ccc19x$resp_failure_tx_fu ==6 | 
                                 ccc19x$current_status_clinical_fu == 8 | 
                                 ccc19x$who_ordinal_scale %in% 6:7)] <- 1
    
    #No
    
    #Baseline
    ccc19x$der_mv[which((ccc19x$o2_requirement_c19 == 0 |
                           ccc19x$resp_failure_tx %in% 1:5 |
                           ccc19x$worst_status_clinical %in% 0:7 |
                           ccc19x$worst_complications_severity___0 == 1) &
                          is.na(ccc19x$der_mv))] <- 0
    
    #Follow-up
    ccc19x$der_mv[which((ccc19x$o2_requirement_fu == 0 |
                                  ccc19x$resp_failure_tx_fu %in% 1:5) &
                                 is.na(ccc19x$der_mv))] <- 0
    
    #Unknown
    
    #Baseline
    ccc19x$der_mv[which((ccc19x$o2_requirement_c19 == 99 |
                                 ccc19x$resp_failure_tx == 99 |
                                 ccc19x$worst_status_clinical == 99) &
                                 is.na(ccc19x$der_mv))] <- 99
    
    #Followup
    ccc19x$der_mv[which((ccc19x$o2_requirement_fu == 99 |
                                  ccc19x$resp_failure_tx_fu == 99) &
                                 is.na(ccc19x$der_mv))] <- 99
    
    #Revert unknowns if no pulmonary complications and no respiratory failure
    
    #Baseline
    ccc19x$der_mv[which(ccc19x$c19_complications_pulm___409622000 == 0 &
                          ccc19x$c19_complications_pulm___none == 1 &
                          (is.na(ccc19x$der_mv)|ccc19x$der_mv ==99))] <- 0
    
    #Followup
    ccc19x$der_mv[which(ccc19x$c19_complications_pulm_fu___409622000 == 0 &
                          ccc19x$c19_complications_pulm_fu___none == 1 &
                          (is.na(ccc19x$der_mv)|ccc19x$der_mv ==99))] <- 0
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_mv[temp.ref]
      temp2 <- ccc19x$der_mv[temp.ref][2:length(temp.ref)]
      temp2 <- temp2[!is.na(temp2)]
      if(length(temp[!is.na(temp)]) > 0)
      {
        if(any(temp[!is.na(temp)] == 1)) ccc19x$der_mv[temp.ref] <- 1 else
        if(length(temp[2:length(temp)][!is.na(temp[2:length(temp)])]) > 0)
        {
          if((is.na(temp[1])|temp[1] == 0) & all(temp2 == 0) & !any(temp[!is.na(temp)] == 1)) ccc19x$der_mv[temp.ref] <- 0
          if((is.na(temp[1])|temp[1] == 0) & any(temp2 == 99) & !any(temp[!is.na(temp)] == 1)) ccc19x$der_mv[temp.ref] <- 99
        }
      }
    }
    
    #Revert patients with unknown status who were on hospice
    temp <- ccc19x$record_id[which(ccc19x$der_mv == 99 & ccc19x$hospice == 1)]
    ccc19x$der_mv[ccc19x$record_id %in% temp] <- 0
    
    #Factor
    ccc19x$der_mv <- as.factor(ccc19x$der_mv)
    
    temp <- summary(ccc19x$der_mv[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_mv',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #O3c. Composite of ICU or mechanical ventilation
    ccc19x$der_ICU_mv <- NA
    ccc19x$der_ICU_mv[which(ccc19x$der_ICU == 1 | ccc19x$der_mv == 1)] <- 1
    ccc19x$der_ICU_mv[which((ccc19x$der_ICU == 99 | ccc19x$der_mv == 99) &
                              is.na(ccc19x$der_ICU_mv))] <- 99
    ccc19x$der_ICU_mv[which(ccc19x$der_ICU == 0 & ccc19x$der_mv == 0)] <- 0
    ccc19x$der_ICU_mv <- as.factor(ccc19x$der_ICU_mv)
    
    temp <- summary(ccc19x$der_ICU_mv[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ICU_mv',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    "recovered"                           
    #O5. Derived recovery variable
    ccc19x$der_recovered <- NA
    
    #Yes, initial form
    ccc19x$der_recovered[which(ccc19x$current_status_v2 %in% c("1", "1b") |
                                 ccc19x$current_status_retro %in% c("1", "1b"))] <- 1
    
    #Yes, followup form
    ccc19x$der_recovered[which(ccc19x$covid_19_status_fu %in% c("1", "1b") |
                                 ccc19x$c19_status_fu_final %in% c("1", "1b"))] <- 1
    
    #No, initial form
    ccc19x$der_recovered[which(ccc19x$current_status_v2 == '2' |
                                 (ccc19x$current_status_v2 == '3' & ccc19x$cause_of_death_2 %in% c(1,3))|
                                 (ccc19x$current_status_retro == '3' & ccc19x$cause_of_death_2 %in% c(1,3)))] <- 0
    
    #No, followup form (including died with COVID-19 as a factor)
    ccc19x$der_recovered[which(ccc19x$covid_19_status_fu == '2' |
                                 (ccc19x$covid_19_status_fu == '3' & ccc19x$cause_of_death_fu %in% c(1,3))|
                                 (ccc19x$c19_status_fu_final == '3' & ccc19x$cause_of_death_fu %in% c(1,3)))] <- 0
    
    #Unknown
    ccc19x$der_recovered[which(ccc19x$current_status_v2 == 99|
                                 ccc19x$current_status_retro == 99|
                                 ccc19x$covid_19_status_fu == 99|
                                 ccc19x$c19_status_fu_final == 99)] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_recovered[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_recovered[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_recovered[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_recovered[temp.ref] <- 0
      }
    }
    
    #Factor
    ccc19x$der_recovered <- as.factor(ccc19x$der_recovered)
    
    temp <- summary(ccc19x$der_recovered[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_recovered',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ###########################
    #O6. Combined days to death
    ###########################
    ccc19x$der_days_to_death_combined <- NA
    temp <- unique(ccc19x$record_id)
    for(i in 1:length(temp))
    {
      temp.ref <- which(ccc19x$record_id == temp[i])
      temp2 <- c(ccc19x$days_to_death[temp.ref], 
                 ccc19x$days_to_death_2[temp.ref],
                 ccc19x$days_to_death_fu[temp.ref],
                 ccc19x$days_to_death_fu_2[temp.ref])
      temp2 <- temp2[!is.na(temp2)]
      if(length(temp2) > 0)
        ccc19x$der_days_to_death_combined[temp.ref] <- min(temp2)
    }
    
    temp <- summary(ccc19x$der_days_to_death_combined[which(ccc19x$der_deadbinary == 1 &
                                                              ccc19x$der_days_to_death_combined != 9999)])
    temp2 <- sum(is.na(ccc19x$der_days_to_death_combined[which(ccc19x$der_deadbinary == 1)]))
    
    temp.var.log <- data.frame(name = 'der_days_to_death_combined',
                               timestamp = Sys.time(),
                               values = paste(c(paste('Median:', temp[3], 'days'),
                                                paste('IQR:', temp[2], '-', temp[5]),
                                                paste('9999:', length(which(ccc19x$der_days_to_death_combined == 9999))),
                                                paste('NA:', temp2)), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #O7. supplemental O2
    ccc19x$der_o2_ever <- NA
    
    #Yes
    ccc19x$der_o2_ever[which(ccc19x$o2_requirement == 1)] <- 1
    ccc19x$der_o2_ever[which(ccc19x$o2_requirement_c19 == 1)] <- 1
    ccc19x$der_o2_ever[which(ccc19x$resp_failure_tx %in% c(1:6))] <- 1
    ccc19x$der_o2_ever[which(ccc19x$o2_requirement_fu == 1)] <- 1
    ccc19x$der_o2_ever[which(ccc19x$resp_failure_tx_fu %in% c(1:6))] <- 1
    
    #No
    
    #Baseline
    ccc19x$der_o2_ever[which(ccc19x$o2_requirement %in% c('',0,99) & 
                               ccc19x$o2_requirement_c19 == 0 &
                               ccc19x$c19_complications_pulm___409622000 == 0 &
                               is.na(ccc19x$der_o2_ever))] <- 0
    
    #Follow-up
    ccc19x$der_o2_ever[which(ccc19x$o2_requirement_fu == 0 &
                               ccc19x$c19_complications_pulm_fu___409622000 == 0 &
                               is.na(ccc19x$der_o2_ever))] <- 0
    
    #Unknown
    
    #Baseline
    ccc19x$der_o2_ever[which((ccc19x$o2_requirement_c19 == 99|
                               (ccc19x$c19_complications_pulm___409622000 == 1 &
                                  ccc19x$resp_failure_tx == 99)) &
                               is.na(ccc19x$der_o2_ever))] <- 99
    
    #Follow-up
    ccc19x$der_o2_ever[which((ccc19x$o2_requirement_fu == 99|
                                (ccc19x$c19_complications_pulm_fu___409622000 == 1 &
                                   ccc19x$resp_failure_tx_fu == 99)) &
                               is.na(ccc19x$der_o2_ever))] <- 99
    
    #Create a temporal table
    temp <- max(ccc19x$redcap_repeat_instance[ccc19x$redcap_repeat_instrument == 'followup'])
    time.df <- as.data.frame(matrix(nrow = length(unique(ccc19x$record_id)), ncol = temp+2), stringsAsFactors = F)
    colnames(time.df)[1] <- 'record_id'
    colnames(time.df)[2:ncol(time.df)] <- paste('t', 0:temp, sep = '')
    time.df$record_id <- unique(ccc19x$record_id)
    
    #Baseline
    temp <- merge(time.df, ccc19x[ccc19x$redcap_repeat_instrument == '', c('record_id', 'der_o2_ever')], all.x = T)
    temp <- temp[order(time.df$record_id),]
    time.df[,2] <- temp$der_o2_ever
    
    #Followup
    for(i in 3:ncol(time.df))
    {
      temp <- merge(time.df, ccc19x[ccc19x$redcap_repeat_instance == i - 2, c('record_id', 'der_o2_ever')], all.x = T)
      temp <- temp[order(time.df$record_id),]
      time.df[,i] <- temp$der_o2_ever
    }
    rm(temp)  
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_o2_ever[temp.ref]
      temp2 <- ccc19x$der_o2_ever[temp.ref][2:length(temp.ref)]
      temp2 <- temp2[!is.na(temp2)]
      if(length(temp[!is.na(temp)]) > 0)
      {
        if(any(temp[!is.na(temp)] == 1)) ccc19x$der_o2_ever[temp.ref] <- 1
        if(length(temp[2:length(temp)][!is.na(temp[2:length(temp)])]) > 0)
        {
          if((is.na(temp[1])|temp[1] == 0) & all(temp2 == 0) & !any(temp[!is.na(temp)] == 1)) ccc19x$der_o2_ever[temp.ref] <- 0
          if((is.na(temp[1])|temp[1] == 0) & any(temp2 == 99) & !any(temp[!is.na(temp)] == 1)) ccc19x$der_o2_ever[temp.ref] <- 99
        }
      }
    }
    
    #Factor
    ccc19x$der_o2_ever <- as.factor(ccc19x$der_o2_ever)
    
    temp <- summary(ccc19x$der_o2_ever[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_o2_ever',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #O7a. supplemental O2 alternative definition, allowing some unknowns in the follow-up forms
    ccc19x$der_o2_ever_v2 <- NA
    
    #Yes
    ccc19x$der_o2_ever_v2[which(ccc19x$o2_requirement == 1)] <- 1
    ccc19x$der_o2_ever_v2[which(ccc19x$o2_requirement_c19 == 1)] <- 1
    ccc19x$der_o2_ever_v2[which(ccc19x$resp_failure_tx %in% c(1:6))] <- 1
    ccc19x$der_o2_ever_v2[which(ccc19x$o2_requirement_fu == 1)] <- 1
    ccc19x$der_o2_ever_v2[which(ccc19x$resp_failure_tx_fu %in% c(1:6))] <- 1
    
    #No
    
    #Baseline
    ccc19x$der_o2_ever_v2[which(ccc19x$o2_requirement %in% c('',0,99) & 
                                  ccc19x$o2_requirement_c19 == 0 &
                                  ccc19x$c19_complications_pulm___409622000 == 0 &
                                  is.na(ccc19x$der_o2_ever_v2))] <- 0
    
    #Follow-up
    ccc19x$der_o2_ever_v2[which(ccc19x$o2_requirement_fu == 0 &
                                  ccc19x$c19_complications_pulm_fu___409622000 == 0 &
                                  is.na(ccc19x$der_o2_ever_v2))] <- 0
    
    #Unknown
    
    #Baseline
    ccc19x$der_o2_ever_v2[which((ccc19x$o2_requirement_c19 == 99|
                                   (ccc19x$c19_complications_pulm___409622000 == 1 &
                                      ccc19x$resp_failure_tx == 99)) &
                                  is.na(ccc19x$der_o2_ever_v2))] <- 99
    
    #Follow-up
    ccc19x$der_o2_ever_v2[which((ccc19x$o2_requirement_fu == 99|
                                   (ccc19x$c19_complications_pulm_fu___409622000 == 1 &
                                      ccc19x$resp_failure_tx_fu == 99)) &
                                  is.na(ccc19x$der_o2_ever_v2))] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_o2_ever_v2[temp.ref]
      temp2 <- ccc19x$der_o2_ever_v2[temp.ref][2:length(temp.ref)]
      temp2 <- temp2[!is.na(temp2)]
      if(length(temp[!is.na(temp)]) > 0)
      {
        if(any(temp[!is.na(temp)] == 1)) ccc19x$der_o2_ever_v2[temp.ref] <- 1
        if(length(temp[2:length(temp)][!is.na(temp[2:length(temp)])]) > 0)
        {
          if((is.na(temp[1])|temp[1] == 0) & any(temp2 == 0) & !any(temp[!is.na(temp)] == 1)) ccc19x$der_o2_ever_v2[temp.ref] <- 0
        }
      }
    }
    
    #Factor
    ccc19x$der_o2_ever_v2 <- as.factor(ccc19x$der_o2_ever_v2)
    
    temp <- summary(ccc19x$der_o2_ever_v2[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_o2_ever_v2',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #O8a. Severe composite outcome - mechanical ventilation, severe illness requiring hospitalization, intensive care unit (ICU) requirement, or death
    ccc19x$der_severe <- NA
    
    #Present
    ccc19x$der_severe[which(ccc19x$der_deadbinary == 1)] <- 1
    ccc19x$der_severe[which(ccc19x$der_mv == 1)] <- 1
    ccc19x$der_severe[which(ccc19x$der_ICU == 1)] <- 1
    ccc19x$der_severe[which(ccc19x$severity_of_covid_19_v2 == 3)] <- 1
    ccc19x$der_severe[which(ccc19x$current_status_clinical %in% 6:8)] <- 1
    ccc19x$der_severe[which(ccc19x$worst_status_clinical %in% 6:8)] <- 1
    ccc19x$der_severe[which(ccc19x$current_status_clinical_fu %in% 6:8)] <- 1
    
    #Absent (requires all 3 derived variables to be absent, and not meeting another criteria)
    ccc19x$der_severe[which(ccc19x$der_deadbinary == 0 &
                              ccc19x$der_ICU == 0 &
                              ccc19x$der_mv == 0 &
                              is.na(ccc19x$der_severe))] <- 0
    
    #Unknown (requires all 3 derived variables to be 99)
    ccc19x$der_severe[which(ccc19x$der_deadbinary == 99 &
                               ccc19x$der_ICU == 99 &
                               ccc19x$der_mv == 99)] <- 99
    
    #Factor
    ccc19x$der_severe <- as.factor(ccc19x$der_severe)
    
    temp <- summary(ccc19x$der_severe[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_severe',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #O8b. Severe composite outcome v2 - mechanical ventilation, intensive care unit (ICU) requirement, or death
    ccc19x$der_severe2 <- NA
    ccc19x$der_severe2[which(ccc19x$der_deadbinary == 1)] <- 1
    ccc19x$der_severe2[which(ccc19x$der_mv == 1)] <- 1
    ccc19x$der_severe2[which(ccc19x$der_ICU == 1)] <- 1
    ccc19x$der_severe2[which(ccc19x$current_status_clinical %in% 7:8)] <- 1
    ccc19x$der_severe2[which(ccc19x$worst_status_clinical %in% 7:8)] <- 1
    ccc19x$der_severe2[which(ccc19x$current_status_clinical %in% 7:8)] <- 1
    
    #Absent (requires all 3 derived variables to be absent, and not meeting another criteria)
    ccc19x$der_severe2[which(ccc19x$der_deadbinary == 0 &
                              ccc19x$der_ICU == 0 &
                              ccc19x$der_mv == 0 &
                              is.na(ccc19x$der_severe2))] <- 0
    
    #Unknown (requires all 3 derived variables to be 99)
    ccc19x$der_severe2[which(ccc19x$der_deadbinary == 99 &
                              ccc19x$der_ICU == 99 &
                              ccc19x$der_mv == 99)] <- 99
    
    #Factor
    ccc19x$der_severe2 <- as.factor(ccc19x$der_severe2)
    
    temp <- summary(ccc19x$der_severe2[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_severe2',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #O8c. Severe composite outcome v3 - death, hospitalization with oxygen requirement, ICU admission/need for mechanical ventilation
    ccc19x$der_severe3 <- NA
    
    #Present
    ccc19x$der_severe3[which(ccc19x$der_deadbinary == 1)] <- 1
    ccc19x$der_severe3[which(ccc19x$der_hosp == 1 & ccc19x$der_o2_ever == 1)] <- 1
    ccc19x$der_severe3[which(ccc19x$der_ICU == 1)] <- 1
    ccc19x$der_severe3[which(ccc19x$der_mv == 1)] <- 1
    
    #Absent (requires all five derived variables to be 0, with extra logic for hosp)
    ccc19x$der_severe3[which(ccc19x$der_deadbinary == 0 &
                               ((ccc19x$der_hosp == 1 & ccc19x$der_o2_ever == 0)|ccc19x$der_hosp == 0) &
                               ccc19x$der_ICU == 0 &
                               ccc19x$der_mv == 0)] <- 0
    
    #Unknown (requires all five derived variables to be 99)
    ccc19x$der_severe3[which(ccc19x$der_deadbinary == 99 &
                               ccc19x$der_hosp == 99 & 
                               ccc19x$der_o2_ever == 99 &
                               ccc19x$der_ICU == 99 &
                               ccc19x$der_mv == 99)] <- 99
    
    #Factor
    ccc19x$der_severe3 <- as.factor(ccc19x$der_severe3)
    
    temp <- summary(ccc19x$der_severe3[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_severe3',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
   
    ####################
    #O28. Cause of death
    ####################
    {
      ccc19x$der_cause_of_death <- NA
      
      #COVID-19
      temp <- ccc19x$record_id[which(ccc19x$cause_of_death == 1|ccc19x$cause_of_death_2 == 1|ccc19x$cause_of_death_fu == 1)]
      ccc19x$der_cause_of_death[ccc19x$record_id %in% temp] <- 1
      
      #Cancer
      temp <- ccc19x$record_id[which(ccc19x$cause_of_death == 2|ccc19x$cause_of_death_2 == 2|ccc19x$cause_of_death_fu == 2)]
      ccc19x$der_cause_of_death[ccc19x$record_id %in% temp] <- 2
      
      #Both
      temp <- ccc19x$record_id[which(ccc19x$cause_of_death == 3|ccc19x$cause_of_death_2 == 3|ccc19x$cause_of_death_fu == 3)]
      ccc19x$der_cause_of_death[ccc19x$record_id %in% temp] <- 3
      
      #Other
      temp <- ccc19x$record_id[which(ccc19x$cause_of_death == 88|ccc19x$cause_of_death_2 == 88|ccc19x$cause_of_death_fu == 88)]
      ccc19x$der_cause_of_death[ccc19x$record_id %in% temp] <- 88
      
      #Unknown
      temp <- ccc19x$record_id[which((ccc19x$cause_of_death == 99|ccc19x$cause_of_death_2 == 99|ccc19x$cause_of_death_fu == 99) &
                                       is.na(ccc19x$der_cause_of_death))]
      ccc19x$der_cause_of_death[ccc19x$record_id %in% temp] <- 99
      
      #Factor
      ccc19x$der_cause_of_death <- as.factor(ccc19x$der_cause_of_death)
      summary(ccc19x$der_cause_of_death[ccc19x$redcap_repeat_instrument == ''])
    }
    
  }
  print('Outcomes completed')
  
  #########
  #Symptoms
  #########
  {
    #S01. Categorical symptoms: asymptomatic, typical, atypical
    ccc19x$der_symptoms_cat <- NA
    
    ccc19x$der_symptoms_cat[which(ccc19x$symptoms___84387000 == 1)] <- "Asymptomatic"
    
    ccc19x$der_symptoms_cat[which((ccc19x$symptoms___386661006 == 1 | ccc19x$symptoms___49727002 == 1 | 
                                     ccc19x$symptoms___267036007 == 1 | ccc19x$symptoms___25064002 == 1 | 
                                     ccc19x$symptoms___68962001 == 1 | ccc19x$symptoms___57676002 == 1 | 
                                     ccc19x$symptoms___162397003 == 1 | ccc19x$symptoms___44169009 == 1 | 
                                     ccc19x$symptoms___36955009 == 1 | ccc19x$symptoms___64531003 == 1 | 
                                     ccc19x$symptoms___422587007 == 1 | ccc19x$symptoms___272044004 == 1 |
                                     ccc19x$symptoms___62315008 == 1 | ccc19x$symptoms___21522001 == 1 | 
                                     ccc19x$symptoms___248595008 == 1))] <- "Typical"
    
    ccc19x$der_symptoms_cat[which((ccc19x$symptoms___9826008 == 1 | ccc19x$symptoms___43364001 == 1 | 
                                     ccc19x$symptoms___oth == 1 | ccc19x$symptoms___367391008 == 1 | 
                                     ccc19x$symptoms___419284004 == 1) & 
                                    (ccc19x$symptoms___386661006 == 0 & ccc19x$symptoms___49727002 == 0 & 
                                       ccc19x$symptoms___267036007 == 0 & ccc19x$symptoms___25064002 == 0 & 
                                       ccc19x$symptoms___68962001 == 0 & ccc19x$symptoms___57676002 == 0 & 
                                       ccc19x$symptoms___162397003 == 0 & ccc19x$symptoms___44169009 == 0 & 
                                       ccc19x$symptoms___36955009 == 0 & ccc19x$symptoms___64531003 == 0 & 
                                       ccc19x$symptoms___422587007 == 0 & ccc19x$symptoms___272044004 == 0 & 
                                       ccc19x$symptoms___62315008 == 0 & ccc19x$symptoms___21522001 == 0 & 
                                       ccc19x$symptoms___248595008 == 0))] <- "Atypical"
    
    ccc19x$der_symptoms_cat[which(ccc19x$symptoms___unk == 1 & 
                                    is.na(ccc19x$der_symptoms_cat))] <- 'Unknown'
    
    ccc19x$der_symptoms_cat <- factor(ccc19x$der_symptoms_cat)
    
    temp <- summary(ccc19x$der_symptoms_cat[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_symptoms_cat',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #S02. Categorical symptoms: typical, atypical, or not known (including asymptomatic)
    ccc19x$der_symptoms_cat_v2 <- 'Not known to have any typical or atypical symptoms at baseline'
    
    ccc19x$der_symptoms_cat_v2[which((ccc19x$symptoms___386661006 == 1 | ccc19x$symptoms___49727002 == 1 | 
                                        ccc19x$symptoms___267036007 == 1 | ccc19x$symptoms___25064002 == 1 | 
                                        ccc19x$symptoms___68962001 == 1 | ccc19x$symptoms___57676002 == 1 | 
                                        ccc19x$symptoms___162397003 == 1 | ccc19x$symptoms___44169009 == 1 | 
                                        ccc19x$symptoms___36955009 == 1 | ccc19x$symptoms___64531003 == 1 | 
                                        ccc19x$symptoms___422587007 == 1 | ccc19x$symptoms___272044004 == 1 |
                                        ccc19x$symptoms___62315008 == 1 | ccc19x$symptoms___21522001 == 1 | 
                                        ccc19x$symptoms___248595008 == 1))] <- "Typical"
    
    ccc19x$der_symptoms_cat_v2[which((ccc19x$symptoms___9826008 == 1 | ccc19x$symptoms___43364001 == 1 | 
                                        ccc19x$symptoms___oth == 1 | ccc19x$symptoms___367391008 == 1 | 
                                        ccc19x$symptoms___419284004 == 1) & 
                                       (ccc19x$symptoms___386661006 == 0 & ccc19x$symptoms___49727002 == 0 & 
                                          ccc19x$symptoms___267036007 == 0 & ccc19x$symptoms___25064002 == 0 & 
                                          ccc19x$symptoms___68962001 == 0 & ccc19x$symptoms___57676002 == 0 & 
                                          ccc19x$symptoms___162397003 == 0 & ccc19x$symptoms___44169009 == 0 & 
                                          ccc19x$symptoms___36955009 == 0 & ccc19x$symptoms___64531003 == 0 & 
                                          ccc19x$symptoms___422587007 == 0 & ccc19x$symptoms___272044004 == 0 & 
                                          ccc19x$symptoms___62315008 == 0 & ccc19x$symptoms___21522001 == 0 & 
                                          ccc19x$symptoms___248595008 == 0))] <- "Atypical"
    
    ccc19x$der_symptoms_cat_v2 <- factor(ccc19x$der_symptoms_cat_v2)
    
    temp <- summary(ccc19x$der_symptoms_cat_v2[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_symptoms_cat_v2',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #S03. Categorical symptoms: asymptomatic, symptomatic
    ccc19x$der_symptoms_cat_v3 <- NA
    
    ccc19x$der_symptoms_cat_v3[which(ccc19x$symptoms___84387000 == 1)] <- "Asymptomatic"
    
    ccc19x$der_symptoms_cat_v3[which(ccc19x$symptoms___386661006 == 1 | ccc19x$symptoms___49727002 == 1 | 
                                       ccc19x$symptoms___267036007 == 1 | ccc19x$symptoms___25064002 == 1 | 
                                       ccc19x$symptoms___68962001 == 1 | ccc19x$symptoms___57676002 == 1 | 
                                       ccc19x$symptoms___162397003 == 1 | ccc19x$symptoms___44169009 == 1 | 
                                       ccc19x$symptoms___36955009 == 1 | ccc19x$symptoms___64531003 == 1 | 
                                       ccc19x$symptoms___422587007 == 1 | ccc19x$symptoms___272044004 == 1 |
                                       ccc19x$symptoms___62315008 == 1 | ccc19x$symptoms___21522001 == 1 | 
                                       ccc19x$symptoms___248595008 == 1| ccc19x$symptoms___9826008 == 1 | ccc19x$symptoms___43364001 == 1 | 
                                       ccc19x$symptoms___oth == 1 | ccc19x$symptoms___367391008 == 1 | 
                                       ccc19x$symptoms___419284004 == 1)] <- "Symptomatic"
    
    ccc19x$der_symptoms_cat_v3[which(ccc19x$symptoms___unk == 1 & 
                                       is.na(ccc19x$der_symptoms_cat_v3))] <- 'Unknown'
    
    ccc19x$der_symptoms_cat_v3 <- factor(ccc19x$der_symptoms_cat_v3)
    
    temp <- summary(ccc19x$der_symptoms_cat_v3[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_symptoms_cat_v3',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    
  }
  print('Symptoms completed')
  
  ##############
  #Complications
  ##############
  {
    #Something is checked besides unknown (including none)
    master_comp <- rep(NA,nrow(ccc19x))
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'complications_') & !grepl(colnames(ccc19x), pattern = 'unk'))
    for(i in 1:nrow(ccc19x))
    {
      temp <- ccc19x[i,temp.ref]
      temp <- temp[!is.na(temp)]
      if(any(temp == 1)) master_comp[i] <- 1
    }
    
    #Thrombotic complications
    {
      #Comp01. PE complications
      ccc19x$der_PE_comp <- NA
      temp.ref <- which(grepl(colnames(ccc19x), pattern = '59282003') & grepl(colnames(ccc19x), pattern = 'complications'))
      
      #Present
      for(i in temp.ref)
        ccc19x$der_PE_comp[which(ccc19x[,i] == 1)] <- 1
      
      #Not present, something else checked besides unknown
      ccc19x$der_PE_comp[which(master_comp == 1 & is.na(ccc19x$der_PE_comp))] <- 0
      
      #Unknown
      
      #Baseline
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk', 'c19_complications_pulm___unk'))
      for(i in which(is.na(ccc19x$der_PE_comp) & ccc19x$redcap_repeat_instrument == ''))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_PE_comp[i] <- 99
      
      #Followup
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk', 'c19_complications_pulm_fu___unk'))
      for(i in which(is.na(ccc19x$der_PE_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_PE_comp[i] <- 99
      
      #Merge baseline and followup if discrepancy
      for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
      {
        temp.ref <- which(ccc19x$record_id == i)
        temp <- ccc19x$der_PE_comp[temp.ref]
        temp <- as.numeric(unique(temp[!is.na(temp)]))
        if(length(temp) > 0)
        {
          if(any(temp == 1)) ccc19x$der_PE_comp[temp.ref] <- 1
          if(!any(temp == 1) & any(temp == 99)) ccc19x$der_PE_comp[temp.ref] <- 99
          if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_PE_comp[temp.ref] <- 0
        }
      }
      
      ccc19x$der_PE_comp <- as.factor(ccc19x$der_PE_comp)
      
      temp <- summary(ccc19x$der_PE_comp[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_PE_comp',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      #Comp01a. PE complications within 30 days
      ccc19x$der_PE_comp_within_30d <- NA
      temp.ref <- which(grepl(colnames(ccc19x), pattern = '59282003') & grepl(colnames(ccc19x), pattern = 'complications'))
      temp.ref2 <- which(ccc19x$redcap_repeat_instrument == ''|
                           ccc19x$fu_weeks %in% c(30)|
                           ccc19x$timing_of_report_weeks <= 4)
      #Present
      for(i in temp.ref)
        ccc19x$der_PE_comp_within_30d[temp.ref2][which(ccc19x[temp.ref2,i] == 1)] <- 1
      
      #Not present, something else checked besides unknown
      temp.ref <- which(grepl(colnames(ccc19x), pattern = 'complications_pulm|complications_card') & !grepl(colnames(ccc19x), pattern = '59282003|unk'))
      for(i in temp.ref2)
        if(any(ccc19x[i,temp.ref] == 1) & !is.na(any(ccc19x[i,temp.ref] == 1)) & is.na(ccc19x$der_PE_comp_within_30d[i])) ccc19x$der_PE_comp_within_30d[i] <- 0
      
      #Unknown
      
      #Baseline
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk', 'c19_complications_pulm___unk'))
      for(i in which(is.na(ccc19x$der_PE_comp_within_30d) & ccc19x$redcap_repeat_instrument == ''))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_PE_comp_within_30d[i] <- 99
      
      #Followup
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk', 'c19_complications_pulm_fu___unk'))
      for(i in which(is.na(ccc19x$der_PE_comp_within_30d) & ccc19x$redcap_repeat_instrument == 'followup' &
                     (ccc19x$fu_weeks %in% c(30,90)|ccc19x$timing_of_report_weeks <= 13)))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_PE_comp_within_30d[i] <- 99
      
      #Merge baseline and followup if discrepancy
      for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
      {
        temp.ref <- which(ccc19x$record_id == i)
        temp <- ccc19x$der_PE_comp_within_30d[temp.ref]
        temp <- as.numeric(unique(temp[!is.na(temp)]))
        if(length(temp) > 0)
        {
          if(any(temp == 1)) ccc19x$der_PE_comp_within_30d[temp.ref] <- 1
          if(!any(temp == 1) & any(temp == 99)) ccc19x$der_PE_comp_within_30d[temp.ref] <- 99
          if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_PE_comp_within_30d[temp.ref] <- 0
        }
      }
      
      ccc19x$der_PE_comp_within_30d <- as.factor(ccc19x$der_PE_comp_within_30d)
      
      temp <- summary(ccc19x$der_PE_comp_within_30d[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_PE_comp_within_30d',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      #Comp01b. PE complications within 90 days (3 months)
      ccc19x$der_PE_comp_within_3mo <- NA
      temp.ref <- which(grepl(colnames(ccc19x), pattern = '59282003') & grepl(colnames(ccc19x), pattern = 'complications'))
      temp.ref2 <- which(ccc19x$redcap_repeat_instrument == ''|
                           ccc19x$fu_weeks %in% c(30,90)|
                           ccc19x$timing_of_report_weeks <= 13)
      #Present
      for(i in temp.ref)
        ccc19x$der_PE_comp_within_3mo[temp.ref2][which(ccc19x[temp.ref2,i] == 1)] <- 1
      
      #Not present, something else checked besides unknown
      temp.ref <- which(grepl(colnames(ccc19x), pattern = 'complications_pulm|complications_card') & !grepl(colnames(ccc19x), pattern = '59282003|unk'))
      for(i in temp.ref2)
        if(any(ccc19x[i,temp.ref] == 1) & !is.na(any(ccc19x[i,temp.ref] == 1)) & is.na(ccc19x$der_PE_comp_within_3mo[i])) ccc19x$der_PE_comp_within_3mo[i] <- 0
      
      #Unknown
      
      #Baseline
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk', 'c19_complications_pulm___unk'))
      for(i in which(is.na(ccc19x$der_PE_comp_within_3mo) & ccc19x$redcap_repeat_instrument == ''))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_PE_comp_within_3mo[i] <- 99
      
      #Followup
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk', 'c19_complications_pulm_fu___unk'))
      for(i in which(is.na(ccc19x$der_PE_comp_within_3mo) & ccc19x$redcap_repeat_instrument == 'followup' &
                     (ccc19x$fu_weeks %in% c(30,90)|ccc19x$timing_of_report_weeks <= 13)))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_PE_comp_within_3mo[i] <- 99
      
      #Merge baseline and followup if discrepancy
      for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
      {
        temp.ref <- which(ccc19x$record_id == i)
        temp <- ccc19x$der_PE_comp_within_3mo[temp.ref]
        temp <- as.numeric(unique(temp[!is.na(temp)]))
        if(length(temp) > 0)
        {
          if(any(temp == 1)) ccc19x$der_PE_comp_within_3mo[temp.ref] <- 1
          if(!any(temp == 1) & any(temp == 99)) ccc19x$der_PE_comp_within_3mo[temp.ref] <- 99
          if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_PE_comp_within_3mo[temp.ref] <- 0
        }
      }
      
      ccc19x$der_PE_comp_within_3mo <- as.factor(ccc19x$der_PE_comp_within_3mo)
      
      temp <- summary(ccc19x$der_PE_comp_within_3mo[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_PE_comp_within_3mo',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      #Comp02. SVT complications
      ccc19x$der_SVT_comp <- NA
      temp.ref <- which(grepl(colnames(ccc19x), pattern = '275517008') & grepl(colnames(ccc19x), pattern = 'complications'))
      
      #Present
      for(i in temp.ref)
        ccc19x$der_SVT_comp[which(ccc19x[,i] == 1)] <- 1
      
      #Not present, something else checked besides unknown
      ccc19x$der_SVT_comp[which(master_comp == 1 & is.na(ccc19x$der_SVT_comp))] <- 0
      
      #Unknown
      
      #Baseline
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk'))
      for(i in which(is.na(ccc19x$der_SVT_comp) & ccc19x$redcap_repeat_instrument == ''))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_SVT_comp[i] <- 99
      
      #Followup
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk'))
      for(i in which(is.na(ccc19x$der_SVT_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_SVT_comp[i] <- 99
      
      #Merge baseline and followup if discrepancy
      for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
      {
        temp.ref <- which(ccc19x$record_id == i)
        temp <- ccc19x$der_SVT_comp[temp.ref]
        temp <- as.numeric(unique(temp[!is.na(temp)]))
        if(length(temp) > 0)
        {
          if(any(temp == 1)) ccc19x$der_SVT_comp[temp.ref] <- 1
          if(!any(temp == 1) & any(temp == 99)) ccc19x$der_SVT_comp[temp.ref] <- 99
          if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_SVT_comp[temp.ref] <- 0
        }
      }
      
      ccc19x$der_SVT_comp <- as.factor(ccc19x$der_SVT_comp)
      
      temp <- summary(ccc19x$der_SVT_comp[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_SVT_comp',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      #Comp03. DVT complications
      ccc19x$der_DVT_comp <- NA
      temp.ref <- which(grepl(colnames(ccc19x), pattern = '128053003') & grepl(colnames(ccc19x), pattern = 'complications'))
      
      #Present
      for(i in temp.ref)
        ccc19x$der_DVT_comp[which(ccc19x[,i] == 1)] <- 1
      
      #Not present, something else checked besides unknown
      ccc19x$der_DVT_comp[which(master_comp == 1 & is.na(ccc19x$der_DVT_comp))] <- 0
      
      #Unknown
      
      #Baseline
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk'))
      for(i in which(is.na(ccc19x$der_DVT_comp) & ccc19x$redcap_repeat_instrument == ''))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_DVT_comp[i] <- 99
      
      #Followup
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk'))
      for(i in which(is.na(ccc19x$der_DVT_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_DVT_comp[i] <- 99
      
      #Merge baseline and followup if discrepancy
      for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
      {
        temp.ref <- which(ccc19x$record_id == i)
        temp <- ccc19x$der_DVT_comp[temp.ref]
        temp <- as.numeric(unique(temp[!is.na(temp)]))
        if(length(temp) > 0)
        {
          if(any(temp == 1)) ccc19x$der_DVT_comp[temp.ref] <- 1
          if(!any(temp == 1) & any(temp == 99)) ccc19x$der_DVT_comp[temp.ref] <- 99
          if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_DVT_comp[temp.ref] <- 0
        }
      }
      
      ccc19x$der_DVT_comp <- as.factor(ccc19x$der_DVT_comp)
      
      temp <- summary(ccc19x$der_DVT_comp[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_DVT_comp',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      #Comp03a. DVT complications within 90 days (3 months)
      ccc19x$der_DVT_comp_within_3mo <- NA
      temp.ref <- which(grepl(colnames(ccc19x), pattern = '128053003') & grepl(colnames(ccc19x), pattern = 'complications'))
      temp.ref2 <- which(ccc19x$redcap_repeat_instrument == ''|
                           ccc19x$fu_weeks %in% c(30,90)|
                           ccc19x$timing_of_report_weeks <= 13)
      #Present
      for(i in temp.ref)
        ccc19x$der_DVT_comp_within_3mo[temp.ref2][which(ccc19x[temp.ref2,i] == 1)] <- 1
      
      #Not present, something else checked besides unknown
      temp.ref <- which(grepl(colnames(ccc19x), pattern = 'complications_card') & !grepl(colnames(ccc19x), pattern = '128053003|unk'))
      for(i in temp.ref2)
        if(any(ccc19x[i,temp.ref] == 1) & !is.na(any(ccc19x[i,temp.ref] == 1)) & is.na(ccc19x$der_DVT_comp_within_3mo[i])) ccc19x$der_DVT_comp_within_3mo[i] <- 0
      
      #Unknown
      
      #Baseline
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk'))
      for(i in which(is.na(ccc19x$der_DVT_comp_within_3mo) & ccc19x$redcap_repeat_instrument == ''))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_DVT_comp_within_3mo[i] <- 99
      
      #Followup
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk'))
      for(i in which(is.na(ccc19x$der_DVT_comp_within_3mo) & ccc19x$redcap_repeat_instrument == 'followup' &
                     (ccc19x$fu_weeks %in% c(30,90)|ccc19x$timing_of_report_weeks <= 13)))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_DVT_comp_within_3mo[i] <- 99
      
      #Merge baseline and followup if discrepancy
      for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
      {
        temp.ref <- which(ccc19x$record_id == i)
        temp <- ccc19x$der_DVT_comp_within_3mo[temp.ref]
        temp <- as.numeric(unique(temp[!is.na(temp)]))
        if(length(temp) > 0)
        {
          if(any(temp == 1)) ccc19x$der_DVT_comp_within_3mo[temp.ref] <- 1
          if(!any(temp == 1) & any(temp == 99)) ccc19x$der_DVT_comp_within_3mo[temp.ref] <- 99
          if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_DVT_comp_within_3mo[temp.ref] <- 0
        }
      }
      
      ccc19x$der_DVT_comp_within_3mo <- as.factor(ccc19x$der_DVT_comp_within_3mo)
      
      temp <- summary(ccc19x$der_DVT_comp_within_3mo[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_DVT_comp_within_3mo',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      #Comp03b. DVT complications within 30 days
      ccc19x$der_DVT_comp_within_30d <- NA
      temp.ref <- which(grepl(colnames(ccc19x), pattern = '128053003') & grepl(colnames(ccc19x), pattern = 'complications'))
      temp.ref2 <- which(ccc19x$redcap_repeat_instrument == ''|
                           ccc19x$fu_weeks %in% c(30)|
                           ccc19x$timing_of_report_weeks <= 4)
      #Present
      for(i in temp.ref)
        ccc19x$der_DVT_comp_within_30d[temp.ref2][which(ccc19x[temp.ref2,i] == 1)] <- 1
      
      #Not present, something else checked besides unknown
      temp.ref <- which(grepl(colnames(ccc19x), pattern = 'complications_card') & !grepl(colnames(ccc19x), pattern = '128053003|unk'))
      for(i in temp.ref2)
        if(any(ccc19x[i,temp.ref] == 1) & !is.na(any(ccc19x[i,temp.ref] == 1)) & is.na(ccc19x$der_DVT_comp_within_30d[i])) ccc19x$der_DVT_comp_within_30d[i] <- 0
      
      #Unknown
      
      #Baseline
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk'))
      for(i in which(is.na(ccc19x$der_DVT_comp_within_30d) & ccc19x$redcap_repeat_instrument == ''))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_DVT_comp_within_30d[i] <- 99
      
      #Followup
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk'))
      for(i in which(is.na(ccc19x$der_DVT_comp_within_30d) & ccc19x$redcap_repeat_instrument == 'followup' &
                     (ccc19x$fu_weeks %in% c(30,90)|ccc19x$timing_of_report_weeks <= 13)))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_DVT_comp_within_30d[i] <- 99
      
      #Merge baseline and followup if discrepancy
      for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
      {
        temp.ref <- which(ccc19x$record_id == i)
        temp <- ccc19x$der_DVT_comp_within_30d[temp.ref]
        temp <- as.numeric(unique(temp[!is.na(temp)]))
        if(length(temp) > 0)
        {
          if(any(temp == 1)) ccc19x$der_DVT_comp_within_30d[temp.ref] <- 1
          if(!any(temp == 1) & any(temp == 99)) ccc19x$der_DVT_comp_within_30d[temp.ref] <- 99
          if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_DVT_comp_within_30d[temp.ref] <- 0
        }
      }
      
      ccc19x$der_DVT_comp_within_30d <- as.factor(ccc19x$der_DVT_comp_within_30d)
      summary(ccc19x$der_DVT_comp_within_30d[ccc19x$redcap_repeat_instrument == ''])
      
      #Comp04. Thrombosis NOS complications
      ccc19x$der_thrombosis_NOS_comp <- NA
      temp.ref <- which(grepl(colnames(ccc19x), pattern = '414086009') & grepl(colnames(ccc19x), pattern = 'complications'))
      
      #Present
      for(i in temp.ref)
        ccc19x$der_thrombosis_NOS_comp[which(ccc19x[,i] == 1)] <- 1
      
      #Not present, something else checked besides unknown
      ccc19x$der_thrombosis_NOS_comp[which(master_comp == 1 & is.na(ccc19x$der_thrombosis_NOS_comp))] <- 0
      
      #Unknown
      
      #Baseline
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk', 'c19_complications_other___unk'))
      for(i in which(is.na(ccc19x$der_thrombosis_NOS_comp) & ccc19x$redcap_repeat_instrument == ''))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_thrombosis_NOS_comp[i] <- 99
      
      #Followup
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk', 'c19_complications_other_fu___unk'))
      for(i in which(is.na(ccc19x$der_thrombosis_NOS_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_thrombosis_NOS_comp[i] <- 99
      
      #Merge baseline and followup if discrepancy
      for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
      {
        temp.ref <- which(ccc19x$record_id == i)
        temp <- ccc19x$der_thrombosis_NOS_comp[temp.ref]
        temp <- as.numeric(unique(temp[!is.na(temp)]))
        if(length(temp) > 0)
        {
          if(any(temp == 1)) ccc19x$der_thrombosis_NOS_comp[temp.ref] <- 1
          if(!any(temp == 1) & any(temp == 99)) ccc19x$der_thrombosis_NOS_comp[temp.ref] <- 99
          if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_thrombosis_NOS_comp[temp.ref] <- 0
        }
      }
      
      ccc19x$der_thrombosis_NOS_comp <- as.factor(ccc19x$der_thrombosis_NOS_comp)
      
      temp <- summary(ccc19x$der_thrombosis_NOS_comp[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_thrombosis_NOS_comp',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      ccc19x$der_thrombosis_NOS_comp_within_30d <- NA
      temp.ref <- which(grepl(colnames(ccc19x), pattern = '414086009') & grepl(colnames(ccc19x), pattern = 'complications'))
      temp.ref2 <- which(ccc19x$redcap_repeat_instrument == ''|
                           ccc19x$fu_weeks %in% c(30)|
                           ccc19x$timing_of_report_weeks <= 4)
      #Present
      for(i in temp.ref)
        ccc19x$der_thrombosis_NOS_comp_within_30d[temp.ref2][which(ccc19x[temp.ref2,i] == 1)] <- 1
      
      #Not present, something else checked besides unknown
      temp.ref <- which(grepl(colnames(ccc19x), pattern = 'complications_other|complications_card') & !grepl(colnames(ccc19x), pattern = '414086009|unk'))
      for(i in temp.ref2)
        if(any(ccc19x[i,temp.ref] == 1) & !is.na(any(ccc19x[i,temp.ref] == 1)) & is.na(ccc19x$der_thrombosis_NOS_comp_within_30d[i])) ccc19x$der_thrombosis_NOS_comp_within_30d[i] <- 0
      
      #Unknown
      
      #Baseline
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk', 'c19_complications_other___unk'))
      for(i in which(is.na(ccc19x$der_thrombosis_NOS_comp_within_30d) & ccc19x$redcap_repeat_instrument == ''))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_thrombosis_NOS_comp_within_30d[i] <- 99
      
      #Followup
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk', 'c19_complications_other_fu___unk'))
      for(i in which(is.na(ccc19x$der_thrombosis_NOS_comp_within_30d) & ccc19x$redcap_repeat_instrument == 'followup' &
                     (ccc19x$fu_weeks %in% c(30,90)|ccc19x$timing_of_report_weeks <= 13)))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_thrombosis_NOS_comp_within_30d[i] <- 99
      
      #Merge baseline and followup if discrepancy
      for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
      {
        temp.ref <- which(ccc19x$record_id == i)
        temp <- ccc19x$der_thrombosis_NOS_comp_within_30d[temp.ref]
        temp <- as.numeric(unique(temp[!is.na(temp)]))
        if(length(temp) > 0)
        {
          if(any(temp == 1)) ccc19x$der_thrombosis_NOS_comp_within_30d[temp.ref] <- 1
          if(!any(temp == 1) & any(temp == 99)) ccc19x$der_thrombosis_NOS_comp_within_30d[temp.ref] <- 99
          if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_thrombosis_NOS_comp_within_30d[temp.ref] <- 0
        }
      }
      
      ccc19x$der_thrombosis_NOS_comp_within_30d <- as.factor(ccc19x$der_thrombosis_NOS_comp_within_30d)
      summary(ccc19x$der_thrombosis_NOS_comp_within_30d[ccc19x$redcap_repeat_instrument == ''])
      
      #Comp04b. thrombosis_NOS complications within 90 days (3 months)
      ccc19x$der_thrombosis_NOS_comp_within_3mo <- NA
      temp.ref <- which(grepl(colnames(ccc19x), pattern = '414086009') & grepl(colnames(ccc19x), pattern = 'complications'))
      temp.ref2 <- which(ccc19x$redcap_repeat_instrument == ''|
                           ccc19x$fu_weeks %in% c(30,90)|
                           ccc19x$timing_of_report_weeks <= 13)
      #Present
      for(i in temp.ref)
        ccc19x$der_thrombosis_NOS_comp_within_3mo[temp.ref2][which(ccc19x[temp.ref2,i] == 1)] <- 1
      
      #Not present, something else checked besides unknown
      temp.ref <- which(grepl(colnames(ccc19x), pattern = 'complications_other|complications_card') & !grepl(colnames(ccc19x), pattern = '414086009|unk'))
      for(i in temp.ref2)
        if(any(ccc19x[i,temp.ref] == 1) & !is.na(any(ccc19x[i,temp.ref] == 1)) & is.na(ccc19x$der_thrombosis_NOS_comp_within_3mo[i])) ccc19x$der_thrombosis_NOS_comp_within_3mo[i] <- 0
      
      #Unknown
      
      #Baseline
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk', 'c19_complications_other___unk'))
      for(i in which(is.na(ccc19x$der_thrombosis_NOS_comp_within_3mo) & ccc19x$redcap_repeat_instrument == ''))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_thrombosis_NOS_comp_within_3mo[i] <- 99
      
      #Followup
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk', 'c19_complications_other_fu___unk'))
      for(i in which(is.na(ccc19x$der_thrombosis_NOS_comp_within_3mo) & ccc19x$redcap_repeat_instrument == 'followup' &
                     (ccc19x$fu_weeks %in% c(30,90)|ccc19x$timing_of_report_weeks <= 13)))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_thrombosis_NOS_comp_within_3mo[i] <- 99
      
      #Merge baseline and followup if discrepancy
      for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
      {
        temp.ref <- which(ccc19x$record_id == i)
        temp <- ccc19x$der_thrombosis_NOS_comp_within_3mo[temp.ref]
        temp <- as.numeric(unique(temp[!is.na(temp)]))
        if(length(temp) > 0)
        {
          if(any(temp == 1)) ccc19x$der_thrombosis_NOS_comp_within_3mo[temp.ref] <- 1
          if(!any(temp == 1) & any(temp == 99)) ccc19x$der_thrombosis_NOS_comp_within_3mo[temp.ref] <- 99
          if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_thrombosis_NOS_comp_within_3mo[temp.ref] <- 0
        }
      }
      
      ccc19x$der_thrombosis_NOS_comp_within_3mo <- as.factor(ccc19x$der_thrombosis_NOS_comp_within_3mo)
      summary(ccc19x$der_thrombosis_NOS_comp_within_3mo[ccc19x$redcap_repeat_instrument == ''])
      
      #Comp05. Combined VTE indicator (excluding SVT)
      ccc19x$der_VTE_comp <- NA
      
      #Any complication
      temp.ref <- which(colnames(ccc19x) %in% c('der_PE_comp', 'der_DVT_comp', 'der_thrombosis_NOS_comp'))
      for(i in temp.ref)
        ccc19x$der_VTE_comp[which(ccc19x[,i] == 1)] <- 1
      
      for(i in which(is.na(ccc19x$der_VTE_comp)))
      {
        temp <- ccc19x[i,temp.ref]
        temp <- temp[!is.na(temp)]
        temp <- unique(temp)
        if(length(temp) >= 1)
          if(length(temp) == 1)
            if(temp == 0) ccc19x$der_VTE_comp[i] <- 0 else ccc19x$der_VTE_comp[i] <- 99
      }
      
      ccc19x$der_VTE_comp <- as.factor(ccc19x$der_VTE_comp)
      
      temp <- summary(ccc19x$der_VTE_comp[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_VTE_comp',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      #Comp05a. Combined VTE indicator (excluding SVT and thrombosis NOS)
      ccc19x$der_VTE_comp_v2 <- NA
      
      #Any complication
      temp.ref <- which(colnames(ccc19x) %in% c('der_PE_comp', 'der_DVT_comp'))
      for(i in temp.ref)
        ccc19x$der_VTE_comp_v2[which(ccc19x[,i] == 1)] <- 1
      
      for(i in which(is.na(ccc19x$der_VTE_comp_v2)))
      {
        temp <- ccc19x[i,temp.ref]
        temp <- temp[!is.na(temp)]
        temp <- unique(temp)
        if(length(temp) >= 1)
          if(length(temp) == 1)
            if(temp == 0) ccc19x$der_VTE_comp_v2[i] <- 0 else ccc19x$der_VTE_comp_v2[i] <- 99
      }
      
      ccc19x$der_VTE_comp_v2 <- as.factor(ccc19x$der_VTE_comp_v2)
      
      temp <- summary(ccc19x$der_VTE_comp_v2[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_VTE_comp_v2',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      #Comp05b. Combined VTE within 3 months (90 days)
      ccc19x$der_VTE_comp_within_3mo <- NA
      ccc19x$der_VTE_comp_within_3mo[which(ccc19x$der_PE_comp_within_3mo == 1|
                                             ccc19x$der_DVT_comp_within_3mo == 1|
                                             ccc19x$der_thrombosis_NOS_comp_within_3mo == 1)] <- 1
      ccc19x$der_VTE_comp_within_3mo[which(ccc19x$der_PE_comp_within_3mo == 0 &
                                             ccc19x$der_DVT_comp_within_3mo == 0 &
                                             ccc19x$der_thrombosis_NOS_comp_within_3mo == 0)] <- 0
      ccc19x$der_VTE_comp_within_3mo[which((ccc19x$der_PE_comp_within_3mo == 99|
                                              ccc19x$der_DVT_comp_within_3mo == 99|
                                              ccc19x$der_thrombosis_NOS_comp_within_3mo == 99) &
                                             is.na(ccc19x$der_VTE_comp_within_3mo))] <- 99
      ccc19x$der_VTE_comp_within_3mo <- as.factor(ccc19x$der_VTE_comp_within_3mo)
      
      temp <- summary(ccc19x$der_VTE_comp_within_3mo[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_VTE_comp_within_3mo',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      #Comp05c. Combined VTE within 30 days
      ccc19x$der_VTE_comp_within_30d <- NA
      ccc19x$der_VTE_comp_within_30d[which(ccc19x$der_PE_comp_within_30d == 1|
                                             ccc19x$der_DVT_comp_within_30d == 1|
                                             ccc19x$der_thrombosis_NOS_comp_within_30d == 1)] <- 1
      ccc19x$der_VTE_comp_within_30d[which(ccc19x$der_PE_comp_within_30d == 0 &
                                             ccc19x$der_DVT_comp_within_30d == 0 &
                                             ccc19x$der_thrombosis_NOS_comp_within_30d == 0)] <- 0
      ccc19x$der_VTE_comp_within_30d[which((ccc19x$der_PE_comp_within_30d == 99|
                                              ccc19x$der_DVT_comp_within_30d == 99|
                                              ccc19x$der_thrombosis_NOS_comp_within_30d == 99) &
                                             is.na(ccc19x$der_VTE_comp_within_30d))] <- 99
      ccc19x$der_VTE_comp_within_30d <- as.factor(ccc19x$der_VTE_comp_within_30d)
      
      temp <- summary(ccc19x$der_VTE_comp_within_30d[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_VTE_comp_within_30d',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      #Comp06. ATE complications (MI, CVA)
      ccc19x$der_ATE_comp <- NA
      temp.ref <- which(grepl(colnames(ccc19x), pattern = '22298006|230690007') & grepl(colnames(ccc19x), pattern = 'complications'))
      
      #Present
      for(i in temp.ref)
        ccc19x$der_ATE_comp[which(ccc19x[,i] == 1)] <- 1
      
      #Not present, something else checked besides unknown
      ccc19x$der_ATE_comp[which(master_comp == 1 & is.na(ccc19x$der_ATE_comp))] <- 0
      
      #Unknown
      
      #Baseline
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk'))
      for(i in which(is.na(ccc19x$der_ATE_comp) & ccc19x$redcap_repeat_instrument == ''))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_ATE_comp[i] <- 99
      
      #Followup
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk'))
      for(i in which(is.na(ccc19x$der_ATE_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_ATE_comp[i] <- 99
      
      #Merge baseline and followup if discrepancy
      for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
      {
        temp.ref <- which(ccc19x$record_id == i)
        temp <- ccc19x$der_ATE_comp[temp.ref]
        temp <- as.numeric(unique(temp[!is.na(temp)]))
        if(length(temp) > 0)
        {
          if(any(temp == 1)) ccc19x$der_ATE_comp[temp.ref] <- 1
          if(!any(temp == 1) & any(temp == 99)) ccc19x$der_ATE_comp[temp.ref] <- 99
          if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_ATE_comp[temp.ref] <- 0
        }
      }
      
      ccc19x$der_ATE_comp <- as.factor(ccc19x$der_ATE_comp)
      
      temp <- summary(ccc19x$der_ATE_comp[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_ATE_comp',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      #Comp06a. ATE complications within 90 days (3 months)
      ccc19x$der_ATE_comp_within_3mo <- NA
      temp.ref <- which(grepl(colnames(ccc19x), pattern = '22298006|230690007') & grepl(colnames(ccc19x), pattern = 'complications'))
      temp.ref2 <- which(ccc19x$redcap_repeat_instrument == ''|
                           ccc19x$fu_weeks %in% c(30,90)|
                           ccc19x$timing_of_report_weeks <= 13)
      #Present
      for(i in temp.ref)
        ccc19x$der_ATE_comp_within_3mo[temp.ref2][which(ccc19x[temp.ref2,i] == 1)] <- 1
      
      #Not present, something else checked besides unknown
      temp.ref <- which(grepl(colnames(ccc19x), pattern = 'complications_card') & !grepl(colnames(ccc19x), pattern = '22298006|230690007|unk'))
      for(i in temp.ref2)
        if(any(ccc19x[i,temp.ref] == 1) & !is.na(any(ccc19x[i,temp.ref] == 1)) & is.na(ccc19x$der_ATE_comp_within_3mo[i])) ccc19x$der_ATE_comp_within_3mo[i] <- 0
      
      #Unknown
      
      #Baseline
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk'))
      for(i in which(is.na(ccc19x$der_ATE_comp_within_3mo) & ccc19x$redcap_repeat_instrument == ''))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_ATE_comp_within_3mo[i] <- 99
      
      #Followup
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk'))
      for(i in which(is.na(ccc19x$der_ATE_comp_within_3mo) & ccc19x$redcap_repeat_instrument == 'followup' &
                     (ccc19x$fu_weeks %in% c(30,90)|ccc19x$timing_of_report_weeks <= 13)))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_ATE_comp_within_3mo[i] <- 99
      
      #Merge baseline and followup if discrepancy
      for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
      {
        temp.ref <- which(ccc19x$record_id == i)
        temp <- ccc19x$der_ATE_comp_within_3mo[temp.ref]
        temp <- as.numeric(unique(temp[!is.na(temp)]))
        if(length(temp) > 0)
        {
          if(any(temp == 1)) ccc19x$der_ATE_comp_within_3mo[temp.ref] <- 1
          if(!any(temp == 1) & any(temp == 99)) ccc19x$der_ATE_comp_within_3mo[temp.ref] <- 99
          if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_ATE_comp_within_3mo[temp.ref] <- 0
        }
      }
      
      ccc19x$der_ATE_comp_within_3mo <- as.factor(ccc19x$der_ATE_comp_within_3mo)
      
      temp <- summary(ccc19x$der_ATE_comp_within_3mo[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_ATE_comp_within_3mo',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      #Comp06b. ATE complications within 30 days
      ccc19x$der_ATE_comp_within_30d <- NA
      temp.ref <- which(grepl(colnames(ccc19x), pattern = '22298006|230690007') & grepl(colnames(ccc19x), pattern = 'complications'))
      temp.ref2 <- which(ccc19x$redcap_repeat_instrument == ''|
                           ccc19x$fu_weeks %in% c(30)|
                           ccc19x$timing_of_report_weeks <= 4)
      #Present
      for(i in temp.ref)
        ccc19x$der_ATE_comp_within_30d[temp.ref2][which(ccc19x[temp.ref2,i] == 1)] <- 1
      
      #Not present, something else checked besides unknown
      temp.ref <- which(grepl(colnames(ccc19x), pattern = 'complications_card') & !grepl(colnames(ccc19x), pattern = '22298006|230690007|unk'))
      for(i in temp.ref2)
        if(any(ccc19x[i,temp.ref] == 1) & !is.na(any(ccc19x[i,temp.ref] == 1)) & is.na(ccc19x$der_ATE_comp_within_30d[i])) ccc19x$der_ATE_comp_within_30d[i] <- 0
      
      #Unknown
      
      #Baseline
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk'))
      for(i in which(is.na(ccc19x$der_ATE_comp_within_30d) & ccc19x$redcap_repeat_instrument == ''))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_ATE_comp_within_30d[i] <- 99
      
      #Followup
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk'))
      for(i in which(is.na(ccc19x$der_ATE_comp_within_30d) & ccc19x$redcap_repeat_instrument == 'followup' &
                     (ccc19x$fu_weeks %in% c(30,90)|ccc19x$timing_of_report_weeks <= 13)))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_ATE_comp_within_30d[i] <- 99
      
      #Merge baseline and followup if discrepancy
      for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
      {
        temp.ref <- which(ccc19x$record_id == i)
        temp <- ccc19x$der_ATE_comp_within_30d[temp.ref]
        temp <- as.numeric(unique(temp[!is.na(temp)]))
        if(length(temp) > 0)
        {
          if(any(temp == 1)) ccc19x$der_ATE_comp_within_30d[temp.ref] <- 1
          if(!any(temp == 1) & any(temp == 99)) ccc19x$der_ATE_comp_within_30d[temp.ref] <- 99
          if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_ATE_comp_within_30d[temp.ref] <- 0
        }
      }
      
      ccc19x$der_ATE_comp_within_30d <- as.factor(ccc19x$der_ATE_comp_within_30d)
      
      temp <- summary(ccc19x$der_ATE_comp_within_30d[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_ATE_comp_within_30d',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      #Comp07. Stroke complication
      ccc19x$der_stroke_comp <- NA
      temp.ref <- which(grepl(colnames(ccc19x), pattern = '230690007') & grepl(colnames(ccc19x), pattern = 'complications'))
      
      #Present
      for(i in temp.ref)
        ccc19x$der_stroke_comp[which(ccc19x[,i] == 1)] <- 1
      
      #Not present, something else checked besides unknown
      ccc19x$der_stroke_comp[which(master_comp == 1 & is.na(ccc19x$der_stroke_comp))] <- 0
      
      #Unknown
      
      #Baseline
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk'))
      for(i in which(is.na(ccc19x$der_stroke_comp) & ccc19x$redcap_repeat_instrument == ''))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_stroke_comp[i] <- 99
      
      #Followup
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk'))
      for(i in which(is.na(ccc19x$der_stroke_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_stroke_comp[i] <- 99
      
      #Merge baseline and followup if discrepancy
      for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
      {
        temp.ref <- which(ccc19x$record_id == i)
        temp <- ccc19x$der_stroke_comp[temp.ref]
        temp <- as.numeric(unique(temp[!is.na(temp)]))
        if(length(temp) > 0)
        {
          if(any(temp == 1)) ccc19x$der_stroke_comp[temp.ref] <- 1
          if(!any(temp == 1) & any(temp == 99)) ccc19x$der_stroke_comp[temp.ref] <- 99
          if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_stroke_comp[temp.ref] <- 0
        }
      }
      
      ccc19x$der_stroke_comp <- as.factor(ccc19x$der_stroke_comp)
      
      temp <- summary(ccc19x$der_stroke_comp[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_stroke_comp',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      #Comp07a. stroke complications within 90 days (3 months)
      ccc19x$der_stroke_comp_within_3mo <- NA
      temp.ref <- which(grepl(colnames(ccc19x), pattern = '230690007') & grepl(colnames(ccc19x), pattern = 'complications'))
      temp.ref2 <- which(ccc19x$redcap_repeat_instrument == ''|
                           ccc19x$fu_weeks %in% c(30,90)|
                           ccc19x$timing_of_report_weeks <= 13)
      #Present
      for(i in temp.ref)
        ccc19x$der_stroke_comp_within_3mo[temp.ref2][which(ccc19x[temp.ref2,i] == 1)] <- 1
      
      #Not present, something else checked besides unknown
      temp.ref <- which(grepl(colnames(ccc19x), pattern = 'complications_card') & !grepl(colnames(ccc19x), pattern = '230690007|unk'))
      for(i in temp.ref2)
        if(any(ccc19x[i,temp.ref] == 1) & !is.na(any(ccc19x[i,temp.ref] == 1)) & is.na(ccc19x$der_stroke_comp_within_3mo[i])) ccc19x$der_stroke_comp_within_3mo[i] <- 0
      
      #Unknown
      
      #Baseline
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk'))
      for(i in which(is.na(ccc19x$der_stroke_comp_within_3mo) & ccc19x$redcap_repeat_instrument == ''))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_stroke_comp_within_3mo[i] <- 99
      
      #Followup
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk'))
      for(i in which(is.na(ccc19x$der_stroke_comp_within_3mo) & ccc19x$redcap_repeat_instrument == 'followup' &
                     (ccc19x$fu_weeks %in% c(30,90)|ccc19x$timing_of_report_weeks <= 13)))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_stroke_comp_within_3mo[i] <- 99
      
      #Merge baseline and followup if discrepancy
      for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
      {
        temp.ref <- which(ccc19x$record_id == i)
        temp <- ccc19x$der_stroke_comp_within_3mo[temp.ref]
        temp <- as.numeric(unique(temp[!is.na(temp)]))
        if(length(temp) > 0)
        {
          if(any(temp == 1)) ccc19x$der_stroke_comp_within_3mo[temp.ref] <- 1
          if(!any(temp == 1) & any(temp == 99)) ccc19x$der_stroke_comp_within_3mo[temp.ref] <- 99
          if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_stroke_comp_within_3mo[temp.ref] <- 0
        }
      }
      
      ccc19x$der_stroke_comp_within_3mo <- as.factor(ccc19x$der_stroke_comp_within_3mo)
      
      temp <- summary(ccc19x$der_stroke_comp_within_3mo[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_stroke_comp_within_3mo',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
    }
    
    #Comp08. Arrhythmia complications (partial derived)
    ccc19x$der_arry <- 0
    ccc19x$der_arry[which(ccc19x$c19_complications_card___71908006 == 1|
                            ccc19x$c19_complications_card___698247007 == 1|
                            ccc19x$c19_complications_card_fu___71908006 == 1|
                            ccc19x$c19_complications_card_fu___698247007 == 1)] <- 1
    
    ccc19x$der_arry <- factor(ccc19x$der_arry)
    summary(ccc19x$der_arry[ccc19x$redcap_repeat_instrument == ''])
    
    #Comp09. CV events 
    ccc19x$der_CV_event <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '22298006|414545008|49436004|71908006|698247007|85898001|42343007') & 
                        grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_CV_event[which(ccc19x[,i] == 1)] <- 1
    
    ccc19x$der_CV_event[which(ccc19x$sepsis_pressors == 1|ccc19x$hotn_pressors_fu == 1)] <- 1
    
    #Not present, something else checked besides unknown
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'complications_card') & 
                        !grepl(colnames(ccc19x), pattern = '22298006|414545008|49436004|71908006|698247007|85898001|42343007|unk'))
    for(i in 1:nrow(ccc19x))
      if(any(ccc19x[i,temp.ref] == 1) & !is.na(any(ccc19x[i,temp.ref] == 1)) & is.na(ccc19x$der_CV_event[i])) ccc19x$der_CV_event[i] <- 0
    
    ccc19x$der_CV_event[which(ccc19x$sepsis_pressors == 0 & is.na(ccc19x$der_CV_event))] <- 0
    ccc19x$der_CV_event[which(ccc19x$hotn_pressors_fu == 0 & is.na(ccc19x$der_CV_event))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk'))
    for(i in which(is.na(ccc19x$der_CV_event) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_CV_event[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk'))
    for(i in which(is.na(ccc19x$der_CV_event) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_CV_event[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_CV_event[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_CV_event[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_CV_event[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_CV_event[temp.ref] <- 0
      }
    }
    
    ccc19x$der_CV_event <- as.factor(ccc19x$der_CV_event)
    summary(ccc19x$der_CV_event[ccc19x$redcap_repeat_instrument == ''])
    
    #Comp10. Worst severity of COVID-19 complications 
    ccc19x$der_worst <- NA
    
    #Moderate
    temp <- ccc19x$record_id[which((ccc19x$complications_severity___2 == 1 & ccc19x$complications_severity___3 == 0)|
                                     (ccc19x$worst_complications_severity___2 == 1 & ccc19x$worst_complications_severity___3 == 0)|
                                     ccc19x$worst_complications_severity_fu == 2|
                                     (ccc19x$complications_severity_fu___2 == 1 & ccc19x$complications_severity_fu___3 == 0))] 
    ccc19x$der_worst[which(ccc19x$record_id %in% temp & is.na(ccc19x$der_worst))] <- 'Moderate'
    
    #Mild
    temp <- ccc19x$record_id[which((ccc19x$complications_severity___1 == 1 & ccc19x$complications_severity___2 == 0 & ccc19x$complications_severity___3 == 0)|
                                     (ccc19x$worst_complications_severity___1 == 1 & ccc19x$worst_complications_severity___2 == 0 & ccc19x$worst_complications_severity___3 == 0)|
                                     ccc19x$worst_complications_severity_fu == 1|
                                     (ccc19x$complications_severity_fu___1 == 1 & ccc19x$complications_severity_fu___2 == 0 & ccc19x$complications_severity_fu___3 == 0))] 
    ccc19x$der_worst[which(ccc19x$record_id %in% temp & is.na(ccc19x$der_worst))] <- 'Mild'
    
    #Other
    temp <- ccc19x$record_id[which(ccc19x$complications_severity___oth ==1|
                                     ccc19x$worst_complications_severity___oth ==1|
                                     ccc19x$worst_complications_severity_fu == 'OTH'|
                                     ccc19x$complications_severity_fu___oth ==1)]
    ccc19x$der_worst[which(ccc19x$record_id %in% temp & is.na(ccc19x$der_worst))] <- 'Other'
    
    #None
    temp <- unique(ccc19x$record_id[which((ccc19x$complications_severity___0 == 1 & ccc19x$complications_severity___1 == 0 & ccc19x$complications_severity___2 == 0 & ccc19x$complications_severity___3 == 0)|
                                     (ccc19x$worst_complications_severity___0 == 1 & ccc19x$worst_complications_severity___1 == 0 & ccc19x$worst_complications_severity___2 == 0 & ccc19x$worst_complications_severity___3 == 0)|
                                     ccc19x$worst_complications_severity_fu == 0|
                                     (ccc19x$complications_severity_fu___0 == 1 & ccc19x$complications_severity_fu___1 == 0 & ccc19x$complications_severity_fu___2 == 0 & ccc19x$complications_severity_fu___3 == 0))]) 
    ccc19x$der_worst[which(ccc19x$record_id %in% temp & is.na(ccc19x$der_worst))] <- 'None'
    
    #Unknown
    temp <- ccc19x$record_id[which(ccc19x$severity_of_covid_19_v2 == 99|
                                     (ccc19x$complications_severity___99 == 1 & ccc19x$complications_severity___0 == 0 & ccc19x$complications_severity___1 == 0 & ccc19x$complications_severity___2 == 0 & ccc19x$complications_severity___3 == 0) |
                                     (ccc19x$worst_complications_severity___99 == 1 & ccc19x$worst_complications_severity___0 == 0 & ccc19x$worst_complications_severity___1 == 0 & ccc19x$worst_complications_severity___2 == 0 & ccc19x$worst_complications_severity___3 == 0) |
                                     ccc19x$worst_complications_severity_fu == 99|
                                     (ccc19x$complications_severity_fu___99 == 1 & ccc19x$complications_severity_fu___0 == 0 & ccc19x$complications_severity_fu___1 == 0 & ccc19x$complications_severity_fu___2 == 0 & ccc19x$complications_severity_fu___3 == 0))]
    ccc19x$der_worst[which(ccc19x$record_id %in% temp & is.na(ccc19x$der_worst))] <- 'Unknown'
    
    #Serious
    temp <- ccc19x$record_id[which(ccc19x$complications_severity___3 == 1|
                                     ccc19x$worst_complications_severity___3 == 1|
                                     ccc19x$worst_complications_severity_fu == 3|
                                     ccc19x$complications_severity_fu___3 == 1)]
    ccc19x$der_worst[which(ccc19x$record_id %in% temp)] <- 'Serious'
                                   
    ccc19x$der_worst <- factor(ccc19x$der_worst)
    summary(ccc19x$der_worst[ccc19x$redcap_repeat_instrument == ''])
    
    #Complications
    
    #### Systemic ####
    
    #Comp11 Multisystem Organ Failure: 57653000 (der_MOF_comp)
    
    ccc19x$der_MOF_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '57653000') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_MOF_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_MOF_comp[which(master_comp == 1 & is.na(ccc19x$der_MOF_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_systemic___unk'))
    for(i in which(is.na(ccc19x$der_MOF_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_MOF_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_systemic_fu___unk'))
    for(i in which(is.na(ccc19x$der_MOF_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_MOF_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_MOF_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_MOF_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_MOF_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_MOF_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_MOF_comp <- as.factor(ccc19x$der_MOF_comp)
    
    temp <- summary(ccc19x$der_MOF_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_MOF_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp12 Sepsis: 91302008 (der_sepsis_comp)
    
    ccc19x$der_sepsis_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '91302008') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_sepsis_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_sepsis_comp[which(master_comp == 1 & is.na(ccc19x$der_sepsis_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_systemic___unk'))
    for(i in which(is.na(ccc19x$der_sepsis_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_sepsis_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_systemic_fu___unk'))
    for(i in which(is.na(ccc19x$der_sepsis_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_sepsis_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_sepsis_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_sepsis_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_sepsis_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_sepsis_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_sepsis_comp <- as.factor(ccc19x$der_sepsis_comp)
    
    temp <- summary(ccc19x$der_sepsis_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_sepsis_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp12a Sepsis including hypotension that required pressors
    ccc19x$der_sepsis_comp_v2 <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '91302008') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_sepsis_comp_v2[which(ccc19x[,i] == 1)] <- 1
    
    ccc19x$der_sepsis_comp_v2[which((ccc19x$c19_complications_card___45007003 == 1 & ccc19x$sepsis_pressors == 1)|
                                      (ccc19x$c19_complications_card_fu___45007003 == 1 & ccc19x$hotn_pressors_fu == 1))] <- 1
    
    #Not present, something else checked besides unknown
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'complications_systemic') & !grepl(colnames(ccc19x), pattern = '91302008|unk'))
    for(i in 1:nrow(ccc19x))
      if(any(ccc19x[i,temp.ref] == 1) & !is.na(any(ccc19x[i,temp.ref] == 1)) & is.na(ccc19x$der_sepsis_comp_v2[i])) ccc19x$der_sepsis_comp_v2[i] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_systemic___unk'))
    for(i in which(is.na(ccc19x$der_sepsis_comp_v2) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1) & any(is.na(ccc19x$der_sepsis_comp_v2))) ccc19x$der_sepsis_comp_v2[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_systemic_fu___unk'))
    for(i in which(is.na(ccc19x$der_sepsis_comp_v2) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1) & any(is.na(ccc19x$der_sepsis_comp_v2))) ccc19x$der_sepsis_comp_v2[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_sepsis_comp_v2[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_sepsis_comp_v2[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_sepsis_comp_v2[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_sepsis_comp_v2[temp.ref] <- 0
      }
    }
    
    ccc19x$der_sepsis_comp_v2 <- as.factor(ccc19x$der_sepsis_comp_v2)
    summary(ccc19x$der_sepsis_comp_v2[ccc19x$redcap_repeat_instrument == ''])
    
    #Comp12b Pressors (der_pressors)
    ccc19x$der_pressors <- NA
    
    #Present at baseline
    ccc19x$der_pressors[which(ccc19x$sepsis_pressors == 1)] <- 1
    
    #Absent at baseline
    ccc19x$der_pressors[which(ccc19x$sepsis_pressors == 0)] <- 0
    
    #Unknown at baseline
    ccc19x$der_pressors[which(ccc19x$sepsis_pressors == 99)] <- 99
    
    #Present at follow-up
    ccc19x$der_pressors[which(ccc19x$hotn_pressors_fu == 1)] <- 1
    
    #Absent at follow-up
    ccc19x$der_pressors[which(ccc19x$hotn_pressors_fu == 0)] <- 0
    
    #Unknown at follow-up
    ccc19x$der_pressors[which(ccc19x$hotn_pressors_fu == 99)] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_pressors[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_pressors[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_pressors[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_pressors[temp.ref] <- 0
      }
    }
    
    ccc19x$der_pressors <- as.factor(ccc19x$der_pressors)
    summary(ccc19x$der_pressors[ccc19x$redcap_repeat_instrument == ''])
    
    #O3b. composite of ICU + mechanical ventilation + vasopressors/inotropes
    
    #Declare as missing
    ccc19x$der_ICU_mv_pressors <- NA
    
    #Yes
    ccc19x$der_ICU_mv_pressors[which(ccc19x$der_ICU == 1 & ccc19x$der_mv == 1 & ccc19x$der_pressors == 1)] <- 1
    
    #No
    ccc19x$der_ICU_mv_pressors[which(ccc19x$der_ICU == 0 & ccc19x$der_mv == 0 & ccc19x$der_pressors == 0)] <- 0
    
    #Unknown (any component)
    ccc19x$der_ICU_mv_pressors[which(ccc19x$der_ICU == 99| ccc19x$der_mv == 99| ccc19x$der_pressors == 99)] <- 99
    
    ccc19x$der_ICU_mv_pressors <- as.factor(ccc19x$der_ICU_mv_pressors)
    
    temp <- summary(ccc19x$der_ICU_mv_pressors[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ICU_mv_pressors',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp13 Bleeding: 50960005 (der_bleeding_comp)
    
    ccc19x$der_bleeding_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '50960005') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_bleeding_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_bleeding_comp[which(master_comp == 1 & is.na(ccc19x$der_bleeding_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_systemic___unk'))
    for(i in which(is.na(ccc19x$der_bleeding_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_bleeding_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_systemic_fu___unk'))
    for(i in which(is.na(ccc19x$der_bleeding_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_bleeding_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_bleeding_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_bleeding_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_bleeding_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_bleeding_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_bleeding_comp <- as.factor(ccc19x$der_bleeding_comp)
    
    temp <- summary(ccc19x$der_bleeding_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_bleeding_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp14 DIC: 67406007 (der_DIC_comp)
    
    ccc19x$der_DIC_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '67406007') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_DIC_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_DIC_comp[which(master_comp == 1 & is.na(ccc19x$der_DIC_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_systemic___unk'))
    for(i in which(is.na(ccc19x$der_DIC_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_DIC_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_systemic_fu___unk'))
    for(i in which(is.na(ccc19x$der_DIC_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_DIC_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_DIC_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_DIC_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_DIC_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_DIC_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_DIC_comp <- as.factor(ccc19x$der_DIC_comp)
    
    temp <- summary(ccc19x$der_DIC_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_DIC_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #### Pulmonary ####
    
    #Comp15 Respiratory Failure: 409622000 (der_resp_failure_comp)
    
    ccc19x$der_resp_failure_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '409622000') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_resp_failure_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_resp_failure_comp[which(master_comp == 1 & is.na(ccc19x$der_resp_failure_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_pulm___unk'))
    for(i in which(is.na(ccc19x$der_resp_failure_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_resp_failure_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_pulm_fu___unk'))
    for(i in which(is.na(ccc19x$der_resp_failure_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_resp_failure_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_resp_failure_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_resp_failure_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_resp_failure_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_resp_failure_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_resp_failure_comp <- as.factor(ccc19x$der_resp_failure_comp)
    
    temp <- summary(ccc19x$der_resp_failure_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_resp_failure_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp16 Pneumonitis: 205237003 (der_pneumonitis_comp)
    
    ccc19x$der_pneumonitis_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '205237003') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_pneumonitis_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_pneumonitis_comp[which(master_comp == 1 & is.na(ccc19x$der_pneumonitis_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_pulm___unk'))
    for(i in which(is.na(ccc19x$der_pneumonitis_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_pneumonitis_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_pulm_fu___unk'))
    for(i in which(is.na(ccc19x$der_pneumonitis_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_pneumonitis_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_pneumonitis_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_pneumonitis_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_pneumonitis_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_pneumonitis_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_pneumonitis_comp <- as.factor(ccc19x$der_pneumonitis_comp)
    
    temp <- summary(ccc19x$der_pneumonitis_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_pneumonitis_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp17 Pneumonia: 233604007 (der_pneumonia_comp)
    
    ccc19x$der_pneumonia_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '233604007') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_pneumonia_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_pneumonia_comp[which(master_comp == 1 & is.na(ccc19x$der_pneumonia_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_pulm___unk'))
    for(i in which(is.na(ccc19x$der_pneumonia_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_pneumonia_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_pulm_fu___unk'))
    for(i in which(is.na(ccc19x$der_pneumonia_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_pneumonia_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_pneumonia_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_pneumonia_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_pneumonia_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_pneumonia_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_pneumonia_comp <- as.factor(ccc19x$der_pneumonia_comp)
    
    temp <- summary(ccc19x$der_pneumonia_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_pneumonia_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp18 Pneumonia/Pneumonitis combined
    ccc19x$der_pneumo_comp <- NA
    ccc19x$der_pneumo_comp[which(ccc19x$der_pneumonia_comp == 1|ccc19x$der_pneumonitis_comp == 1)] <- 1
    ccc19x$der_pneumo_comp[which(ccc19x$der_pneumonia_comp == 0 & ccc19x$der_pneumonitis_comp == 0)] <- 0
    ccc19x$der_pneumo_comp[which((ccc19x$der_pneumonia_comp == 99|ccc19x$der_pneumonitis_comp == 99) &
                                   is.na(ccc19x$der_pneumo_comp))] <- 99
    ccc19x$der_pneumo_comp <- as.factor(ccc19x$der_pneumo_comp)
    summary(ccc19x$der_pneumo_comp[ccc19x$redcap_repeat_instrument == ''])
    
    #Comp19 ARDS: 67782005 (der_ARDS_comp)
    
    ccc19x$der_ARDS_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '67782005') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_ARDS_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_ARDS_comp[which(master_comp == 1 & is.na(ccc19x$der_ARDS_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_pulm___unk'))
    for(i in which(is.na(ccc19x$der_ARDS_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_ARDS_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_pulm_fu___unk'))
    for(i in which(is.na(ccc19x$der_ARDS_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_ARDS_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_ARDS_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_ARDS_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_ARDS_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_ARDS_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_ARDS_comp <- as.factor(ccc19x$der_ARDS_comp)
    
    temp <- summary(ccc19x$der_ARDS_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ARDS_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp20 Pleural Effusion: 60046008 (der_pleural_eff_comp)
    
    ccc19x$der_pleural_eff_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '60046008') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_pleural_eff_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_pleural_eff_comp[which(master_comp == 1 & is.na(ccc19x$der_pleural_eff_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_pulm___unk'))
    for(i in which(is.na(ccc19x$der_pleural_eff_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_pleural_eff_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_pulm_fu___unk'))
    for(i in which(is.na(ccc19x$der_pleural_eff_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_pleural_eff_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_pleural_eff_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_pleural_eff_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_pleural_eff_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_pleural_eff_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_pleural_eff_comp <- as.factor(ccc19x$der_pleural_eff_comp)
    
    temp <- summary(ccc19x$der_pleural_eff_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_pleural_eff_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp21 Empyema: 312682007 (der_empyema_comp)
    
    ccc19x$der_empyema_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '312682007') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_empyema_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_empyema_comp[which(master_comp == 1 & is.na(ccc19x$der_empyema_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_pulm___unk'))
    for(i in which(is.na(ccc19x$der_empyema_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_empyema_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_pulm_fu___unk'))
    for(i in which(is.na(ccc19x$der_empyema_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_empyema_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_empyema_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_empyema_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_empyema_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_empyema_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_empyema_comp <- as.factor(ccc19x$der_empyema_comp)
    
    temp <- summary(ccc19x$der_empyema_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_empyema_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #### Cardio ####
    
    #Comp22 Hypotension: 45007003 (der_hotn_comp)
    
    ccc19x$der_hotn_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '45007003') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_hotn_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_hotn_comp[which(master_comp == 1 & is.na(ccc19x$der_hotn_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk'))
    for(i in which(is.na(ccc19x$der_hotn_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_hotn_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk'))
    for(i in which(is.na(ccc19x$der_hotn_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_hotn_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_hotn_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_hotn_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_hotn_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_hotn_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_hotn_comp <- as.factor(ccc19x$der_hotn_comp)
    
    temp <- summary(ccc19x$der_hotn_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_hotn_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    
    #Comp23 Myocardial Infarction: 22298006 (def_MI_comp)
    
    ccc19x$der_MI_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '22298006') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_MI_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_MI_comp[which(master_comp == 1 & is.na(ccc19x$der_MI_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk'))
    for(i in which(is.na(ccc19x$der_MI_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_MI_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk'))
    for(i in which(is.na(ccc19x$der_MI_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_MI_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_MI_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_MI_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_MI_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_MI_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_MI_comp <- as.factor(ccc19x$der_MI_comp)
    
    temp <- summary(ccc19x$der_MI_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_MI_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp24 Other Cardiac Ischemia: 414545008 (der_card_isch_comp)
    
    ccc19x$der_card_isch_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '414545008') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_card_isch_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_card_isch_comp[which(master_comp == 1 & is.na(ccc19x$der_card_isch_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk'))
    for(i in which(is.na(ccc19x$der_card_isch_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_card_isch_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk'))
    for(i in which(is.na(ccc19x$der_card_isch_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_card_isch_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_card_isch_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_card_isch_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_card_isch_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_card_isch_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_card_isch_comp <- as.factor(ccc19x$der_card_isch_comp)
    
    temp <- summary(ccc19x$der_card_isch_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_card_isch_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp25 Atrial Fibrillation: 49436004 (der_AFib_comp)
    
    ccc19x$der_AFib_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '49436004') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_AFib_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_AFib_comp[which(master_comp == 1 & is.na(ccc19x$der_AFib_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk'))
    for(i in which(is.na(ccc19x$der_AFib_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_AFib_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk'))
    for(i in which(is.na(ccc19x$der_AFib_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_AFib_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_AFib_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_AFib_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_AFib_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_AFib_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_AFib_comp <- as.factor(ccc19x$der_AFib_comp)
    
    temp <- summary(ccc19x$der_Afib_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_Afib_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    
    #Comp26 Ventricular Fibrillation: 71908006 (der_VF_comp)
    
    ccc19x$der_VF_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '71908006') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_VF_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_VF_comp[which(master_comp == 1 & is.na(ccc19x$der_VF_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk'))
    for(i in which(is.na(ccc19x$der_VF_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_VF_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk'))
    for(i in which(is.na(ccc19x$der_VF_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_VF_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_VF_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_VF_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_VF_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_VF_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_VF_comp <- as.factor(ccc19x$der_VF_comp)
    
    temp <- summary(ccc19x$der_VF_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_VF_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp27 Other Cardiac Arrhythmia: 698247007 (der_arry_oth_comp)
    
    ccc19x$der_arry_oth_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '698247007') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_arry_oth_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_arry_oth_comp[which(master_comp == 1 & is.na(ccc19x$der_arry_oth_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk'))
    for(i in which(is.na(ccc19x$der_arry_oth_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_arry_oth_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk'))
    for(i in which(is.na(ccc19x$der_arry_oth_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_arry_oth_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_arry_oth_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_arry_oth_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_arry_oth_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_arry_oth_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_arry_oth_comp <- as.factor(ccc19x$der_arry_oth_comp)
    
    temp <- summary(ccc19x$der_arry_oth_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_arry_oth_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp28 Cardiomyopathy: 85898001 (der_CMY_comp)
    
    ccc19x$der_CMY_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '85898001') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_CMY_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_CMY_comp[which(master_comp == 1 & is.na(ccc19x$der_CMY_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk'))
    for(i in which(is.na(ccc19x$der_CMY_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_CMY_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk'))
    for(i in which(is.na(ccc19x$der_CMY_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_CMY_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_CMY_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_CMY_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_CMY_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_CMY_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_CMY_comp <- as.factor(ccc19x$der_CMY_comp)
    
    temp <- summary(ccc19x$der_CMY_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_CMY_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp29 Congestive Heart Failure: 42343007 (der_CHF_comp)
    
    ccc19x$der_CHF_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '42343007') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_CHF_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_CHF_comp[which(master_comp == 1 & is.na(ccc19x$der_CHF_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk'))
    for(i in which(is.na(ccc19x$der_CHF_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_CHF_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk'))
    for(i in which(is.na(ccc19x$der_CHF_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_CHF_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_CHF_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_CHF_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_CHF_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_CHF_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_CHF_comp <- as.factor(ccc19x$der_CHF_comp)
    
    temp <- summary(ccc19x$der_CHF_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_CHF_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    
    #### GI ####
    
    #Comp30 Acute Hepatic Injury: 427044009 (der_AHI_comp)
    
    ccc19x$der_AHI_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '427044009') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_AHI_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_AHI_comp[which(master_comp == 1 & is.na(ccc19x$der_AHI_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_gi___unk'))
    for(i in which(is.na(ccc19x$der_AHI_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_AHI_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_gi_fu___unk'))
    for(i in which(is.na(ccc19x$der_AHI_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_AHI_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_AHI_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_AHI_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_AHI_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_AHI_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_AHI_comp <- as.factor(ccc19x$der_AHI_comp)
    
    temp <- summary(ccc19x$der_AHI_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_AHI_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp31 Ascites: 389026000 (der_ascites_comp)
    
    ccc19x$der_ascites_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '389026000') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_ascites_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_ascites_comp[which(master_comp == 1 & is.na(ccc19x$der_ascites_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_gi___unk'))
    for(i in which(is.na(ccc19x$der_ascites_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_ascites_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_gi_fu___unk'))
    for(i in which(is.na(ccc19x$der_ascites_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_ascites_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_ascites_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_ascites_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_ascites_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_ascites_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_ascites_comp <- as.factor(ccc19x$der_ascites_comp)
    
    temp <- summary(ccc19x$der_ascites_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ascites_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp32 Bowel Obstruction: 81060008 (der_BO_comp)
    
    ccc19x$der_BO_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '81060008') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_BO_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_BO_comp[which(master_comp == 1 & is.na(ccc19x$der_BO_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_gi___unk'))
    for(i in which(is.na(ccc19x$der_BO_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_BO_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_gi_fu___unk'))
    for(i in which(is.na(ccc19x$der_BO_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_BO_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_BO_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_BO_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_BO_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_BO_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_BO_comp <- as.factor(ccc19x$der_BO_comp)
    
    temp <- summary(ccc19x$der_BO_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_BO_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp33 Bowel Perforation: 56905009 (der_bowelPerf_comp)
    
    ccc19x$der_bowelPerf_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '56905009') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_bowelPerf_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_bowelPerf_comp[which(master_comp == 1 & is.na(ccc19x$der_bowelPerf_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_gi___unk'))
    for(i in which(is.na(ccc19x$der_bowelPerf_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_bowelPerf_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_gi_fu___unk'))
    for(i in which(is.na(ccc19x$der_bowelPerf_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_bowelPerf_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_bowelPerf_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_bowelPerf_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_bowelPerf_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_bowelPerf_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_bowelPerf_comp <- as.factor(ccc19x$der_bowelPerf_comp)
    
    temp <- summary(ccc19x$der_bowelPerf_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_bowelPerf_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp34 Ileus: 710572000 (der_ileus_comp)
    
    ccc19x$der_ileus_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '710572000') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_ileus_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_ileus_comp[which(master_comp == 1 & is.na(ccc19x$der_ileus_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_gi___unk'))
    for(i in which(is.na(ccc19x$der_ileus_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_ileus_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_gi_fu___unk'))
    for(i in which(is.na(ccc19x$der_ileus_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_ileus_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_ileus_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_ileus_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_ileus_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_ileus_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_ileus_comp <- as.factor(ccc19x$der_ileus_comp)
    
    temp <- summary(ccc19x$der_ileus_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ileus_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    
    #Comp35 Peritonitis: 48661000 (der_peritonitis_comp)
    
    ccc19x$der_peritonitis_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '48661000') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_peritonitis_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_peritonitis_comp[which(master_comp == 1 & is.na(ccc19x$der_peritonitis_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_gi___unk'))
    for(i in which(is.na(ccc19x$der_peritonitis_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_peritonitis_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_gi_fu___unk'))
    for(i in which(is.na(ccc19x$der_peritonitis_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_peritonitis_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_peritonitis_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_peritonitis_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_peritonitis_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_peritonitis_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_peritonitis_comp <- as.factor(ccc19x$der_peritonitis_comp)
    
    temp <- summary(ccc19x$der_peritonitis_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_peritonitis_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp36 AKI: 14669001 (der_AKI_comp)
    
    ccc19x$der_AKI_comp <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '14669001') & grepl(colnames(ccc19x), pattern = 'complications'))
    
    #Present
    for(i in temp.ref)
      ccc19x$der_AKI_comp[which(ccc19x[,i] == 1)] <- 1
    
    #Not present, something else checked besides unknown
    ccc19x$der_AKI_comp[which(master_comp == 1 & is.na(ccc19x$der_AKI_comp))] <- 0
    
    #Unknown
    
    #Baseline
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_other___unk'))
    for(i in which(is.na(ccc19x$der_AKI_comp) & ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_AKI_comp[i] <- 99
    
    #Followup
    temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_other_fu___unk'))
    for(i in which(is.na(ccc19x$der_AKI_comp) & ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_AKI_comp[i] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_AKI_comp[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_AKI_comp[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_AKI_comp[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_AKI_comp[temp.ref] <- 0
      }
    }
    
    ccc19x$der_AKI_comp <- as.factor(ccc19x$der_AKI_comp)
    
    temp <- summary(ccc19x$der_AKI_comp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_AKI_comp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp37 Viral co-infection within +/- 2 weeks of COVID-19 diagnosis
    ccc19x$der_coinfection_viral <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'coinfection___') & !grepl(colnames(ccc19x), pattern = 'unk|none|442376007'))
    
    #Yes
    ccc19x$der_coinfection_viral[which(ccc19x$coinfection___49872002 == 1|
                                         ccc19x$coinfection___407479009 == 1|
                                         ccc19x$coinfection___407480007 == 1|
                                         ccc19x$coinfection___1838001 == 1|
                                         ccc19x$coinfection___6415009 == 1)] <- 1
    
    #No
    ccc19x$der_coinfection_viral[which((ccc19x$coinfection_yn == 0|ccc19x$coinfection___none == 1) & is.na(ccc19x$der_coinfection_viral))] <- 0
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(any(ccc19x[i,temp.ref] == 1) & is.na(ccc19x$der_coinfection_viral[i])) ccc19x$der_coinfection_viral[i] <- 0 
    
    #Unknown
    ccc19x$der_coinfection_viral[which((ccc19x$coinfection_yn == 99|ccc19x$coinfection___unk == 1) &
                                         is.na(ccc19x$der_coinfection_viral))] <- 99
    
    ccc19x$der_coinfection_viral <- factor(ccc19x$der_coinfection_viral)
    
    temp <- summary(ccc19x$der_coinfection_viral[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_coinfection_viral',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp38 Bacterial co-infection within +/- 2 weeks of COVID-19 diagnosis
    ccc19x$der_coinfection_bacterial <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'coinfection___') & !grepl(colnames(ccc19x), pattern = 'unk|none|442376007'))
    
    #Yes
    ccc19x$der_coinfection_bacterial[which(ccc19x$coinfection___409822003 == 1|
                                             ccc19x$coinfection___8745002 == 1|
                                             ccc19x$coinfection___233607000 == 1|
                                             ccc19x$coinfection___81325006 == 1)] <- 1
    
    #No
    ccc19x$der_coinfection_bacterial[which((ccc19x$coinfection_yn == 0|ccc19x$coinfection___none == 1) & is.na(ccc19x$der_coinfection_bacterial))] <- 0
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(any(ccc19x[i,temp.ref] == 1) & is.na(ccc19x$der_coinfection_bacterial[i])) ccc19x$der_coinfection_bacterial[i] <- 0 
    
    #Unknown
    ccc19x$der_coinfection_bacterial[which((ccc19x$coinfection_yn == 99|ccc19x$coinfection___unk == 1) &
                                             is.na(ccc19x$der_coinfection_bacterial))] <- 99
    
    ccc19x$der_coinfection_bacterial <- factor(ccc19x$der_coinfection_bacterial)
    
    temp <- summary(ccc19x$der_coinfection_bacterial[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_coinfection_bacterial',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp38a Gram Positive Bacterial co-infection within +/- 2 weeks of COVID-19 diagnosis
    ccc19x$der_coinfection_bact_gram_pos <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'coinfection___') & !grepl(colnames(ccc19x), pattern = 'unk|none|442376007'))
    
    #Yes
    ccc19x$der_coinfection_bact_gram_pos[which(ccc19x$coinfection___8745002 == 1|
                                                 ccc19x$coinfection___233607000 == 1)] <- 1
    
    #No
    ccc19x$der_coinfection_bact_gram_pos[which((ccc19x$coinfection_yn == 0|ccc19x$coinfection___none == 1) & is.na(ccc19x$der_coinfection_bact_gram_pos))] <- 0
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(any(ccc19x[i,temp.ref] == 1) & is.na(ccc19x$der_coinfection_bact_gram_pos[i])) ccc19x$der_coinfection_bact_gram_pos[i] <- 0 
    
    #Unknown
    ccc19x$der_coinfection_bact_gram_pos[which((ccc19x$coinfection_yn == 99|ccc19x$coinfection___unk == 1) &
                                                 is.na(ccc19x$der_coinfection_bact_gram_pos))] <- 99
    
    ccc19x$der_coinfection_bact_gram_pos <- factor(ccc19x$der_coinfection_bact_gram_pos)
    summary(ccc19x$der_coinfection_bact_gram_pos[ccc19x$redcap_repeat_instrument == ''])
    
    #Comp38b Gram Negative Bacterial co-infection within +/- 2 weeks of COVID-19 diagnosis
    ccc19x$der_coinfection_bact_gram_neg <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'coinfection___') & !grepl(colnames(ccc19x), pattern = 'unk|none|442376007'))
    
    #Yes
    ccc19x$der_coinfection_bact_gram_neg[which(ccc19x$coinfection___81325006 == 1)] <- 1
    
    #No
    ccc19x$der_coinfection_bact_gram_neg[which((ccc19x$coinfection_yn == 0|ccc19x$coinfection___none == 1) & is.na(ccc19x$der_coinfection_bact_gram_neg))] <- 0
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(any(ccc19x[i,temp.ref] == 1) & is.na(ccc19x$der_coinfection_bact_gram_neg[i])) ccc19x$der_coinfection_bact_gram_neg[i] <- 0 
    
    #Unknown
    ccc19x$der_coinfection_bact_gram_neg[which((ccc19x$coinfection_yn == 99|ccc19x$coinfection___unk == 1) &
                                                 is.na(ccc19x$der_coinfection_bact_gram_neg))] <- 99
    
    ccc19x$der_coinfection_bact_gram_neg <- factor(ccc19x$der_coinfection_bact_gram_neg)
    summary(ccc19x$der_coinfection_bact_gram_neg[ccc19x$redcap_repeat_instrument == ''])
    
    #Comp38c Bacterial co-infection NOS within +/- 2 weeks of COVID-19 diagnosis
    ccc19x$der_coinfection_bact_nos <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'coinfection___') & !grepl(colnames(ccc19x), pattern = 'unk|none|442376007'))
    
    #Yes
    ccc19x$der_coinfection_bact_nos[which(ccc19x$coinfection___409822003 == 1)] <- 1
    
    #No
    ccc19x$der_coinfection_bact_nos[which((ccc19x$coinfection_yn == 0|ccc19x$coinfection___none == 1) & is.na(ccc19x$der_coinfection_bact_nos))] <- 0
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(any(ccc19x[i,temp.ref] == 1) & is.na(ccc19x$der_coinfection_bact_nos[i])) ccc19x$der_coinfection_bact_nos[i] <- 0 
    
    #Unknown
    ccc19x$der_coinfection_bact_nos[which((ccc19x$coinfection_yn == 99|ccc19x$coinfection___unk == 1) &
                                            is.na(ccc19x$der_coinfection_bact_nos))] <- 99
    
    ccc19x$der_coinfection_bact_nos <- factor(ccc19x$der_coinfection_bact_nos)
    summary(ccc19x$der_coinfection_bact_nos[ccc19x$redcap_repeat_instrument == ''])
    
    #Comp39 Fungal co-infection within +/- 2 weeks of COVID-19 diagnosis
    ccc19x$der_coinfection_fungal <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'coinfection___') & !grepl(colnames(ccc19x), pattern = 'unk|none|442376007'))
    
    #Yes
    ccc19x$der_coinfection_fungal[which(ccc19x$coinfection___414561005 == 1|
                                          ccc19x$coinfection___2429008 == 1|
                                          ccc19x$coinfection___709601002 == 1)] <- 1
    
    #No
    ccc19x$der_coinfection_fungal[which((ccc19x$coinfection_yn == 0|ccc19x$coinfection___none == 1) & is.na(ccc19x$der_coinfection_fungal))] <- 0
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(any(ccc19x[i,temp.ref] == 1) & is.na(ccc19x$der_coinfection_fungal[i])) ccc19x$der_coinfection_fungal[i] <- 0 
    
    #Unknown
    ccc19x$der_coinfection_fungal[which((ccc19x$coinfection_yn == 99|ccc19x$coinfection___unk == 1) &
                                          is.na(ccc19x$der_coinfection_fungal))] <- 99
    
    ccc19x$der_coinfection_fungal <- factor(ccc19x$der_coinfection_fungal)
    
    temp <- summary(ccc19x$der_coinfection_fungal[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_coinfection_fungal',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp41 Other co-infection
    ccc19x$der_coinfection_other <- NA
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'coinfection___') & !grepl(colnames(ccc19x), pattern = 'unk|none|442376007'))
    
    #Yes
    ccc19x$der_coinfection_other[which(ccc19x$coinfection___oth == 1)] <- 1
    
    #No
    ccc19x$der_coinfection_other[which((ccc19x$coinfection_yn == 0|ccc19x$coinfection___none == 1) & is.na(ccc19x$der_coinfection_other))] <- 0
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(any(ccc19x[i,temp.ref] == 1) & is.na(ccc19x$der_coinfection_other[i])) ccc19x$der_coinfection_other[i] <- 0 
    
    #Unknown
    ccc19x$der_coinfection_other[which((ccc19x$coinfection_yn == 99|ccc19x$coinfection___unk == 1) &
                                          is.na(ccc19x$der_coinfection_other))] <- 99
    
    ccc19x$der_coinfection_other <- factor(ccc19x$der_coinfection_other)
    
    temp <- summary(ccc19x$der_coinfection_other[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_coinfection_other',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp40 Any co-infection 
    ccc19x$der_coinfection_any <- NA
    
    #Yes, known organism
    ccc19x$der_coinfection_any[which(ccc19x$der_coinfection_bacterial == 1|
                                       ccc19x$der_coinfection_fungal == 1|
                                       ccc19x$der_coinfection_viral == 1)] <- 1
    
    #Yes, organism remains undefined
    ccc19x$der_coinfection_any[which(ccc19x$coinfection___oth == 1 & is.na(ccc19x$der_coinfection_any))] <- 88
    
    #No
    ccc19x$der_coinfection_any[which(((ccc19x$der_coinfection_bacterial == 0 &
                                       ccc19x$der_coinfection_fungal == 0 &
                                       ccc19x$der_coinfection_viral == 0)|
                                        ccc19x$coinfection___none == 1) &
                                       is.na(ccc19x$der_coinfection_any))] <- 0
    
    #Unknown
    ccc19x$der_coinfection_any[which((ccc19x$der_coinfection_bacterial == 99|
                                        ccc19x$der_coinfection_fungal == 99|
                                        ccc19x$der_coinfection_viral == 99|
                                        ccc19x$coinfection___unk == 1) & is.na(ccc19x$der_coinfection_any))] <- 99
    
    ccc19x$der_coinfection_any <- factor(ccc19x$der_coinfection_any)
    summary(ccc19x$der_coinfection_any[ccc19x$redcap_repeat_instrument == ''])
    
    ########################
    #Composite complications
    ########################
    
    #Comp42a. Combined cardiovascular event (more comprehensive than der_CV_event)
    ccc19x$der_CV_event_v2 <- NA
    
    #Yes
    ccc19x$der_CV_event_v2[which(ccc19x$der_hotn_comp == 1|
                                       ccc19x$der_MI_comp == 1|
                                       ccc19x$der_card_isch_comp == 1|
                                       ccc19x$der_AFib_comp == 1|
                                       ccc19x$der_VF_comp == 1|
                                       ccc19x$der_arry_oth_comp == 1|
                                       ccc19x$der_CMY_comp == 1|
                                       ccc19x$der_CHF_comp == 1|
                                       ccc19x$der_PE_comp == 1|
                                       ccc19x$der_DVT_comp == 1|
                                       ccc19x$der_stroke_comp == 1|
                                       ccc19x$der_thrombosis_NOS_comp == 1)] <- 1

    #No
    ccc19x$der_CV_event_v2[which(ccc19x$der_hotn_comp == 0 &
                                       ccc19x$der_MI_comp == 0 &
                                       ccc19x$der_card_isch_comp == 0 &
                                       ccc19x$der_AFib_comp == 0 &
                                       ccc19x$der_VF_comp == 0 &
                                       ccc19x$der_arry_oth_comp == 0 &
                                       ccc19x$der_CMY_comp == 0 &
                                       ccc19x$der_CHF_comp == 0 &
                                       ccc19x$der_PE_comp == 0 &
                                       ccc19x$der_DVT_comp == 0 &
                                       ccc19x$der_stroke_comp == 0 &
                                       ccc19x$der_thrombosis_NOS_comp == 0 &
                                       is.na(ccc19x$der_CV_event_v2))] <- 0
    
    #Unknown
    ccc19x$der_CV_event_v2[which((ccc19x$der_hotn_comp == 99|
                                        ccc19x$der_MI_comp == 99|
                                        ccc19x$der_card_isch_comp == 99|
                                        ccc19x$der_AFib_comp == 99|
                                        ccc19x$der_VF_comp == 99|
                                        ccc19x$der_arry_oth_comp == 99|
                                        ccc19x$der_CMY_comp == 99|
                                        ccc19x$der_CHF_comp == 99|
                                        ccc19x$der_PE_comp == 99|
                                        ccc19x$der_DVT_comp == 99|
                                        ccc19x$der_stroke_comp == 99|
                                        ccc19x$der_thrombosis_NOS_comp == 99) & 
                                       is.na(ccc19x$der_CV_event_v2))] <- 99
  
    ccc19x$der_CV_event_v2 <- factor(ccc19x$der_CV_event_v2)
    
    temp <- summary(ccc19x$der_CV_event_v2[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_CV_event_v2',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp42b. Combined cardiovascular event (more comprehensive than der_CV_event, slightly different than above)
    ccc19x$der_CV_event_v3 <- NA
    
    #Yes
    ccc19x$der_CV_event_v3[which(ccc19x$der_SVT_comp == 1|
                                   ccc19x$der_MI_comp == 1|
                                   ccc19x$der_card_isch_comp == 1|
                                   ccc19x$der_AFib_comp == 1|
                                   ccc19x$der_VF_comp == 1|
                                   ccc19x$der_arry_oth_comp == 1|
                                   ccc19x$der_CMY_comp == 1|
                                   ccc19x$der_CHF_comp == 1|
                                   ccc19x$der_PE_comp == 1|
                                   ccc19x$der_DVT_comp == 1|
                                   ccc19x$der_stroke_comp == 1|
                                   ccc19x$der_thrombosis_NOS_comp == 1)] <- 1
    
    #No
    ccc19x$der_CV_event_v3[which(ccc19x$der_SVT_comp == 0 &
                                   ccc19x$der_MI_comp == 0 &
                                   ccc19x$der_card_isch_comp == 0 &
                                   ccc19x$der_AFib_comp == 0 &
                                   ccc19x$der_VF_comp == 0 &
                                   ccc19x$der_arry_oth_comp == 0 &
                                   ccc19x$der_CMY_comp == 0 &
                                   ccc19x$der_CHF_comp == 0 &
                                   ccc19x$der_PE_comp == 0 &
                                   ccc19x$der_DVT_comp == 0 &
                                   ccc19x$der_stroke_comp == 0 &
                                   ccc19x$der_thrombosis_NOS_comp == 0 &
                                   is.na(ccc19x$der_CV_event_v3))] <- 0
    
    #Unknown
    ccc19x$der_CV_event_v3[which((ccc19x$der_SVT_comp == 99|
                                    ccc19x$der_MI_comp == 99|
                                    ccc19x$der_card_isch_comp == 99|
                                    ccc19x$der_AFib_comp == 99|
                                    ccc19x$der_VF_comp == 99|
                                    ccc19x$der_arry_oth_comp == 99|
                                    ccc19x$der_CMY_comp == 99|
                                    ccc19x$der_CHF_comp == 99|
                                    ccc19x$der_PE_comp == 99|
                                    ccc19x$der_DVT_comp == 99|
                                    ccc19x$der_stroke_comp == 99|
                                    ccc19x$der_thrombosis_NOS_comp == 99) & 
                                   is.na(ccc19x$der_CV_event_v3))] <- 99
    
    ccc19x$der_CV_event_v3 <- factor(ccc19x$der_CV_event_v3)
    
    temp <- summary(ccc19x$der_CV_event_v3[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_CV_event_v3',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp43. Combined cardiac event
    ccc19x$der_cardiac_event <- NA
    
    #Yes
    ccc19x$der_cardiac_event[which(ccc19x$der_MI_comp == 1|
                                     ccc19x$der_card_isch_comp == 1|
                                     ccc19x$der_AFib_comp == 1|
                                     ccc19x$der_VF_comp == 1|
                                     ccc19x$der_arry_oth_comp == 1|
                                     ccc19x$der_CMY_comp == 1|
                                     ccc19x$der_CHF_comp == 1)] <- 1
    
    #No
    ccc19x$der_cardiac_event[which(ccc19x$der_MI_comp == 0 &
                                     ccc19x$der_card_isch_comp == 0 &
                                     ccc19x$der_AFib_comp == 0 &
                                     ccc19x$der_VF_comp == 0 &
                                     ccc19x$der_arry_oth_comp == 0 &
                                     ccc19x$der_CMY_comp == 0 &
                                     ccc19x$der_CHF_comp == 0 &
                                     is.na(ccc19x$der_cardiac_event))] <- 0
    
    #Unknown
    ccc19x$der_cardiac_event[which((ccc19x$der_MI_comp == 99|
                                      ccc19x$der_card_isch_comp == 99|
                                      ccc19x$der_AFib_comp == 99|
                                      ccc19x$der_VF_comp == 99|
                                      ccc19x$der_arry_oth_comp == 99|
                                      ccc19x$der_CMY_comp == 99|
                                      ccc19x$der_CHF_comp == 99) & 
                                     is.na(ccc19x$der_cardiac_event))] <- 99
    
    ccc19x$der_cardiac_event <- factor(ccc19x$der_cardiac_event)
    
    temp <- summary(ccc19x$der_cardiac_event[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_cardiac_event',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp44. Combined pulmonary event
    ccc19x$der_pulm_event <- NA
    
    #Yes
    ccc19x$der_pulm_event[which(ccc19x$der_resp_failure_comp == 1|
                                  ccc19x$der_pneumonitis_comp == 1|
                                  ccc19x$der_pneumonia_comp == 1|
                                  ccc19x$der_ARDS_comp == 1|
                                  ccc19x$der_PE_comp == 1|
                                  ccc19x$der_pleural_eff_comp == 1|
                                  ccc19x$der_empyema_comp == 1)] <- 1
    
    #No
    ccc19x$der_pulm_event[which(ccc19x$der_resp_failure_comp == 0 &
                                  ccc19x$der_pneumonitis_comp == 0 &
                                  ccc19x$der_pneumonia_comp == 0 &
                                  ccc19x$der_ARDS_comp == 0 &
                                  ccc19x$der_PE_comp == 0 &
                                  ccc19x$der_pleural_eff_comp == 0 &
                                  ccc19x$der_empyema_comp == 0 &
                                  is.na(ccc19x$der_pulm_event))] <- 0
    
    #Unknown
    ccc19x$der_pulm_event[which((ccc19x$der_resp_failure_comp == 99|
                                   ccc19x$der_pneumonitis_comp == 99|
                                   ccc19x$der_pneumonia_comp == 99|
                                   ccc19x$der_ARDS_comp == 99|
                                   ccc19x$der_PE_comp == 99|
                                   ccc19x$der_pleural_eff_comp == 99|
                                   ccc19x$der_empyema_comp == 99) & 
                                  is.na(ccc19x$der_pulm_event))] <- 99
    
    ccc19x$der_pulm_event <- factor(ccc19x$der_pulm_event)
    
    temp <- summary(ccc19x$der_pulm_event[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_pulm_event',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Comp45. Combined GI event
    ccc19x$der_GI_event <- NA
    
    #Yes
    ccc19x$der_GI_event[which(ccc19x$der_AHI_comp == 1|
                                ccc19x$der_ascites_comp == 1|
                                ccc19x$der_BO_comp == 1|
                                ccc19x$der_bowelPerf_comp == 1|
                                ccc19x$der_ileus_comp == 1|
                                ccc19x$der_peritonitis_comp == 1)] <- 1
    
    #No
    ccc19x$der_GI_event[which(ccc19x$der_AHI_comp == 0 &
                                ccc19x$der_ascites_comp == 0 &
                                ccc19x$der_BO_comp == 0 &
                                ccc19x$der_bowelPerf_comp == 0 &
                                ccc19x$der_ileus_comp == 0 &
                                ccc19x$der_peritonitis_comp == 0 &
                                is.na(ccc19x$der_GI_event))] <- 0
    
    #Unknown
    ccc19x$der_GI_event[which((ccc19x$der_AHI_comp == 99|
                                 ccc19x$der_ascites_comp == 99|
                                 ccc19x$der_BO_comp == 99|
                                 ccc19x$der_bowelPerf_comp == 99|
                                 ccc19x$der_ileus_comp == 99|
                                 ccc19x$der_peritonitis_comp == 99) & 
                                is.na(ccc19x$der_GI_event))] <- 99
    
    ccc19x$der_GI_event <- factor(ccc19x$der_GI_event)
    
    temp <- summary(ccc19x$der_GI_event[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_GI_event',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ####################################################
    #Comp46. Complications with severity by adjudication
    ####################################################
    {
      ccc19x$der_complications_severity <- NA
      
      #Named complications
      comp <- read.csv(file = 'Mapping - complications/2022-01-22 complications graded.csv', header = T, stringsAsFactors = F)
      
      #Graded
      temp.ref1 <- which(colnames(ccc19x) %in% comp$variable[comp$value == 'Mild'])
      temp.ref2 <- which(colnames(ccc19x) %in% comp$variable[comp$value == 'Moderate'])
      temp.ref3 <- which(colnames(ccc19x) %in% comp$variable[comp$value == 'Serious'])
      
      #Go through the rows and overwrite from least to most complicated
      for(i in 1:nrow(ccc19x))
      {
        if(length(which(ccc19x[i,temp.ref1] == 1)) > 0) ccc19x$der_complications_severity[i] <- 'Mild'
        if(length(which(ccc19x[i,temp.ref2] == 1)) > 0) ccc19x$der_complications_severity[i] <- 'Moderate'
        if(length(which(ccc19x[i,temp.ref3] == 1)) > 0) ccc19x$der_complications_severity[i] <- 'Serious'
      }
      
      #None of the named complications
      temp.ref <- which(colnames(ccc19x) %in% comp$variable)
      temp.ref2 <- which(is.na(ccc19x$der_complications_severity))
      for(i in temp.ref2)
      {
        temp <- ccc19x[i,temp.ref]
        #Remove the missing
        temp <- temp[!is.na(temp)]
        if(all(temp == 0)) ccc19x$der_complications_severity[i] <- 'None'
      }
      
      #Other
      
      #Baseline
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_systemic___238147009',
                                                'c19_complications_pulm___50043002',
                                                'c19_complications_card___49601007',
                                                'c19_complications_gi___53619000',
                                                'c19_complications_other___362965005'))
      temp.ref2 <- which(is.na(ccc19x$der_complications_severity) & ccc19x$redcap_repeat_instrument == '')
      for(i in temp.ref2)
        if(any(ccc19x[i,temp.ref] == 1)) ccc19x$der_complications_severity[i] <- 'Other'
      
      #Follow-up (affects full case if present)
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_systemic_fu___238147009',
                                                'c19_complications_pulm_fu___50043002',
                                                'c19_complications_card_fu___49601007',
                                                'c19_complications_gi_fu___53619000',
                                                'c19_complications_other_fu___362965005'))
      temp.ref2 <- which(is.na(ccc19x$der_complications_severity) & ccc19x$redcap_repeat_instrument == 'followup')
      for(i in temp.ref2)
        if(any(ccc19x[i,temp.ref] == 1)) 
        {
          temp.ref3 <- which(ccc19x$record_id == ccc19x$record_id[i])
          if(all(is.na(ccc19x$der_complications_severity[temp.ref3]))) ccc19x$der_complications_severity[temp.ref3] <- 'Other'
        }
      
      #Unknown
      temp.ref <- which(colnames(ccc19x) %in% comp$variable)
      temp.ref2 <- which(is.na(ccc19x$der_complications_severity))
      for(i in temp.ref2)
      {
        temp <- ccc19x[i,temp.ref]
        #Remove the missing
        temp <- temp[!is.na(temp)]
        if(any(temp == 99)) ccc19x$der_complications_severity[i] <- 'Unknown'
      }
      
      #Categorical complications based on der_worst (overwrite to higher grade if needed)
      ccc19x$der_complications_severity[which(ccc19x$der_complications_severity %in% c('None', 'Other', 'Unknown') &
                                                ccc19x$der_worst == 'Mild')] <- 'Mild'
      
      ccc19x$der_complications_severity[which(ccc19x$der_complications_severity %in% c('None', 'Mild', 'Other', 'Unknown') &
                                                ccc19x$der_worst == 'Moderate')] <- 'Moderate'
      
      ccc19x$der_complications_severity[which(ccc19x$der_complications_severity %in% c('None', 'Mild', 'Moderate', 'Other', 'Unknown') &
                                                ccc19x$der_worst == 'Serious')] <- 'Serious'
      
      ccc19x$der_complications_severity[which(ccc19x$der_complications_severity %in% c('None', 'Unknown') &
                                                ccc19x$der_worst == 'Other')] <- 'Other'
      
      ccc19x$der_complications_severity <- factor(ccc19x$der_complications_severity)
      
      temp <- summary(ccc19x$der_complications_severity[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_complications_severity',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
    }
    
    ######
    rm(master_comp)
    }
  print('Complications completed')
  
  ##################
  #Time measurements
  ##################
  {
    #Create interval bounds and anchors
    {
      #Data fix
      ccc19x$ts_5 <- gsub(ccc19x$ts_5, pattern = '/20 ', replacement = '/2020 ')
      
      #T1 & T2. Time of last known followup (if alive) or to death (if dead) in days
      ccc19x$meta_righttime <- as.POSIXlt("2099-12-31 00:00:00 CDT")
      ccc19x$meta_righttime[ccc19x$ts_0 != ''] <- as.POSIXct(ccc19x$ts_0[ccc19x$ts_0 != ''])
      
      ccc19x$meta_lefttime_lb <- as.POSIXlt("2099-12-31 00:00:00 CDT")
      
      #First initial form
      temp.ref <- which(ccc19x$covid_19_dx_interval == 1)
      ccc19x$meta_lefttime_lb[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 7*24*60*60
      
      temp.ref <- which(ccc19x$covid_19_dx_interval == 2)
      ccc19x$meta_lefttime_lb[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 14*24*60*60
      
      temp.ref <- which(ccc19x$covid_19_dx_interval == 3)
      ccc19x$meta_lefttime_lb[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 28*24*60*60
      
      temp.ref <- which(ccc19x$covid_19_dx_interval == 4)
      ccc19x$meta_lefttime_lb[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 56*24*60*60
      
      temp.ref <- which(ccc19x$covid_19_dx_interval == 5)
      ccc19x$meta_lefttime_lb[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 84*24*60*60
      
      temp.ref <- which(ccc19x$covid_19_dx_interval == 6)
      ccc19x$meta_lefttime_lb[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 180*24*60*60
      
      temp.ref <- which(ccc19x$covid_19_dx_interval == 8)
      ccc19x$meta_lefttime_lb[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 270*24*60*60
      
      temp.ref <- which(ccc19x$covid_19_dx_interval == 9)
      ccc19x$meta_lefttime_lb[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 360*24*60*60
      
      #Pin the open-ended intervals at January 1, 2020
      temp.ref <- which(ccc19x$covid_19_dx_interval %in% c(7,10))
      ccc19x$meta_lefttime_lb[temp.ref] <- as.POSIXlt("2020-01-01 00:00:00 CDT")
      
      
      # #now deal with followup time based on time stamps
      temp <- unique(ccc19x$record_id[ccc19x$redcap_repeat_instrument == 'followup'])
      for(i in 1:length(temp))
      {
        temp.ref <- which(ccc19x$record_id == temp[i])
        temp.time <- unique(ccc19x$ts_5[temp.ref])
        temp.time <- temp.time[temp.time != '']
        if(length(temp.time) > 0)
        {
          temp.list <- list() 
          for(j in 1:length(temp.time))
            temp.list[[j]] <- as.POSIXlt(temp.time[j], tryFormats = c('%m/%d/%Y %H:%M',
                                                                      '%Y-%m-%d %H:%M',
                                                                      '%Y-%m-%d'))
          for(j in 1:length(temp.time))
            temp.time[j] <- as.character(temp.list[[j]])
          
          temp.time <- as.POSIXlt(temp.time, tryFormats = c('%m/%d/%Y %H:%M',
                                                            '%Y-%m-%d %H:%M',
                                                            '%Y-%m-%d'))
          temp.time <- temp.time[which(temp.time == max(temp.time))]
          ccc19x$meta_righttime[temp.ref] <- temp.time[1]
        }
      }
      
      #Fix left time if there is a discrepancy with the year of diagnosis
      temp.ref <- which(substr(ccc19x$meta_lefttime_lb, start=1, stop=4) == 2019 &
                          ccc19x$dx_year == 2020)
      ccc19x$meta_lefttime_lb[temp.ref] <- as.POSIXct('2020-01-01 00:00:00 CST')
      
      temp.ref <- which(substr(ccc19x$meta_lefttime_lb, start=1, stop=4) == 2020 &
                          ccc19x$dx_year == 2021)
      ccc19x$meta_lefttime_lb[temp.ref] <- as.POSIXct('2021-01-01 00:00:00 CST')
      
      temp.ref <- which(substr(ccc19x$meta_lefttime_lb, start=1, stop=4) == 2021 &
                          ccc19x$dx_year == 2020)
      ccc19x$meta_lefttime_lb[temp.ref] <- as.POSIXct('2020-12-31 00:00:00 CST')
      
      temp.ref <- which(substr(ccc19x$meta_lefttime_lb, start=1, stop=4) == 2021 &
                          ccc19x$dx_year == 2022)
      ccc19x$meta_lefttime_lb[temp.ref] <- as.POSIXct('2022-01-01 00:00:00 CST')
      
      temp.ref <- which(substr(ccc19x$meta_lefttime_lb, start=1, stop=4) == 2022 &
                          ccc19x$dx_year == 2021)
      ccc19x$meta_lefttime_lb[temp.ref] <- as.POSIXct('2021-12-31 00:00:00 CST')
      
      #T7 & T8 Calculated time from diagnosis has to be at least 30 days
      ccc19x$meta_lefttime_ub <- as.POSIXlt("2099-12-31 00:00:00 CDT")
      
      #First initial form
      temp.ref <- which(ccc19x$covid_19_dx_interval == 1)
      ccc19x$meta_lefttime_ub[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 0*24*60*60
      
      temp.ref <- which(ccc19x$covid_19_dx_interval == 2)
      ccc19x$meta_lefttime_ub[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 7*24*60*60
      
      temp.ref <- which(ccc19x$covid_19_dx_interval == 3)
      ccc19x$meta_lefttime_ub[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 14*24*60*60
      
      temp.ref <- which(ccc19x$covid_19_dx_interval == 4)
      ccc19x$meta_lefttime_ub[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 28*24*60*60
      
      temp.ref <- which(ccc19x$covid_19_dx_interval == 5)
      ccc19x$meta_lefttime_ub[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 56*24*60*60
      
      temp.ref <- which(ccc19x$covid_19_dx_interval == 6)
      ccc19x$meta_lefttime_ub[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 90*24*60*60
      
      temp.ref <- which(ccc19x$covid_19_dx_interval %in% 7:8)
      ccc19x$meta_lefttime_ub[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 180*24*60*60
      
      temp.ref <- which(ccc19x$covid_19_dx_interval == 9)
      ccc19x$meta_lefttime_ub[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 270*24*60*60
      
      temp.ref <- which(ccc19x$covid_19_dx_interval == 10)
      ccc19x$meta_lefttime_ub[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 360*24*60*60
      
      # #now deal with followup time based on time stamps
      temp <- unique(ccc19x$record_id[ccc19x$redcap_repeat_instrument == 'followup'])
      for(i in 1:length(temp))
      {
        temp.ref <- which(ccc19x$record_id == temp[i])
        temp.time <- unique(ccc19x$ts_5[temp.ref])
        temp.time <- temp.time[temp.time != '']
        if(length(temp.time) > 0)
        {
          temp.list <- list() 
          for(j in 1:length(temp.time))
            temp.list[[j]] <- as.POSIXlt(temp.time[j], tryFormats = c('%m/%d/%Y %H:%M',
                                                                      '%Y-%m-%d %H:%M',
                                                                      '%Y-%m-%d'))
          for(j in 1:length(temp.time))
            temp.time[j] <- as.character(temp.list[[j]])
          
          temp.time <- as.POSIXlt(temp.time, tryFormats = c('%m/%d/%Y %H:%M',
                                                            '%Y-%m-%d %H:%M',
                                                            '%Y-%m-%d'))
          temp.time <- temp.time[which(temp.time == max(temp.time))]
          ccc19x$meta_righttime[temp.ref] <- temp.time[1]
        }
      }
      
      #Fix left time if there is a discrepancy with the year of diagnosis
      temp.ref <- which(substr(ccc19x$meta_lefttime_ub, start=1, stop=4) == 2019 &
                          ccc19x$dx_year == 2020)
      ccc19x$meta_lefttime_ub[temp.ref] <- as.POSIXct('2020-01-01 00:00:00 CST')
      
      temp.ref <- which(substr(ccc19x$meta_lefttime_ub, start=1, stop=4) == 2020 &
                          ccc19x$dx_year == 2021)
      ccc19x$meta_lefttime_ub[temp.ref] <- as.POSIXct('2021-01-01 00:00:00 CST')
      
      temp.ref <- which(substr(ccc19x$meta_lefttime_ub, start=1, stop=4) == 2021 &
                          ccc19x$dx_year == 2020)
      ccc19x$meta_lefttime_ub[temp.ref] <- as.POSIXct('2020-12-31 00:00:00 CST')
      
      temp.ref <- which(substr(ccc19x$meta_lefttime_ub, start=1, stop=4) == 2021 &
                          ccc19x$dx_year == 2022)
      ccc19x$meta_lefttime_ub[temp.ref] <- as.POSIXct('2022-01-01 00:00:00 CST')
      
      temp.ref <- which(substr(ccc19x$meta_lefttime_ub, start=1, stop=4) == 2022 &
                          ccc19x$dx_year == 2021)
      ccc19x$meta_lefttime_ub[temp.ref] <- as.POSIXct('2021-12-31 00:00:00 CST')
      
      #T4. Middle of meta_lefttime_lb and meta_lefttime_ub
      temp <- as.numeric(difftime(ccc19x$meta_lefttime_ub, 
                                  ccc19x$meta_lefttime_lb, 
                                  units = 'days'))/2
      
      ccc19x$meta_lefttime_med <- ccc19x$meta_lefttime_ub - temp*24*60*60
      
    }
    
    #T6. 30-day follow-up available (0 = no; 1 = yes; 99 = unknown)
    {
      ccc19x$der_d30 <- NA 
      
      #30-day question filled out
      ccc19x$der_d30[which(ccc19x$mortality == 1)] <- 1
      ccc19x$der_d30[which(ccc19x$mortality %in% c(0,88))] <- 0
      ccc19x$der_d30[which(ccc19x$mortality == 99)] <- 99
      
      #30, 90, 180, or 365-day f/u form filled out
      temp <- ccc19x$record_id[which(ccc19x$fu_weeks %in% c(30,90,180,365))]
      ccc19x$der_d30[which(ccc19x$record_id %in% temp)] <- 1
      
      #Other f/u form with weeks > 4 filled out
      temp <- ccc19x$record_id[which(ccc19x$fu_weeks == 'OTH' & ccc19x$timing_of_report_weeks > 4)]
      ccc19x$der_d30[which(ccc19x$record_id %in% temp)] <- 1
      
      temp.diff <- difftime(ccc19x$meta_righttime, ccc19x$meta_lefttime_ub, units = 'days')
      temp <- ccc19x$record_id[which(as.numeric(temp.diff) >= 30)]
      
      ccc19x$der_d30[which(ccc19x$record_id %in% temp)] <- 1
      
      ccc19x$der_d30 <- factor(ccc19x$der_d30)
      
      temp <- summary(ccc19x$der_d30[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_d30',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
    }
    
    ###############
    #T3. Median f/u
    ###############
    {
      ccc19x$der_days_fu <- NA
      pts <- unique(ccc19x$record_id)
      
      #Middle of follow-up intervals or right side of open intervals (from covid_19_dx_interval)
      fu <- c(7/2, #within the past week
              (7+14)/2, #within the past 1-2 weeks
              (14+28)/2, #within the past 2-4 weeks
              (28+56)/2, #within the past 4-8 weeks
              (56+84)/2, #within the past 8-12 weeks 
              (90+180)/2, #within the past 3-6 months
              180, #more than 6 months ago
              (180+270)/2, #within the past 6-9 months
              (270+360)/2, #within the past 9-12 months
              365 #more than 12 months ago
      )
      
      for(i in 1:length(pts))
      {
        temp.ref <- which(ccc19x$record_id == pts[i])
        #Check that f/u form has been completed or is unverified, remove otherwise
        temp <- ccc19x$followup_complete[temp.ref]
        temp.ref <- temp.ref[which(is.na(temp) | temp %in% 1:2)]
        if(length(temp.ref) == 1) #No follow-up forms
        {
          #Check if days to death has data, if so use it
          if(!is.na(ccc19x$der_days_to_death_combined[temp.ref])) 
          {
            if(ccc19x$der_days_to_death_combined[temp.ref] != 9999) 
            {
              ccc19x$der_days_fu[temp.ref] <- ccc19x$der_days_to_death_combined[temp.ref]
            } else #Default is the median of the time interval of diagnosis or the f/u period for the form reporting death, whichever is smaller
            {
              ccc19x$der_days_fu[temp.ref] <- fu[ccc19x$covid_19_dx_interval[temp.ref]]
            }
          } else
          {
            #Default is the median of the time interval of diagnosis
            ccc19x$der_days_fu[temp.ref] <- fu[ccc19x$covid_19_dx_interval[temp.ref]]
            
            # #Check that LOS aren't longer than this
            # if(!is.na(ccc19x$der_days_fu[temp.ref]))
            # {
            #   temp <- ccc19x[temp.ref,c('hosp_los', 'hosp_los_2', 'icu_los')]
            #   temp <- sum(temp[!is.na(temp)])
            #   if(temp > ccc19x$der_days_fu[temp.ref]) ccc19x$der_days_fu[temp.ref] <- temp
            # }
            
            #If patient is deceased and days are missing, check the mortality variables for floor adjustment
            temp <- ccc19x$der_deadbinary[temp.ref] == 1 & ccc19x$der_days_fu[temp.ref] > 30 & 
              ccc19x$mortality[temp.ref] == 0 & (is.na(ccc19x$der_days_to_death_combined[temp.ref])|ccc19x$der_days_to_death_combined[temp.ref] == 9999)
            if(!is.na(temp) & temp) ccc19x$der_days_fu[temp.ref] <- 30
            
            temp <- ccc19x$der_deadbinary[temp.ref] == 1 & ccc19x$der_days_fu[temp.ref] > 90 & 
              ccc19x$mortality_90[temp.ref] == 0 & (is.na(ccc19x$der_days_to_death_combined[temp.ref])|ccc19x$der_days_to_death_combined[temp.ref] == 9999)
            if(!is.na(temp) & temp) ccc19x$der_days_fu[temp.ref] <- 90
            
            temp <- ccc19x$der_deadbinary[temp.ref] == 1 & ccc19x$der_days_fu[temp.ref] > 180 & 
              ccc19x$mortality_180[temp.ref] == 0 & (is.na(ccc19x$der_days_to_death_combined[temp.ref])|ccc19x$der_days_to_death_combined[temp.ref] == 9999)
            if(!is.na(temp) & temp) ccc19x$der_days_fu[temp.ref] <- 180
            
          }
        } else
        {
          temp <- unique(ccc19x$der_days_to_death_combined[temp.ref])
          temp <- temp[!is.na(temp)]
          temp.ref2 <- which(ccc19x$redcap_repeat_instrument[temp.ref] == '')
          #Check if days to death has data, if so use it
          if(length(temp) == 1) 
          {
            if(temp != 9999) 
            {
              ccc19x$der_days_fu[temp.ref] <- temp
            } else #Default is the follow up time
            {
              temp <- ccc19x$fu_weeks[temp.ref]
              if(any(temp == 'OTH'))
              {
                temp[temp == 'OTH'] <- 7*ccc19x$timing_of_report_weeks[temp.ref[which(temp == 'OTH')]]
                #Attempt to address local site subversion of the required f/u in weeks field
                if(any(is.na(temp[temp != ''])))
                {
                  temp.ref3 <- which(ccc19x$redcap_repeat_instrument[temp.ref] == 'followup')
                  t1 <- fu[ccc19x$covid_19_dx_interval[temp.ref[temp.ref2]]]
                  t2 <- max(as.POSIXct(ccc19x$ts_5[temp.ref[temp.ref3]]))
                  #Use the baseline timestamp, if it exists
                  if(ccc19x$ts_0[temp.ref[temp.ref2]] != '')
                  {
                    t2 <- as.numeric(difftime(t2, 
                                              as.POSIXct(ccc19x$ts_0[temp.ref[temp.ref2]]),
                                              units = 'days'))
                  } else t2 <- 0
                  ccc19x$der_days_fu[temp.ref] <- t1 + t2
                }
              }
              if(length(temp[temp != '']) > 0 & all(!is.na(temp[temp != '']))) ccc19x$der_days_fu[temp.ref] <- max(as.numeric(temp[temp != '']))
            }
            
          } else
          {
            temp <- ccc19x$fu_weeks[temp.ref]
            if(any(temp == 'OTH'))
            {
              temp[temp == 'OTH'] <- 7*ccc19x$timing_of_report_weeks[temp.ref[which(temp == 'OTH')]]
              #Attempt to address local site subversion of the required f/u in weeks field
              if(any(is.na(temp[temp != ''])))
              {
                temp.ref3 <- which(ccc19x$redcap_repeat_instrument[temp.ref] == 'followup')
                t1 <- fu[ccc19x$covid_19_dx_interval[temp.ref[temp.ref2]]]
                t2 <- max(as.POSIXct(ccc19x$ts_5[temp.ref[temp.ref3]]))
                #Use the baseline timestamp, if it exists
                if(ccc19x$ts_0[temp.ref[temp.ref2]] != '')
                {
                  t2 <- as.numeric(difftime(t2, 
                                            as.POSIXct(ccc19x$ts_0[temp.ref[temp.ref2]]),
                                            units = 'days'))
                } else t2 <- 0
                ccc19x$der_days_fu[temp.ref] <- t1 + t2
              }
            }
            if(length(temp[temp != '']) > 0 & all(!is.na(temp[temp != '']))) ccc19x$der_days_fu[temp.ref] <- max(as.numeric(temp[temp != '']))
          }
          
          #If patient is deceased and days are missing, check the vital status indicators for floor adjustment
          temp <- any(ccc19x$der_deadbinary[temp.ref] == 1) & any(ccc19x$der_days_fu[temp.ref] > 30) & 
            any(ccc19x$d30_vital_status[temp.ref] == 1) & any(is.na(ccc19x$der_days_to_death_combined[temp.ref])|ccc19x$der_days_to_death_combined[temp.ref] == 9999)
          if(!is.na(temp) & temp) ccc19x$der_days_fu[temp.ref] <- 30
          
          temp <- any(ccc19x$der_deadbinary[temp.ref] == 1) & any(ccc19x$der_days_fu[temp.ref] > 90) & 
            any(ccc19x$d90_vital_status[temp.ref] == 1) & any(is.na(ccc19x$der_days_to_death_combined[temp.ref])|ccc19x$der_days_to_death_combined[temp.ref] == 9999)
          if(!is.na(temp) & temp) ccc19x$der_days_fu[temp.ref] <- 90
          
          temp <- any(ccc19x$der_deadbinary[temp.ref] == 1) & any(ccc19x$der_days_fu[temp.ref] > 180) & 
            any(ccc19x$d180_vital_status[temp.ref] == 1) & any(is.na(ccc19x$der_days_to_death_combined[temp.ref])|ccc19x$der_days_to_death_combined[temp.ref] == 9999)
          if(!is.na(temp) & temp) ccc19x$der_days_fu[temp.ref] <- 180
          
          temp <- any(ccc19x$der_deadbinary[temp.ref] == 1) & any(ccc19x$der_days_fu[temp.ref] > 365) & 
            any(ccc19x$d365_vital_status[temp.ref] == 1) & any(is.na(ccc19x$der_days_to_death_combined[temp.ref])|ccc19x$der_days_to_death_combined[temp.ref] == 9999)
          if(!is.na(temp) & temp) ccc19x$der_days_fu[temp.ref] <- 365
        }
      }
      
      #Make sure that all forms (baseline and follow-up) for a given patient have the same f/u value
      for(i in 1:length(pts))
      {
        temp.ref <- which(ccc19x$record_id == pts[i])
        temp <- ccc19x$der_days_fu[temp.ref]
        temp <- unique(temp[!is.na(temp)])
        if(length(temp) == 1) ccc19x$der_days_fu[temp.ref] <- temp
      }
      
      #Ceiling adjustments for patients marked as alive at the landmarks
      
      #30-day
      temp.ref <- which(ccc19x$der_days_fu < 30 &
                          ccc19x$der_deadbinary == 0 &
                          (ccc19x$mortality == 1|ccc19x$d30_vital_status == 0))
      ccc19x$der_days_fu[ccc19x$record_id %in% ccc19x$record_id[temp.ref]] <- 30
      
      #90-day
      temp.ref <- which(ccc19x$der_days_fu < 90 &
                          ccc19x$der_deadbinary == 0 &
                          (ccc19x$mortality_90 == 1|ccc19x$d90_vital_status == 0))
      ccc19x$der_days_fu[ccc19x$record_id %in% ccc19x$record_id[temp.ref]] <- 90
      
      #180-day
      temp.ref <- which(ccc19x$der_days_fu < 180 &
                          ccc19x$der_deadbinary == 0 &
                          (ccc19x$mortality_180 == 1|ccc19x$d180_vital_status == 0))
      ccc19x$der_days_fu[ccc19x$record_id %in% ccc19x$record_id[temp.ref]] <- 180
      
      #365-day
      temp.ref <- which(ccc19x$der_days_fu < 365 &
                          ccc19x$der_deadbinary == 0 &
                          ccc19x$d365_vital_status == 0)
      ccc19x$der_days_fu[ccc19x$record_id %in% ccc19x$record_id[temp.ref]] <- 365
      
      temp <- summary(ccc19x$der_days_fu[ccc19x$redcap_repeat_instrument == ''])
      
      temp.var.log <- data.frame(name = 'der_days_fu',
                                 timestamp = Sys.time(),
                                 values = paste(c(paste('Median:', temp[3], 'days'),
                                                  paste('IQR:', temp[2], '-', temp[5]),
                                                  paste('NA:', temp[7])), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
    }
    
    #########################
    #O18. Dead within 30 days
    #########################
    {
      #Default is the mortality variable
      # ccc19x$der_dead30 <- ccc19x$mortality
      # ccc19x$der_dead30[which(ccc19x$der_dead30 == 88)] <- NA
      
      #Default is not dead at 30 days
      ccc19x$der_dead30 <- 0
      
      temp.ref <- which(ccc19x$der_deadbinary == 1 & ccc19x$redcap_repeat_instrument == '')
      
      #1. Calculated time to death is <= 30 days
      temp.diff <- difftime(ccc19x$meta_righttime, ccc19x$meta_lefttime_lb, units = 'days')
      temp.ref2 <- which(temp.diff[temp.ref] <= 30)
      temp <- ccc19x$record_id[temp.ref[temp.ref2]]
      ccc19x$der_dead30[which(ccc19x$record_id %in% temp)] <- 1
      
      #2. 30-day mortality flag is set (baseline)
      temp.ref2 <- which(ccc19x$mortality[temp.ref] == 0)
      temp <- ccc19x$record_id[temp.ref[temp.ref2]]
      ccc19x$der_dead30[which(ccc19x$record_id %in% temp)] <- 1
      
      #3. 30-day mortality flag is set (follow-up)
      temp.ref2 <- which(ccc19x$d30_vital_status[temp.ref] == 1)
      temp <- ccc19x$record_id[temp.ref[temp.ref2]]
      ccc19x$der_dead30[which(ccc19x$record_id %in% temp)] <- 1
      
      #4. 30-day follow-up form is filled out as death
      temp <- ccc19x$record_id[which(ccc19x$fu_weeks == 30 & (
        ccc19x$fu_reason == 3 |
          ccc19x$covid_19_status_fu == 3 |
          ccc19x$current_status_fu == 9 ))]
      ccc19x$der_dead30[which(ccc19x$record_id %in% temp)] <- 1
      
      #5. Follow-up form filled out as other and timing <= 4 weeks
      temp <- ccc19x$record_id[which(ccc19x$timing_of_report_weeks <= 4 & (
        ccc19x$fu_reason == 3 |
          ccc19x$covid_19_status_fu == 3 |
          ccc19x$current_status_fu == 9 ))]
      ccc19x$der_dead30[which(ccc19x$record_id %in% temp)] <- 1
      
      #6. Days to death <= 30
      temp <- ccc19x$record_id[which(ccc19x$der_days_to_death_combined <= 30)]
      ccc19x$der_dead30[which(ccc19x$record_id %in% temp)] <- 1
      
      #7. Rescind status if days to death > 30
      temp <- ccc19x$record_id[which(ccc19x$der_days_to_death_combined > 30 & ccc19x$der_days_to_death_combined < 9999)]
      ccc19x$der_dead30[which(ccc19x$record_id %in% temp)] <- 0
      
      #8. Declare unknown if days to death cannot be calculated and mortality flag not set
      temp <- ccc19x$record_id[which(ccc19x$der_deadbinary == 1 & ccc19x$der_dead30 == 0 &
                                       (is.na(ccc19x$mortality)|ccc19x$mortality == 99) & 
                                       (is.na(ccc19x$d30_vital_status)|ccc19x$d30_vital_status == 99) & #Mortality flags
                                       (is.na(ccc19x$der_days_to_death_combined) | ccc19x$der_days_to_death_combined == 9999))]
      flag <- rep(T, length(temp))
      for(i in 1:length(temp))
      {
        temp.ref <- which(ccc19x$record_id == temp[i])
        temp2 <- c(ccc19x$hosp_los[temp.ref],
                   ccc19x$hosp_los_2[temp.ref],
                   ccc19x$hosp_los_fu[temp.ref],
                   ccc19x$hosp_los_fu_2[temp.ref],
                   ccc19x$icu_los[temp.ref],
                   ccc19x$icu_los_fu[temp.ref])
        temp2 <- temp2[!is.na(temp2)]
        temp3 <- ccc19x$mortality[temp.ref] == 1
        temp3 <- temp3[!is.na(temp3)]
        if(length(temp2) > 0)
        {
          temp2 <- sum(temp2)
          if(temp2 > 30) flag[i] <- F
        }
        if(length(temp3) > 0)
          if(any(temp3)) flag[i] <- F
      }
      temp <- temp[flag]
      
      ccc19x$der_dead30[which(ccc19x$record_id %in% temp)] <- 99
      
      #9. Recover some patients with unknown or missing days to death
      #Estimate days to death for patients with missing/unknown days and retrospective reporting (baseline form only)
      #Estimate as the maximum length of time possible based on the interval
      temp <- ccc19x$record_id[which(ccc19x$der_dead30 %in% c(0,99) &
                                       (ccc19x$der_days_to_death_combined == 9999|is.na(ccc19x$der_days_to_death_combined)) &
                                       ccc19x$current_status_retro == 3)]
      if(length(temp) > 0)
      {
        for(i in 1:length(temp))
        {
          temp.ref <- which(ccc19x$record_id == temp[i])
          temp2 <- ccc19x$covid_19_dx_interval[temp.ref]
          temp2 <- temp2[!is.na(temp2)]
          if(temp2 %in% 1:3) ccc19x$der_dead30[temp.ref] <- 1
        }
      }
      
      #10. Rescind unknown status if 90-day or 180-day follow-up form is filled out as death and is not the first f/u form
      temp <- ccc19x$record_id[which(ccc19x$fu_weeks %in% c(90,180,365) & 
                                       ccc19x$redcap_repeat_instance > 1 &
                                       (ccc19x$fu_reason == 3 |
                                          ccc19x$covid_19_status_fu == 3 |
                                          ccc19x$current_status_fu == 9 ) &
                                       ccc19x$der_dead30 == 99)]
      ccc19x$der_dead30[which(ccc19x$record_id %in% temp)] <- 0
      
      ccc19x$der_dead30 <- as.factor(ccc19x$der_dead30)
      
      temp <- summary(ccc19x$der_dead30[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_dead30',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
    }
    
    #Alternate variable that does NOT default to alive at 30 days
    {
      ccc19x$der_dead30a <- NA
      
      temp.ref <- which(ccc19x$der_deadbinary == 1 & ccc19x$redcap_repeat_instrument == '')
      
      #0. Median f/u time is > 30 days or alive on a follow-up form
      temp.ref2 <- which(ccc19x$der_dead30a[which(ccc19x$der_days_fu > 30)])
      temp <- ccc19x$record_id[temp.ref2]
      ccc19x$der_dead30a[which(ccc19x$record_id %in% temp)] <- 0
      
      #Alive on followup form
      temp.ref2 <- which((ccc19x$covid_19_status_fu %in% c('1', '1b', '2') | 
                            ccc19x$fu_reason %in% 1:2) &
                           (ccc19x$fu_weeks %in% c(30,90,180,365,2,3) | ccc19x$timing_of_report_weeks > 4))
      temp <- ccc19x$record_id[temp.ref2]
      ccc19x$der_dead30a[which(ccc19x$record_id %in% temp)] <- 0
      
      #1. Calculated time to death is <= 30 days
      temp.diff <- difftime(ccc19x$meta_righttime, ccc19x$meta_lefttime_lb, units = 'days')
      temp.ref2 <- which(temp.diff[temp.ref] <= 30)
      temp <- ccc19x$record_id[temp.ref[temp.ref2]]
      ccc19x$der_dead30a[which(ccc19x$record_id %in% temp)] <- 1
      
      #2. 30-day mortality flag is set (baseline)
      temp.ref2 <- which(ccc19x$mortality[temp.ref] == 0)
      temp <- ccc19x$record_id[temp.ref[temp.ref2]]
      ccc19x$der_dead30a[which(ccc19x$record_id %in% temp)] <- 1
      
      #2a. 30-day mortality flag is set to alive (baseline)
      temp.ref2 <- which(ccc19x$mortality == 1 & is.na(ccc19x$der_dead30a))
      temp <- ccc19x$record_id[temp.ref2]
      ccc19x$der_dead30a[which(ccc19x$record_id %in% temp)] <- 0
      
      #3. 30-day mortality flag is set (follow-up)
      temp.ref2 <- which(ccc19x$d30_vital_status[temp.ref] == 1)
      temp <- ccc19x$record_id[temp.ref[temp.ref2]]
      ccc19x$der_dead30a[which(ccc19x$record_id %in% temp)] <- 1
      
      #3a. 30-day mortality flag is set to alive (follow-up)
      temp.ref2 <- which(ccc19x$d30_vital_status == 0 & is.na(ccc19x$der_dead30a))
      temp <- ccc19x$record_id[temp.ref2]
      ccc19x$der_dead30a[which(ccc19x$record_id %in% temp)] <- 0
      
      #4. 30-day follow-up form is filled out as death
      temp <- ccc19x$record_id[which(ccc19x$fu_weeks == 30 & (
        ccc19x$fu_reason == 3 |
          ccc19x$covid_19_status_fu == 3 |
          ccc19x$current_status_fu == 9 ))]
      ccc19x$der_dead30a[which(ccc19x$record_id %in% temp)] <- 1
      
      #5. Follow-up form filled out as other and timing <= 4 weeks
      temp <- ccc19x$record_id[which(ccc19x$timing_of_report_weeks <= 4 & (
        ccc19x$fu_reason == 3 |
          ccc19x$covid_19_status_fu == 3 |
          ccc19x$current_status_fu == 9 ))]
      ccc19x$der_dead30a[which(ccc19x$record_id %in% temp)] <- 1
      
      #6. Days to death <= 30
      temp <- ccc19x$record_id[which(ccc19x$der_days_to_death_combined <= 30)]
      ccc19x$der_dead30a[which(ccc19x$record_id %in% temp)] <- 1
      
      #7. Rescind status if days to death > 30
      temp <- ccc19x$record_id[which(ccc19x$der_days_to_death_combined > 30 & ccc19x$der_days_to_death_combined < 9999)]
      ccc19x$der_dead30a[which(ccc19x$record_id %in% temp)] <- 0
      
      #8. Declare unknown if days to death cannot be calculated and mortality flag not set
      temp <- ccc19x$record_id[which(ccc19x$der_deadbinary == 1 & ccc19x$der_dead30a == 0 &
                                       (is.na(ccc19x$mortality)|ccc19x$mortality == 99) & 
                                       (is.na(ccc19x$d30_vital_status)|ccc19x$d30_vital_status == 99) & #Mortality flags
                                       (is.na(ccc19x$der_days_to_death_combined) | ccc19x$der_days_to_death_combined == 9999))]
      flag <- rep(T, length(temp))
      for(i in 1:length(temp))
      {
        temp.ref <- which(ccc19x$record_id == temp[i])
        temp2 <- c(ccc19x$hosp_los[temp.ref],
                   ccc19x$hosp_los_2[temp.ref],
                   ccc19x$hosp_los_fu[temp.ref],
                   ccc19x$hosp_los_fu_2[temp.ref],
                   ccc19x$icu_los[temp.ref],
                   ccc19x$icu_los_fu[temp.ref])
        temp2 <- temp2[!is.na(temp2)]
        temp3 <- ccc19x$mortality[temp.ref] == 1
        temp3 <- temp3[!is.na(temp3)]
        if(length(temp2) > 0)
        {
          temp2 <- sum(temp2)
          if(temp2 > 30) flag[i] <- F
        }
        if(length(temp3) > 0)
          if(any(temp3)) flag[i] <- F
      }
      temp <- temp[flag]
      
      ccc19x$der_dead30a[which(ccc19x$record_id %in% temp)] <- 99
      
      #9. Recover some patients with unknown or missing days to death
      #Estimate days to death for patients with missing/unknown days and retrospective reporting (baseline form only)
      #Estimate as the maximum length of time possible based on the interval
      temp <- ccc19x$record_id[which(ccc19x$der_dead30a %in% c(0,99) &
                                       (ccc19x$der_days_to_death == 9999|is.na(ccc19x$der_days_to_death)) &
                                       ccc19x$current_status_retro == 3)]
      if(length(temp) > 0)
      {
        for(i in 1:length(temp))
        {
          temp.ref <- which(ccc19x$record_id == temp[i])
          temp2 <- ccc19x$covid_19_dx_interval[temp.ref]
          temp2 <- temp2[!is.na(temp2)]
          if(temp2 %in% 1:3) ccc19x$der_dead30a[temp.ref] <- 1
        }
      }
      
      #10. Rescind unknown status if 90/180/365-day follow-up form is filled out as death and is not the first f/u form
      temp <- ccc19x$record_id[which(ccc19x$fu_weeks %in% c(90,180,365) & 
                                       ccc19x$redcap_repeat_instance > 1 &
                                       (ccc19x$fu_reason == 3 |
                                          ccc19x$covid_19_status_fu == 3 |
                                          ccc19x$current_status_fu == 9 ) &
                                       ccc19x$der_dead30a == 99)]
      ccc19x$der_dead30a[which(ccc19x$record_id %in% temp)] <- 0
      
      ccc19x$der_dead30a <- as.factor(ccc19x$der_dead30a)
      
      temp <- summary(ccc19x$der_dead30a[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_dead30a',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
    }
    
    #O24. Dead within 90 days, does NOT default to alive at 90 days
    ccc19x$der_dead90a <- NA
    
    temp.ref <- which(ccc19x$der_deadbinary == 1 & ccc19x$redcap_repeat_instrument == '')
    
    #0. Median f/u time is > 90 days or alive on a follow-up form
    temp.ref2 <- which(ccc19x$der_dead90a[which(ccc19x$der_days_fu > 90)])
    temp <- ccc19x$record_id[temp.ref2]
    ccc19x$der_dead90a[which(ccc19x$record_id %in% temp)] <- 0
    
    #Alive on followup form
    temp.ref2 <- which((ccc19x$covid_19_status_fu %in% c('1', '1b', '2') | 
                          ccc19x$fu_reason %in% 1:2) &
                         (ccc19x$fu_weeks %in% c(90,180,365,2,3) | ccc19x$timing_of_report_weeks > 13))
    temp <- ccc19x$record_id[temp.ref2]
    ccc19x$der_dead90a[which(ccc19x$record_id %in% temp)] <- 0
    
    #1. Calculated time to death is <= 90 days
    temp.diff <- difftime(ccc19x$meta_righttime, ccc19x$meta_lefttime_lb, units = 'days')
    temp.ref2 <- which(temp.diff[temp.ref] <= 90)
    temp <- ccc19x$record_id[temp.ref[temp.ref2]]
    ccc19x$der_dead90a[which(ccc19x$record_id %in% temp)] <- 1
    
    #2. 90-day mortality flag is set (baseline)
    temp.ref2 <- which(ccc19x$mortality_90[temp.ref] == 0)
    temp <- ccc19x$record_id[temp.ref[temp.ref2]]
    ccc19x$der_dead90a[which(ccc19x$record_id %in% temp)] <- 1
    
    #2a. 90-day mortality flag is set to alive (baseline)
    temp.ref2 <- which(ccc19x$mortality_90 == 1 & is.na(ccc19x$der_dead90a))
    temp <- ccc19x$record_id[temp.ref2]
    ccc19x$der_dead90a[which(ccc19x$record_id %in% temp)] <- 0
    
    #3. 90-day mortality flag is set (follow-up)
    temp.ref2 <- which(ccc19x$d90_vital_status[temp.ref] == 1)
    temp <- ccc19x$record_id[temp.ref[temp.ref2]]
    ccc19x$der_dead90a[which(ccc19x$record_id %in% temp)] <- 1
    
    #3a. 90-day mortality flag is set to alive (follow-up)
    temp.ref2 <- which(ccc19x$d90_vital_status == 0 & is.na(ccc19x$der_dead90a))
    temp <- ccc19x$record_id[temp.ref2]
    ccc19x$der_dead90a[which(ccc19x$record_id %in% temp)] <- 0
    
    #4. 90-day follow-up form is filled out as death
    temp <- ccc19x$record_id[which(ccc19x$fu_weeks == 90 & (
      ccc19x$fu_reason == 3 |
        ccc19x$covid_19_status_fu == 3 |
        ccc19x$current_status_fu == 9 ))]
    ccc19x$der_dead90a[which(ccc19x$record_id %in% temp)] <- 1
    
    #5. Follow-up form filled out as other and timing <= 13 weeks
    temp <- ccc19x$record_id[which(ccc19x$timing_of_report_weeks <= 13 & (
      ccc19x$fu_reason == 3 |
        ccc19x$covid_19_status_fu == 3 |
        ccc19x$current_status_fu == 9 ))]
    ccc19x$der_dead90a[which(ccc19x$record_id %in% temp)] <- 1
    
    #6. Days to death <= 90
    temp <- ccc19x$record_id[which(ccc19x$der_days_to_death_combined <= 90)]
    ccc19x$der_dead90a[which(ccc19x$record_id %in% temp)] <- 1
    
    #7. Rescind status if days to death > 90
    temp <- ccc19x$record_id[which(ccc19x$der_days_to_death_combined > 90 & ccc19x$der_days_to_death_combined < 9999)]
    ccc19x$der_dead90a[which(ccc19x$record_id %in% temp)] <- 0
    
    #8. Declare unknown if days to death cannot be calculated and mortality flag not set
    temp <- ccc19x$record_id[which(ccc19x$der_deadbinary == 1 & ccc19x$der_dead90a == 0 &
                                     (is.na(ccc19x$mortality_90)|ccc19x$mortality_90 == 99) & 
                                     (is.na(ccc19x$d90_vital_status)|ccc19x$d90_vital_status == 99) & #Mortality flags
                                     (is.na(ccc19x$der_days_to_death_combined) | ccc19x$der_days_to_death_combined == 9999))]
    flag <- rep(T, length(temp))
    for(i in 1:length(temp))
    {
      temp.ref <- which(ccc19x$record_id == temp[i])
      temp2 <- c(ccc19x$hosp_los[temp.ref],
                 ccc19x$hosp_los_2[temp.ref],
                 ccc19x$hosp_los_fu[temp.ref],
                 ccc19x$hosp_los_fu_2[temp.ref],
                 ccc19x$icu_los[temp.ref],
                 ccc19x$icu_los_fu[temp.ref])
      temp2 <- temp2[!is.na(temp2)]
      temp3 <- ccc19x$mortality_90[temp.ref] == 1
      temp3 <- temp3[!is.na(temp3)]
      if(length(temp2) > 0)
      {
        temp2 <- sum(temp2)
        if(temp2 > 90) flag[i] <- F
      }
      if(length(temp3) > 0)
        if(any(temp3)) flag[i] <- F
    }
    temp <- temp[flag]
    
    ccc19x$der_dead90a[which(ccc19x$record_id %in% temp)] <- 99
    
    #9. Recover some patients with unknown or missing days to death
    #Estimate days to death for patients with missing/unknown days and retrospective reporting (baseline form only)
    #Estimate as the maximum length of time possible based on the interval
    temp <- ccc19x$record_id[which(ccc19x$der_dead90a %in% c(0,99) &
                                     (ccc19x$der_days_to_death == 9999|is.na(ccc19x$der_days_to_death)) &
                                     ccc19x$current_status_retro == 3)]
    if(length(temp) > 0)
    {
      for(i in 1:length(temp))
      {
        temp.ref <- which(ccc19x$record_id == temp[i])
        temp2 <- ccc19x$covid_19_dx_interval[temp.ref]
        temp2 <- temp2[!is.na(temp2)]
        if(temp2 %in% 1:5) ccc19x$der_dead90a[temp.ref] <- 1
      }
    }
    
    #10. Rescind unknown status if 180-day or 365-day follow-up form is filled out as death and is not the first f/u form
    temp <- ccc19x$record_id[which(ccc19x$fu_weeks %in% c(180,365) & 
                                     ccc19x$redcap_repeat_instance > 1 &
                                     (ccc19x$fu_reason == 3 |
                                        ccc19x$covid_19_status_fu == 3 |
                                        ccc19x$current_status_fu == 9 ) &
                                     ccc19x$der_dead90a == 99)]
    ccc19x$der_dead90a[which(ccc19x$record_id %in% temp)] <- 0
    
    ccc19x$der_dead90a <- as.factor(ccc19x$der_dead90a)
    
    temp <- summary(ccc19x$der_dead90a[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_dead90a',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #O25. Dead within 180 days, default is missing
    ccc19x$der_dead180a <- NA
    
    temp.ref <- which(ccc19x$der_deadbinary == 1 & ccc19x$redcap_repeat_instrument == '')
    
    #0. Median f/u time is > 180 days or alive on a follow-up form
    temp.ref2 <- which(ccc19x$der_dead180a[which(ccc19x$der_days_fu > 180)])
    temp <- ccc19x$record_id[temp.ref2]
    ccc19x$der_dead180a[which(ccc19x$record_id %in% temp)] <- 0
    
    #Alive on followup form
    temp.ref2 <- which((ccc19x$covid_19_status_fu %in% c('1', '1b', '2') | 
                          ccc19x$fu_reason %in% 1:2) &
                         (ccc19x$fu_weeks %in% c(180,365,2,3) | ccc19x$timing_of_report_weeks > 26))
    temp <- ccc19x$record_id[temp.ref2]
    ccc19x$der_dead180a[which(ccc19x$record_id %in% temp)] <- 0
    
    #1. Calculated time to death is <= 180 days
    temp.diff <- difftime(ccc19x$meta_righttime, ccc19x$meta_lefttime_lb, units = 'days')
    temp.ref2 <- which(temp.diff[temp.ref] <= 180)
    temp <- ccc19x$record_id[temp.ref[temp.ref2]]
    ccc19x$der_dead180a[which(ccc19x$record_id %in% temp)] <- 1
    
    #2. 180-day mortality flag is set (baseline)
    temp.ref2 <- which(ccc19x$mortality_180[temp.ref] == 0)
    temp <- ccc19x$record_id[temp.ref[temp.ref2]]
    ccc19x$der_dead180a[which(ccc19x$record_id %in% temp)] <- 1
    
    #2a. 180-day mortality flag is set to alive (baseline)
    temp.ref2 <- which(ccc19x$mortality_180 == 1 & is.na(ccc19x$der_dead180a))
    temp <- ccc19x$record_id[temp.ref2]
    ccc19x$der_dead180a[which(ccc19x$record_id %in% temp)] <- 0
    
    #3. 180-day mortality flag is set (follow-up)
    temp.ref2 <- which(ccc19x$d180_vital_status[temp.ref] == 1)
    temp <- ccc19x$record_id[temp.ref[temp.ref2]]
    ccc19x$der_dead180a[which(ccc19x$record_id %in% temp)] <- 1
    
    #3a. 180-day mortality flag is set to alive (follow-up)
    temp.ref2 <- which(ccc19x$d180_vital_status == 0 & is.na(ccc19x$der_dead180a))
    temp <- ccc19x$record_id[temp.ref2]
    ccc19x$der_dead180a[which(ccc19x$record_id %in% temp)] <- 0
    
    #4. 180-day follow-up form is filled out as death
    temp <- ccc19x$record_id[which(ccc19x$fu_weeks == 180 & (
      ccc19x$fu_reason == 3 |
        ccc19x$covid_19_status_fu == 3 |
        ccc19x$current_status_fu == 9 ))]
    ccc19x$der_dead180a[which(ccc19x$record_id %in% temp)] <- 1
    
    #5. Follow-up form filled out as other and timing <= 26 weeks
    temp <- ccc19x$record_id[which(ccc19x$timing_of_report_weeks <= 26 & (
      ccc19x$fu_reason == 3 |
        ccc19x$covid_19_status_fu == 3 |
        ccc19x$current_status_fu == 9 ))]
    ccc19x$der_dead180a[which(ccc19x$record_id %in% temp)] <- 1
    
    #6. Days to death <= 180
    temp <- ccc19x$record_id[which(ccc19x$der_days_to_death_combined <= 180)]
    ccc19x$der_dead180a[which(ccc19x$record_id %in% temp)] <- 1
    
    #7. Rescind status if days to death > 180
    temp <- ccc19x$record_id[which(ccc19x$der_days_to_death_combined > 180 & ccc19x$der_days_to_death_combined < 9999)]
    ccc19x$der_dead180a[which(ccc19x$record_id %in% temp)] <- 0
    
    #8. Declare unknown if days to death cannot be calculated and mortality flag not set
    temp <- ccc19x$record_id[which(ccc19x$der_deadbinary == 1 & ccc19x$der_dead180a == 0 &
                                     (is.na(ccc19x$mortality_180)|ccc19x$mortality_180 == 99) & 
                                     (is.na(ccc19x$d180_vital_status)|ccc19x$d180_vital_status == 99) & #Mortality flags
                                     (is.na(ccc19x$der_days_to_death_combined) | ccc19x$der_days_to_death_combined == 9999))]
    flag <- rep(T, length(temp))
    for(i in 1:length(temp))
    {
      temp.ref <- which(ccc19x$record_id == temp[i])
      temp2 <- c(ccc19x$hosp_los[temp.ref],
                 ccc19x$hosp_los_2[temp.ref],
                 ccc19x$hosp_los_fu[temp.ref],
                 ccc19x$hosp_los_fu_2[temp.ref],
                 ccc19x$icu_los[temp.ref],
                 ccc19x$icu_los_fu[temp.ref])
      temp2 <- temp2[!is.na(temp2)]
      temp3 <- ccc19x$mortality_180[temp.ref] == 1
      temp3 <- temp3[!is.na(temp3)]
      if(length(temp2) > 0)
      {
        temp2 <- sum(temp2)
        if(temp2 > 180) flag[i] <- F
      }
      if(length(temp3) > 0)
        if(any(temp3)) flag[i] <- F
    }
    temp <- temp[flag]
    
    ccc19x$der_dead180a[which(ccc19x$record_id %in% temp)] <- 99
    
    #9. Recover some patients with unknown or missing days to death
    #Estimate days to death for patients with missing/unknown days and retrospective reporting (baseline form only)
    #Estimate as the maximum length of time possible based on the interval
    temp <- ccc19x$record_id[which(ccc19x$der_dead180a %in% c(0,99) &
                                     (ccc19x$der_days_to_death == 9999|is.na(ccc19x$der_days_to_death)) &
                                     ccc19x$current_status_retro == 3)]
    if(length(temp) > 0)
    {
      for(i in 1:length(temp))
      {
        temp.ref <- which(ccc19x$record_id == temp[i])
        temp2 <- ccc19x$covid_19_dx_interval[temp.ref]
        temp2 <- temp2[!is.na(temp2)]
        if(temp2 %in% 1:6) ccc19x$der_dead180a[temp.ref] <- 1
      }
    }
    
    #10. Rescind unknown status if 365-day follow-up form is filled out as death and is not the first f/u form
    temp <- ccc19x$record_id[which(ccc19x$fu_weeks %in% c(365) & 
                                     ccc19x$redcap_repeat_instance > 1 &
                                     (ccc19x$fu_reason == 3 |
                                        ccc19x$covid_19_status_fu == 3 |
                                        ccc19x$current_status_fu == 9 ) &
                                     ccc19x$der_dead180a == 99)]
    ccc19x$der_dead180a[which(ccc19x$record_id %in% temp)] <- 0
    
    ccc19x$der_dead180a <- as.factor(ccc19x$der_dead180a)
    
    temp <- summary(ccc19x$der_dead180a[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_dead180a',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #O25. Dead within 365 days, default is missing
    ccc19x$der_dead365 <- NA
    
    temp.ref <- which(ccc19x$der_deadbinary == 1 & ccc19x$redcap_repeat_instrument == '')
    
    #0. Median f/u time is > 365 days or alive on a follow-up form
    temp.ref2 <- which(ccc19x$der_dead365[which(ccc19x$der_days_fu > 365)])
    temp <- ccc19x$record_id[temp.ref2]
    ccc19x$der_dead365[which(ccc19x$record_id %in% temp)] <- 0
    
    #Alive on followup form
    temp.ref2 <- which((ccc19x$covid_19_status_fu %in% c('1', '1b', '2') | 
                          ccc19x$fu_reason %in% 1:2) &
                         (ccc19x$fu_weeks %in% c(365,2,3) | ccc19x$timing_of_report_weeks > 26))
    temp <- ccc19x$record_id[temp.ref2]
    ccc19x$der_dead365[which(ccc19x$record_id %in% temp)] <- 0
    
    #1. Calculated time to death is <= 365 days
    temp.diff <- difftime(ccc19x$meta_righttime, ccc19x$meta_lefttime_lb, units = 'days')
    temp.ref2 <- which(temp.diff[temp.ref] <= 365)
    temp <- ccc19x$record_id[temp.ref[temp.ref2]]
    ccc19x$der_dead365[which(ccc19x$record_id %in% temp)] <- 1
    
    #3. 365-day mortality flag is set (follow-up)
    temp.ref2 <- which(ccc19x$d365_vital_status[temp.ref] == 1)
    temp <- ccc19x$record_id[temp.ref[temp.ref2]]
    ccc19x$der_dead365[which(ccc19x$record_id %in% temp)] <- 1
    
    #3a. 365-day mortality flag is set to alive (follow-up)
    temp.ref2 <- which(ccc19x$d365_vital_status == 0 & is.na(ccc19x$der_dead365))
    temp <- ccc19x$record_id[temp.ref2]
    ccc19x$der_dead365[which(ccc19x$record_id %in% temp)] <- 0
    
    #4. 365-day follow-up form is filled out as death
    temp <- ccc19x$record_id[which(ccc19x$fu_weeks == 365 & (
      ccc19x$fu_reason == 3 |
        ccc19x$covid_19_status_fu == 3 |
        ccc19x$current_status_fu == 9 ))]
    ccc19x$der_dead365[which(ccc19x$record_id %in% temp)] <- 1
    
    #5. Follow-up form filled out as other and timing <= 52 weeks
    temp <- ccc19x$record_id[which(ccc19x$timing_of_report_weeks <= 52 & (
      ccc19x$fu_reason == 3 |
        ccc19x$covid_19_status_fu == 3 |
        ccc19x$current_status_fu == 9 ))]
    ccc19x$der_dead365[which(ccc19x$record_id %in% temp)] <- 1
    
    #6. Days to death <= 365
    temp <- ccc19x$record_id[which(ccc19x$der_days_to_death_combined <= 365)]
    ccc19x$der_dead365[which(ccc19x$record_id %in% temp)] <- 1
    
    #7. Rescind status if days to death > 365
    temp <- ccc19x$record_id[which(ccc19x$der_days_to_death_combined > 365 & ccc19x$der_days_to_death_combined < 9999)]
    ccc19x$der_dead365[which(ccc19x$record_id %in% temp)] <- 0
    
    #8. Declare unknown if days to death cannot be calculated and mortality flag not set
    temp <- ccc19x$record_id[which(ccc19x$der_deadbinary == 1 & ccc19x$der_dead365 == 0 &
                                     (is.na(ccc19x$d365_vital_status)|ccc19x$d365_vital_status == 99) & #Mortality flags
                                     (is.na(ccc19x$der_days_to_death_combined) | ccc19x$der_days_to_death_combined == 9999))]
    flag <- rep(T, length(temp))
    for(i in 1:length(temp))
    {
      temp.ref <- which(ccc19x$record_id == temp[i])
      temp2 <- c(ccc19x$hosp_los[temp.ref],
                 ccc19x$hosp_los_2[temp.ref],
                 ccc19x$hosp_los_fu[temp.ref],
                 ccc19x$hosp_los_fu_2[temp.ref],
                 ccc19x$icu_los[temp.ref],
                 ccc19x$icu_los_fu[temp.ref])
      temp2 <- temp2[!is.na(temp2)]
      if(length(temp2) > 0)
      {
        temp2 <- sum(temp2)
        if(temp2 > 365) flag[i] <- F
      }
    }
    temp <- temp[flag]
    
    ccc19x$der_dead365[which(ccc19x$record_id %in% temp)] <- 99
    
    #9. Recover some patients with unknown or missing days to death
    #Estimate days to death for patients with missing/unknown days and retrospective reporting (baseline form only)
    #Estimate as the maximum length of time possible based on the interval
    temp <- ccc19x$record_id[which(ccc19x$der_dead365 %in% c(0,99) &
                                     (ccc19x$der_days_to_death == 9999|is.na(ccc19x$der_days_to_death)) &
                                     ccc19x$current_status_retro == 3)]
    if(length(temp) > 0)
    {
      for(i in 1:length(temp))
      {
        temp.ref <- which(ccc19x$record_id == temp[i])
        temp2 <- ccc19x$covid_19_dx_interval[temp.ref]
        temp2 <- temp2[!is.na(temp2)]
        if(temp2 %in% 1:9) ccc19x$der_dead365[temp.ref] <- 1
      }
    }
    
    #10. Rescind unknown status if 2-year follow-up form is filled out as death and is not the first f/u form
    temp <- ccc19x$record_id[which(ccc19x$fu_weeks %in% c(2) & 
                                     ccc19x$redcap_repeat_instance > 1 &
                                     (ccc19x$fu_reason == 3 |
                                        ccc19x$covid_19_status_fu == 3 |
                                        ccc19x$current_status_fu == 9 ) &
                                     ccc19x$der_dead365 == 99)]
    ccc19x$der_dead365[which(ccc19x$record_id %in% temp)] <- 0
    
    ccc19x$der_dead365 <- as.factor(ccc19x$der_dead365)
    
    temp <- summary(ccc19x$der_dead365[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_dead365',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #O9. Composite outcome - hospitalization (ever/never), or death within 30 days
    ccc19x$der_composite_hosp_death <- NA
    
    #Present
    ccc19x$der_composite_hosp_death[which(ccc19x$der_dead30 == 1)] <- 1
    ccc19x$der_composite_hosp_death[which(ccc19x$der_hosp == 1)] <- 1
    
    #Absent (requires all 2 derived variables to be absent, and not meeting another criteria)
    ccc19x$der_composite_hosp_death[which(ccc19x$der_dead30 == 0 &
                              ccc19x$der_hosp == 0 &
                              is.na(ccc19x$der_composite_hosp_death))] <- 0
    
    #Unknown
    ccc19x$der_composite_hosp_death[which((ccc19x$der_dead30 == 99 |ccc19x$der_hosp == 99) &
                                      is.na(ccc19x$der_composite_hosp_death))] <- 99
    
    #Factor
    ccc19x$der_composite_hosp_death <- as.factor(ccc19x$der_composite_hosp_death)
    
    temp <- summary(ccc19x$der_composite_hosp_death[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_composite_hosp_death',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #O10. Composite outcome - hospitalization, ICU, or death (all ever/never)
    ccc19x$der_composite_ICU_mv_death <- NA
    
    #Present
    ccc19x$der_composite_ICU_mv_death[which(ccc19x$der_deadbinary == 1)] <- 1
    ccc19x$der_composite_ICU_mv_death[which(ccc19x$der_ICU == 1)] <- 1
    ccc19x$der_composite_ICU_mv_death[which(ccc19x$der_mv == 1)] <- 1
    
    #Absent (requires all 2 derived variables to be absent, and not meeting another criteria)
    ccc19x$der_composite_ICU_mv_death[which(ccc19x$der_deadbinary == 0 &
                                              ccc19x$der_ICU == 0 &
                                              ccc19x$der_mv == 0 &
                                              is.na(ccc19x$der_composite_ICU_mv_death))] <- 0
    
    #Unknown
    ccc19x$der_composite_ICU_mv_death[which((ccc19x$der_deadbinary == 99 |ccc19x$der_ICU == 99|ccc19x$der_mv == 99) &
                                              is.na(ccc19x$der_composite_ICU_mv_death))] <- 99
    
    #Factor
    ccc19x$der_composite_ICU_mv_death <- as.factor(ccc19x$der_composite_ICU_mv_death)
    
    temp <- summary(ccc19x$der_composite_ICU_mv_death[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_composite_ICU_mv_death',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #T9 Month and year of diagnosis, accounting for interval bounds
    temp.ref <- which(ccc19x$redcap_repeat_instrument == '')
    
    #First, extract month and year from the POSIXlt objects
    x1 <- months(ccc19x$meta_lefttime_lb[temp.ref], abbreviate = T)
    xm <- months(ccc19x$meta_lefttime_med[temp.ref], abbreviate = T)
    x2 <- months(ccc19x$meta_lefttime_ub[temp.ref], abbreviate = T)
    y1 <- format(ccc19x$meta_lefttime_lb[temp.ref], format = '%Y')
    ym <- format(ccc19x$meta_lefttime_med[temp.ref], format = '%Y')
    y2 <- format(ccc19x$meta_lefttime_ub[temp.ref], format = '%Y')
    
    #Format months for table ordering later
    x1[x1 == 'Jan'] <- '01 (Jan)'
    x1[x1 == 'Feb'] <- '02 (Feb)'
    x1[x1 == 'Mar'] <- '03 (Mar)'
    x1[x1 == 'Apr'] <- '04 (Apr)'
    x1[x1 == 'May'] <- '05 (May)'
    x1[x1 == 'Jun'] <- '06 (Jun)'
    x1[x1 == 'Jul'] <- '07 (Jul)'
    x1[x1 == 'Aug'] <- '08 (Aug)'
    x1[x1 == 'Sep'] <- '09 (Sep)'
    x1[x1 == 'Oct'] <- '10 (Oct)'
    x1[x1 == 'Nov'] <- '11 (Nov)'
    x1[x1 == 'Dec'] <- '12 (Dec)'
    
    xm[xm == 'Jan'] <- '01 (Jan)'
    xm[xm == 'Feb'] <- '02 (Feb)'
    xm[xm == 'Mar'] <- '03 (Mar)'
    xm[xm == 'Apr'] <- '04 (Apr)'
    xm[xm == 'May'] <- '05 (May)'
    xm[xm == 'Jun'] <- '06 (Jun)'
    xm[xm == 'Jul'] <- '07 (Jul)'
    xm[xm == 'Aug'] <- '08 (Aug)'
    xm[xm == 'Sep'] <- '09 (Sep)'
    xm[xm == 'Oct'] <- '10 (Oct)'
    xm[xm == 'Nov'] <- '11 (Nov)'
    xm[xm == 'Dec'] <- '12 (Dec)'
    
    x2[x2 == 'Jan'] <- '01 (Jan)'
    x2[x2 == 'Feb'] <- '02 (Feb)'
    x2[x2 == 'Mar'] <- '03 (Mar)'
    x2[x2 == 'Apr'] <- '04 (Apr)'
    x2[x2 == 'May'] <- '05 (May)'
    x2[x2 == 'Jun'] <- '06 (Jun)'
    x2[x2 == 'Jul'] <- '07 (Jul)'
    x2[x2 == 'Aug'] <- '08 (Aug)'
    x2[x2 == 'Sep'] <- '09 (Sep)'
    x2[x2 == 'Oct'] <- '10 (Oct)'
    x2[x2 == 'Nov'] <- '11 (Nov)'
    x2[x2 == 'Dec'] <- '12 (Dec)'
    
    #T09 Month and year of diagnosis, accounting for interval bounds (missing if it crosses bounds)
    ccc19x$der_month_dx <- NA
    temp.ref2 <- which(x1 == x2 & y1 == y2 & y1 != 2099)
    
    ccc19x$der_month_dx[temp.ref[temp.ref2]] <- paste(y1[temp.ref2], '-', x1[temp.ref2], sep = '')
    ccc19x$der_month_dx <- factor(ccc19x$der_month_dx)
    summary(ccc19x$der_month_dx[ccc19x$redcap_repeat_instrument == ''])
    
    #T09a Month and year of diagnosis, using the middle of the interval as the anchor
    ccc19x$der_month_mid_dx <- NA
    ccc19x$der_month_mid_dx[temp.ref] <- paste(ym, '-', xm, sep = '')
    ccc19x$der_month_mid_dx <- factor(ccc19x$der_month_mid_dx)
    summary(ccc19x$der_month_mid_dx[ccc19x$redcap_repeat_instrument == ''])
    
    #T09b Month and year of diagnosis, using the right side of the interval as the anchor
    ccc19x$der_month_rt_dx <- NA
    ccc19x$der_month_rt_dx[temp.ref] <- paste(y2, '-', x2, sep = '')
    ccc19x$der_month_rt_dx <- factor(ccc19x$der_month_rt_dx)
    summary(ccc19x$der_month_rt_dx[ccc19x$redcap_repeat_instrument == ''])
    
    #T10 Quarter and year of diagnosis, accounting for interval bounds
    ccc19x$der_quarter_dx <- NA
    
    #Q1
    temp.ref2 <- which(x1 %in% c('01 (Jan)', '02 (Feb)', '03 (Mar)') & 
                         x2 %in% c('01 (Jan)', '02 (Feb)', '03 (Mar)') & y1 == y2 & y1 != 2099)
    ccc19x$der_quarter_dx[temp.ref[temp.ref2]] <- paste('Q1', y1[temp.ref2])
    
    #Q2
    temp.ref2 <- which(x1 %in% c('04 (Apr)', '05 (May)', '06 (Jun)') & 
                         x2 %in% c('04 (Apr)', '05 (May)', '06 (Jun)') & y1 == y2 & y1 != 2099)
    ccc19x$der_quarter_dx[temp.ref[temp.ref2]] <- paste('Q2', y1[temp.ref2])
    
    #Q3
    temp.ref2 <- which(x1 %in% c('07 (Jul)', '08 (Aug)', '09 (Sep)') & 
                         x2 %in% c('07 (Jul)', '08 (Aug)', '09 (Sep)') & y1 == y2 & y1 != 2099)
    ccc19x$der_quarter_dx[temp.ref[temp.ref2]] <- paste('Q3', y1[temp.ref2])
    
    #Q4
    temp.ref2 <- which(x1 %in% c('10 (Oct)', '11 (Nov)', '12 (Dec)') & 
                         x2 %in% c('10 (Oct)', '11 (Nov)', '12 (Dec)') & y1 == y2 & y1 != 2099)
    ccc19x$der_quarter_dx[temp.ref[temp.ref2]] <- paste('Q4', y1[temp.ref2])
    
    ccc19x$der_quarter_dx <- factor(ccc19x$der_quarter_dx)
    summary(ccc19x$der_quarter_dx[ccc19x$redcap_repeat_instrument == ''])
    
    #T10a Quarter and year of diagnosis, using the middle of the interval as the anchor
    ccc19x$der_quarter_mid_dx <- NA
    
    #Q1
    temp.ref2 <- which(xm %in% c('01 (Jan)', '02 (Feb)', '03 (Mar)') & ym != 2099)
    ccc19x$der_quarter_mid_dx[temp.ref[temp.ref2]] <- paste('Q1', ym[temp.ref2])
    
    #Q2
    temp.ref2 <- which(xm %in% c('04 (Apr)', '05 (May)', '06 (Jun)') & ym != 2099)
    ccc19x$der_quarter_mid_dx[temp.ref[temp.ref2]] <- paste('Q2', ym[temp.ref2])
    
    #Q3
    temp.ref2 <- which(xm %in% c('07 (Jul)', '08 (Aug)', '09 (Sep)') & ym != 2099)
    ccc19x$der_quarter_mid_dx[temp.ref[temp.ref2]] <- paste('Q3', ym[temp.ref2])
    
    #Q4
    temp.ref2 <- which(xm %in% c('10 (Oct)', '11 (Nov)', '12 (Dec)') & ym != 2099)
    
    ccc19x$der_quarter_mid_dx[temp.ref[temp.ref2]] <- paste('Q4', ym[temp.ref2])
    ccc19x$der_quarter_mid_dx <- factor(ccc19x$der_quarter_mid_dx)
    summary(ccc19x$der_quarter_mid_dx[ccc19x$redcap_repeat_instrument == ''])
    
    #T10b Quarter and year of diagnosis, using the right side of the interval as the anchor
    ccc19x$der_quarter_rt_dx <- NA
    
    #Q1
    temp.ref2 <- which(x2 %in% c('01 (Jan)', '02 (Feb)', '03 (Mar)') & y2 != 2099)
    ccc19x$der_quarter_rt_dx[temp.ref[temp.ref2]] <- paste('Q1', y2[temp.ref2])
    
    #Q2
    temp.ref2 <- which(x2 %in% c('04 (Apr)', '05 (May)', '06 (Jun)') & y2 != 2099)
    ccc19x$der_quarter_rt_dx[temp.ref[temp.ref2]] <- paste('Q2', y2[temp.ref2])
    
    #Q3
    temp.ref2 <- which(x2 %in% c('07 (Jul)', '08 (Aug)', '09 (Sep)') & y2 != 2099)
    ccc19x$der_quarter_rt_dx[temp.ref[temp.ref2]] <- paste('Q3', y2[temp.ref2])
    
    #Q4
    temp.ref2 <- which(x2 %in% c('10 (Oct)', '11 (Nov)', '12 (Dec)') & y2 != 2099)
    
    ccc19x$der_quarter_rt_dx[temp.ref[temp.ref2]] <- paste('Q4', y2[temp.ref2])
    ccc19x$der_quarter_rt_dx <- factor(ccc19x$der_quarter_rt_dx)
    summary(ccc19x$der_quarter_rt_dx[ccc19x$redcap_repeat_instrument == ''])
    
    #T11a Trimester (actually "quadrimester") and year of diagnosis, using the middle of the interval as the anchor
    ccc19x$der_tri_mid_dx <- NA
    
    #T1
    temp.ref2 <- which(xm %in% c('01 (Jan)', '02 (Feb)', '03 (Mar)', '04 (Apr)') & ym != 2099)
    ccc19x$der_tri_mid_dx[temp.ref[temp.ref2]] <- paste('T1', ym[temp.ref2])
    
    #T2
    temp.ref2 <- which(xm %in% c('05 (May)', '06 (Jun)', '07 (Jul)', '08 (Aug)') & ym != 2099)
    ccc19x$der_tri_mid_dx[temp.ref[temp.ref2]] <- paste('T2', ym[temp.ref2])
    
    #T3
    temp.ref2 <- which(xm %in% c('09 (Sep)', '10 (Oct)', '11 (Nov)', '12 (Dec)') & ym != 2099)
    ccc19x$der_tri_mid_dx[temp.ref[temp.ref2]] <- paste('T3', ym[temp.ref2])
    
    ccc19x$der_tri_mid_dx <- factor(ccc19x$der_tri_mid_dx)
    
    temp <- summary(ccc19x$der_tri_mid_dx[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_tri_mid_dx',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #T11b Trimester (actually "quadrimester") and year of diagnosis, using the right side of the interval as the anchor
    ccc19x$der_tri_rt_dx <- NA
    
    #T1
    temp.ref2 <- which(x2 %in% c('01 (Jan)', '02 (Feb)', '03 (Mar)', '04 (Apr)') & y2 != 2099)
    ccc19x$der_tri_rt_dx[temp.ref[temp.ref2]] <- paste('T1', y2[temp.ref2])
    
    #T2
    temp.ref2 <- which(x2 %in% c('05 (May)', '06 (Jun)', '07 (Jul)', '08 (Aug)') & y2 != 2099)
    ccc19x$der_tri_rt_dx[temp.ref[temp.ref2]] <- paste('T2', y2[temp.ref2])
    
    #T3
    temp.ref2 <- which(x2 %in% c('09 (Sep)', '10 (Oct)', '11 (Nov)', '12 (Dec)') & y2 != 2099)
    ccc19x$der_tri_rt_dx[temp.ref[temp.ref2]] <- paste('T3', y2[temp.ref2])
    
    ccc19x$der_tri_rt_dx <- factor(ccc19x$der_tri_rt_dx)
    
    temp <- summary(ccc19x$der_tri_rt_dx[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_tri_rt_dx',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #T12 Hemi-year of diagnosis, accounting for interval bounds
    ccc19x$der_hemi_dx <- NA
    
    #H1
    temp.ref2 <- which(x1 %in% c('01 (Jan)', '02 (Feb)', '03 (Mar)', '04 (Apr)', '05 (May)', '06 (Jun)') & 
                         x2 %in% c('01 (Jan)', '02 (Feb)', '03 (Mar)', '04 (Apr)', '05 (May)', '06 (Jun)') & y1 == y2 & y1 != 2099)
    ccc19x$der_hemi_dx[temp.ref[temp.ref2]] <- paste('H1', y1[temp.ref2])
    
    #H2
    temp.ref2 <- which(x1 %in% c('07 (Jul)', '08 (Aug)', '09 (Sep)', '10 (Oct)', '11 (Nov)', '12 (Dec)') & 
                         x2 %in% c('07 (Jul)', '08 (Aug)', '09 (Sep)', '10 (Oct)', '11 (Nov)', '12 (Dec)') & y1 == y2 & y1 != 2099)
    ccc19x$der_hemi_dx[temp.ref[temp.ref2]] <- paste('H2', y1[temp.ref2])
    
    ccc19x$der_hemi_dx <- factor(ccc19x$der_hemi_dx)
    summary(ccc19x$der_hemi_dx[ccc19x$redcap_repeat_instrument == ''])
    
    #T14 Early ICU admission (within 48 hours)
    ccc19x$der_early_icu <- NA
    
    #Baseline
    ccc19x$der_early_icu[which(ccc19x$hosp_status == 3)] <- 1
    ccc19x$der_early_icu[which(ccc19x$hosp_status == 2 & ccc19x$hosp_los_2 <= 2)] <- 1
    ccc19x$der_early_icu[which(ccc19x$hosp_status %in% 0:1)] <- 0
    ccc19x$der_early_icu[which(ccc19x$hosp_status == 2 & ccc19x$hosp_los_2 > 2)] <- 0
    
    #Follow-up if baseline was outpatient
    temp <- ccc19x$record_id[which(ccc19x$hosp_status == 0)]
    temp <- temp[which(ccc19x$record_id %in% temp & ccc19x$redcap_repeat_instrument == 'followup')]
    for(i in 1:length(temp))
    {
      temp.ref <- which(ccc19x$record_id == temp[i] & ccc19x$redcap_repeat_instrument == 'followup')
      query <- any(ccc19x$hosp_status_fu[temp.ref] == 3 |
                     (ccc19x$hosp_status_fu[temp.ref] == 2 & ccc19x$hosp_los_fu_2[temp.ref] <= 2))
      if(is.na(query)) query <- F
      if(query) ccc19x$der_early_icu[temp.ref] <- 1
      query <- all(ccc19x$hosp_status_fu[temp.ref] %in% 0:1 |
                     (ccc19x$hosp_status_fu[temp.ref] == 2 & ccc19x$hosp_los_fu_2[temp.ref] > 2))
      if(is.na(query)) query <- F
      if(query) ccc19x$der_early_icu[temp.ref] <- 0
    }
    
    #Unknown
    
    #Baseline
    ccc19x$der_early_icu[which(ccc19x$hosp_status == 99 & is.na(ccc19x$der_early_icu))] <- 99
    
    #Follow-up
    ccc19x$der_early_icu[which(ccc19x$hosp_status_fu == 99 & is.na(ccc19x$der_early_icu))] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_early_icu[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_early_icu[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_early_icu[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_early_icu[temp.ref] <- 0
      }
    }
    
    ccc19x$der_early_icu <- factor(ccc19x$der_early_icu)
    summary(ccc19x$der_early_icu[ccc19x$redcap_repeat_instrument == ''])
    
    #############################
    #T13. Cancer treatment timing - do not allow overwrite
    #############################
    ccc19x$der_cancer_tx_timing <- NA
    
    #Less than 2 weeks
    ccc19x$der_cancer_tx_timing[which(ccc19x$recent_treatment == 1)] <- 1
    
    #2-4 weeks
    ccc19x$der_cancer_tx_timing[which(ccc19x$recent_treatment == 2 & is.na(ccc19x$der_cancer_tx_timing))] <- 2
    
    #1-3 months, including the hx_treatment = within 3 months
    ccc19x$der_cancer_tx_timing[which((ccc19x$hx_treatment == 1|ccc19x$recent_treatment == 3) & 
                                        is.na(ccc19x$der_cancer_tx_timing))] <- 3
    
    #More than 3 months (referent)
    ccc19x$der_cancer_tx_timing[which((ccc19x$hx_treatment %in% 2:3|ccc19x$recent_treatment == 88) & 
                                  is.na(ccc19x$der_cancer_tx_timing))] <- 0
    
    #Never, including treatment starting AFTER the COVID-19 diagnosis
    ccc19x$der_cancer_tx_timing[which((ccc19x$hx_treatment == 88|ccc19x$recent_treatment == 98) & 
                                  is.na(ccc19x$der_cancer_tx_timing))] <- 88
    
    #Unknown
    ccc19x$der_cancer_tx_timing[which((ccc19x$hx_treatment == 99|ccc19x$recent_treatment == 99) & 
                                        is.na(ccc19x$der_cancer_tx_timing))] <- 99
    ccc19x$der_cancer_tx_timing[which(ccc19x$on_treatment == 99 & 
                                  is.na(ccc19x$der_cancer_tx_timing))] <- 99
    
    ccc19x$der_cancer_tx_timing <- as.factor(ccc19x$der_cancer_tx_timing)
    summary(ccc19x$der_cancer_tx_timing[ccc19x$redcap_repeat_instrument == ''])
    
    #############################
    #T13a. Cancer treatment timing - collapse 0-2 and 2-4 weeks
    #############################
    ccc19x$der_cancer_tx_timing_v2 <- NA
    
    #0-4 weeks
    ccc19x$der_cancer_tx_timing_v2[which(ccc19x$recent_treatment %in% 1:2)] <- 1
    
    #1-3 months, including the hx_treatment = within 3 months
    ccc19x$der_cancer_tx_timing_v2[which((ccc19x$hx_treatment == 1|ccc19x$recent_treatment == 3) & 
                                           is.na(ccc19x$der_cancer_tx_timing_v2))] <- 2
    
    #More than 3 months (referent)
    ccc19x$der_cancer_tx_timing_v2[which((ccc19x$hx_treatment %in% 2:3|ccc19x$recent_treatment == 88) & 
                                           is.na(ccc19x$der_cancer_tx_timing_v2))] <- 0
    
    #Never, including treatment starting AFTER the COVID-19 diagnosis
    ccc19x$der_cancer_tx_timing_v2[which((ccc19x$hx_treatment == 88|ccc19x$recent_treatment == 98) & 
                                           is.na(ccc19x$der_cancer_tx_timing_v2))] <- 88
    
    #Unknown
    ccc19x$der_cancer_tx_timing_v2[which((ccc19x$hx_treatment == 99|ccc19x$recent_treatment == 99) & 
                                           is.na(ccc19x$der_cancer_tx_timing_v2))] <- 99
    ccc19x$der_cancer_tx_timing_v2[which(ccc19x$on_treatment == 99 & 
                                        is.na(ccc19x$der_cancer_tx_timing_v2))] <- 99
    
    ccc19x$der_cancer_tx_timing_v2 <- as.factor(ccc19x$der_cancer_tx_timing_v2)
    
    temp <- summary(ccc19x$der_cancer_tx_timing_v2[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_cancer_tx_timing_v2',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #############################
    #T19. Systemic cancer treatment timing - collapse 0-2 and 2-4 weeks
    #############################
    ccc19x$der_systemic_cancer_tx_timing <- NA
    
    #Modality known to be systemic
    temp.ref <- which(ccc19x$treatment_modality___685 == 1|
                        ccc19x$treatment_modality___694 == 1|
                        ccc19x$treatment_modality___58229 == 1|
                        ccc19x$treatment_modality___691 == 1|
                        ccc19x$treatment_modality___45186 == 1)
    
    ccc19x$der_systemic_cancer_tx_timing[temp.ref] <- ccc19x$der_cancer_tx_timing_v2[temp.ref]
    
    #Known to have never received treatment (including after COVID-19 for the first time)
    temp.ref <- which(ccc19x$der_cancer_tx_timing_v2 == 88)
    ccc19x$der_systemic_cancer_tx_timing[temp.ref] <- 88
    
    #Unknown
    temp.ref <- which(ccc19x$der_cancer_tx_timing_v2 == 99)
    ccc19x$der_systemic_cancer_tx_timing[temp.ref] <- 99
    
    ccc19x$der_systemic_cancer_tx_timing <- as.factor(ccc19x$der_systemic_cancer_tx_timing)
    
    temp <- summary(ccc19x$der_systemic_cancer_tx_timing[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_systemic_cancer_tx_timing',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #############################
    #T19a. Systemic cancer treatment timing not including endocrine therapy - collapse 0-2 and 2-4 weeks
    #############################
    ccc19x$der_systemic_cancer_tx_timing_v2 <- NA
    
    #Modality known to be systemic
    temp.ref <- which(ccc19x$treatment_modality___685 == 1|
                        ccc19x$treatment_modality___694 == 1|
                        ccc19x$treatment_modality___58229 == 1|
                        ccc19x$treatment_modality___45186 == 1)
    
    ccc19x$der_systemic_cancer_tx_timing_v2[temp.ref] <- ccc19x$der_cancer_tx_timing_v2[temp.ref]
    
    #Known to have never received treatment (including after COVID-19 for the first time)
    temp.ref <- which(ccc19x$der_cancer_tx_timing_v2 == 88)
    ccc19x$der_systemic_cancer_tx_timing_v2[temp.ref] <- 88
    
    #Unknown
    temp.ref <- which(ccc19x$der_cancer_tx_timing_v2 == 99)
    ccc19x$der_systemic_cancer_tx_timing_v2[temp.ref] <- 99
    
    ccc19x$der_systemic_cancer_tx_timing_v2 <- as.factor(ccc19x$der_systemic_cancer_tx_timing_v2)
    
    temp <- summary(ccc19x$der_systemic_cancer_tx_timing_v2[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_systemic_cancer_tx_timing_v2',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #############################
    #T13b. Cancer treatment timing - collapse 0-3 months
    #############################
    ccc19x$der_cancer_tx_timing_v3 <- NA
    
    #0-3 months
    ccc19x$der_cancer_tx_timing_v3[which((ccc19x$hx_treatment == 1|ccc19x$recent_treatment %in% 1:3) & 
                                           is.na(ccc19x$der_cancer_tx_timing_v3))] <- 1
    
    #More than 3 months (referent)
    ccc19x$der_cancer_tx_timing_v3[which((ccc19x$hx_treatment %in% 2:3|ccc19x$recent_treatment == 88) & 
                                           is.na(ccc19x$der_cancer_tx_timing_v3))] <- 0
    
    #Never, including treatment starting AFTER the COVID-19 diagnosis
    ccc19x$der_cancer_tx_timing_v3[which((ccc19x$hx_treatment == 88|ccc19x$recent_treatment == 98) & 
                                           is.na(ccc19x$der_cancer_tx_timing_v3))] <- 88
    
    #Unknown
    ccc19x$der_cancer_tx_timing_v3[which((ccc19x$hx_treatment == 99|ccc19x$recent_treatment == 99) & 
                                           is.na(ccc19x$der_cancer_tx_timing_v3))] <- 99
    ccc19x$der_cancer_tx_timing_v3[which(ccc19x$on_treatment == 99 & 
                                        is.na(ccc19x$der_cancer_tx_timing_v3))] <- 99
    
    ccc19x$der_cancer_tx_timing_v3 <- as.factor(ccc19x$der_cancer_tx_timing_v3)
    
    temp <- summary(ccc19x$der_cancer_tx_timing_v3[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_cancer_tx_timing_v3',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #T13c. Cancer treatment never or after COVID-19 diagnosis
    ccc19x$der_cancer_tx_never <- as.character(ccc19x$der_cancer_tx_timing_v3)
    ccc19x$der_cancer_tx_never[which(ccc19x$der_cancer_tx_never == 1)] <- 0
    ccc19x$der_cancer_tx_never[which(ccc19x$der_cancer_tx_never == 88)] <- 1
    ccc19x$der_cancer_tx_never <- as.factor(ccc19x$der_cancer_tx_never)
    
    temp <- summary(ccc19x$der_cancer_tx_never[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_cancer_tx_never',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ####################################################################
    #T15 & T16. Total hospital and ICU LOS for the index hospitalization
    #These variables assume that follow-up forms are entered in linear (temporal) order
    ccc19x$der_los_total <- NA
    ccc19x$der_los_icu <- NA
    
    #Create a temporary data frame to hold the hospitalization intervals
    temp.df <- data.frame(record_id = ccc19x$record_id[which(ccc19x$der_hosp == 1 & ccc19x$redcap_repeat_instrument == '')],
                          hosp_los = 0,
                          hosp_los_2 = 0,
                          icu_los = 0,
                          chained = F,
                          hosp_los_fu = 0,
                          hosp_los_fu_2 = 0,
                          icu_los_fu = 0,
                          der_los_total = NA,
                          der_los_icu = NA,
                          exact = F,
                          stringsAsFactors = F)
    
    for(i in 1:nrow(temp.df))
    {
      temp.ref <- which(ccc19x$record_id == temp.df$record_id[i])
      
      #Only baseline information present
      if(length(temp.ref) == 1)
      {
        if(!is.na(ccc19x$hosp_los[temp.ref]) & ccc19x$hosp_los[temp.ref] < 9999) temp.df$hosp_los[i] <- ccc19x$hosp_los[temp.ref]
        if(!is.na(ccc19x$hosp_los_2[temp.ref]) & ccc19x$hosp_los_2[temp.ref] < 9999) temp.df$hosp_los_2[i] <- ccc19x$hosp_los_2[temp.ref]
        if(!is.na(ccc19x$icu_los[temp.ref]) & ccc19x$icu_los[temp.ref] < 9999) temp.df$icu_los[i] <- ccc19x$icu_los[temp.ref]
      }
      
      #Follow-up forms are present
      if(length(temp.ref) > 1)
      {
        temp.ref2 <- which(ccc19x$redcap_repeat_instrument[temp.ref] == '')
        if(!is.na(ccc19x$hosp_los[temp.ref][temp.ref2]) & ccc19x$hosp_los[temp.ref][temp.ref2] < 9999) temp.df$hosp_los[i] <- ccc19x$hosp_los[temp.ref][temp.ref2]
        if(!is.na(ccc19x$hosp_los_2[temp.ref][temp.ref2]) & ccc19x$hosp_los_2[temp.ref][temp.ref2] < 9999) temp.df$hosp_los_2[i] <- ccc19x$hosp_los_2[temp.ref][temp.ref2]
        if(!is.na(ccc19x$icu_los[temp.ref][temp.ref2]) & ccc19x$icu_los[temp.ref][temp.ref2] < 9999) temp.df$icu_los[i] <- ccc19x$icu_los[temp.ref][temp.ref2]
        
        #Look for forward chaining
        if(!is.na(ccc19x$hosp_los[temp.ref][temp.ref2]) & ccc19x$hosp_los[temp.ref][temp.ref2] == 9999) temp.df$chained[i] <- T
        if(!is.na(ccc19x$icu_los[temp.ref][temp.ref2]) & ccc19x$icu_los[temp.ref][temp.ref2] == 9999) temp.df$chained[i] <- T
        
      }
    }
    
    #Look for backward chaining from 1st follow-up to baseline form
    temp.ref <- which(ccc19x$redcap_repeat_instance == 1 & ccc19x$hosp_status_fu == 88)
    temp.df$chained[temp.df$record_id %in% ccc19x$record_id[temp.ref]] <- T
    
    #Add 1st follow-up data for chained cases
    index <- which(temp.df$chained)
    for(i in 1:length(index))
    {
      temp.ref <- which(ccc19x$record_id == temp.df$record_id[index[i]] &
                          ccc19x$redcap_repeat_instance == 1)
      if(!is.na(ccc19x$hosp_los_fu[temp.ref]) & ccc19x$hosp_los_fu[temp.ref] < 9999) temp.df$hosp_los_fu[index[i]] <- ccc19x$hosp_los_fu[temp.ref]
      if(!is.na(ccc19x$hosp_los_fu_2[temp.ref]) & ccc19x$hosp_los_fu_2[temp.ref] < 9999) temp.df$hosp_los_fu_2[index[i]] <- ccc19x$hosp_los_fu_2[temp.ref]
      if(!is.na(ccc19x$icu_los_fu[temp.ref]) & ccc19x$icu_los_fu[temp.ref] < 9999) temp.df$icu_los_fu[index[i]] <- ccc19x$icu_los_fu[temp.ref]
    }
    
    #Pull LOS for first hospitalization not mentioned on the baseline form
    index <- which(temp.df$record_id %in% ccc19x$record_id[which(ccc19x$der_hosp == 1 & ccc19x$der_hosp_bl == 0)])
    for(i in 1:length(index))
    {
      temp.ref <- which(ccc19x$record_id == temp.df$record_id[index[i]] &
                          ccc19x$redcap_repeat_instrument == 'followup')
      
      #Only one follow-up form present
      if(length(temp.ref) == 1)
      {
        if(!is.na(ccc19x$hosp_los_fu[temp.ref]) & ccc19x$hosp_los_fu[temp.ref] < 9999) temp.df$hosp_los_fu[index[i]] <- ccc19x$hosp_los_fu[temp.ref]
        if(!is.na(ccc19x$hosp_los_fu_2[temp.ref]) & ccc19x$hosp_los_fu_2[temp.ref] < 9999) temp.df$hosp_los_fu_2[index[i]] <- ccc19x$hosp_los_fu_2[temp.ref]
        if(!is.na(ccc19x$icu_los_fu[temp.ref]) & ccc19x$icu_los_fu[temp.ref] < 9999) temp.df$icu_los_fu[index[i]] <- ccc19x$icu_los_fu[temp.ref]
      }
      
      #More than one follow-up form present - find the earliest hospitalization
      if(length(temp.ref) > 1)
      {
        temp.ref2 <- which(ccc19x$fu_reason[temp.ref] == 1 | 
                             ccc19x$hosp_status_fu[temp.ref] %in% c(1:3) | 
                             ccc19x$current_status_fu[temp.ref] %in% c(5:8) |
                             ccc19x$current_status_clinical_fu[temp.ref] %in% c(4:8) |
                             ccc19x$who_ordinal_scale[temp.ref] %in% 3:7|
                             ccc19x$c19_anticoag_reason_fu___3[temp.ref] == 1|
                             ccc19x$resp_failure_tx_fu[temp.ref] %in% 2:6)
        temp.ref2 <- min(temp.ref2)
        if(!is.na(ccc19x$hosp_los_fu[temp.ref][temp.ref2]) & ccc19x$hosp_los_fu[temp.ref][temp.ref2] < 9999) temp.df$hosp_los_fu[index[i]] <- ccc19x$hosp_los_fu[temp.ref][temp.ref2]
        if(!is.na(ccc19x$hosp_los_fu_2[temp.ref][temp.ref2]) & ccc19x$hosp_los_fu_2[temp.ref][temp.ref2] < 9999) temp.df$hosp_los_fu_2[index[i]] <- ccc19x$hosp_los_fu_2[temp.ref][temp.ref2]
        if(!is.na(ccc19x$icu_los_fu[temp.ref][temp.ref2]) & ccc19x$icu_los_fu[temp.ref][temp.ref2] < 9999) temp.df$icu_los_fu[index[i]] <- ccc19x$icu_los_fu[temp.ref][temp.ref2]
      }
    }
    
    #Tally up - total LOS first
    
    #Exact unchained baseline
    temp.ref <- which(temp.df$hosp_los != 0 & !temp.df$chained)
    temp.df$der_los_total[temp.ref] <- temp.df$hosp_los[temp.ref]
    temp.df$exact[temp.ref] <- T
    
    #Exact unchained follow-up
    temp.ref <- which(temp.df$hosp_los_fu != 0 & !temp.df$chained)
    temp.df$der_los_total[temp.ref] <- temp.df$hosp_los_fu[temp.ref]
    temp.df$exact[temp.ref] <- T
    
    #Exact chained (these need manual review)
    temp.ref <- which((temp.df$hosp_los != 0|temp.df$hosp_los_fu != 0) & temp.df$chained)
    
    #Inexact unchained with baseline LOS
    temp.ref <- which(temp.df$hosp_los == 0 &
                        (temp.df$hosp_los_2 != 0|temp.df$icu_los != 0) & 
                        !temp.df$chained)
    temp.df$der_los_total[temp.ref] <- temp.df$hosp_los_2[temp.ref] + temp.df$icu_los[temp.ref]
    
    #Inexact unchained follow-up
    temp.ref <- which(temp.df$hosp_los_fu == 0 &
                        (temp.df$hosp_los_fu_2 != 0|temp.df$icu_los_fu != 0) & 
                        !temp.df$chained)
    temp.df$der_los_total[temp.ref] <- temp.df$hosp_los_fu_2[temp.ref] + temp.df$icu_los_fu[temp.ref]
    
    #Inexact chained with baseline or f/u LOS
    temp.ref <- which((temp.df$hosp_los_2 != 0|temp.df$icu_los != 0|temp.df$hosp_los_fu_2 != 0|temp.df$icu_los_fu != 0) & temp.df$chained)
    temp.df$der_los_total[temp.ref] <- temp.df$hosp_los_2[temp.ref] + temp.df$icu_los[temp.ref] + 
      temp.df$hosp_los_fu_2[temp.ref] + temp.df$icu_los_fu[temp.ref]
    
    #ICU LOS, next
    
    #Baseline unchained
    temp.ref <- which(temp.df$icu_los != 0 & !temp.df$chained)
    temp.df$der_los_icu[temp.ref] <- temp.df$icu_los[temp.ref]
    
    #Follow-up unchained
    temp.ref <- which(temp.df$hosp_los == 0 & temp.df$hosp_los_2 == 0 &
                        temp.df$icu_los == 0 & temp.df$icu_los_fu != 0 &
                        !temp.df$chained)
    temp.df$der_los_icu[temp.ref] <- temp.df$icu_los_fu[temp.ref]
    
    #Chained
    temp.ref <- which((temp.df$icu_los != 0|temp.df$icu_los_fu != 0) &
                        temp.df$chained)
    temp.df$der_los_icu[temp.ref] <- temp.df$icu_los[temp.ref] + temp.df$icu_los_fu[temp.ref]
    
    #Add the tally to the derived variables
    for(i in which(!is.na(temp.df$der_los_total)))
    {
      temp.ref <- which(ccc19x$record_id == temp.df$record_id[i])
      if(temp.df$exact[i]) ccc19x$der_los_total[temp.ref] <- temp.df$der_los_total[i] else
        ccc19x$der_los_total[temp.ref] <- paste(temp.df$der_los_total[i], '+', sep = '')
      
      if(!is.na(temp.df$der_los_icu[i])) ccc19x$der_los_icu[temp.ref] <- temp.df$der_los_icu[i]
    }
    
    ccc19x$der_los_total <- factor(ccc19x$der_los_total)
    
    temp <- summary(ccc19x$der_los_total[which(ccc19x$der_hosp == 1 & ccc19x$redcap_repeat_instrument == '')])
    temp.var.log <- data.frame(name = 'der_los_total',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    temp <- summary(ccc19x$der_los_icu[which(ccc19x$der_ICU == 1 & ccc19x$redcap_repeat_instrument == '')])
    temp.var.log <- data.frame(name = 'der_los_icu',
                               timestamp = Sys.time(), 
                               values = paste(c(paste('Median:', temp[3], 'days'),
                                                paste('IQR:', temp[2], '-', temp[5]),
                                                paste('NA:', temp[7])), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    }
  print('Time measurements completed')
  
  #################
  #Ordinal outcomes
  #################
  {
    ##WHO Ordinal scale derived
    #O11. WHO orginal scale
    ccc19x$der_who <- NA
    #1. Outpatient with no limitation of activity, must exclude ECOG > 1
    ccc19x$der_who[which((ccc19x$severity_of_covid_19_v2 == 1|
                            ccc19x$current_status == 1|
                            ccc19x$worst_status_clinical %in% 0:1) 
                         & !(ccc19x$ecog_status %in% c(1,2,3,4)))] <- 1 
    #2. Outpatient with limitation of activities
    ccc19x$der_who[which((ccc19x$severity_of_covid_19_v2 == 1|
                            ccc19x$current_status == 1) & 
                           (ccc19x$worst_status_clinical %in% c(2:3) |
                              ccc19x$current_status_clinical_fu %in% c(2:3)| 
                              ccc19x$current_status_clinical %in% c(2:3)))] <- 2
    #2a. Recapture patients who only ruled out of 1 due to ECOG 1+
    ccc19x$der_who[which((ccc19x$severity_of_covid_19_v2 == 1|
                            ccc19x$current_status == 1|
                            ccc19x$worst_status_clinical %in% 0:1) 
                         & (ccc19x$ecog_status %in% c(1,2,3,4)))] <- 2
    #3. Hospitalized, no oxygen
    ccc19x$der_who[which(ccc19x$der_o2_ever == 0 & ccc19x$der_hosp == 1)] <- 3
    #4. Hospitalized, oxygen
    ccc19x$der_who[which(ccc19x$der_o2_ever == 1 & 
                           ccc19x$der_hosp == 1)] <- 4
    #5. Hospitalized, oxygen or NIV
    ccc19x$der_who[which((ccc19x$der_o2_ever == 1 & 
                            ccc19x$der_hosp == 1) & 
                           (ccc19x$resp_failure_tx %in% c(2:5) | 
                              ccc19x$resp_failure_tx_fu %in% c(2:5)))] <- 5
    #6 Intubated
    ccc19x$der_who[ccc19x$der_mv == 1] <- 6
    #7. Intubated + additional organ support such as pressors, RRT, or ECMO
    ccc19x$der_who[ccc19x$der_mv == 1 & 
                     (ccc19x$hotn_pressors_fu == 1 | 
                        ccc19x$sepsis_pressors == 1 | 
                        ccc19x$significant_comorbidities___236435004 == 1)] <- 7
    #8. Dead
    ccc19x$der_who[ccc19x$der_deadbinary == 1] <- 8
    
    ccc19x$der_who <- as.factor(ccc19x$der_who)
    summary(ccc19x$der_who[ccc19x$redcap_repeat_instrument == ''])
    
    #O22. ordinal_v1 (0 = never hospitalized; 1 = hospitalized; 2 = ICU; 3 = vent; 4 = death in 30 days)
    {
      #Declare as missing
      ccc19x$der_ordinal_v1 <- NA
      
      #Start with non-hospitalized
      ccc19x$der_ordinal_v1[which(ccc19x$der_hosp == 0)] <- 0
      
      #Hospital (ever/never)
      ccc19x$der_ordinal_v1[which(ccc19x$der_hosp == 1)] <- 1
      ccc19x$der_ordinal_v1[which(ccc19x$der_hosp == 99)] <- 99
      
      #ICU (ever/never)
      #ccc19x$der_ordinal_v1[which(ccc19x$der_ICU == 0)]
      ccc19x$der_ordinal_v1[which(ccc19x$der_ICU == 1)] <- 2
      ccc19x$der_ordinal_v1[which(ccc19x$der_ICU == 99)] <- 99
      
      #Mechanical ventilation (ever/never)
      #ccc19x$der_ordinal_v1[which(ccc19x$der_mv == 0)]
      ccc19x$der_ordinal_v1[which(ccc19x$der_mv == 1)] <- 3
      ccc19x$der_ordinal_v1[which(ccc19x$der_mv == 99)] <- 99
      
      #Death within 30 days
      ccc19x$der_ordinal_v1[which(ccc19x$der_dead30 == 1)] <- 4
      ccc19x$der_ordinal_v1[which(ccc19x$der_dead30 == 99)] <- 99
      
      ccc19x$der_ordinal_v1 <- factor(ccc19x$der_ordinal_v1, ordered = T)
      
      temp <- summary(ccc19x$der_ordinal_v1[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_ordinal_v1',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
    }
    
    #O22a. ordinal_v1a (0 = never hospitalized; 1 = hospitalized; 2 = ICU; 3 = vent; 4 = death EVER)
    {
      #Declare as missing
      ccc19x$der_ordinal_v1a <- NA
      
      #Start with non-hospitalized
      ccc19x$der_ordinal_v1a[which(ccc19x$der_hosp == 0)] <- 0
      
      #Hospital (ever/never)
      ccc19x$der_ordinal_v1a[which(ccc19x$der_hosp == 1)] <- 1
      ccc19x$der_ordinal_v1a[which(ccc19x$der_hosp == 99)] <- 99
      
      #ICU (ever/never)
      #ccc19x$der_ordinal_v1a[which(ccc19x$der_ICU == 0)]
      ccc19x$der_ordinal_v1a[which(ccc19x$der_ICU == 1)] <- 2
      ccc19x$der_ordinal_v1a[which(ccc19x$der_ICU == 99)] <- 99
      
      #Mechanical ventilation (ever/never)
      #ccc19x$der_ordinal_v1a[which(ccc19x$der_mv == 0)]
      ccc19x$der_ordinal_v1a[which(ccc19x$der_mv == 1)] <- 3
      ccc19x$der_ordinal_v1a[which(ccc19x$der_mv == 99)] <- 99
      
      #Death at any time
      ccc19x$der_ordinal_v1a[which(ccc19x$der_deadbinary == 1)] <- 4
      #Only declare unknown if patient not known to be alive at 30, 90, or 180 days
      ccc19x$der_ordinal_v1a[which(ccc19x$der_deadbinary == 99 &
                                     !(ccc19x$der_dead30 == 0|ccc19x$der_dead90a == 0|ccc19x$der_dead180a == 0))] <- 99
      
      ccc19x$der_ordinal_v1a <- factor(ccc19x$der_ordinal_v1a, ordered = T)
      
      temp <- summary(ccc19x$der_ordinal_v1a[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_ordinal_v1a',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
    }
    
    #O22b. ordinal_v1b -- including "at least" - so it is actually a categorical
    #(0 = never hospitalized; 1 = hospitalized; 2 = ICU; 3 = vent; 4 = death ever)
    {
      #Declare as missing
      ccc19x$der_ordinal_v1b <- NA
      
      #Start with none of the below
      ccc19x$der_ordinal_v1b[which(ccc19x$der_hosp == 0 & 
                                     ccc19x$der_ICU == 0 &
                                     ccc19x$der_mv == 0 &
                                     ccc19x$der_deadbinary == 0)] <- 0
      
      #Hospital (ever/never)
      ccc19x$der_ordinal_v1b[which(ccc19x$der_hosp == 1)] <- 1
      ccc19x$der_ordinal_v1b[which(ccc19x$der_hosp == 99 & ccc19x$der_ordinal_v1b == 0)] <- "At least 0"
      ccc19x$der_ordinal_v1b[which(ccc19x$der_hosp == 99 & is.na(ccc19x$der_ordinal_v1b))] <- 99
      
      #ICU (ever/never)
      ccc19x$der_ordinal_v1b[which(ccc19x$der_ICU == 1)] <- 2
      ccc19x$der_ordinal_v1b[which(ccc19x$der_ICU == 99 & ccc19x$der_ordinal_v1b == 0)] <- "At least 0"
      ccc19x$der_ordinal_v1b[which(ccc19x$der_ICU == 99 & ccc19x$der_ordinal_v1b == 1)] <- "At least 1"
      ccc19x$der_ordinal_v1b[which(ccc19x$der_ICU == 99 & is.na(ccc19x$der_ordinal_v1b))] <- 99
      
      #Mechanical ventilation (ever/never)
      ccc19x$der_ordinal_v1b[which(ccc19x$der_mv == 1)] <- 3
      ccc19x$der_ordinal_v1b[which(ccc19x$der_mv == 99 & ccc19x$der_ordinal_v1b == 0)] <- "At least 0"
      ccc19x$der_ordinal_v1b[which(ccc19x$der_mv == 99 & ccc19x$der_ordinal_v1b == 1)] <- "At least 1"
      ccc19x$der_ordinal_v1b[which(ccc19x$der_mv == 99 & ccc19x$der_ordinal_v1b == 2)] <- "At least 2"
      ccc19x$der_ordinal_v1b[which(ccc19x$der_mv == 99 & is.na(ccc19x$der_ordinal_v1b))] <- 99
      
      #Death within 30 days
      ccc19x$der_ordinal_v1b[which(ccc19x$der_deadbinary == 1)] <- 4
      ccc19x$der_ordinal_v1b[which(ccc19x$der_deadbinary == 99 & ccc19x$der_ordinal_v1b == 0)] <- "At least 0"
      ccc19x$der_ordinal_v1b[which(ccc19x$der_deadbinary == 99 & ccc19x$der_ordinal_v1b == 1)] <- "At least 1"
      ccc19x$der_ordinal_v1b[which(ccc19x$der_deadbinary == 99 & ccc19x$der_ordinal_v1b == 2)] <- "At least 2"
      ccc19x$der_ordinal_v1b[which(ccc19x$der_deadbinary == 99 & ccc19x$der_ordinal_v1b == 3)] <- "At least 3"
      ccc19x$der_ordinal_v1b[which(ccc19x$der_deadbinary == 99 & is.na(ccc19x$der_ordinal_v1b))] <- 99
      
      ccc19x$der_ordinal_v1b <- as.factor(ccc19x$der_ordinal_v1b)
      summary(ccc19x$der_ordinal_v1b[ccc19x$redcap_repeat_instrument == ''])
    }
    
    #O23. ordinal_v2 (0 = never hospitalized; 1 = hospitalized; 2 = required O2; 3 = ICU; 4 = vent; 5 = death within 30 days)
    {
      #Declare as missing
      ccc19x$der_ordinal_v2 <- NA
      
      #Start with non-hospitalized
      ccc19x$der_ordinal_v2[which(ccc19x$der_hosp == 0)] <- 0
      
      #Hospitalized regardless of O2
      ccc19x$der_ordinal_v2[which(ccc19x$der_hosp == 1)] <- 1
      ccc19x$der_ordinal_v2[which(ccc19x$der_hosp == 99)] <- 99
      
      #O2 required
      #ccc19x$der_ordinal_v2[which(ccc19x$der_o2_ever == 0)]
      ccc19x$der_ordinal_v2[which(ccc19x$der_o2_ever == 1)] <- 2
      ccc19x$der_ordinal_v2[which(ccc19x$der_o2_ever == 99)] <- 99
      
      #ICU
      #ccc19x$der_ordinal_v2[which(ccc19x$der_ICU == 0)]
      ccc19x$der_ordinal_v2[which(ccc19x$der_ICU == 1)] <- 3
      ccc19x$der_ordinal_v2[which(ccc19x$der_ICU == 99)] <- 99
      
      #Mechanical ventilation
      #ccc19x$der_ordinal_v2[which(ccc19x$der_mv == 0)]
      ccc19x$der_ordinal_v2[which(ccc19x$der_mv == 1)] <- 4
      ccc19x$der_ordinal_v2[which(ccc19x$der_mv == 99)] <- 99
      
      #Death within 30 days
      ccc19x$der_ordinal_v2[which(ccc19x$der_dead30 == 1)] <- 5
      ccc19x$der_ordinal_v2[which(ccc19x$der_dead30 == 99)] <- 99
      
      ccc19x$der_ordinal_v2 <- factor(ccc19x$der_ordinal_v2, ordered = T)
      
      temp <- summary(ccc19x$der_ordinal_v2[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_ordinal_v2',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
    }
    
    #O26. ordinal_v3 (0 = never hospitalized; 1 = hospitalized and did NOT require O2;
    #                  2 = hospitalized and required O2; 3 = ICU; 4 = vent; 5 = death within 30d)
    
    #Declare as missing
    ccc19x$der_ordinal_v3 <- NA
    
    #Start with non-hospitalized
    ccc19x$der_ordinal_v3[which(ccc19x$der_hosp == 0)] <- 0
    
    #Hospitalized and did NOT require O2
    ccc19x$der_ordinal_v3[which(ccc19x$der_hosp == 1 & ccc19x$der_o2_ever == 0)] <- 1
    ccc19x$der_ordinal_v3[which(ccc19x$der_hosp == 99|ccc19x$der_o2_ever == 99)] <- 99
    
    #Hospitalized and did require O2
    ccc19x$der_ordinal_v3[which(ccc19x$der_hosp == 1 & ccc19x$der_o2_ever == 1)] <- 2
    
    #ICU
    #ccc19x$der_ordinal_v3[which(ccc19x$der_ICU == 0)]
    ccc19x$der_ordinal_v3[which(ccc19x$der_ICU == 1)] <- 3
    ccc19x$der_ordinal_v3[which(ccc19x$der_ICU == 99)] <- 99
    
    #Mechanical ventilation
    #ccc19x$der_ordinal_v3[which(ccc19x$der_mv == 0)]
    ccc19x$der_ordinal_v3[which(ccc19x$der_mv == 1)] <- 4
    ccc19x$der_ordinal_v3[which(ccc19x$der_mv == 99)] <- 99
    
    #Death within 30 days
    ccc19x$der_ordinal_v3[which(ccc19x$der_dead30 == 1)] <- 5
    ccc19x$der_ordinal_v3[which(ccc19x$der_dead30 == 99)] <- 99
    
    ccc19x$der_ordinal_v3 <- factor(ccc19x$der_ordinal_v3, ordered = T)
    
    temp <- summary(ccc19x$der_ordinal_v3[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ordinal_v3',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #O26a. ordinal_v3a (0 = never hospitalized; 1 = hospitalized and did NOT require O2;
    #                  2 = hospitalized and required O2; 3 = ICU; 4 = vent; 5 = death at any time)
    
    #Declare as missing
    ccc19x$der_ordinal_v3a <- NA
    
    #Start with non-hospitalized
    ccc19x$der_ordinal_v3a[which(ccc19x$der_hosp == 0)] <- 0
    
    #Hospitalized and did NOT require O2
    ccc19x$der_ordinal_v3a[which(ccc19x$der_hosp == 1 & ccc19x$der_o2_ever == 0)] <- 1
    ccc19x$der_ordinal_v3a[which(ccc19x$der_hosp == 99|ccc19x$der_o2_ever == 99)] <- 99
    
    #Hospitalized and did require O2
    ccc19x$der_ordinal_v3a[which(ccc19x$der_hosp == 1 & ccc19x$der_o2_ever == 1)] <- 2
    
    #ICU
    #ccc19x$der_ordinal_v3a[which(ccc19x$der_ICU == 0)]
    ccc19x$der_ordinal_v3a[which(ccc19x$der_ICU == 1)] <- 3
    ccc19x$der_ordinal_v3a[which(ccc19x$der_ICU == 99)] <- 99
    
    #Mechanical ventilation
    #ccc19x$der_ordinal_v3a[which(ccc19x$der_mv == 0)]
    ccc19x$der_ordinal_v3a[which(ccc19x$der_mv == 1)] <- 4
    ccc19x$der_ordinal_v3a[which(ccc19x$der_mv == 99)] <- 99
    
    #Death at any time
    ccc19x$der_ordinal_v3a[which(ccc19x$der_deadbinary == 1)] <- 5
    ccc19x$der_ordinal_v3a[which(ccc19x$der_deadbinary == 99)] <- 99
    
    ccc19x$der_ordinal_v3a <- factor(ccc19x$der_ordinal_v3a, ordered = T)
    
    temp <- summary(ccc19x$der_ordinal_v3a[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ordinal_v3a',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #O26b. ordinal_v3b (0 = never hospitalized; 1 = hospitalized and did NOT require O2;
    #                  2 = hospitalized and required O2; 3 = ICU and/or vent; 4 = death at any time)
    
    #Declare as missing
    ccc19x$der_ordinal_v3b <- NA
    
    #Start with non-hospitalized
    ccc19x$der_ordinal_v3b[which(ccc19x$der_hosp == 0)] <- 0
    
    #Hospitalized and did NOT require O2
    ccc19x$der_ordinal_v3b[which(ccc19x$der_hosp == 1 & ccc19x$der_o2_ever == 0)] <- 1
    ccc19x$der_ordinal_v3b[which(ccc19x$der_hosp == 99|ccc19x$der_o2_ever == 99)] <- 99
    
    #Hospitalized and did require O2
    ccc19x$der_ordinal_v3b[which(ccc19x$der_hosp == 1 & ccc19x$der_o2_ever == 1)] <- 2
    
    #ICU
    #ccc19x$der_ordinal_v3b[which(ccc19x$der_ICU == 0)]
    ccc19x$der_ordinal_v3b[which(ccc19x$der_ICU == 1|ccc19x$der_mv == 1)] <- 3
    ccc19x$der_ordinal_v3b[which((ccc19x$der_ICU == 99|ccc19x$der_mv == 99) &
                                   ccc19x$der_ordinal_v3b != 3)] <- 99
    
    #Death at any time
    ccc19x$der_ordinal_v3b[which(ccc19x$der_deadbinary == 1)] <- 4
    ccc19x$der_ordinal_v3b[which(ccc19x$der_deadbinary == 99)] <- 99
    
    ccc19x$der_ordinal_v3b <- factor(ccc19x$der_ordinal_v3b, ordered = T)
    
    temp <- summary(ccc19x$der_ordinal_v3b[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ordinal_v3b',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #O27. ordinal_v4 (0 = ambulatory, 1 = hospitalized, 2= hospitalized with supplemental oxygen, 
    #     3 = ICU admission, 4 = mechanical ventilation, 
    #     5 = ICU or mechanical ventilation, with vasopressors/inotropes, 6 = death)
    
    #Declare as missing
    ccc19x$der_ordinal_v4 <- NA
    
    #Start with non-hospitalized
    ccc19x$der_ordinal_v4[which(ccc19x$der_hosp == 0)] <- 0
    
    #Hospitalized
    ccc19x$der_ordinal_v4[which(ccc19x$der_hosp == 1)] <- 1
    ccc19x$der_ordinal_v4[which(ccc19x$der_hosp == 99)] <- 99
    
    #Hospitalized and required O2
    ccc19x$der_ordinal_v4[which(ccc19x$der_hosp == 1 & ccc19x$der_o2_ever == 1)] <- 2
    ccc19x$der_ordinal_v4[which(ccc19x$der_hosp == 1 & ccc19x$der_o2_ever == 99)] <- 99
    
    #ICU
    ccc19x$der_ordinal_v4[which(ccc19x$der_ICU == 1)] <- 3
    ccc19x$der_ordinal_v4[which(ccc19x$der_ICU == 99)] <- 99
    
    #Mechanical ventilation
    ccc19x$der_ordinal_v4[which(ccc19x$der_mv == 1)] <- 4
    ccc19x$der_ordinal_v4[which(ccc19x$der_mv == 99)] <- 99
    
    #ICU or Mechanical ventilation, with pressors
    ccc19x$der_ordinal_v4[which((ccc19x$der_ICU == 1|ccc19x$der_mv == 1) & ccc19x$der_pressors == 1)] <- 5
    ccc19x$der_ordinal_v4[which((ccc19x$der_ICU == 1|ccc19x$der_mv == 1) & ccc19x$der_pressors == 99)] <- 99
    
    #Death at any time
    ccc19x$der_ordinal_v4[which(ccc19x$der_deadbinary == 1)] <- 6
    ccc19x$der_ordinal_v4[which(ccc19x$der_deadbinary == 99)] <- 99
    
    ccc19x$der_ordinal_v4 <- factor(ccc19x$der_ordinal_v4, ordered = T)
    
    temp <- summary(ccc19x$der_ordinal_v1[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ordinal_v4',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
  }
  print('Ordinal outcomes completed')
  
  ###########
  #Treatments - takes some time, not as much as complications
  ###########
  {
    "hca" 
    # #Rx1. Derived variable for hydroxychloroquine/azithro exposure used for TREATMENT of COVID-19
    # ccc19x$der_hca <- NA
    # 
    # #Non-trial
    # #Neither, baseline
    # temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___') & 
    #                     !grepl(colnames(ccc19x), pattern = '19_treatment___unk|19_treatment___rxcui_18631|19_treatment___rxcui_5521'))
    # for(i in which(ccc19x$redcap_repeat_instrument == ''))
    #   if(ccc19x$covid_19_treatment___rxcui_18631[i] == 0 &
    #      ccc19x$covid_19_treatment___rxcui_5521[i] == 0 &
    #      any(ccc19x[i,temp.ref])) ccc19x$der_hca[i] <- 'Neither HCQ nor AZ'
    # 
    # #Neither, followup
    # temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
    #                     !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk|19_treatment_fu___rxcui_18631|19_treatment_fu___rxcui_5521'))
    # for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
    #   if(ccc19x$covid_19_treatment_fu___rxcui_18631[i] == 0 &
    #      ccc19x$covid_19_treatment_fu___rxcui_5521[i] == 0 &
    #      any(ccc19x[i,temp.ref])) ccc19x$der_hca[i] <- 'Neither HCQ nor AZ'
    # 
    # ccc19x$der_hca[which(ccc19x$covid_19_treatment___rxcui_5521 == 1 & ccc19x$covid_19_treatment___rxcui_18631 == 0)] <- 'HCQ alone'
    # ccc19x$der_hca[which(ccc19x$covid_19_treatment_fu___rxcui_5521 == 1 & ccc19x$covid_19_treatment_fu___rxcui_18631 == 0)] <- 'HCQ alone'
    # 
    # ccc19x$der_hca[which(ccc19x$covid_19_treatment___rxcui_5521 == 0 & ccc19x$covid_19_treatment___rxcui_18631 == 1)] <- 'AZ alone'
    # ccc19x$der_hca[which(ccc19x$covid_19_treatment_fu___rxcui_5521 == 0 & ccc19x$covid_19_treatment_fu___rxcui_18631 == 1)] <- 'AZ alone'
    # 
    # ccc19x$der_hca[which(ccc19x$covid_19_treatment___rxcui_5521 == 1 & ccc19x$covid_19_treatment___rxcui_18631 == 1)] <- 'AZ+HCQ'
    # ccc19x$der_hca[which(ccc19x$covid_19_treatment_fu___rxcui_5521 == 1 & ccc19x$covid_19_treatment_fu___rxcui_18631 == 1)] <- 'AZ+HCQ'
    # 
    # temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___') & 
    #                     !grepl(colnames(ccc19x), pattern = '19_treatment___unk'))
    # for(i in which(ccc19x$redcap_repeat_instrument == ''))
    #   if(ccc19x$covid_19_treatment___unk[i] == 1 &
    #      all(ccc19x[i,temp.ref] == 0 )) ccc19x$der_hca[i] <- 'Unknown'
    # 
    # temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
    #                     !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
    # for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
    #   if(ccc19x$covid_19_treatment_fu___unk[i] == 1 &
    #      all(ccc19x[i,temp.ref] == 0 )) ccc19x$der_hca[i] <- 'Unknown'
    # 
    # #Trial baseline
    # trial.ref <- which(ccc19x$covid_19_treatment_trial == 1)
    # for(i in 1:length(trial.ref))
    # {
    #   temp <- ccc19x[trial.ref[i],c('covid_19_trial_tx___rxcui_5521', 'covid_19_trial_tx___rxcui_18631')]
    #   if(temp[1] == 1 & temp[2] == 0) temp <- 'HCQ alone' else
    #     if(temp[1] == 0 & temp[2] == 1) temp <- 'AZ alone' else
    #       if(temp[1] == 1 & temp[2] == 1) temp <- 'AZ+HCQ' else temp <- 'Neither HCQ nor AZ'
    #       
    #       if(ccc19x$der_hca[trial.ref[i]] == 'HCQ alone' & temp == 'AZ alone' |
    #          ccc19x$der_hca[trial.ref[i]] == 'AZ alone' & temp == 'HCQ alone' |
    #          ccc19x$der_hca[trial.ref[i]] == 'AZ+HCQ') ccc19x$der_hca[trial.ref[i]] <- 'AZ+HCQ' 
    #       
    # }
    # 
    # #Trial f/u
    # trial.ref <- which(ccc19x$covid_19_treatment_trial_fu == 1)
    # if(length(trial.ref) > 0)
    # {
    #   for(i in 1:length(trial.ref))
    #   {
    #     temp <- ccc19x[trial.ref[i],c('covid_19_trial_tx_fu___rxcui_5521', 'covid_19_trial_tx_fu___rxcui_18631')]
    #     if(temp[1] != 0 & temp[2] != 0)
    #     {
    #     if(temp[1] == 1 & temp[2] == 0) temp <- 'HCQ alone' else
    #       if(temp[1] == 0 & temp[2] == 1) temp <- 'AZ alone' else
    #         if(temp[1] == 1 & temp[2] == 1) temp <- 'AZ+HCQ' else temp <- 'Neither HCQ nor AZ'
    #         
    #         if(ccc19x$der_hca[trial.ref[i]] == 'HCQ alone' & temp == 'AZ alone' |
    #            ccc19x$der_hca[trial.ref[i]] == 'AZ alone' & temp == 'HCQ alone' |
    #            ccc19x$der_hca[trial.ref[i]] == 'AZ+HCQ') ccc19x$der_hca[trial.ref[i]] <- 'AZ+HCQ'
    #     }
    #   }         
    # }
    # 
    # #Resolve discrepancies between baseline and f/u
    # temp.ref <- which(ccc19x$redcap_repeat_instrument == 'followup')
    # for(i in temp.ref)
    # {
    #   temp <- ccc19x$der_hca[ccc19x$record_id == ccc19x$record_id[i]]
    #   temp <- temp[!is.na(temp)]
    #   if(length(temp) > 0)
    #   {
    #     if((any(temp == 'AZ alone') & any(temp=='HCQ alone'))|any(temp == 'AZ+HCQ')) ccc19x$der_hca[ccc19x$record_id == ccc19x$record_id[i]] <- 'AZ+HCQ'
    #     else if(any(temp %in% c('Neither HCQ nor AZ', 'Unknown')) & any(!temp %in% c('Neither HCQ nor AZ', 'Unknown'))) ccc19x$der_hca[ccc19x$record_id == ccc19x$record_id[i]] <- unique(temp[!temp %in% c('Neither HCQ nor AZ', 'Unknown')])
    #   }
    # }
    # 
    # #Factor
    # ccc19x$der_hca <- as.factor(ccc19x$der_hca)
    # ccc19x$der_hca <- relevel(ccc19x$der_hca, ref = 'Neither HCQ nor AZ')
    # 
    # temp <- summary(ccc19x$der_hca[ccc19x$redcap_repeat_instrument == ''])
    # temp.var.log <- data.frame(name = 'der_hca',
    #                            timestamp = Sys.time(),
    #                            values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
    #                            stringsAsFactors = F)
    # var.log <- rbind(var.log, temp.var.log)
    
    #Rx2. Oseltamivir ever (partial derived)
    ccc19x$der_oselt <- NA
    ccc19x$der_oselt[which(ccc19x$concomitant_meds___rxcui_260101 == 1|
                             ccc19x$covid_19_treatment___rxcui_260101 == 1|
                             ccc19x$covid_19_treatment_fu___rxcui_260101 == 1)] <- 1
    
    #Factor
    ccc19x$der_oselt <- as.factor(ccc19x$der_oselt)
    summary(ccc19x$der_oselt[ccc19x$redcap_repeat_instrument == ''])
    
    #Rx3. HCQ ever used for TREATMENT of COVID-19
    ccc19x$der_hcq <- NA
    ccc19x$der_hcq[which(ccc19x$covid_19_treatment___rxcui_5521 == 1|
                           ccc19x$covid_19_trial_tx___rxcui_5521 == 1|
                           ccc19x$covid_19_treatment_fu___rxcui_5521 == 1|
                           ccc19x$covid_19_trial_tx_fu___rxcui_5521 == 1)] <- 1
    
    #Never
    ccc19x$der_hcq[which((ccc19x$covid_19_treatment___rxcui_5521 == 0 & is.na(ccc19x$der_hcq))|
                           (ccc19x$covid_19_treatment_fu___rxcui_5521 == 0 & is.na(ccc19x$der_hcq)))] <- 0
    
    #Unknown
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$covid_19_treatment___unk[i] == 1 &
         (ccc19x$der_hcq[i] == 0 | is.na(ccc19x$der_hcq[i]))) ccc19x$der_hcq[i] <- 99
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0)& ccc19x$covid_19_treatment_fu___unk[i] == 1 &
         (ccc19x$der_hcq[i] == 0 | is.na(ccc19x$der_hcq[i]))) ccc19x$der_hcq[i] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_hcq[i] == 0) ccc19x$der_hcq[i] <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_hcq[i] == 0) ccc19x$der_hcq[i] <- NA
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_hcq[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_hcq[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_hcq[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_hcq[temp.ref] <- 0
      }
    }
    
    ccc19x$der_hcq <- factor(ccc19x$der_hcq)
    
    temp <- summary(ccc19x$der_hcq[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_hcq',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Rx4. High-dose steroids as COVID-19 treatment ever (dose > 20 mg/d)
    ccc19x$der_steroids_hd_c19 <- NA
    ccc19x$der_steroids_hd_c19[which(ccc19x$steroid_specific %in% 2:3|
                                ccc19x$steroid_specific_fu %in% 2:3)] <- 1
    
    #Never or less than 20 mg/d
    ccc19x$der_steroids_hd_c19[which((ccc19x$steroid_specific %in% c('1', '1a', '1b') & is.na(ccc19x$der_steroids_hd_c19))|
                                       (ccc19x$steroid_specific_fu %in% c('1', '1a', '1b') & is.na(ccc19x$der_steroids_hd_c19))|
                                       (ccc19x$covid_19_treatment___ho_45523 == 0 & is.na(ccc19x$der_steroids_hd_c19))|
                                       (ccc19x$covid_19_treatment_fu___ho_45523 == 0 & is.na(ccc19x$der_steroids_hd_c19)))] <- 0
    
    #Unknown
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$covid_19_treatment___unk[i] == 1 &
         (ccc19x$der_steroids_hd_c19[i] == 0 | is.na(ccc19x$der_steroids_hd_c19[i]))) ccc19x$der_steroids_hd_c19[i] <- 99
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$covid_19_treatment_fu___unk[i] == 1 &
         (ccc19x$der_steroids_hd_c19[i] == 0 | is.na(ccc19x$der_steroids_hd_c19[i]))) ccc19x$der_steroids_hd_c19[i] <- 99
    
    ccc19x$der_steroids_hd_c19[which(ccc19x$steroid_specific == 99 & is.na(ccc19x$der_steroids_hd_c19)|
                                ccc19x$steroid_specific_fu == 99 & is.na(ccc19x$der_steroids_hd_c19))] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_steroids_hd_c19[i] == 0) ccc19x$der_steroids_hd_c19[i] <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_steroids_hd_c19[i] == 0) ccc19x$der_steroids_hd_c19[i] <- NA
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_steroids_hd_c19[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_steroids_hd_c19[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_steroids_hd_c19[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_steroids_hd_c19[temp.ref] <- 0
      }
    }
    
    ccc19x$der_steroids_hd_c19 <- factor(ccc19x$der_steroids_hd_c19)
    
    temp <- summary(ccc19x$der_steroids_hd_c19[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_steroids_hd_c19',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Rx4a. Steroids ever used for TREATMENT of COVID-19
    {
      ccc19x$der_steroids_c19 <- NA
      ccc19x$der_steroids_c19[which(ccc19x$covid_19_treatment___ho_45523 == 1|
                                      ccc19x$covid_19_trial_tx___ho_45523 == 1|
                                      ccc19x$covid_19_treatment_fu___ho_45523 == 1|
                                      ccc19x$covid_19_trial_tx_fu___ho_45523 == 1)] <- 1
      
      #Never
      ccc19x$der_steroids_c19[which((ccc19x$covid_19_treatment___ho_45523 == 0 & is.na(ccc19x$der_steroids_c19))|
                                      (ccc19x$covid_19_treatment_fu___ho_45523 == 0 & is.na(ccc19x$der_steroids_c19)))] <- 0
      
      #Unknown
      temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___') & 
                          !grepl(colnames(ccc19x), pattern = '19_treatment___unk'))
      for(i in which(ccc19x$redcap_repeat_instrument == ''))
        if(all(ccc19x[i,temp.ref] == 0) & ccc19x$covid_19_treatment___unk[i] == 1 &
           (ccc19x$der_steroids_c19[i] == 0 | is.na(ccc19x$der_steroids_c19[i]))) ccc19x$der_steroids_c19[i] <- 99
      
      temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                          !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
      for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
        if(all(ccc19x[i,temp.ref] == 0)& ccc19x$covid_19_treatment_fu___unk[i] == 1 &
           (ccc19x$der_steroids_c19[i] == 0 | is.na(ccc19x$der_steroids_c19[i]))) ccc19x$der_steroids_c19[i] <- 99
      
      #Missing
      temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___'))
      for(i in which(ccc19x$redcap_repeat_instrument == ''))
        if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_steroids_c19[i] == 0) ccc19x$der_steroids_c19[i] <- NA
      
      temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___'))
      for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
        if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_steroids_c19[i] == 0) ccc19x$der_steroids_c19[i] <- NA
      
      #Merge baseline and followup if discrepancy
      for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
      {
        temp.ref <- which(ccc19x$record_id == i)
        temp <- ccc19x$der_steroids_c19[temp.ref]
        temp <- as.numeric(unique(temp[!is.na(temp)]))
        if(length(temp) > 0)
        {
          if(any(temp == 1)) ccc19x$der_steroids_c19[temp.ref] <- 1
          if(!any(temp == 1) & any(temp == 99)) ccc19x$der_steroids_c19[temp.ref] <- 99
          if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_steroids_c19[temp.ref] <- 0
        }
      }
      
      ccc19x$der_steroids_c19 <- factor(ccc19x$der_steroids_c19)
      
      temp <- summary(ccc19x$der_steroids_c19[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_steroids_c19',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
    }
    
    #Rx5. Azithromycin ever used for TREATMENT of COVID-19
    ccc19x$der_azithro <- NA
    ccc19x$der_azithro[which(ccc19x$covid_19_treatment___rxcui_18631 == 1|
                               ccc19x$covid_19_trial_tx___rxcui_18631 == 1|
                               ccc19x$covid_19_treatment_fu___rxcui_18631 == 1|
                               ccc19x$covid_19_trial_tx_fu___rxcui_18631 == 1)] <- 1
    
    #Never
    ccc19x$der_azithro[which((ccc19x$covid_19_treatment___rxcui_18631 == 0 & is.na(ccc19x$der_azithro))|
                               (ccc19x$covid_19_treatment_fu___rxcui_18631 == 0 & is.na(ccc19x$der_azithro)))] <- 0
    
    #Unknown
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$covid_19_treatment___unk[i] == 1 &
         (ccc19x$der_azithro[i] == 0 | is.na(ccc19x$der_azithro[i]))) ccc19x$der_azithro[i] <- 99
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0)& ccc19x$covid_19_treatment_fu___unk[i] == 1 &
         (ccc19x$der_azithro[i] == 0 | is.na(ccc19x$der_azithro[i]))) ccc19x$der_azithro[i] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_azithro[i] == 0) ccc19x$der_azithro[i] <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_azithro[i] == 0) ccc19x$der_azithro[i] <- NA
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_azithro[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_azithro[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_azithro[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_azithro[temp.ref] <- 0
      }
    }
    
    ccc19x$der_azithro <- factor(ccc19x$der_azithro)
    
    temp <- summary(ccc19x$der_azithro[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_azithro',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Rx01a. Combined HCQ/Azithro using the derived variables
    ccc19x$der_hca_v2 <- ifelse(ccc19x$der_hcq == 1 & ccc19x$der_azithro == 1, "Both", 
                            ifelse(ccc19x$der_hcq == 1 & ccc19x$der_azithro == 0, "HCQ Only",
                                   ifelse(ccc19x$der_hcq == 0 & ccc19x$der_azithro == 1, "Azithro Only",
                                          ifelse(ccc19x$der_hcq == 0 & ccc19x$der_azithro == 0, "Neither",
                                                 ifelse(ccc19x$der_hcq == 99 | ccc19x$der_azithro == 99, "Unknown",
                                                        NA)))))
    
    temp <- summary(ccc19x$der_hca_v2[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_hca_v2',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Rx16. Convalescent plasma ever used for treatment of COVID-19
    ccc19x$der_plasma <- NA
    ccc19x$der_plasma[which(ccc19x$covid_19_treatment___b05ax03 == 1|
                              ccc19x$covid_19_trial_tx___b05ax03 == 1|
                              ccc19x$covid_19_treatment_fu___b05ax03 == 1|
                              ccc19x$covid_19_trial_tx_fu___b05ax03 == 1)] <- 1
    
    #Never
    ccc19x$der_plasma[which((ccc19x$covid_19_treatment___b05ax03 == 0 & is.na(ccc19x$der_plasma))|
                              (ccc19x$covid_19_treatment_fu___b05ax03 == 0 & is.na(ccc19x$der_plasma)))] <- 0
    
    #Unknown
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$covid_19_treatment___unk[i] == 1 &
         (ccc19x$der_plasma[i] == 0 | is.na(ccc19x$der_plasma[i]))) ccc19x$der_plasma[i] <- 99
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0)& ccc19x$covid_19_treatment_fu___unk[i] == 1 &
         (ccc19x$der_plasma[i] == 0 | is.na(ccc19x$der_plasma[i]))) ccc19x$der_plasma[i] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_plasma[i] == 0) ccc19x$der_plasma[i] <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_plasma[i] == 0) ccc19x$der_plasma[i] <- NA
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_plasma[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_plasma[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_plasma[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_plasma[temp.ref] <- 0
      }
    }
    
    ccc19x$der_plasma <- factor(ccc19x$der_plasma)
    
    temp <- summary(ccc19x$der_plasma[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_plasma',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Rx16. Monoclonal antibody ever used for treatment of COVID-19
    ccc19x$der_monoclonals <- NA
    ccc19x$der_monoclonals[which(ccc19x$covid_19_treatment___db_15718 == 1|
                                   ccc19x$covid_19_treatment___bam_et == 1|
                                   ccc19x$covid_19_treatment___regen_cov2 == 1|
                                   ccc19x$covid_19_treatment___rxcui_2550731 == 1|
                                   ccc19x$covid_19_trial_tx___db_15718 == 1|
                                   ccc19x$covid_19_trial_tx___bam_et == 1|
                                   ccc19x$covid_19_trial_tx___regen_cov2 == 1|
                                   ccc19x$covid_19_trial_tx___rxcui_2550731 == 1|
                                   ccc19x$covid_19_treatment_fu___db_15718 == 1|
                                   ccc19x$covid_19_treatment_fu___bam_et == 1|
                                   ccc19x$covid_19_treatment_fu___regen_cov2 == 1|
                                   ccc19x$covid_19_treatment_fu___rxcui_2550731 == 1|
                                   ccc19x$covid_19_trial_tx_fu___db_15718 == 1|
                                   ccc19x$covid_19_trial_tx_fu___bam_et == 1|
                                   ccc19x$covid_19_trial_tx_fu___regen_cov2 == 1|
                                   ccc19x$covid_19_trial_tx_fu___rxcui_2550731 == 1)] <- 1
    
    #Never
    ccc19x$der_monoclonals[which((ccc19x$covid_19_treatment___db_15718 == 0 & 
                                    ccc19x$covid_19_treatment___bam_et == 0 &
                                    ccc19x$covid_19_treatment___regen_cov2 == 0 &
                                    is.na(ccc19x$der_monoclonals))|
                                   (ccc19x$covid_19_treatment_fu___db_15718 == 0 & 
                                      ccc19x$covid_19_treatment_fu___bam_et == 0 &
                                      ccc19x$covid_19_treatment_fu___regen_cov2 == 0 &
                                      is.na(ccc19x$der_monoclonals)))] <- 0
    
    #Unknown
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$covid_19_treatment___unk[i] == 1 &
         (ccc19x$der_monoclonals[i] == 0 | is.na(ccc19x$der_monoclonals[i]))) ccc19x$der_monoclonals[i] <- 99
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0)& ccc19x$covid_19_treatment_fu___unk[i] == 1 &
         (ccc19x$der_monoclonals[i] == 0 | is.na(ccc19x$der_monoclonals[i]))) ccc19x$der_monoclonals[i] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_monoclonals[i] == 0) ccc19x$der_monoclonals[i] <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_monoclonals[i] == 0) ccc19x$der_monoclonals[i] <- NA
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_monoclonals[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_monoclonals[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_monoclonals[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_monoclonals[temp.ref] <- 0
      }
    }
    
    ccc19x$der_monoclonals <- factor(ccc19x$der_monoclonals)
    
    temp <- summary(ccc19x$der_monoclonals[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_monoclonals',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Rx6. Tocilizumab ever used for TREATMENT of COVID-19
    ccc19x$der_toci <- NA
    ccc19x$der_toci[which(ccc19x$covid_19_treatment___rxcui_612865 == 1|
                            ccc19x$covid_19_trial_tx___l04ac07 == 1|
                            ccc19x$covid_19_treatment_fu___rxcui_612865 == 1|
                            ccc19x$covid_19_tx_interleukin_fu___l04ac07 == 1|
                            ccc19x$covid_19_trial_tx_fu___l04ac07 == 1)] <- 1
    
    #Never
    ccc19x$der_toci[which(ccc19x$covid_19_treatment___rxcui_612865 == 0  &
                            (is.na(ccc19x$covid_19_trial_tx___l04ac07) | ccc19x$covid_19_trial_tx___l04ac07 == 0) &
                            ((is.na(ccc19x$covid_19_treatment_fu___rxcui_612865) | ccc19x$covid_19_treatment_fu___rxcui_612865 == 0) & 
                               (is.na(ccc19x$covid_19_tx_interleukin_fu___l04ac07) | ccc19x$covid_19_tx_interleukin_fu___l04ac07 == 0))
                          &
                            (is.na(ccc19x$covid_19_trial_tx_fu___l04ac07) | ccc19x$covid_19_trial_tx_fu___l04ac07 == 0))] <- 0
    
    #Unknown
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$covid_19_treatment___unk[i] == 1 &
         (ccc19x$der_toci[i] == 0 | is.na(ccc19x$der_toci[i]))) ccc19x$der_toci[i] <- 99
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$covid_19_treatment_fu___unk[i] == 1 &
         (ccc19x$der_toci[i] == 0 | is.na(ccc19x$der_toci[i]))) ccc19x$der_toci[i] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_toci[i] <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_toci[i] <- NA
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_toci[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_toci[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_toci[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_toci[temp.ref] <- 0
      }
    }
    
    ccc19x$der_toci <- factor(ccc19x$der_toci)
    
    temp <- summary(ccc19x$der_toci[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_toci',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Rx7. COVID-19 treatments other than HCQ, AZ, high-dose steroids, remdesivir, or toci (excluding oyxgen, anticoag, ECMO, CRRT)
    ccc19x$der_other_tx_c19 <- NA
    
    #Build the exclusion list
    x <- c('b01a', 'n02ba', #Anticoag, Aspirin, APA
           '233573008', '714749008', #ECMO, CRRT
           'rxcui_5521', 'omop4873974', 'rxcui_18631', #HCQ, Rem, AZ
           'ho_44995', 'rxcui_260101', #Antivirals NOS, oseltamivir
           'ho_45523', #Steroids
           '612865', 'l04ac07', #Toci
           'atc_c10aa', #Statins
           '19_treatment___oth', '19_treatment_fu___oth', 'trial_tx___oth', 'tx_fu___oth', #"Other" treatments (usually antibiotics)
           '19_treatment___unk', '19_treatment_fu___unk', 'trial_tx___unk', 'tx_fu___unk',
           'treatment___none', 'treatment_fu___none')
    x <- paste(x, collapse = '|')
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment__|19_treatment_fu_|covid_19_trial_tx|covid_19_tx') & 
                        !grepl(colnames(ccc19x), pattern = x))
    
    #Exposure to any of the above
    for(i in 1:nrow(ccc19x))
    {
      temp <- ccc19x[i, temp.ref]
      temp <- temp[!is.na(temp)]
      if(length(temp) > 0) 
      {
        if(any(temp))
        {
          ccc19x$der_other_tx_c19[i] <- 1
        } else ccc19x$der_other_tx_c19[i] <- 0
      }
    }
    
    #Exposure to a clinical trial drug
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'covid_19_trial_tx|covid_19_tx') & 
                        !grepl(colnames(ccc19x), pattern = 'rxcui_5521|omop4873974|rxcui_18631|ho_45523|l04ac07|unk'))
    
    for(i in 1:nrow(ccc19x))
    {
      temp <- ccc19x[i, temp.ref]
      temp <- temp[!is.na(temp)]
      if(length(temp) > 0) 
      {
        if(any(temp))
        {
          ccc19x$der_other_tx_c19[i] <- 1
        }
      }
    }
   
    #Unknown
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$covid_19_treatment___unk[i] == 1) ccc19x$der_other_tx_c19[i] <- 99
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$covid_19_treatment_fu___unk[i] == 1) ccc19x$der_other_tx_c19[i] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_other_tx_c19[i] <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_other_tx_c19[i] <- NA
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_other_tx_c19[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_other_tx_c19[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_other_tx_c19[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_other_tx_c19[temp.ref] <- 0
      }
    }
    
    ccc19x$der_other_tx_c19 <- factor(ccc19x$der_other_tx_c19)
    
    temp <- summary(ccc19x$der_other_tx_c19[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_other_tx_c19',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Rx7a. COVID-19 treatments other than HCQ, steroids, or remdesivir
    ccc19x$der_other_tx_c19_v2 <- NA
    
    #Build the exclusion list
    x <- c('b01a', 'n02ba', #Anticoag, Aspirin, APA
           '233573008', '714749008', #ECMO, CRRT
           'rxcui_5521', 'omop4873974', #HCQ, Rem
           'ho_44995', 'rxcui_260101', #Antivirals NOS, oseltamivir
           'ho_45523', #Steroids
           'atc_c10aa', #Statins
           '19_treatment___oth', '19_treatment_fu___oth', 'trial_tx___oth', 'tx_fu___oth', #"Other" treatments (usually antibiotics)
           '19_treatment___unk', '19_treatment_fu___unk', 'trial_tx___unk', 'tx_fu___unk',
           'treatment___none', 'treatment_fu___none')
    x <- paste(x, collapse = '|')
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment__|19_treatment_fu_|covid_19_trial_tx|covid_19_tx') & 
                        !grepl(colnames(ccc19x), pattern = x))
    
    #Exposure to any of the above
    for(i in 1:nrow(ccc19x))
    {
      temp <- ccc19x[i, temp.ref]
      temp <- temp[!is.na(temp)]
      if(length(temp) > 0) 
      {
        if(any(temp))
        {
          ccc19x$der_other_tx_c19_v2[i] <- 1
        } else ccc19x$der_other_tx_c19_v2[i] <- 0
      }
    }
    
    #Exposure to a clinical trial drug
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'covid_19_trial_tx|covid_19_tx') & 
                        !grepl(colnames(ccc19x), pattern = 'rxcui_5521|omop4873974|ho_45523|unk'))
    
    for(i in 1:nrow(ccc19x))
    {
      temp <- ccc19x[i, temp.ref]
      temp <- temp[!is.na(temp)]
      if(length(temp) > 0) 
      {
        if(any(temp))
        {
          ccc19x$der_other_tx_c19_v2[i] <- 1
        }
      }
    }
    
    #Unknown
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$covid_19_treatment___unk[i] == 1) ccc19x$der_other_tx_c19_v2[i] <- 99
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$covid_19_treatment_fu___unk[i] == 1) ccc19x$der_other_tx_c19_v2[i] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_other_tx_c19_v2[i] <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_other_tx_c19_v2[i] <- NA
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_other_tx_c19_v2[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_other_tx_c19_v2[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_other_tx_c19_v2[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_other_tx_c19_v2[temp.ref] <- 0
      }
    }
    
    ccc19x$der_other_tx_c19_v2 <- factor(ccc19x$der_other_tx_c19_v2)
    
    temp <- summary(ccc19x$der_other_tx_c19_v2[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_other_tx_c19_v2',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Rx8. Statins ever (treatment or baseline)
    ccc19x$der_statins <- NA
    ccc19x$der_statins[which(ccc19x$concomitant_meds___atc_c10aa == 1|
                               ccc19x$covid_19_treatment___atc_c10aa == 1|
                               ccc19x$covid_19_treatment_fu___atc_c10aa == 1|
                               ccc19x$covid_19_trial_tx_fu___atc_c10aa == 1)] <- 1
    
    #Never
    ccc19x$der_statins[which(ccc19x$concomitant_meds___atc_c10aa == 0 &
                               ccc19x$covid_19_treatment___atc_c10aa == 0 &
                               (is.na(ccc19x$covid_19_treatment_fu___atc_c10aa) | ccc19x$covid_19_treatment_fu___atc_c10aa == 0) &
                               (is.na(ccc19x$covid_19_trial_tx_fu___atc_c10aa) | ccc19x$covid_19_trial_tx_fu___atc_c10aa == 0))] <- 0
    
    #Unknown baseline or treatment
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___|19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = 'concomitant_meds___unk|19_treatment___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_statins[i] == 0 | is.na(ccc19x$der_statins[i]))) ccc19x$der_statins[i] <- 99
    
    #Unknown f/u treatment
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_statins[i] == 0 | is.na(ccc19x$der_statins[i]))) ccc19x$der_statins[i] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___|concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_statins[i] != 1|is.na(ccc19x$der_statins[i]))) ccc19x$der_statins[i] <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_statins[i] != 1|is.na(ccc19x$der_statins[i]))) ccc19x$der_statins[i] <- NA
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_statins[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_statins[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_statins[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_statins[temp.ref] <- 0
      }
    }
    
    ccc19x$der_statins <- factor(ccc19x$der_statins)
    summary(ccc19x$der_statins[ccc19x$redcap_repeat_instrument == ''])
    
    #Rx8a. Statins at baseline
    ccc19x$der_statins_baseline <- NA
    ccc19x$der_statins_baseline[which(ccc19x$concomitant_meds___atc_c10aa == 1)] <- 1
    
    #Never
    ccc19x$der_statins_baseline[which(ccc19x$concomitant_meds___atc_c10aa == 0)] <- 0
    
    #Unknown
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___') & 
                        !grepl(colnames(ccc19x), pattern = 'concomitant_meds___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_statins_baseline[i] == 0 | is.na(ccc19x$der_statins_baseline[i]))) ccc19x$der_statins_baseline[i] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_statins_baseline[i] != 1|is.na(ccc19x$der_statins_baseline[i]))) ccc19x$der_statins_baseline[i] <- NA
    
    ccc19x$der_statins_baseline <- factor(ccc19x$der_statins_baseline)
    
    temp <- summary(ccc19x$der_statins_baseline[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_statins_baseline',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Rx22. Beta blockers (BB) at baseline
    ccc19x$der_BB_baseline <- NA
    ccc19x$der_BB_baseline[which(ccc19x$concomitant_meds___c07a == 1)] <- 1
    
    #Never
    ccc19x$der_BB_baseline[which(ccc19x$concomitant_meds___c07a == 0)] <- 0
    
    #Unknown 
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___') &
                        !grepl(colnames(ccc19x), pattern = 'concomitant_meds___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_BB_baseline[i] == 0 | is.na(ccc19x$der_BB_baseline[i]))) ccc19x$der_BB_baseline[i] <- 99

    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_BB_baseline[i] != 1|is.na(ccc19x$der_BB_baseline[i]))) ccc19x$der_BB_baseline[i] <- NA

    ccc19x$der_BB_baseline <- factor(ccc19x$der_BB_baseline)
    
    temp <- summary(ccc19x$der_BB_baseline[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_BB_baseline',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Rx9. Antivirals ever (except oseltamivir or remdesivir) for treatment of COVID-19
    ccc19x$der_antivirals <- NA
    ccc19x$der_antivirals[which(ccc19x$covid_19_treatment___ho_44995 == 1|
                                  ccc19x$covid_19_treatment___atc_j05ar10 == 1|
                                  #ccc19x$covid_19_treatment___rxcui_260101 == 1| #Ignore oseltamivir
                                  ccc19x$covid_19_treatment_fu___ho_44995 == 1|
                                  #ccc19x$covid_19_treatment_fu___atc_j05ar10 == 1| #Ignore oseltamivir
                                  ccc19x$covid_19_treatment_fu___rxcui_260101 == 1)] <- 1
    
    #Never
    ccc19x$der_antivirals[which((ccc19x$covid_19_treatment___ho_44995 == 0 &
                                   ccc19x$covid_19_treatment___atc_j05ar10 == 0 &
                                   ccc19x$covid_19_treatment___rxcui_260101 == 0) &
                                  (is.na(ccc19x$covid_19_treatment_fu___ho_44995) | ccc19x$covid_19_treatment_fu___ho_44995 == 0) &
                                  (is.na(ccc19x$covid_19_treatment_fu___atc_j05ar10) | ccc19x$covid_19_treatment_fu___atc_j05ar10 == 0) &
                                  (is.na(ccc19x$covid_19_treatment_fu___rxcui_260101) | ccc19x$covid_19_treatment_fu___rxcui_260101 == 0))] <- 0
    
    #Unknown
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_antivirals[i] <- 99
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_antivirals[i] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_antivirals[i] <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_antivirals[i] <- NA
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_antivirals[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_antivirals[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_antivirals[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_antivirals[temp.ref] <- 0
      }
    }
    
    ccc19x$der_antivirals <- factor(ccc19x$der_antivirals)
    summary(ccc19x$der_antivirals[ccc19x$redcap_repeat_instrument == ''])
    
    #Rx10. Low-dose steroids ever (dose up to 20 mg/d, baseline or treatment of COVID-19)
    ccc19x$der_steroids_ld <- NA
    ccc19x$der_steroids_ld[which(ccc19x$steroid_specific_2 %in% c('1', '1a', '1b')|
                                   ccc19x$steroid_specific %in% c('1', '1a', '1b')|
                                   ccc19x$steroid_specific_fu %in% c('1', '1a', '1b'))] <- 1
    
    #Never
    ccc19x$der_steroids_ld[which((ccc19x$concomitant_meds___h02 == 0 & is.na(ccc19x$der_steroids_ld))|
                                   (ccc19x$steroid_specific_2 %in% 2:3 & is.na(ccc19x$der_steroids_ld))|
                                   (ccc19x$covid_19_treatment___ho_45523 == 0 & is.na(ccc19x$der_steroids_ld))|
                                   (ccc19x$steroid_specific %in% 2:3 & is.na(ccc19x$der_steroids_ld))|
                                   (ccc19x$covid_19_treatment_fu___ho_45523 == 0 & is.na(ccc19x$der_steroids_ld))|
                                   (ccc19x$steroid_specific_fu %in% 2:3 & is.na(ccc19x$der_steroids_ld)))] <- 0
    
    #Unknown
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___') & 
                        !grepl(colnames(ccc19x), pattern = 'concomitant_meds___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$covid_19_treatment___unk[i] == 1 &
         (ccc19x$der_steroids_ld[i] == 0 | is.na(ccc19x$der_steroids_ld[i]))) ccc19x$der_steroids_ld[i] <- 99
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$covid_19_treatment___unk[i] == 1 &
         (ccc19x$der_steroids_ld[i] == 0 | is.na(ccc19x$der_steroids_ld[i]))) ccc19x$der_steroids_ld[i] <- 99
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$covid_19_treatment_fu___unk[i] == 1 &
         (ccc19x$der_steroids_ld[i] == 0 | is.na(ccc19x$der_steroids_ld[i]))) ccc19x$der_steroids_ld[i] <- 99
    
    ccc19x$der_steroids_ld[which(ccc19x$steroid_specific == 99 & is.na(ccc19x$der_steroids_ld)|
                                   ccc19x$steroid_specific_2 == 99 & is.na(ccc19x$der_steroids_ld)|
                                   ccc19x$steroid_specific_fu == 99 & is.na(ccc19x$der_steroids_ld))] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == '' & ccc19x$der_steroids_ld[i] == 0))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_steroids_ld[i] <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___'))
    for(i in which(ccc19x$redcap_repeat_instrument == '' & ccc19x$der_steroids_ld[i] == 0))
      if(all(ccc19x[i,temp.ref] == 0) ) ccc19x$der_steroids_ld[i] <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup' & ccc19x$der_steroids_ld[i] == 0))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_steroids_ld[i] <- NA
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_steroids_ld[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_steroids_ld[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_steroids_ld[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_steroids_ld[temp.ref] <- 0
      }
    }
    
    ccc19x$der_steroids_ld <- factor(ccc19x$der_steroids_ld)
    
    temp <- summary(ccc19x$der_steroids_ld[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_steroids_ld',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Rx11. Anticoagulation, aspirin, or APA ever (baseline or treatment for COVID-19)
    ccc19x$der_ac_apa <- NA
    ccc19x$der_ac_apa[which(ccc19x$concomitant_meds___n02ba == 1|
                              ccc19x$concomitant_meds___b01ac == 1|
                              ccc19x$concomitant_meds___b01a == 1|
                              ccc19x$covid_19_treatment___n02ba == 1|
                              ccc19x$covid_19_treatment___b01ac == 1|
                              ccc19x$covid_19_treatment___b01a == 1|
                              ccc19x$covid_19_treatment_fu___n02ba == 1|
                              ccc19x$covid_19_treatment_fu___b01ac == 1|
                              ccc19x$covid_19_treatment_fu___b01a == 1|
                              ccc19x$c19_anticoag_reason___1 == 1|
                              ccc19x$c19_anticoag_reason___2 == 1|
                              ccc19x$c19_anticoag_reason___2a == 1|
                              ccc19x$c19_anticoag_reason___2b == 1|
                              ccc19x$c19_anticoag_reason___3 == 1|
                              ccc19x$c19_anticoag_reason___oth == 1)] <- 1
    
    #Never
    ccc19x$der_ac_apa[which(is.na(ccc19x$der_ac_apa) & ccc19x$concomitant_meds___n02ba == 0 &
                              ccc19x$concomitant_meds___b01ac == 0 &
                              ccc19x$concomitant_meds___b01a == 0 &
                              ccc19x$covid_19_treatment___n02ba == 0 &
                              ccc19x$covid_19_treatment___b01ac == 0 &
                              ccc19x$covid_19_treatment___b01a == 0 &
                              (is.na(ccc19x$covid_19_treatment_fu___n02ba) | ccc19x$covid_19_treatment_fu___n02ba == 0) &
                              (is.na(ccc19x$covid_19_treatment_fu___b01ac) | ccc19x$covid_19_treatment_fu___b01ac == 0) &
                              (is.na(ccc19x$covid_19_treatment_fu___b01a) | ccc19x$covid_19_treatment_fu___b01a == 0))] <- 0
    
    #Unknown baseline or treatment
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___|19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = 'concomitant_meds___unk|19_treatment___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_ac_apa[i] == 0 | is.na(ccc19x$der_ac_apa[i]))) ccc19x$der_ac_apa[i] <- 99
    
    #Unknown f/u treatment
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_ac_apa[i] == 0 | is.na(ccc19x$der_ac_apa[i]))) ccc19x$der_ac_apa[i] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___|concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_ac_apa[i] != 1|is.na(ccc19x$der_ac_apa[i]))) ccc19x$der_ac_apa[i] <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_ac_apa[i] != 1|is.na(ccc19x$der_ac_apa[i]))) ccc19x$der_ac_apa[i] <- NA
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_ac_apa[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_ac_apa[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_ac_apa[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_ac_apa[temp.ref] <- 0
      }
    }
    
    ccc19x$der_ac_apa <- factor(ccc19x$der_ac_apa)
    
    temp <- summary(ccc19x$der_ac_apa[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ac_apa',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Rx15. Anticoagulation, aspirin, or APA at baseline
    ccc19x$der_ac_apa_baseline <- NA
    ccc19x$der_ac_apa_baseline[which(ccc19x$concomitant_meds___n02ba == 1|
                                       ccc19x$concomitant_meds___b01ac == 1|
                                       ccc19x$concomitant_meds___b01a == 1)] <- 1
    
    #Unexposed
    ccc19x$der_ac_apa_baseline[which(is.na(ccc19x$der_ac_apa_baseline) & ccc19x$concomitant_meds___n02ba == 0 &
                                       ccc19x$concomitant_meds___b01ac == 0 &
                                       ccc19x$concomitant_meds___b01a == 0 &
                                       ccc19x$concomitant_meds___unk == 0)] <- 0
    
    #Unknown baseline
    ccc19x$der_ac_apa_baseline[which(ccc19x$concomitant_meds___unk == 1 & is.na(ccc19x$der_ac_apa_baseline))] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_ac_apa_baseline[i] <- NA
   
    ccc19x$der_ac_apa_baseline <- factor(ccc19x$der_ac_apa_baseline)
    
    temp <- summary(ccc19x$der_ac_apa_baseline[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ac_apa_baseline',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Rx13a. Anticoagulation at baseline
    ccc19x$der_ac_baseline <- NA
    ccc19x$der_ac_baseline[which(ccc19x$concomitant_meds___b01a == 1)] <- 1
    
    #Unexposed
    ccc19x$der_ac_baseline[which(is.na(ccc19x$der_ac_baseline) &
                                   ccc19x$concomitant_meds___b01a == 0 &
                                   ccc19x$concomitant_meds___unk == 0)] <- 0
    
    #Unknown baseline
    ccc19x$der_ac_baseline[which(ccc19x$concomitant_meds___unk == 1 & is.na(ccc19x$der_ac_baseline))] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_ac_baseline[i] <- NA
    
    ccc19x$der_ac_baseline <- factor(ccc19x$der_ac_baseline)
    
    temp <- summary(ccc19x$der_ac_baseline[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ac_baseline',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Rx13a1. Anticoagulation at baseline with dosing
    ccc19x$der_ac_baseline_dose <- NA
    
    #Prophylaxis
    ccc19x$der_ac_baseline_dose[which(ccc19x$bl_anticoag_reason == 360271000)] <- 1
    
    #Therapeutic dosing
    ccc19x$der_ac_baseline_dose[which(ccc19x$bl_anticoag_reason == 262202000)] <- 2
    
    #Unexposed
    ccc19x$der_ac_baseline_dose[which(ccc19x$concomitant_meds___b01a == 0)] <- 0
    
    #Unknown baseline
    ccc19x$der_ac_baseline_dose[which(ccc19x$concomitant_meds___unk == 1 & is.na(ccc19x$der_ac_baseline_dose))] <- 99
    
    #Unknown dosing
    ccc19x$der_ac_baseline_dose[which(ccc19x$bl_anticoag_reason == 261665006)] <- 99
    
    #Missing medication (will overwrite)
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_ac_baseline_dose[i] <- NA
    
    #Missing dosing (will overwrite)
    ccc19x$der_ac_baseline_dose[which(ccc19x$concomitant_meds___b01a == 1 &
                                        is.na(ccc19x$bl_anticoag_reason))] <- NA
    
    ccc19x$der_ac_baseline_dose <- factor(ccc19x$der_ac_baseline_dose)
    
    temp <- summary(ccc19x$der_ac_baseline_dose[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ac_baseline_dose',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Rx13b. APA at baseline (aspirin and other antiplatelet agents)
    ccc19x$der_apa_baseline <- NA
    ccc19x$der_apa_baseline[which(ccc19x$concomitant_meds___n02ba == 1|
                                    ccc19x$concomitant_meds___b01ac == 1)] <- 1
    
    #Unexposed
    ccc19x$der_apa_baseline[which(is.na(ccc19x$der_apa_baseline) & ccc19x$concomitant_meds___n02ba == 0 &
                                    ccc19x$concomitant_meds___b01ac == 0 &
                                    ccc19x$concomitant_meds___unk == 0)] <- 0
    
    #Unknown baseline
    ccc19x$der_apa_baseline[which(ccc19x$concomitant_meds___unk == 1 & is.na(ccc19x$der_apa_baseline))] <- 99

    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_apa_baseline[i] <- NA
    
    ccc19x$der_apa_baseline <- factor(ccc19x$der_apa_baseline)
    
    temp <- summary(ccc19x$der_apa_baseline[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_apa_baseline',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Rx13c. Aspirin at baseline with dosing
    ccc19x$der_asa_baseline_dose <- NA
    
    #Low dose (less than 200 mg/day)
    ccc19x$der_asa_baseline_dose[which(ccc19x$aspirin_dose == 262459003)] <- 1
    
    #Full dose
    ccc19x$der_asa_baseline_dose[which(ccc19x$aspirin_dose == 261829003)] <- 2
    
    #Unexposed
    ccc19x$der_asa_baseline_dose[which(ccc19x$concomitant_meds___n02ba == 0 &
                                         is.na(ccc19x$der_asa_baseline_dose))] <- 0
    
    #Unknown baseline
    ccc19x$der_asa_baseline_dose[which(ccc19x$concomitant_meds___unk == 1 & 
                                         is.na(ccc19x$der_asa_baseline_dose))] <- 99
    
    #Unknown dosing
    ccc19x$der_asa_baseline_dose[which(ccc19x$aspirin_dose == 261665006)] <- 99
    
    #Missing medication (will overwrite)
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_asa_baseline_dose[i] <- NA
    
    #Missing dosing (will overwrite)
    ccc19x$der_asa_baseline_dose[which(ccc19x$concomitant_meds___n02ba == 1 &
                                         is.na(ccc19x$aspirin_dose))] <- NA
    
    ccc19x$der_asa_baseline_dose <- factor(ccc19x$der_asa_baseline_dose)
    
    temp <- summary(ccc19x$der_asa_baseline_dose[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_asa_baseline_dose',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Rx13d. APA at baseline (other antiplatelet agents, NOT including aspirin)
    ccc19x$der_apa_baseline_v2 <- NA
    
    #Exposed
    ccc19x$der_apa_baseline_v2[which(ccc19x$concomitant_meds___b01ac == 1)] <- 1
    
    #Unexposed (can be overwritten later)
    ccc19x$der_apa_baseline_v2[which(ccc19x$concomitant_meds___b01ac == 0)] <- 0
    
    #Unknown baseline
    ccc19x$der_apa_baseline_v2[which(ccc19x$concomitant_meds___unk == 1 & is.na(ccc19x$der_apa_baseline_v2))] <- 99
    
    #Missing (will overwrite)
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_apa_baseline_v2[i] <- NA
    
    ccc19x$der_apa_baseline_v2 <- factor(ccc19x$der_apa_baseline_v2)
    
    temp <- summary(ccc19x$der_apa_baseline_v2[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_apa_baseline_v2',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ###################################
    #Rx23. Anticoagulation for COVID-19
    ###################################
    ccc19x$der_ac_c19_dose <- NA
    
    #Prophylactic dosing
    ccc19x$der_ac_c19_dose[which(ccc19x$c19_anticoag_reason___1 == 1|
                                   ccc19x$c19_anticoag_reason_fu___1 == 1)] <- 1
    
    #Therapeutic dosing (will overwrite)
    ccc19x$der_ac_c19_dose[which(ccc19x$c19_anticoag_reason___2a == 1|
                                   ccc19x$c19_anticoag_reason_fu___2a == 1|
                                   ccc19x$c19_anticoag_reason___2b == 1|
                                   ccc19x$c19_anticoag_reason_fu___2b == 1|
                                   ccc19x$c19_anticoag_reason___2c == 1|
                                   ccc19x$c19_anticoag_reason_fu___2c == 1|
                                   ccc19x$c19_anticoag_reason___3 == 1|
                                   ccc19x$c19_anticoag_reason_fu___3 == 1)] <- 2
    
    #Other dosing (does not overwrite)
    ccc19x$der_ac_c19_dose[which((ccc19x$c19_anticoag_reason___oth == 1|
                                    ccc19x$c19_anticoag_reason_fu___oth == 1) &
                                   is.na(ccc19x$der_ac_c19_dose))] <- 88
    
    #Unexposed (declared, only)
    ccc19x$der_ac_c19_dose[which((ccc19x$c19_anticoag_reason___none == 1|
                                    ccc19x$covid_19_treatment___none == 1) &
                                   is.na(ccc19x$der_ac_c19_dose))] <- 0
    
    #Unknown (does not overwrite)
    ccc19x$der_ac_c19_dose[which((ccc19x$c19_anticoag_reason___unk == 1|
                                    ccc19x$c19_anticoag_reason_fu___unk == 1|
                                    ccc19x$covid_19_treatment___unk == 1|
                                    ccc19x$covid_19_treatment_fu___unk)
                                 & is.na(ccc19x$der_ac_c19_dose))] <- 99
    
    #Merge baseline and followup if discrepancy (ignore other/missing/unknown)
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_ac_c19_dose[temp.ref]
      temp <- unique(temp[which(temp %in% 0:2)])
      if(length(temp) > 1)
      {
        if(any(temp == 2)) ccc19x$der_ac_c19_dose[temp.ref] <- 2
        if(!any(temp == 2) & any(temp == 1)) ccc19x$der_ac_c19_dose[temp.ref] <- 1
        if(!any(temp == 2) & !any(temp == 1) & any(temp == 0)) ccc19x$der_ac_c19_dose[temp.ref] <- 0
      }
    }
    
    ccc19x$der_ac_c19_dose <- factor(ccc19x$der_ac_c19_dose)
    
    temp <- summary(ccc19x$der_ac_c19_dose[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ac_c19_dose',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Rx17. ACEi at baseline
    ccc19x$der_acei_bl <- NA
    ccc19x$der_acei_bl[which(ccc19x$concomitant_meds___c09a == 1)] <- 1
    
    #Unexposed
    ccc19x$der_acei_bl[which(is.na(ccc19x$der_acei_bl) &
                               ccc19x$concomitant_meds___c09a == 0 &
                               ccc19x$concomitant_meds___unk == 0)] <- 0
    
    #Unknown baseline
    ccc19x$der_acei_bl[which(ccc19x$concomitant_meds___unk == 1 & is.na(ccc19x$der_acei_bl))] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_acei_bl[i] <- NA
    
    ccc19x$der_acei_bl <- factor(ccc19x$der_acei_bl)
    
    temp <- summary(ccc19x$der_acei_bl[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_acei_bl',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ######################
    #Rx18. ARB at baseline
    ######################
    ccc19x$der_arb_bl <- NA
    ccc19x$der_arb_bl[which(ccc19x$concomitant_meds___c09c == 1)] <- 1
    
    #Unexposed
    ccc19x$der_arb_bl[which(is.na(ccc19x$der_arb_bl) &
                              ccc19x$concomitant_meds___c09c == 0 &
                              ccc19x$concomitant_meds___unk == 0)] <- 0
    
    #Unknown baseline
    ccc19x$der_arb_bl[which(ccc19x$concomitant_meds___unk == 1 & is.na(ccc19x$der_arb_bl))] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_arb_bl[i] <- NA
    
    ccc19x$der_arb_bl <- factor(ccc19x$der_arb_bl)
    
    temp <- summary(ccc19x$der_arb_bl[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_arb_bl',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #####################################
    #Rx19. Immunosuppressants at baseline
    #####################################
    ccc19x$der_immunosupp_bl <- NA
    ccc19x$der_immunosupp_bl[which(ccc19x$concomitant_meds___l04a == 1)] <- 1
    
    summary(factor(ccc19x$der_immunosupp_bl))
    
    #Unexposed
    ccc19x$der_immunosupp_bl[which(is.na(ccc19x$der_immunosupp_bl) &
                                     ccc19x$concomitant_meds___l04a == 0 &
                                     ccc19x$concomitant_meds___unk == 0)] <- 0
    
    #Unknown baseline
    ccc19x$der_immunosupp_bl[which(ccc19x$concomitant_meds___unk == 1 & is.na(ccc19x$der_immunosupp_bl))] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_immunosupp_bl[i] <- NA
    
    ccc19x$der_immunosupp_bl <- factor(ccc19x$der_immunosupp_bl)
    summary(ccc19x$der_immunosupp_bl[ccc19x$redcap_repeat_instrument == ''])
    
    #####################################
    #Rx20. High-dose steroids at baseline
    #####################################
    ccc19x$der_steroids_hd_bl <- NA
    ccc19x$der_steroids_hd_bl[which(ccc19x$steroid_specific_2 %in% 2:3)] <- 1
    
    #Unexposed or under-exposed
    ccc19x$der_steroids_hd_bl[which((ccc19x$steroid_specific_2 %in% c(1, '1a', '1b') |
                                       ccc19x$concomitant_meds___h02 == 0) &
                                      ccc19x$concomitant_meds___unk == 0)] <- 0
    
    #Unknown baseline
    ccc19x$der_steroids_hd_bl[which((ccc19x$concomitant_meds___unk == 1 |
                                       ccc19x$steroid_specific_2 == 99) & 
                                      is.na(ccc19x$der_steroids_hd_bl))] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_steroids_hd_bl[i] <- NA
    
    ccc19x$der_steroids_hd_bl <- factor(ccc19x$der_steroids_hd_bl)
    summary(ccc19x$der_steroids_hd_bl[ccc19x$redcap_repeat_instrument == ''])
    
    ###########################
    #Rx24. Steroids at baseline
    ###########################
    ccc19x$der_steroids_bl <- NA
    
    #Low-dose #1
    ccc19x$der_steroids_bl[which(ccc19x$steroid_specific_2 == '1a')] <- 'Low-dose (<=10 mg PDE/day)'
    
    #Low-dose #2
    ccc19x$der_steroids_bl[which(ccc19x$steroid_specific_2 == 1)] <- 'Low-dose (<=20 mg PDE/day)'
    
    #High-dose
    ccc19x$der_steroids_bl[which(ccc19x$steroid_specific_2 %in% c('1b', '2', '3'))] <- 'High-dose (>10 mg PDE/day)'
    
    #Dose unknown
    ccc19x$der_steroids_bl[which(ccc19x$steroid_specific_2 %in% 99)] <- 'Steroids, unknown dose'
    
    #Dose missing
    ccc19x$der_steroids_bl[which(ccc19x$concomitant_meds___h02 == 1 & ccc19x$steroid_specific_2 == '')] <- 'Steroids, missing dose'
    
    #No steroids
    ccc19x$der_steroids_bl[which(ccc19x$concomitant_meds___h02 == 0)] <- 'None'
    
    #Unknown
    ccc19x$der_steroids_bl[which(ccc19x$concomitant_meds___unk == 1 & 
                                   (ccc19x$der_steroids_bl == 'None'|is.na(ccc19x$der_steroids_bl)))] <- 'Unknown'
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_steroids_bl[i] <- NA
    
    ccc19x$der_steroids_bl <- factor(ccc19x$der_steroids_bl)
    summary(ccc19x$der_steroids_bl[ccc19x$redcap_repeat_instrument == ''])
    
    ###########################
    #Rx24a. Steroids at baseline - simplified
    ###########################
    ccc19x$der_steroids_bl_simple <- NA
    
    #Yes
    ccc19x$der_steroids_bl_simple[which(ccc19x$concomitant_meds___h02 == 1)] <- 1
    
    #No steroids
    ccc19x$der_steroids_bl_simple[which(ccc19x$concomitant_meds___h02 == 0)] <- 0
    
    #Unknown
    ccc19x$der_steroids_bl_simple[which(ccc19x$concomitant_meds___unk == 1 & 
                                          (ccc19x$der_steroids_bl_simple == 0|is.na(ccc19x$der_steroids_bl_simple)))] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_steroids_bl_simple[i] <- NA
    
    ccc19x$der_steroids_bl_simple <- factor(ccc19x$der_steroids_bl_simple)
    
    temp <- summary(ccc19x$der_steroids_bl_simple[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_steroids_bl_simple',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ###########################
    #Rx24b. Steroids at baseline dichotomized at 10 mg PDE/day
    ###########################
    ccc19x$der_steroids_bl_10 <- NA
    
    #None
    ccc19x$der_steroids_bl_10[which(ccc19x$concomitant_meds___h02 == 0)] <- "None"
    
    #10 or less
    ccc19x$der_steroids_bl_10[which(ccc19x$steroid_specific_2 %in% c('1a'))] <- "10 mg PDE or less per day"
    
    #More than 10
    ccc19x$der_steroids_bl_10[which(ccc19x$steroid_specific_2 %in% c('1b', '2', '3'))] <- "More than 10 mg PDE per day"
    
    #Unknown
    ccc19x$der_steroids_bl_10[which((ccc19x$concomitant_meds___unk == 1 & 
                                       (ccc19x$der_steroids_bl_10 == 0|is.na(ccc19x$der_steroids_bl_10)))|
                                      ccc19x$steroid_specific_2 %in% c(1,99))] <- 'Unknown'
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_steroids_bl_10[i] <- NA
    
    ccc19x$der_steroids_bl_10 <- factor(ccc19x$der_steroids_bl_10)
    summary(ccc19x$der_steroids_bl_10[ccc19x$redcap_repeat_instrument == ''])
    
    ###############################################################
    #Rx12. Aspirin or APA ever (baseline or treatment for COVID-19)
    ccc19x$der_as_apa <- NA
    ccc19x$der_as_apa[which(ccc19x$concomitant_meds___n02ba == 1|
                              ccc19x$concomitant_meds___b01ac == 1|
                              ccc19x$covid_19_treatment___n02ba == 1|
                              ccc19x$covid_19_treatment___b01ac == 1|
                              ccc19x$covid_19_treatment_fu___n02ba == 1|
                              ccc19x$covid_19_treatment_fu___b01ac == 1
    )] <- 1
    
    #Never
    ccc19x$der_as_apa[which(is.na(ccc19x$der_as_apa) & ccc19x$concomitant_meds___n02ba == 0 &
                              ccc19x$concomitant_meds___b01ac == 0 &
                              ccc19x$covid_19_treatment___n02ba == 0 &
                              ccc19x$covid_19_treatment___b01ac == 0 &
                              (is.na(ccc19x$covid_19_treatment_fu___n02ba) | ccc19x$covid_19_treatment_fu___n02ba == 0) &
                              (is.na(ccc19x$covid_19_treatment_fu___b01ac) | ccc19x$covid_19_treatment_fu___b01ac == 0))] <- 0
    
    #Unknown baseline or treatment
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___|19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = 'concomitant_meds___unk|19_treatment___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_as_apa[i] == 0 | is.na(ccc19x$der_as_apa[i]))) ccc19x$der_as_apa[i] <- 99
    
   #Unknown f/u treatment
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_as_apa[i] == 0 | is.na(ccc19x$der_as_apa[i]))) ccc19x$der_as_apa[i] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___|concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_as_apa[i] != 1|is.na(ccc19x$der_as_apa[i]))) ccc19x$der_as_apa[i] <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_as_apa[i] != 1|is.na(ccc19x$der_as_apa[i]))) ccc19x$der_as_apa[i] <- NA
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_as_apa[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_as_apa[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_as_apa[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_as_apa[temp.ref] <- 0
      }
    }
    
    ccc19x$der_as_apa <- factor(ccc19x$der_as_apa)
    summary(ccc19x$der_as_apa[ccc19x$redcap_repeat_instrument == ''])
    
    #Rx13. Anticoagulation ever (baseline or treatment for COVID-19)
    ccc19x$der_ac <- NA
    ccc19x$der_ac[which(ccc19x$concomitant_meds___b01a == 1|
                          ccc19x$covid_19_treatment___b01a == 1|
                          ccc19x$covid_19_treatment_fu___b01a == 1|
                          ccc19x$c19_anticoag_reason___1 == 1|
                          ccc19x$c19_anticoag_reason___2 == 1|
                          ccc19x$c19_anticoag_reason___2a == 1|
                          ccc19x$c19_anticoag_reason___2b == 1|
                          ccc19x$c19_anticoag_reason___3 == 1|
                          ccc19x$c19_anticoag_reason___oth == 1)] <- 1
    
    #Never
    ccc19x$der_ac[which(is.na(ccc19x$der_ac) & 
                          ccc19x$concomitant_meds___b01a == 0 &
                          ccc19x$covid_19_treatment___b01a == 0 &
                          (is.na(ccc19x$covid_19_treatment_fu___b01a) | ccc19x$covid_19_treatment_fu___b01a == 0))] <- 0
    
    #Unknown baseline or treatment
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___|19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = 'concomitant_meds___unk|19_treatment___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_ac[i] == 0 | is.na(ccc19x$der_ac[i]))) ccc19x$der_ac[i] <- 99
    
    #Unknown f/u treatment
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_ac[i] == 0 | is.na(ccc19x$der_ac[i]))) ccc19x$der_ac[i] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___|concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_ac[i] != 1|is.na(ccc19x$der_ac[i]))) ccc19x$der_ac[i] <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_ac[i] != 1|is.na(ccc19x$der_ac[i]))) ccc19x$der_ac[i] <- NA
   
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_ac[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_ac[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_ac[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_ac[temp.ref] <- 0
      }
    }
    
    ccc19x$der_ac <- factor(ccc19x$der_ac)
    
    temp <- summary(ccc19x$der_ac[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ac',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)

    #Rx14. Remdesivir ever used for TREATMENT of COVID-19
    ccc19x$der_rem <- NA
    ccc19x$der_rem[which(ccc19x$covid_19_treatment___omop4873974 == 1|
                           ccc19x$covid_19_trial_tx___omop4873974 == 1|
                           ccc19x$covid_19_treatment_fu___omop4873974 == 1|
                           ccc19x$covid_19_trial_tx_fu___omop4873974 == 1)] <- 1
    
    #Never
    ccc19x$der_rem[which((ccc19x$covid_19_treatment___omop4873974 == 0 & is.na(ccc19x$der_rem))|
                           (ccc19x$covid_19_treatment_fu___omop4873974 == 0 & is.na(ccc19x$der_rem)))] <- 0
    
    #Unknown
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$covid_19_treatment___unk[i] == 1 &
         (ccc19x$der_rem[i] == 0 | is.na(ccc19x$der_rem[i]))) ccc19x$der_rem[i] <- 99
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0)& ccc19x$covid_19_treatment_fu___unk[i] == 1 &
         (ccc19x$der_rem[i] == 0 | is.na(ccc19x$der_rem[i]))) ccc19x$der_rem[i] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_rem[i] == 0) ccc19x$der_rem[i] <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_rem[i] == 0) ccc19x$der_rem[i] <- NA
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_rem[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_rem[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_rem[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_rem[temp.ref] <- 0
      }
    }
    
    ccc19x$der_rem <- factor(ccc19x$der_rem)
    
    temp <- summary(ccc19x$der_rem[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_rem',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Rx25. Received "promising" COVID-19 treatment
    ccc19x$der_c19_treatment <- NA
    
    #Yes
    ccc19x$der_c19_treatment[which(ccc19x$der_rem == 1|
                                     ccc19x$der_toci == 1|
                                     ccc19x$der_steroids_c19 == 1|
                                     ccc19x$der_plasma == 1|
                                     ccc19x$der_monoclonals == 1)] <- 1
    
    #No
    ccc19x$der_c19_treatment[which(ccc19x$der_rem == 0 &
                                     ccc19x$der_toci == 0 &
                                     ccc19x$der_steroids_c19 == 0 &
                                     ccc19x$der_plasma == 0 &
                                     ccc19x$der_monoclonals == 0)] <- 0
    
    #Unknown (do not overwrite)
    ccc19x$der_c19_treatment[which((ccc19x$der_rem == 99|
                                     ccc19x$der_toci == 99|
                                     ccc19x$der_steroids_c19 == 99|
                                     ccc19x$der_plasma == 99|
                                     ccc19x$der_monoclonals == 99) &
                                     is.na(ccc19x$der_c19_treatment))] <- 99
    
    ccc19x$der_c19_treatment <- factor(ccc19x$der_c19_treatment)
    
    temp <- summary(ccc19x$der_c19_treatment[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_c19_treatment',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
  }
  print('Treatments completed')
  
  #############
  #Demographics
  #############
  {
    #D1. age with estimation for categoricals (NOTE: THIS IS A CATEGORICAL DERIVED VARIABLE)
    ccc19x$der_age <- ccc19x$age_exact
    
    #Make sure there is not accidental PHI or age <18
    ccc19x$der_age[which(ccc19x$age_exact < 18)] <- '18-'
    ccc19x$der_age[which(ccc19x$age_exact > 89)] <- '90+'
    
    ccc19x$der_age[which(is.na(ccc19x$der_age))] <- ccc19x$age[which(is.na(ccc19x$der_age))]
    ccc19x$der_age[which(ccc19x$der_age == 1)] <- '18-' #Truncate patients younger than 18 to 18-
    ccc19x$der_age[which(ccc19x$der_age == 2)] <- (18+29)/2
    ccc19x$der_age[which(ccc19x$der_age == 3)] <- (30+39)/2
    ccc19x$der_age[which(ccc19x$der_age == 4)] <- (40+49)/2
    ccc19x$der_age[which(ccc19x$der_age == 5)] <- (50+59)/2
    ccc19x$der_age[which(ccc19x$der_age == 6)] <- (60+69)/2
    ccc19x$der_age[which(ccc19x$der_age == 7)] <- (70+79)/2
    ccc19x$der_age[which(ccc19x$der_age == 8)] <- (80+89)/2
    ccc19x$der_age[which(ccc19x$der_age == 9)] <- '90+' #Truncate patients older than 89 to 90+
    
    #Treat unknown as missing
    ccc19x$der_age[which(ccc19x$der_age == 99)] <- NA
    
    summary(factor(ccc19x$der_age)[ccc19x$redcap_repeat_instrument == ''])
    
    #D1a. Age with truncation so that it can be continuous
    ccc19x$der_age_trunc <- ccc19x$der_age
    ccc19x$der_age_trunc[which(ccc19x$der_age_trunc == '18-')] <- 18
    ccc19x$der_age_trunc[which(ccc19x$der_age_trunc == '90+')] <- 90
    ccc19x$der_age_trunc <- as.numeric(ccc19x$der_age_trunc)
    
    temp <- summary(ccc19x$der_age_trunc[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_age_trunc',
                               timestamp = Sys.time(),
                               values = paste(c(paste('Median:', temp[3], 'years'),
                                                paste('IQR:', temp[2], '-', temp[5]),
                                                paste('NA:', temp[7])), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #D1b. Age re-categorized
    ccc19x$der_age_cat <- ccc19x$der_age
    temp <- ccc19x$der_age
    temp[which(ccc19x$der_age == '18-')] <- 0
    temp[which(ccc19x$der_age == '90+')] <- 90
    temp <- as.numeric(temp)
    ccc19x$der_age_cat[which(temp < 18)] <- 'Less than 18 years'
    ccc19x$der_age_cat[which(temp >= 18 & temp < 40)] <- '18-39 years'
    ccc19x$der_age_cat[which(temp >= 40 & temp < 60)] <- '40-59 years'
    ccc19x$der_age_cat[which(temp >= 60 & temp < 70)] <- '60-69 years'
    ccc19x$der_age_cat[which(temp >= 70 & temp < 80)] <- '70-79 years'
    ccc19x$der_age_cat[which(temp >= 80)] <- '80+ years'
    rm(temp)
    
    ccc19x$der_age_cat <- factor(ccc19x$der_age_cat)
    summary(ccc19x$der_age_cat[ccc19x$redcap_repeat_instrument == ''])
    
    #D1c. Age re-categorized v2
    ccc19x$der_age_cat2 <- ccc19x$der_age
    temp <- ccc19x$der_age
    temp[which(ccc19x$der_age == '18-')] <- 0
    temp[which(ccc19x$der_age == '90+')] <- 90
    temp <- as.numeric(temp)
    ccc19x$der_age_cat2[which(temp < 18)] <- 'Less than 18 years'
    ccc19x$der_age_cat2[which(temp >= 18 & temp < 60)] <- '18-59 years'
    ccc19x$der_age_cat2[which(temp >= 60 & temp < 75)] <- '60-74 years'
    ccc19x$der_age_cat2[which(temp >= 75)] <- '75+ years'
    rm(temp)
    
    ccc19x$der_age_cat2 <- factor(ccc19x$der_age_cat2)
    summary(ccc19x$der_age_cat2[ccc19x$redcap_repeat_instrument == ''])
    
    #T17 & T18 Lower and upper bound years of cancer diagnosis
    {
      ccc19x$der_cancer_timing_lb <- NA
      ccc19x$der_cancer_timing_ub <- NA
      
      #Exact year is provided
      temp.ref <- which(!is.na(ccc19x$cancer_timing_yr))
      ccc19x$der_cancer_timing_lb[temp.ref] <- ccc19x$cancer_timing_yr[temp.ref]
      ccc19x$der_cancer_timing_ub[temp.ref] <- ccc19x$cancer_timing_yr[temp.ref]
      
      #At the time of COVID-19
      temp.ref <- which(ccc19x$cancer_timing == 0 & is.na(ccc19x$der_cancer_timing_lb))
      ccc19x$der_cancer_timing_lb[temp.ref] <- ccc19x$dx_year[temp.ref]
      ccc19x$der_cancer_timing_ub[temp.ref] <- ccc19x$dx_year[temp.ref]
      
      #Within a year of COVID-19
      temp.ref <- which(ccc19x$cancer_timing == 1 & is.na(ccc19x$der_cancer_timing_lb))
      ccc19x$der_cancer_timing_lb[temp.ref] <- ccc19x$dx_year[temp.ref] - 1
      ccc19x$der_cancer_timing_ub[temp.ref] <- ccc19x$dx_year[temp.ref]
      
      #Within 0-5 years of COVID-19
      temp.ref <- which(ccc19x$cancer_timing == 2 & is.na(ccc19x$der_cancer_timing_lb))
      ccc19x$der_cancer_timing_lb[temp.ref] <- ccc19x$dx_year[temp.ref] - 5
      ccc19x$der_cancer_timing_ub[temp.ref] <- ccc19x$dx_year[temp.ref]
      
      #More than 5 years before COVID-19
      temp.ref <- which(ccc19x$cancer_timing == 3 & is.na(ccc19x$der_cancer_timing_lb))
      ccc19x$der_cancer_timing_lb[temp.ref] <- ccc19x$dx_year[temp.ref] - ccc19x$der_age_trunc[temp.ref]
      ccc19x$der_cancer_timing_ub[temp.ref] <- ccc19x$dx_year[temp.ref] - 5
      
      temp <- summary(ccc19x$der_cancer_timing_lb[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_cancer_timing_lb',
                                 timestamp = Sys.time(), 
                                 values = paste(c(paste('Median:', temp[3]),
                                                  paste('IQR:', temp[2], '-', temp[5]),
                                                  paste('NA:', temp[7])), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      temp <- summary(ccc19x$der_cancer_timing_ub[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_cancer_timing_ub',
                                 timestamp = Sys.time(), 
                                 values = paste(c(paste('Median:', temp[3]),
                                                  paste('IQR:', temp[2], '-', temp[5]),
                                                  paste('NA:', temp[7])), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
    }
    
    "sex"
    #D2. Sex
    #recode other/prefer not to say as missing
    ccc19x$der_sex <- ccc19x$gender
    ccc19x$der_sex[which(ccc19x$der_sex == 0)] <- 'Female'
    ccc19x$der_sex[which(ccc19x$der_sex == 1)] <- 'Male'
    ccc19x$der_sex[which(ccc19x$der_sex %in% c(2:3))] <- NA
    
    #Factor
    ccc19x$der_sex <- as.factor(ccc19x$der_sex)
    
    temp <- summary(ccc19x$der_sex[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_sex',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ###############
    #Smoking status
    ###############
    
    #D03a. Derived variable for smoking status collapsing the former smoker variable
    ccc19x$der_smoking <- NA
    ccc19x$der_smoking[which(ccc19x$smoking_status == 3)] <- 'Never'
    ccc19x$der_smoking[which(ccc19x$smoking_status == 1)] <- 'Current'
    ccc19x$der_smoking[which(ccc19x$smoking_status %in% c("2", "2b", "2c", "2d", "2a"))] <- "Former"
    ccc19x$der_smoking[which(ccc19x$smoking_status == 99)] <- 'Unknown'
    
    #Factor
    ccc19x$der_smoking <- as.factor(ccc19x$der_smoking)
    ccc19x$der_smoking <- relevel(ccc19x$der_smoking, ref = 'Never')
    
    temp <- summary(ccc19x$der_smoking[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_smoking',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #D03b. Derived variable for smoking status collapsing the current/former smoker variables
    ccc19x$der_smoking2 <- NA
    ccc19x$der_smoking2[which(ccc19x$smoking_status == 3)] <- 'Never'
    ccc19x$der_smoking2[which(ccc19x$smoking_status %in% c("1","2", "2b", "2c", "2d", "2a"))] <- "Current or Former"
    ccc19x$der_smoking2[which(ccc19x$smoking_status == 99)] <- 'Unknown'
    
    #Factor
    ccc19x$der_smoking2 <- as.factor(ccc19x$der_smoking2)
    ccc19x$der_smoking2 <- relevel(ccc19x$der_smoking2, ref = 'Never')
    
    temp <- summary(ccc19x$der_smoking2[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_smoking2',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #D03c. Derived variable for smoking status collapsing the current/former smoker variables
    ccc19x$der_smoking3 <- NA
    ccc19x$der_smoking3[which(ccc19x$smoking_status == 1)] <- 'Current'
    ccc19x$der_smoking3[which(ccc19x$smoking_status %in% c("3","2", "2b", "2c", "2d", "2a"))] <- 'Never or Former'
    ccc19x$der_smoking3[which(ccc19x$smoking_status == 99)] <- 'Unknown'
    
    #Factor
    ccc19x$der_smoking3 <- as.factor(ccc19x$der_smoking3)
    ccc19x$der_smoking3 <- relevel(ccc19x$der_smoking3, ref = 'Never or Former')
    
    temp <- summary(ccc19x$der_smoking3[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_smoking3',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ###############
    #Race/Ethnicity
    ###############
    
    #D04a. Derived variable for race/ethnicity
    ccc19x$der_race <- NA
    
    ccc19x$der_race[which(ccc19x$race___2054_5 == 1 & ccc19x$race___2106_3 == 0 &
                            ccc19x$ethnicity %in% c('2186-5', 'UNK'))] <- "Non-Hispanic Black"
    ccc19x$der_race[which(ccc19x$race___2106_3 == 1 & ccc19x$race___2054_5 == 0 &
                            ccc19x$ethnicity %in% c('2186-5', 'UNK'))] <- "Non-Hispanic White"
    
    ccc19x$der_race[which((ccc19x$race___1002_5 == 1|ccc19x$race___2028_9 == 1|
                             ccc19x$race___2076_8 ==1|ccc19x$race___2131_1 == 1|
                             ccc19x$race___unk == 1) & ccc19x$race___2106_3 == 0 &
                            ccc19x$race___2054_5 == 0)] <- 'Other'
    
    #Overwrite "Other" race with Hispanic ethnicity
    ccc19x$der_race[which(ccc19x$ethnicity == "2135-2")] <- "Hispanic"
    
    #Factor
    ccc19x$der_race <- as.factor(ccc19x$der_race)
    ccc19x$der_race <- relevel(ccc19x$der_race, ref = 'Non-Hispanic White')
    
    temp <- summary(ccc19x$der_race[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_race',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #D04b. Collapse all but NHW
    ccc19x$der_race_collapsed <- ccc19x$der_race
    ccc19x$der_race_collapsed[ccc19x$der_race_collapsed %in% c('Hispanic', 'Non-Hispanic Black')] <- 'Other'
    ccc19x$der_race_collapsed <- droplevels(ccc19x$der_race_collapsed)
    
    temp <- summary(ccc19x$der_race_collapsed[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_race_collapsed',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #D04c. Derived variable for race/ethnicity including Asian
    ccc19x$der_race_v2 <- NA
    
    ccc19x$der_race_v2[which(ccc19x$race___2054_5 == 1 & ccc19x$race___2106_3 == 0 &
                               ccc19x$ethnicity %in% c('2186-5', 'UNK'))] <- "Non-Hispanic Black"
    ccc19x$der_race_v2[which(ccc19x$race___2106_3 == 1 & ccc19x$race___2054_5 == 0 &
                               ccc19x$ethnicity %in% c('2186-5', 'UNK'))] <- "Non-Hispanic White"
    
    #Overwrite the preceding if AAPI
    ccc19x$der_race_v2[which((ccc19x$race___2028_9 == 1|ccc19x$race___2076_8 ==1) &
                               ccc19x$ethnicity %in% c('2186-5', 'UNK'))] <- 'Non-Hispanic AAPI'
    
    #Other
    ccc19x$der_race_v2[which((ccc19x$race___1002_5 == 1|ccc19x$race___2131_1 == 1|
                                ccc19x$race___unk == 1) & !ccc19x$der_race_v2 %in% c('Non-Hispanic Black',
                                                                                     'Non-Hispanic White',
                                                                                     'Non-Hispanic AAPI'))] <- 'Other'
    
    #Overwrite "Other" with Hispanic ethnicity
    ccc19x$der_race_v2[which(ccc19x$ethnicity == "2135-2")] <- "Hispanic"
    
    #Factor
    ccc19x$der_race_v2 <- as.factor(ccc19x$der_race_v2)
    
    temp <- summary(ccc19x$der_race_v2[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_race_v2',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #D04d. Derived variable for race/ethnicity including Asian and Native Hawaiian/Pacific Islander kept separate
    ccc19x$der_race_v2b <- NA
    
    ccc19x$der_race_v2b[which(ccc19x$race___2054_5 == 1 & ccc19x$race___2106_3 == 0 &
                                ccc19x$ethnicity %in% c('2186-5', 'UNK'))] <- "Non-Hispanic Black"
    ccc19x$der_race_v2b[which(ccc19x$race___2106_3 == 1 & ccc19x$race___2054_5 == 0 &
                                ccc19x$ethnicity %in% c('2186-5', 'UNK'))] <- "Non-Hispanic White"
    
    #Overwrite the preceding if Asian
    ccc19x$der_race_v2b[which(ccc19x$race___2028_9 == 1 &
                                ccc19x$ethnicity %in% c('2186-5', 'UNK'))] <- 'Non-Hispanic Asian'
    
    #Overwrite the preceding if Native Hawaiian/Pacific Islander
    ccc19x$der_race_v2b[which(ccc19x$race___2076_8 ==1 &
                                ccc19x$ethnicity %in% c('2186-5', 'UNK'))] <- 'Non-Hispanic PI'
    
    #Other
    ccc19x$der_race_v2b[which((ccc19x$race___1002_5 == 1|ccc19x$race___2131_1 == 1|
                                 ccc19x$race___unk == 1) & !ccc19x$der_race_v2b %in% c('Non-Hispanic Black',
                                                                                       'Non-Hispanic White',
                                                                                       'Non-Hispanic Asian',
                                                                                       'Non-Hispanic PI'))] <- 'Other'
    
    #Overwrite "Other" with Hispanic ethnicity
    ccc19x$der_race_v2b[which(ccc19x$ethnicity == "2135-2")] <- "Hispanic"
    
    #Factor
    ccc19x$der_race_v2b <- as.factor(ccc19x$der_race_v2b)
    
    temp <- summary(ccc19x$der_race_v2b[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_race_v2b',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #D04e. Derived variable for race
    ccc19x$der_race_v3 <- NA
    
    ccc19x$der_race_v3[which(ccc19x$race___2054_5 == 1 & ccc19x$race___2106_3 == 0)] <- "Black"
    ccc19x$der_race_v3[which(ccc19x$race___2106_3 == 1 & ccc19x$race___2054_5 == 0)] <- "White"
    
    ccc19x$der_race_v3[which(ccc19x$race___1002_5 == 1|ccc19x$race___2028_9 == 1|
                               ccc19x$race___2076_8 ==1|ccc19x$race___2131_1 == 1|
                               (ccc19x$race___2106_3 == 1 & ccc19x$race___2054_5 == 1))] <- 'Other'
    
    #Factor
    ccc19x$der_race_v3 <- as.factor(ccc19x$der_race_v3)
    
    temp <- summary(ccc19x$der_race_v3[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_race_v3',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #D05. Ethnicity (simply factor and redefine levels, declare blanks as missing)
    ccc19x$der_ethnicity <- ccc19x$ethnicity
    ccc19x$der_ethnicity[which(ccc19x$der_ethnicity == "2135-2")] <- 'Hispanic/Latinx'
    ccc19x$der_ethnicity[which(ccc19x$der_ethnicity == "2186-5")] <- 'Not Hispanic/Latinx'
    ccc19x$der_ethnicity[which(ccc19x$der_ethnicity == "UNK")] <- 'Unknown'
    ccc19x$der_ethnicity[which(ccc19x$der_ethnicity == "")] <- NA
    ccc19x$der_ethnicity <- factor(ccc19x$der_ethnicity)
    summary(ccc19x$der_ethnicity[ccc19x$redcap_repeat_instrument == ''])
    
    #surgery
    #D6. derived variable indicating if there has been surgery within 4 weeks
    ccc19x$der_surgery <- NA
    ccc19x$der_surgery[which((ccc19x$recent_treatment %in% 1:2 & ccc19x$treatment_modality___14051 == 1)|
                               (ccc19x$recent_surgery == 1 & ccc19x$surgery_timing == '1'))] <- 'Recent surgery'
    ccc19x$der_surgery[which((ccc19x$treatment_modality___14051 == 1 & ccc19x$recent_treatment %in% c(3,88))|
                               (ccc19x$recent_surgery == 1 & ccc19x$surgery_timing %in% c('2', '3'))|
                               ccc19x$recent_surgery == 0
    )] <- 'None'
    ccc19x$der_surgery[which((ccc19x$treatment_modality___14051 == 1 & ccc19x$recent_treatment == 99)|
                               (ccc19x$recent_surgery == 1 & ccc19x$surgery_timing == 'UNK')|
                               ccc19x$recent_surgery == 99
    )] <- 'Unknown'
    
    #Factor
    ccc19x$der_surgery <- as.factor(ccc19x$der_surgery)
    ccc19x$der_surgery <- relevel(ccc19x$der_surgery, ref = 'None')
    summary(ccc19x$der_surgery[ccc19x$redcap_repeat_instrument == ''])
    
    #D18. derived variable indicating if there has been surgery within 3 months
    ccc19x$der_surgery2 <- NA
    
    ccc19x$der_surgery2[which(((ccc19x$recent_treatment %in% 1:3|ccc19x$hx_treatment == 1) & 
                                 ccc19x$treatment_modality___14051 == 1)|
                               (ccc19x$recent_surgery == 1 & ccc19x$surgery_timing %in% 1:2))] <- 'Recent surgery'
    
    ccc19x$der_surgery2[which(((ccc19x$treatment_modality___14051 == 1 & ccc19x$recent_treatment %in% c(88))|
                               (ccc19x$recent_surgery == 1 & ccc19x$surgery_timing %in% 3)|
                               ccc19x$recent_surgery == 0) & is.na(ccc19x$der_surgery2)
    )] <- 'None'
    
    ccc19x$der_surgery2[which(((ccc19x$treatment_modality___14051 == 1 & ccc19x$recent_treatment == 99)|
                               (ccc19x$recent_surgery == 1 & ccc19x$surgery_timing == 'UNK')|
                               ccc19x$recent_surgery == 99) & is.na(ccc19x$der_surgery2)
    )] <- 'Unknown'
    
    #Factor
    ccc19x$der_surgery2 <- as.factor(ccc19x$der_surgery2)
    ccc19x$der_surgery2 <- relevel(ccc19x$der_surgery2, ref = 'None')
    summary(ccc19x$der_surgery2[ccc19x$redcap_repeat_instrument == ''])
    
    #D19. Baseline VTE
    ccc19x$der_VTE_baseline <- NA
    
    #Present
    ccc19x$der_VTE_baseline[which(ccc19x$significant_comorbidities___128053003 ==1|
                                    ccc19x$significant_comorbidities___59282003 == 1)] <- 1
    
    #Not present, something else checked besides unknown
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_com') & !grepl(colnames(ccc19x), pattern = '128053003|59282003|unk'))
    for(i in which(is.na(ccc19x$der_VTE_baseline)))
      if(any(ccc19x[i,temp.ref] == 1) & !is.na(any(ccc19x[i,temp.ref] == 1))) ccc19x$der_VTE_baseline[i] <- 0
    
    #Unknown
    ccc19x$der_VTE_baseline[which(ccc19x$significant_comorbidities___unk ==1 &
                                    is.na(ccc19x$der_VTE_baseline))] <- 99
    
    ccc19x$der_VTE_baseline <- as.factor(ccc19x$der_VTE_baseline)
    
    temp <- summary(ccc19x$der_VTE_baseline[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_VTE_baseline',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ############
    #D14. Region
    ccc19x$der_region <- NA
    
    ccc19x$der_region[which(ccc19x$state_of_patient_residence %in% c("ME", "NH", "VT", "MA", "RI", "CT", 
                                                                     "PA", "NY", "NJ"))] <- "US Northeast"
    ccc19x$der_region[which(ccc19x$state_of_patient_residence %in% c("WI", "MI", "IL", "IN", "OH", "MO", "ND", 
                                                                     "SD", "NE", "KS", "MN", "IA"))] <- "US Midwest"
    ccc19x$der_region[which(ccc19x$state_of_patient_residence %in% c("DE","MD", "DC", "VA", "WV", "NC", 
                                                                     "SC", "GA", "FL", "TN", "KY", "MS", "AL", 
                                                                     "OK", "TX", "LA", "AR"))] <- "US South"
    ccc19x$der_region[which(ccc19x$state_of_patient_residence %in% c("ID", "MT", "WY", "NV", "UT", "CO", "AZ", 
                                                                     "NM", "AK", "WA", "OR", "CA", "HI"))] <- "US West"
    
    ccc19x$der_region[which(ccc19x$country_of_patient_residen == 39)] <- "Canada"
    ccc19x$der_region[which(ccc19x$country_of_patient_residen == 197)] <- "Spain"
    ccc19x$der_region[is.na(ccc19x$der_region) & ccc19x$redcap_repeat_instrument == ''] <- 'Other'
    
    #Factor
    ccc19x$der_region <- as.factor(ccc19x$der_region)
    
    summary(ccc19x$der_region[ccc19x$redcap_repeat_instrument == ''])
    
    #D14a. Region with ex-US collapsed
    ccc19x$der_region_v2 <- as.character(ccc19x$der_region)
    ccc19x$der_region_v2[ccc19x$country_of_patient_residen != 1 & ccc19x$redcap_repeat_instrument == ''] <- 'Non-US'
    ccc19x$der_region_v2[ccc19x$country_of_patient_residen == 1 & ccc19x$der_region_v2 == 'Other'] <- 'Undesignated US'
    
    #Factor
    ccc19x$der_region_v2 <- as.factor(ccc19x$der_region_v2)
    
    temp <- summary(ccc19x$der_region_v2[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_region_v2',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #D14b. Region with US collapsed
    ccc19x$der_region_v3 <- NA
    ccc19x$der_region_v3[ccc19x$country_of_patient_residen != 1 & ccc19x$redcap_repeat_instrument == ''] <- 'Non-US'
    ccc19x$der_region_v3[ccc19x$country_of_patient_residen == 1 & ccc19x$redcap_repeat_instrument == ''] <- 'US'
    
    #Factor
    ccc19x$der_region_v3 <- as.factor(ccc19x$der_region_v3)
    
    temp <- summary(ccc19x$der_region_v3[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_region_v3',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #D15. US Census Division
    ccc19x$der_division <- NA
    ccc19x$der_division[which(ccc19x$state_of_patient_residence %in% c("ME", "NH", "VT", "MA", "RI", "CT"))] <- "New England"
    ccc19x$der_division[which(ccc19x$state_of_patient_residence %in% c("PA", "NY", "NJ"))] <- "Middle Atlantic"
    ccc19x$der_division[which(ccc19x$state_of_patient_residence %in% c("WI", "MI", "IL", "IN", "OH"))] <- "East North Central"
    ccc19x$der_division[which(ccc19x$state_of_patient_residence %in% c("MO", "ND", "SD", "NE", "KS", "MN", "IA"))] <- "West North Central"
    ccc19x$der_division[which(ccc19x$state_of_patient_residence %in% c("DE","MD", "DC", "VA", "WV", "NC", 
                                                                       "SC", "GA", "FL"))] <- "South Atlantic"
    ccc19x$der_division[which(ccc19x$state_of_patient_residence %in% c("TN", "KY", "MS", "AL"))] <- "East South Central"
    ccc19x$der_division[which(ccc19x$state_of_patient_residence %in% c("OK", "TX", "LA", "AR"))] <- "West South Central"
    ccc19x$der_division[which(ccc19x$state_of_patient_residence %in% c("ID", "MT", "WY", "NV", "UT", "CO", "AZ", 
                                                                       "NM"))] <- "Mountain"
    ccc19x$der_division[which(ccc19x$state_of_patient_residence %in% c("AK", "WA", "OR", "CA", "HI"))] <- "Pacific"
    
    #Factor
    ccc19x$der_division <- as.factor(ccc19x$der_division)
    
    temp <- summary(ccc19x$der_division[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_division',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #D16. Sunbelt residence
    ccc19x$der_sunbelt <- NA
    
    ccc19x$der_sunbelt[which(ccc19x$state_of_patient_residence %in% c("AL", "AR", "AZ", "CA", "CO", "GA", "FL", 
                                                                      "KS", "LA", "MS", "NC", "NM", "NV", 
                                                                      "SC", "TN", "OK", "TX", "UT"))] <- 1
    ccc19x$der_sunbelt[which(ccc19x$state_of_patient_residence %in% c("ME", "NH", "VT", "MA", "RI", "CT", 
                                                                     "PA", "NY", "NJ", "WI", "MI", "IL", "IN", "OH", "MO", "ND", 
                                                                     "SD", "NE", "MN", "IA", "DE","MD", "DC", 
                                                                     "VA", "WV", "KY", "ID", "MT", "WY",  
                                                                     "AK", "WA", "OR", "HI"))] <- 0
    #Spain & Mexico
    ccc19x$der_sunbelt[which(ccc19x$country_of_patient_residen %in% c(140,197))] <- 1
    
    #Canada
    ccc19x$der_sunbelt[which(ccc19x$country_of_patient_residen %in% c(39))] <- 0
    
    #Factor
    ccc19x$der_sunbelt <- as.factor(ccc19x$der_sunbelt)
    
    temp <- summary(ccc19x$der_sunbelt[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_sunbelt',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    
    #D20. Insurance 
    ccc19x$der_insurance <- NA
    
    #Uninsured
    ccc19x$der_insurance[which(ccc19x$insurance___0 == 1 & ccc19x$insurance___1 == 0 & ccc19x$insurance___2 == 0 &
                                 ccc19x$insurance___3 == 0 & ccc19x$insurance___4 == 0 & ccc19x$insurance___99 == 0)] <- 'Uninsured'
    
    #Private insurance +/- any other
    ccc19x$der_insurance[which(ccc19x$insurance___1 == 1)] <- 'Private +/- other'
    
    #Medicaid alone
    ccc19x$der_insurance[which(ccc19x$insurance___0 == 0 & ccc19x$insurance___1 == 0 & ccc19x$insurance___2 == 1 &
                                 ccc19x$insurance___3 == 0 & ccc19x$insurance___4 == 0 & ccc19x$insurance___99 == 0)] <- 'Medicaid alone'
    
    #Medicare alone
    ccc19x$der_insurance[which(ccc19x$insurance___0 == 0 & ccc19x$insurance___1 == 0 & ccc19x$insurance___2 == 0 &
                                 ccc19x$insurance___3 == 1 & ccc19x$insurance___4 == 0 & ccc19x$insurance___99 == 0)] <- 'Medicare alone'
    
    #Medicare/Medicaid
    ccc19x$der_insurance[which(ccc19x$insurance___2 == 1 &ccc19x$insurance___3 == 1)] <- 'Medicare/Medicaid +/- other'
    
    #Other government
    ccc19x$der_insurance[which(ccc19x$insurance___4 == 1)] <- 'Other government +/- other'
    
    #Unknown
    ccc19x$der_insurance[which(ccc19x$insurance___0 == 0 & ccc19x$insurance___1 == 0 & ccc19x$insurance___2 == 0 &
                                 ccc19x$insurance___3 == 0 & ccc19x$insurance___4 == 0 & ccc19x$insurance___99 == 1)] <- 'Unknown'
    
    #Factor
    ccc19x$der_insurance <- as.factor(ccc19x$der_insurance)
    
    temp <- summary(ccc19x$der_insurance[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_insurance',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
  }
  print('Demographics completed')
  
  ##############
  #Comorbidities
  ##############
  {
    #Something is checked besides unknown (including none)
    master_comorbid <- rep(NA,nrow(ccc19x))
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '^significant_') & !grepl(colnames(ccc19x), pattern = 'unk'))
    for(i in 1:nrow(ccc19x))
    {
      temp <- ccc19x[i,temp.ref]
      temp <- temp[!is.na(temp)]
      if(any(temp == 1)) master_comorbid[i] <- 1
    }
    
    #C01. BMI, with derived BMI for records that have height and weight recorded and not BMI
    {
      ccc19x$der_bmi <- ccc19x$bmi
      
      #Records with height/weight recorded, no BMI
      temp.ref <- which(is.na(ccc19x$der_bmi) & grepl(ccc19x$height, pattern = '[0-9]') & grepl(ccc19x$weight, pattern = '[0-9]'))
      
      #removing rows that are missing height or weight
      temp <- ccc19x[temp.ref,c('height', 'weight')]
      temp$height <- trimws(temp$height)
      temp$weight <- trimws(temp$weight)
      
      #fixing transposed data
      temp[grepl("kg", temp$height, ignore.case = T), c("height", "weight")] <- temp[grepl("kg", temp$height, ignore.case = T), c("weight", "height")]
      temp[grepl("lb", temp$height, fixed = TRUE), c("height", "weight")] <- temp[grepl("lb", temp$height, fixed = TRUE), c("weight", "height")]
      temp[grepl("'", temp$weight, fixed = TRUE), c("height", "weight")] <- temp[grepl("'", temp$weight, fixed = TRUE), c("weight", "height")]
      
      #converting all heights to meters
      
      #fixing data in the format ft'in" (e.g 5'11"), could also be used in a similar way to fix data in format "x feet y inches"
      x <- temp[grepl("'", temp$height, fixed = TRUE), "height"]
      x <- gsub("'", "", x)
      x <- gsub("\"", "", x)
      x <- gsub(" ", "", x)
      y <- strtoi(substr(x, 1, 1))
      z <- strtoi(substr(x, 2, 3))
      x <- y * 12 + z
      x <- toString(x)
      x <- strsplit(x, ", ")
      x <- paste(x[[1]], 'inches')
      temp[grepl("'", temp$height, fixed = TRUE), "height"] <- x
      
      #fixed the height entries in the format "x foot y inches"
      temp$height <- gsub(temp$height, pattern = 'foot', replacement = 'feet')
      temp$height[which(temp$height == '5 ft 2.5 in')] <- '62.5 inches'
      temp$height[grep(temp$height, pattern = '5 f[e]{0,2}t 1 in')] <- '61 inches'
      temp$height[grep(temp$height, pattern = '5 f[e]{0,2}t 3 in')] <- '63 inches'
      temp$height[grep(temp$height, pattern = '5 f[e]{0,2}t 4 in')] <- '64 inches'
      temp$height[grep(temp$height, pattern = '5 f[e]{0,2}t 5 in')] <- '65 inches'
      temp$height[grep(temp$height, pattern = '5 f[e]{0,2}t 6 in')] <- '66 inches'
      temp$height[grep(temp$height, pattern = '5[ ]?f[e]{0,2}t[ ]?7[ ]?in')] <- '67 inches'
      temp$height[grep(temp$height, pattern = '5 f[e]{0,2}t 8 in')] <- '68 inches'
      temp$height[grep(temp$height, pattern = '5 f[e]{0,2}t 9 in')] <- '69 inches'
      temp$height[grep(temp$height, pattern = '5 f[e]{0,2}t 10 in')] <- '70 inches'
      temp$height[grep(temp$height, pattern = '5 f[e]{0,2}t 11 in')] <- '71 inches'
      temp$height[grep(temp$height, pattern = '6 f[e]{0,2}t')] <- '72 inches'
      temp$height[which(temp$height == '6 feet 2 inches')] <- '74 inches'
      
      #converted height strings into double values and put them in a new column
      temp$mheight <- temp$height
      temp$mheight <- gsub(temp$mheight, pattern = 'cm|[mM]|meters| |in|inches', replacement = '', ignore.case = T)
      if(any(grepl(temp$mheight, pattern = '[a-z]')))
      {
        err <- temp[grepl(temp$mheight, pattern = '[a-z]'),]
        out <- ccc19x$record_id[temp.ref[grep(temp$mheight, pattern = '[a-z]')]]
      }
      
      temp$mheight <- as.numeric(temp$mheight)
      
      #converting each height in the mheight double value column into height in meters (values greater than 100 are assumed to be in centimeters)
      temp.ref2 <- grep(temp$height, pattern = 'cm')
      temp$mheight[temp.ref2] <- temp$mheight[temp.ref2]/100
      
      temp.ref2 <- grep(temp$height, pattern = 'in')
      temp$mheight[temp.ref2] <- temp$mheight[temp.ref2]*0.0254
      
      temp.ref2 <- which(temp$mheight > 48 & temp$mheight < 100)
      temp$mheight[temp.ref2] <- temp$mheight[temp.ref2]*0.0254
      
      temp.ref2 <- which(temp$mheight >= 100)
      temp$mheight[temp.ref2] <- temp$mheight[temp.ref2]/100
      
      #converting weight value strings to double values and entering them into a new column
      temp$kgweight <- temp$weight
      temp$kgweight <- gsub(temp$kgweight, pattern = 'lbs[\\.]?|lb|pounds|kg| |', replacement = '', ignore.case = T)
      if(any(grepl(temp$kgweight, pattern = '[a-z]', ignore.case = T))) 
        err <- data.frame(record_id = ccc19x$record_id[temp.ref[grepl(temp$kgweight, pattern = '[a-z]', ignore.case = T)]],
                          error = temp$kgweight[grepl(temp$kgweight, pattern = '[a-z]', ignore.case = T)],
                          stringsAsFactors = F)
      
      temp$kgweight <- as.double(temp$kgweight)
      
      #Convert lbs into kg
      temp.ref2 <- grep(temp$weight, pattern = 'lb|pound')
      temp$kgweight[temp.ref2] <- temp$kgweight[temp.ref2]*0.454
      
      #No units, height in English units
      temp.ref2 <- which(!grepl(temp$weight, pattern = 'lb|pound|kg') & grepl(temp$height, pattern = 'in'))
      temp$kgweight[temp.ref2] <- temp$kgweight[temp.ref2]*0.454
      
      #No units, magnitude of height >3 and <100
      temp.ref2 <- which(!grepl(temp$weight, pattern = 'lb|pound|kg') &
                           as.numeric(temp$mheight) > 3 & as.numeric(temp$mheight) < 100)
      temp$kgweight[temp.ref2] <- temp$kgweight[temp.ref2]*0.454
      
      #calculating bmi and storing final result into a new copy
      temp$bmi <- temp$kgweight / (temp$mheight)^2
      
      ccc19x$der_bmi[temp.ref] <- temp$bmi
      
      temp <- summary(ccc19x$der_bmi[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_bmi',
                                 timestamp = Sys.time(),
                                 values = paste(c(paste('Median:', round(temp[3],digits=2), 'kg/m2'),
                                                  paste('IQR:', round(temp[2],digits=2), '-', round(temp[5],digits=2)),
                                                  paste('NAs:', temp[7])), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
    }
   
    ##C02. derived variable coding the obesity status (binary)
    {
      ccc19x$der_obesity_v2 <- NA
      
      ccc19x$der_obesity_v2[which(ccc19x$significant_comorbidities___414916001 == 1 |
                                    ccc19x$significant_comorbidities___238136002 == 1)] <- 1
      
      #Records with numeric BMI recorded or derived as >= 30
      ccc19x$der_obesity_v2[which(ccc19x$der_bmi >= 30)] <- 1
      
      #Records with numeric BMI recorded or derived as < 30 (do not overwrite, for now)
      ccc19x$der_obesity_v2[which(ccc19x$der_bmi < 30 & is.na(ccc19x$der_obesity_v2))] <- 0
      
      #Not specified (map to Not obese for now)
      ccc19x$der_obesity_v2[which(ccc19x$significant_comorbidities___238136002 == 0 &
                                    ccc19x$significant_comorbidities___414916001 == 0 &
                                    is.na(ccc19x$der_obesity_v2))] <- 0
      
      #Revert "not obese" to NA if all the significant comorbidities are unchecked and BMI data not available
      temp.ref <- grep(colnames(ccc19x), pattern = 'significant_comorbidities___')
      for(i in which(ccc19x$redcap_repeat_instrument == ''))
        if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_obesity_v2[i] == 0) ccc19x$der_obesity_v2[i] <- NA
      
      #Unknown
      ccc19x$der_obesity_v2[which(ccc19x$significant_comorbidities___unk == 1 & ccc19x$der_obesity_v2 == 0)] <- 99
      
      #Factor
      ccc19x$der_obesity_v2 <- as.factor(ccc19x$der_obesity_v2)
      
      temp <- summary(ccc19x$der_obesity_v2[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_obesity_v2',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
    }
    
    ##C02b. derived variable coding the morbid obesity status (binary) with BMI cutoff of 40
    {
      ccc19x$der_morbid_obesity_bmi40 <- NA
      
      ccc19x$der_morbid_obesity_bmi40[which(ccc19x$significant_comorbidities___238136002 == 1)] <- 1
      
      #Records with numeric BMI recorded or derived as >= 40
      ccc19x$der_morbid_obesity_bmi40[which(ccc19x$der_bmi >= 40)] <- 1
      
      #Records with numeric BMI recorded or derived as < 40 (do not overwrite, for now)
      ccc19x$der_morbid_obesity_bmi40[which(ccc19x$der_bmi < 40 & is.na(ccc19x$der_morbid_obesity_bmi40))] <- 0
      
      #Not specified
      ccc19x$der_morbid_obesity_bmi40[which(ccc19x$significant_comorbidities___238136002 == 0 &
                                              is.na(ccc19x$der_morbid_obesity_bmi40))] <- 0
      
      #Revert "not obese" to NA if all the significant comorbidities are unchecked and BMI data not available
      temp.ref <- grep(colnames(ccc19x), pattern = 'significant_comorbidities___')
      for(i in which(ccc19x$redcap_repeat_instrument == ''))
        if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_morbid_obesity_bmi40[i] == 0) ccc19x$der_morbid_obesity_bmi40[i] <- NA
      
      #Unknown
      ccc19x$der_morbid_obesity_bmi40[which(ccc19x$significant_comorbidities___unk == 1 & ccc19x$der_morbid_obesity_bmi40 == 0)] <- 99
      
      #Factor
      ccc19x$der_morbid_obesity_bmi40 <- as.factor(ccc19x$der_morbid_obesity_bmi40)
      
      temp <- summary(ccc19x$der_morbid_obesity_bmi40[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_morbid_obesity_bmi40',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
    }
    
    ##C02c. derived variable coding the morbid obesity status (binary, cutoff BMI 35)
    {
      ccc19x$der_morbid_obesity_bmi35 <- NA
      
      ccc19x$der_morbid_obesity_bmi35[which(ccc19x$significant_comorbidities___238136002 == 1)] <- 1
      
      #Records with numeric BMI recorded or derived as >= 35
      ccc19x$der_morbid_obesity_bmi35[which(ccc19x$der_bmi >= 35)] <- 1
      
      #Records with numeric BMI recorded or derived as < 35 (do not overwrite, for now)
      ccc19x$der_morbid_obesity_bmi35[which(ccc19x$der_bmi < 35 & is.na(ccc19x$der_morbid_obesity_bmi35))] <- 0
      
      #Not specified
      ccc19x$der_morbid_obesity_bmi35[which(ccc19x$significant_comorbidities___238136002 == 0 &
                                              is.na(ccc19x$der_morbid_obesity_bmi35))] <- 0
      
      #Revert "not obese" to NA if all the significant comorbidities are unchecked and BMI data not available
      temp.ref <- grep(colnames(ccc19x), pattern = 'significant_comorbidities___')
      for(i in which(ccc19x$redcap_repeat_instrument == ''))
        if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_morbid_obesity_bmi35[i] == 0) ccc19x$der_morbid_obesity_bmi35[i] <- NA
      
      #Unknown
      ccc19x$der_morbid_obesity_bmi35[which(ccc19x$significant_comorbidities___unk == 1 & ccc19x$der_morbid_obesity_bmi35 == 0)] <- 99
      
      #Factor
      ccc19x$der_morbid_obesity_bmi35 <- as.factor(ccc19x$der_morbid_obesity_bmi35)
      
      temp <- summary(ccc19x$der_morbid_obesity_bmi35[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_morbid_obesity_bmi35',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
    }
    
    #C02d. Obesity categorical
    ccc19x$der_obesity_cat <- as.character(ccc19x$der_obesity_v2)
    ccc19x$der_obesity_cat[which(ccc19x$der_morbid_obesity_bmi40 == 1)] <- 2
    ccc19x$der_obesity_cat[which(ccc19x$der_morbid_obesity_bmi40 == 99)] <- 99
    ccc19x$der_obesity_cat <- factor(ccc19x$der_obesity_cat)
    
    temp <- summary(ccc19x$der_obesity_cat[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_obesity_cat',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #C01a. BMI categorical
    {
      ccc19x$der_bmi_cat <- NA
      
      ccc19x$der_bmi_cat[which(ccc19x$der_bmi < 18.5)] <- 'Underweight (BMI < 18.5)'
      ccc19x$der_bmi_cat[which(ccc19x$der_bmi >= 18.5 & ccc19x$der_bmi < 25)] <- 'Normal (BMI 18.5-24.9)'
      ccc19x$der_bmi_cat[which(ccc19x$der_bmi >= 25 & ccc19x$der_bmi < 30)] <- 'Overweight (BMI 25-29.9)'
      ccc19x$der_bmi_cat[which(ccc19x$der_bmi >= 30 & ccc19x$der_bmi < 35)] <- 'Obesity Class I (BMI 30-34.9)'
      ccc19x$der_bmi_cat[which(ccc19x$der_bmi >= 35 & ccc19x$der_bmi < 40)] <- 'Obesity Class II (BMI 35-39.9)'
      ccc19x$der_bmi_cat[which(ccc19x$der_bmi >= 40)] <- 'Obesity Class III (BMI 40+)'
      
      #Factor
      ccc19x$der_bmi_cat <- factor(ccc19x$der_bmi_cat)
      
      temp <- summary(ccc19x$der_bmi_cat[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_bmi_cat',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
    }
    
    #C03. Number of comorbidities (just factor)
    ccc19x$der_comorbid_no <- factor(ccc19x$comorbid_no)
    summary(ccc19x$der_comorbid_no)
    
    #C03a. Simplified # of comorbidities
    ccc19x$der_comorbid_no_collapsed <- as.character(ccc19x$der_comorbid_no)
    ccc19x$der_comorbid_no_collapsed[which(ccc19x$der_comorbid_no %in% c('2', '3', '4'))] <- 2
    ccc19x$der_comorbid_no_collapsed <- factor(ccc19x$der_comorbid_no_collapsed)
    summary(ccc19x$der_comorbid_no_collapsed[ccc19x$redcap_repeat_instrument == ''])
    
    #C03b. Simplified # of comorbidities 2
    ccc19x$der_comorbid_no_collapsed2 <- as.character(ccc19x$der_comorbid_no)
    ccc19x$der_comorbid_no_collapsed2[which(ccc19x$der_comorbid_no %in% c('1', '2'))] <- '1 to 2'
    ccc19x$der_comorbid_no_collapsed2[which(ccc19x$der_comorbid_no %in% c('3', '4'))] <- '3+'
    ccc19x$der_comorbid_no_collapsed2 <- factor(ccc19x$der_comorbid_no_collapsed2)
    summary(ccc19x$der_comorbid_no_collapsed2[ccc19x$redcap_repeat_instrument == ''])
    
    #C04. Diabetes mellitus
    ccc19x$der_dm2 <- NA
    ccc19x$der_dm2[which(ccc19x$significant_comorbidities___73211009 == 1|
                           ccc19x$significant_comorbidities___190388001 == 1)] <- 1
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___73211009|significant_comorbidities___190388001|significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(any(ccc19x[i,temp.ref]) & ccc19x$significant_comorbidities___73211009[i] == 0 &
         ccc19x$significant_comorbidities___190388001[i] == 0) ccc19x$der_dm2[i] <- 0
    }
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$significant_comorbidities___unk[i] == 1) ccc19x$der_dm2[i] <- 99
    }
    
    ccc19x$der_dm2 <- factor(ccc19x$der_dm2)
    
    temp <- summary(ccc19x$der_dm2[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_dm2',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #C14. Autoimmune disease including IBD
    ccc19x$der_autoimmune <- NA
    ccc19x$der_autoimmune[which(ccc19x$significant_comorbidities___24526004 == 1 |
                                  ccc19x$significant_comorbidities___85828009 == 1)] <- 1
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___73211009|significant_comorbidities___190388001|significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(any(ccc19x[i,temp.ref]) & ccc19x$significant_comorbidities___73211009[i] == 0 &
         ccc19x$significant_comorbidities___190388001[i] == 0) ccc19x$der_autoimmune[i] <- 0
    }
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$significant_comorbidities___unk[i] == 1) ccc19x$der_autoimmune[i] <- 99
    }
    
    ccc19x$der_autoimmune <- factor(ccc19x$der_autoimmune)
    
    temp <- summary(ccc19x$der_autoimmune[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_autoimmune',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #C05. Immunosuppressed
    ccc19x$der_immunosuppressed <- NA
    
    #First, determine NOT immunosuppressed per comorbidity variable
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___38013005|significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(any(ccc19x[i,temp.ref]) & ccc19x$significant_comorbidities___38013005[i] == 0) ccc19x$der_immunosuppressed[i] <- 0
    }
    
    #Next, rule in immunosuppression
    
    #1. Immunosuppression comorbidity checked
    ccc19x$der_immunosuppressed[which(ccc19x$significant_comorbidities___38013005 == 1)] <- 1
    
    #2. Receiving >20 mg/d prednisone equivalents at baseline
    ccc19x$der_immunosuppressed[which(ccc19x$steroid_specific_2 %in% 2:3)] <- 1
    
    #3. Receiving "immunuosuppressants" at baseline
    ccc19x$der_immunosuppressed[which(ccc19x$concomitant_meds___l04a == 1)] <- 1
    
    #Unknown
    ccc19x$der_immunosuppressed[which((ccc19x$concomitant_meds___unk == 1|ccc19x$significant_comorbidities___unk == 1) &
                                        is.na(ccc19x$der_immunosuppressed))] <- 99
    
    ccc19x$der_immunosuppressed <- factor(ccc19x$der_immunosuppressed)
    summary(ccc19x$der_immunosuppressed[ccc19x$redcap_repeat_instrument == ''])
    
    #C06. Pulmonary comorbidities
    ccc19x$der_pulm <- NA
    ccc19x$der_pulm[which(ccc19x$significant_comorbidities___13645005 == 1|
                            ccc19x$significant_comorbidities___19829001 == 1|
                            ccc19x$significant_comorbidities___195967001 == 1|
                            ccc19x$significant_comorbidities___84004001 == 1|
                            ccc19x$significant_comorbidities___427046006 == 1|
                            ccc19x$o2_requirement == 1)] <- 1
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), 
                        pattern = 'comorbidities___13645005|comorbidities___19829001|comorbidities___195967001|comorbidities___84004001|comorbidities___427046006|comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(any(ccc19x[i,temp.ref]) & all(c(ccc19x$significant_comorbidities___13645005[i] == 0,
                                         ccc19x$significant_comorbidities___19829001[i] == 0,
                                         ccc19x$significant_comorbidities___195967001[i] == 0,
                                         ccc19x$significant_comorbidities___84004001[i] == 0,
                                         ccc19x$significant_comorbidities___427046006[i] == 0)
      )) ccc19x$der_pulm[i] <- 0
    }
    
    #Declare missing values to be No if patient is known NOT to have an O2 requirement
    temp.ref <- which(ccc19x$o2_requirement == 0 & is.na(ccc19x$der_pulm))
    ccc19x$der_pulm[temp.ref] <- 0
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$significant_comorbidities___unk[i] == 1) ccc19x$der_pulm[i] <- 99
    }
   
    ccc19x$der_pulm <- factor(ccc19x$der_pulm)
    
    temp <- summary(ccc19x$der_pulm[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_pulm',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #C07. Cardiovascular comorbidity (CAD, CHF, Afib, arrhythmia NOS, PVD, CVA, cardiac disease NOS)
    ccc19x$der_card <- NA
    ccc19x$der_card[which( ccc19x$significant_comorbidities___53741008 == 1|
                             ccc19x$significant_comorbidities___56265001 == 1|
                             ccc19x$significant_comorbidities___42343007 == 1|
                             ccc19x$significant_comorbidities___698247007 == 1|
                             ccc19x$significant_comorbidities___49436004 == 1|
                             ccc19x$significant_comorbidities___400047006 == 1|
                             ccc19x$significant_comorbidities___275526006 == 1
    )] <- 1
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'comorbidities___53741008|comorbidities___42343007|comorbidities___698247007|comorbidities___49436004|comorbidities___56265001|comorbidities___400047006|comorbidities___275526006|comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(any(ccc19x[i,temp.ref]) & all(c(ccc19x$significant_comorbidities___53741008[i] == 0,
                                         ccc19x$significant_comorbidities___56265001[i] == 0,
                                         ccc19x$significant_comorbidities___42343007[i] == 0,
                                         ccc19x$significant_comorbidities___698247007[i] == 0,
                                         ccc19x$significant_comorbidities___49436004[i] == 0,
                                         ccc19x$significant_comorbidities___400047006[i] == 0,
                                         ccc19x$significant_comorbidities___275526006[i] == 0)
      )) ccc19x$der_card[i] <- 0
    }
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$significant_comorbidities___unk[i] == 1) ccc19x$der_card[i] <- 99
    }
    
    ccc19x$der_card <- factor(ccc19x$der_card)
    
    temp <- summary(ccc19x$der_card[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_card',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #C07a. Cardiovascular comorbidity v2 (CAD, CHF, PVD, CVA)
    ccc19x$der_card_v2 <- NA
    ccc19x$der_card_v2[which( ccc19x$significant_comorbidities___53741008 == 1|#CAD
                                ccc19x$significant_comorbidities___42343007 == 1|#CHF
                                ccc19x$significant_comorbidities___400047006 == 1|#PVD
                                ccc19x$significant_comorbidities___275526006 == 1#CVA
    )] <- 1
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'comorbidities___53741008|comorbidities___42343007|comorbidities___400047006|comorbidities___275526006|comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(any(ccc19x[i,temp.ref]) & all(c(ccc19x$significant_comorbidities___53741008[i] == 0,
                                         ccc19x$significant_comorbidities___42343007[i] == 0,
                                         ccc19x$significant_comorbidities___400047006[i] == 0,
                                         ccc19x$significant_comorbidities___275526006[i] == 0)
      )) ccc19x$der_card_v2[i] <- 0
    }
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$significant_comorbidities___unk[i] == 1) ccc19x$der_card_v2[i] <- 99
    }
    
    ccc19x$der_card_v2 <- factor(ccc19x$der_card_v2)
    
    temp <- summary(ccc19x$der_card_v2[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_card_v2',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #C07a1. Cardiovascular comorbidity v3 (CAD, PVD, CVA)
    ccc19x$der_card_v3 <- NA
    ccc19x$der_card_v3[which( ccc19x$significant_comorbidities___53741008 == 1|#CAD
                                ccc19x$significant_comorbidities___400047006 == 1|#PVD
                                ccc19x$significant_comorbidities___275526006 == 1#CVA
    )] <- 1
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'comorbidities___53741008|comorbidities___400047006|comorbidities___275526006|comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(any(ccc19x[i,temp.ref]) & all(c(ccc19x$significant_comorbidities___53741008[i] == 0,
                                         ccc19x$significant_comorbidities___400047006[i] == 0,
                                         ccc19x$significant_comorbidities___275526006[i] == 0)
      )) ccc19x$der_card_v3[i] <- 0
    }
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$significant_comorbidities___unk[i] == 1) ccc19x$der_card_v3[i] <- 99
    }
    
    ccc19x$der_card_v3 <- factor(ccc19x$der_card_v3)
    
    temp <- summary(ccc19x$der_card_v3[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_card_v3',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #C07b. CAD comorbidity
    ccc19x$der_CAD_bl <- NA
    ccc19x$der_CAD_bl[which( ccc19x$significant_comorbidities___53741008 == 1)] <- 1
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'comorbidities___53741008|comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(any(ccc19x[i,temp.ref]) & all(c(ccc19x$significant_comorbidities___53741008[i] == 0)
      )) ccc19x$der_CAD_bl[i] <- 0
    }
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$significant_comorbidities___unk[i] == 1) ccc19x$der_CAD_bl[i] <- 99
    }
    
    ccc19x$der_CAD_bl <- factor(ccc19x$der_CAD_bl)
    
    temp <- summary(ccc19x$der_CAD_bl[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_CAD_bl',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #C07c. CHF comorbidity
    ccc19x$der_CHF_bl <- NA
    ccc19x$der_CHF_bl[which( ccc19x$significant_comorbidities___42343007 == 1)] <- 1
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'comorbidities___42343007|comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(any(ccc19x[i,temp.ref]) & all(c(ccc19x$significant_comorbidities___42343007[i] == 0)
      )) ccc19x$der_CHF_bl[i] <- 0
    }
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$significant_comorbidities___unk[i] == 1) ccc19x$der_CHF_bl[i] <- 99
    }
    
    ccc19x$der_CHF_bl <- factor(ccc19x$der_CHF_bl)
    
    temp <- summary(ccc19x$der_CHF_bl[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_CHF_bl',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #C07d. PVD comorbidity
    ccc19x$der_PVD_bl <- NA
    ccc19x$der_PVD_bl[which( ccc19x$significant_comorbidities___400047006 == 1)] <- 1
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'comorbidities___400047006|comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(any(ccc19x[i,temp.ref]) & all(c(ccc19x$significant_comorbidities___400047006[i] == 0)
      )) ccc19x$der_PVD_bl[i] <- 0
    }
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$significant_comorbidities___unk[i] == 1) ccc19x$der_PVD_bl[i] <- 99
    }
    
    ccc19x$der_PVD_bl <- factor(ccc19x$der_PVD_bl)
    
    temp <- summary(ccc19x$der_PVD_bl[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_PVD_bl',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #C07e. CVA comorbidity
    ccc19x$der_CVA_bl <- NA
    ccc19x$der_CVA_bl[which( ccc19x$significant_comorbidities___275526006 == 1)] <- 1
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'comorbidities___275526006|comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(any(ccc19x[i,temp.ref]) & all(c(ccc19x$significant_comorbidities___275526006[i] == 0)
      )) ccc19x$der_CVA_bl[i] <- 0
    }
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$significant_comorbidities___unk[i] == 1) ccc19x$der_CVA_bl[i] <- 99
    }
    
    ccc19x$der_CVA_bl <- factor(ccc19x$der_CVA_bl)
    
    temp <- summary(ccc19x$der_CVA_bl[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_CVA_bl',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #C07f. Atrial fibrillation comorbidity
    ccc19x$der_afib_bl <- NA
    ccc19x$der_afib_bl[which( ccc19x$significant_comorbidities___49436004 == 1)] <- 1
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'comorbidities___49436004|comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(any(ccc19x[i,temp.ref]) & all(c(ccc19x$significant_comorbidities___49436004[i] == 0)
      )) ccc19x$der_afib_bl[i] <- 0
    }
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$significant_comorbidities___unk[i] == 1) ccc19x$der_afib_bl[i] <- 99
    }
    
    ccc19x$der_afib_bl <- factor(ccc19x$der_afib_bl)
    
    temp <- summary(ccc19x$der_afib_bl[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_afib_bl',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #######################
    #C08. Renal comorbidity
    #######################
    ccc19x$der_renal <- NA
    ccc19x$der_renal[which( ccc19x$significant_comorbidities___90708001 == 1|
                              ccc19x$significant_comorbidities___723190009 == 1|
                              ccc19x$significant_comorbidities___46177005 == 1|
                              ccc19x$significant_comorbidities___236435004 == 1
    )] <- 1
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), 
                        pattern = 'comorbidities___90708001|comorbidities___723190009|comorbidities___46177005|comorbidities___236435004|comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(any(ccc19x[i,temp.ref]) & all(c(ccc19x$significant_comorbidities___90708001[i] == 0,
                                         ccc19x$significant_comorbidities___723190009[i] == 0,
                                         ccc19x$significant_comorbidities___46177005[i] == 0,
                                         ccc19x$significant_comorbidities___236435004[i] == 0)
      )) ccc19x$der_renal[i] <- 0
    }
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$significant_comorbidities___unk[i] == 1) ccc19x$der_renal[i] <- 99
    }
    
    ccc19x$der_renal <- factor(ccc19x$der_renal)
    
    temp <- summary(ccc19x$der_renal[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_renal',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ##########################
    #C08a. Patient on dialysis
    ##########################
    ccc19x$der_dialysis <- NA
    ccc19x$der_dialysis[which(ccc19x$significant_comorbidities___236435004 == 1)] <- 1
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), 
                               pattern = 'comorbidities___236435004|comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(any(ccc19x[i,temp.ref]) & ccc19x$significant_comorbidities___236435004[i] == 0) ccc19x$der_dialysis[i] <- 0
    }
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$significant_comorbidities___unk[i] == 1) ccc19x$der_dialysis[i] <- 99
    }
    
    ccc19x$der_dialysis <- factor(ccc19x$der_dialysis)
    
    temp <- summary(ccc19x$der_dialysis[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_dialysis',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ##################
    #C09. Hypertension
    ##################
    ccc19x$der_htn <- NA
    ccc19x$der_htn[which(ccc19x$significant_comorbidities___38341003 == 1)] <- 1
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___38341003|significant_comorbidities___unk'))
    
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(any(ccc19x[i,temp.ref]) & ccc19x$significant_comorbidities___38341003[i] == 0) ccc19x$der_htn[i] <- 0
    }
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$significant_comorbidities___unk[i] == 1) ccc19x$der_htn[i] <- 99
    }
    
    ccc19x$der_htn <- factor(ccc19x$der_htn)
    
    temp <- summary(ccc19x$der_htn[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_htn',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ####################
    #C15. Hyperlipidemia
    ####################
    ccc19x$der_hld <- NA
    ccc19x$der_hld[which(ccc19x$significant_comorbidities___55822004 == 1)] <- 1
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___55822004|significant_comorbidities___unk'))
    
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(any(ccc19x[i,temp.ref]) & ccc19x$significant_comorbidities___55822004[i] == 0) ccc19x$der_hld[i] <- 0
    }
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$significant_comorbidities___unk[i] == 1) ccc19x$der_hld[i] <- 99
    }
    
    ccc19x$der_hld <- factor(ccc19x$der_hld)
    
    temp <- summary(ccc19x$der_hld[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_hld',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ####################
    #C15a. Hyperlipidemia v2 - includes concomitant statins as an HLD equivalent
    ####################
    ccc19x$der_hld_v2 <- NA
    ccc19x$der_hld_v2[which(ccc19x$significant_comorbidities___55822004 == 1|
                              ccc19x$concomitant_meds___atc_c10aa == 1)] <- 1
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___55822004|significant_comorbidities___unk'))
    
    for(i in which(is.na(ccc19x$der_hld_v2) & ccc19x$redcap_repeat_instrument == ''))
    {
      if(any(ccc19x[i,temp.ref]) & ccc19x$significant_comorbidities___55822004[i] == 0) ccc19x$der_hld_v2[i] <- 0
    }
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___unk'))
    for(i in which(is.na(ccc19x$der_hld_v2) & ccc19x$redcap_repeat_instrument == ''))
    {
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$significant_comorbidities___unk[i] == 1) ccc19x$der_hld_v2[i] <- 99
    }
    
    ccc19x$der_hld_v2 <- factor(ccc19x$der_hld_v2)
    summary(ccc19x$der_hld_v2[ccc19x$redcap_repeat_instrument == ''])
    
    #########
    #C16. HIV
    #########
    ccc19x$der_HIV <- NA
    ccc19x$der_HIV[which( ccc19x$significant_comorbidities___62479008 == 1)] <- 1
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'comorbidities___62479008|comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(any(ccc19x[i,temp.ref]) & all(c(ccc19x$significant_comorbidities___62479008[i] == 0)
      )) ccc19x$der_HIV[i] <- 0
    }
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$significant_comorbidities___unk[i] == 1) ccc19x$der_HIV[i] <- 99
    }
    
    ccc19x$der_HIV <- factor(ccc19x$der_HIV)
    summary(ccc19x$der_HIV[ccc19x$redcap_repeat_instrument == ''])
    
    #################
    #C17. Sleep Apnea
    #################
    ccc19x$der_OSA <- NA
    ccc19x$der_OSA[which(ccc19x$significant_comorbidities___78275009 == 1)] <- 1
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___78275009|significant_comorbidities___unk'))
    
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(any(ccc19x[i,temp.ref]) & ccc19x$significant_comorbidities___78275009[i] == 0) ccc19x$der_OSA[i] <- 0
    }
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$significant_comorbidities___unk[i] == 1) ccc19x$der_OSA[i] <- 99
    }
    
    ccc19x$der_OSA <- factor(ccc19x$der_OSA)
    
    temp <- summary(ccc19x$der_OSA[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_OSA',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ########################
    #C18. Metabolic syndrome
    ########################
    ccc19x$der_metabolic_syndrome <- NA
    ccc19x$der_metabolic_syndrome[which(ccc19x$significant_comorbidities___237602007 == 1)] <- 1
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___237602007|significant_comorbidities___unk'))
    
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(any(ccc19x[i,temp.ref]) & ccc19x$significant_comorbidities___237602007[i] == 0) ccc19x$der_metabolic_syndrome[i] <- 0
    }
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$significant_comorbidities___unk[i] == 1) ccc19x$der_metabolic_syndrome[i] <- 99
    }
    
    ccc19x$der_metabolic_syndrome <- factor(ccc19x$der_metabolic_syndrome)
    
    temp <- summary(ccc19x$der_metabolic_syndrome[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_metabolic_syndrome',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ###################
    #C19. Liver disease
    ###################
    ccc19x$der_liver <- NA
    ccc19x$der_liver[which(ccc19x$significant_comorbidities___235856003 == 1 |
                             ccc19x$significant_comorbidities___19943007 == 1)] <- 1
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___235856003|significant_comorbidities___19943007|significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(any(ccc19x[i,temp.ref]) & ccc19x$significant_comorbidities___235856003[i] == 0 &
         ccc19x$significant_comorbidities___19943007[i] == 0) ccc19x$der_liver[i] <- 0
    }
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$significant_comorbidities___unk[i] == 1) ccc19x$der_liver[i] <- 99
    }
    
    ccc19x$der_liver <- factor(ccc19x$der_liver)
    
    temp <- summary(ccc19x$der_liver[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_liver',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #######################
    #C10. Baseline dementia
    #######################
    ccc19x$der_dementia <- NA
    
    #Present
    ccc19x$der_dementia[which(ccc19x$significant_comorbidities___52448006 ==1)] <- 1
    
    #Not present, something else checked besides unknown
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_com') & !grepl(colnames(ccc19x), pattern = '52448006|unk'))
    for(i in which(is.na(ccc19x$der_dementia)))
      if(any(ccc19x[i,temp.ref] == 1) & !is.na(any(ccc19x[i,temp.ref] == 1))) ccc19x$der_dementia[i] <- 0
    
    #Unknown
    ccc19x$der_dementia[which(ccc19x$significant_comorbidities___unk ==1 &
                                is.na(ccc19x$der_dementia))] <- 99
    
    ccc19x$der_dementia <- as.factor(ccc19x$der_dementia)
    
    temp <- summary(ccc19x$der_dementia[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_dementia',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ############################
    #C11. Combined comorbidities
    ############################
    ccc19x$der_comorbid_combined <- NA
    
    ccc19x$der_comorbid_combined[which(ccc19x$der_card == 0 &
                                         ccc19x$der_pulm == 0 &
                                         ccc19x$der_renal == 0 &
                                         ccc19x$der_dm2 == 0)] <- 'None'
    
    ccc19x$der_comorbid_combined[which(ccc19x$der_card == 1 &
                                         ccc19x$der_pulm == 0 &
                                         ccc19x$der_renal == 0 &
                                         ccc19x$der_dm2 == 0)] <- 'Cardiovascular'
    
    ccc19x$der_comorbid_combined[which(ccc19x$der_card == 0 &
                                         ccc19x$der_pulm == 1 &
                                         ccc19x$der_renal == 0 &
                                         ccc19x$der_dm2 == 0)] <- 'Pulmonary'
    
    ccc19x$der_comorbid_combined[which(ccc19x$der_card == 0 &
                                         ccc19x$der_pulm == 0 &
                                         ccc19x$der_renal == 1 &
                                         ccc19x$der_dm2 == 0)] <- 'Renal'
    
    ccc19x$der_comorbid_combined[which(ccc19x$der_card == 0 &
                                         ccc19x$der_pulm == 0 &
                                         ccc19x$der_renal == 0 &
                                         ccc19x$der_dm2 == 1)] <- 'Diabetes'
    
    ccc19x$der_comorbid_combined[which((ccc19x$der_card == 1 & (ccc19x$der_pulm == 1|ccc19x$der_renal == 1|ccc19x$der_dm2 == 1))|
                                         (ccc19x$der_pulm == 1 & (ccc19x$der_card == 1|ccc19x$der_renal == 1|ccc19x$der_dm2 == 1))|
                                         (ccc19x$der_renal == 1 & (ccc19x$der_pulm == 1|ccc19x$der_card == 1|ccc19x$der_dm2 == 1))|
                                         (ccc19x$der_dm2 == 1 & (ccc19x$der_pulm == 1|ccc19x$der_renal == 1|ccc19x$der_card == 1)))] <- 'Multiple'
    
    ccc19x$der_comorbid_combined[which(ccc19x$der_card == 99|ccc19x$der_pulm == 99|ccc19x$der_renal == 99|ccc19x$der_dm2 == 99)] <- 'Unknown'
    
    ccc19x$der_comorbid_combined <- as.factor(ccc19x$der_comorbid_combined)
    ccc19x$der_comorbid_combined <- relevel(ccc19x$der_comorbid_combined, ref = 'None')
    summary(ccc19x$der_comorbid_combined[ccc19x$redcap_repeat_instrument == ''])
    
    #######################
    #C12. Modified Charlson
    #######################
    ccc19x$der_ccc19cci <- NA
    
    #Set to 0 for patients with any comorbidities or "none" checked
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_co') & colnames(ccc19x) != 'significant_comorbidities___unk')
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(any(ccc19x[i,temp.ref] == 1)) ccc19x$der_ccc19cci[i] <- 0
    
    #CAD
    temp.ref <- which(ccc19x$significant_comorbidities___53741008 == 1)
    ccc19x$der_ccc19cci[temp.ref] <- ccc19x$der_ccc19cci[temp.ref] + 1
    
    #CHF
    temp.ref <- which(ccc19x$significant_comorbidities___42343007 == 1)
    ccc19x$der_ccc19cci[temp.ref] <- ccc19x$der_ccc19cci[temp.ref] + 1
    
    #PVD
    temp.ref <- which(ccc19x$significant_comorbidities___400047006 == 1)
    ccc19x$der_ccc19cci[temp.ref] <- ccc19x$der_ccc19cci[temp.ref] + 1
    
    #CVA
    temp.ref <- which(ccc19x$significant_comorbidities___275526006 == 1)
    ccc19x$der_ccc19cci[temp.ref] <- ccc19x$der_ccc19cci[temp.ref] + 1
    
    #COPD
    temp.ref <- which(ccc19x$significant_comorbidities___13645005 == 1)
    ccc19x$der_ccc19cci[temp.ref] <- ccc19x$der_ccc19cci[temp.ref] + 1
    
    #Dementia
    temp.ref <- which(ccc19x$significant_comorbidities___52448006 == 1)
    ccc19x$der_ccc19cci[temp.ref] <- ccc19x$der_ccc19cci[temp.ref] + 1
    
    #Diabetes without complications
    temp.ref <- which(ccc19x$significant_comorbidities___73211009 == 1 &
                        ccc19x$significant_comorbidities___46177005 == 0 &
                        ccc19x$significant_comorbidities___723190009 == 0 &
                        ccc19x$significant_comorbidities___236435004 == 0 &
                        ccc19x$significant_comorbidities___53741008 == 0 &
                        ccc19x$significant_comorbidities___46177005 == 0 &
                        ccc19x$significant_comorbidities___190388001 == 0)
    ccc19x$der_ccc19cci[temp.ref] <- ccc19x$der_ccc19cci[temp.ref] + 1
    
    #Diabetes with complications
    temp.ref <- which((ccc19x$significant_comorbidities___73211009 == 1 &
                         (ccc19x$significant_comorbidities___723190009 == 1 |
                            ccc19x$significant_comorbidities___46177005 == 1 |
                            ccc19x$significant_comorbidities___236435004 == 1 |
                            ccc19x$significant_comorbidities___53741008 == 1 |
                            ccc19x$significant_comorbidities___46177005 == 1)) |
                        ccc19x$significant_comorbidities___190388001 == 1)
    ccc19x$der_ccc19cci[temp.ref] <- ccc19x$der_ccc19cci[temp.ref] + 2
    
    #Renal
    temp.ref <- which(ccc19x$significant_comorbidities___723190009 == 1 |
                        ccc19x$significant_comorbidities___46177005 == 1 |
                        ccc19x$significant_comorbidities___236435004 == 1)
    ccc19x$der_ccc19cci[temp.ref] <- ccc19x$der_ccc19cci[temp.ref] + 2
    
    #Liver disease NOS
    temp.ref <- which(ccc19x$significant_comorbidities___235856003 == 1)
    ccc19x$der_ccc19cci[temp.ref] <- ccc19x$der_ccc19cci[temp.ref] + 1
    
    #Cirrhosis
    temp.ref <- which(ccc19x$significant_comorbidities___19943007 == 1)
    ccc19x$der_ccc19cci[temp.ref] <- ccc19x$der_ccc19cci[temp.ref] + 3
    
    #Rheum/autoimmune disease
    temp.ref <- which(ccc19x$significant_comorbidities___24526004 == 1 |
                        ccc19x$significant_comorbidities___85828009 == 1)
    ccc19x$der_ccc19cci[temp.ref] <- ccc19x$der_ccc19cci[temp.ref] + 1
    
    #HIV with CD4 <200
    temp.ref <- which(ccc19x$significant_comorbidities___62479008 == 1)
    temp.ref2 <- which(ccc19x$hiv_cd4[temp.ref] < 200)
    ccc19x$der_ccc19cci[temp.ref[temp.ref2]] <- ccc19x$der_ccc19cci[temp.ref[temp.ref2]] + 6
    
    #Unknown (unknown is the ONLY "comorbidity" that is checked)
    temp.ref <- which(ccc19x$significant_comorbidities___unk == 1 & is.na(ccc19x$der_ccc19cci))
    ccc19x$der_ccc19cci[temp.ref] <- 99
    
    ccc19x$der_ccc19cci <- factor(ccc19x$der_ccc19cci)
    
    temp <- summary(ccc19x$der_ccc19cci[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ccc19cci',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #C12a. Modified Charlson with aggregated categories 9+
    ccc19x$der_ccc19cci_v2 <- as.character(ccc19x$der_ccc19cci)
    ccc19x$der_ccc19cci_v2[which(ccc19x$der_ccc19cci_v2 %in% 9:22)] <- '9+'
    ccc19x$der_ccc19cci_v2 <- factor(ccc19x$der_ccc19cci_v2)
    summary(ccc19x$der_ccc19cci_v2[ccc19x$redcap_repeat_instrument == ''])
    
    #C12b. Modified Charlson with aggregated categories 3+
    ccc19x$der_ccc19cci_v3 <- as.character(ccc19x$der_ccc19cci)
    ccc19x$der_ccc19cci_v3[which(ccc19x$der_ccc19cci_v3 %in% 3:22)] <- '3+'
    ccc19x$der_ccc19cci_v3 <- factor(ccc19x$der_ccc19cci_v3)
    summary(ccc19x$der_ccc19cci_v3[ccc19x$redcap_repeat_instrument == ''])
    
    #C12c. Modified Charlson with aggregated categories 2+
    ccc19x$der_ccc19cci_v4 <- as.character(ccc19x$der_ccc19cci)
    ccc19x$der_ccc19cci_v4[which(ccc19x$der_ccc19cci_v4 %in% 2:22)] <- '2+'
    ccc19x$der_ccc19cci_v4 <- factor(ccc19x$der_ccc19cci_v4)
    summary(ccc19x$der_ccc19cci_v4[ccc19x$redcap_repeat_instrument == ''])
    
    #C12d. charlson_group: 0, 1-2, 3+
    ccc19x$der_ccc19cci_v5 <- ifelse(ccc19x$der_ccc19cci == 0, 0, 
                                     ifelse((ccc19x$der_ccc19cci == 1 | ccc19x$der_ccc19cci == 2), 1, 
                                            ifelse(ccc19x$der_ccc19cci == 99, 99, 2)))
    
    ccc19x$der_ccc19cci_v5 <- factor(ccc19x$der_ccc19cci_v5, levels = c(0,1,2,99), 
                                     labels = c("0", "1-2", "3+", "99"))
    
    temp <- summary(ccc19x$der_ccc19cci_v5[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ccc19cci_v5',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #C12e. charlson_group: 0-1, 2+
    ccc19x$der_ccc19cci_v6 <- as.character(ccc19x$der_ccc19cci)
    ccc19x$der_ccc19cci_v6[which(ccc19x$der_ccc19cci_v6 %in% 0:1)] <- '0-1'
    ccc19x$der_ccc19cci_v6[which(ccc19x$der_ccc19cci_v6 %in% 2:22)] <- '2+'
    ccc19x$der_ccc19cci_v6 <- factor(ccc19x$der_ccc19cci_v6)
    summary(ccc19x$der_ccc19cci_v6[ccc19x$redcap_repeat_instrument == ''])
    
    ##############################
    #C13. CVD risk factor (binary)
    ##############################
    ccc19x$der_CVD_risk <- NA
    
    #CVD present at baseline
    ccc19x$der_CVD_risk[which(ccc19x$significant_comorbidities___53741008 == 1|
                                ccc19x$significant_comorbidities___42343007 == 1|
                                ccc19x$significant_comorbidities___400047006 == 1|
                                ccc19x$significant_comorbidities___275526006 == 1)] <- 1
    
    #Two or more risk factors
    temp <- rep(0, nrow(ccc19x))
    #Sex and age
    temp[which(ccc19x$der_sex == 'Male' & ccc19x$der_age_trunc >= 55)] <- temp[which(ccc19x$der_sex == 'Male' & ccc19x$der_age_trunc >= 55)] + 1
    temp[which(ccc19x$der_sex == 'Female' & ccc19x$der_age_trunc >= 60)] <- temp[which(ccc19x$der_sex == 'Female' & ccc19x$der_age_trunc >= 60)] + 1
    #Obesity
    temp[which(ccc19x$der_obesity_v2 == 'Obese')] <- temp[which(ccc19x$der_obesity_v2 == 'Obese')] + 1
    #Hypertension
    temp[which(ccc19x$significant_comorbidities___38341003 == 1)] <- temp[which(ccc19x$significant_comorbidities___38341003 == 1)] + 1
    #Hyperlipidemia
    temp[which(ccc19x$significant_comorbidities___55822004 == 1)] <- temp[which(ccc19x$significant_comorbidities___55822004 == 1)] + 1
    #Diabetes
    temp[which(ccc19x$der_dm2 == 1)] <- temp[which(ccc19x$der_dm2 == 1)] + 1
    #Tobacco use
    temp[which(ccc19x$der_smoking == 'Current')] <- temp[which(ccc19x$der_smoking == 'Current')] + 1
    
    ccc19x$der_CVD_risk[which(temp >= 2)] <- 1
    
    #1 risk factor
    ccc19x$der_CVD_risk[which(temp == 1 & is.na(ccc19x$der_CVD_risk) & ccc19x$redcap_repeat_instrument == '')] <- 0
    
    #No CVD comorbidities
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), 
                               pattern = 'comorbidities___53741008|comorbidities___42343007|comorbidities___400047006|comorbidities___275526006|significant_comorbidities___unk'))
    
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(any(ccc19x[i,temp.ref]) & is.na(ccc19x$der_CVD_risk[i])) ccc19x$der_CVD_risk[i] <- 0
    }
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$significant_comorbidities___unk[i] == 1) ccc19x$der_CVD_risk[i] <- 99
    }
    
    ccc19x$der_CVD_risk <- factor(ccc19x$der_CVD_risk)
    
    temp <- summary(ccc19x$der_CVD_risk[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_CVD_risk',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #########################
    #C13a. CVD risk factor v2 
    #########################
    ccc19x$der_CVD_risk_v2 <- NA
    
    #Two or more risk factors
    temp <- rep(0, nrow(ccc19x))
    
    #Sex and age
    temp[which(ccc19x$der_sex == 'Male' & ccc19x$der_age_trunc >= 55)] <- temp[which(ccc19x$der_sex == 'Male' & ccc19x$der_age_trunc >= 55)] + 1
    temp[which(ccc19x$der_sex == 'Female' & ccc19x$der_age_trunc >= 60)] <- temp[which(ccc19x$der_sex == 'Female' & ccc19x$der_age_trunc >= 60)] + 1
    
    #Hypertension
    temp[which(ccc19x$significant_comorbidities___38341003 == 1)] <- temp[which(ccc19x$significant_comorbidities___38341003 == 1)] + 1
    #Hyperlipidemia
    temp[which(ccc19x$significant_comorbidities___55822004 == 1)] <- temp[which(ccc19x$significant_comorbidities___55822004 == 1)] + 1
    #Diabetes
    temp[which(ccc19x$der_dm2 == 1)] <- temp[which(ccc19x$der_dm2 == 1)] + 1
    #Tobacco use
    temp[which(ccc19x$der_smoking == 'Current')] <- temp[which(ccc19x$der_smoking == 'Current')] + 1
    
    ccc19x$der_CVD_risk_v2[which(temp >= 2)] <- 1
    
    #Removals
    
    #1 risk factor
    ccc19x$der_CVD_risk_v2[which(temp == 1 & is.na(ccc19x$der_CVD_risk_v2) & ccc19x$redcap_repeat_instrument == '')] <- 0
    
    #Too young
    ccc19x$der_CVD_risk_v2[which(ccc19x$der_sex == 'Male' & ccc19x$der_age_trunc < 55)] <- 0
    ccc19x$der_CVD_risk_v2[which(ccc19x$der_sex == 'Female' & ccc19x$der_age_trunc < 60)] <- 0
    
    #Unknowns
    ccc19x$der_CVD_risk_v2[which((ccc19x$significant_comorbidities___38341003 == 0 & ccc19x$significant_comorbidities___unk == 1)|
                                   (ccc19x$significant_comorbidities___55822004 == 0 & ccc19x$significant_comorbidities___unk == 1)|
                                   ccc19x$der_dm2 == 99|
                                   ccc19x$der_smoking == 'Unknown')
    ] <- 'Unknown'
    
    #Already has CVD
    ccc19x$der_CVD_risk_v2[which(ccc19x$der_card_v2 == 1)] <- 'CVD already present'
    
    ccc19x$der_CVD_risk_v2 <- factor(ccc19x$der_CVD_risk_v2)
    
    temp <- summary(ccc19x$der_CVD_risk_v2[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_CVD_risk_v2',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #########################
    #C13b. CVD risk factor v3 
    #########################
    ccc19x$der_CVD_risk_v3 <- NA
    
    #Hypertension
    ccc19x$der_CVD_risk_v3[which(ccc19x$der_htn == 1)] <- 1
    ccc19x$der_CVD_risk_v3[which(ccc19x$der_htn == 0 &
                                   is.na(ccc19x$der_CVD_risk_v3))] <- 0
    
    #Hyperlipidemia
    ccc19x$der_CVD_risk_v3[which(ccc19x$der_hld == 1)] <- 1
    ccc19x$der_CVD_risk_v3[which(ccc19x$der_hld == 0 &
                                   is.na(ccc19x$der_CVD_risk_v3))] <- 0
    
    #Diabetes
    ccc19x$der_CVD_risk_v3[which(ccc19x$der_dm2 == 1)] <- 1
    ccc19x$der_CVD_risk_v3[which(ccc19x$der_dm2 == 0 &
                                   is.na(ccc19x$der_CVD_risk_v3))] <- 0
    
    #Tobacco use
    ccc19x$der_CVD_risk_v3[which(ccc19x$der_smoking == 'Current')] <- 1
    ccc19x$der_CVD_risk_v3[which(ccc19x$der_smoking %in% c('Never', 'Former') &
                                   is.na(ccc19x$der_CVD_risk_v3))] <- 0
    
    #Unknowns
    ccc19x$der_CVD_risk_v3[which((ccc19x$der_htn == 99|
                                   ccc19x$der_hld == 99|
                                   ccc19x$der_dm2 == 99|
                                   ccc19x$der_smoking == 'Unknown') &
                                   (ccc19x$der_CVD_risk_v3 == 0|is.na(ccc19x$der_CVD_risk_v3)))] <- 'Unknown'
    
    #Removals
    
    #Too young
    ccc19x$der_CVD_risk_v3[which(ccc19x$der_sex == 'Male' & ccc19x$der_age_trunc < 55)] <- 0
    ccc19x$der_CVD_risk_v3[which(ccc19x$der_sex == 'Female' & ccc19x$der_age_trunc < 60)] <- 0
    
    #Already has CVD
    ccc19x$der_CVD_risk_v3[which(ccc19x$der_card_v2 == 1)] <- 'CVD already present'
    
    ccc19x$der_CVD_risk_v3 <- factor(ccc19x$der_CVD_risk_v3)
    
    temp <- summary(ccc19x$der_CVD_risk_v3[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_CVD_risk_v3',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #########################
    #C13b1. CVD risk factor v3 collapsed
    #########################
    ccc19x$der_CVD_risk_v3_collapsed <- ccc19x$der_CVD_risk_v3
    ccc19x$der_CVD_risk_v3_collapsed[which(ccc19x$der_CVD_risk_v3_collapsed == 'CVD already present')] <- 1
    ccc19x$der_CVD_risk_v3_collapsed <- droplevels(ccc19x$der_CVD_risk_v3_collapsed)
    
    temp <- summary(ccc19x$der_CVD_risk_v3_collapsed[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_CVD_risk_v3_collapsed',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ######################
    #C13c. CVD risk factor indicator that patient has 2+ of the CVD risk factors including age
    {
      ccc19x$der_CVD_risk_num <- NA
      
      #Two or more risk factors
      temp <- rep(0, nrow(ccc19x))
      
      #Sex and age
      temp[which(ccc19x$der_sex == 'Male' & ccc19x$der_age_trunc >= 55)] <- temp[which(ccc19x$der_sex == 'Male' & ccc19x$der_age_trunc >= 55)] + 1
      temp[which(ccc19x$der_sex == 'Female' & ccc19x$der_age_trunc >= 60)] <- temp[which(ccc19x$der_sex == 'Female' & ccc19x$der_age_trunc >= 60)] + 1
      
      #Hypertension
      temp[which(ccc19x$der_htn == 1)] <- temp[which(ccc19x$der_htn == 1)] + 1
      #Hyperlipidemia
      temp[which(ccc19x$der_hld == 1)] <- temp[which(ccc19x$der_hld == 1)] + 1
      #Diabetes
      temp[which(ccc19x$der_dm2 == 1)] <- temp[which(ccc19x$der_dm2 == 1)] + 1
      #Tobacco use
      temp[which(ccc19x$der_smoking == 'Current')] <- temp[which(ccc19x$der_smoking == 'Current')] + 1
      
      ccc19x$der_CVD_risk_num[which(temp >= 2)] <- 1
      
      #Only one risk factor
      ccc19x$der_CVD_risk_num[which(temp == 1)] <- 0
      
      #No risk factors
      ccc19x$der_CVD_risk_num[which(ccc19x$der_sex == 'Male' & 
                                      ccc19x$der_age_trunc < 55 & 
                                      ccc19x$der_htn == 0 & 
                                      ccc19x$der_hld == 0 & 
                                      ccc19x$der_dm2 == 0 &
                                      ccc19x$der_smoking %in% c('Never', 'Former'))] <- 0
      
      ccc19x$der_CVD_risk_num[which(ccc19x$der_sex == 'Female' & 
                                      ccc19x$der_age_trunc < 60 & 
                                      ccc19x$der_htn == 0 & 
                                      ccc19x$der_hld == 0 & 
                                      ccc19x$der_dm2 == 0 &
                                      ccc19x$der_smoking %in% c('Never', 'Former'))] <- 0
      
      #Unknown with risk score of 1 (needs 1+ unknowns)
      ccc19x$der_CVD_risk_num[which((ccc19x$der_htn == 99|ccc19x$der_hld == 99|
                                       ccc19x$der_dm2 == 99|ccc19x$der_smoking == "Unknown") &
                                      temp == 1)] <- 99
      
      #Unknown with risk score of 0 (needs 2+ unknowns)
      ccc19x$der_CVD_risk_num[which(((ccc19x$der_htn == 99 & (ccc19x$der_hld == 99|ccc19x$der_dm2 == 99|ccc19x$der_smoking == "Unknown"))|
                                       (ccc19x$der_hld == 99 & (ccc19x$der_htn == 99|ccc19x$der_dm2 == 99|ccc19x$der_smoking == "Unknown"))|
                                       (ccc19x$der_dm2 == 99 & (ccc19x$der_hld == 99|ccc19x$der_htn == 99|ccc19x$der_smoking == "Unknown"))|
                                       (ccc19x$der_smoking == 'Unknown' & (ccc19x$der_htn == 99|ccc19x$der_hld == 99|ccc19x$der_dm2 == 99))) &
                                      temp == 0)] <- 99
      
      ccc19x$der_CVD_risk_num <- factor(ccc19x$der_CVD_risk_num)
      
      temp <- summary(ccc19x$der_CVD_risk_num[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_CVD_risk_num',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
    }
    
    }
  print('Comorbidities completed')
  
  #############
  #Cancer types
  #############
  {
    
    #Dx1-9. Binary indicators for heme types
    ccc19x$der_HemeNOS <- 0
    ccc19x$der_HemeNOS[which(ccc19x$cancer_type %in% c("C27134","C9300","C3106","OTH_H")|
                               ccc19x$cancer_type_2 %in% c("C27134","C9300","C3106","OTH_H")|
                               ccc19x$cancer_type_3 %in% c("C27134","C9300","C3106","OTH_H")|
                               ccc19x$cancer_type_4 %in% c("C27134","C9300","C3106","OTH_H")|
                               ccc19x$cancer_type_5 %in% c("C27134","C9300","C3106","OTH_H"))] <- 1
    ccc19x$der_HemeNOS <- factor(ccc19x$der_HemeNOS)
    
    temp <- summary(ccc19x$der_HemeNOS[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_HemeNOS',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #B-cell malignancies + acute lymphoblastic leukemia + aggressive NHL + indolent NHL and excluding plasma cell neoplasms
    ccc19x$der_Bcell <- 0
    ccc19x$der_Bcell[which(ccc19x$cancer_type %in% c("C3167","C8851","C9357","C9244","C4337","C2912","C8504","C3209","C3163","C4341","C9308")|
                                   ccc19x$cancer_type_2 %in% c("C3167","C8851","C9357","C9244","C4337","C2912","C8504","C3209","C3163","C4341","C9308")|
                                   ccc19x$cancer_type_3 %in% c("C3167","C8851","C9357","C9244","C4337","C2912","C8504","C3209","C3163","C4341","C9308")|
                                   ccc19x$cancer_type_4 %in% c("C3167","C8851","C9357","C9244","C4337","C2912","C8504","C3209","C3163","C4341","C9308")|
                                   ccc19x$cancer_type_5 %in% c("C3167","C8851","C9357","C9244","C4337","C2912","C8504","C3209","C3163","C4341","C9308"))] <- 1
    ccc19x$der_Bcell <- factor(ccc19x$der_Bcell)
    
    temp <- summary(ccc19x$der_Bcell[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_Bcell',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ccc19x$der_Lymph_HGNHL <- 0
    ccc19x$der_Lymph_HGNHL[which(ccc19x$cancer_type %in% c("C8851","C9244","C2912")|
                                   ccc19x$cancer_type_2 %in% c("C8851","C9244","C2912")|
                                   ccc19x$cancer_type_3 %in% c("C8851","C9244","C2912")|
                                   ccc19x$cancer_type_4 %in% c("C8851","C9244","C2912")|
                                   ccc19x$cancer_type_5 %in% c("C8851","C9244","C2912"))] <- 1
    ccc19x$der_Lymph_HGNHL <- factor(ccc19x$der_Lymph_HGNHL)
    
    temp <- summary(ccc19x$der_Lymph_HGNHL[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_Lymph_HGNHL',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ccc19x$der_Lymph_LGNHL <- 0
    ccc19x$der_Lymph_LGNHL[which(ccc19x$cancer_type %in% c("C3209", "C3163", "C4341", "C4337", "C8504")|
                                   ccc19x$cancer_type_2 %in% c("C3209", "C3163", "C4341", "C4337", "C8504")|
                                   ccc19x$cancer_type_3 %in% c("C3209", "C3163", "C4341", "C4337", "C8504")|
                                   ccc19x$cancer_type_4 %in% c("C3209", "C3163", "C4341", "C4337", "C8504")|
                                   ccc19x$cancer_type_5 %in% c("C3209", "C3163", "C4341", "C4337", "C8504"))] <- 1
    ccc19x$der_Lymph_LGNHL <- factor(ccc19x$der_Lymph_LGNHL)
    
    temp <- summary(ccc19x$der_Lymph_LGNHL[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_Lymph_LGNHL',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ccc19x$der_ALL <- 0
    ccc19x$der_ALL[which(ccc19x$cancer_type %in% c("C3167")|
                           ccc19x$cancer_type_2 %in% c("C3167")|
                           ccc19x$cancer_type_3 %in% c("C3167")|
                           ccc19x$cancer_type_4 %in% c("C3167")|
                           ccc19x$cancer_type_5 %in% c("C3167"))] <- 1
    ccc19x$der_ALL <- factor(ccc19x$der_ALL)
    
    temp <- summary(ccc19x$der_ALL[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ALL',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ccc19x$der_CLL <- 0
    ccc19x$der_CLL[which(ccc19x$cancer_type %in% c("C3163")|
                           ccc19x$cancer_type_2 %in% c("C3163")|
                           ccc19x$cancer_type_3 %in% c("C3163")|
                           ccc19x$cancer_type_4 %in% c("C3163")|
                           ccc19x$cancer_type_5 %in% c("C3163"))] <- 1
    ccc19x$der_CLL <- factor(ccc19x$der_CLL)
    
    temp <- summary(ccc19x$der_CLL[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_CLL',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ccc19x$der_Hodgkin <- 0
    ccc19x$der_Hodgkin[which(ccc19x$cancer_type %in% c("C9357")|
                               ccc19x$cancer_type_2 %in% c("C9357")|
                               ccc19x$cancer_type_3 %in% c("C9357")|
                               ccc19x$cancer_type_4 %in% c("C9357")|
                               ccc19x$cancer_type_5 %in% c("C9357"))] <- 1
    ccc19x$der_Hodgkin <- factor(ccc19x$der_Hodgkin)
    summary(ccc19x$der_Hodgkin[ccc19x$redcap_repeat_instrument == ''])
    
    ccc19x$der_Lymph_Other <- 0
    ccc19x$der_Lymph_Other[which(ccc19x$cancer_type %in% c("C9308", "C3211")|
                                   ccc19x$cancer_type_2 %in% c("C9308", "C3211")|
                                   ccc19x$cancer_type_3 %in% c("C9308", "C3211")|
                                   ccc19x$cancer_type_4 %in% c("C9308", "C3211")|
                                   ccc19x$cancer_type_5 %in% c("C9308", "C3211"))] <- 1
    ccc19x$der_Lymph_Other <- factor(ccc19x$der_Lymph_Other)
    
    temp <- summary(ccc19x$der_Lymph_Other[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_Lymph_Other',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ccc19x$der_AML <- 0
    ccc19x$der_AML[which(ccc19x$cancer_type %in% c("C3171")|
                           ccc19x$cancer_type_2 %in% c("C3171")|
                           ccc19x$cancer_type_3 %in% c("C3171")|
                           ccc19x$cancer_type_4 %in% c("C3171")|
                           ccc19x$cancer_type_5 %in% c("C3171"))] <- 1
    ccc19x$der_AML <- factor(ccc19x$der_AML)
    summary(ccc19x$der_AML[ccc19x$redcap_repeat_instrument == ''])
    
    ccc19x$der_PCDs <- 0
    ccc19x$der_PCDs[which(ccc19x$cancer_type %in% c("C3242","C4665","C3819")|
                            ccc19x$cancer_type_2 %in% c("C3242","C4665","C3819")|
                            ccc19x$cancer_type_3 %in% c("C3242","C4665","C3819")|
                            ccc19x$cancer_type_4 %in% c("C3242","C4665","C3819")|
                            ccc19x$cancer_type_5 %in% c("C3242","C4665","C3819"))] <- 1
    ccc19x$der_PCDs <- factor(ccc19x$der_PCDs)
    
    temp <- summary(ccc19x$der_PCDs[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_PCDs',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca11 Primary heme type
    ccc19x$der_heme_type <- NA
    ccc19x$der_heme_type[which(ccc19x$cancer_type %in% c("C3171"))] <- 'Acute myeloid malignancies'
    ccc19x$der_heme_type[which(ccc19x$cancer_type %in% c("C4345","C3174","C3247"))] <- 'Chronic myeloid malignancies'
    ccc19x$der_heme_type[which(ccc19x$cancer_type %in% c("C9244","C9357","C3211","C8851","C2912","C27908","C3167"))] <- 'Aggressive lymphoid malignancies'
    ccc19x$der_heme_type[which(ccc19x$cancer_type %in% c("C3163","C3209","C8504","C4341","C4337","C9308"))] <- 'Indolent lymphoid malignancies'
    ccc19x$der_heme_type[which(ccc19x$cancer_type %in% c("C3242","C4665","C3819"))] <- 'Plasma cell neoplasms'
    ccc19x$der_heme_type[which(ccc19x$cancer_type %in% c("C9300","C27134","C3106","OTH_H"))] <- 'Other'
    ccc19x$der_heme_type <- factor(ccc19x$der_heme_type)
    summary(ccc19x$der_heme_type[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca11b Secondary heme type
    ccc19x$der_heme_type_secondary <- NA
    ccc19x$der_heme_type_secondary[which(ccc19x$cancer_type_2 %in% c("C3171"))] <- 'Acute myeloid malignancies'
    ccc19x$der_heme_type_secondary[which(ccc19x$cancer_type_2 %in% c("C4345","C3174","C3247"))] <- 'Chronic myeloid malignancies'
    ccc19x$der_heme_type_secondary[which(ccc19x$cancer_type_2 %in% c("C9244","C9357","C3211","C8851","C2912","C27908","C3167"))] <- 'Aggressive lymphoid malignancies'
    ccc19x$der_heme_type_secondary[which(ccc19x$cancer_type_2 %in% c("C3163","C3209","C8504","C4341","C4337","C9308"))] <- 'Indolent lymphoid malignancies'
    ccc19x$der_heme_type_secondary[which(ccc19x$cancer_type_2 %in% c("C3242","C4665","C3819"))] <- 'Plasma cell neoplasms'
    ccc19x$der_heme_type_secondary[which(ccc19x$cancer_type_2 %in% c("C9300","C27134","C3106","OTH_H"))] <- 'Other'
    ccc19x$der_heme_type_secondary <- factor(ccc19x$der_heme_type_secondary)
    summary(ccc19x$der_heme_type_secondary[ccc19x$redcap_repeat_instrument == ''])
    
    #Lymphoid malignancy
    ccc19x$der_Lymph <- 0
    ccc19x$der_Lymph[which(ccc19x$cancer_type %in% c("C3457","C9244","C9357","C3211","C8851","C2912","C27908","C3167","C3163","C3209","C8504","C4341","C4337","C9308")|
                             ccc19x$cancer_type_2 %in% c("C3457","C9244","C9357","C3211","C8851","C2912","C27908","C3167","C3163","C3209","C8504","C4341","C4337","C9308")|
                             ccc19x$cancer_type_3 %in% c("C3457","C9244","C9357","C3211","C8851","C2912","C27908","C3167","C3163","C3209","C8504","C4341","C4337","C9308")|
                             ccc19x$cancer_type_4 %in% c("C3457","C9244","C9357","C3211","C8851","C2912","C27908","C3167","C3163","C3209","C8504","C4341","C4337","C9308")|
                             ccc19x$cancer_type_5 %in% c("C3457","C9244","C9357","C3211","C8851","C2912","C27908","C3167","C3163","C3209","C8504","C4341","C4337","C9308"))] <- 1
    ccc19x$der_Lymph <- factor(ccc19x$der_Lymph)
    
    temp <- summary(ccc19x$der_Lymph[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_Lymph',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Myeloid malignancy
    ccc19x$der_Myeloid <- 0
    ccc19x$der_Myeloid[which(ccc19x$cancer_type %in% c("C3171","C4345","C3174","C3247")|
                             ccc19x$cancer_type_2 %in% c("C3171","C4345","C3174","C3247")|
                             ccc19x$cancer_type_3 %in% c("C3171","C4345","C3174","C3247")|
                             ccc19x$cancer_type_4 %in% c("C3171","C4345","C3174","C3247")|
                             ccc19x$cancer_type_5 %in% c("C3171","C4345","C3174","C3247"))] <- 1
    ccc19x$der_Myeloid <- factor(ccc19x$der_Myeloid)
    
    temp <- summary(ccc19x$der_Myeloid[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_Myeloid',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Myeloproliferative neoplasms
    ccc19x$der_MPN <- 0
    ccc19x$der_MPN[which(ccc19x$cancer_type %in% c("C4345")|
                               ccc19x$cancer_type_2 %in% c("C4345")|
                               ccc19x$cancer_type_3 %in% c("C4345")|
                               ccc19x$cancer_type_4 %in% c("C4345")|
                               ccc19x$cancer_type_5 %in% c("C4345"))] <- 1
    ccc19x$der_MPN <- factor(ccc19x$der_MPN)
    summary(ccc19x$der_MPN[ccc19x$redcap_repeat_instrument == ''])
    
    #Myelodysplastic syndrome
    ccc19x$der_MDS <- 0
    ccc19x$der_MDS[which(ccc19x$cancer_type %in% c("C3247")|
                           ccc19x$cancer_type_2 %in% c("C3247")|
                           ccc19x$cancer_type_3 %in% c("C3247")|
                           ccc19x$cancer_type_4 %in% c("C3247")|
                           ccc19x$cancer_type_5 %in% c("C3247"))] <- 1
    ccc19x$der_MDS <- factor(ccc19x$der_MDS)
    summary(ccc19x$der_MDS[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca8 Heme indicator
    ccc19x$der_heme <- NA
    ccc19x$der_heme[ccc19x$redcap_repeat_instrument == ''] <- 0
    ccc19x$der_heme[which(ccc19x$der_HemeNOS == 1|
                            ccc19x$der_Lymph == 1|
                            ccc19x$der_Myeloid == 1|
                            ccc19x$der_PCDs == 1)] <- 1
    
    ccc19x$der_heme <- factor(ccc19x$der_heme)
    
    temp <- summary(ccc19x$der_heme[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_heme',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Dx10- Solid tumor binary indicators
    ccc19x$der_Breast <- 0
    ccc19x$der_Breast[which(ccc19x$cancer_type %in% c("C4872")|
                              ccc19x$cancer_type_2 %in% c("C4872")|
                              ccc19x$cancer_type_3 %in% c("C4872")|
                              ccc19x$cancer_type_4 %in% c("C4872")|
                              ccc19x$cancer_type_5 %in% c("C4872"))] <- 1
    ccc19x$der_Breast <- factor(ccc19x$der_Breast)
    
    temp <- summary(ccc19x$der_Breast[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_Breast',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #GU
    ccc19x$der_GU <- 0
    ccc19x$der_GU[which(ccc19x$cancer_type %in% c("C4863","C9061","C9063","C4912","C6389",
                                                  "C7355","C9385","C3267")|
                          ccc19x$cancer_type_2 %in% c("C4863","C9061","C9063","C4912","C6389",
                                                      "C7355","C9385","C3267")|
                          ccc19x$cancer_type_3 %in% c("C4863","C9061","C9063","C4912","C6389",
                                                      "C7355","C9385","C3267")|
                          ccc19x$cancer_type_4 %in% c("C4863","C9061","C9063","C4912","C6389",
                                                      "C7355","C9385","C3267")|
                          ccc19x$cancer_type_5 %in% c("C4863","C9061","C9063","C4912","C6389",
                                                      "C7355","C9385","C3267"))] <- 1
    ccc19x$der_GU <- factor(ccc19x$der_GU)
    summary(ccc19x$der_GU[ccc19x$redcap_repeat_instrument == ''])
    
    #Prostate
    ccc19x$der_Prostate <- 0
    ccc19x$der_Prostate[which(ccc19x$cancer_type %in% c("C4863")|
                                ccc19x$cancer_type_2 %in% c("C4863")|
                                ccc19x$cancer_type_3 %in% c("C4863")|
                                ccc19x$cancer_type_4 %in% c("C4863")|
                                ccc19x$cancer_type_5 %in% c("C4863"))] <- 1
    ccc19x$der_Prostate <- factor(ccc19x$der_Prostate)
    summary(ccc19x$der_Prostate[ccc19x$redcap_repeat_instrument == ''])
    
    temp <- summary(ccc19x$der_Prostate[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_Prostate',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Bladder
    ccc19x$der_Bladder <- 0
    ccc19x$der_Bladder[which(ccc19x$cancer_type %in% c("C4912")|
                                ccc19x$cancer_type_2 %in% c("C4912")|
                               ccc19x$cancer_type_3 %in% c("C4912")|
                               ccc19x$cancer_type_4 %in% c("C4912")|
                               ccc19x$cancer_type_5 %in% c("C4912"))] <- 1
    ccc19x$der_Bladder <- factor(ccc19x$der_Bladder)
    summary(ccc19x$der_Bladder[ccc19x$redcap_repeat_instrument == ''])
    
    #Kidney (RCC)
    ccc19x$der_RCC <- 0
    ccc19x$der_RCC[which(ccc19x$cancer_type %in% c("C9385")|
                               ccc19x$cancer_type_2 %in% c("C9385")|
                               ccc19x$cancer_type_3 %in% c("C9385")|
                               ccc19x$cancer_type_4 %in% c("C9385")|
                               ccc19x$cancer_type_5 %in% c("C9385"))] <- 1
    ccc19x$der_RCC <- factor(ccc19x$der_RCC)
    summary(ccc19x$der_RCC[ccc19x$redcap_repeat_instrument == ''])
    
    #Other GU (except prostate)
    ccc19x$der_Other_GU <- 0
    ccc19x$der_Other_GU[which(ccc19x$cancer_type %in% c("C9061","C9063","C4912","C6389",
                                                        "C7355","C9385","C3267")|
                                ccc19x$cancer_type_2 %in% c("C9061","C9063","C4912","C6389",
                                                            "C7355","C9385","C3267")|
                                ccc19x$cancer_type_3 %in% c("C9061","C9063","C4912","C6389",
                                                            "C7355","C9385","C3267")|
                                ccc19x$cancer_type_4 %in% c("C9061","C9063","C4912","C6389",
                                                            "C7355","C9385","C3267")|
                                ccc19x$cancer_type_5 %in% c("C9061","C9063","C4912","C6389",
                                                            "C7355","C9385","C3267"))] <- 1
    ccc19x$der_Other_GU <- factor(ccc19x$der_Other_GU)
   
    temp <- summary(ccc19x$der_Other_GU[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_Other_GU',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Thoracic
    ccc19x$der_Thoracic <- 0
    ccc19x$der_Thoracic[which(ccc19x$cancer_type %in% c("C4917", "C2926", "C4878", "C3234","C3411")|
                                ccc19x$cancer_type_2 %in% c("C4917", "C2926", "C4878", "C3234","C3411")|
                                ccc19x$cancer_type_3 %in% c("C4917", "C2926", "C4878", "C3234","C3411")|
                                ccc19x$cancer_type_4 %in% c("C4917", "C2926", "C4878", "C3234","C3411")|
                                ccc19x$cancer_type_5 %in% c("C4917", "C2926", "C4878", "C3234","C3411"))] <- 1
    ccc19x$der_Thoracic <- factor(ccc19x$der_Thoracic)
    
    temp <- summary(ccc19x$der_Thoracic[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_Thoracic',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Lung cancers
    ccc19x$der_Lung <- 0
    ccc19x$der_Lung[which(ccc19x$cancer_type %in% c("C4917", "C2926", "C4878")|
                            ccc19x$cancer_type_2 %in% c("C4917", "C2926", "C4878")|
                            ccc19x$cancer_type_3 %in% c("C4917", "C2926", "C4878")|
                            ccc19x$cancer_type_4 %in% c("C4917", "C2926", "C4878")|
                            ccc19x$cancer_type_5 %in% c("C4917", "C2926", "C4878"))] <- 1
    ccc19x$der_Lung <- factor(ccc19x$der_Lung)
    
    temp <- summary(ccc19x$der_Lung[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_Lung',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #GI cancers
    
    #Lower GI (including appendix and small bowel)
    ccc19x$der_LowerGI <- 0
    ccc19x$der_LowerGI[which(ccc19x$cancer_type %in% c("C9291","C4910","C9330","C7724","C2955","C9382")|
                               ccc19x$cancer_type_2 %in% c("C9291","C4910","C9330","C7724","C2955","C9382")|
                               ccc19x$cancer_type_3 %in% c("C9291","C4910","C9330","C7724","C2955","C9382")|
                               ccc19x$cancer_type_4 %in% c("C9291","C4910","C9330","C7724","C2955","C9382")|
                               ccc19x$cancer_type_5 %in% c("C9291","C4910","C9330","C7724","C2955","C9382"))] <- 1
    ccc19x$der_LowerGI <- factor(ccc19x$der_LowerGI)
    
    temp <- summary(ccc19x$der_LowerGI[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_LowerGI',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Lower GI v2 (not including appendix and small bowel)
    ccc19x$der_LowerGI_v2 <- 0
    ccc19x$der_LowerGI_v2[which(ccc19x$cancer_type %in% c("C9291","C4910","C2955","C9382")|
                                  ccc19x$cancer_type_2 %in% c("C9291","C4910","C2955","C9382")|
                                  ccc19x$cancer_type_3 %in% c("C9291","C4910","C2955","C9382")|
                                  ccc19x$cancer_type_4 %in% c("C9291","C4910","C2955","C9382")|
                                  ccc19x$cancer_type_5 %in% c("C9291","C4910","C2955","C9382"))] <- 1
    ccc19x$der_LowerGI_v2 <- factor(ccc19x$der_LowerGI_v2)
    
    temp <- summary(ccc19x$der_LowerGI_v2[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_LowerGI_v2',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Colorectal
    ccc19x$der_colorectal <- 0
    ccc19x$der_colorectal[which(ccc19x$cancer_type %in% c("C4910","C2955","C9382")|
                                  ccc19x$cancer_type_2 %in% c("C4910","C2955","C9382")|
                                  ccc19x$cancer_type_3 %in% c("C4910","C2955","C9382")|
                                  ccc19x$cancer_type_4 %in% c("C4910","C2955","C9382")|
                                  ccc19x$cancer_type_5 %in% c("C4910","C2955","C9382"))] <- 1
    ccc19x$der_colorectal <- factor(ccc19x$der_colorectal)
    summary(ccc19x$der_colorectal[ccc19x$redcap_repeat_instrument == ''])
    
    #Anal
    ccc19x$der_anal <- 0
    ccc19x$der_anal[which(ccc19x$cancer_type %in% c("C9291")|
                            ccc19x$cancer_type_2 %in% c("C9291")|
                            ccc19x$cancer_type_3 %in% c("C9291")|
                            ccc19x$cancer_type_4 %in% c("C9291")|
                            ccc19x$cancer_type_5 %in% c("C9291"))] <- 1
    ccc19x$der_anal <- factor(ccc19x$der_anal)
    summary(ccc19x$der_anal[ccc19x$redcap_repeat_instrument == ''])
    
    #Upper GI including pancreaticohepatobiliary
    ccc19x$der_UpperGI <- 0
    ccc19x$der_UpperGI[which(ccc19x$cancer_type %in% c("C3844","C3850","C4436","C4911", "C3099", "C3513")|
                               ccc19x$cancer_type_2 %in% c("C3844","C3850","C4436","C4911", "C3099", "C3513")|
                               ccc19x$cancer_type_3 %in% c("C3844","C3850","C4436","C4911", "C3099", "C3513")|
                               ccc19x$cancer_type_4 %in% c("C3844","C3850","C4436","C4911", "C3099", "C3513")|
                               ccc19x$cancer_type_5 %in% c("C3844","C3850","C4436","C4911", "C3099", "C3513"))] <- 1
    ccc19x$der_UpperGI <- factor(ccc19x$der_UpperGI)
    summary(ccc19x$der_UpperGI[ccc19x$redcap_repeat_instrument == ''])
    
    #Upper GI excluding pancreaticohepatobiliary and including appendix, small bowel
    ccc19x$der_UpperGI_v2 <- 0
    ccc19x$der_UpperGI_v2[which(ccc19x$cancer_type %in% c("C4911","C3513","C9330","C7724")|
                                  ccc19x$cancer_type_2 %in% c("C4911","C3513","C9330","C7724")|
                                  ccc19x$cancer_type_3 %in% c("C4911","C3513","C9330","C7724")|
                                  ccc19x$cancer_type_4 %in% c("C4911","C3513","C9330","C7724")|
                                  ccc19x$cancer_type_5 %in% c("C4911","C3513","C9330","C7724"))] <- 1
    ccc19x$der_UpperGI_v2 <- factor(ccc19x$der_UpperGI_v2)
    summary(ccc19x$der_UpperGI_v2[ccc19x$redcap_repeat_instrument == ''])
    
    #Hepatobiliary (excluding pancreas)
    ccc19x$der_hepatobiliary <- 0
    ccc19x$der_hepatobiliary[which(ccc19x$cancer_type %in% c("C4436","C3844","C3099")|
                                     ccc19x$cancer_type_2 %in% c("C4436","C3844","C3099")|
                                     ccc19x$cancer_type_3 %in% c("C4436","C3844","C3099")|
                                     ccc19x$cancer_type_4 %in% c("C4436","C3844","C3099")|
                                     ccc19x$cancer_type_5 %in% c("C4436","C3844","C3099"))] <- 1
    ccc19x$der_hepatobiliary <- factor(ccc19x$der_hepatobiliary)
    summary(ccc19x$der_hepatobiliary[ccc19x$redcap_repeat_instrument == ''])
    
    #Pancreaticohepatobiliary
    ccc19x$der_pancreaticohepatobiliary <- 0
    ccc19x$der_pancreaticohepatobiliary[which(ccc19x$cancer_type %in% c("C4436","C3844","C3099","C3850")|
                                                ccc19x$cancer_type_2 %in% c("C4436","C3844","C3099","C3850")|
                                                ccc19x$cancer_type_3 %in% c("C4436","C3844","C3099","C3850")|
                                                ccc19x$cancer_type_4 %in% c("C4436","C3844","C3099","C3850")|
                                                ccc19x$cancer_type_5 %in% c("C4436","C3844","C3099","C3850"))] <- 1
    ccc19x$der_pancreaticohepatobiliary <- factor(ccc19x$der_pancreaticohepatobiliary)
    summary(ccc19x$der_pancreaticohepatobiliary[ccc19x$redcap_repeat_instrument == ''])
    
    #Esophagogastric
    ccc19x$der_esophagogastric <- 0
    ccc19x$der_esophagogastric[which(ccc19x$cancer_type %in% c("C3513","C4911")|
                                       ccc19x$cancer_type_2 %in% c("C3513","C4911")|
                                       ccc19x$cancer_type_3 %in% c("C3513","C4911")|
                                       ccc19x$cancer_type_4 %in% c("C3513","C4911")|
                                       ccc19x$cancer_type_5 %in% c("C3513","C4911"))] <- 1
    ccc19x$der_esophagogastric <- factor(ccc19x$der_esophagogastric)
    summary(ccc19x$der_esophagogastric[ccc19x$redcap_repeat_instrument == ''])
    
    #Gastric & Pancreatobiliary
    ccc19x$der_gastric_pancreatobiliary <- 0
    ccc19x$der_gastric_pancreatobiliary[which(ccc19x$cancer_type %in% c("C4911","C3850","C4436","C3844")|
                                                ccc19x$cancer_type_2 %in% c("C4911","C3850","C4436","C3844")|
                                                ccc19x$cancer_type_3 %in% c("C4911","C3850","C4436","C3844")|
                                                ccc19x$cancer_type_4 %in% c("C4911","C3850","C4436","C3844")|
                                                ccc19x$cancer_type_5 %in% c("C4911","C3850","C4436","C3844"))] <- 1
    ccc19x$der_gastric_pancreatobiliary <- factor(ccc19x$der_gastric_pancreatobiliary)
    summary(ccc19x$der_gastric_pancreatobiliary[ccc19x$redcap_repeat_instrument == ''])
    
    #GI overall
    ccc19x$der_GI <- 0
    ccc19x$der_GI[which(ccc19x$der_LowerGI == 1|ccc19x$der_UpperGI == 1)] <- 1
    ccc19x$der_GI <- factor(ccc19x$der_GI)
    
    temp <- summary(ccc19x$der_GI[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_GI',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Gyn
    ccc19x$der_Gyn <- 0
    ccc19x$der_Gyn[which(ccc19x$cancer_type %in% c("C7558", "C9039", "C7431","C3867","C3555","C3917","C4866")|
                           ccc19x$cancer_type_2 %in% c("C7558", "C9039", "C7431","C3867","C3555","C3917","C4866")|
                           ccc19x$cancer_type_3 %in% c("C7558", "C9039", "C7431","C3867","C3555","C3917","C4866")|
                           ccc19x$cancer_type_4 %in% c("C7558", "C9039", "C7431","C3867","C3555","C3917","C4866")|
                           ccc19x$cancer_type_5 %in% c("C7558", "C9039", "C7431","C3867","C3555","C3917","C4866"))] <- 1
    ccc19x$der_Gyn <- factor(ccc19x$der_Gyn)
    
    temp <- summary(ccc19x$der_Gyn[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_Gyn',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Cervix
    ccc19x$der_Cervix <- 0
    ccc19x$der_Cervix[which(ccc19x$cancer_type %in% c("C9039")|
                              ccc19x$cancer_type_2 %in% c("C9039")|
                              ccc19x$cancer_type_3 %in% c("C9039")|
                              ccc19x$cancer_type_4 %in% c("C9039")|
                              ccc19x$cancer_type_5 %in% c("C9039"))] <- 1
    ccc19x$der_Cervix <- factor(ccc19x$der_Cervix)
    summary(ccc19x$der_Cervix[ccc19x$redcap_repeat_instrument == ''])
    
    #Ovarian + Fallopian
    ccc19x$der_Ovary <- 0
    ccc19x$der_Ovary[which(ccc19x$cancer_type %in% c("C7431","C3867")|
                             ccc19x$cancer_type_2 %in% c("C7431","C3867")|
                             ccc19x$cancer_type_3 %in% c("C7431","C3867")|
                             ccc19x$cancer_type_4 %in% c("C7431","C3867")|
                             ccc19x$cancer_type_5 %in% c("C7431","C3867"))] <- 1
    ccc19x$der_Ovary <- factor(ccc19x$der_Ovary)
    summary(ccc19x$der_Ovary[ccc19x$redcap_repeat_instrument == ''])
    
    #Ovarian
    ccc19x$der_Ovary_v2 <- 0
    ccc19x$der_Ovary_v2[which(ccc19x$cancer_type %in% c("C7431")|
                                ccc19x$cancer_type_2 %in% c("C7431")|
                                ccc19x$cancer_type_3 %in% c("C7431")|
                                ccc19x$cancer_type_4 %in% c("C7431")|
                                ccc19x$cancer_type_5 %in% c("C7431"))] <- 1
    ccc19x$der_Ovary_v2 <- factor(ccc19x$der_Ovary_v2)
    summary(ccc19x$der_Ovary_v2[ccc19x$redcap_repeat_instrument == ''])
    
    #Uterine
    ccc19x$der_Uterine <- 0
    ccc19x$der_Uterine[which(ccc19x$cancer_type %in% c("C7558")|
                               ccc19x$cancer_type_2 %in% c("C7558")|
                               ccc19x$cancer_type_3 %in% c("C7558")|
                               ccc19x$cancer_type_4 %in% c("C7558")|
                               ccc19x$cancer_type_5 %in% c("C7558"))] <- 1
    ccc19x$der_Uterine <- factor(ccc19x$der_Uterine)
    summary(ccc19x$der_Uterine[ccc19x$redcap_repeat_instrument == ''])
    
    #Vagina
    ccc19x$der_Vagina <- 0
    ccc19x$der_Vagina[which(ccc19x$cancer_type %in% c("C3917")|
                              ccc19x$cancer_type_2 %in% c("C3917")|
                              ccc19x$cancer_type_3 %in% c("C3917")|
                              ccc19x$cancer_type_4 %in% c("C3917")|
                              ccc19x$cancer_type_5 %in% c("C3917"))] <- 1
    ccc19x$der_Vagina <- factor(ccc19x$der_Vagina)
    summary(ccc19x$der_Vagina[ccc19x$redcap_repeat_instrument == ''])
    
    #Vulva
    ccc19x$der_Vulva <- 0
    ccc19x$der_Vulva[which(ccc19x$cancer_type %in% c("C4866")|
                             ccc19x$cancer_type_2 %in% c("C4866")|
                             ccc19x$cancer_type_3 %in% c("C4866")|
                             ccc19x$cancer_type_4 %in% c("C4866")|
                             ccc19x$cancer_type_5 %in% c("C4866"))] <- 1
    ccc19x$der_Vulva <- factor(ccc19x$der_Vulva)
    summary(ccc19x$der_Vulva[ccc19x$redcap_repeat_instrument == ''])
    
    #Endocrine
    ccc19x$der_Endo <- 0
    ccc19x$der_Endo[which(ccc19x$cancer_type %in% c("C4815", "C9325", "C3809", "C4906")|
                            ccc19x$cancer_type_2 %in% c("C4815", "C9325", "C3809", "C4906")|
                            ccc19x$cancer_type_3 %in% c("C4815", "C9325", "C3809", "C4906")|
                            ccc19x$cancer_type_4 %in% c("C4815", "C9325", "C3809", "C4906")|
                            ccc19x$cancer_type_5 %in% c("C4815", "C9325", "C3809", "C4906"))] <- 1
    ccc19x$der_Endo <- factor(ccc19x$der_Endo)
    
    temp <- summary(ccc19x$der_Endo[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_Endo',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Neuroendocrine tumors
    ccc19x$der_NET <- 0
    ccc19x$der_NET[which(ccc19x$cancer_type %in% c("C3809")|
                           ccc19x$cancer_type_2 %in% c("C3809")|
                           ccc19x$cancer_type_3 %in% c("C3809")|
                           ccc19x$cancer_type_4 %in% c("C3809")|
                           ccc19x$cancer_type_5 %in% c("C3809"))] <- 1
    ccc19x$der_NET <- factor(ccc19x$der_NET)
    summary(ccc19x$der_NET[ccc19x$redcap_repeat_instrument == ''])
    
    #Dermatologic including invasive NMSC
    ccc19x$der_Derm <- 0
    ccc19x$der_Derm[which(ccc19x$cancer_type %in% c("C3224", "C9231","C4819","C2921")|
                            ccc19x$cancer_type_2 %in% c("C3224", "C9231","C4819","C2921")|
                            ccc19x$cancer_type_3 %in% c("C3224", "C9231","C4819","C2921")|
                            ccc19x$cancer_type_4 %in% c("C3224", "C9231","C4819","C2921")|
                            ccc19x$cancer_type_5 %in% c("C3224", "C9231","C4819","C2921"))] <- 1
    ccc19x$der_Derm <- factor(ccc19x$der_Derm)
    
    temp <- summary(ccc19x$der_Derm[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_Derm',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ccc19x$der_Melanoma <- 0
    ccc19x$der_Melanoma[which(ccc19x$cancer_type %in% c("C3224")|
                                ccc19x$cancer_type_2 %in% c("C3224")|
                                ccc19x$cancer_type_3 %in% c("C3224")|
                                ccc19x$cancer_type_4 %in% c("C3224")|
                                ccc19x$cancer_type_5 %in% c("C3224"))] <- 1
    ccc19x$der_Melanoma <- factor(ccc19x$der_Melanoma)
    summary(ccc19x$der_Melanoma[ccc19x$redcap_repeat_instrument == ''])
    
    #Head & Neck
    ccc19x$der_HN <- 0
    ccc19x$der_HN[which(ccc19x$cancer_type %in% c("C4013", "C3871")|
                          ccc19x$cancer_type_2 %in% c("C4013", "C3871")|
                          ccc19x$cancer_type_3 %in% c("C4013", "C3871")|
                          ccc19x$cancer_type_4 %in% c("C4013", "C3871")|
                          ccc19x$cancer_type_5 %in% c("C4013", "C3871"))] <- 1
    ccc19x$der_HN <- factor(ccc19x$der_HN)
    
    temp <- summary(ccc19x$der_HN[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_HN',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Sarcoma
    ccc19x$der_Sarcoma <- 0
    ccc19x$der_Sarcoma[which(ccc19x$cancer_type %in% c("C9306", "C3868", "C9145","C9312","C4817","C3359","C8538")|
                               ccc19x$cancer_type_2 %in% c("C9306", "C3868", "C9145","C9312","C4817","C3359","C8538")|
                               ccc19x$cancer_type_3 %in% c("C9306", "C3868", "C9145","C9312","C4817","C3359","C8538")|
                               ccc19x$cancer_type_4 %in% c("C9306", "C3868", "C9145","C9312","C4817","C3359","C8538")|
                               ccc19x$cancer_type_5 %in% c("C9306", "C3868", "C9145","C9312","C4817","C3359","C8538"))] <- 1
    ccc19x$der_Sarcoma <- factor(ccc19x$der_Sarcoma)
    
    temp <- summary(ccc19x$der_Sarcoma[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_Sarcoma',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    
    #Dx23a. Sarcoma subtype
    ccc19x$der_sarcoma_type <- NA
    ccc19x$der_sarcoma_type[which(ccc19x$cancer_type == 'C4817'|ccc19x$cancer_type_2 == 'C4817'|ccc19x$cancer_type_3 == 'C4817'|ccc19x$cancer_type_4 == 'C4817'|ccc19x$cancer_type_5 == 'C4817')] <- 'Ewing sarcoma'
    ccc19x$der_sarcoma_type[which((ccc19x$cancer_type == 'C3868'|ccc19x$cancer_type_2 == 'C3868'|ccc19x$cancer_type_3 == 'C3868'|ccc19x$cancer_type_4 == 'C3868'|ccc19x$cancer_type_5 == 'C3868') &
                                    !is.na(ccc19x$der_sarcoma_type))] <- 'Multiple'
    ccc19x$der_sarcoma_type[which((ccc19x$cancer_type == 'C3868'|ccc19x$cancer_type_2 == 'C3868'|ccc19x$cancer_type_3 == 'C3868'|ccc19x$cancer_type_4 == 'C3868'|ccc19x$cancer_type_5 == 'C3868') &
                                    is.na(ccc19x$der_sarcoma_type))] <- 'GIST'
    ccc19x$der_sarcoma_type[which((ccc19x$cancer_type == 'C9145'|ccc19x$cancer_type_2 == 'C9145'|ccc19x$cancer_type_3 == 'C9145'|ccc19x$cancer_type_4 == 'C9145'|ccc19x$cancer_type_5 == 'C9145') &
                                    !is.na(ccc19x$der_sarcoma_type))] <- 'Multiple'
    ccc19x$der_sarcoma_type[which((ccc19x$cancer_type == 'C9145'|ccc19x$cancer_type_2 == 'C9145'|ccc19x$cancer_type_3 == 'C9145'|ccc19x$cancer_type_4 == 'C9145'|ccc19x$cancer_type_5 == 'C9145') &
                                    is.na(ccc19x$der_sarcoma_type))] <- 'Osteosarcoma'
    ccc19x$der_sarcoma_type[which((ccc19x$cancer_type == 'C9312'|ccc19x$cancer_type_2 == 'C9312'|ccc19x$cancer_type_3 == 'C9312'|ccc19x$cancer_type_4 == 'C9312'|ccc19x$cancer_type_5 == 'C9312') &
                                    !is.na(ccc19x$der_sarcoma_type))] <- 'Multiple'
    ccc19x$der_sarcoma_type[which((ccc19x$cancer_type == 'C9312'|ccc19x$cancer_type_2 == 'C9312'|ccc19x$cancer_type_3 == 'C9312'|ccc19x$cancer_type_4 == 'C9312'|ccc19x$cancer_type_5 == 'C9312') &
                                    is.na(ccc19x$der_sarcoma_type))] <- 'Bone cancer, NOS'
    ccc19x$der_sarcoma_type[which((ccc19x$cancer_type == 'C3359'|ccc19x$cancer_type_2 == 'C3359'|ccc19x$cancer_type_3 == 'C3359'|ccc19x$cancer_type_4 == 'C3359'|ccc19x$cancer_type_5 == 'C3359') &
                                    !is.na(ccc19x$der_sarcoma_type))] <- 'Multiple'
    ccc19x$der_sarcoma_type[which((ccc19x$cancer_type == 'C3359'|ccc19x$cancer_type_2 == 'C3359'|ccc19x$cancer_type_3 == 'C3359'|ccc19x$cancer_type_4 == 'C3359'|ccc19x$cancer_type_5 == 'C3359') &
                                    is.na(ccc19x$der_sarcoma_type))] <- 'Rhabdomyosarcoma'
    ccc19x$der_sarcoma_type[which((ccc19x$cancer_type == 'C9306'|ccc19x$cancer_type_2 == 'C9306'|ccc19x$cancer_type_3 == 'C9306'|ccc19x$cancer_type_4 == 'C9306'|ccc19x$cancer_type_5 == 'C9306') &
                                    !is.na(ccc19x$der_sarcoma_type))] <- 'Multiple'
    ccc19x$der_sarcoma_type[which((ccc19x$cancer_type == 'C9306'|ccc19x$cancer_type_2 == 'C9306'|ccc19x$cancer_type_3 == 'C9306'|ccc19x$cancer_type_4 == 'C9306'|ccc19x$cancer_type_5 == 'C9306') &
                                    is.na(ccc19x$der_sarcoma_type))] <- 'Soft tisue sarcoma NOS'
    ccc19x$der_sarcoma_type[which((ccc19x$cancer_type == 'C8538'|ccc19x$cancer_type_2 == 'C8538'|ccc19x$cancer_type_3 == 'C8538'|ccc19x$cancer_type_4 == 'C8538'|ccc19x$cancer_type_5 == 'C8538') &
                                    !is.na(ccc19x$der_sarcoma_type))] <- 'Multiple'
    ccc19x$der_sarcoma_type[which((ccc19x$cancer_type == 'C8538'|ccc19x$cancer_type_2 == 'C8538'|ccc19x$cancer_type_3 == 'C8538'|ccc19x$cancer_type_4 == 'C8538'|ccc19x$cancer_type_5 == 'C8538') &
                                    is.na(ccc19x$der_sarcoma_type))] <- 'Vascular sarcoma NOS'
    
    #Soft tissue and vascular sarcoma subtypes
    ccc19x$der_sarcoma_type[which(ccc19x$sarcoma_type == 'C6496')] <- 'Undifferentiated pleomorphic sarcoma (UPS)/ Myxofibrosarcoma'
    ccc19x$der_sarcoma_type[which(ccc19x$sarcoma_type == 'C6340')] <-	'Uterine leiomyosarcoma'
    ccc19x$der_sarcoma_type[which(ccc19x$sarcoma_type == 'C3158')] <-	'Non-uterine Leiomyosarcoma'
    ccc19x$der_sarcoma_type[which(ccc19x$sarcoma_type == 'C3704')] <-	'Dedifferentiated liposarcoma'
    ccc19x$der_sarcoma_type[which(ccc19x$sarcoma_type == 'C4250')] <-	'Well differentiated liposarcoma'
    ccc19x$der_sarcoma_type[which(ccc19x$sarcoma_type == 'C27781')] <- 'Myxoid/round cell liposarcoma'
    ccc19x$der_sarcoma_type[which(ccc19x$sarcoma_type == 'C3400')] <-	'Synovial sarcoma'
    ccc19x$der_sarcoma_type[which(ccc19x$sarcoma_type == 'C3798')] <-	'Malignant peripheral nerve sheath tumor (MPNST)'
    ccc19x$der_sarcoma_type[which(ccc19x$sarcoma_type == 'C3088')] <-	'Angiosarcoma'
    ccc19x$der_sarcoma_type[which(ccc19x$sarcoma_type == 'C27005')] <-	'Spindle cell/soft tissue sarcoma NOS'
    ccc19x$der_sarcoma_type[which(ccc19x$sarcoma_type == 'C9087')] <-	'Kaposi sarcoma'
    ccc19x$der_sarcoma_type[which(ccc19x$sarcoma_type == 'OTH')] <-	'STS or Vascular - Other'
    ccc19x$der_sarcoma_type[which(ccc19x$sarcoma_type == 'UNK')] <-	'STS or Vascular - Unknown'
    
    #Check again for multiples
    temp <- data.frame(s1 = rep(0, nrow(ccc19x)),
                       s2 = rep(0, nrow(ccc19x)),
                       s3 = rep(0, nrow(ccc19x)),
                       s4 = rep(0, nrow(ccc19x)),
                       s5 = rep(0, nrow(ccc19x)), stringsAsFactors = F)
    temp$s1[which(ccc19x$cancer_type %in% c("C9306", "C3868", "C9145","C9312","C4817","C3359","C8538"))] <- 1
    temp$s2[which(ccc19x$cancer_type_2 %in% c("C9306", "C3868", "C9145","C9312","C4817","C3359","C8538"))] <- 1
    temp$s3[which(ccc19x$cancer_type_3 %in% c("C9306", "C3868", "C9145","C9312","C4817","C3359","C8538"))] <- 1
    temp$s4[which(ccc19x$cancer_type_4 %in% c("C9306", "C3868", "C9145","C9312","C4817","C3359","C8538"))] <- 1
    temp$s5[which(ccc19x$cancer_type_5 %in% c("C9306", "C3868", "C9145","C9312","C4817","C3359","C8538"))] <- 1
    ccc19x$der_sarcoma_type[which(rowSums(temp) > 1)] <- 'Multiple'
    
    ccc19x$der_sarcoma_type <- factor(ccc19x$der_sarcoma_type)
    
    temp <- summary(ccc19x$der_sarcoma_type[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_sarcoma_type',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Dx23b. Group sarcoma subtypes
    ccc19x <- within(ccc19x, {der_sarcoma_group <- 
      as.factor(ifelse(der_sarcoma_type == "Bone cancer, NOS" | 
                         der_sarcoma_type == "Osteosarcoma" | 
                         der_sarcoma_type == "Ewing sarcoma", "Bone", 
                       ifelse(der_sarcoma_type == "GIST", "GIST", 
                              ifelse(der_sarcoma_type == "Kaposi sarcoma" | 
                                       der_sarcoma_type == "Well differentiated liposarcoma", 
                                     "Other/Indolent histologies", "STS"))))})
    
    temp <- summary(ccc19x$der_sarcoma_group[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_sarcoma_group',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Neuro
    ccc19x$der_Neuro <- 0
    ccc19x$der_Neuro[which(ccc19x$cancer_type %in% c("C3059","C4627","C5111","C132067", "C3270", "C7541")|
                             ccc19x$cancer_type_2 %in% c("C3059","C4627","C5111","C132067", "C3270", "C7541")|
                             ccc19x$cancer_type_3 %in% c("C3059","C4627","C5111","C132067", "C3270", "C7541")|
                             ccc19x$cancer_type_4 %in% c("C3059","C4627","C5111","C132067", "C3270", "C7541")|
                             ccc19x$cancer_type_5 %in% c("C3059","C4627","C5111","C132067", "C3270", "C7541"))] <- 1
    ccc19x$der_Neuro <- factor(ccc19x$der_Neuro)
    
    temp <- summary(ccc19x$der_Neuro[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_Neuro',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Solid tumors NOS (including unassigned "Other" cancers)
    ccc19x$der_SolidNOS <- 0
    ccc19x$der_SolidNOS[which(ccc19x$cancer_type %in% c("C132146","C3538","C3708","C4039","OTH_S","OTH")|
                                ccc19x$cancer_type_2 %in% c("C132146","C3538","C3708","C4039","OTH_S","OTH")|
                                ccc19x$cancer_type_3 %in% c("C132146","C3538","C3708","C4039","OTH_S","OTH")|
                                ccc19x$cancer_type_4 %in% c("C132146","C3538","C3708","C4039","OTH_S","OTH")|
                                ccc19x$cancer_type_5 %in% c("C132146","C3538","C3708","C4039","OTH_S","OTH"))] <- 1
    ccc19x$der_SolidNOS <- factor(ccc19x$der_SolidNOS)
    
    temp <- summary(ccc19x$der_SolidNOS[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_SolidNOS',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Solid indicator
    ccc19x$der_solid <- NA
    ccc19x$der_solid[ccc19x$redcap_repeat_instrument == ''] <- 0
    ccc19x$der_solid[which(ccc19x$der_Breast == 1|
                             ccc19x$der_GU == 1|
                             ccc19x$der_GI == 1|
                             ccc19x$der_Thoracic == 1|
                             ccc19x$der_Gyn == 1|
                             ccc19x$der_Endo == 1|
                             ccc19x$der_Derm == 1|
                             ccc19x$der_HN == 1|
                             ccc19x$der_Sarcoma == 1|
                             ccc19x$der_Neuro == 1|
                             ccc19x$der_SolidNOS == 1)] <- 1
    
    ccc19x$der_solid <- factor(ccc19x$der_solid)
    
    temp <- summary(ccc19x$der_solid[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_solid',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Dx30. Simple single cancer type indicator
    ccc19x$der_cancer_type <- NA
    ccc19x$der_cancer_type[which(ccc19x$der_solid == 1)] <- 'Solid'
    ccc19x$der_cancer_type[which(ccc19x$der_heme == 1)] <- 'Heme'
    ccc19x$der_cancer_type[which(ccc19x$der_heme == 1 & ccc19x$der_solid == 1)] <- 'Both'
    ccc19x$der_cancer_type <- factor(ccc19x$der_cancer_type)
    summary(ccc19x$der_cancer_type[ccc19x$redcap_repeat_instrument == ''])
    
    #Dx31. der_ttype: 0 = Solid; 1 = Heme; 2 = Multiple
    ccc19x$der_ttype <- ifelse((ccc19x$der_solid == 1 & ccc19x$cancer_type_2 == ''), 0, 
                               ifelse((ccc19x$der_heme == 1 & ccc19x$cancer_type_2 == ''), 1, 2))
    
    ccc19x$der_ttype <- factor(ccc19x$der_ttype, levels = c(0,1,2), 
                               labels = c("Solid", "Heme", "Multiple"))
    
    temp <- summary(ccc19x$der_ttype[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ttype',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Dx32. Primary cancer type, with 8 levels (5 solid, 3 heme)
    ccc19x$der_pri_tumor_type <- NA
    ccc19x$der_pri_tumor_type[which(ccc19x$cancer_type %in% c("C4872"))] <- 'Breast'
    ccc19x$der_pri_tumor_type[which(ccc19x$cancer_type %in% c("C9291","C4910","C9330","C7724","C2955",
                                                              "C9382","C3844","C3850","C4436","C4911",
                                                              "C3099", "C3513"))] <- 'GI'
    ccc19x$der_pri_tumor_type[which(ccc19x$cancer_type %in% c("C4863","C9061","C9063","C4912","C6389",
                                                              "C7355","C9385","C3267","C7558", "C9039", 
                                                              "C7431","C3867","C3555","C3917","C4866",
                                                              "C3708"))] <- 'GYN/GU'
    ccc19x$der_pri_tumor_type[which(ccc19x$cancer_type %in% c("C4917", "C2926", "C4878", "C3234","C3411"))] <- 'Thoracic'
    ccc19x$der_pri_tumor_type[which(ccc19x$cancer_type %in% c("C132146","C9325","C9312","C5111","C132067",
                                                              "C3059","C4627","C4817","C3868","C4013",
                                                              "C4819","C2921","C4039","C3224","C9231",
                                                              "C3270","C3809","C9145","C4906","C7541",
                                                              "C3359","C9306","C4815","C8538","C3267",
                                                              "C3538","C3871","OTH_S"))] <- 'Other solid'
    ccc19x$der_pri_tumor_type[which(ccc19x$cancer_type %in% c("C3171","C4345","C3174","C3247"))] <- 'Myeloid'
    ccc19x$der_pri_tumor_type[which(ccc19x$cancer_type %in% c("C3457","C9244","C9357","C3211","C8851",
                                                              "C2912","C27908","C3167","C3163","C3209",
                                                              "C8504","C4341","C4337","C9308","C3242",
                                                              "C3819","C4665"))] <- 'Lymphoid'
    ccc19x$der_pri_tumor_type[which(ccc19x$cancer_type %in% c("C27134","C9300","C3106","OTH_H"))] <- 'Other heme'
    
    ccc19x$der_pri_tumor_type <- factor(ccc19x$der_pri_tumor_type)
    
    temp <- summary(ccc19x$der_pri_tumor_type[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_pri_tumor_type',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Dx32a. Primary cancer type, with 9 levels (6 solid, 3 heme)
    ccc19x$der_pri_tumor_type_v2 <- NA
    ccc19x$der_pri_tumor_type_v2[which(ccc19x$cancer_type %in% c("C4872"))] <- 'Breast'
    ccc19x$der_pri_tumor_type_v2[which(ccc19x$cancer_type %in% c("C9291","C4910","C9330","C7724","C2955",
                                                                 "C9382","C3844","C3850","C4436","C4911",
                                                                 "C3099", "C3513"))] <- 'GI'
    ccc19x$der_pri_tumor_type_v2[which(ccc19x$cancer_type %in% c("C7558", "C9039","C7431","C3867","C3555","C3917",
                                                                 "C4866","C3708"))] <- 'GYN'
    ccc19x$der_pri_tumor_type_v2[which(ccc19x$cancer_type %in% c("C4863","C9061","C9063","C4912","C6389",
                                                                 "C7355","C9385","C3267","C3708"))] <- 'GU'
    ccc19x$der_pri_tumor_type_v2[which(ccc19x$cancer_type %in% c("C4917", "C2926", "C4878", "C3234","C3411"))] <- 'Thoracic'
    ccc19x$der_pri_tumor_type_v2[which(ccc19x$cancer_type %in% c("C132146","C9325","C9312","C5111","C132067",
                                                                 "C3059","C4627","C4817","C3868","C4013",
                                                                 "C4819","C2921","C4039","C3224","C9231",
                                                                 "C3270","C3809","C9145","C4906","C7541",
                                                                 "C3359","C9306","C4815","C8538","C3267",
                                                                 "C3538","C3871","OTH_S"))] <- 'Other solid'
    ccc19x$der_pri_tumor_type_v2[which(ccc19x$cancer_type %in% c("C3171","C4345","C3174","C3247"))] <- 'Myeloid'
    ccc19x$der_pri_tumor_type_v2[which(ccc19x$cancer_type %in% c("C3457","C9244","C9357","C3211","C8851",
                                                                 "C2912","C27908","C3167","C3163","C3209",
                                                                 "C8504","C4341","C4337","C9308","C3242",
                                                                 "C3819","C4665"))] <- 'Lymphoid'
    ccc19x$der_pri_tumor_type_v2[which(ccc19x$cancer_type %in% c("C27134","C9300","C3106","OTH_H"))] <- 'Other heme'
    
    ccc19x$der_pri_tumor_type_v2 <- factor(ccc19x$der_pri_tumor_type_v2)
    
    temp <- summary(ccc19x$der_pri_tumor_type_v2[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_pri_tumor_type_v2',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
  }
  print('Cancer types completed')
  
  #######################################
  #Cancer treatment and related variables
  #######################################
  {
    "activetx" 
    #Ca1. Derived variable for whether a patient is on active cancer therapy
    #this is defined as within 1 month. 
    #If patients are on treatment and their last treatment was 2-4 wk ago, this counts
    #Note that the "hx treatment" variable lower bounds at 3 months, not 1 month
    #1 = no active tx, 2 = active therapy other than chemo, 3 = chemo, 4 = unknown
    #Recent surgery excluded from this as it is a separate variable
    ccc19x$der_activetx <- NA
    
    ccc19x$der_activetx[which(ccc19x$on_treatment == 0 |
                                (ccc19x$on_treatment == 1 & ccc19x$recent_treatment %in% c(3,88)))] <- 'None'
    
    temp.ref <- which(ccc19x$on_treatment == 1 & ccc19x$recent_treatment %in% 1:2)
    
    #First everything but chemo
    ccc19x$der_activetx[temp.ref[which(ccc19x$treatment_modality___694[temp.ref] == 1|
                                         ccc19x$treatment_modality___58229[temp.ref] == 1|
                                         ccc19x$treatment_modality___691[temp.ref] == 1|
                                         ccc19x$treatment_modality___695[temp.ref] == 1|
                                         ccc19x$treatment_modality___45186[temp.ref] == 1|
                                         ccc19x$treatment_modality___14051[temp.ref] == 1|
                                         ccc19x$treatment_modality___oth[temp.ref] == 1|
                                         ccc19x$treatment_modality___45215[temp.ref] == 1)
    ]] <- 'Non-cytotoxic'
    
    #Chemo - overwrite if needed
    ccc19x$der_activetx[temp.ref[which(ccc19x$treatment_modality___685[temp.ref] == 1)]] <- 'Cytotoxic'
    
    #Re-assign surgery only to no active treatment
    ccc19x$der_activetx[temp.ref[which(ccc19x$treatment_modality___685[temp.ref] == 0 &
                                         ccc19x$treatment_modality___694[temp.ref] == 0 &
                                         ccc19x$treatment_modality___58229[temp.ref] == 0 &
                                         ccc19x$treatment_modality___691[temp.ref] == 0 &
                                         ccc19x$treatment_modality___695[temp.ref] == 0 &
                                         ccc19x$treatment_modality___45186[temp.ref] == 0 &
                                         ccc19x$treatment_modality___oth[temp.ref] == 0 &
                                         ccc19x$treatment_modality___45215[temp.ref] == 0 &
                                         ccc19x$treatment_modality___14051[temp.ref] == 1)
    ]] <- 'None'
    
    ccc19x$der_activetx[which(ccc19x$on_treatment == 99 | 
                                ccc19x$recent_treatment == 99 |
                                (ccc19x$on_treatment == 1 & is.na(ccc19x$recent_treatment))
    )] <- 'Unknown'
    
    #Factor
    ccc19x$der_activetx <- as.factor(ccc19x$der_activetx)
    ccc19x$der_activetx <- relevel(ccc19x$der_activetx, ref = 'None')
    summary(ccc19x$der_activetx)
    
    #Ca1a. Derived variable for what type of active cancer therapy within 3 months - 4 levels with overwriting
    ccc19x$der_activetx3mo <- NA
    
    ccc19x$der_activetx3mo[which(ccc19x$on_treatment == 0 |
                                   (ccc19x$on_treatment == 1 & ccc19x$recent_treatment %in% c(88)))] <- 'None'
    
    temp.ref <- which((ccc19x$on_treatment == 1 & ccc19x$recent_treatment %in% 1:3)|
                        ccc19x$hx_treatment == 1)
    
    #First everything but chemo
    ccc19x$der_activetx3mo[temp.ref[which(ccc19x$treatment_modality___694[temp.ref] == 1|
                                            ccc19x$treatment_modality___58229[temp.ref] == 1|
                                            ccc19x$treatment_modality___691[temp.ref] == 1|
                                            ccc19x$treatment_modality___695[temp.ref] == 1|
                                            ccc19x$treatment_modality___45186[temp.ref] == 1|
                                            ccc19x$treatment_modality___14051[temp.ref] == 1|
                                            ccc19x$treatment_modality___oth[temp.ref] == 1|
                                            ccc19x$treatment_modality___45215[temp.ref] == 1)
    ]] <- 'Non-cytotoxic'
    
    #Chemo - overwrite if needed
    ccc19x$der_activetx3mo[temp.ref[which(ccc19x$treatment_modality___685[temp.ref] == 1)]] <- 'Cytotoxic'
    
    #Re-assign surgery only to no active treatment
    ccc19x$der_activetx3mo[temp.ref[which(ccc19x$treatment_modality___685[temp.ref] == 0 &
                                            ccc19x$treatment_modality___694[temp.ref] == 0 &
                                            ccc19x$treatment_modality___58229[temp.ref] == 0 &
                                            ccc19x$treatment_modality___691[temp.ref] == 0 &
                                            ccc19x$treatment_modality___695[temp.ref] == 0 &
                                            ccc19x$treatment_modality___45186[temp.ref] == 0 &
                                            ccc19x$treatment_modality___oth[temp.ref] == 0 &
                                            ccc19x$treatment_modality___45215[temp.ref] == 0 &
                                            ccc19x$treatment_modality___14051[temp.ref] == 1)
    ]] <- 'None'
    
    ccc19x$der_activetx3mo[which(ccc19x$on_treatment == 99 | 
                                   ccc19x$recent_treatment == 99 |
                                   ccc19x$hx_treatment == 99 |
                                   (ccc19x$on_treatment == 1 & is.na(ccc19x$recent_treatment))
    )] <- 'Unknown'
    
    #Factor
    ccc19x$der_activetx3mo <- as.factor(ccc19x$der_activetx3mo)
    ccc19x$der_activetx3mo <- relevel(ccc19x$der_activetx3mo, ref = 'None')
    summary(ccc19x$der_activetx3mo[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca1b. Derived variable for what type of active cancer therapy within 3 months - 11 levels, mutually exclusive
    ccc19x$der_activetx3mo_v2 <- NA
    
    ccc19x$der_activetx3mo_v2[which((ccc19x$on_treatment == 0 & ccc19x$hx_treatment != 1)|
                                      (ccc19x$on_treatment == 1 & ccc19x$recent_treatment %in% c(88) & ccc19x$hx_treatment != 1))] <- 'None'
    
    temp.ref <- which((ccc19x$on_treatment == 1 & ccc19x$recent_treatment %in% 1:3)|
                        ccc19x$hx_treatment == 1)
    
    #Chemotherapy
    ccc19x$der_activetx3mo_v2[temp.ref[which((ccc19x$treatment_modality___685[temp.ref] == 1|
                                                (ccc19x$treatment_modality___45186[temp.ref] == 1 & ccc19x$transplant_cellular_therapy[temp.ref] == 1)) &
                                               ccc19x$treatment_modality___694[temp.ref] == 0 &
                                               ccc19x$treatment_modality___58229[temp.ref] == 0 &
                                               ccc19x$treatment_modality___691[temp.ref] == 0 &
                                               ccc19x$treatment_modality___695[temp.ref] == 0 &
                                               !(ccc19x$treatment_modality___45186[temp.ref] == 1 & ccc19x$transplant_cellular_therapy[temp.ref] %in% c(10,2,3,4,5,6)) &
                                               ccc19x$treatment_modality___14051[temp.ref] == 0 &
                                               ccc19x$treatment_modality___oth[temp.ref] == 0 &
                                               ccc19x$treatment_modality___45215[temp.ref] == 0)]] <- 'Cytotoxic chemotherapy'
    
    #Immunotherapy
    ccc19x$der_activetx3mo_v2[temp.ref[which(ccc19x$treatment_modality___685[temp.ref] == 0 &
                                               !(ccc19x$treatment_modality___45186[temp.ref] == 1 & ccc19x$transplant_cellular_therapy[temp.ref] == 1) &
                                               (ccc19x$treatment_modality___694[temp.ref] == 1|
                                                  (ccc19x$treatment_modality___45186[temp.ref] == 1 & ccc19x$transplant_cellular_therapy[temp.ref] %in% c(10,2,3,4,5,6))) &
                                               ccc19x$treatment_modality___58229[temp.ref] == 0 &
                                               ccc19x$treatment_modality___691[temp.ref] == 0 &
                                               ccc19x$treatment_modality___695[temp.ref] == 0 &
                                               ccc19x$treatment_modality___14051[temp.ref] == 0 &
                                               ccc19x$treatment_modality___oth[temp.ref] == 0 &
                                               ccc19x$treatment_modality___45215[temp.ref] == 0)]] <- 'Immunotherapy'
    
    #Targeted therapy
    ccc19x$der_activetx3mo_v2[temp.ref[which(ccc19x$treatment_modality___685[temp.ref] == 0 &
                                               ccc19x$treatment_modality___694[temp.ref] == 0 &
                                               ccc19x$treatment_modality___58229[temp.ref] == 1 &
                                               ccc19x$treatment_modality___691[temp.ref] == 0 &
                                               ccc19x$treatment_modality___695[temp.ref] == 0 &
                                               ccc19x$treatment_modality___45186[temp.ref] == 0 &
                                               ccc19x$treatment_modality___14051[temp.ref] == 0 &
                                               ccc19x$treatment_modality___oth[temp.ref] == 0 &
                                               ccc19x$treatment_modality___45215[temp.ref] == 0)]] <- 'Targeted therapy'
    
    #Endocrine therapy
    ccc19x$der_activetx3mo_v2[temp.ref[which(ccc19x$treatment_modality___685[temp.ref] == 0 &
                                               ccc19x$treatment_modality___694[temp.ref] == 0 &
                                               ccc19x$treatment_modality___58229[temp.ref] == 0 &
                                               ccc19x$treatment_modality___691[temp.ref] == 1 &
                                               ccc19x$treatment_modality___695[temp.ref] == 0 &
                                               ccc19x$treatment_modality___45186[temp.ref] == 0 &
                                               ccc19x$treatment_modality___14051[temp.ref] == 0 &
                                               ccc19x$treatment_modality___oth[temp.ref] == 0 &
                                               ccc19x$treatment_modality___45215[temp.ref] == 0)]] <- 'Endocrine therapy'
    
    #Radiotherapy
    ccc19x$der_activetx3mo_v2[temp.ref[which(ccc19x$treatment_modality___685[temp.ref] == 0 &
                                               ccc19x$treatment_modality___694[temp.ref] == 0 &
                                               ccc19x$treatment_modality___58229[temp.ref] == 0 &
                                               ccc19x$treatment_modality___691[temp.ref] == 0 &
                                               ccc19x$treatment_modality___695[temp.ref] == 1 &
                                               ccc19x$treatment_modality___45186[temp.ref] == 0 &
                                               ccc19x$treatment_modality___14051[temp.ref] == 0 &
                                               ccc19x$treatment_modality___oth[temp.ref] == 0 &
                                               ccc19x$treatment_modality___45215[temp.ref] == 0)]] <- 'Radiotherapy'
    
    #Surgery
    ccc19x$der_activetx3mo_v2[temp.ref[which(ccc19x$treatment_modality___685[temp.ref] == 0 &
                                               ccc19x$treatment_modality___694[temp.ref] == 0 &
                                               ccc19x$treatment_modality___58229[temp.ref] == 0 &
                                               ccc19x$treatment_modality___691[temp.ref] == 0 &
                                               ccc19x$treatment_modality___695[temp.ref] == 0 &
                                               ccc19x$treatment_modality___45186[temp.ref] == 0 &
                                               ccc19x$treatment_modality___14051[temp.ref] == 1 &
                                               ccc19x$treatment_modality___oth[temp.ref] == 0 &
                                               ccc19x$treatment_modality___45215[temp.ref] == 0)]] <- 'Surgery'
    
    #Chemoradiotherapy
    ccc19x$der_activetx3mo_v2[temp.ref[which((ccc19x$treatment_modality___685[temp.ref] == 1|
                                                (ccc19x$treatment_modality___45186[temp.ref] == 1 & ccc19x$transplant_cellular_therapy[temp.ref] == 1)) &
                                               ccc19x$treatment_modality___694[temp.ref] == 0 &
                                               ccc19x$treatment_modality___58229[temp.ref] == 0 &
                                               ccc19x$treatment_modality___691[temp.ref] == 0 &
                                               ccc19x$treatment_modality___695[temp.ref] == 1 &
                                               !(ccc19x$treatment_modality___45186[temp.ref] == 1 & ccc19x$transplant_cellular_therapy[temp.ref] %in% c(10,2,3,4,5,6)) &
                                               ccc19x$treatment_modality___14051[temp.ref] == 0 &
                                               ccc19x$treatment_modality___oth[temp.ref] == 0 &
                                               ccc19x$treatment_modality___45215[temp.ref] == 0)]] <- 'Chemoradiotherapy'
    
    #Chemoimmunotherapy
    ccc19x$der_activetx3mo_v2[temp.ref[which((ccc19x$treatment_modality___685[temp.ref] == 1|
                                                (ccc19x$treatment_modality___45186[temp.ref] == 1 & ccc19x$transplant_cellular_therapy[temp.ref] == 1)) &
                                               (ccc19x$treatment_modality___694[temp.ref] == 1|
                                                  (ccc19x$treatment_modality___45186[temp.ref] == 1 & ccc19x$transplant_cellular_therapy[temp.ref] %in% c(10,2,3,4,5,6))) &
                                               ccc19x$treatment_modality___58229[temp.ref] == 0 &
                                               ccc19x$treatment_modality___691[temp.ref] == 0 &
                                               ccc19x$treatment_modality___695[temp.ref] == 0 &
                                               ccc19x$treatment_modality___14051[temp.ref] == 0 &
                                               ccc19x$treatment_modality___oth[temp.ref] == 0 &
                                               ccc19x$treatment_modality___45215[temp.ref] == 0)]] <- 'Chemoimmunotherapy'
    
    #Other
    temp.ref2 <- which(is.na(ccc19x$der_activetx3mo_v2[temp.ref]))
    ccc19x$der_activetx3mo_v2[temp.ref[temp.ref2]] <- 'Other'
    
    #Unknown (do not allow overwriting)
    ccc19x$der_activetx3mo_v2[which((ccc19x$on_treatment == 99 | 
                                       ccc19x$recent_treatment == 99 |
                                       ccc19x$hx_treatment == 99 |
                                       (ccc19x$on_treatment == 1 & is.na(ccc19x$recent_treatment))) &
                                      is.na(ccc19x$der_activetx3mo_v2))] <- 'Unknown'
    
    #Factor
    ccc19x$der_activetx3mo_v2 <- as.factor(ccc19x$der_activetx3mo_v2)
    ccc19x$der_activetx3mo_v2 <- relevel(ccc19x$der_activetx3mo_v2, ref = 'None')
    summary(ccc19x$der_activetx3mo_v2[ccc19x$redcap_repeat_instrument == ''])
    
    ############################################
    #Ca10. Any cancer treatment in past 3 months
    ############################################
    ccc19x$der_anytx_3mo <- NA
    
    #Definitely yes
    ccc19x$der_anytx_3mo[which((ccc19x$on_treatment == 1 & ccc19x$recent_treatment %in% 1:3)|
                                 ccc19x$hx_treatment == 1)] <- 1
    
    #Definitely no
    ccc19x$der_anytx_3mo[which((ccc19x$on_treatment == 0 & ccc19x$hx_treatment != 1)|
                                 (ccc19x$on_treatment == 1 & ccc19x$recent_treatment %in% 88 & ccc19x$hx_treatment != 1)|
                                 ccc19x$recent_treatment == 98)] <- 0
    
    #Unknown
    ccc19x$der_anytx_3mo[which((ccc19x$on_treatment == 99 | 
                                  ccc19x$recent_treatment == 99 |
                                  ccc19x$hx_treatment == 99 ) &
                                 is.na(ccc19x$der_anytx_3mo))] <- 99
    
    #Probably yes (timing variable left unfilled)
    ccc19x$der_anytx_3mo[which(ccc19x$on_treatment == 1 & 
                                 is.na(ccc19x$recent_treatment) &
                                 is.na(ccc19x$der_anytx_3mo))] <- 1
    
    #Probably no (timing variable left unfilled)
    ccc19x$der_anytx_3mo[which(ccc19x$on_treatment == 0 & 
                                 is.na(ccc19x$hx_treatment) &
                                 is.na(ccc19x$der_anytx_3mo))] <- 0
    
    ccc19x$der_anytx_3mo <- factor(ccc19x$der_anytx_3mo)
    
    temp <- summary(ccc19x$der_anytx_3mo[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_anytx_3mo',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #####################################################################
    #Second- and third-order derived variables for timing within 3 months
    #####################################################################
    {
      #There are four "buckets" of systemic anticancer therapy: cytotoxic chemotherapy,
      # immunotherapy, targeted therapy, and endocrine therapy.
      #####################################
      #Ca10a. Any cytotoxic within 3 months
      #####################################
      ccc19x$der_any_cyto_3mo <- ccc19x$der_anytx_3mo
      ccc19x$der_any_cyto_3mo[which(ccc19x$der_any_cyto_3mo == 1 & 
                                      ((ccc19x$treatment_modality___685 == 0 & ccc19x$treatment_modality___45186 == 0)|
                                         (ccc19x$treatment_modality___694 == 0 &
                                            ccc19x$treatment_modality___45186 == 1 & ccc19x$transplant_cellular_therapy %in% c(10,2,3,4,5,6))))] <- 0
      
      temp <- summary(ccc19x$der_any_cyto_3mo[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_any_cyto_3mo',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      #########################################
      #Ca10b. Any immunotherapy within 3 months
      #########################################
      ccc19x$der_any_immuno_3mo <- ccc19x$der_anytx_3mo
      ccc19x$der_any_immuno_3mo[which(ccc19x$der_any_immuno_3mo == 1 & 
                                        ((ccc19x$treatment_modality___694 == 0 & ccc19x$treatment_modality___45186 == 0)|
                                           (ccc19x$treatment_modality___694 == 0 &
                                              ccc19x$treatment_modality___45186 == 1 & ccc19x$transplant_cellular_therapy == 1)))] <- 0
      
      temp <- summary(ccc19x$der_any_immuno_3mo[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_any_immuno_3mo',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      ############################################
      #Ca10c. Any targeted therapy within 3 months
      ############################################
      ccc19x$der_any_targeted_3mo <- ccc19x$der_anytx_3mo
      ccc19x$der_any_targeted_3mo[which(ccc19x$der_any_targeted_3mo == 1 & ccc19x$treatment_modality___58229 == 0)] <- 0
      
      temp <- summary(ccc19x$der_any_targeted_3mo[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_any_targeted_3mo',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      #############################################
      #Ca10d. Any endocrine therapy within 3 months
      #############################################
      ccc19x$der_any_endo_3mo <- ccc19x$der_anytx_3mo
      ccc19x$der_any_endo_3mo[which(ccc19x$der_any_endo_3mo == 1 & ccc19x$treatment_modality___691 == 0)] <- 0
      
      temp <- summary(ccc19x$der_any_endo_3mo[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_any_endo_3mo',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      #####################################
      #Ca10k. Any systemic therapy within 3 months (third-order variable)
      #####################################
      ccc19x$der_any_systemic_3mo <- ccc19x$der_any_cyto_3mo
      ccc19x$der_any_systemic_3mo[which(ccc19x$der_any_endo_3mo == 1|
                                          ccc19x$der_any_immuno_3mo == 1|
                                          ccc19x$der_any_targeted_3mo == 1)] <- 1
      
      temp <- summary(ccc19x$der_any_systemic_3mo[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_any_systemic_3mo',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      #Ca10ka. Same as above but omits the endocrine
      ccc19x$der_any_systemic_3mo_v2 <- ccc19x$der_any_cyto_3mo
      ccc19x$der_any_systemic_3mo_v2[which(ccc19x$der_any_immuno_3mo == 1|
                                             ccc19x$der_any_targeted_3mo == 1)] <- 1
      
      temp <- summary(ccc19x$der_any_systemic_3mo_v2[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_any_systemic_3mo_v2',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      #Ca10e. Any radiation therapy within 3 months
      ccc19x$der_any_rt_3mo <- ccc19x$der_anytx_3mo
      ccc19x$der_any_rt_3mo[which(ccc19x$der_any_rt_3mo == 1 & ccc19x$treatment_modality___695 == 0)] <- 0
      
      temp <- summary(ccc19x$der_any_rt_3mo[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_any_rt_3mo',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      #Ca10f. Any cancer surgery within 3 months
      ccc19x$der_any_ca_surgery_3mo <- ccc19x$der_anytx_3mo
      ccc19x$der_any_ca_surgery_3mo[which(ccc19x$der_any_ca_surgery_3mo == 1 & ccc19x$treatment_modality___14051 == 0)] <- 0
      
      temp <- summary(ccc19x$der_any_ca_surgery_3mo[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_any_ca_surgery_3mo',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      #Ca10g. Any other therapy within 3 months
      ccc19x$der_any_other_3mo <- ccc19x$der_anytx_3mo
      ccc19x$der_any_other_3mo[which(ccc19x$der_any_other_3mo == 1 & 
                                       ccc19x$treatment_modality___45215 == 0 &
                                       ccc19x$treatment_modality___oth == 0)] <- 0
      
      temp <- summary(ccc19x$der_any_other_3mo[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_any_other_3mo',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      #Ca10h. Any targeted therapy or ICI within 3 months
      ccc19x$der_any_targeted_ici_3mo <- ccc19x$der_anytx_3mo
      ccc19x$der_any_targeted_ici_3mo[which(ccc19x$der_any_targeted_ici_3mo == 1 & 
                                              (ccc19x$treatment_modality___58229 == 1|
                                                 ccc19x$what_immunotherapy %in% c('45838', '45446',
                                                                                  '45170', '45838-45446')))] <- 1
      ccc19x$der_any_targeted_ici_3mo[which(ccc19x$der_any_targeted_ici_3mo == 1 & (ccc19x$treatment_modality___58229 == 0|
                                                                                      !ccc19x$what_immunotherapy %in% c('45838', '45446',
                                                                                                                        '45170', '45838-45446')))] <- 0
      
      ccc19x$der_any_targeted_ici_3mo <- factor(ccc19x$der_any_targeted_ici_3mo)
      
      temp <- summary(ccc19x$der_any_targeted_ici_3mo[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_any_targeted_ici_3mo',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
      
      #Ca10i. Any transplant or cellular therapy within 3 months
      ccc19x$der_any_sct_cellular_3mo <- ccc19x$der_anytx_3mo
      ccc19x$der_any_sct_cellular_3mo[which(ccc19x$der_any_sct_cellular_3mo == 1 & ccc19x$treatment_modality___45186 == 0)] <- 0
      ccc19x$der_any_sct_cellular_3mo[which(ccc19x$transplant_cellular_timing %in% c(0,3,4))] <- 0
      ccc19x$der_any_sct_cellular_3mo[which(ccc19x$transplant_cellular_timing %in% 1:2)] <- 1
      ccc19x$der_any_sct_cellular_3mo[which(ccc19x$transplant_cellular_timing == 99)] <- 99
      
      ccc19x$der_any_sct_cellular_3mo <- factor(ccc19x$der_any_sct_cellular_3mo)
      
      temp <- summary(ccc19x$der_any_sct_cellular_3mo[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_any_sct_cellular_3mo',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
    }
    
    ############################################
    #Ca10x. Any cancer treatment in past 4 weeks
    ############################################
    ccc19x$der_anytx_4wk <- NA
    
    ccc19x$der_anytx_4wk[which(ccc19x$on_treatment == 1 & ccc19x$recent_treatment %in% 1:2)] <- 1
    
    ccc19x$der_anytx_4wk[which((ccc19x$on_treatment == 0 & ccc19x$hx_treatment != 1)|
                                 (ccc19x$on_treatment == 1 & ccc19x$recent_treatment %in% 3:88)|
                                 ccc19x$recent_treatment == 98)] <- 0
    
    ccc19x$der_anytx_4wk[which((ccc19x$on_treatment == 99 | 
                                  ccc19x$recent_treatment == 99 |
                                  ccc19x$hx_treatment %in% c(1,99) |  #This includes treatment completed within 3 months
                                  (ccc19x$on_treatment == 1 & is.na(ccc19x$recent_treatment))) &
                                 is.na(ccc19x$der_anytx_4wk))] <- 99
    
    ccc19x$der_anytx_4wk <- factor(ccc19x$der_anytx_4wk)
    summary(ccc19x$der_anytx_4wk[ccc19x$redcap_repeat_instrument == ''])
    
    ###################################
    #Ca22. Recency of cytotoxic therapy
    ###################################
    ccc19x$der_cyto_timing<- NA
    
    #Within 4 weeks
    ccc19x$der_cyto_timing[which(ccc19x$recent_treatment %in% 1:2 & ccc19x$treatment_modality___685 == 1)] <- "Within 4 weeks"
    
    #4 weeks - 3 months
    ccc19x$der_cyto_timing[which(ccc19x$recent_treatment %in% 3 & ccc19x$treatment_modality___685 == 1)] <- '4 weeks to 3 months'
    
    #Within 3 months (special case)
    ccc19x$der_cyto_timing[which(ccc19x$hx_treatment == 1 & ccc19x$treatment_modality___685 == 1 &
                                   is.na(ccc19x$der_cyto_timing))] <- 'Within 3 months'
    
    #3 - 12 mo
    ccc19x$der_cyto_timing[which(ccc19x$hx_treatment %in% 2 & ccc19x$treatment_modality___685 == 1)] <- '3 to 12 months'
    
    #More than 3 months (special case)
    ccc19x$der_cyto_timing[which(ccc19x$recent_treatment == 88 & ccc19x$treatment_modality___685 == 1 &
                                   is.na(ccc19x$der_cyto_timing))] <- 'More than 3 months'
    
    #Treatment initiated after COVID-19
    ccc19x$der_cyto_timing[which(ccc19x$recent_treatment == 98 & ccc19x$treatment_modality___685 == 1 &
                                   is.na(ccc19x$der_cyto_timing))] <- 'After COVID-19 diagnosis'
    
    #12 mo or greater
    ccc19x$der_cyto_timing[which(ccc19x$hx_treatment %in% c(3,88))] <- 'None within a year'
    
    #Unknown (masked)
    ccc19x$der_cyto_timing[which((ccc19x$recent_treatment %in% 1:88|ccc19x$hx_treatment %in% c(1,2)) & 
                                   ccc19x$treatment_modality___685 == 0 &
                                   is.na(ccc19x$der_cyto_timing))] <- 'Unknown (masked)'
    
    #Unknown
    ccc19x$der_cyto_timing[which(ccc19x$recent_treatment == 99|ccc19x$hx_treatment == 99)] <- 'Unknown'
    
    #Factor
    ccc19x$der_cyto_timing <- as.factor(ccc19x$der_cyto_timing)
    ccc19x$der_cyto_timing <- relevel(ccc19x$der_cyto_timing, ref = 'None within a year')
    summary(ccc19x$der_cyto_timing[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca10f1. Any ***surgery*** within 30 days
    ccc19x$der_any_surgery_1mo <- ccc19x$der_anytx_4wk
    ccc19x$der_any_surgery_1mo[which(ccc19x$der_any_surgery_1mo == 1 & ccc19x$treatment_modality___14051 == 0)] <- 0
    ccc19x$der_any_surgery_1mo[which(ccc19x$surgery_timing == 1)] <- 1
    ccc19x$der_any_surgery_1mo[which(ccc19x$surgery_timing %in% 2:3 &
                                       (is.na(ccc19x$der_any_surgery_1mo)|ccc19x$der_any_surgery_1mo == 99))] <- 0
    ccc19x$der_any_surgery_1mo[which(ccc19x$surgery_timing == 'UNK' & ccc19x$der_any_surgery_1mo == 0)] <- 99
    summary(ccc19x$der_any_surgery_1mo[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca10j. Any local therapy within 3 months (surgery or radiation)
    ccc19x$der_any_local_3mo <- ccc19x$der_any_ca_surgery_3mo
    ccc19x$der_any_local_3mo[which(ccc19x$der_any_rt_3mo == 1)] <- 1
    
    temp <- summary(ccc19x$der_any_local_3mo[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_any_local_3mo',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca10z. No cancer therapy within 3 months
    ccc19x$der_cancertr_none_3mo <- NA
    
    #Yes
    ccc19x$der_cancertr_none_3mo[which(ccc19x$der_any_cyto_3mo == 0 &
                                     ccc19x$der_any_targeted_3mo == 0 &
                                     ccc19x$der_any_endo_3mo == 0 &
                                     ccc19x$der_any_immuno_3mo == 0 &
                                     ccc19x$der_any_local_3mo == 0 &
                                     ccc19x$der_any_other_3mo == 0)] <- 1
    
    #No
    ccc19x$der_cancertr_none_3mo[which(ccc19x$der_any_cyto_3mo == 1|
                                     ccc19x$der_any_targeted_3mo == 1|
                                     ccc19x$der_any_endo_3mo == 1|
                                     ccc19x$der_any_immuno_3mo == 1|
                                     ccc19x$der_any_local_3mo == 1|
                                     ccc19x$der_any_other_3mo == 1)] <- 0
    
    #Unknown
    ccc19x$der_cancertr_none_3mo[which(ccc19x$der_any_cyto_3mo == 99|
                                     ccc19x$der_any_targeted_3mo == 99|
                                     ccc19x$der_any_endo_3mo == 99|
                                     ccc19x$der_any_immuno_3mo == 99|
                                     ccc19x$der_any_local_3mo == 99|
                                     ccc19x$der_any_other_3mo == 99)] <- 99

    ccc19x$der_cancertr_none_3mo <- as.factor(ccc19x$der_cancertr_none_3mo)
    
    temp <- summary(ccc19x$der_cancertr_none_3mo[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_cancertr_none_3mo',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca10k1. Any systemic therapy within 12 months
    ccc19x$der_any_systemic_v2 <- NA
    
    #Yes
    ccc19x$der_any_systemic_v2[which((ccc19x$treatment_modality___685 == 1|
                                    ccc19x$treatment_modality___694 == 1|
                                    ccc19x$treatment_modality___58229 == 1|
                                      ccc19x$treatment_modality___691 == 1|
                                      ccc19x$treatment_modality___45186 == 1) &
                                      (ccc19x$recent_treatment %in% 1:3|ccc19x$hx_treatment %in% 1:2))] <- 1
    
    #No
    ccc19x$der_any_systemic_v2[which(ccc19x$hx_treatment %in% 3:88)] <- 0
    ccc19x$der_any_systemic_v2[which(ccc19x$on_treatment == 1 & ccc19x$recent_treatment == 98)] <- 0
    ccc19x$der_any_systemic_v2[which((ccc19x$treatment_modality___685 == 0 &
                                        ccc19x$treatment_modality___694 == 0 &
                                        ccc19x$treatment_modality___58229 == 0 &
                                        ccc19x$treatment_modality___691 == 0 &
                                        ccc19x$treatment_modality___45186 == 0) &
                                       (ccc19x$recent_treatment %in% 1:3|ccc19x$hx_treatment %in% 1:2))] <- 0
    #Unknown
    ccc19x$der_any_systemic_v2[which((ccc19x$on_treatment == 99|
                                        ccc19x$recent_treatment == 99|
                                        ccc19x$hx_treatment == 99) & is.na(ccc19x$der_any_systemic_v2))] <- 99
    
    ccc19x$der_any_systemic_v2 <- factor(ccc19x$der_any_systemic_v2)
    summary(ccc19x$der_any_systemic_v2[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca10k2. Any systemic therapy within 3-12 months
    ccc19x$der_any_systemic_v3 <- ccc19x$der_any_systemic_v2
    ccc19x$der_any_systemic_v3[which(ccc19x$der_any_systemic_3mo == 1)] <- 0
    summary(ccc19x$der_any_systemic_v3[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca10a1. Any cytotoxic chemotherapy within 12 months
    ccc19x$der_any_cyto_12mo <- ccc19x$der_any_systemic_v2
    ccc19x$der_any_cyto_12mo[which(ccc19x$der_any_cyto_12mo == 1 & ccc19x$treatment_modality___685 == 0)] <- 0
    
    temp <- summary(ccc19x$der_any_cyto_12mo[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_any_cyto_12mo',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca10c1. Any targeted therapy within 12 months
    ccc19x$der_any_targeted_12mo <- ccc19x$der_any_systemic_v2
    ccc19x$der_any_targeted_12mo[which(ccc19x$der_any_targeted_12mo == 1 & ccc19x$treatment_modality___58229 == 0)] <- 0
    
    temp <- summary(ccc19x$der_any_targeted_12mo[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_any_targeted_12mo',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca10d1. Any endocrine therapy within 12 months
    ccc19x$der_any_endo_12mo <- ccc19x$der_any_systemic_v2
    ccc19x$der_any_endo_12mo[which(ccc19x$der_any_endo_12mo == 1 & ccc19x$treatment_modality___691 == 0)] <- 0
    
    temp <- summary(ccc19x$der_any_endo_12mo[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_any_endo_12mo',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca10d1. Any cellular therapy within 12 months
    ccc19x$der_any_sct_cellular_12mo <- ccc19x$der_any_systemic_v2
    ccc19x$der_any_sct_cellular_12mo[which(ccc19x$der_any_sct_cellular_12mo == 1 & ccc19x$treatment_modality___45186 == 0)] <- 0
    ccc19x$der_any_sct_cellular_12mo[which(ccc19x$transplant_cellular_timing %in% c(0,4))] <- 0
    ccc19x$der_any_sct_cellular_12mo[which(ccc19x$transplant_cellular_timing %in% 1:3)] <- 1
    ccc19x$der_any_sct_cellular_12mo[which(ccc19x$transplant_cellular_timing == 99)] <- 99
    summary(ccc19x$der_any_sct_cellular_12mo[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca10l. Any intravesicular BCG therapy (no time restriction) - declare as none
    ccc19x$der_any_intravesicular_bcg <- 0
    ccc19x$der_any_intravesicular_bcg[which(ccc19x$intravesicular_bcg == 1|
                                    ccc19x$bcg_intraves_ever == 1)] <- 1
    ccc19x$der_any_intravesicular_bcg[which((ccc19x$intravesicular_bcg == 99|
                                              ccc19x$bcg_intraves_ever == 99) &
                                              ccc19x$der_any_intravesicular_bcg == 0)] <- 99
    ccc19x$der_any_intravesicular_bcg <- factor(ccc19x$der_any_intravesicular_bcg)
    summary(ccc19x$der_any_intravesicular_bcg[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca12. Allogeneic transplant within one year (default is no)
    ccc19x$der_allo365 <- 0
    ccc19x$der_allo365[which(ccc19x$transplant_cellular_therapy %in% c(2:5,10) &
                           ccc19x$transplant_cellular_timing %in% 1:3)] <- 1
    ccc19x$der_allo365[which(ccc19x$transplant_cellular_therapy %in% c(2:5,10) &
                           ccc19x$transplant_cellular_timing == 99)] <- 99
    ccc19x$der_allo365[which(ccc19x$transplant_cellular_therapy %in% c(2:5,10) &
                               is.na(ccc19x$transplant_cellular_timing))] <- NA
    
    ccc19x$der_allo365 <- factor(ccc19x$der_allo365)
    
    temp <- summary(ccc19x$der_allo365[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_allo365',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca13. Autologous transplant within 100 days (default is no)
    ccc19x$der_auto100 <- 0
    ccc19x$der_auto100[which(ccc19x$transplant_cellular_therapy == 1 &
                               ccc19x$transplant_cellular_timing %in% 1:2)] <- 1
    ccc19x$der_auto100[which(ccc19x$transplant_cellular_therapy == 1 &
                               ccc19x$transplant_cellular_timing == 99)] <- 99
    ccc19x$der_auto100[which(ccc19x$transplant_cellular_therapy == 1 &
                               is.na(ccc19x$transplant_cellular_timing))] <- NA
    
    ccc19x$der_auto100 <- factor(ccc19x$der_auto100)
    
    temp <- summary(ccc19x$der_auto100[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_auto100',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #########################################
    #Ca23. Hematopoeitic cell transplant ever
    #########################################
    ccc19x$der_hct <- NA
    
    #Yes
    ccc19x$der_hct[which(ccc19x$significant_comorbidities___234336002 == 1)] <- 1
    ccc19x$der_hct[which(ccc19x$transplant_cellular_therapy %in% 1:5 &
                           ccc19x$transplant_cellular_timing != 0)] <- 1
    
    #No
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___234336002|significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == '' & is.na(ccc19x$der_hct)))
    {
      if(any(ccc19x[i,temp.ref]) & ccc19x$significant_comorbidities___234336002[i] == 0) ccc19x$der_hct[i] <- 0
    }
    
    #Unknown
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == '' & ccc19x$der_hct == 0))
    {
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$significant_comorbidities___unk[i] == 1) ccc19x$der_hct[i] <- 99
    }
    
    ccc19x$der_hct[which(ccc19x$transplant_cellular_therapy == 99 & ccc19x$der_hct == 0)] <- 99
    
    ccc19x$der_hct <- factor(ccc19x$der_hct)
    summary(ccc19x$der_hct[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca23a. Recent allo (within 1 year) or auto (within 100 days)
    ccc19x$der_hct_recent <- ccc19x$der_allo365
    ccc19x$der_hct_recent[which(ccc19x$der_auto100 == 1)] <- 1
    ccc19x$der_hct_recent[which(ccc19x$der_auto100 == 0 & is.na(ccc19x$der_hct_recent))] <- 0
    ccc19x$der_hct_recent[which(ccc19x$der_auto100 == 99 & is.na(ccc19x$der_hct_recent))] <- 99
    
    temp <- summary(ccc19x$der_hct_recent[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_hct_recent',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ################
    #Line of therapy
    ################
    
    #Ca02. Line of active therapy (includes non-systemic therapies)
    ccc19x$der_txline<- NA
    
    #Untreated in last 12 months (context variable only triggers for treatment within 12 months)
    ccc19x$der_txline[which(ccc19x$hx_treatment %in% c(3,88)|
                              (ccc19x$on_treatment == 0 & !ccc19x$hx_treatment %in% 1:2))] <- 'Untreated in last 12 months'
    
    #First-line
    ccc19x$der_txline[which(ccc19x$treatment_context %in% c(5250,2618,3175,813,1526,1901))] <- 'First line'
    
    #Second-line or later
    ccc19x$der_txline[which(ccc19x$treatment_context %in% c(14900,1874))] <- 'Second line or greater'
    
    #Curative NOS
    ccc19x$der_txline[which(ccc19x$treatment_context %in% c(46235))] <- 'Curative NOS'
    
    #Non-curative NOS
    ccc19x$der_txline[which(ccc19x$treatment_context %in% c(2648))] <- 'Non-curative NOS'
    
    #Other
    ccc19x$der_txline[which(ccc19x$treatment_context == 'OTH')] <- 'Other'
    
    #Unknown
    ccc19x$der_txline[which(ccc19x$treatment_context == 'UNK')] <- 'Unknown'
    
    #Factor
    ccc19x$der_txline <- as.factor(ccc19x$der_txline)
    ccc19x$der_txline <- relevel(ccc19x$der_txline, ref = 'Untreated in last 12 months')
    
    temp <- summary(ccc19x$der_txline[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_txline',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca02a. Line of systemic therapy for patients who received treatment within 12 months
    ccc19x$der_txline_systemic_12 <- NA
    
    #First-line
    ccc19x$der_txline_systemic_12[which(ccc19x$der_any_systemic_v2 == 1 & ccc19x$treatment_context %in% c(5250,2618,3175,813,1526,1901))] <- 'First line'
    
    #Second-line or later
    ccc19x$der_txline_systemic_12[which(ccc19x$der_any_systemic_v2 == 1 & ccc19x$treatment_context %in% c(14900,1874))] <- 'Second line or greater'
    
    #Curative NOS
    ccc19x$der_txline_systemic_12[which(ccc19x$der_any_systemic_v2 == 1 & ccc19x$treatment_context %in% c(46235))] <- 'Curative NOS'
    
    #Non-curative NOS
    ccc19x$der_txline_systemic_12[which(ccc19x$der_any_systemic_v2 == 1 & ccc19x$treatment_context %in% c(2648))] <- 'Non-curative NOS'
    
    #Other
    ccc19x$der_txline_systemic_12[which(ccc19x$der_any_systemic_v2 == 1 & ccc19x$treatment_context == 'OTH')] <- 'Other'
    
    #Unknown
    ccc19x$der_txline_systemic_12[which(ccc19x$der_any_systemic_v2 == 1 & ccc19x$treatment_context == 'UNK')] <- 'Unknown'
    
    #Factor
    ccc19x$der_txline_systemic_12 <- as.factor(ccc19x$der_txline_systemic_12)
    summary(ccc19x$der_txline_systemic_12[ccc19x$redcap_repeat_instrument == ''])
    
    ########################
    #ECOG Performance Status
    ########################
    
    #Ca3a. categorical ecog variable, lumping 1 = 0/1, 2 = 2, and 3 = 3/4, 4 = unknown
    ccc19x$der_ecogcat <- NA
    ccc19x$der_ecogcat[which(ccc19x$ecog_status %in% c(0,1))] <- '0 or 1'
    ccc19x$der_ecogcat[which(ccc19x$ecog_status == 2)] <- 2
    ccc19x$der_ecogcat[which(ccc19x$ecog_status %in% c(3,4))] <- '3 or 4'
    ccc19x$der_ecogcat[which(ccc19x$ecog_status %in% 88:99)] <- 'Unknown'
    
    #Factor
    ccc19x$der_ecogcat <- as.factor(ccc19x$der_ecogcat)
    
    temp <- summary(ccc19x$der_ecogcat[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ecogcat',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca3b. ECOG 0, 1, 2+
    ccc19x$der_ecogcat2 <- ccc19x$ecog_status
    ccc19x$der_ecogcat2[which(ccc19x$der_ecogcat2 %in% 2:4)] <- '2+'
    ccc19x$der_ecogcat2[which(ccc19x$der_ecogcat2 %in% 88:99)] <- 'Unknown'
    ccc19x$der_ecogcat2 <- factor(ccc19x$der_ecogcat2)
    
    temp <- summary(ccc19x$der_ecogcat2[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ecogcat2',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ##############
    #Cancer status
    ##############
    
    #Ca07a. cancer_status
    ##Recode to add "NA/unknowns, combining stable and responding into one category
    ccc19x$der_cancer_status <- ccc19x$cancer_status
    ccc19x$der_cancer_status[which(ccc19x$der_cancer_status == 1)] <- '0 - Remission/NED'
    ccc19x$der_cancer_status[which(ccc19x$der_cancer_status %in% c(2,3))] <- '1 - Active, stable/responding'
    ccc19x$der_cancer_status[which(ccc19x$der_cancer_status == 4)] <- '2 - Active, progressing'
    ccc19x$der_cancer_status[which(ccc19x$der_cancer_status %in% c(5,99))] <- '99 - Unknown'
    
    #Factor
    ccc19x$der_cancer_status <- as.factor(ccc19x$der_cancer_status)
    
    temp <- summary(ccc19x$der_cancer_status[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_cancer_status',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca07b. Cancer status without combining stable and responding
    ccc19x$der_cancer_status_v2 <- ccc19x$cancer_status
    ccc19x$der_cancer_status_v2[which(ccc19x$der_cancer_status_v2 == 1)] <- '0 - Remission/NED'
    ccc19x$der_cancer_status_v2[which(ccc19x$der_cancer_status_v2 == 2)] <- '1 - Active, responding'
    ccc19x$der_cancer_status_v2[which(ccc19x$der_cancer_status_v2 == 3)] <- '2 - Active, stable'
    ccc19x$der_cancer_status_v2[which(ccc19x$der_cancer_status_v2 == 4)] <- '3 - Active, progressing'
    ccc19x$der_cancer_status_v2[which(ccc19x$der_cancer_status_v2 %in% c(5,99))] <- '99 - Unknown'
    
    #Factor
    ccc19x$der_cancer_status_v2 <- as.factor(ccc19x$der_cancer_status_v2)
    
    temp <- summary(ccc19x$der_cancer_status_v2[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_cancer_status_v2',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca07c. Cancer status without combining active unknown and unknown
    ccc19x$der_cancer_status_v3 <- ccc19x$cancer_status
    ccc19x$der_cancer_status_v3[which(ccc19x$der_cancer_status_v3 == 1)] <- '0 - Remission/NED'
    ccc19x$der_cancer_status_v3[which(ccc19x$der_cancer_status_v3 %in% c(2,3))] <- '1- Active, stable/responding'
    ccc19x$der_cancer_status_v3[which(ccc19x$der_cancer_status_v3 == 4)] <- '2 - Active, progressing'
    ccc19x$der_cancer_status_v3[which(ccc19x$der_cancer_status_v3 == 5)] <- '2-99 - Active, unknown status'
    ccc19x$der_cancer_status_v3[which(ccc19x$der_cancer_status_v3 == 99)] <- '99 - Unknown'
    
    #Factor
    ccc19x$der_cancer_status_v3 <- as.factor(ccc19x$der_cancer_status_v3)
    
    temp <- summary(ccc19x$der_cancer_status_v3[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_cancer_status_v3',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca07d. Cancer status with remission conditioned on cancer timing
    ccc19x$der_cancer_status_v4 <- NA
    ccc19x$der_cancer_status_v4[which(ccc19x$cancer_status == 1 &
                                        ccc19x$cancer_timing %in% 1:2)] <- '1- Remission/NED, recent diagnosis'
    ccc19x$der_cancer_status_v4[which(ccc19x$cancer_status == 1 &
                                        ccc19x$cancer_timing == 3)] <- '0 - Remission/NED, remote diagnosis'
    ccc19x$der_cancer_status_v4[which(ccc19x$cancer_status == 2)] <- '2 - Active, responding'
    ccc19x$der_cancer_status_v4[which(ccc19x$cancer_status == 3)] <- '3 - Active, stable'
    ccc19x$der_cancer_status_v4[which(ccc19x$cancer_status == 4)] <- '4 - Active, progressing'
    
    #Unknown status
    ccc19x$der_cancer_status_v4[which(ccc19x$cancer_status %in% c(5,99))] <- '99 - Unknown'
    #Unknown timing
    ccc19x$der_cancer_status_v4[which(ccc19x$cancer_timing == 99 & is.na(ccc19x$der_cancer_status_v4))] <- '99 - Unknown'
    
    #Factor
    ccc19x$der_cancer_status_v4 <- as.factor(ccc19x$der_cancer_status_v4)
    
    temp <- summary(ccc19x$der_cancer_status_v4[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_cancer_status_v4',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca07e. Cancer status combining all active categories
    ccc19x$der_cancer_status_v5 <- ccc19x$cancer_status
    ccc19x$der_cancer_status_v5[which(ccc19x$der_cancer_status_v5 == 1)] <- '0 - Remission/NED'
    ccc19x$der_cancer_status_v5[which(ccc19x$der_cancer_status_v5 %in% c(2,3,4))] <- '1 - Active, stable/responding/progressing'
    ccc19x$der_cancer_status_v5[which(ccc19x$der_cancer_status_v5 %in% c(5,99))] <- '99 - Unknown'
    
    #Factor
    ccc19x$der_cancer_status_v5 <- as.factor(ccc19x$der_cancer_status_v5)
    
    temp <- summary(ccc19x$der_cancer_status_v5[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_cancer_status_v5',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ############################################################
    #Ca07f. Cancer progressing at the time of COVID-19 diagnosis
    ccc19x$der_cancer_prog_bl <- NA
    
    #No
    ccc19x$der_cancer_prog_bl[which(ccc19x$cancer_status %in% 1:3)] <- 0
    
    #Yes
    ccc19x$der_cancer_prog_bl[which(ccc19x$cancer_status == 4)] <- 1
    
    #Unknown status
    ccc19x$der_cancer_prog_bl[which(ccc19x$cancer_status %in% c(5,99))] <- 99
    
    #Factor
    ccc19x$der_cancer_prog_bl <- as.factor(ccc19x$der_cancer_prog_bl)
    
    temp <- summary(ccc19x$der_cancer_prog_bl[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_cancer_prog_bl',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #######################################################################################
    #Ca07g. Cancer progressing on any follow-up form, including attributable cause of death
    ccc19x$der_cancer_prog_fu <- NA
    
    temp <- unique(ccc19x$record_id[ccc19x$redcap_repeat_instrument == 'followup'])
    
    for(i in 1:length(temp))
    {
      temp.ref <- which(ccc19x$record_id == temp[i] & ccc19x$redcap_repeat_instrument == 'followup')
      temp.ref2 <- which(ccc19x$record_id == temp[i] & ccc19x$redcap_repeat_instrument == '')
      temp2 <- ccc19x$cancer_status_fu[temp.ref]
      temp2 <- temp2[!is.na(temp2)]
      if(length(temp2) > 0)
      {
        if(any(temp2 == 4)) 
        {
          ccc19x$der_cancer_prog_fu[temp.ref2] <- 1 
        } else if(all(temp2 %in% c(1,2,3))) 
        { 
          ccc19x$der_cancer_prog_fu[temp.ref2] <- 0 
        } else ccc19x$der_cancer_prog_fu[temp.ref2] <- 99
      }
    }
    
    #Check for cancer as attributable cause of death
    temp <- unique(ccc19x$record_id[which(ccc19x$der_deadbinary == 1 &
                                            ccc19x$redcap_repeat_instrument == 'followup')])
    for(i in 1:length(temp))
    {
      temp.ref <- which(ccc19x$record_id == temp[i] & ccc19x$redcap_repeat_instrument == '')
      if(ccc19x$der_cause_of_death[temp.ref] %in% c(1,3)) ccc19x$der_cancer_prog_fu[temp.ref] <- 1
    }
    
    #Factor
    ccc19x$der_cancer_prog_fu <- as.factor(ccc19x$der_cancer_prog_fu)
    
    temp <- summary(ccc19x$der_cancer_prog_fu[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_cancer_prog_fu',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ######################################################
    #Ca26. Cancer treatment modified on any follow-up form
    ccc19x$der_cancer_tx_mod <- NA
    
    temp <- unique(ccc19x$record_id[ccc19x$redcap_repeat_instrument == 'followup'])
    
    for(i in 1:length(temp))
    {
      temp.ref <- which(ccc19x$record_id == temp[i] & ccc19x$redcap_repeat_instrument == 'followup')
      temp.ref2 <- which(ccc19x$record_id == temp[i] & ccc19x$redcap_repeat_instrument == '')
      temp2 <- ccc19x$cancer_tx_fu[temp.ref]
      temp2 <- temp2[!is.na(temp2)]
      if(length(temp2) > 0)
      {
        if(any(temp2 == 1)) 
        {
          ccc19x$der_cancer_tx_mod[temp.ref2] <- 1 
        } else if(all(temp2 == 0)) 
        { 
          ccc19x$der_cancer_tx_mod[temp.ref2] <- 0 
        } else ccc19x$der_cancer_tx_mod[temp.ref2] <- 99
      }
    }
    
    #Factor
    ccc19x$der_cancer_tx_mod <- as.factor(ccc19x$der_cancer_tx_mod)
    
    temp <- summary(ccc19x$der_cancer_tx_mod[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_cancer_tx_mod',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca19. Metastatic status with stage assumption (only applicable to solid tumors/lymphoma)
    ccc19x$der_metastatic <- NA
    
    #Yes
    ccc19x$der_metastatic[which(ccc19x$stage %in% c(4, '764-7'))] <- 1
    ccc19x$der_metastatic[which(ccc19x$mets_yn == 1)] <- 1
  
    #No
    ccc19x$der_metastatic[which(ccc19x$mets_yn == 0)] <- 0
    ccc19x$der_metastatic[which(ccc19x$cancer_status == 1)] <- 0
    
    #Unknown
    ccc19x$der_metastatic[which(ccc19x$cancer_status == 99 & is.na(ccc19x$der_metastatic))] <- 99
    ccc19x$der_metastatic[which(ccc19x$stage == 99 & is.na(ccc19x$der_metastatic))] <- 99
    ccc19x$der_metastatic[which(ccc19x$mets_yn == 99 & is.na(ccc19x$der_metastatic))] <- 99
    
    ccc19x$der_metastatic <- as.factor(ccc19x$der_metastatic)
    
    temp <- summary(ccc19x$der_metastatic[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_metastatic',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca19x. Metastatic status without stage assumption
    ccc19x$der_metastatic_v2 <- NA
    
    #Yes
    ccc19x$der_metastatic_v2[which(ccc19x$mets_yn == 1)] <- 'Yes'
    
    #Yes, but sites missing
    temp.ref <- grep(colnames(ccc19x), pattern = 'mets_sites___')
    ccc19x$der_metastatic_v2[which(ccc19x$mets_yn == 1 &
                                     rowSums(ccc19x[,temp.ref]) == 0)] <- 'Yes, sites missing'
    
    #No
    ccc19x$der_metastatic_v2[which(ccc19x$mets_yn == 0)] <- 'No'
    ccc19x$der_metastatic_v2[which(ccc19x$cancer_status == 1 & is.na(ccc19x$der_metastatic_v2))] <- 'No'
    
    #Not applicable
    ccc19x$der_metastatic_v2[which(ccc19x$mets_yn == 88)] <- 'Not applicable'
    
    #Unknown
    ccc19x$der_metastatic_v2[which(ccc19x$cancer_status == 99 & is.na(ccc19x$der_metastatic_v2))] <- 'Unknown'
    ccc19x$der_metastatic_v2[which(ccc19x$mets_yn == 99 & is.na(ccc19x$der_metastatic_v2))] <- 'Unknown'
    
    ccc19x$der_metastatic_v2 <- as.factor(ccc19x$der_metastatic_v2)
    
    temp <- summary(ccc19x$der_metastatic_v2[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_metastatic_v2',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca19a. Metastatic disease to lung
    ccc19x$der_met_lung_v2 <- NA
    
    #Yes
    ccc19x$der_met_lung_v2[which(ccc19x$mets_sites___1116_1 == 1)] <- 1
    
    #No
    ccc19x$der_met_lung_v2[which(ccc19x$cancer_status == 1 & is.na(ccc19x$der_met_lung_v2))] <- 0
    ccc19x$der_met_lung_v2[which(ccc19x$mets_yn == 0 & is.na(ccc19x$der_met_lung_v2))] <- 0
    
    #Metastatic, something checked besides mets_sites___1116_1
    ccc19x$der_met_lung_v2[which(ccc19x$mets_yn == 1 & ccc19x$mets_sites___1116_1 == 0 &
                                   (ccc19x$mets_sites___1112_1 == 1|ccc19x$mets_sites___1113_1 == 1|
                                      ccc19x$mets_sites___1114_1 == 1|ccc19x$mets_sites___1115_1 == 1|
                                      ccc19x$mets_sites___1117_1 == 1|ccc19x$mets_sites___1117_2 == 1))] <- 0
    
    #Unknown
    ccc19x$der_met_lung_v2[which(ccc19x$mets_yn == 99 & is.na(ccc19x$der_met_lung_v2))] <- 99
    ccc19x$der_met_lung_v2[which(ccc19x$mets_sites___99 == 1 & is.na(ccc19x$der_met_lung_v2))] <- 99
    
    ccc19x$der_met_lung_v2 <- as.factor(ccc19x$der_met_lung_v2)
    
    temp <- summary(ccc19x$der_met_lung_v2[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_met_lung_v2',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca19b. Metastatic to lung combined variable (unknown --> missing)
    ccc19x <- within(ccc19x, {
      der_met_lung_comb <- ifelse(der_metastatic == 0 & der_met_lung_v2 == 0, 0,
                                  ifelse(der_met_lung_v2 == 1, 1, 
                                         ifelse(der_metastatic == 1 & der_met_lung_v2 == 0, 2, NA))) %>% as.factor()
    })
    
    temp <- summary(ccc19x$der_met_lung_comb[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_met_lung_comb',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca19c. Metastatic disease to bone
    ccc19x$der_met_bone <- NA
    
    #Yes
    ccc19x$der_met_bone[which(ccc19x$mets_sites___1112_1 == 1)] <- 1
    
    #No
    ccc19x$der_met_bone[which(ccc19x$cancer_status == 1 & is.na(ccc19x$der_met_bone))] <- 0
    ccc19x$der_met_bone[which(ccc19x$mets_yn == 0 & is.na(ccc19x$der_met_bone))] <- 0
    
    #Metastatic, something checked besides mets_sites___1112_1
    ccc19x$der_met_bone[which(ccc19x$mets_yn == 1 & ccc19x$mets_sites___1112_1 == 0 &
                                (ccc19x$mets_sites___1116_1 == 1|ccc19x$mets_sites___1113_1 == 1|
                                   ccc19x$mets_sites___1114_1 == 1|ccc19x$mets_sites___1115_1 == 1|
                                   ccc19x$mets_sites___1117_1 == 1|ccc19x$mets_sites___1117_2 == 1))] <- 0
    
    #Unknown
    ccc19x$der_met_bone[which(ccc19x$mets_yn == 99 & is.na(ccc19x$der_met_bone))] <- 99
    ccc19x$der_met_bone[which(ccc19x$mets_sites___99 == 1 & is.na(ccc19x$der_met_bone))] <- 99
    
    ccc19x$der_met_bone <- as.factor(ccc19x$der_met_bone)
    
    temp <- summary(ccc19x$der_met_bone[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_met_bone',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca19d. Metastatic disease to distant lymph nodes
    ccc19x$der_met_ln <- NA
    
    #Yes
    ccc19x$der_met_ln[which(ccc19x$mets_sites___1114_1 == 1)] <- 1
    
    #No
    ccc19x$der_met_ln[which(ccc19x$cancer_status == 1 & is.na(ccc19x$der_met_ln))] <- 0
    ccc19x$der_met_ln[which(ccc19x$mets_yn == 0 & is.na(ccc19x$der_met_ln))] <- 0
    
    #Metastatic, something checked besides mets_sites___1114_1
    ccc19x$der_met_ln[which(ccc19x$mets_yn == 1 & ccc19x$mets_sites___1114_1 == 0 &
                              (ccc19x$mets_sites___1116_1 == 1|ccc19x$mets_sites___1113_1 == 1|
                                 ccc19x$mets_sites___1112_1 == 1|ccc19x$mets_sites___1115_1 == 1|
                                 ccc19x$mets_sites___1117_1 == 1|ccc19x$mets_sites___1117_2 == 1))] <- 0
    
    #Unknown
    ccc19x$der_met_ln[which(ccc19x$mets_yn == 99 & is.na(ccc19x$der_met_ln))] <- 99
    ccc19x$der_met_ln[which(ccc19x$mets_sites___99 == 1 & is.na(ccc19x$der_met_ln))] <- 99
    
    ccc19x$der_met_ln <- as.factor(ccc19x$der_met_ln)
    
    temp <- summary(ccc19x$der_met_ln[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_met_ln',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca19e. Metastatic disease to liver
    ccc19x$der_met_liver <- NA
    
    #Yes
    ccc19x$der_met_liver[which(ccc19x$mets_sites___1115_1 == 1)] <- 1
    
    #No
    ccc19x$der_met_liver[which(ccc19x$cancer_status == 1 & is.na(ccc19x$der_met_liver))] <- 0
    ccc19x$der_met_liver[which(ccc19x$mets_yn == 0 & is.na(ccc19x$der_met_liver))] <- 0
    
    #Metastatic, something checked besides mets_sites___1115_1
    ccc19x$der_met_liver[which(ccc19x$mets_yn == 1 & ccc19x$mets_sites___1115_1 == 0 &
                                 (ccc19x$mets_sites___1116_1 == 1|ccc19x$mets_sites___1113_1 == 1|
                                    ccc19x$mets_sites___1112_1 == 1|ccc19x$mets_sites___1114_1 == 1|
                                    ccc19x$mets_sites___1117_1 == 1|ccc19x$mets_sites___1117_2 == 1))] <- 0
    
    #Unknown
    ccc19x$der_met_liver[which(ccc19x$mets_yn == 99 & is.na(ccc19x$der_met_liver))] <- 99
    ccc19x$der_met_liver[which(ccc19x$mets_sites___99 == 1 & is.na(ccc19x$der_met_liver))] <- 99
    
    ccc19x$der_met_liver <- as.factor(ccc19x$der_met_liver)
    
    temp <- summary(ccc19x$der_met_liver[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_met_liver',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca19f. Metastatic disease to brain
    ccc19x$der_met_brain <- NA
    
    #Yes
    ccc19x$der_met_brain[which(ccc19x$mets_sites___1113_1 == 1)] <- 1
    
    #No
    ccc19x$der_met_brain[which(ccc19x$cancer_status == 1 & is.na(ccc19x$der_met_brain))] <- 0
    ccc19x$der_met_brain[which(ccc19x$mets_yn == 0 & is.na(ccc19x$der_met_brain))] <- 0
    
    #Metastatic, something checked besides mets_sites___1113_1
    ccc19x$der_met_brain[which(ccc19x$mets_yn == 1 & ccc19x$mets_sites___1113_1 == 0 &
                                 (ccc19x$mets_sites___1116_1 == 1|ccc19x$mets_sites___1115_1 == 1|
                                    ccc19x$mets_sites___1112_1 == 1|ccc19x$mets_sites___1114_1 == 1|
                                    ccc19x$mets_sites___1117_1 == 1|ccc19x$mets_sites___1117_2 == 1))] <- 0
    
    #Unknown
    ccc19x$der_met_brain[which(ccc19x$mets_yn == 99 & is.na(ccc19x$der_met_brain))] <- 99
    ccc19x$der_met_brain[which(ccc19x$mets_sites___99 == 1 & is.na(ccc19x$der_met_brain))] <- 99
    
    ccc19x$der_met_brain <- as.factor(ccc19x$der_met_brain)
    
    temp <- summary(ccc19x$der_met_brain[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_met_brain',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca19g. Metastatic disease to visceral organs (lung or liver)
    ccc19x$der_met_visceral <- NA
    
    #Yes
    ccc19x$der_met_visceral[which(ccc19x$der_met_lung_v2 == 1|ccc19x$der_met_liver == 1)] <- 1
    
    #No
    ccc19x$der_met_visceral[which(ccc19x$der_met_lung_v2 == 0 & ccc19x$der_met_liver == 0)] <- 0
    
    #Unknown
    ccc19x$der_met_visceral[which((ccc19x$der_met_lung_v2 == 99|ccc19x$der_met_liver == 99) &
                                    is.na(ccc19x$der_met_visceral))] <- 99
    
    ccc19x$der_met_visceral <- as.factor(ccc19x$der_met_visceral)
    
    temp <- summary(ccc19x$der_met_visceral[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_met_visceral',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca20. Stage at cancer diagnosis, simplified
    ccc19x$der_stage <- NA
    
    #Localized
    ccc19x$der_stage[which(ccc19x$stage %in% c(1:3, '764-1'))] <- 'Localized'
    
    #Disseminated
    ccc19x$der_stage[which(ccc19x$stage %in% c(4, '764-7'))] <- 'Disseminated'
    
    #Unknown
    ccc19x$der_stage[which(ccc19x$stage == 99)] <- 'Unknown'
    
    ccc19x$der_stage <- as.factor(ccc19x$der_stage)
    
    temp <- summary(ccc19x$der_stage[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_stage',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca21. Active versus inactive cancer, including patients on treatment in definition of active
    ccc19x$der_active <- NA
    
    #Active
    ccc19x$der_active[which(ccc19x$on_treatment == 1|
                              ccc19x$hx_treatment %in% 1:2|
                              ccc19x$cancer_status %in% 2:5|
                              ccc19x$mets_yn == 1|
                              ccc19x$cancer_timing %in% c(0,1,88)
                              )] <- 'Active'
    
    #Inactive
    ccc19x$der_active[which(ccc19x$on_treatment == 0 &
                              ccc19x$hx_treatment %in% 3:88 &
                              ccc19x$cancer_status == 1 &
                              ccc19x$cancer_timing %in% 2:3 &
                              is.na(ccc19x$der_active))] <- 'Inactive'
    
    #Unknown
    ccc19x$der_active[which((ccc19x$on_treatment == 99|
                              ccc19x$hx_treatment == 99|
                              ccc19x$cancer_status == 99|
                              ccc19x$mets_yn == 99|
                              ccc19x$cancer_timing == 99) &
                              is.na(ccc19x$der_active))] <- 'Unknown'
    
    ccc19x$der_active <- as.factor(ccc19x$der_active)
    
    temp <- summary(ccc19x$der_active[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_active',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca21b. Active versus inactive cancer, only based on cancer status
    ccc19x$der_active_v2 <- NA
    
    #Active
    ccc19x$der_active_v2[which(ccc19x$cancer_status %in% 2:5|
                                 ccc19x$mets_yn == 1)] <- 'Active'
    
    #Inactive
    ccc19x$der_active_v2[which(ccc19x$cancer_status == 1 &
                                 is.na(ccc19x$der_active_v2))] <- 'Inactive'
    
    #Unknown
    ccc19x$der_active_v2[which(ccc19x$cancer_status == 99 &
                                 is.na(ccc19x$der_active_v2))] <- 'Unknown'
    
    ccc19x$der_active_v2 <- as.factor(ccc19x$der_active_v2)
    
    temp <- summary(ccc19x$der_active_v2[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_active_v2',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca4. Number of anti-cancer drugs
    
    #Load the curated file
    drugs <- read.csv(file = 'Mapping - medications/CCC19-ca-drugs-2023-01-18 with modalities.csv', header = T, stringsAsFactors = F)
    drugs <- drugs[order(drugs$record_id),]
    
    #Just keep the rows with drug information
    drugs <- drugs[drugs$drug1 != '',]
    
    for(i in 4:ncol(drugs)) drugs[,i] <- trimws(drugs[,i])
    
    ccc19x$der_no_drugs <- NA
    
    #Count the drugs
    drugs.ref <- which(colnames(drugs) == 'drug1'):which(colnames(drugs) == 'drug8')
    for(i in 1:nrow(drugs))
    {
      temp.ref <- which(ccc19x$record_id == drugs$record_id[i] & ccc19x$redcap_repeat_instrument == '')
      temp <- drugs[i,drugs.ref]
      temp <- temp[!is.na(temp)]
      temp <- temp[temp != '']
      ccc19x$der_no_drugs[temp.ref] <- length(temp)
    }
    
    #If a systemic treatment modality was checked but no drug names available --> missing
    ccc19x$der_no_drugs[which((ccc19x$treatment_modality___685 == 1 | 
                                 ccc19x$treatment_modality___694 == 1 |
                                 ccc19x$treatment_modality___58229 == 1 |
                                 ccc19x$treatment_modality___691 == 1) & ccc19x$der_no_drugs == 0
    )] <- NA
    
    ccc19x$der_no_drugs <- factor(ccc19x$der_no_drugs)
    summary(ccc19x$der_no_drugs[ccc19x$redcap_repeat_instrument == ''])
    
    #Add the drugs into the main data table
    drugs <- drugs[,c('record_id', 'drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')]
    ccc19x$drug1 <- NA
    ccc19x$drug2 <- NA
    ccc19x$drug3 <- NA
    ccc19x$drug4 <- NA
    ccc19x$drug5 <- NA
    ccc19x$drug6 <- NA
    ccc19x$drug7 <- NA
    ccc19x$drug8 <- NA
    
    #In case things are out of order in either table, load drugs one at a time
    for(i in 1:nrow(drugs))
    {
      temp.ref <- which(ccc19x$record_id %in% drugs$record_id[i] & ccc19x$redcap_repeat_instrument == '')
      if(length(temp.ref) == 1) ccc19x[temp.ref,c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] <- drugs[i,2:9]
    }
   
    #Result checking
    sum(!is.na(ccc19x$drug1))
    temp <- c(ccc19x$drug1[!is.na(ccc19x$drug1)],
              ccc19x$drug2[!is.na(ccc19x$drug2)],
              ccc19x$drug3[!is.na(ccc19x$drug3)],
              ccc19x$drug4[!is.na(ccc19x$drug4)],
              ccc19x$drug5[!is.na(ccc19x$drug5)],
              ccc19x$drug6[!is.na(ccc19x$drug6)],
              ccc19x$drug7[!is.na(ccc19x$drug7)],
              ccc19x$drug8[!is.na(ccc19x$drug8)])
    temp <- temp[temp != '']
    
    length(unique(temp))
    temp <- factor(temp)
     
    #Create drug list for data dictionary and QA needs
    temp <- c(ccc19x$drug1, ccc19x$drug2, ccc19x$drug3, ccc19x$drug4, ccc19x$drug5, ccc19x$drug6, ccc19x$drug7, ccc19x$drug8)
    temp <- temp[!is.na(temp)]
    temp <- temp[temp != '']
    temp <- unique(temp)
    temp <- temp[order(temp)]
    write.csv(temp, file = paste('Mapping - medications/', Sys.Date(), ' anticancer treatment list.csv',sep=''), row.names = F)
    
    #Ca24: Regimen match - this will only work if the HemOnc ontology is in the workspace
    ccc19x$der_regimen <- NA
    if(exists('concept_stage'))
    {
      for(i in which(!is.na(ccc19x$drug1)))
      {
        #Get list of drugs
        temp <- ccc19x[i,c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')]
        temp <- temp[temp != '']
        temp <- paste('[', paste(temp[order(temp)],collapse = ', '), ']', sep = '')
        
        #Look for match
        list.match <- which(concept_stage$concept_name == temp)
        
        #If there is a match, pull the potential regimens that contain these drugs
        if(length(list.match) > 0)
        {
          out <- concept_relationship_stage$concept_code_1[concept_relationship_stage$concept_code_2 %in% concept_stage$concept_code[list.match] &
                                                             concept_relationship_stage$relationship_id == 'Has anti-cancer drugs']
          if(length(out) > 1) out <- paste(out, collapse = '|')
          if(length(out) > 0) ccc19x$der_regimen[i] <- out else ccc19x$der_regimen[i] <- 'No match'
        } else ccc19x$der_regimen[i] <- 'No match'      
      }
    }
    
    #Ca14. 1st generation ARA (only fill for prostate cancer patients, for now)
    ccc19x$der_ARA_1st_gen <- NA
    
    temp.ref <- which(ccc19x$der_Prostate == 1)
    ccc19x$der_ARA_1st_gen[temp.ref] <- 0
    ccc19x$der_ARA_1st_gen[which(ccc19x$prostate_tx___83008 == 1|ccc19x$prostate_tx___4508 == 1|ccc19x$prostate_tx___31805 == 1)] <- 1
    ccc19x$der_ARA_1st_gen[which(ccc19x$drug1 %in% c('Bicalutamide', 'Flutamide', 'Nilutamide')|
                                   ccc19x$drug2 %in% c('Bicalutamide', 'Flutamide', 'Nilutamide')|
                                   ccc19x$drug3 %in% c('Bicalutamide', 'Flutamide', 'Nilutamide')|
                                   ccc19x$drug4 %in% c('Bicalutamide', 'Flutamide', 'Nilutamide')|
                                   ccc19x$drug5 %in% c('Bicalutamide', 'Flutamide', 'Nilutamide')|
                                   ccc19x$drug6 %in% c('Bicalutamide', 'Flutamide', 'Nilutamide')|
                                   ccc19x$drug7 %in% c('Bicalutamide', 'Flutamide', 'Nilutamide'))] <- 1
    
    #Unknown
    ccc19x$der_ARA_1st_gen[which(ccc19x$prostate_tx___unk == 1 & ccc19x$der_ARA_1st_gen == 0)] <- 99
    
    #Missing (everything is unchecked)
    temp.ref2 <- grep(colnames(ccc19x), pattern = 'prostate_tx___')
    temp <- rowSums(ccc19x[which(ccc19x$der_Prostate == 1),temp.ref2])
    ccc19x$der_ARA_1st_gen[temp.ref][which(temp == 0 & ccc19x$der_ARA_1st_gen[temp.ref] == 0)] <- NA
    
    ccc19x$der_ARA_1st_gen <- factor(ccc19x$der_ARA_1st_gen)
    summary(ccc19x$der_ARA_1st_gen[ccc19x$der_Prostate == 1])
    
    #Ca15. 2nd generation ARA (only fill for prostate cancer patients, for now)
    ccc19x$der_ARA_2nd_gen <- NA
    ccc19x$der_ARA_2nd_gen[which(ccc19x$der_Prostate == 1)] <- 0
    ccc19x$der_ARA_2nd_gen[which(ccc19x$prostate_tx___1307298 == 1|ccc19x$prostate_tx___1999574 ==1|ccc19x$prostate_tx___2180325 == 1)] <- 1
    ccc19x$der_ARA_2nd_gen[which(ccc19x$drug1 %in% c('Apalutamide', 'Enzalutamide', 'Darolutamide')|
                                   ccc19x$drug2 %in% c('Apalutamide', 'Enzalutamide', 'Darolutamide')|
                                   ccc19x$drug3 %in% c('Apalutamide', 'Enzalutamide', 'Darolutamide')|
                                   ccc19x$drug4 %in% c('Apalutamide', 'Enzalutamide', 'Darolutamide')|
                                   ccc19x$drug5 %in% c('Apalutamide', 'Enzalutamide', 'Darolutamide')|
                                   ccc19x$drug6 %in% c('Apalutamide', 'Enzalutamide', 'Darolutamide')|
                                   ccc19x$drug7 %in% c('Apalutamide', 'Enzalutamide', 'Darolutamide'))] <- 1
    
    #Unknown
    ccc19x$der_ARA_2nd_gen[which(ccc19x$prostate_tx___unk == 1 & ccc19x$der_ARA_2nd_gen == 0)] <- 99
    
    #Missing (everything is unchecked)
    temp.ref2 <- grep(colnames(ccc19x), pattern = 'prostate_tx___')
    temp <- rowSums(ccc19x[which(ccc19x$der_Prostate == 1),temp.ref2])
    ccc19x$der_ARA_2nd_gen[temp.ref][which(temp == 0 & ccc19x$der_ARA_2nd_gen[temp.ref] == 0)] <- NA
    
    ccc19x$der_ARA_2nd_gen <- factor(ccc19x$der_ARA_2nd_gen)
    summary(ccc19x$der_ARA_2nd_gen[ccc19x$der_Prostate == 1])
    
    #Ca16. Abiraterone (only fill for prostate cancer patients, for now)
    ccc19x$der_abi <- NA
    ccc19x$der_abi[which(ccc19x$der_Prostate == 1)] <- 0
    ccc19x$der_abi[which(ccc19x$prostate_tx___1100072 == 1)] <- 1
    ccc19x$der_abi[which(ccc19x$drug1 %in% c('Abiraterone')|
                           ccc19x$drug2 %in% c('Abiraterone')|
                           ccc19x$drug3 %in% c('Abiraterone')|
                           ccc19x$drug4 %in% c('Abiraterone')|
                           ccc19x$drug5 %in% c('Abiraterone')|
                           ccc19x$drug6 %in% c('Abiraterone')|
                           ccc19x$drug7 %in% c('Abiraterone'))] <- 1
    
    #Unknown
    ccc19x$der_abi[which(ccc19x$prostate_tx___unk == 1 & ccc19x$der_abi == 0)] <- 99
    
    #Missing (everything is unchecked)
    temp.ref2 <- grep(colnames(ccc19x), pattern = 'prostate_tx___')
    temp <- rowSums(ccc19x[which(ccc19x$der_Prostate == 1),temp.ref2])
    ccc19x$der_abi[temp.ref][which(temp == 0 & ccc19x$der_abi[temp.ref] == 0)] <- NA
    
    ccc19x$der_abi <- factor(ccc19x$der_abi)
    summary(ccc19x$der_abi[ccc19x$der_Prostate == 1])
    
    #Ca17. Chemotherapy for prostate cancer (only fill for prostate cancer patients, for now)
    ccc19x$der_chemo_prca <- NA
    ccc19x$der_chemo_prca[which(ccc19x$der_Prostate == 1)] <- 0
    ccc19x$der_chemo_prca[which(ccc19x$prostate_tx___996051 == 1|ccc19x$prostate_tx___40048 == 1|ccc19x$prostate_tx___72962 == 1)] <- 1
    ccc19x$der_chemo_prca[which(ccc19x$drug1 %in% c('Cabazitaxel', 'Carboplatin', 'Docetaxel')|
                                  ccc19x$drug2 %in% c('Cabazitaxel', 'Carboplatin', 'Docetaxel')|
                                  ccc19x$drug3 %in% c('Cabazitaxel', 'Carboplatin', 'Docetaxel')|
                                  ccc19x$drug4 %in% c('Cabazitaxel', 'Carboplatin', 'Docetaxel')|
                                  ccc19x$drug5 %in% c('Cabazitaxel', 'Carboplatin', 'Docetaxel')|
                                  ccc19x$drug6 %in% c('Cabazitaxel', 'Carboplatin', 'Docetaxel')|
                                  ccc19x$drug7 %in% c('Cabazitaxel', 'Carboplatin', 'Docetaxel'))] <- 1
    
    #Unknown
    ccc19x$der_chemo_prca[which(ccc19x$prostate_tx___unk == 1 & ccc19x$der_chemo_prca == 0)] <- 99
    
    #Missing (everything is unchecked)
    temp.ref2 <- grep(colnames(ccc19x), pattern = 'prostate_tx___')
    temp <- rowSums(ccc19x[which(ccc19x$der_Prostate == 1),temp.ref2])
    ccc19x$der_chemo_prca[temp.ref][which(temp == 0 & ccc19x$der_chemo_prca[temp.ref] == 0)] <- NA
    
    ccc19x$der_chemo_prca <- factor(ccc19x$der_chemo_prca)
    summary(ccc19x$der_chemo_prca[ccc19x$der_Prostate == 1])
    
    #Ca18. ADT (only fill for prostate cancer patients, for now)
    ccc19x$der_adt <- NA
    
    #Yes
    ccc19x$der_adt[which(ccc19x$adt == 1)] <- 1
    ccc19x$der_adt[which(ccc19x$orchiectomy == 1)] <- 1
    ccc19x$der_adt[which(is.na(ccc19x$adt) & ccc19x$treatment_modality___691 == 1 & ccc19x$der_Prostate == 1 & is.na(ccc19x$der_adt))] <- 1
    
    ccc19x$der_adt[which((ccc19x$drug1 %in% c('ADT', 'Leuprolide', 'Degarelix', 'Goserelin', 'Histrelin', 'Triptorelin')|
                            ccc19x$drug2 %in% c('ADT', 'Leuprolide', 'Degarelix', 'Goserelin', 'Histrelin', 'Triptorelin')|
                            ccc19x$drug3 %in% c('ADT', 'Leuprolide', 'Degarelix', 'Goserelin', 'Histrelin', 'Triptorelin')|
                            ccc19x$drug4 %in% c('ADT', 'Leuprolide', 'Degarelix', 'Goserelin', 'Histrelin', 'Triptorelin')|
                            ccc19x$drug5 %in% c('ADT', 'Leuprolide', 'Degarelix', 'Goserelin', 'Histrelin', 'Triptorelin')|
                            ccc19x$drug6 %in% c('ADT', 'Leuprolide', 'Degarelix', 'Goserelin', 'Histrelin', 'Triptorelin')|
                            ccc19x$drug7 %in% c('ADT', 'Leuprolide', 'Degarelix', 'Goserelin', 'Histrelin', 'Triptorelin'))
                         & ccc19x$der_Prostate == 1 & is.na(ccc19x$der_adt))] <- 2
    
    #No
    ccc19x$der_adt[which(ccc19x$adt == 0 & ccc19x$der_Prostate == 1 & is.na(ccc19x$der_adt))] <- 0
    ccc19x$der_adt[which(ccc19x$treatment_modality___691 == 0 &
                           (ccc19x$treatment_modality___685 == 1|
                              ccc19x$treatment_modality___694 == 1|
                              ccc19x$treatment_modality___58229 == 1|
                              ccc19x$treatment_modality___695 == 1|
                              ccc19x$treatment_modality___14051 == 1|
                              ccc19x$treatment_modality___45186 == 1|
                              ccc19x$treatment_modality___45215 == 1|
                              ccc19x$treatment_modality___oth == 1) &
                           ccc19x$der_Prostate == 1 & is.na(ccc19x$der_adt))] <- 0
    ccc19x$der_adt[which(ccc19x$on_treatment == 0 & ccc19x$der_Prostate == 1 & is.na(ccc19x$der_adt))] <- 0
    
    #Unknown
    ccc19x$der_adt[which((ccc19x$prostate_tx___unk == 1|ccc19x$orchiectomy == 99|ccc19x$adt == 99) & 
                     is.na(ccc19x$der_adt))] <- 99
    
    ccc19x$der_adt <- factor(ccc19x$der_adt)
    summary(ccc19x$der_adt[ccc19x$der_Prostate == 1])
    
    #Ca4a: CD20 drugs (ever)
    ccc19x$der_cd20 <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Rituximab', 'Obinutuzumab', 'Ofatumumab', 'Ublituximab', 'Veltuzumab'))) ccc19x$der_cd20[temp.ref[i]] <- 1 else
               ccc19x$der_cd20[temp.ref[i]] <- 0
    }
    
    ccc19x$der_cd20 <- factor(ccc19x$der_cd20)
    summary(ccc19x$der_cd20[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca4a1: CD20 drugs within 12 months
    ccc19x$der_cd20_12mo <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '' &
                        (ccc19x$recent_treatment %in% 1:3|ccc19x$hx_treatment %in% 1:2))
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Rituximab', 'Obinutuzumab', 'Ofatumumab', 'Ublituximab', 'Veltuzumab'))) ccc19x$der_cd20_12mo[temp.ref[i]] <- 1 
    }
    
    #Another drug mentioned, within 12 months
    ccc19x$der_cd20_12mo[temp.ref[which(!is.na(ccc19x$drug1[temp.ref]) &
                                is.na(ccc19x$der_cd20_12mo[temp.ref]))]] <- 0
    
    #Treatment more than 12 months ago, or after COVID-19
    ccc19x$der_cd20_12mo[which(ccc19x$recent_treatment == 98|ccc19x$hx_treatment %in% 3:88)] <- 0
    
    #Unknown
    ccc19x$der_cd20_12mo[which(ccc19x$recent_treatment == 99|ccc19x$hx_treatment == 99|ccc19x$on_treatment == 99)] <- 99
    
    ccc19x$der_cd20_12mo <- factor(ccc19x$der_cd20_12mo)
    summary(ccc19x$der_cd20_12mo[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca4a2: CD20 drugs within 3 months
    ccc19x$der_cd20_3mo <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '' &
                        (ccc19x$recent_treatment %in% 1:3|ccc19x$hx_treatment == 1))
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Rituximab', 'Obinutuzumab', 'Ofatumumab', 'Ublituximab', 'Veltuzumab'))) ccc19x$der_cd20_3mo[temp.ref[i]] <- 1 
    }
    
    #Another drug mentioned, within 3 months
    ccc19x$der_cd20_3mo[temp.ref[which(!is.na(ccc19x$drug1[temp.ref]) & is.na(ccc19x$der_cd20_3mo[temp.ref]))]] <- 0
    
    #Treatment more than 3 months ago, or after COVID-19
    ccc19x$der_cd20_3mo[which(ccc19x$recent_treatment %in% c(88,98)|ccc19x$hx_treatment %in% 2:88)] <- 0
    
    #Unknown
    ccc19x$der_cd20_3mo[which((ccc19x$recent_treatment == 99|ccc19x$hx_treatment == 99|ccc19x$on_treatment == 99) &
                                is.na(ccc19x$der_cd20_3mo))] <- 99
    
    ccc19x$der_cd20_3mo <- factor(ccc19x$der_cd20_3mo)
    summary(ccc19x$der_cd20_3mo[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca4a2: CD20 + chemo within 12 months
    ccc19x$der_cd20_cyto_12mo <- NA
    ccc19x$der_cd20_cyto_12mo[which(ccc19x$der_cd20_12mo == 1 & ccc19x$der_any_cyto_12mo == 1)] <- 1
    
    #Extra level if they were also receiving a steroid (e.g., R-CHOP)
    for(i in which(ccc19x$der_cd20_cyto_12mo == 1))
      if(any(ccc19x[i,c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Prednisone', 'Prednisolone', 'Methylprednisolone', 'Dexamethasone', 'Hydrocortisone'))) ccc19x$der_cd20_cyto_12mo[i] <- 2
    
    ccc19x$der_cd20_cyto_12mo[which(ccc19x$der_cd20_12mo == 0|ccc19x$der_any_cyto_12mo == 0)] <- 0
    ccc19x$der_cd20_cyto_12mo[which(ccc19x$der_cd20_12mo == 99|ccc19x$der_any_cyto_12mo == 99)] <- 99
    
    ccc19x$der_cd20_cyto_12mo <- factor(ccc19x$der_cd20_cyto_12mo)
    summary(ccc19x$der_cd20_cyto_12mo[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca4m: CD38 drugs
    ccc19x$der_cd38 <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Daratumumab', 'Isatuximab'))) ccc19x$der_cd38[temp.ref[i]] <- 1 else
               ccc19x$der_cd38[temp.ref[i]] <- 0
    }
    
    ccc19x$der_cd38 <- factor(ccc19x$der_cd38)
    summary(ccc19x$der_cd38[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca4b: BTKi (ever)
    ccc19x$der_btki <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Ibrutinib', 'Acalabrutinib', 'LOXO-305'))) ccc19x$der_btki[temp.ref[i]] <- 1 else
               ccc19x$der_btki[temp.ref[i]] <- 0
    }
    
    #Overwrite if structured data are present
    ccc19x$der_btki[which(ccc19x$what_targeted_tx___l01xe51 == 1|ccc19x$what_targeted_tx___l01xe27 == 1)] <- 1
    
    ccc19x$der_btki <- factor(ccc19x$der_btki)
    
    temp <- summary(ccc19x$der_btki[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_btki',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca4t: JAKi as cancer treatment (does not include JAKi for immunosuppression)
    ccc19x$der_jaki <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Fedratinib', 'Ruxolitinib', 'Lestaurtinib', 'Momelotinib', 'Pacritinib', 'Tofacitinib'))) ccc19x$der_jaki[temp.ref[i]] <- 1 else
               ccc19x$der_jaki[temp.ref[i]] <- 0
    }
    
    #Overwrite if structured data are present
    ccc19x$der_jaki[which(ccc19x$what_targeted_tx___l01xe57 == 1|ccc19x$what_targeted_tx___l01xe18 == 1)] <- 1
    
    ccc19x$der_jaki <- factor(ccc19x$der_jaki)
    
    temp <- summary(ccc19x$der_jaki[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_jaki',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca4u: BCR-ABL inhibitor as cancer treatment
    ccc19x$der_bcrabli <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Asciminib', 'Bosutinib', 'Dasatinib', 'Flumatinib',
               'Imatinib', 'Nilotinib', 'Ponatinib', 'Radotinib'))) ccc19x$der_bcrabli[temp.ref[i]] <- 1 else
                 ccc19x$der_bcrabli[temp.ref[i]] <- 0
    }
    
    #Overwrite if structured data are present
    ccc19x$der_bcrabli[which(ccc19x$what_targeted_tx___l01xe06 == 1|
                               ccc19x$what_targeted_tx___l01xe01 == 1|
                               ccc19x$what_targeted_tx___l01xe08 == 1)] <- 1
    
    ccc19x$der_bcrabli <- factor(ccc19x$der_bcrabli)
    
    temp <- summary(ccc19x$der_bcrabli[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_bcrabli',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca4c: Venetoclax
    ccc19x$der_venet <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Venetoclax'))) ccc19x$der_venet[temp.ref[i]] <- 1 else
               ccc19x$der_venet[temp.ref[i]] <- 0
    }
    
    ccc19x$der_venet <- factor(ccc19x$der_venet)
    summary(ccc19x$der_venet[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca4d: Tamoxifen
    ccc19x$der_tam <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Tamoxifen'))) ccc19x$der_tam[temp.ref[i]] <- 1 else
               ccc19x$der_tam[temp.ref[i]] <- 0
    }
    
    ccc19x$der_tam <- factor(ccc19x$der_tam)
    summary(ccc19x$der_tam[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca4e: Aromatase inhibitor
    ccc19x$der_ai <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Letrozole', 'Exemestane', 'Anastrozole', 'Aromatase inhibitor'))) ccc19x$der_ai[temp.ref[i]] <- 1 else
               ccc19x$der_ai[temp.ref[i]] <- 0
    }
    
    ccc19x$der_ai <- factor(ccc19x$der_ai)
    summary(ccc19x$der_ai[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca04f: VEGF inhibitor
    ccc19x$der_vegfi <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Axitinib', 'Bevacizumab', 'Cabozantinib', 'Lenvatinib', 'Pazopanib',
               'Sorafenib', 'Sunitinib', 'Vandetanib', 'Ramucirumab'))) ccc19x$der_vegfi[temp.ref[i]] <- 1 else
               ccc19x$der_vegfi[temp.ref[i]] <- 0
    }
    
    ccc19x$der_vegfi <- factor(ccc19x$der_vegfi)
    summary(ccc19x$der_vegfi[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca04f1b: VEGF inhibitor (expanded list)
    ccc19x$der_vegfi_v2 <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Apatinib','Axitinib', 'Bevacizumab', 'Cabozantinib',
               'Cediranib','Dovitinib','Fruquintinib',
               'Lenvatinib','Motesanib','Nintedanib','Pazopanib','Ponatinib','Regorafenib','Ripretinib',
               'Sorafenib', 'Sunitinib','Tivozanib','Vandetanib','Vorolanib', 
               'Ramucirumab','Ziv-aflibercept'))) ccc19x$der_vegfi_v2[temp.ref[i]] <- 1 else
                 ccc19x$der_vegfi_v2[temp.ref[i]] <- 0
    }
    
    ccc19x$der_vegfi_v2 <- factor(ccc19x$der_vegfi_v2)
    summary(ccc19x$der_vegfi_v2[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca04g: TKI inhibitor (incomplete list, possibly)
    ccc19x$der_tki <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Afatinib', 'Alectinib', 'Axitinib', 'Brigatinib', 'Cabozantinib', 'Ceritinib',
               'Crizotinib', 'Entrectinib', 'Erlotinib', 'Gefitinib', 'Gilteritinib', 'Lapatinib',
               'Lenvatinib', 'Lorlatinib', 'Neratinib', 'Nilotinib', 'Niraparib', 'Osimertinib',
               'Pazopanib', 'Ponatinib', 'Regorafenib', 'Ripretinib', 'Selpercatinib', 'Sorafenib',
               'Sunitinib', 'Acalabrutinib', 'Bosutinib', 'Dasatinib', 'Fedratinib', 'Ibrutinib',
               'Imatinib', 'Midostaurin', 'Pexidartinib', 'Pralsetinib', 'Ruxolitinib', 'Tucatinib',
               'Vandetinib'))) ccc19x$der_tki[temp.ref[i]] <- 1 else
                 ccc19x$der_tki[temp.ref[i]] <- 0
    }
    
    #Overwrite if structured data are present
    ccc19x$der_tki[which(ccc19x$what_targeted_tx___l01xe51 == 1|
                            ccc19x$what_targeted_tx___l01xe06 == 1|
                            ccc19x$what_targeted_tx___l01xe57 == 1|
                            ccc19x$what_targeted_tx___l01xe27 == 1|
                            ccc19x$what_targeted_tx___l01xe01 == 1|
                            ccc19x$what_targeted_tx___l01xe08 == 1|
                            ccc19x$what_targeted_tx___l01xe18 == 1)] <- 1
    
    ccc19x$der_tki <- factor(ccc19x$der_tki)
    summary(ccc19x$der_tki[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca04g2: TKI inhibitors used in lung cancer
    ccc19x$der_tki_lung <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Afatinib', 'Alectinib', 'Brigatinib', 'Ceritinib', 'Crizotinib', 'Entrectinib', 'Erlotinib', 'Gefitinib', 'Lorlatinib', 'Osimertinib', 'Selpercatinib'))) ccc19x$der_tki_lung[temp.ref[i]] <- 1 else
                 ccc19x$der_tki_lung[temp.ref[i]] <- 0
    }
    
    ccc19x$der_tki_lung <- factor(ccc19x$der_tki_lung)
    summary(ccc19x$der_tki_lung[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca04h: CDK4/6 inhibitor
    ccc19x$der_cdk46i <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Abemaciclib', 'Palbociclib', 'Ribociclib'))) ccc19x$der_cdk46i[temp.ref[i]] <- 1 else
               ccc19x$der_cdk46i[temp.ref[i]] <- 0
    }
    
    ccc19x$der_cdk46i <- factor(ccc19x$der_cdk46i)
    
    temp <- summary(ccc19x$der_cdk46i[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_cdk46i',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca04i: GNRH agonists and antagonists
    ccc19x$der_gnrh <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('ADT', 'Goserelin', 'Histrelin', 'Leuprolide', 'Triptorelin',
               'Abarelix', 'Degarelix', 'Relugolix'))) ccc19x$der_gnrh[temp.ref[i]] <- 1 else
                 ccc19x$der_gnrh[temp.ref[i]] <- 0
    }
    
    #Overwrite if structured data are present
    ccc19x$der_gnrh[which(ccc19x$adt == 1)] <- 1
    
    ccc19x$der_gnrh <- factor(ccc19x$der_gnrh)
    summary(ccc19x$der_gnrh[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca04j: Oral anti-androgen
    ccc19x$der_oral_antiandrogen <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Abiraterone', 'Apalutamide', 'Darolutamide', 'Enzalutmide',
               'Flutamide', 'Nilutamide', 'Bicalutamide'))) ccc19x$der_oral_antiandrogen[temp.ref[i]] <- 1 else
                 ccc19x$der_oral_antiandrogen[temp.ref[i]] <- 0
    }
    
    #Overwrite if structured data are present
    ccc19x$der_oral_antiandrogen[which(ccc19x$prostate_tx___1100072 == 1|
                                         ccc19x$prostate_tx___1307298 == 1|
                                         ccc19x$prostate_tx___1999574 == 1|
                                         ccc19x$prostate_tx___2180325 == 1|
                                         ccc19x$prostate_tx___31805 == 1|
                                         ccc19x$prostate_tx___4508 == 1|
                                         ccc19x$prostate_tx___83008 == 1)] <- 1
    
    ccc19x$der_oral_antiandrogen <- factor(ccc19x$der_oral_antiandrogen)
    summary(ccc19x$der_oral_antiandrogen[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca04k: PD-1/PD-L1 antibodies
    ccc19x$der_pd1_l1 <- NA
    
    #Named exposure in the drug table
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Nivolumab', 'Pembrolizumab', 'Atezolizumab', 'Avelumab',
               'Durvalumab', 'Cemiplimab', 'Anti-PD-1 antibody', 'Anti-PD-L1 antibody',
               'LAG/PD-1'))) ccc19x$der_pd1_l1[temp.ref[i]] <- 1 else
                 ccc19x$der_pd1_l1[temp.ref[i]] <- 0
    }
    
    #Structured data are present
    ccc19x$der_pd1_l1[which(ccc19x$what_immunotherapy %in% c('45446', '45170', '45838-45446'))] <- 1
    
    ccc19x$der_pd1_l1 <- factor(ccc19x$der_pd1_l1)
    
    temp <- summary(ccc19x$der_pd1_l1[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_pd1_l1',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca04k1: PD-1 or PD-L1 inhibitor within 3 months
    ccc19x$der_pd1_l1_3mo <- NA
    
    ccc19x$der_pd1_l1_3mo[which(ccc19x$der_pd1_l1 == 1 & ccc19x$der_any_systemic_3mo == 1)] <- 1
    ccc19x$der_pd1_l1_3mo[which(ccc19x$der_pd1_l1 == 0|ccc19x$der_any_systemic_3mo == 0)] <- 0
    ccc19x$der_pd1_l1_3mo[which(ccc19x$der_pd1_l1 == 1 & ccc19x$der_any_systemic_3mo == 99)] <- 99
    
    ccc19x$der_pd1_l1_3mo <- factor(ccc19x$der_pd1_l1_3mo)
    
    temp <- summary(ccc19x$der_pd1_l1_3mo[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_pd1_l1_3mo',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca04k2: PD-1 or PD-L1 inhibitor within 12 months
    ccc19x$der_pd1_l1_12mo <- NA
    
    ccc19x$der_pd1_l1_12mo[which(ccc19x$der_pd1_l1 == 1 & ccc19x$der_any_systemic_v2 == 1)] <- 1
    ccc19x$der_pd1_l1_12mo[which(ccc19x$der_pd1_l1 == 0|ccc19x$der_any_systemic_v2 == 0)] <- 0
    ccc19x$der_pd1_l1_12mo[which(ccc19x$der_pd1_l1 == 1 & ccc19x$der_any_systemic_v2 == 99)] <- 99
    
    ccc19x$der_pd1_l1_12mo <- factor(ccc19x$der_pd1_l1_12mo)
    
    temp <- summary(ccc19x$der_pd1_l1_12mo[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_pd1_l1_12mo',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca04s: CTLA4 antibodies
    ccc19x$der_ctla4 <- NA
    
    #Named exposure in the drug table
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Ipilimumab', 'Tremilimumab', 'Anti-CTLA4 antibody'))) ccc19x$der_ctla4[temp.ref[i]] <- 1 else
               ccc19x$der_ctla4[temp.ref[i]] <- 0
    }
    
    #Structured data are present
    ccc19x$der_ctla4[which(ccc19x$what_immunotherapy %in% c('45838', '45838-45446'))] <- 1
    
    ccc19x$der_ctla4 <- factor(ccc19x$der_ctla4)
    summary(ccc19x$der_ctla4[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca4l: Anti-HER2
    ccc19x$der_her2 <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Lapatinib', 'Neratinib', 'Tucatinib', 'Trastuzumab', 'Margetuximab',
               'Trastuzumab emtansine', 'Trastuzumab deruxtecan', 'Pertuzumab'))) ccc19x$der_her2[temp.ref[i]] <- 1 else
                 ccc19x$der_her2[temp.ref[i]] <- 0
    }
    
    ccc19x$der_her2 <- factor(ccc19x$der_her2)
    
    temp <- summary(ccc19x$der_her2[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_her2',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #CaTxChemo01: Anthracycline
    ccc19x$der_anthracycline <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Doxorubicin', 'Epirubicin', 'Daunorubicin', 'Mitoxantrone',
               'Aclarubicin', 'Amrubicin', 'Vyxeos', 'Daunorubicin liposomal',
               'Idarubicin', 'Pegylated liposomal doxorubicin', 'Valrubicin'))) ccc19x$der_anthracycline[temp.ref[i]] <- 1 else
                 ccc19x$der_anthracycline[temp.ref[i]] <- 0
    }
    
    ccc19x$der_anthracycline <- factor(ccc19x$der_anthracycline)
    
    temp <- summary(ccc19x$der_anthracycline[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_anthracycline',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #CaTxChemo02: Taxane
    ccc19x$der_taxane <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Docetaxel', 'Paclitaxel', 'Cabazitaxel', 'nab-Paclitaxel'))) ccc19x$der_taxane[temp.ref[i]] <- 1 else
               ccc19x$der_taxane[temp.ref[i]] <- 0
    }
    
    ccc19x$der_taxane <- factor(ccc19x$der_taxane)
    
    temp <- summary(ccc19x$der_taxane[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_taxane',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #CaTxChemo03: Gemcitabine
    ccc19x$der_gemcitabine <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Gemcitabine'))) ccc19x$der_gemcitabine[temp.ref[i]] <- 1 else
               ccc19x$der_gemcitabine[temp.ref[i]] <- 0
    }
    
    ccc19x$der_gemcitabine <- factor(ccc19x$der_gemcitabine)
    
    temp <- summary(ccc19x$der_gemcitabine[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_gemcitabine',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #CaTxChemo04: Irinotecan
    ccc19x$der_irinotecan <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Irinotecan'))) ccc19x$der_irinotecan[temp.ref[i]] <- 1 else
               ccc19x$der_irinotecan[temp.ref[i]] <- 0
    }
    
    ccc19x$der_irinotecan <- factor(ccc19x$der_irinotecan)
    
    temp <- summary(ccc19x$der_irinotecan[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_irinotecan',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #CaTxChemo05: Platinum agent
    ccc19x$der_platinum <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Cisplatin', 'Carboplatin', 'Oxaliplatin', 'Heptaplatin', 'Nedaplatin'))) ccc19x$der_platinum[temp.ref[i]] <- 1 else
               ccc19x$der_platinum[temp.ref[i]] <- 0
    }
    
    ccc19x$der_platinum <- factor(ccc19x$der_platinum)
    
    temp <- summary(ccc19x$der_platinum[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_platinum',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #CaTxChemo06: Fluoropyrimidine
    ccc19x$der_fluoropyrimidine <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Capecitabine', 'Doxifluridine', 'Fluorouracil', 'Floxuridine', 'Tegafur and uracil', 'Trifluridine and tipiracil'))) ccc19x$der_fluoropyrimidine[temp.ref[i]] <- 1 else
               ccc19x$der_fluoropyrimidine[temp.ref[i]] <- 0
    }
    
    ccc19x$der_fluoropyrimidine <- factor(ccc19x$der_fluoropyrimidine)
    
    temp <- summary(ccc19x$der_fluoropyrimidine[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_fluoropyrimidine',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #CaTxTargeted01: EGFR inhibitor (small molecule inhibitors)
    ccc19x$der_egfri <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Afatinib', 'Dacomitinib', 'Erlotinib', 'Gefitinib', 'Icotinib', 'Mobocertinib', 'Osimertinib'))) ccc19x$der_egfri[temp.ref[i]] <- 1 else
               ccc19x$der_egfri[temp.ref[i]] <- 0
    }
    
    ccc19x$der_egfri <- factor(ccc19x$der_egfri)
    
    temp <- summary(ccc19x$der_egfri[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_egfri',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #CaTxTargeted02: anti-EGFR antibodies
    ccc19x$der_egfr_mab <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Amivantamab', 'Cetuximab', 'Necitumumab', 'Panitumumab'))) ccc19x$der_egfr_mab[temp.ref[i]] <- 1 else
               ccc19x$der_egfr_mab[temp.ref[i]] <- 0
    }
    
    ccc19x$der_egfr_mab <- factor(ccc19x$der_egfr_mab)
    
    temp <- summary(ccc19x$der_egfr_mab[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_egfr_mab',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca4o: PARP inhibitor
    ccc19x$der_parpi <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Olaparib', 'Niraparib', 'Rucaparib', 'Talazoparib', 'Veliparib'))) ccc19x$der_parpi[temp.ref[i]] <- 1 else
                 ccc19x$der_parpi[temp.ref[i]] <- 0
    }
    
    ccc19x$der_parpi <- factor(ccc19x$der_parpi)
    summary(ccc19x$der_parpi[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca4p: Targeted not HER2 or CDK4/6i (focusing on drugs used in the patients with breast cancer)
    ccc19x$der_targeted_not_her2_cdk46i <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Alpelisib', 'Bevacizumab', 'Bortezomib', 'Cetuximab', 'Daratumumab',
               'Dasatinib', 'Erlotinib', 'Everolimus', 'Fedratinib', 'Gemtuzumab ozogamicin',
               'Gilteritinib', 'Ibrutinib', 'Imatinib', 'Lenalidomide', 'Neratinib',
               'Olaparib', 'Osimertinib', 'Rituximab', 'Rucaparib', 'Ruxolitinib',
               'Sacituzimab', 'Sacituzimab govitecan', 'SAR439859', 'Selpercatinib', 'Sunitinib',
               'Talazoparib', 'Tucatinib', 'Venetoclax', 'Zandelisib'))) ccc19x$der_targeted_not_her2_cdk46i[temp.ref[i]] <- 1 else
                 ccc19x$der_targeted_not_her2_cdk46i[temp.ref[i]] <- 0
    }
    
    ccc19x$der_targeted_not_her2_cdk46i <- factor(ccc19x$der_targeted_not_her2_cdk46i)
    
    temp <- summary(ccc19x$der_targeted_not_her2_cdk46i[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_targeted_not_her2_cdk46i',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca4q: BiTE antibodies
    ccc19x$der_BiTE <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Blinatumomab','Amivantamab','Flotetuzumab','Mosunetuzumab','Zenocutuzumab',
               'BiTE antibody','REGN5458'))) ccc19x$der_BiTE[temp.ref[i]] <- 1 else
               ccc19x$der_BiTE[temp.ref[i]] <- 0
    }
    
    ccc19x$der_BiTE <- factor(ccc19x$der_BiTE)
    summary(ccc19x$der_BiTE[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca4r. CAR-T
    ccc19x$der_cart <- NA
    ccc19x$der_cart[which(ccc19x$transplant_cellular_therapy == 6|
                            grepl(ccc19x$drug1, pattern = '[oca]leucel|CAR-T')|
                            grepl(ccc19x$drug2, pattern = '[oca]leucel|CAR-T')|
                            grepl(ccc19x$drug3, pattern = '[oca]leucel|CAR-T')|
                            grepl(ccc19x$drug4, pattern = '[oca]leucel|CAR-T')|
                            grepl(ccc19x$drug5, pattern = '[oca]leucel|CAR-T')|
                            grepl(ccc19x$drug6, pattern = '[oca]leucel|CAR-T')|
                            grepl(ccc19x$drug7, pattern = '[oca]leucel|CAR-T')|
                            grepl(ccc19x$drug8, pattern = '[oca]leucel|CAR-T'))] <- 1
    ccc19x$der_cart[which(ccc19x$transplant_cellular_therapy == 99)] <- 99
    
    ccc19x$der_cart <- factor(ccc19x$der_cart)
    summary(ccc19x$der_cart[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca4s: IMiDs
    ccc19x$der_imid <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1', 'drug2', 'drug3', 'drug4', 'drug5', 'drug6', 'drug7', 'drug8')] %in%
             c('Thalidomide', 'Lenalidomide', 'Pomalidomide'))) ccc19x$der_imid[temp.ref[i]] <- 1 else
               ccc19x$der_imid[temp.ref[i]] <- 0
    }
    
    ccc19x$der_imid <- factor(ccc19x$der_imid)
    
    temp <- summary(ccc19x$der_imid[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_imid',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca4s1: IMiDS within 3 months
    ccc19x$der_imid_3mo <- NA
    
    ccc19x$der_imid_3mo[which(ccc19x$der_imid == 1 & ccc19x$der_any_systemic_3mo == 1)] <- 1
    ccc19x$der_imid_3mo[which(ccc19x$der_imid == 0|ccc19x$der_any_systemic_3mo == 0)] <- 0
    ccc19x$der_imid_3mo[which(ccc19x$der_imid == 1 & ccc19x$der_any_systemic_3mo == 99)] <- 99
    
    ccc19x$der_imid_3mo <- factor(ccc19x$der_imid_3mo)
    
    temp <- summary(ccc19x$der_imid_3mo[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_imid_3mo',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca4s2: IMiDS within 12 months
    ccc19x$der_imid_12mo <- NA
    
    ccc19x$der_imid_12mo[which(ccc19x$der_imid == 1 & ccc19x$der_any_systemic_v2 == 1)] <- 1
    ccc19x$der_imid_12mo[which(ccc19x$der_imid == 0|ccc19x$der_any_systemic_v2 == 0)] <- 0
    ccc19x$der_imid_12mo[which(ccc19x$der_imid == 1 & ccc19x$der_any_systemic_v2 == 99)] <- 99
    
    ccc19x$der_imid_12mo <- factor(ccc19x$der_imid_12mo)
    
    temp <- summary(ccc19x$der_imid_12mo[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_imid_12mo',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca25a: IO within 3 months
    ccc19x$der_IO_3mo <- NA
    
    #ICI or BiTE with known timing within 3 months
    ccc19x$der_IO_3mo[which((ccc19x$der_pd1_l1 == 1|ccc19x$der_ctla4 == 1|ccc19x$der_BiTE == 1) &
                              (ccc19x$recent_treatment %in% 1:3|ccc19x$hx_treatment == 1))] <- 1
    
    #No exposure to the three therapy types
    ccc19x$der_IO_3mo[which(((ccc19x$der_pd1_l1 == 0 & 
                                ccc19x$der_ctla4 == 0 &
                                ccc19x$der_BiTE == 0)|ccc19x$hx_treatment == 88) &
                              is.na(ccc19x$der_IO_3mo))] <- 0
    
    #Not "on treatment" regardless of exposure
    ccc19x$der_IO_3mo[which(ccc19x$on_treatment == 0)] <- 0
    
    #CAR-T with known timing within 3 months (100 days)
    ccc19x$der_IO_3mo[which(ccc19x$der_cart == 1 &
                              ccc19x$transplant_cellular_timing %in% 1:2)] <- 1
    
    #CAR-T, wrong timing
    ccc19x$der_IO_3mo[which(ccc19x$der_cart == 1 &
                              ccc19x$transplant_cellular_timing %in% c(0,3:4) &
                              is.na(ccc19x$der_IO_3mo))] <- 0
    
    #CAR-T, unknown timing
    ccc19x$der_IO_3mo[which(ccc19x$der_cart == 1 &
                              ccc19x$transplant_cellular_timing == 99 &
                              is.na(ccc19x$der_IO_3mo))] <- 99
    
    #Wrong timing
    ccc19x$der_IO_3mo[which((ccc19x$recent_treatment %in% 88:98|ccc19x$hx_treatment %in% c(2:3,98)) &
                              is.na(ccc19x$der_IO_3mo))] <- 0
    
    #Unknown exposure
    ccc19x$der_IO_3mo[which(ccc19x$on_treatment == 99 & is.na(ccc19x$der_IO_3mo))] <- 99
    
    #Unknown timing
    ccc19x$der_IO_3mo[which((ccc19x$recent_treatment == 99|ccc19x$hx_treatment == 99) & 
                               is.na(ccc19x$der_IO_3mo))] <- 99
    
    #Default to unexposed if on treatment with some other modality and this variable remains NA
    ccc19x$der_IO_3mo[which((ccc19x$treatment_modality___685 == 1|
                               ccc19x$treatment_modality___58229 == 1|
                               ccc19x$treatment_modality___691 == 1|
                               ccc19x$treatment_modality___695 == 1|
                               ccc19x$treatment_modality___14051 == 1|
                               ccc19x$treatment_modality___45215 == 1|
                               ccc19x$treatment_modality___oth == 1) &
                              (ccc19x$recent_treatment %in% 1:3|ccc19x$hx_treatment == 1) &
                              is.na(ccc19x$der_IO_3mo))] <- 0
    
    ccc19x$der_IO_3mo <- factor(ccc19x$der_IO_3mo)
    summary(ccc19x$der_IO_3mo[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca25b: IO within 12 months
    ccc19x$der_IO_12mo <- NA
    
    #ICI or BiTE with known timing within 12 months
    ccc19x$der_IO_12mo[which((ccc19x$der_pd1_l1 == 1|ccc19x$der_ctla4 == 1|ccc19x$der_BiTE == 1) &
                               (ccc19x$recent_treatment %in% 1:3|ccc19x$hx_treatment %in% 1:2))] <- 1
    
    #No exposure to the three therapy types
    ccc19x$der_IO_12mo[which(((ccc19x$der_pd1_l1 == 0 & 
                                 ccc19x$der_ctla4 == 0 &
                                 ccc19x$der_BiTE == 0)|ccc19x$hx_treatment == 88) &
                               is.na(ccc19x$der_IO_12mo))] <- 0
    
    #Not "on treatment" and history of treatment more than 1 year prior
    ccc19x$der_IO_12mo[which(ccc19x$on_treatment == 0 & ccc19x$hx_treatment == 3)] <- 0
    
    #CAR-T with known timing within 12 months
    ccc19x$der_IO_12mo[which(ccc19x$der_cart == 1 &
                               ccc19x$transplant_cellular_timing %in% 1:3)] <- 1
    
    #CAR-T, wrong timing
    ccc19x$der_IO_12mo[which(ccc19x$der_cart == 1 &
                               ccc19x$transplant_cellular_timing %in% c(0,4) &
                               is.na(ccc19x$der_IO_12mo))] <- 0
    
    #CAR-T, unknown timing
    ccc19x$der_IO_12mo[which(ccc19x$der_cart == 1 &
                               ccc19x$transplant_cellular_timing == 99 &
                               is.na(ccc19x$der_IO_12mo))] <- 99
    
    #Wrong timing
    ccc19x$der_IO_12mo[which((ccc19x$recent_treatment %in% 98|ccc19x$hx_treatment %in% c(3,98)) &
                               is.na(ccc19x$der_IO_12mo))] <- 0
    
    #Unknown exposure
    ccc19x$der_IO_12mo[which(ccc19x$on_treatment == 99 & is.na(ccc19x$der_IO_12mo))] <- 99
    
    #Unknown timing
    ccc19x$der_IO_12mo[which((ccc19x$recent_treatment == 99|ccc19x$hx_treatment == 99) & 
                               is.na(ccc19x$der_IO_12mo))] <- 99
    
    #Default to unexposed if on treatment with some other modality and this variable remains NA
    ccc19x$der_IO_12mo[which((ccc19x$treatment_modality___685 == 1|
                                ccc19x$treatment_modality___58229 == 1|
                                ccc19x$treatment_modality___691 == 1|
                                ccc19x$treatment_modality___695 == 1|
                                ccc19x$treatment_modality___14051 == 1|
                                ccc19x$treatment_modality___45215 == 1|
                                ccc19x$treatment_modality___oth == 1) &
                               (ccc19x$recent_treatment %in% 1:3|ccc19x$hx_treatment %in% 1:2) &
                               is.na(ccc19x$der_IO_12mo))] <- 0
    
    ccc19x$der_IO_12mo <- factor(ccc19x$der_IO_12mo)
    summary(ccc19x$der_IO_12mo[ccc19x$redcap_repeat_instrument == ''])
    
    # Drug variables within 3 months (requires modality/drug matching to be correct!)
    ccc19x=within(ccc19x, {
      
      der_tki_3m = ifelse(der_any_targeted_3mo == 1 & der_tki == 1, 1,
                          ifelse(der_any_targeted_3mo == 1 & der_tki == 0 | der_any_targeted_3mo == 0, 0, NA)) %>% as.factor()
      
      der_vegfi_3m = ifelse(der_any_targeted_3mo == 1 & der_vegfi == 1, 1,
                            ifelse(der_any_targeted_3mo == 1 & der_vegfi == 0 | der_any_targeted_3mo == 0, 0, NA)) %>% as.factor()
      
      der_tki_vegfi_3m = ifelse(der_any_targeted_3mo == 1 & (der_tki == 1|der_vegfi == 1), 1,
                                 ifelse(der_any_targeted_3mo == 1 & der_tki == 0 & der_vegfi == 0 | der_any_targeted_3mo == 0, 0, NA)) %>% as.factor()
      
      der_pd1_l1_3m = ifelse(der_any_immuno_3mo == 1 & der_pd1_l1 == 1, 1,
                          ifelse((der_any_immuno_3mo == 1 & der_pd1_l1 == 0) | der_any_immuno_3mo == 0, 0, NA)) %>% as.factor()
      
      der_anthracycline_3m = ifelse(der_any_cyto_3mo == 1 & der_anthracycline == 1, 1,
                                    ifelse((der_any_cyto_3mo == 1 & der_anthracycline == 0) | der_any_cyto_3mo == 0, 0, NA)) %>% as.factor()
    })
    
    temp <- summary(ccc19x$der_tki_3m[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_tki_3m',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    temp <- summary(ccc19x$der_vegfi_3m[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_vegfi_3m',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    temp <- summary(ccc19x$der_tki_vegfi_3m[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_tki_vegfi_3m',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    temp <- summary(ccc19x$der_pd1_l1_3m[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_pd1_l1_3m',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    temp <- summary(ccc19x$der_anthracycline_3m[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_anthracycline_3m',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    # Drug variables within 12 months (requires modality/drug matching to be correct!)
    ccc19x=within(ccc19x, {
      
      der_tki_12m = ifelse(der_any_targeted_12mo == 1 & der_tki == 1, 1,
                           ifelse(der_any_targeted_12mo == 1 & der_tki == 0 | der_any_targeted_12mo == 0, 0, NA)) %>% as.factor()
      
      der_vegfi_12m = ifelse(der_any_targeted_12mo == 1 & der_vegfi == 1, 1,
                             ifelse(der_any_targeted_12mo == 1 & der_vegfi == 0 | der_any_targeted_12mo == 0, 0, NA)) %>% as.factor()
      
      der_tki_vegfi_12m = ifelse(der_any_targeted_12mo == 1 & (der_tki == 1|der_vegfi == 1), 1,
                                 ifelse(der_any_targeted_12mo == 1 & der_tki == 0 & der_vegfi == 0 | der_any_targeted_12mo == 0, 0, NA)) %>% as.factor()
      
      
    })
    
    temp <- summary(ccc19x$der_tki_12m[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_tki_12m',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    temp <- summary(ccc19x$der_vegfi_12m[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_vegfi_12m',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    temp <- summary(ccc19x$der_tki_vegfi_12m[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_tki_vegfi_12m',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ###############################################################
    #C05a. Immunosuppressed version 2 - with cytotoxic chemotherapy
    ###############################################################
    ccc19x$der_immunosuppressed_v2 <- NA
    
    #First, determine NOT immunosuppressed per comorbidity variable
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___38013005|significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(any(ccc19x[i,temp.ref]) & ccc19x$significant_comorbidities___38013005[i] == 0) ccc19x$der_immunosuppressed_v2[i] <- 0
    }
    
    #Next, rule in immunosuppression
    
    #1. Immunosuppression comorbidity checked
    ccc19x$der_immunosuppressed_v2[which(ccc19x$significant_comorbidities___38013005 == 1)] <- 1
    
    #2. Receiving >20 mg/d prednisone equivalents at baseline
    ccc19x$der_immunosuppressed_v2[which(ccc19x$steroid_specific_2 %in% 2:3)] <- 1
    
    #3. Receiving "immunosuppressants" at baseline
    ccc19x$der_immunosuppressed_v2[which(ccc19x$concomitant_meds___l04a == 1)] <- 1
    
    #4. Receiving cytotoxic chemotherapy within 3 months
    ccc19x$der_immunosuppressed_v2[which(ccc19x$der_any_cyto_3mo == 1)] <- 1
    
    #Unknown
    ccc19x$der_immunosuppressed_v2[which((ccc19x$concomitant_meds___unk == 1|ccc19x$significant_comorbidities___unk == 1) &
                                           is.na(ccc19x$der_immunosuppressed_v2))] <- 99
    
    ccc19x$der_immunosuppressed_v2 <- factor(ccc19x$der_immunosuppressed_v2)
    
    temp <- summary(ccc19x$der_immunosuppressed_v2[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_immunosuppressed_v2',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ################################################################################################
    #C05a1. Immunosuppressed version 2a - with cytotoxic chemotherapy and not based on "comorbidity"
    ################################################################################################
    
    #Default is not immunosuppressed
    ccc19x$der_immunosuppressed_v2a <- 0
    
    #Next, rule in immunosuppression
    
    #1. Receiving "immunosuppressants" medication at baseline
    ccc19x$der_immunosuppressed_v2a[which(ccc19x$concomitant_meds___l04a == 1)] <- 1
    
    #2. Receiving >20 mg/d prednisone equivalents at baseline
    ccc19x$der_immunosuppressed_v2a[which(ccc19x$steroid_specific_2 %in% 2:3)] <- 1
    
    #3. Receiving cytotoxic chemotherapy within 3 months
    ccc19x$der_immunosuppressed_v2a[which(ccc19x$der_any_cyto_3mo == 1)] <- 1
    
    #Unknown
    
    #1. Baseline medications unknown
    ccc19x$der_immunosuppressed_v2a[which(ccc19x$concomitant_meds___unk == 1 &
                                            ccc19x$der_immunosuppressed_v2a == 0)] <- 99
    
    #2. Known to be on steroids but dose is unknown
    ccc19x$der_immunosuppressed_v2a[which(ccc19x$concomitant_meds___h02 == 1 &
                                            ccc19x$steroid_specific_2 == 99 &
                                            ccc19x$der_immunosuppressed_v2a == 0)] <- 99
    
    #3. Unknown if patient was on cytotoxic cancer treatment in 3 months preceding COVID-19
    ccc19x$der_immunosuppressed_v2a[which(ccc19x$der_any_cyto_3mo == 99 &
                                            ccc19x$der_immunosuppressed_v2a == 0)] <- 99
    
    ccc19x$der_immunosuppressed_v2a <- factor(ccc19x$der_immunosuppressed_v2a)
    
    temp <- summary(ccc19x$der_immunosuppressed_v2a[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_immunosuppressed_v2a',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ############################################################################################
    #C05b. Immunosuppressed version 3 - with cytotoxic chemotherapy, lymphodepletion, transplant
    ############################################################################################
    
    #Default is not immunosuppressed
    ccc19x$der_immunosuppressed_v3 <- 0
    
    #Next, rule in immunosuppression
    
    #1. Receiving "immunosuppressants" at baseline
    ccc19x$der_immunosuppressed_v3[which(ccc19x$concomitant_meds___l04a == 1)] <- 1
    
    #2. Receiving >20 mg/d prednisone equivalents at baseline
    ccc19x$der_immunosuppressed_v3[which(ccc19x$steroid_specific_2 %in% 2:3)] <- 1
    
    #3. Received cytotoxic chemotherapy within 3 months
    ccc19x$der_immunosuppressed_v3[which(ccc19x$der_any_cyto_3mo == 1)] <- 1
    
    #4. Received BTKi within 3 months
    ccc19x$der_immunosuppressed_v3[which(ccc19x$der_btki == 1 & ccc19x$der_anytx_3mo == 1)] <- 1
    
    #5. Received anti-CD20 within 12 months
    ccc19x$der_immunosuppressed_v3[which(ccc19x$der_cd20_12mo == 1)] <- 1
    
    #6. Received SCT or cellular treatment within 12 months
    ccc19x$der_immunosuppressed_v3[which((ccc19x$transplant_cellular_therapy %in% c(1:6,10)|
                                            ccc19x$significant_comorbidities___234336002 == 1) &
                                           ccc19x$transplant_cellular_timing %in% 1:3)] <- 1
    
    #Unknown
    
    #1. Baseline medications unknown
    ccc19x$der_immunosuppressed_v3[which(ccc19x$concomitant_meds___unk == 1 &
                                           ccc19x$der_immunosuppressed_v3 == 0)] <- 99
    
    #2. Known to be on steroids but dose is unknown
    ccc19x$der_immunosuppressed_v3[which(ccc19x$concomitant_meds___h02 == 1 &
                                           ccc19x$steroid_specific_2 == 99 &
                                           ccc19x$der_immunosuppressed_v3 == 0)] <- 99
    
    #3. Unknown if patient was on cytotoxic cancer treatment in 3 months preceding COVID-19
    ccc19x$der_immunosuppressed_v3[which(ccc19x$der_any_cyto_3mo == 99 &
                                           ccc19x$der_immunosuppressed_v3 == 0)] <- 99
    
    #4. Received BTKi or anti-CD20 but unknown when
    ccc19x$der_immunosuppressed_v3[which((ccc19x$der_btki == 1|ccc19x$der_cd20 == 1) & 
                                           (ccc19x$recent_treatment == 99|ccc19x$hx_treatment == 99|ccc19x$on_treatment == 99) &
                                           ccc19x$der_immunosuppressed_v3 == 0)] <- 99
    
    #6. Received SCT or cellular treatment but unknown when
    ccc19x$der_immunosuppressed_v3[which((ccc19x$transplant_cellular_therapy %in% c(1:6,10)|
                                            ccc19x$significant_comorbidities___234336002 == 1) &
                                           ccc19x$transplant_cellular_timing == 99 &
                                           ccc19x$der_immunosuppressed_v3 == 0)] <- 99
    
    ccc19x$der_immunosuppressed_v3 <- factor(ccc19x$der_immunosuppressed_v3)
    
    temp <- summary(ccc19x$der_immunosuppressed_v3[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_immunosuppressed_v3',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ####################################################################
    #C05c. Immunosuppressed version 4 - with lymphodepletion, transplant
    ####################################################################
    
    #Default is not immunosuppressed
    ccc19x$der_immunosuppressed_v4 <- 0
    
    #Next, rule in immunosuppression
    
    #1. Receiving "immunosuppressants" at baseline
    ccc19x$der_immunosuppressed_v4[which(ccc19x$concomitant_meds___l04a == 1)] <- 1
    
    #2. Receiving >20 mg/d prednisone equivalents at baseline
    ccc19x$der_immunosuppressed_v4[which(ccc19x$steroid_specific_2 %in% 2:3)] <- 1
    
    #3. Received BTKi within 3 months
    ccc19x$der_immunosuppressed_v4[which(ccc19x$der_btki == 1 & ccc19x$der_anytx_3mo == 1)] <- 1
    
    #4. Received anti-CD20 within 12 months
    ccc19x$der_immunosuppressed_v4[which(ccc19x$der_cd20_12mo == 1)] <- 1
    
    #5. Received SCT or cellular treatment within 12 months
    ccc19x$der_immunosuppressed_v4[which((ccc19x$transplant_cellular_therapy %in% c(1:6,10)|
                                            ccc19x$significant_comorbidities___234336002 == 1) &
                                           ccc19x$transplant_cellular_timing %in% 1:3)] <- 1
    
    #Unknown
    
    #1. Baseline medications unknown
    ccc19x$der_immunosuppressed_v4[which(ccc19x$concomitant_meds___unk == 1 &
                                           ccc19x$der_immunosuppressed_v4 == 0)] <- 99
    
    #2. Known to be on steroids but dose is unknown
    ccc19x$der_immunosuppressed_v4[which(ccc19x$concomitant_meds___h02 == 1 &
                                           ccc19x$steroid_specific_2 == 99 &
                                           ccc19x$der_immunosuppressed_v4 == 0)] <- 99
    
    #3. Received BTKi or anti-CD20 but unknown when
    ccc19x$der_immunosuppressed_v4[which((ccc19x$der_btki == 1|ccc19x$der_cd20 == 1) & 
                                           (ccc19x$recent_treatment == 99|ccc19x$hx_treatment == 99|ccc19x$on_treatment == 99) &
                                           ccc19x$der_immunosuppressed_v4 == 0)] <- 99
    
    #4. Received SCT or cellular treatment but unknown when
    ccc19x$der_immunosuppressed_v4[which((ccc19x$transplant_cellular_therapy %in% c(1:6,10)|
                                            ccc19x$significant_comorbidities___234336002 == 1) &
                                           ccc19x$transplant_cellular_timing == 99 &
                                           ccc19x$der_immunosuppressed_v4 == 0)] <- 99
    
    ccc19x$der_immunosuppressed_v4 <- factor(ccc19x$der_immunosuppressed_v4)
    
    temp <- summary(ccc19x$der_immunosuppressed_v4[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_immunosuppressed_v4',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Ca6: Center type
    ccc19x$der_site_type <- NA
    
    #ccc19x institutions, first
    sites <- read.csv(file = '~/Box Sync/CCC19 data/Institution list.csv', header = T, stringsAsFactors = F)
    
    for(i in 1:nrow(sites))
    {
      temp.ref <- which(ccc19x$ccc19_institution == sites$Site_ID[i])
      if(length(temp.ref) > 0) ccc19x$der_site_type[temp.ref] <- sites$Center.Type[i]
    }
    
    #Next, the unaffiliated
    temp.ref <- which(ccc19x$ccc19x != 1 & ccc19x$role == 1)
    ccc19x$der_site_type[temp.ref[which(ccc19x$practice_setting___1[temp.ref] == 1|
                                          ccc19x$practice_setting___2[temp.ref] == 1)]] <- 'CP'
    ccc19x$der_site_type[temp.ref[which(ccc19x$practice_setting___3[temp.ref] == 1)]] <- 'AMC'
    ccc19x$der_site_type[temp.ref[which(ccc19x$practice_setting___4[temp.ref] == 1|
                                          ccc19x$practice_setting___5[temp.ref] == 1|
                                          ccc19x$practice_setting___6[temp.ref] == 1)]] <- 'TCC'
    
    #Recode
    ccc19x$der_site_type[which(ccc19x$der_site_type %in% c('NCI-CC', 'NCI-CCC', 'CCC'))] <- 'TCC'
    
    #Factor
    ccc19x$der_site_type <- as.factor(ccc19x$der_site_type)
    
    temp <- summary(ccc19x$der_site_type[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_site_type',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
  }
  print('Cancer treatment and related variables completed')
  
  ##################
  #Laboratory values
  ##################
  {
    #L1. Neutrophil:Lympocyte ratio, using categorical values
    ccc19x$der_nlr_cat <- NA
    
    #High
    ccc19x$der_nlr_cat[which(ccc19x$anc_range == 'HI' & ccc19x$alc_range %in% c('LO', 'WNL'))] <- 'HI'
    ccc19x$der_nlr_cat[which(ccc19x$anc_range == 'WNL' & ccc19x$alc_range %in% c('LO'))] <- 'HI'
    
    #Low
    ccc19x$der_nlr_cat[which(ccc19x$anc_range == 'WNL' & ccc19x$alc_range %in% c('HI'))] <- 'LO'
    ccc19x$der_nlr_cat[which(ccc19x$anc_range == 'LO' & ccc19x$alc_range %in% c('WNL', 'HI'))] <- 'LO'
    
    #Neither high nor low
    ccc19x$der_nlr_cat[which(ccc19x$anc_range == 'WNL' & ccc19x$alc_range %in% c('WNL'))] <- 'Neither'
    ccc19x$der_nlr_cat[which(ccc19x$anc_range == 'LO' & ccc19x$alc_range %in% c('LO'))] <- 'Neither'
    ccc19x$der_nlr_cat[which(ccc19x$anc_range == 'HI' & ccc19x$alc_range %in% c('HI'))] <- 'Neither'
    
    #Not tested
    ccc19x$der_nlr_cat[which((ccc19x$anc_range == 'NT' & ccc19x$alc_range %in% c('NT'))|
                               ccc19x$labs == 3)] <- 'Not drawn/Not available'
    
    ccc19x$der_nlr_cat <- factor(ccc19x$der_nlr_cat)
    
    temp <- summary(ccc19x$der_nlr_cat[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_nlr_cat',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #L2. D-Dimer
    ccc19x$der_ddimer <- NA
    ccc19x$der_ddimer[which(ccc19x$ddimer == 0)] <- 'Normal'
    ccc19x$der_ddimer[which(ccc19x$ddimer == 1)] <- 'Abnormal'
    ccc19x$der_ddimer[which(ccc19x$ddimer == 99|(ccc19x$labs == 'UNK' & ccc19x$ddimer == ''))] <- 'Unknown'
    ccc19x$der_ddimer[which(ccc19x$labs == 3|ccc19x$ddimer == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_ddimer <- factor(ccc19x$der_ddimer)
    ccc19x$der_ddimer <- relevel(ccc19x$der_ddimer, ref = 'Normal')
    
    temp <- summary(ccc19x$der_ddimer[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ddimer',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #L3. Fibrinogen
    ccc19x$der_fibrinogen <- NA
    ccc19x$der_fibrinogen[which(ccc19x$fibrinogen == 0)] <- 'Normal'
    ccc19x$der_fibrinogen[which(ccc19x$fibrinogen == 1)] <- 'Abnormal'
    ccc19x$der_fibrinogen[which(ccc19x$fibrinogen == 99|(ccc19x$labs == 'UNK' & ccc19x$fibrinogen == ''))] <- 'Unknown'
    ccc19x$der_fibrinogen[which(ccc19x$labs == 3|ccc19x$fibrinogen == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_fibrinogen <- factor(ccc19x$der_fibrinogen)
    ccc19x$der_fibrinogen <- relevel(ccc19x$der_fibrinogen, ref = 'Normal')
    
    temp <- summary(ccc19x$der_fibrinogen[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_fibrinogen',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #L4. PT
    ccc19x$der_pt <- NA
    ccc19x$der_pt[which(ccc19x$pt == 0)] <- 'Normal'
    ccc19x$der_pt[which(ccc19x$pt == 1)] <- 'Abnormal'
    ccc19x$der_pt[which(ccc19x$pt == 99|(ccc19x$labs == 'UNK' & ccc19x$pt == ''))] <- 'Unknown'
    ccc19x$der_pt[which(ccc19x$labs == 3|ccc19x$pt == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_pt <- factor(ccc19x$der_pt)
    ccc19x$der_pt <- relevel(ccc19x$der_pt, ref = 'Normal')
    summary(ccc19x$der_pt[ccc19x$redcap_repeat_instrument == ''])
    
    #L5. aPTT
    ccc19x$der_aptt <- NA
    ccc19x$der_aptt[which(ccc19x$aptt == 0)] <- 'Normal'
    ccc19x$der_aptt[which(ccc19x$aptt == 1)] <- 'Abnormal'
    ccc19x$der_aptt[which(ccc19x$aptt == 99|(ccc19x$labs == 'UNK' & ccc19x$aptt == ''))] <- 'Unknown'
    ccc19x$der_aptt[which(ccc19x$labs == 3|ccc19x$aptt == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_aptt <- factor(ccc19x$der_aptt)
    ccc19x$der_aptt <- relevel(ccc19x$der_aptt, ref = 'Normal')
    summary(ccc19x$der_aptt[ccc19x$redcap_repeat_instrument == ''])
    
    #L6. High-sensitivity troponin
    ccc19x$der_hs_trop <- NA
    ccc19x$der_hs_trop[which(ccc19x$hs_trop == 0)] <- 'Normal'
    ccc19x$der_hs_trop[which(ccc19x$hs_trop == 1)] <- 'Abnormal'
    ccc19x$der_hs_trop[which(ccc19x$hs_trop == 99|(ccc19x$labs == 'UNK' & ccc19x$hs_trop == ''))] <- 'Unknown'
    ccc19x$der_hs_trop[which(ccc19x$labs == 3|ccc19x$hs_trop == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_hs_trop <- factor(ccc19x$der_hs_trop)
    ccc19x$der_hs_trop <- relevel(ccc19x$der_hs_trop, ref = 'Normal')
    summary(ccc19x$der_hs_trop[ccc19x$redcap_repeat_instrument == ''])
    
    #L7. BNP
    ccc19x$der_bnp <- NA
    ccc19x$der_bnp[which(ccc19x$bnp == 0)] <- 'Normal'
    ccc19x$der_bnp[which(ccc19x$bnp == 1)] <- 'Abnormal'
    ccc19x$der_bnp[which(ccc19x$bnp == 99|(ccc19x$labs == 'UNK' & ccc19x$bnp == ''))] <- 'Unknown'
    ccc19x$der_bnp[which(ccc19x$labs == 3|ccc19x$bnp == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_bnp <- factor(ccc19x$der_bnp)
    ccc19x$der_bnp <- relevel(ccc19x$der_bnp, ref = 'Normal')
    summary(ccc19x$der_bnp[ccc19x$redcap_repeat_instrument == ''])
    
    #L8. crp
    ccc19x$der_crp <- NA
    ccc19x$der_crp[which(ccc19x$crp == 0)] <- 'Normal'
    ccc19x$der_crp[which(ccc19x$crp == 1)] <- 'Abnormal'
    ccc19x$der_crp[which(ccc19x$crp == 99|(ccc19x$labs == 'UNK' & ccc19x$crp == ''))] <- 'Unknown'
    ccc19x$der_crp[which(ccc19x$labs == 3|ccc19x$crp == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_crp <- factor(ccc19x$der_crp)
    ccc19x$der_crp <- relevel(ccc19x$der_crp, ref = 'Normal')
    
    temp <- summary(ccc19x$der_crp[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_crp',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #L9. ldh
    ccc19x$der_ldh <- NA
    ccc19x$der_ldh[which(ccc19x$ldh == 0)] <- 'Normal'
    ccc19x$der_ldh[which(ccc19x$ldh == 1)] <- 'Abnormal'
    ccc19x$der_ldh[which(ccc19x$ldh == 99|(ccc19x$labs == 'UNK' & ccc19x$ldh == ''))] <- 'Unknown'
    ccc19x$der_ldh[which(ccc19x$labs == 3|ccc19x$ldh == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_ldh <- factor(ccc19x$der_ldh)
    ccc19x$der_ldh <- relevel(ccc19x$der_ldh, ref = 'Normal')
    
    temp <- summary(ccc19x$der_ldh[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ldh',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #L09t. Transformed LDH - normalize to U/L (same as IU/L)
    ccc19x$transformed_ldh <- ccc19x$ldh_numeric
    
    #Strip away U/L and IU/L
    ccc19x$transformed_ldh <- gsub(ccc19x$transformed_ldh, pattern = '[ ]?[I]?U/L[ ]?$|[ ]?unit[s]?/L[ ]?$', ignore.case = T, replacement = '')
    
    #Transform to numeric (this will cause loss of the values reported in atypical units, for now)
    ccc19x$transformed_ldh <- as.numeric(ccc19x$transformed_ldh)
    length(which(!is.na(ccc19x$transformed_ldh)))
    summary(ccc19x$transformed_ldh)
    
    #L10. il6
    ccc19x$der_il6 <- NA
    ccc19x$der_il6[which(ccc19x$il6 == 0)] <- 'Normal'
    ccc19x$der_il6[which(ccc19x$il6 == 1)] <- 'Abnormal'
    ccc19x$der_il6[which(ccc19x$il6 == 99|(ccc19x$labs == 'UNK' & ccc19x$il6 == ''))] <- 'Unknown'
    ccc19x$der_il6[which(ccc19x$labs == 3|ccc19x$il6 == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_il6 <- factor(ccc19x$der_il6)
    ccc19x$der_il6 <- relevel(ccc19x$der_il6, ref = 'Normal')
    
    temp <- summary(ccc19x$der_il6[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_il6',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #L11. creatinine
    ccc19x$der_creat <- NA
    ccc19x$der_creat[which(ccc19x$creat == 0)] <- 'Normal'
    ccc19x$der_creat[which(ccc19x$creat == 1)] <- 'Abnormal'
    ccc19x$der_creat[which(ccc19x$creat == 99|(ccc19x$labs == 'UNK' & ccc19x$creat == ''))] <- 'Unknown'
    ccc19x$der_creat[which(ccc19x$labs == 3|ccc19x$creat == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_creat <- factor(ccc19x$der_creat)
    ccc19x$der_creat <- relevel(ccc19x$der_creat, ref = 'Normal')
    
    temp <- summary(ccc19x$der_creat[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_creat',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #L11t. Creatinine transformation
    ccc19x$creat_flag <- FALSE
    ccc19x$transformed_creat <- ccc19x$creat_numeric
    ccc19x[which(ccc19x[,"creat_numeric"] > 30), "transformed_creat"] <- ccc19x[which(ccc19x[,"creat_numeric"] > 30, arr.ind=TRUE), "transformed_creat"] / 88.4
    ccc19x$transformed_creat <- round(ccc19x$transformed_creat, digits = 2)
    
    #couldn't determine units for 2 rows
    ccc19x[which(ccc19x[,"creat_numeric"] == 42), "creat_flag"] <- TRUE
    ccc19x[which(ccc19x[,"creat_numeric"] == 14), "creat_flag"] <- TRUE
    
    summary(ccc19x$creat[ccc19x$redcap_repeat_instrument == ''])
    summary(ccc19x$transformed_creat[ccc19x$redcap_repeat_instrument == ''])
    boxplot(log10(ccc19x$transformed_creat))
    
    #L12. wbc
    ccc19x$der_wbc <- NA
    ccc19x$der_wbc[which(ccc19x$wbc_range == 'WNL')] <- 'Normal'
    ccc19x$der_wbc[which(ccc19x$wbc_range == 'LO')] <- 'Low'
    ccc19x$der_wbc[which(ccc19x$wbc_range == 'HI')] <- 'High'
    ccc19x$der_wbc[which(ccc19x$wbc_range == 99|(ccc19x$labs == 'UNK' & ccc19x$wbc_range == ''))] <- 'Unknown'
    ccc19x$der_wbc[which(ccc19x$labs == 3|ccc19x$wbc_range == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_wbc <- factor(ccc19x$der_wbc)
    ccc19x$der_wbc <- relevel(ccc19x$der_wbc, ref = 'Normal')
    
    temp <- summary(ccc19x$der_wbc[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_wbc',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #L12t. WBC value transformation
    
    #creating the transformed row
    ccc19x$transformed_wbc <- ccc19x$wbc_numeric
    
    #Determine the units (cells/uL or cells*10^9/L) for any wbc with a decimal value, greater than 500, or less than 100 (proper units for this variable is cells*10^9/L, as stated on REDCAP variable description)
    
    ccc19x[which(ccc19x[,"transformed_wbc"] > 500), "transformed_wbc"] <- ccc19x[which(ccc19x[,"transformed_wbc"] > 500), "transformed_wbc"]/1000
    
    summary(ccc19x$wbc_numeric[ccc19x$redcap_repeat_instrument == ''])
    summary(ccc19x$transformed_wbc[ccc19x$redcap_repeat_instrument == ''])
    boxplot(ccc19x$transformed_wbc[which(as.numeric(ccc19x$transformed_wbc) <= 100)])
    
    #L13. hgb
    ccc19x$der_hgb <- NA
    ccc19x$der_hgb[which(ccc19x$hgb_range == 'WNL')] <- 'Normal'
    ccc19x$der_hgb[which(ccc19x$hgb_range == 'LO')] <- 'Low'
    ccc19x$der_hgb[which(ccc19x$hgb_range == 'HI')] <- 'High'
    ccc19x$der_hgb[which(ccc19x$hgb_range == 99|(ccc19x$labs == 'UNK' & ccc19x$hgb == ''))] <- 'Unknown'
    ccc19x$der_hgb[which(ccc19x$labs == 3|ccc19x$hgb_range == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_hgb <- factor(ccc19x$der_hgb)
    ccc19x$der_hgb <- relevel(ccc19x$der_hgb, ref = 'Normal')
    summary(ccc19x$der_hgb[ccc19x$redcap_repeat_instrument == ''])
    
    #L14. plt
    ccc19x$der_plt <- NA
    ccc19x$der_plt[which(ccc19x$plt_range == 'WNL')] <- 'Normal'
    ccc19x$der_plt[which(ccc19x$plt_range == 'LO')] <- 'Low'
    ccc19x$der_plt[which(ccc19x$plt_range == 'HI')] <- 'High'
    ccc19x$der_plt[which(ccc19x$plt_range == 99|(ccc19x$labs == 'UNK' & ccc19x$plt_range == ''))] <- 'Unknown'
    ccc19x$der_plt[which(ccc19x$labs == 3|ccc19x$plt_range == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_plt <- factor(ccc19x$der_plt)
    ccc19x$der_plt <- relevel(ccc19x$der_plt, ref = 'Normal')
    
    temp <- summary(ccc19x$der_plt[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_plt',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #L15. alc
    ccc19x$der_alc <- NA
    ccc19x$der_alc[which(ccc19x$alc_range == 'WNL')] <- 'Normal'
    ccc19x$der_alc[which(ccc19x$alc_range == 'LO')] <- 'Low'
    ccc19x$der_alc[which(ccc19x$alc_range == 'HI')] <- 'High'
    ccc19x$der_alc[which(ccc19x$alc_range == 99|(ccc19x$labs == 'UNK' & ccc19x$alc_range == ''))] <- 'Unknown'
    ccc19x$der_alc[which(ccc19x$labs == 3|ccc19x$alc_range == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_alc <- factor(ccc19x$der_alc)
    ccc19x$der_alc <- relevel(ccc19x$der_alc, ref = 'Normal')
    
    temp <- summary(ccc19x$der_alc[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_alc',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #L15a. lymphopenia
    ccc19x$der_lymphopenia <- NA
    ccc19x$der_lymphopenia[which(ccc19x$alc_range %in% c('WNL', 'HI'))] <- 'Not lymphopenic'
    ccc19x$der_lymphopenia[which(ccc19x$alc_range == 'LO')] <- 'Lymphopenic'
    ccc19x$der_lymphopenia[which(ccc19x$alc_range == 99|(ccc19x$labs == 'UNK' & ccc19x$alc_range == ''))] <- 'Unknown'
    ccc19x$der_lymphopenia[which(ccc19x$labs == 3|ccc19x$alc_range == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_lymphopenia <- factor(ccc19x$der_lymphopenia)
    ccc19x$der_lymphopenia <- relevel(ccc19x$der_lymphopenia, ref = 'Not lymphopenic')
    summary(ccc19x$der_lymphopenia[ccc19x$redcap_repeat_instrument == ''])
    
    #L16. anc
    ccc19x$der_anc <- NA
    ccc19x$der_anc[which(ccc19x$anc_range == 'WNL')] <- 'Normal'
    ccc19x$der_anc[which(ccc19x$anc_range == 'LO')] <- 'Low'
    ccc19x$der_anc[which(ccc19x$anc_range == 'HI')] <- 'High'
    ccc19x$der_anc[which(ccc19x$anc_range == 99|(ccc19x$labs == 'UNK' & ccc19x$anc_range == ''))] <- 'Unknown'
    ccc19x$der_anc[which(ccc19x$labs == 3|ccc19x$anc_range == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_anc <- factor(ccc19x$der_anc)
    ccc19x$der_anc <- relevel(ccc19x$der_anc, ref = 'Normal')
    
    temp <- summary(ccc19x$der_anc[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_anc',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #L17. Troponin
    ccc19x$der_tni <- NA
    ccc19x$der_tni[which(ccc19x$tni == 0)] <- 'Normal'
    ccc19x$der_tni[which(ccc19x$tni == 1)] <- 'Abnormal'
    ccc19x$der_tni[which(ccc19x$tni == 99|(ccc19x$labs == 'UNK' & ccc19x$tni == ''))] <- 'Unknown'
    ccc19x$der_tni[which(ccc19x$labs == 3|ccc19x$tni == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_tni <- factor(ccc19x$der_tni)
    ccc19x$der_tni <- relevel(ccc19x$der_tni, ref = 'Normal')
    summary(ccc19x$der_tni[ccc19x$redcap_repeat_instrument == ''])
    
    #L18. Combined lymphopenia and neutropenia
    ccc19x$der_lnpenia <- NA
    ccc19x$der_lnpenia[which(ccc19x$anc_range == 'LO')] <- 'Neutropenia'
    ccc19x$der_lnpenia[which(ccc19x$alc_range == 'LO')] <- 'Lymphopenia'
    ccc19x$der_lnpenia[which(ccc19x$alc_range == 'LO' & ccc19x$anc_range == 'LO')] <- 'Both'
    ccc19x$der_lnpenia[which(ccc19x$alc_range %in% c('WNL', 'HI') & ccc19x$anc_range %in% c('WNL', 'HI'))] <- 'Neither'
    
    #If either is unknown - mark as unknown
    ccc19x$der_lnpenia[which(ccc19x$alc_range == 99 | ccc19x$anc_range == 99)] <- 'Unknown'
    
    ccc19x$der_lnpenia <- factor(ccc19x$der_lnpenia)
    summary(ccc19x$der_lnpenia[ccc19x$redcap_repeat_instrument == ''])
    
    #L19. Combined troponin and hs-troponin
    ccc19x$der_trop_combined <- NA
    
    ccc19x$der_trop_combined[which(ccc19x$der_tni == 'Abnormal'|ccc19x$der_hs_trop == 'Abnormal')] <- 'Abnormal'
    ccc19x$der_trop_combined[which((ccc19x$der_tni == 'Normal' & ccc19x$der_hs_trop != 'Abnormal')|
                                     (ccc19x$der_tni != 'Abnormal' & ccc19x$der_hs_trop == 'Normal'))] <- 'Normal'
    
    temp.ref <- which(ccc19x$der_tni == ccc19x$der_hs_trop)
    ccc19x$der_trop_combined[temp.ref] <- as.character(ccc19x$der_tni[temp.ref])
    
    #If one not drawn, take the other
    temp.ref <- which(ccc19x$der_tni == 'Not drawn/Not available' & ccc19x$der_hs_trop != 'Not drawn/Not available')
    ccc19x$der_trop_combined[temp.ref] <- as.character(ccc19x$der_hs_trop[temp.ref])
    temp.ref <- which(ccc19x$der_tni != 'Not drawn/Not available' & ccc19x$der_hs_trop == 'Not drawn/Not available')
    ccc19x$der_trop_combined[temp.ref] <- as.character(ccc19x$der_tni[temp.ref])
    
    ccc19x$der_trop_combined <- factor(ccc19x$der_trop_combined)
    
    temp <- summary(ccc19x$der_trop_combined[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_trop_combined',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #L21. ALC value transformation (started out with alc_flag set to all NA values since this variable was slightly more complicated to deal with)
    
    #creation of flag and transformed_alc
    ccc19x$alc_flag <- NA
    ccc19x[which(ccc19x[,"alc"] < 10), "alc_flag"] <- FALSE
    ccc19x[which(ccc19x[,"alc"] > 120), "alc_flag"] <- FALSE
    ccc19x$transformed_alc <- ccc19x$alc
    #values between 10-120 I am unsure about units (cells/uL or cells*10^9/L)
    
    #transforming: multiply by 1000 when needed (cells/uL is proper units)
    ccc19x[which(ccc19x[,"alc"] < 10), "transformed_alc"] <- ccc19x[which(ccc19x[,"alc"] < 10), "transformed_alc"]*1000
    ccc19x[which(ccc19x$transformed_alc != floor(ccc19x$transformed_alc)), "alc_flag"] <- FALSE
    ccc19x[which(ccc19x$transformed_alc != floor(ccc19x$transformed_alc)), "transformed_alc"] <- ccc19x[which(ccc19x$transformed_alc != floor(ccc19x$transformed_alc)), "transformed_alc"] * 1000
    
    #used total wbc count to determine the units of alc
    ccc19x[which(ccc19x[,"transformed_wbc"] < 30), "alc_flag"] <- FALSE
    
    #patients with CLL
    ccc19x[which(ccc19x[,"record_id"] == 1064), "alc_flag"] <- FALSE
    ccc19x[which(ccc19x[,"record_id"] == 1064), "transformed_alc"] <- ccc19x[which(ccc19x[,"record_id"] == 1064), "transformed_alc"]*1000
    ccc19x[which(ccc19x[,"record_id"] == 133386), "alc_flag"] <- FALSE
    ccc19x[which(ccc19x[,"record_id"] == 133386), "transformed_alc"] <- ccc19x[which(ccc19x[,"record_id"] == 133386), "transformed_alc"]*1000
    
    #If there was no total wbc count for alc values between 10-120, could not easily infer the alc units
    ccc19x[which(ccc19x[, "transformed_alc"] < 120, is.na(ccc19x$alc_flag)), "alc_flag"] <- TRUE
    
    #most of the remaining with alc between 10-120, had a total wbc count to determine correct alc units
    ccc19x[which(ccc19x[, "transformed_wbc"] < 80), "alc_flag"] <- FALSE
    ccc19x[which(ccc19x[, "transformed_alc"] == 0), "alc_flag"] <- FALSE
    
    #for remaining alc, determined their units individually, or flagged them true if couldn't determine their units
    ccc19x[which(ccc19x[, "record_id"] == 1023), "alc_flag"] <- FALSE
    ccc19x[which(ccc19x[, "record_id"] == 133419), "alc_flag"] <- FALSE
    ccc19x[which(ccc19x[, "record_id"] == 1398), "alc_flag"] <- FALSE
    ccc19x[which(ccc19x[, "alc"] == 0.1), "alc_flag"] <- FALSE
    ccc19x[which(ccc19x[, "record_id"] == 462), "alc_flag"] <- TRUE
    
    summary(ccc19x$alc[ccc19x$redcap_repeat_instrument == ''])
    summary(ccc19x$transformed_alc[ccc19x$redcap_repeat_instrument == ''])
    boxplot(log10(ccc19x$transformed_alc))
    
    #L30. Transformed CD4
    ccc19x$transformed_cd4 <- ccc19x$hiv_cd4
    temp.ref <- which(!is.na(ccc19x$transformed_cd4))
    
    #transforming: multiply by 1000 when needed (cells/uL are the proper units)
    temp.ref2 <- which(ccc19x$transformed_cd4[temp.ref] < 1 &
                         ccc19x$transformed_cd4[temp.ref] > 0)
    ccc19x$transformed_cd4[temp.ref[temp.ref2]] <- 1000*ccc19x$transformed_cd4[temp.ref[temp.ref2]]
    
    #L21a. Dichotomized transformed ALC
    ccc19x$transformed_alc_v2 <- NA
    ccc19x$transformed_alc_v2[which(ccc19x$transformed_alc <= 1000)] <- "<=1000"
    ccc19x$transformed_alc_v2[which(ccc19x$transformed_alc > 1000|
                                      ccc19x$der_lymphopenia == 'Not lymphopenic')] <- ">1000"
    
    ccc19x$transformed_alc_v2 <- factor(ccc19x$transformed_alc_v2)
    summary(ccc19x$transformed_alc_v2[ccc19x$redcap_repeat_instrument == ''])
    
    #L22. ANC value transformation, similar to ALC
    
    ccc19x$anc_flag <- FALSE
    ccc19x$transformed_anc <- ccc19x$anc
    
    #individual anc where there was no wbc count to determine correct units (in gray area at anc = 100)
    ccc19x[which(ccc19x[, "transformed_anc"] == 100), "anc_flag"] <- TRUE
    
    #rest with anc < 100 and not ending in .000 were determined to be in units of cells*10^9/L, multiplied by 1000 to get cells/uL
    temp.ref <- which(!grepl(ccc19x$transformed_anc, pattern = '.000') & ccc19x$transformed_anc < 100)
    ccc19x[temp.ref, "transformed_anc"] <- ccc19x[temp.ref, "transformed_anc"]*1000
    
    #individual record for which could not determine anc value units
    ccc19x[which(ccc19x[, "record_id"] == 133398), "anc_flag"] <- TRUE
    
    #only anc value less than 100 where could conclude that it was in cells/uL (fixed this after multiplying anc values less than 100)
    ccc19x[which(ccc19x[, "record_id"] == 773), "transformed_anc"] <- 20.0
    
    summary(ccc19x$anc[ccc19x$redcap_repeat_instrument == ''])
    summary(ccc19x$transformed_anc[ccc19x$redcap_repeat_instrument == ''])
    boxplot(ccc19x$transformed_anc)
    
    #L23. AEC value calculation
    
    ccc19x$aec_flag <- FALSE
    ccc19x$transformed_aec <- ccc19x$aec
    
    #similar to other WBC counts, multiplied all values with a non-zero digit after the decimal point by 1000, to get values in cells/uL
    ccc19x[which(ccc19x[,"transformed_aec"] %% 1 >0), "transformed_aec"] <- ccc19x[which(ccc19x[,"transformed_aec"] %% 1 >0), "transformed_aec"] * 1000
    
    #determined if aec values could be transformed into correct units 
    ccc19x[which(ccc19x[, "record_id"] == 335), "aec_flag"] <- TRUE
    ccc19x[which(ccc19x[, "record_id"] == 1485), "aec_flag"] <- TRUE
    ccc19x[which(ccc19x[, "transformed_aec"] == 11), "aec_flag"] <- TRUE
    ccc19x[which(ccc19x[, "record_id"] == 824), "aec_flag"] <- TRUE
    ccc19x[which(ccc19x[, "record_id"] == 133193), "aec_flag"] <- TRUE
    ccc19x[which(ccc19x[,"transformed_aec"] < 10), "transformed_aec"] <- ccc19x[which(ccc19x[,"transformed_aec"] < 10), "transformed_aec"] * 1000
    ccc19x[which(ccc19x[, "record_id"] == 801), "aec_flag"] <- TRUE
    
    summary(ccc19x$aec[ccc19x$redcap_repeat_instrument == ''])
    summary(ccc19x$transformed_aec[ccc19x$redcap_repeat_instrument == ''])
    boxplot(ccc19x$transformed_aec)
    
    #L24. HGB value transformation (transform a few values that were reported in g/L instead of g/dL)
    
    ccc19x$transformed_hgb <- ccc19x$hgb
    ccc19x$transformed_hgb[which(ccc19x$transformed_hgb >20 & ccc19x$transformed_hgb <= 500)] <- ccc19x$transformed_hgb[which(ccc19x$transformed_hgb >20 & ccc19x$transformed_hgb <= 500)] / 10
    ccc19x$transformed_hgb[which(ccc19x$transformed_hgb >500)] <- ccc19x$transformed_hgb[which(ccc19x$transformed_hgb >500)] / 100
    
    summary(ccc19x$hgb[ccc19x$redcap_repeat_instrument == ''])
    summary(ccc19x$transformed_hgb[ccc19x$redcap_repeat_instrument == ''])
    boxplot(ccc19x$transformed_hgb)
    
    #L25. PLT value transformation (only needed to transform a few values that were off by 1000)
    
    ccc19x$transformed_plt <- ccc19x$plt
    ccc19x[which(ccc19x[,"transformed_plt"] > 10000), "transformed_plt"] <- ccc19x[which(ccc19x[,"transformed_plt"] > 10000), "transformed_plt"] / 1000
    ccc19x[which(ccc19x[,"transformed_plt"] < 1), "transformed_plt"] <- ccc19x[which(ccc19x[,"transformed_plt"] < 1), "transformed_plt"] * 1000
    ccc19x[which(ccc19x[,"transformed_plt"] < 10), "transformed_plt"] <- ccc19x[which(ccc19x[,"transformed_plt"] < 10), "transformed_plt"] * 10
    
    summary(ccc19x$plt[ccc19x$redcap_repeat_instrument == ''])
    summary(ccc19x$transformed_plt[ccc19x$redcap_repeat_instrument == ''])
    boxplot(ccc19x$transformed_plt)
    
    #L27. AST
    ccc19x$der_ast <- NA
    ccc19x$der_ast[which(ccc19x$ast == 0)] <- 'Normal'
    ccc19x$der_ast[which(ccc19x$ast == 1)] <- 'Abnormal'
    ccc19x$der_ast[which(ccc19x$ast == 99|(ccc19x$labs == 'UNK' & ccc19x$ast == ''))] <- 'Unknown'
    ccc19x$der_ast[which(ccc19x$labs == 3|ccc19x$ast == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_ast <- factor(ccc19x$der_ast)
    ccc19x$der_ast <- relevel(ccc19x$der_ast, ref = 'Normal')
    
    temp <- summary(ccc19x$der_ast[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_ast',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #L28. ALT
    ccc19x$der_alt <- NA
    ccc19x$der_alt[which(ccc19x$alt == 0)] <- 'Normal'
    ccc19x$der_alt[which(ccc19x$alt == 1)] <- 'Abnormal'
    ccc19x$der_alt[which(ccc19x$alt == 99|(ccc19x$labs == 'UNK' & ccc19x$alt == ''))] <- 'Unknown'
    ccc19x$der_alt[which(ccc19x$labs == 3|ccc19x$alt == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_alt <- factor(ccc19x$der_alt)
    ccc19x$der_alt <- relevel(ccc19x$der_alt, ref = 'Normal')
    
    temp <- summary(ccc19x$der_alt[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_alt',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #L29. Total bilirubin
    ccc19x$der_tbili <- NA
    ccc19x$der_tbili[which(ccc19x$tbili == 0)] <- 'Normal'
    ccc19x$der_tbili[which(ccc19x$tbili == 1)] <- 'Abnormal'
    ccc19x$der_tbili[which(ccc19x$tbili == 99|(ccc19x$labs == 'UNK' & ccc19x$tbili == ''))] <- 'Unknown'
    ccc19x$der_tbili[which(ccc19x$labs == 3|ccc19x$tbili == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_tbili <- factor(ccc19x$der_tbili)
    ccc19x$der_tbili <- relevel(ccc19x$der_tbili, ref = 'Normal')
    
    temp <- summary(ccc19x$der_tbili[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_tbili',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    }
  print('Laboratory values completed')
  
  ######
  #Other
  ######
  {
    
    #########################################
    #X1. Negative controls (partial variable)
    #########################################
    ccc19x$der_neg_control <- NA
    
    #Non-Exposure to the categories of interest
    ccc19x$der_neg_control[which(   ccc19x$der_other_tx_c19 == 0 &
                                      ccc19x$der_hcq == 0 &
                                      ccc19x$der_azithro == 0 &
                                      ccc19x$der_steroids_hd_c19 == 0 &
                                      ccc19x$der_rem == 0 &
                                      ccc19x$der_toci == 0)] <- 1
    
    summary(factor(ccc19x$der_neg_control))
    
    #Definitive evidence of no treatment
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment___none'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$covid_19_treatment___none[i] == 1 & is.na(ccc19x$der_neg_control[i])) ccc19x$der_neg_control[i] <- 1
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___none'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$covid_19_treatment_fu___none[i] == 1 & is.na(ccc19x$der_neg_control[i])) ccc19x$der_neg_control[i] <- 1
    
    summary(factor(ccc19x$der_neg_control))
    
    #Unknown
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$covid_19_treatment___unk[i] == 1) ccc19x$der_neg_control[i] <- 99
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$covid_19_treatment_fu___unk[i] == 1) ccc19x$der_neg_control[i] <- 99
    
    summary(factor(ccc19x$der_neg_control))
    
    #Missing (should not have any hits)
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_neg_control[i] <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_neg_control[i] <- NA
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_neg_control[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_neg_control[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_neg_control[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_neg_control[temp.ref] <- 0
      }
    }
    
    ccc19x$der_neg_control <- factor(ccc19x$der_neg_control)
    summary(ccc19x$der_neg_control[ccc19x$redcap_repeat_instrument == ''])
    
    #######################
    #X2. IMWG frailty index
    #######################
    
    #Frailty scored using adapted simplified IMWG frailty score (Palumbo et al, Blood, 2015; Facon et al, Leukemia, 2019)
    #Patients will be categorized as nonfrail (0-1 pt), frail (≥2)  
    ccc19x$der_imwg <- NA
    
    #Age
    ccc19x$der_imwg[which(ccc19x$der_age_trunc <= 75)] <- 0
    ccc19x$der_imwg[which(ccc19x$der_age_trunc > 75 & ccc19x$der_age_trunc <= 80)] <- 1
    ccc19x$der_imwg[which(ccc19x$der_age_trunc > 80)] <- 2
    
    #Charlson comorbidity score
    ccc19x$der_imwg[which(ccc19x$der_ccc19cci %in% 1:2)] <- ccc19x$der_imwg[which(ccc19x$der_ccc19cci %in% 1:2)] + 1
    ccc19x$der_imwg[which(ccc19x$der_ccc19cci %in% 3:20)] <- ccc19x$der_imwg[which(ccc19x$der_ccc19cci %in% 3:20)] + 2
    
    #ECOG
    ccc19x$der_imwg[which(ccc19x$der_ecogcat2 == 1)] <- ccc19x$der_imwg[which(ccc19x$der_ecogcat2 == 1)] + 1
    ccc19x$der_imwg[which(ccc19x$der_ecogcat2 == '2+')] <- ccc19x$der_imwg[which(ccc19x$der_ecogcat2 == '2+')] + 2
    
    #Recast unknowns and missing (partial missing) as unknown
    ccc19x$der_imwg[which((ccc19x$der_ccc19cci == '99'|is.na(ccc19x$der_ccc19cci)) & ccc19x$der_imwg < 2)] <- 99
    ccc19x$der_imwg[which(is.na(ccc19x$der_ecogcat2) & ccc19x$der_imwg < 2)] <- 99
    
    #Recast ECOG unknowns as "at least"
    ccc19x$der_imwg[which(ccc19x$der_ecogcat2 == 'Unknown' & ccc19x$der_imwg %in% 0:1)] <- 'At least non-frail'
    
    #Recast scores to frailty
    ccc19x$der_imwg[which(ccc19x$der_imwg %in% 0:1)] <- 'Non-frail'
    ccc19x$der_imwg[which(ccc19x$der_imwg %in% 2:6)] <- 'Frail'
    ccc19x$der_imwg[which(ccc19x$der_imwg == 99)] <- 'Unknown'
    
    ccc19x$der_imwg <- factor(ccc19x$der_imwg)
    
    temp <- summary(ccc19x$der_imwg[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_imwg',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Collapsed IMWG combining "At least non-frail" with Non-frail
    ccc19x <- within(ccc19x, {der_imwg_comb <- 
      as.factor(ifelse(der_imwg == 'Frail', 'Frail',
                       ifelse(der_imwg == "Unknown", "Unknown", 'Non-frail')))})
    
    temp <- summary(ccc19x$der_imwg_comb[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_imwg_comb',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #X2a. Modified IMWG frailty index
    #Patients will be categorized as nonfrail (0-1 pt), prefrail/intermediate (2-3 pts) or frail (4-6 pts)
    ccc19x$der_imwg_mod <- NA
    
    #Age
    ccc19x$der_imwg_mod[which(ccc19x$der_age_trunc <= 75)] <- 0
    ccc19x$der_imwg_mod[which(ccc19x$der_age_trunc > 75 & ccc19x$der_age_trunc <= 80)] <- 1
    ccc19x$der_imwg_mod[which(ccc19x$der_age_trunc > 80)] <- 2
    
    #Charlson comorbidity score
    ccc19x$der_imwg_mod[which(ccc19x$der_ccc19cci %in% 1:2)] <- ccc19x$der_imwg_mod[which(ccc19x$der_ccc19cci %in% 1:2)] + 1
    ccc19x$der_imwg_mod[which(ccc19x$der_ccc19cci %in% 3:20)] <- ccc19x$der_imwg_mod[which(ccc19x$der_ccc19cci %in% 3:20)] + 2
    
    #ECOG
    ccc19x$der_imwg_mod[which(ccc19x$der_ecogcat2 == 1)] <- ccc19x$der_imwg_mod[which(ccc19x$der_ecogcat2 == 1)] + 1
    ccc19x$der_imwg_mod[which(ccc19x$der_ecogcat2 == '2+')] <- ccc19x$der_imwg_mod[which(ccc19x$der_ecogcat2 == '2+')] + 2
    
    #Recast unknowns and missing (partial missing) as unknown
    ccc19x$der_imwg_mod[which((ccc19x$der_ccc19cci == '99'|is.na(ccc19x$der_ccc19cci)) & ccc19x$der_imwg_mod < 2)] <- 99
    ccc19x$der_imwg_mod[which(is.na(ccc19x$der_ecogcat2) & ccc19x$der_imwg_mod < 2)] <- 99
    
    #Recast ECOG unknowns as "at least"
    ccc19x$der_imwg_mod[which(ccc19x$der_ecogcat2 == 'Unknown' & ccc19x$der_imwg_mod %in% 0:1)] <- 'At least non-frail'
    ccc19x$der_imwg_mod[which(ccc19x$der_ecogcat2 == 'Unknown' & ccc19x$der_imwg_mod %in% 2:3)] <- 'At least prefrail'
    
    #Recast scores to frailty
    ccc19x$der_imwg_mod[which(ccc19x$der_imwg_mod %in% 0:1)] <- 'Non-frail'
    ccc19x$der_imwg_mod[which(ccc19x$der_imwg_mod %in% 2:3)] <- 'Prefrail'
    ccc19x$der_imwg_mod[which(ccc19x$der_imwg_mod %in% 4:6)] <- 'Frail'
    ccc19x$der_imwg_mod[which(ccc19x$der_imwg_mod == 99)] <- 'Unknown'
    
    ccc19x$der_imwg_mod <- factor(ccc19x$der_imwg_mod)
    
    temp <- summary(ccc19x$der_imwg_mod[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_imwg_mod',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #Collapsed modified IMWG
    ccc19x <- within(ccc19x, {der_imwg_mod_comb <- 
      as.factor(ifelse(der_imwg_mod == 'Frail', 'Frail', 
                       ifelse(der_imwg_mod == "Non-frail" | der_imwg_mod == 'At least non-frail', 
                              'Non-frail', 
                              ifelse(der_imwg_mod == "Unknown", "Unknown", 'Prefrail'))))})
    
    temp <- summary(ccc19x$der_imwg_mod_comb[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_imwg_mod_comb',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #####################
    #X3. Modified Khorana only based on cancer_type (most active tumor)
    #####################
    ccc19x$der_VTE_risk_v3 <- NA
    
    ccc19x$der_VTE_risk_v3[which(ccc19x$cancer_type %in% c("C3234","C3411","C3099","C3844","C4436","C3267", "C7558", "C9039","C3867","C3555","C3917","C4866","C4815", "C9325", "C3809", "C4906","C9306", "C3868", "C9145","C9312","C4817","C3359","C8538","C3059","C4627","C5111","C132067", "C3270", "C7541","C3224", "C9231","C4819","C2921","C132146","C4039","C3538","C9061","C6389","C4189","OTH","OTH_S"))] <- 'Other solid malignancy'
    ccc19x$der_VTE_risk_v3[which(ccc19x$cancer_type %in% c("C3167","C3163","C9308","C3247","C3171","C4345","C3106","C3174","C3242","C4665","C3819","C27134","C9300","OTH_H"))] <- 'Other heme malignancy'
    ccc19x$der_VTE_risk_v3[which(ccc19x$cancer_type %in% c("C4872","C4863","C9291","C4910","C9330","C7724","C2955","C9382","C4013", "C3871"))] <- 'Low-risk VTE malignancy'
    ccc19x$der_VTE_risk_v3[which(ccc19x$cancer_type %in% c("C4917","C2926","C4878","C7431","C9063","C4912","C3708","C7355","C9385","C8851","C3209","C9244","C4341","C3211","C3457","C9357","C4337","C2912","C8504","C27908"))] <- 'Intermediate-risk VTE malignancy'
    ccc19x$der_VTE_risk_v3[which(ccc19x$cancer_type %in% c("C3850","C4911","C3513"))] <- 'High-risk VTE malignancy'
    
    ccc19x$der_VTE_risk_v3 <- factor(ccc19x$der_VTE_risk_v3)
    ccc19x$der_VTE_risk_v3 <- relevel(ccc19x$der_VTE_risk_v3, ref = 'Low-risk VTE malignancy')
    
    temp <- summary(ccc19x$der_VTE_risk_v3[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_VTE_risk_v3',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ############################################
    #X3a. Khorana risk with collapsed categories
    ############################################
    ccc19x$der_VTE_risk_v3a <- ccc19x$der_VTE_risk_v3
    ccc19x$der_VTE_risk_v3a[which(ccc19x$der_VTE_risk_v3a %in% c('Other heme malignancy', 'Other solid malignancy'))] <- 'Low-risk VTE malignancy'
    ccc19x$der_VTE_risk_v3a <- droplevels(ccc19x$der_VTE_risk_v3a)
    
    temp <- summary(ccc19x$der_VTE_risk_v3a[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_VTE_risk_v3a',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ######################
    #X3c. Khorana low-risk
    ######################
    ccc19x$der_VTE_risk_low <- 0
    
    #Create dummy variables if the data set does not have cancer_type_3, 4, 5
    if(is.null(ccc19x$cancer_type_3))
    {
      ccc19x$cancer_type_3 <- ''
      ccc19x$cancer_type_4 <- ''
      ccc19x$cancer_type_5 <- ''
    }
    
    ccc19x$der_VTE_risk_low[which((ccc19x$cancer_type %in% c("C4872","C4863","C9291","C4910","C9330","C7724","C2955","C9382","C4013", "C3871")| 
                                    ccc19x$cancer_type_2 %in% c("C4872","C4863","C9291","C4910","C9330","C7724","C2955","C9382","C4013", "C3871")| 
                                    ccc19x$cancer_type_3 %in% c("C4872","C4863","C9291","C4910","C9330","C7724","C2955","C9382","C4013", "C3871")| 
                                    ccc19x$cancer_type_4 %in% c("C4872","C4863","C9291","C4910","C9330","C7724","C2955","C9382","C4013", "C3871")| 
                                    ccc19x$cancer_type_5 %in% c("C4872","C4863","C9291","C4910","C9330","C7724","C2955","C9382","C4013", "C3871")))] <- 1
    
    ccc19x$der_VTE_risk_low <- factor(ccc19x$der_VTE_risk_low)
    summary(ccc19x$der_VTE_risk_low[ccc19x$redcap_repeat_instrument == ''])
    
    ###############################
    #X3d. Khorana intermediate-risk
    ###############################
    ccc19x$der_VTE_risk_int <- 0
    
    #Create dummy variables if the data set does not have cancer_type_3, 4, 5
    if(is.null(ccc19x$cancer_type_3))
    {
      ccc19x$cancer_type_3 <- ''
      ccc19x$cancer_type_4 <- ''
      ccc19x$cancer_type_5 <- ''
    }
    
    ccc19x$der_VTE_risk_int[which((ccc19x$cancer_type %in% c("C4917","C2926","C4878","C7431","C9063","C4912","C3708","C7355","C9385","C8851","C3209","C9244","C4341","C3211","C9357","C4337","C2912","C8504","C27908")| 
                                     ccc19x$cancer_type_2 %in% c("C4917","C2926","C4878","C7431","C9063","C4912","C3708","C7355","C9385","C8851","C3209","C9244","C4341","C3211","C9357","C4337","C2912","C8504","C27908")| 
                                     ccc19x$cancer_type_3 %in% c("C4917","C2926","C4878","C7431","C9063","C4912","C3708","C7355","C9385","C8851","C3209","C9244","C4341","C3211","C9357","C4337","C2912","C8504","C27908")| 
                                     ccc19x$cancer_type_4 %in% c("C4917","C2926","C4878","C7431","C9063","C4912","C3708","C7355","C9385","C8851","C3209","C9244","C4341","C3211","C9357","C4337","C2912","C8504","C27908")| 
                                     ccc19x$cancer_type_5 %in% c("C4917","C2926","C4878","C7431","C9063","C4912","C3708","C7355","C9385","C8851","C3209","C9244","C4341","C3211","C9357","C4337","C2912","C8504","C27908")))] <- 1
    
    ccc19x$der_VTE_risk_int <- factor(ccc19x$der_VTE_risk_int)
    summary(ccc19x$der_VTE_risk_int[ccc19x$redcap_repeat_instrument == ''])
    
    ###############################
    #X3e. Khorana high-risk cancers
    ###############################
    ccc19x$der_VTE_risk_high <- 0
    
    #Create dummy variables if the data set does not have cancer_type_3, 4, 5
    if(is.null(ccc19x$cancer_type_3))
    {
      ccc19x$cancer_type_3 <- ''
      ccc19x$cancer_type_4 <- ''
      ccc19x$cancer_type_5 <- ''
    }
    
    ccc19x$der_VTE_risk_high[which((ccc19x$cancer_type %in% c("C3850","C4911","C3513")| 
                                      ccc19x$cancer_type_2 %in% c("C3850","C4911", "C3513")| 
                                      ccc19x$cancer_type_3 %in% c("C3850","C4911", "C3513")| 
                                      ccc19x$cancer_type_4 %in% c("C3850","C4911", "C3513")| 
                                      ccc19x$cancer_type_5 %in% c("C3850","C4911", "C3513")))] <- 1
    
    ccc19x$der_VTE_risk_high <- factor(ccc19x$der_VTE_risk_high)
    summary(ccc19x$der_VTE_risk_high[ccc19x$redcap_repeat_instrument == ''])
    
    #####################
    #X3f. Modified Khorana with uterine re-assigned, and only two categories
    #####################
    ccc19x$der_VTE_risk_v4 <- NA
    
    ccc19x$der_VTE_risk_v4[which(ccc19x$cancer_type %in% c("C4872","C4863","C9291","C4910","C9330","C7724","C2955","C9382","C4013", "C3871","C3167","C3163","C9308","C3247","C3171","C4345","C3106","C3174","C3242","C4665","C3819","C27134","C9300","OTH_H","C3234","C3411","C3099","C3844","C4436","C3267","C9039","C3867","C3555","C3917","C4866","C4815", "C9325", "C3809", "C4906","C9306", "C3868", "C9145","C9312","C4817","C3359","C8538","C3059","C4627","C5111","C132067", "C3270", "C7541","C3224", "C9231","C4819","C2921","C132146","C4039","C3538","C9061","C6389","C4189","OTH","OTH_S"))] <- 'Low/Other'
    ccc19x$der_VTE_risk_v4[which(ccc19x$cancer_type %in% c("C3850","C4911","C3513","C4917","C2926","C4878","C7431","C9063","C4912","C3708","C7355","C9385","C8851","C3209","C9244","C4341","C3211","C3457","C9357","C4337","C2912","C8504","C7558","C27908"))] <- 'High/Very high'
    
    ccc19x$der_VTE_risk_v4 <- factor(ccc19x$der_VTE_risk_v4)
    
    temp <- summary(ccc19x$der_VTE_risk_v4[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_VTE_risk_v4',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #X6a-c. Due dates for follow-up forms, assuming right-sided diagnosis (i.e., at least)
    ccc19x$meta_30d_due <- NA
    ccc19x$meta_90d_due <- NA
    ccc19x$meta_180d_due <- NA
    ccc19x$meta_365d_due <- NA
    
    temp.ref <- which(ccc19x$redcap_repeat_instrument == '')
    ccc19x$meta_30d_due[temp.ref] <- as.character(as.POSIXct(ccc19x$meta_lefttime_ub[temp.ref]) + 30*24*60*60)
    ccc19x$meta_30d_due[temp.ref] <- gsub(ccc19x$meta_30d_due[temp.ref], pattern = ' [0-9]{2}:[0-9]{2}:[0-9]{2}', replacement = '')
    ccc19x$meta_90d_due[temp.ref] <- as.character(as.POSIXct(ccc19x$meta_lefttime_ub[temp.ref]) + 90*24*60*60)
    ccc19x$meta_90d_due[temp.ref] <- gsub(ccc19x$meta_90d_due[temp.ref], pattern = ' [0-9]{2}:[0-9]{2}:[0-9]{2}', replacement = '')
    ccc19x$meta_180d_due[temp.ref] <- as.character(as.POSIXct(ccc19x$meta_lefttime_ub[temp.ref]) + 180*24*60*60)
    ccc19x$meta_180d_due[temp.ref] <- gsub(ccc19x$meta_180d_due[temp.ref], pattern = ' [0-9]{2}:[0-9]{2}:[0-9]{2}', replacement = '')
    ccc19x$meta_365d_due[temp.ref] <- as.character(as.POSIXct(ccc19x$meta_lefttime_ub[temp.ref]) + 365*24*60*60)
    ccc19x$meta_365d_due[temp.ref] <- gsub(ccc19x$meta_365d_due[temp.ref], pattern = ' [0-9]{2}:[0-9]{2}:[0-9]{2}', replacement = '')
    
    
    #X8a-c. Completion of 30d, 90d, 180d forms
    ccc19x$der_30d_complete <- NA
    ccc19x$der_90d_complete <- NA
    ccc19x$der_180d_complete <- NA
    
    #Yes - named f/u form is completed
    ccc19x$der_30d_complete[which(ccc19x$record_id %in% ccc19x$record_id[which(ccc19x$fu_weeks == 30)] & ccc19x$redcap_repeat_instrument == '')] <- 'Yes'
    ccc19x$der_90d_complete[which(ccc19x$record_id %in% ccc19x$record_id[which(ccc19x$fu_weeks == 90)] & ccc19x$redcap_repeat_instrument == '')] <- 'Yes'
    ccc19x$der_180d_complete[which(ccc19x$record_id %in% ccc19x$record_id[which(ccc19x$fu_weeks == 180)] & ccc19x$redcap_repeat_instrument == '')] <- 'Yes'
    
    #Yes - other time interval f/u form is completed on or after the due date
    ccc19x$der_30d_complete[which(ccc19x$record_id %in% ccc19x$record_id[which(ccc19x$timing_of_report_weeks > 4)] 
                                  & ccc19x$redcap_repeat_instrument == '')] <- 'Yes, through an "other time interval" follow-up form'
    ccc19x$der_90d_complete[which(ccc19x$record_id %in% ccc19x$record_id[which(ccc19x$timing_of_report_weeks > 12)] 
                                  & ccc19x$redcap_repeat_instrument == '')] <- 'Yes, through an "other time interval" follow-up form'
    ccc19x$der_180d_complete[which(ccc19x$record_id %in% ccc19x$record_id[which(ccc19x$timing_of_report_weeks > 25)] 
                                   & ccc19x$redcap_repeat_instrument == '')] <- 'Yes, through an "other time interval" follow-up form'
    
    #NA - patient is deceased prior to the due date by days
    temp.ref <- which(ccc19x$der_days_to_death_combined %in% 0:9998 & ccc19x$redcap_repeat_instrument == '')
    ccc19x$der_30d_complete[temp.ref][which(ccc19x$der_days_to_death_combined[temp.ref] < 30 & is.na(ccc19x$der_30d_complete[temp.ref]))] <- "N/A - Patient died before form was due"
    ccc19x$der_90d_complete[temp.ref][which(ccc19x$der_days_to_death_combined[temp.ref] < 90 & is.na(ccc19x$der_90d_complete[temp.ref]))] <- "N/A - Patient died before form was due"
    ccc19x$der_180d_complete[temp.ref][which(ccc19x$der_days_to_death_combined[temp.ref] < 180 & is.na(ccc19x$der_180d_complete[temp.ref]))] <- "N/A - Patient died before form was due"
    
    #NA - patient is deceased prior to the due date by flags
    ccc19x$der_30d_complete[which(ccc19x$der_dead30 == 1 & is.na(ccc19x$der_30d_complete))] <- "N/A - Patient died before form was due"
    ccc19x$der_90d_complete[which(ccc19x$der_dead90a == 1 & is.na(ccc19x$der_90d_complete))] <- "N/A - Patient died before form was due"
    ccc19x$der_180d_complete[which(ccc19x$der_dead180a == 1 & is.na(ccc19x$der_180d_complete))] <- "N/A - Patient died before form was due"
    
    #NA - baseline diagnostic interval exceed the required follow-up timeframe
    ccc19x$der_30d_complete[which(ccc19x$covid_19_dx_interval %in% 4:10 & is.na(ccc19x$der_30d_complete))] <- "N/A - Baseline diagnostic interval exceeds the follow-up period"
    ccc19x$der_90d_complete[which(ccc19x$covid_19_dx_interval %in% 6:10 & is.na(ccc19x$der_90d_complete))] <- "N/A - Baseline diagnostic interval exceeds the follow-up period"
    ccc19x$der_180d_complete[which(ccc19x$covid_19_dx_interval %in% 7:10 & is.na(ccc19x$der_180d_complete))] <- "N/A - Baseline diagnostic interval exceeds the follow-up period"
    
    #All others default to No
    ccc19x$der_30d_complete[which(is.na(ccc19x$der_30d_complete) & ccc19x$redcap_repeat_instrument == '')] <- 'No'
    ccc19x$der_90d_complete[which(is.na(ccc19x$der_90d_complete) & ccc19x$redcap_repeat_instrument == '')] <- 'No'
    ccc19x$der_180d_complete[which(is.na(ccc19x$der_180d_complete) & ccc19x$redcap_repeat_instrument == '')] <- 'No'
    
    ccc19x$der_30d_complete <- factor(ccc19x$der_30d_complete)
    ccc19x$der_90d_complete <- factor(ccc19x$der_90d_complete)
    ccc19x$der_180d_complete <- factor(ccc19x$der_180d_complete)
    
    temp <- summary(ccc19x$der_30d_complete[which(ccc19x$redcap_repeat_instrument == '' & ccc19x$der_Breast == 1)])
    temp.var.log <- data.frame(name = 'der_30d_complete',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    temp <- summary(ccc19x$der_90d_complete[which(ccc19x$redcap_repeat_instrument == '' & ccc19x$der_Breast == 1)])
    temp.var.log <- data.frame(name = 'der_90d_complete',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    temp <- summary(ccc19x$der_180d_complete[which(ccc19x$redcap_repeat_instrument == '' & ccc19x$der_Breast == 1)])
    temp.var.log <- data.frame(name = 'der_180d_complete',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    ##############################################
    #X4. Quality score and X5. Enumerated problems
    ##############################################
    {
      #Calculate a quality score for each case
      ccc19x$meta_quality <- 0
      ccc19x$meta_problems <- ''
      
      quality_report <- data.frame(class = character(), metric = character(), n = character(), stringsAsFactors = F)
      n <- length(unique(ccc19x$record_id))
      
      ###############
      #Major problems
      ###############
      
      #High levels of baseline missingness
      dict <- read.csv(file = 'CCC19_DataDictionary.csv', header = T, stringsAsFactors = F)
      colnames(dict)[1] <- 'name'
      
      ccc19x$missing <- 0
      ref <- c(which(colnames(ccc19x) == 'timing_of_report'),
               which(colnames(ccc19x) == 'dx_year'),
               which(colnames(ccc19x) == 'covid_19_dx_interval'),
               which(colnames(ccc19x) == 'ts_1'):which(colnames(ccc19x) == 'cancer_details_complete'))
      for(i in which(ccc19x$redcap_repeat_instrument == ''))
      {
        ccc19x$missing[i] <- sum(is.na(ccc19x[i,ref]))
        for(j in which(dict$Field.Type == 'checkbox' & dict$Form.Name != 'followup'))
        {
          temp.ref <- grep(colnames(ccc19x), pattern = paste(dict$name[j], '___', sep = ''))
          if(all(ccc19x[i,temp.ref] == 0)) ccc19x$missing[i] <- ccc19x$missing[i] + 1
        }
      }
      
      #Create density plot of missingness
      x <- density(ccc19x$missing[ccc19x$redcap_repeat_instrument == ''])
      plot(x)
      threshold <- 126
      abline(v = threshold, col = 'red', lty = 2)
      
      ccc19x$meta_quality[which(ccc19x$missing > threshold)] <- ccc19x$meta_quality[which(ccc19x$missing > threshold)] + 5
      ccc19x$meta_problems[which(ccc19x$missing > threshold)] <- paste(ccc19x$meta_problems[which(ccc19x$missing > threshold)],
                                                                       '; High levels of baseline missingness', sep = '')
      
      quality_report[nrow(quality_report)+1,] <- c('Major criteria (5 points)',
                                                   'Acceptable level of baseline missingness',
                                                   paste(format(length(which(ccc19x$missing <= threshold & ccc19x$redcap_repeat_instrument == '')), big.mark = ', '),
                                                         ' of ',
                                                         format(n, big.mark = ', '),
                                                         ' (', 
                                                         round(100*length(which(ccc19x$missing <= threshold & ccc19x$redcap_repeat_instrument == ''))/n, digits = 2),
                                                         '%)', sep = ''))
      
      #Large number of unknowns
      dict.unk <- dict[grep(dict$Choices..Calculations..OR.Slider.Labels, pattern = 'Unknown'),]
      
      u1 <- dict.unk$name[dict.unk$Field.Type == 'radio']
      temp <- dict.unk$name[dict.unk$Field.Type == 'checkbox']
      u2 <- colnames(ccc19x)[colnames(ccc19x) %in% c(paste(temp, '___unk',sep = ''),
                                                     paste(temp, '___99', sep = ''),
                                                     paste(temp, '___la4489_6', sep = ''),
                                                     paste(temp, '___1506_98', sep = ''),
                                                     paste(temp, '___1502_99', sep = ''),
                                                     paste(temp, '___1504_99', sep = ''))]
      
      ccc19x$unknown <- 0
      for(i in which(ccc19x$redcap_repeat_instrument == ''))
      {
        ccc19x$unknown[i] <- length(which(ccc19x[i,u1] %in% c('UNK', '99', '261665006')))
        ccc19x$unknown[i] <- ccc19x$unknown[i] + length(which(ccc19x[i,u2] == 1))
      }
      
      ccc19x$meta_quality[which(ccc19x$unknown >= 20)] <- ccc19x$meta_quality[which(ccc19x$unknown >= 20)] + 5
      ccc19x$meta_problems[which(ccc19x$unknown >= 20)] <- paste(ccc19x$meta_problems[which(ccc19x$unknown >= 20)],
                                                                 '; Large number of unknowns', sep = '')
      
      quality_report[nrow(quality_report)+1,] <- c('Major criteria (5 points)',
                                                   'Acceptable level of unknowns',
                                                   paste(format(length(which(ccc19x$unknown < 20 & ccc19x$redcap_repeat_instrument == '')), big.mark = ', '),
                                                         ' of ',
                                                         format(n, big.mark = ', '),
                                                         ' (', 
                                                         round(100*length(which(ccc19x$unknown < 20 & ccc19x$redcap_repeat_instrument == ''))/n, digits = 2),
                                                         '%)', sep = ''))
      
      ##################
      #Moderate problems
      ##################
      
      #Cancer status missing
      temp.ref <- which(is.na(ccc19x$der_cancer_status) &
                          ccc19x$redcap_repeat_instrument == '')
      ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 3
      ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                              '; Cancer status missing', sep = '')
      
      quality_report[nrow(quality_report)+1,] <- c('Moderate criteria (3 points)',
                                                   'Cancer status not missing',
                                                   paste(format(length(which(!is.na(ccc19x$der_cancer_status) & ccc19x$redcap_repeat_instrument == '')), big.mark = ', '),
                                                         ' of ',
                                                         format(n, big.mark = ', '),
                                                         ' (', 
                                                         round(100*length(which(!is.na(ccc19x$der_cancer_status) & ccc19x$redcap_repeat_instrument == ''))/n, digits = 2),
                                                         '%)', sep = ''))
      
      #ECOG performance status missing
      temp.ref <- which(is.na(ccc19x$der_ecogcat) &
                          ccc19x$redcap_repeat_instrument == '')
      ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 3
      ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                              '; ECOG PS missing', sep = '')
      
      quality_report[nrow(quality_report)+1,] <- c('Moderate criteria (3 points)',
                                                   'ECOG performance status not missing',
                                                   paste(format(length(which(!is.na(ccc19x$der_ecogcat) & ccc19x$redcap_repeat_instrument == '')), big.mark = ', '),
                                                         ' of ',
                                                         format(n, big.mark = ', '),
                                                         ' (', 
                                                         round(100*length(which(!is.na(ccc19x$der_ecogcat) & ccc19x$redcap_repeat_instrument == ''))/n, digits = 2),
                                                         '%)', sep = ''))
      
      #Death status missing/unk
      temp.ref <- which((ccc19x$der_deadbinary == 99|is.na(ccc19x$der_deadbinary)) &
                          ccc19x$redcap_repeat_instrument == '')
      ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 3
      ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                              '; Death status missing or unknown', sep = '')
      
      quality_report[nrow(quality_report)+1,] <- c('Moderate criteria (3 points)',
                                                   'Death status not missing or unknown',
                                                   paste(format(length(which(ccc19x$der_deadbinary %in% 0:1 & ccc19x$redcap_repeat_instrument == '')), big.mark = ', '),
                                                         ' of ',
                                                         format(n, big.mark = ', '),
                                                         ' (', 
                                                         round(100*length(which(ccc19x$der_deadbinary %in% 0:1 & ccc19x$redcap_repeat_instrument == ''))/n, digits = 2),
                                                         '%)', sep = ''))
      
      #Baseline COVID-19 severity missing/unk
      temp.ref <- which((ccc19x$severity_of_covid_19_v2 == 99|is.na(ccc19x$severity_of_covid_19_v2)) &
                          ccc19x$redcap_repeat_instrument == '')
      ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 3
      ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                              '; Baseline COVID-19 severity missing or unknown', sep = '')
      
      quality_report[nrow(quality_report)+1,] <- c('Moderate criteria (3 points)',
                                                   'Baseline COVID-19 severity not missing or unknown',
                                                   paste(format(length(which(ccc19x$severity_of_covid_19_v2 %in% 1:3 & ccc19x$redcap_repeat_instrument == '')), big.mark = ', '),
                                                         ' of ',
                                                         format(n, big.mark = ', '),
                                                         ' (', 
                                                         round(100*length(which(ccc19x$severity_of_covid_19_v2 %in% 1:3 & ccc19x$redcap_repeat_instrument == ''))/n, digits = 2),
                                                         '%)', sep = ''))
      
      #30-day f/u is 60+ days overdue (if applicable and not superseded by 90-day f/u)
      temp.diff <- difftime(Sys.time(), ccc19x$meta_lefttime_ub, units = 'days')
      temp <- as.numeric(temp.diff)
      
      temp.ref <- which(temp >= 90 & ccc19x$der_days_fu < 30 & ccc19x$der_30d_complete == 'No')
      ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 3
      ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                              '; 30-day f/u is at least 60 days overdue', sep = '')
      
      quality_report[nrow(quality_report)+1,] <- c('Moderate criteria (3 points)',
                                                   '30-day f/u is at least 60 days overdue',
                                                   paste(format(length(temp.ref), big.mark = ', '),
                                                         ' of ',
                                                         format(n, big.mark = ', '),
                                                         ' (', 
                                                         round(100*length(temp.ref)/n, digits = 2),
                                                         '%)', sep = ''))
      
      #90-day f/u is 60+ days overdue (if applicable)
      temp.diff <- difftime(Sys.time(), ccc19x$meta_lefttime_ub, units = 'days')
      temp <- as.numeric(temp.diff)
      
      temp.ref <- which(temp >= 150 & ccc19x$der_days_fu < 90 & ccc19x$der_90d_complete == 'No')
      ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 0 #No penalty, yet
      ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                              '; 90-day f/u is at least 60 days overdue', sep = '')
      
      quality_report[nrow(quality_report)+1,] <- c('Moderate criteria (unscored)',
                                                   '90-day f/u is at least 60 days overdue',
                                                   paste(format(length(temp.ref), big.mark = ', '),
                                                         ' of ',
                                                         format(n, big.mark = ', '),
                                                         ' (', 
                                                         round(100*length(temp.ref)/n, digits = 2),
                                                         '%)', sep = ''))
      
      #180-day f/u is 60+ days overdue (if applicable)
      temp.diff <- difftime(Sys.time(), ccc19x$meta_lefttime_ub, units = 'days')
      temp <- as.numeric(temp.diff)
      
      temp.ref <- which(temp >= 240 & ccc19x$der_days_fu < 180 & ccc19x$der_180d_complete == 'No')
      ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 0 #No penalty, yet
      ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                              '; 180-day f/u is at least 60 days overdue', sep = '')
      
      quality_report[nrow(quality_report)+1,] <- c('Moderate criteria (unscored)',
                                                   '180-day f/u is at least 60 days overdue',
                                                   paste(format(length(temp.ref), big.mark = ', '),
                                                         ' of ',
                                                         format(n, big.mark = ', '),
                                                         ' (', 
                                                         round(100*length(temp.ref)/n, digits = 2),
                                                         '%)', sep = ''))
      
      ###############
      #Minor problems
      ###############
      
      #ADT (prostate)
      temp.ref <- which((ccc19x$adt == 99|is.na(ccc19x$adt)) &
                          !ccc19x$hx_treatment %in% c(3,88) &
                          ccc19x$der_Prostate == 1)
      ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 1
      ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                              '; ADT missing or unknown', sep = '')
      
      np <- length(which(ccc19x$der_Prostate == 1))
      
      quality_report[nrow(quality_report)+1,] <- c('Minor criteria (prostate cancer only)',
                                                   'Recent ADT exposure missing or unknown',
                                                   paste(format(length(temp.ref), big.mark = ', '),
                                                         ' of ',
                                                         format(np, big.mark = ', '),
                                                         ' (', 
                                                         round(100*length(temp.ref)/np, digits = 2),
                                                         '%)', sep = ''))
      
      #Biomarkers (breast)
      temp.ref <- which(((ccc19x$breast_biomarkers___er == 0 & ccc19x$breast_biomarkers___her2 == 0 &
                            ccc19x$breast_biomarkers___tnbc == 0) | (ccc19x$breast_biomarkers___99 == 1)) &
                          ccc19x$der_Breast == 1)
      ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 1
      ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                              '; Breast cancer biomarkers missing or unknown', sep = '')
      
      nb <- length(which(ccc19x$der_Breast == 1))
      
      quality_report[nrow(quality_report)+1,] <- c('Minor criteria (breast cancer only)',
                                                   'Breast cancer biomarkers missing or unknown',
                                                   paste(format(length(temp.ref), big.mark = ', '),
                                                         ' of ',
                                                         format(nb, big.mark = ', '),
                                                         ' (', 
                                                         round(100*length(temp.ref)/nb, digits = 2),
                                                         '%)', sep = ''))
      
      #BCG (bladder) - DEPRECATED
      # temp.ref <- which((ccc19x$bcg_intraves_ever == 99|is.na(ccc19x$bcg_intraves_ever)) &
      #                     (ccc19x$cancer_type == 'C4912'|
      #                        ccc19x$cancer_type_2 == 'C4912'|
      #                        ccc19x$cancer_type_3 == 'C4912'|
      #                        ccc19x$cancer_type_4 == 'C4912'|
      #                        ccc19x$cancer_type_5 == 'C4912'))
      # ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 1
      # ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
      #                                        '; Intravesicular BCG missing or unknown', sep = '')
      
      #Cancer status unknown
      temp.ref <- which(ccc19x$der_cancer_status == 'Unknown' &
                          ccc19x$redcap_repeat_instrument == '')
      ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 1
      ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                              '; Cancer status unknown', sep = '')
      
      n1 <- length(which(ccc19x$cancer_status %in% 1:99 & ccc19x$redcap_repeat_instrument == ''))
      
      quality_report[nrow(quality_report)+1,] <- c('Minor criteria (1 point)',
                                                   'Baseline cancer status non-missing with a known value',
                                                   paste(format(length(which(ccc19x$cancer_status %in% 1:5 & ccc19x$redcap_repeat_instrument == '')), big.mark = ', '),
                                                         ' of ',
                                                         format(n1, big.mark = ', '),
                                                         ' (', 
                                                         round(100*length(which(ccc19x$cancer_status %in% 1:5 & ccc19x$redcap_repeat_instrument == ''))/n1, digits = 2),
                                                         '%)', sep = ''))
      
      #Mets status missing or unknown, unless patient has stage IV/disseminated cancer with active disease
      temp.ref <- which((ccc19x$mets_yn == 99|is.na(ccc19x$mets_yn)) &
                          ccc19x$cancer_status != '1' &
                          !(ccc19x$cancer_status %in% 2:5 & ccc19x$stage %in% c('4', '764-7')) &
                          ccc19x$redcap_repeat_instrument == '')
      ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 1
      ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                              '; Metastatic status missing or unknown', sep = '')
      
      quality_report[nrow(quality_report)+1,] <- c('Minor criteria (1 point)',
                                                   'Metastatic status with a known value',
                                                   paste(format(n - length(temp.ref), big.mark = ', '),
                                                         ' of ',
                                                         format(n, big.mark = ', '),
                                                         ' (', 
                                                         round(100*(n - length(temp.ref))/n, digits = 2),
                                                         '%)', sep = ''))
      
      #ECOG performance status unknown
      temp.ref <- which(ccc19x$der_ecogcat == 'Unknown' &
                          ccc19x$redcap_repeat_instrument == '')
      ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 1
      
      temp.ref <- which(ccc19x$ecog_status == 99 &
                          ccc19x$redcap_repeat_instrument == '')
      ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                              '; ECOG PS unknown', sep = '')
      
      n1 <- length(which(ccc19x$ecog_status %in% 0:99 & ccc19x$redcap_repeat_instrument == ''))
      
      quality_report[nrow(quality_report)+1,] <- c('Minor criteria (1 point)',
                                                   'Baseline ECOG performance status non-missing with a known value, including not documented in the 3 months preceding COVID-19 diagnosis',
                                                   paste(format(length(which(ccc19x$ecog_status %in% 0:88 & ccc19x$redcap_repeat_instrument == '')), big.mark = ', '),
                                                         ' of ',
                                                         format(n1, big.mark = ', '),
                                                         ' (', 
                                                         round(100*length(which(ccc19x$ecog_status %in% 0:88 & ccc19x$redcap_repeat_instrument == ''))/n1, digits = 2),
                                                         '%)', sep = ''))
      
      #ICU status missing/unk
      temp.ref <- which((ccc19x$der_ICU == 99|is.na(ccc19x$der_ICU)) &
                          ccc19x$redcap_repeat_instrument == '')
      ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 1
      ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                              '; ICU status missing or unknown', sep = '')
      
      quality_report[nrow(quality_report)+1,] <- c('Minor criteria (1 point)',
                                                   'ICU status with a known value',
                                                   paste(format(n - length(temp.ref), big.mark = ', '),
                                                         ' of ',
                                                         format(n, big.mark = ', '),
                                                         ' (', 
                                                         round(100*(n - length(temp.ref))/n, digits = 2),
                                                         '%)', sep = ''))
      
      #Hospital status missing/unk
      temp.ref <- which((ccc19x$der_hosp == 99|is.na(ccc19x$der_hosp)) &
                          ccc19x$redcap_repeat_instrument == '')
      ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 1
      ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                              '; Hospital status missing or unknown', sep = '')
      
      quality_report[nrow(quality_report)+1,] <- c('Minor criteria (1 point)',
                                                   'Hospital status with a known value',
                                                   paste(format(n - length(temp.ref), big.mark = ', '),
                                                         ' of ',
                                                         format(n, big.mark = ', '),
                                                         ' (', 
                                                         round(100*(n - length(temp.ref))/n, digits = 2),
                                                         '%)', sep = ''))
      
      #Intubation status missing/unk
      temp.ref <- which((ccc19x$der_mv == 99|is.na(ccc19x$der_mv)) &
                          ccc19x$redcap_repeat_instrument == '')
      ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 1
      ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                              '; Intubation status missing or unknown', sep = '')
      
      quality_report[nrow(quality_report)+1,] <- c('Minor criteria (1 point)',
                                                   'Intubation status with a known value',
                                                   paste(format(n - length(temp.ref), big.mark = ', '),
                                                         ' of ',
                                                         format(n, big.mark = ', '),
                                                         ' (', 
                                                         round(100*(n - length(temp.ref))/n, digits = 2),
                                                         '%)', sep = ''))
      
      #O2 status missing/unk
      temp.ref <- which((ccc19x$der_o2_ever == 99|is.na(ccc19x$der_o2_ever)) &
                          ccc19x$redcap_repeat_instrument == '')
      ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 1
      ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                              '; O2 requirement missing or unknown', sep = '')
      
      quality_report[nrow(quality_report)+1,] <- c('Minor criteria (1 point)',
                                                   'Supplemental oxygen status with a known value',
                                                   paste(format(n - length(temp.ref), big.mark = ', '),
                                                         ' of ',
                                                         format(n, big.mark = ', '),
                                                         ' (', 
                                                         round(100*(n - length(temp.ref))/n, digits = 2),
                                                         '%)', sep = ''))
      
      #Days to death missing or 9999
      temp.ref <- which(ccc19x$der_deadbinary == 1 & 
                          (ccc19x$der_days_to_death_combined == 9999|is.na(ccc19x$der_days_to_death_combined)) &
                          ccc19x$redcap_repeat_instrument == '')
      ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 1
      ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                              '; Days to death missing or unknown', sep = '')
      
      n1 <- length(which(ccc19x$der_deadbinary == 1 & ccc19x$redcap_repeat_instrument == ''))
      
      quality_report[nrow(quality_report)+1,] <- c('Minor criteria (1 point)',
                                                   'Days to death non-missing with a known value',
                                                   paste(format(n1 - length(temp.ref), big.mark = ', '),
                                                         ' of ',
                                                         format(n1, big.mark = ', '),
                                                         ' (', 
                                                         round(100*(n1 - length(temp.ref))/n1, digits = 2),
                                                         '%)', sep = ''))
      
      #30-day f/u is 30+ days overdue (if applicable and not superseded by 90-day f/u)
      temp.diff <- difftime(Sys.time(), ccc19x$meta_lefttime_ub, units = 'days')
      temp <- as.numeric(temp.diff)
      
      temp.ref <- which(temp >= 60 & temp < 90 & ccc19x$der_days_fu < 30 & ccc19x$der_30d_complete == 'No')
      ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 1
      ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                              '; 30-day f/u is 30-59 days overdue', sep = '')
      
      quality_report[nrow(quality_report)+1,] <- c('Minor criteria (1 point)',
                                                   '30-day f/u is 30-59 days overdue',
                                                   paste(format(length(temp.ref), big.mark = ', '),
                                                         ' of ',
                                                         format(n, big.mark = ', '),
                                                         ' (', 
                                                         round(100*length(temp.ref)/n, digits = 2),
                                                         '%)', sep = ''))
      
      #90-day f/u is 30+ days overdue (if applicable)
      temp.diff <- difftime(Sys.time(), ccc19x$meta_lefttime_ub, units = 'days')
      temp <- as.numeric(temp.diff)
      
      temp.ref <- which(temp >= 120 & temp < 150 & ccc19x$der_days_fu < 90 & ccc19x$der_90d_complete == 'No')
      ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 0 #No penalty, yet
      ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                              '; 90-day f/u is 30-59 days overdue', sep = '')
      
      quality_report[nrow(quality_report)+1,] <- c('Minor criteria (unscored)',
                                                   '90-day f/u is 30-59 days overdue',
                                                   paste(format(length(temp.ref), big.mark = ', '),
                                                         ' of ',
                                                         format(n, big.mark = ', '),
                                                         ' (', 
                                                         round(100*length(temp.ref)/n, digits = 2),
                                                         '%)', sep = ''))
      
      #180-day f/u is 30+ days overdue (if applicable)
      temp.diff <- difftime(Sys.time(), ccc19x$meta_lefttime_ub, units = 'days')
      temp <- as.numeric(temp.diff)
      
      temp.ref <- which(temp >= 210 & temp < 240 & ccc19x$der_days_fu < 180 & ccc19x$der_180d_complete == 'No')
      ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 0 #No penalty, yet
      ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                              '; 180-day f/u is 30-59 days overdue', sep = '')
      
      quality_report[nrow(quality_report)+1,] <- c('Minor criteria (unscored)',
                                                   '180-day f/u is 30-59 days overdue',
                                                   paste(format(length(temp.ref), big.mark = ', '),
                                                         ' of ',
                                                         format(n, big.mark = ', '),
                                                         ' (', 
                                                         round(100*length(temp.ref)/n, digits = 2),
                                                         '%)', sep = ''))
      
      #Remove leading semicolon
      ccc19x$meta_problems <- gsub(ccc19x$meta_problems, pattern = '^; ', replacement = '')
    }
    
    write.csv(quality_report, file = paste('QA/', Sys.Date(), ' overall quality report.csv',sep=''), row.names = F)
    
    #######################
    #X07. Breast biomarkers
    #######################
    {
      ccc19x$der_breast_biomarkers <- NA
      
      #HR+, HER2-
      ccc19x$der_breast_biomarkers[which(ccc19x$breast_biomarkers___er == 1 & ccc19x$breast_biomarkers___her2 == 0)] <- 1
      
      #HR+, HER2+
      ccc19x$der_breast_biomarkers[which(ccc19x$breast_biomarkers___er == 1 & ccc19x$breast_biomarkers___her2 == 1)] <- 2
      
      #HR-, HER2+
      ccc19x$der_breast_biomarkers[which(ccc19x$breast_biomarkers___er == 0 & ccc19x$breast_biomarkers___her2 == 1)] <- 3
      
      #Triple negative
      ccc19x$der_breast_biomarkers[which(ccc19x$breast_biomarkers___tnbc == 1)] <- 4
      
      #Unknown
      ccc19x$der_breast_biomarkers[which(ccc19x$breast_biomarkers___99 == 1 & is.na(ccc19x$der_breast_biomarkers))] <- 99
      
      ccc19x$der_breast_biomarkers <- factor(ccc19x$der_breast_biomarkers)
      
      temp <- summary(ccc19x$der_breast_biomarkers[which(ccc19x$redcap_repeat_instrument == '' & ccc19x$der_Breast == 1)])
      temp.var.log <- data.frame(name = 'der_breast_biomarkers',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
    }
    
    #############
    #X08. Gleason
    #############
    ccc19x$der_gleason <- ccc19x$gleason
    ccc19x$der_gleason[which(ccc19x$der_gleason %in% c('X7', 'X8', 'X9'))] <- 'Unknown'
    ccc19x$der_gleason[ccc19x$der_gleason == ''] <- NA
    ccc19x$der_gleason <- factor(ccc19x$der_gleason)
    summary(ccc19x$der_gleason[ccc19x$der_Prostate == 1])
    
    #X08a. Gleason with grouped categories
    ccc19x$der_gleason_v2 <- as.character(ccc19x$der_gleason)
    ccc19x$der_gleason_v2[which(ccc19x$der_gleason_v2 %in% c('03', '04', '05'))] <- 'Less than 6'
    ccc19x$der_gleason_v2 <- factor(ccc19x$der_gleason_v2)
    summary(ccc19x$der_gleason_v2[ccc19x$der_Prostate == 1])
    
    ####################
    #X09. Cytokine storm
    ####################
    {
      ccc19x$der_cytokine_storm <- NA
      
      temp.ref <- which(ccc19x$redcap_repeat_instrument == '')
      
      #Yes
      
      #At least one marker of inflammation (IL-6, CRP, or D-dimer) is abnormal
      x1 <- rep(F, length(temp.ref))
      x1[which(ccc19x$der_il6[temp.ref] == 'Abnormal'|ccc19x$der_crp[temp.ref] == 'Abnormal'|ccc19x$der_ddimer[temp.ref] == 'Abnormal')] <- T
      
      #At least one of hypotension/sepsis/pressors or pneumonitis/ARDS
      x2 <- rep(F, length(temp.ref))
      x2[which(ccc19x$der_hotn_comp[temp.ref] == 1|ccc19x$der_sepsis_comp[temp.ref] == 1|ccc19x$der_sepsis_comp_v2[temp.ref] == 1|
                 ccc19x$der_ARDS_comp[temp.ref] == 1|ccc19x$der_pneumonitis_comp[temp.ref] == 1)] <- T
      
      #At least one of fever, abnormal AST or ALT or creatinine
      x3 <- rep(F, length(temp.ref))
      x3[which(ccc19x$symptoms___386661006[temp.ref] == 1|ccc19x$ast[temp.ref] == 1|ccc19x$alt[temp.ref] == 1|ccc19x$der_creat[temp.ref] == 'Abnormal')] <- T
      
      ccc19x$der_cytokine_storm[temp.ref][x1 & x2 & x3] <- 1
      
      #No
      #No marker of inflammation (IL-6, CRP, or D-dimer) is abnormal
      x1 <- rep(F, length(temp.ref))
      x1[which(ccc19x$der_il6[temp.ref] != 'Abnormal' & ccc19x$der_crp[temp.ref] != 'Abnormal' & ccc19x$der_ddimer[temp.ref] != 'Abnormal')] <- T
      
      #No hypotension/sepsis/pressors or pneumonitis/ARDS
      x2 <- rep(F, length(temp.ref))
      x2[which(ccc19x$der_hotn_comp[temp.ref] == 0 & ccc19x$der_sepsis_comp[temp.ref] == 0 & ccc19x$der_sepsis_comp_v2[temp.ref] == 0 &
                 ccc19x$der_ARDS_comp[temp.ref] == 0 & ccc19x$der_pneumonitis_comp[temp.ref] == 0)] <- T
      
      #No fever, abnormal AST or ALT or creatinine
      x3 <- rep(F, length(temp.ref))
      x3[which(ccc19x$symptoms___386661006[temp.ref] == 0 & ccc19x$ast[temp.ref] != 1 & ccc19x$alt[temp.ref] != 1 & ccc19x$der_creat[temp.ref] != 'Abnormal')] <- T
      
      ccc19x$der_cytokine_storm[temp.ref][x1 & x2 & x3] <- 0
      
      #Unknown
      #At least one marker of inflammation (IL-6, CRP, or D-dimer) is unknown
      x1 <- rep(F, length(temp.ref))
      x1[which((ccc19x$der_il6[temp.ref] == 'Unknown'|ccc19x$der_crp[temp.ref] == 'Unknown'|ccc19x$der_ddimer[temp.ref] == 'Unknown') &
                 is.na(ccc19x$der_cytokine_storm[temp.ref]))] <- T
      
      #At least one of hypotension/sepsis/pressors or pneumonitis/ARDS is unknown
      x2 <- rep(F, length(temp.ref))
      x2[which((ccc19x$der_hotn_comp[temp.ref] == 99|ccc19x$der_sepsis_comp[temp.ref] == 99|ccc19x$der_sepsis_comp_v2[temp.ref] == 99|
                  ccc19x$der_ARDS_comp[temp.ref] == 99|ccc19x$der_pneumonitis_comp[temp.ref] == 99) &
                 is.na(ccc19x$der_cytokine_storm[temp.ref]))] <- T
      
      #At least one of unknown AST or ALT or creatinine
      x3 <- rep(F, length(temp.ref))
      x3[which((ccc19x$ast[temp.ref] == 99|ccc19x$alt[temp.ref] == 99|ccc19x$der_creat[temp.ref] == 'Unknown') &
                 is.na(ccc19x$der_cytokine_storm[temp.ref]))] <- T
      
      ccc19x$der_cytokine_storm[temp.ref][x1|x2|x3] <- 99
      
      ccc19x$der_cytokine_storm <- factor(ccc19x$der_cytokine_storm)
      
      temp <- summary(ccc19x$der_cytokine_storm[ccc19x$redcap_repeat_instrument == ''])
      temp.var.log <- data.frame(name = 'der_cytokine_storm',
                                 timestamp = Sys.time(),
                                 values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                                 stringsAsFactors = F)
      var.log <- rbind(var.log, temp.var.log)
    }
    
    #####################################
    #X10. Lower respiratory tract disease
    #####################################
    {
      ccc19x$der_lrtd <- NA
      
      #Yes
      ccc19x$der_lrtd[which(ccc19x$der_mv == 1)] <- 1 #Intubated
      
      #Baseline
      ccc19x$der_lrtd[which(ccc19x$resp_failure_tx %in% 2:6 | 
                              ccc19x$c19_complications_pulm___205237003 == 1 | 
                              ccc19x$c19_complications_pulm___233604007 == 1 |
                              ccc19x$c19_complications_pulm___67782005 == 1)] <- 1
      
      #Follow-up
      ccc19x$der_lrtd[which(ccc19x$resp_failure_tx_fu %in% 2:6 | 
                              ccc19x$who_ordinal_scale %in% 5:7 | 
                              ccc19x$c19_complications_pulm_fu___205237003 == 1 | 
                              ccc19x$c19_complications_pulm_fu___233604007 == 1 |
                              ccc19x$c19_complications_pulm_fu___67782005 == 1)] <- 1
      
      #No
      
      #Baseline
      ccc19x$der_lrtd[which((ccc19x$o2_requirement_c19 == 0 |
                               ccc19x$resp_failure_tx == 1 |
                               ccc19x$c19_complications_pulm___none == 1) &
                              is.na(ccc19x$der_lrtd))] <- 0
      
      #Follow-up
      ccc19x$der_lrtd[which((ccc19x$o2_requirement_fu == 0 |
                               ccc19x$resp_failure_tx_fu == 1|
                               ccc19x$c19_complications_pulm_fu___none == 1|
                               ccc19x$who_ordinal_scale %in% c(1,3,4)) &
                              is.na(ccc19x$der_lrtd))] <- 0
      
      #Unknown
      
      #Baseline
      ccc19x$der_lrtd[which((ccc19x$o2_requirement_c19 == 99 |
                               ccc19x$resp_failure_tx == 99 |
                               ccc19x$c19_complications_pulm___unk == 1) &
                              is.na(ccc19x$der_lrtd))] <- 99
      
      #Followup
      ccc19x$der_lrtd[which((ccc19x$o2_requirement_fu == 99 |
                               ccc19x$resp_failure_tx_fu == 99 |
                               ccc19x$c19_complications_pulm_fu___unk == 1) &
                              is.na(ccc19x$der_lrtd))] <- 99
      
      #Merge baseline and followup if discrepancy
      for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
      {
        temp.ref <- which(ccc19x$record_id == i)
        temp <- ccc19x$der_lrtd[temp.ref]
        temp2 <- ccc19x$der_lrtd[temp.ref][2:length(temp.ref)]
        temp2 <- temp2[!is.na(temp2)]
        if(length(temp[!is.na(temp)]) > 0)
        {
          if(any(temp[!is.na(temp)] == 1)) ccc19x$der_lrtd[temp.ref] <- 1 else
            if(length(temp[2:length(temp)][!is.na(temp[2:length(temp)])]) > 0)
            {
              if((is.na(temp[1])|temp[1] == 0) & all(temp2 == 0) & !any(temp[!is.na(temp)] == 1)) ccc19x$der_lrtd[temp.ref] <- 0
              if((is.na(temp[1])|temp[1] == 0) & any(temp2 == 99) & !any(temp[!is.na(temp)] == 1)) ccc19x$der_lrtd[temp.ref] <- 99
            }
        }
      }
      
      ccc19x$der_lrtd <- factor(ccc19x$der_lrtd)
      summary(ccc19x$der_lrtd[ccc19x$redcap_repeat_instrument == ''])
    }
    
    ###############
    #X11. PASC (v2)
    ###############
    ccc19x$der_pasc <- NA
    
    #Create a dummy variable that assumes once fully recovered in follow-up, always fully recovered
    t.status <- ccc19x$current_status_v2
    t.status[which(ccc19x$current_status_retro != '')] <- ccc19x$current_status_retro[which(ccc19x$current_status_retro != '')]
    t.status[which(ccc19x$covid_19_status_fu != '')] <- ccc19x$covid_19_status_fu[which(ccc19x$covid_19_status_fu != '')]
    t.status[which(ccc19x$c19_status_fu_final != '')] <- ccc19x$c19_status_fu_final[which(ccc19x$c19_status_fu_final != '')]
    
    #Assume follow-ups are entered in temporal order (check this assumption!)
    pts <- unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instance == 2)])
    for(i in 1:length(pts))
    {
      temp.ref <- which(ccc19x$record_id == pts[i] & ccc19x$redcap_repeat_instrument == 'followup')
      temp <- t.status[temp.ref]
      if(any(temp == '1'))
      {
        temp.ref2 <- min(which(temp == '1'))
        temp[temp.ref2:length(temp)] <- '1'
        t.status[temp.ref] <- temp
      }
    }
    
    #No - patient marked as fully recovered within 90 days
    
    #Baseline
    ccc19x$der_pasc[which(t.status == 1 &
                            ccc19x$covid_19_dx_interval %in% 1:5)] <- 0
    
    #Follow-up
    ccc19x$der_pasc[which(t.status == 1 &
                            (ccc19x$fu_weeks %in% c(30,90)|ccc19x$timing_of_report_weeks <= 13))] <- 0
    
    #Yes - patient marked as having ongoing infection or recovered with complications at 90 days or beyond
    
    #Baseline
    ccc19x$der_pasc[which(t.status %in% c('1b', '2') &
                            ccc19x$covid_19_dx_interval %in% 6:10)] <- 1
    
    #Follow-up, alive
    ccc19x$der_pasc[which(t.status %in% c('1b', '2') &
                            (ccc19x$fu_weeks %in% c(90,180,365)|
                               ccc19x$timing_of_report_weeks >= 13))] <- 1
    
    #For those reported as having died at 90+ days having recovered or having ongoing infection, 
    #additionally require that cause of death is at least partially attributed to COVID-19 or "other"
    ccc19x$der_pasc[which(t.status %in% c('1b', '3') &
                            (ccc19x$cause_of_death_fu %in% c(1,3,88)|ccc19x$cause_of_death_fu_2 %in% c(1,3,88)) &
                            ccc19x$der_days_to_death_combined >= 90)] <- 1
    
    #Reset patients with less than 90 days of follow-up as missing
    ccc19x$der_pasc[which(ccc19x$record_id %in% ccc19x$record_id[which(ccc19x$der_days_fu < 90|
                                                                         is.na(ccc19x$der_days_fu))] &
                            ccc19x$der_pasc == 1)] <- NA
    
    #Unknown
    ccc19x$der_pasc[which(t.status == 99 & is.na(ccc19x$der_pasc))] <- 99
    
    #For those reported as having died at 90+ days having recovered or having ongoing infection, 
    #and cause of death is cancer or unknown, assign to unknown (require queries)
    ccc19x$der_pasc[which(t.status %in% c('1b', '3') &
                            (ccc19x$cause_of_death_fu %in% c(2,99)|ccc19x$cause_of_death_fu_2 %in% c(2,99)) &
                            ccc19x$der_days_to_death_combined >= 90)] <- 99
    
    #For those reported as having died at 90+ days having fully recovered, 
    #and cause of death is COVID-19 or both, assign to unknown (require queries)
    ccc19x$der_pasc[which(t.status %in% c('1') &
                            (ccc19x$cause_of_death_fu %in% c(1,3)|ccc19x$cause_of_death_fu_2 %in% c(1,3)) &
                            ccc19x$der_days_to_death_combined >= 90)] <- 99
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_pasc[temp.ref]
      temp2 <- ccc19x$der_pasc[temp.ref][2:length(temp.ref)]
      temp2 <- temp2[!is.na(temp2)]
      if(length(temp[!is.na(temp)]) > 0)
      {
        if(any(temp[!is.na(temp)] == 1)) ccc19x$der_pasc[temp.ref] <- 1 else
          if(length(temp[2:length(temp)][!is.na(temp[2:length(temp)])]) > 0)
          {
            if((is.na(temp[1])|temp[1] == 0) & all(temp2 == 0) & !any(temp[!is.na(temp)] == 1)) ccc19x$der_pasc[temp.ref] <- 0
            if((is.na(temp[1])|temp[1] == 0) & any(temp2 == 99) & !any(temp[!is.na(temp)] == 1)) ccc19x$der_pasc[temp.ref] <- 99
          }
      }
    }
    
    ccc19x$der_pasc <- factor(ccc19x$der_pasc)
    
    temp <- summary(ccc19x$der_pasc[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_pasc',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    # #X12a. COVID-19 vaccination with temporal collapse -- needs updating
    # ccc19x$der_vax_collapsed <- NA
    # 
    # #At least one dose prior to COVID-19
    # ccc19x$der_vax_collapsed[which(ccc19x$der_vax %in% c('Partially vaccinated', 'Fully vaccinated'))] <- 1
    # 
    # #Unvaccinated or vaccinated after COVID-19
    # ccc19x$der_vax_collapsed[which(ccc19x$der_vax %in% c('Unvaccinated', 'After COVID-19'))] <- 0
    # 
    # #Unknown
    # ccc19x$der_vax_collapsed[which(ccc19x$der_vax == 'Unknown')] <- 99
    # 
    # ccc19x$der_vax_collapsed <- factor(ccc19x$der_vax_collapsed)
    # 
    # temp <- summary(ccc19x$der_vax_collapsed[ccc19x$redcap_repeat_instrument == ''])
    # temp.var.log <- data.frame(name = 'der_vax_collapsed',
    #                            timestamp = Sys.time(),
    #                            values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
    #                            stringsAsFactors = F)
    # var.log <- rbind(var.log, temp.var.log)
    
    #X12b. Simple derived variable counting doses of COVID-19 vaccine
    ccc19x$der_vax_count <- NA
    
    #No doses
    temp.ref <- which(ccc19x$sars_vax == 0|ccc19x$sars_vax_when == 88)
    ccc19x$der_vax_count[temp.ref] <- '0 doses'
    
    #One dose non-mrna
    temp.ref <- which(ccc19x$sars_vax_which %in% c('1a','4') & is.na(ccc19x$der_vax_count))
    ccc19x$der_vax_count[temp.ref] <- '1 non-mrna dose'
    
    #One dose mrna
    temp.ref <- which(ccc19x$sars_vax_which %in% c('2a','3a','5a') & is.na(ccc19x$der_vax_count))
    ccc19x$der_vax_count[temp.ref] <- '1 mrna dose'
    
    #Two or more doses non-mrna
    temp.ref <- which(ccc19x$sars_vax_which %in% c('1b','4b') & is.na(ccc19x$der_vax_count))
    ccc19x$der_vax_count[temp.ref] <- '2+ non-mrna doses'
    
    #Two dose mrna
    temp.ref <- which(ccc19x$sars_vax_which %in% c('2b','3b','5b') & is.na(ccc19x$der_vax_count))
    ccc19x$der_vax_count[temp.ref] <- '2 mrna doses'
    
    #Three or more doses mrna
    temp.ref <- which(ccc19x$sars_vax_which %in% c('2c','3c','5c') & is.na(ccc19x$der_vax_count))
    ccc19x$der_vax_count[temp.ref] <- '3+ mrna doses'
    
    #Others
    temp.ref <- which(ccc19x$sars_vax_which %in% c(88) & is.na(ccc19x$der_vax_count))
    ccc19x$der_vax_count[temp.ref] <- 'Other'
    
    #Unknown
    temp.ref <- which((ccc19x$sars_vax == 99|
                         ccc19x$sars_vax_which == 99|
                         ccc19x$sars_vax_when == 99) & is.na(ccc19x$der_vax_count))
    ccc19x$der_vax_count[temp.ref] <- 'Unknown (dose and/or timing)'
    
    ccc19x$der_vax_count <- factor(ccc19x$der_vax_count)
    
    temp <- summary(ccc19x$der_vax_count[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_vax_count',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #X12c. Derived variable counting doses of COVID-19 vaccine before COVID-19
    ccc19x$der_vax_count_before <- NA
    
    #No doses before
    temp.ref <- which(ccc19x$sars_vax == 0|ccc19x$sars_vax_when == 88|ccc19x$sars_vax_before_num == 0)
    ccc19x$der_vax_count_before[temp.ref] <- '0 doses before'
    
    #One dose non-mrna
    
    #Definite
    temp.ref <- which(ccc19x$sars_vax_which %in% c('1a','4') & 
                        (ccc19x$sars_vax_when %in% 1:4|ccc19x$sars_vax_before_num == 1) &
                        is.na(ccc19x$der_vax_count_before))
    ccc19x$der_vax_count_before[temp.ref] <- '1 non-mrna dose before, definite'
    
    #Unknown
    temp.ref <- which(ccc19x$sars_vax_which %in% c('1a','4') & ccc19x$sars_vax_when %in% 1:4 &
                        is.na(ccc19x$der_vax_count_before))
    ccc19x$der_vax_count_before[temp.ref] <- '1 non-mrna dose before, unknown'
    
    #One dose mrna
    
    #Definite
    temp.ref <- which(((ccc19x$sars_vax_which %in% c('2a','3a','5a') & 
                          ccc19x$sars_vax_when %in% 1:4) |
                         (ccc19x$sars_vax_which %in% c('2a','3a','5a','2b','3b','5b','2c','3c','5c') & 
                            ccc19x$sars_vax_before_num == 1)) &
                        is.na(ccc19x$der_vax_count_before))
    ccc19x$der_vax_count_before[temp.ref] <- '1 mrna dose before, definite'
    
    #Unknown
    temp.ref <- which(ccc19x$sars_vax_which %in% c('2a','3a','5a') & ccc19x$sars_vax_when %in% 1:4 &
                        is.na(ccc19x$der_vax_count_before))
    ccc19x$der_vax_count_before[temp.ref] <- '1 mrna dose before, unknown'
    
    #Two or more doses non-mrna
    
    #Definite
    temp.ref <- which(ccc19x$sars_vax_which %in% c('1b','4b') & 
                        (ccc19x$sars_vax_before_num == 2|ccc19x$sars_vax_before == 1) &
                        is.na(ccc19x$der_vax_count_before))
    ccc19x$der_vax_count_before[temp.ref] <- '2+ non-mrna doses before, definite'
    
    #Probable
    temp.ref <- which(ccc19x$sars_vax_which %in% c('1b','4b') & 
                        (ccc19x$covid_19_dx_interval %in% 1:4|ccc19x$sars_vax_when_exact >= 30) &
                        is.na(ccc19x$der_vax_count_before))
    ccc19x$der_vax_count_before[temp.ref] <- '2+ non-mrna doses before, probable'
    
    #Possible
    temp.ref <- which(ccc19x$sars_vax_which %in% c('1b','4b') & ccc19x$covid_19_dx_interval %in% 5:99 &
                        is.na(ccc19x$der_vax_count_before))
    ccc19x$der_vax_count_before[temp.ref] <- '2+ non-mrna doses before, possible'
    
    #Two dose mrna
    #Definite
    temp.ref <- which(((ccc19x$sars_vax_which %in% c('2b','3b','5b') & 
                          ccc19x$sars_vax_before == 1) |
                         (ccc19x$sars_vax_which %in% c('2b','3b','5b','2c','3c','5c') & 
                            ccc19x$sars_vax_before_num == 2)) &
                        is.na(ccc19x$der_vax_count_before))
    ccc19x$der_vax_count_before[temp.ref] <- '2 mrna doses before, definite'
    
    #Probable
    temp.ref <- which(ccc19x$sars_vax_which %in% c('2b','3b','5b') & 
                        (ccc19x$covid_19_dx_interval %in% 1:4|ccc19x$sars_vax_when_exact >= 30) &
                        is.na(ccc19x$der_vax_count_before))
    ccc19x$der_vax_count_before[temp.ref] <- '2 mrna doses before, probable'
    
    #Possible
    temp.ref <- which(ccc19x$sars_vax_which %in% c('2b','3b','5b') & ccc19x$covid_19_dx_interval %in% 5:99 &
                        is.na(ccc19x$der_vax_count_before))
    ccc19x$der_vax_count_before[temp.ref] <- '2 mrna doses before, possible'
    
    #Three or more doses mrna
    #Definite
    temp.ref <- which(ccc19x$sars_vax_which %in% c('2c','3c','5c') & 
                        (ccc19x$sars_vax_before_num >= 3|ccc19x$sars_vax_before == 1) &
                        is.na(ccc19x$der_vax_count_before))
    ccc19x$der_vax_count_before[temp.ref] <- '3+ mrna doses before, definite'
    
    #Probable
    temp.ref <- which(ccc19x$sars_vax_which %in% c('2c','3c','5c') & 
                        (ccc19x$covid_19_dx_interval %in% 1:4|ccc19x$sars_vax_when_exact >= 180) &
                        is.na(ccc19x$der_vax_count_before))
    ccc19x$der_vax_count_before[temp.ref] <- '3+ mrna doses before, probable'
    
    #Possible
    temp.ref <- which(ccc19x$sars_vax_which %in% c('2c','3c','5c') & ccc19x$covid_19_dx_interval %in% 5:99 &
                        is.na(ccc19x$der_vax_count_before))
    ccc19x$der_vax_count_before[temp.ref] <- '3+ mrna doses before, possible'
    
    #Others
    temp.ref <- which(ccc19x$sars_vax_which %in% c(88) & is.na(ccc19x$der_vax_count_before))
    ccc19x$der_vax_count_before[temp.ref] <- 'Other'
    
    #Unknown
    temp.ref <- which((ccc19x$sars_vax == 99|
                         ccc19x$sars_vax_which == 99|
                         ccc19x$sars_vax_when == 99) & is.na(ccc19x$der_vax_count_before))
    ccc19x$der_vax_count_before[temp.ref] <- 'Unknown (dose and/or timing)'
    
    ccc19x$der_vax_count_before <- factor(ccc19x$der_vax_count_before)
    
    temp <- summary(ccc19x$der_vax_count_before[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_vax_count_before',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
    
    #X13. Derived variable whether any COVID-19 vaccine before COVID-19
    ccc19x$der_vax_before <- NA
    
    #Yes, any vaccination received before COVID-19
    ccc19x$der_vax_before[which(ccc19x$sars_vax_before == 1|
                                  ccc19x$sars_vax_before_num > 0|
                                  ccc19x$sars_vax_before_num_2 %in% 1:66|
                                  ccc19x$sars_vax_when %in% 1:4)] <- 1
    
    #No
    ccc19x$der_vax_before[which((ccc19x$sars_vax == 0|
                                   ccc19x$sars_vax_before_num == 0|
                                   ccc19x$sars_vax_before_num_2 == 0|
                                   ccc19x$sars_vax_when == 88) &
                                  is.na(ccc19x$der_vax_before))] <- 0
    
    #Unknown
    ccc19x$der_vax_before[which((ccc19x$sars_vax == 99|
                                   ccc19x$sars_vax_before == 99|
                                   ccc19x$sars_vax_before_num_2 == 99|
                                   ccc19x$sars_vax_when == 99) &
                                  is.na(ccc19x$der_vax_before))] <- 99
    
    #Patient diagnosed in 2020 and value remains missing -> assume no
    ccc19x$der_vax_before[which(ccc19x$dx_year == 2020 &
                                  is.na(ccc19x$der_vax_before))] <- 0
    
    ccc19x$der_vax_before <- factor(ccc19x$der_vax_before)
    
    temp <- summary(ccc19x$der_vax_before[ccc19x$redcap_repeat_instrument == ''])
    temp.var.log <- data.frame(name = 'der_vax_before',
                               timestamp = Sys.time(),
                               values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
                               stringsAsFactors = F)
    var.log <- rbind(var.log, temp.var.log)
  }
  print('Other derived variables completed')
  
  #These variables are deprecated and should remain commented out
   {
  #   ############################################
  #   #Dep01. Any cancer treatment in past 3 months
  #   ############################################
  #   ccc19x$der_anytx <- NA
  #   
  #   ccc19x$der_anytx[which((ccc19x$on_treatment == 1 & ccc19x$recent_treatment %in% 1:3)|
  #                            ccc19x$hx_treatment == 1)] <- 1
  #   
  #   ccc19x$der_anytx[which((ccc19x$on_treatment == 0 & ccc19x$hx_treatment != 1)|
  #                            (ccc19x$on_treatment == 1 & ccc19x$recent_treatment %in% 88 & ccc19x$hx_treatment != 1)|
  #                            ccc19x$recent_treatment == 98)] <- 0
  #   
  #   ccc19x$der_anytx[which((ccc19x$on_treatment == 99 | 
  #                             ccc19x$recent_treatment == 99 |
  #                             ccc19x$hx_treatment == 99 |
  #                             (ccc19x$on_treatment == 1 & is.na(ccc19x$recent_treatment))) &
  #                            is.na(ccc19x$der_anytx))] <- 99
  #   
  #   ccc19x$der_anytx <- factor(ccc19x$der_anytx)
  #   
  #   temp <- summary(ccc19x$der_anytx[ccc19x$redcap_repeat_instrument == ''])
  #   temp.var.log <- data.frame(name = 'der_anytx',
  #                              timestamp = Sys.time(),
  #                              values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
  #                              stringsAsFactors = F)
  #   var.log <- rbind(var.log, temp.var.log)
  #   
  #   #####################################
  #   #Dep02. Any cytotoxic within 3 months
  #   #####################################
  #   ccc19x$der_any_cyto <- ccc19x$der_anytx
  #   ccc19x$der_any_cyto[which(ccc19x$der_any_cyto == 1 & 
  #                               ((ccc19x$treatment_modality___685 == 0 & ccc19x$treatment_modality___45186 == 0)|
  #                                  (ccc19x$treatment_modality___694 == 0 &
  #                                     ccc19x$treatment_modality___45186 == 1 & ccc19x$transplant_cellular_therapy %in% c(10,2,3,4,5,6))))] <- 0
  #   
  #   temp <- summary(ccc19x$der_any_cyto[ccc19x$redcap_repeat_instrument == ''])
  #   temp.var.log <- data.frame(name = 'der_any_cyto',
  #                              timestamp = Sys.time(),
  #                              values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
  #                              stringsAsFactors = F)
  #   var.log <- rbind(var.log, temp.var.log)
  #   
  #   #Dep03. Any immunotherapy within 3 months
  #   ccc19x$der_any_immuno <- ccc19x$der_anytx
  #   ccc19x$der_any_immuno[which(ccc19x$der_any_immuno == 1 & 
  #                                 ((ccc19x$treatment_modality___694 == 0 & ccc19x$treatment_modality___45186 == 0)|
  #                                    (ccc19x$treatment_modality___694 == 0 &
  #                                       ccc19x$treatment_modality___45186 == 1 & ccc19x$transplant_cellular_therapy == 1)))] <- 0
  #   
  #   temp <- summary(ccc19x$der_any_immuno[ccc19x$redcap_repeat_instrument == ''])
  #   temp.var.log <- data.frame(name = 'der_any_immuno',
  #                              timestamp = Sys.time(),
  #                              values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
  #                              stringsAsFactors = F)
  #   var.log <- rbind(var.log, temp.var.log)
  #   
  #   #Dep04. Any targeted therapy within 3 months
  #   ccc19x$der_any_targeted <- ccc19x$der_anytx
  #   ccc19x$der_any_targeted[which(ccc19x$der_any_targeted == 1 & ccc19x$treatment_modality___58229 == 0)] <- 0
  #   
  #   temp <- summary(ccc19x$der_any_targeted[ccc19x$redcap_repeat_instrument == ''])
  #   temp.var.log <- data.frame(name = 'der_any_targeted',
  #                              timestamp = Sys.time(),
  #                              values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
  #                              stringsAsFactors = F)
  #   var.log <- rbind(var.log, temp.var.log)
  #   
  #   #Dep05. Any endocrine therapy within 3 months
  #   ccc19x$der_any_endo <- ccc19x$der_anytx
  #   ccc19x$der_any_endo[which(ccc19x$der_any_endo == 1 & ccc19x$treatment_modality___691 == 0)] <- 0
  #   
  #   temp <- summary(ccc19x$der_any_endo[ccc19x$redcap_repeat_instrument == ''])
  #   temp.var.log <- data.frame(name = 'der_any_endo',
  #                              timestamp = Sys.time(),
  #                              values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
  #                              stringsAsFactors = F)
  #   var.log <- rbind(var.log, temp.var.log)
  #   
  #   #Dep06. Any radiation therapy within 3 months
  #   ccc19x$der_any_rt <- ccc19x$der_anytx
  #   ccc19x$der_any_rt[which(ccc19x$der_any_rt == 1 & ccc19x$treatment_modality___695 == 0)] <- 0
  #   summary(ccc19x$der_any_rt[ccc19x$redcap_repeat_instrument == ''])
  #   
  #   #Dep07. Any cancer surgery within 3 months
  #   ccc19x$der_any_ca_surgery <- ccc19x$der_anytx
  #   ccc19x$der_any_ca_surgery[which(ccc19x$der_any_ca_surgery == 1 & ccc19x$treatment_modality___14051 == 0)] <- 0
  #   summary(ccc19x$der_any_ca_surgery[ccc19x$redcap_repeat_instrument == ''])
  #   
  #   #Dep08. Any other therapy within 3 months
  #   ccc19x$der_any_other <- ccc19x$der_anytx
  #   ccc19x$der_any_other[which(ccc19x$der_any_other == 1 & 
  #                                ccc19x$treatment_modality___45215 == 0 &
  #                                ccc19x$treatment_modality___oth == 0)] <- 0
  #   
  #   temp <- summary(ccc19x$der_any_other[ccc19x$redcap_repeat_instrument == ''])
  #   temp.var.log <- data.frame(name = 'der_any_other',
  #                              timestamp = Sys.time(),
  #                              values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
  #                              stringsAsFactors = F)
  #   var.log <- rbind(var.log, temp.var.log)
  #   
  #   #Dep09. Any targeted therapy or ICI within 3 months
  #   ccc19x$der_any_targeted_ici <- ccc19x$der_anytx
  #   ccc19x$der_any_targeted_ici[which(ccc19x$der_any_targeted_ici == 1 & 
  #                                       (ccc19x$treatment_modality___58229 == 1|
  #                                          ccc19x$what_immunotherapy %in% c('45838', '45446',
  #                                                                           '45170', '45838-45446')))] <- 1
  #   ccc19x$der_any_targeted_ici[which(ccc19x$der_any_targeted_ici == 1 & (ccc19x$treatment_modality___58229 == 0|
  #                                                                           !ccc19x$what_immunotherapy %in% c('45838', '45446',
  #                                                                                                             '45170', '45838-45446')))] <- 0
  #   
  #   ccc19x$der_any_targeted_ici <- factor(ccc19x$der_any_targeted_ici)
  #   summary(ccc19x$der_any_targeted_ici[ccc19x$redcap_repeat_instrument == ''])
  #   
  #   #Dep10. Any transplant or cellular therapy within 3 months
  #   ccc19x$der_any_sct_cellular <- ccc19x$der_anytx
  #   ccc19x$der_any_sct_cellular[which(ccc19x$der_any_sct_cellular == 1 & ccc19x$treatment_modality___45186 == 0)] <- 0
  #   ccc19x$der_any_sct_cellular[which(ccc19x$transplant_cellular_timing %in% c(0,3,4))] <- 0
  #   ccc19x$der_any_sct_cellular[which(ccc19x$transplant_cellular_timing %in% 1:2)] <- 1
  #   ccc19x$der_any_sct_cellular[which(ccc19x$transplant_cellular_timing == 99)] <- 99
  #   
  #   ccc19x$der_any_sct_cellular <- factor(ccc19x$der_any_sct_cellular)
  #   summary(ccc19x$der_any_sct_cellular[ccc19x$redcap_repeat_instrument == ''])
  #   
  #   #Dep11. Any local therapy within 3 months (surgery or radiation)
  #   ccc19x$der_any_local <- ccc19x$der_any_ca_surgery
  #   ccc19x$der_any_local[which(ccc19x$der_any_rt == 1)] <- 1
  #   
  #   temp <- summary(ccc19x$der_any_local[ccc19x$redcap_repeat_instrument == ''])
  #   temp.var.log <- data.frame(name = 'der_any_local',
  #                              timestamp = Sys.time(),
  #                              values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
  #                              stringsAsFactors = F)
  #   var.log <- rbind(var.log, temp.var.log)
  #   
  #   #Dep12. Any systemic therapy within 3 months
  #   ccc19x$der_any_systemic <- ccc19x$der_any_cyto
  #   ccc19x$der_any_systemic[which(ccc19x$der_any_endo == 1|
  #                                   ccc19x$der_any_immuno == 1|
  #                                   ccc19x$der_any_targeted == 1)] <- 1
  #   
     # #Dep13. No cancer therapy within 3 months
     # ccc19x$der_cancertr_none <- NA
     # 
     # #Yes
     # ccc19x$der_cancertr_none[which(ccc19x$der_any_cyto == 0 &
     #                                  ccc19x$der_any_targeted == 0 &
     #                                  ccc19x$der_any_endo == 0 &
     #                                  ccc19x$der_any_immuno == 0 &
     #                                  ccc19x$der_any_local == 0 &
     #                                  ccc19x$der_any_other == 0)] <- 1
     # 
     # #No
     # ccc19x$der_cancertr_none[which(ccc19x$der_any_cyto == 1|
     #                                  ccc19x$der_any_targeted == 1|
     #                                  ccc19x$der_any_endo == 1|
     #                                  ccc19x$der_any_immuno == 1|
     #                                  ccc19x$der_any_local == 1|
     #                                  ccc19x$der_any_other == 1)] <- 0
     # 
     # #Unknown
     # ccc19x$der_cancertr_none[which(ccc19x$der_any_cyto == 99|
     #                                  ccc19x$der_any_targeted == 99|
     #                                  ccc19x$der_any_endo == 99|
     #                                  ccc19x$der_any_immuno == 99|
     #                                  ccc19x$der_any_local == 99|
     #                                  ccc19x$der_any_other == 99)] <- 99
     # 
     # ccc19x$der_cancertr_none <- as.factor(ccc19x$der_cancertr_none)
     
     # ##Dep14. derived variable coding the obesity status (binary) - DEPRECATED
     # {
     #   ccc19x$der_obesity <- NA
     #   
     #   ccc19x$der_obesity[which(ccc19x$significant_comorbidities___414916001 == 1 |
     #                              ccc19x$significant_comorbidities___238136002 == 1)] <- 1
     #   
     #   #Records with numeric BMI recorded or derived as >= 30
     #   ccc19x$der_obesity[which(ccc19x$der_bmi >= 30)] <- 1
     #   
     #   #Records with numeric BMI recorded or derived as < 30 (do not overwrite, for now)
     #   ccc19x$der_obesity[which(ccc19x$der_bmi < 30 & is.na(ccc19x$der_obesity))] <- 0
     #   
     #   #Not specified (map to Not obese for now)
     #   ccc19x$der_obesity[which(ccc19x$significant_comorbidities___238136002 == 0 &
     #                              ccc19x$significant_comorbidities___414916001 == 0 &
     #                              is.na(ccc19x$der_obesity))] <- 0
     #   
     #   #Revert "not obese" to NA if all the significant comorbidities are unchecked and BMI data not available
     #   temp.ref <- grep(colnames(ccc19x), pattern = 'significant_comorbidities___')
     #   for(i in which(ccc19x$redcap_repeat_instrument == ''))
     #     if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_obesity[i] == 0) ccc19x$der_obesity[i] <- NA
     #   
     #   #Unknown
     #   ccc19x$der_obesity[which(ccc19x$significant_comorbidities___unk == 1 & is.na(ccc19x$der_obesity))] <- 99
     #   
     #   #Factor
     #   ccc19x$der_obesity <- as.factor(ccc19x$der_obesity)
     #   
     #   temp <- summary(ccc19x$der_obesity[ccc19x$redcap_repeat_instrument == ''])
     #   temp.var.log <- data.frame(name = 'der_obesity',
     #                              timestamp = Sys.time(),
     #                              values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
     #                              stringsAsFactors = F)
     #   var.log <- rbind(var.log, temp.var.log)
     # }
     # 
     # ##zDep15. derived variable coding the morbid obesity status (binary) with BMI cutoff of 40
     # {
     #   ccc19x$der_morbid_obesity <- NA
     #   
     #   ccc19x$der_morbid_obesity[which(ccc19x$significant_comorbidities___238136002 == 1)] <- 1
     #   
     #   #Records with numeric BMI recorded or derived as >= 40
     #   ccc19x$der_morbid_obesity[which(ccc19x$der_bmi >= 40)] <- 1
     #   
     #   #Records with numeric BMI recorded or derived as < 40 (do not overwrite, for now)
     #   ccc19x$der_morbid_obesity[which(ccc19x$der_bmi < 40 & is.na(ccc19x$der_morbid_obesity))] <- 0
     #   
     #   #Not specified
     #   ccc19x$der_morbid_obesity[which(ccc19x$significant_comorbidities___238136002 == 0 &
     #                                     is.na(ccc19x$der_morbid_obesity))] <- 0
     #   
     #   #Revert "not obese" to NA if all the significant comorbidities are unchecked and BMI data not available
     #   temp.ref <- grep(colnames(ccc19x), pattern = 'significant_comorbidities___')
     #   for(i in which(ccc19x$redcap_repeat_instrument == ''))
     #     if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_morbid_obesity[i] == 0) ccc19x$der_morbid_obesity[i] <- NA
     #   
     #   #Unknown
     #   ccc19x$der_morbid_obesity[which(ccc19x$significant_comorbidities___unk == 1 & is.na(ccc19x$der_morbid_obesity))] <- 99
     # 
     #   #Factor
     #   ccc19x$der_morbid_obesity <- as.factor(ccc19x$der_morbid_obesity)
     #   
     #   temp <- summary(ccc19x$der_morbid_obesity[ccc19x$redcap_repeat_instrument == ''])
     #   temp.var.log <- data.frame(name = 'der_morbid_obesity',
     #                              timestamp = Sys.time(),
     #                              values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
     #                              stringsAsFactors = F)
     #   var.log <- rbind(var.log, temp.var.log)
     #   
     # }
     # 
     # ##zDep16. derived variable coding the morbid obesity status (binary, cutoff BMI 35)
     # {
     #   ccc19x$der_morbid_obesity_v2 <- NA
     #   
     #   ccc19x$der_morbid_obesity_v2[which(ccc19x$significant_comorbidities___238136002 == 1)] <- 1
     #   
     #   #Records with numeric BMI recorded or derived as >= 35
     #   ccc19x$der_morbid_obesity_v2[which(ccc19x$der_bmi >= 35)] <- 1
     #   
     #   #Records with numeric BMI recorded or derived as < 35 (do not overwrite, for now)
     #   ccc19x$der_morbid_obesity_v2[which(ccc19x$der_bmi < 35 & is.na(ccc19x$der_morbid_obesity_v2))] <- 0
     #   
     #   #Not specified
     #   ccc19x$der_morbid_obesity_v2[which(ccc19x$significant_comorbidities___238136002 == 0 &
     #                                        is.na(ccc19x$der_morbid_obesity_v2))] <- 0
     #   
     #   #Revert "not obese" to NA if all the significant comorbidities are unchecked and BMI data not available
     #   temp.ref <- grep(colnames(ccc19x), pattern = 'significant_comorbidities___')
     #   for(i in which(ccc19x$redcap_repeat_instrument == ''))
     #     if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_morbid_obesity_v2[i] == 0) ccc19x$der_morbid_obesity_v2[i] <- NA
     #   
     #   #Unknown
     #   ccc19x$der_morbid_obesity_v2[which(ccc19x$significant_comorbidities___unk == 1 & is.na(ccc19x$der_morbid_obesity_v2))] <- 99
     #   
     #   #Factor
     #   ccc19x$der_morbid_obesity_v2 <- as.factor(ccc19x$der_morbid_obesity_v2)
     #   
     #   temp <- summary(ccc19x$der_morbid_obesity_v2[ccc19x$redcap_repeat_instrument == ''])
     #   temp.var.log <- data.frame(name = 'der_morbid_obesity_v2',
     #                              timestamp = Sys.time(),
     #                              values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
     #                              stringsAsFactors = F)
     #   var.log <- rbind(var.log, temp.var.log)
     # }
     
     # #zDep17. Thromboprophylaxis, including consideration of baseline anticoagulation
     # #Does not yet account for discrepancies in baseline and follow-up forms
     # ccc19x$der_thromboprophy <- NA
     # 
     # #Baseline prophylaxis
     # ccc19x$der_thromboprophy[which(ccc19x$bl_anticoag_reason == 360271000)] <- 'Baseline prophylactic anticoagulation'
     # 
     # #Baseline therapeutic
     # ccc19x$der_thromboprophy[which(ccc19x$bl_anticoag_reason == 262202000)] <- 'Baseline therapeutic anticoagulation'
     # 
     # #COVID-19 prophylaxis
     # ccc19x$der_thromboprophy[which((ccc19x$c19_anticoag_reason___1 == 1|
     #                                   ccc19x$c19_anticoag_reason_fu___1 == 1) &
     #                                  ccc19x$der_thromboprophy == 'Baseline prophylactic anticoagulation')] <- 'Baseline prophy--COVID-19 prophylactic anticoagulation'
     # ccc19x$der_thromboprophy[which((ccc19x$c19_anticoag_reason___1 == 1|
     #                                   ccc19x$c19_anticoag_reason_fu___1 == 1) &
     #                                  ccc19x$der_thromboprophy == 'Baseline therapeutic anticoagulation')] <- 'Baseline therapeutic--COVID-19 prophylactic anticoagulation'
     # ccc19x$der_thromboprophy[which((ccc19x$c19_anticoag_reason___1 == 1|
     #                                   ccc19x$c19_anticoag_reason_fu___1 == 1) &
     #                                  ccc19x$der_ac_baseline == 0)] <- 'No baseline--COVID-19 prophylactic anticoagulation'
     # ccc19x$der_thromboprophy[which((ccc19x$c19_anticoag_reason___1 == 1|
     #                                   ccc19x$c19_anticoag_reason_fu___1 == 1) &
     #                                  ccc19x$der_ac_baseline == 99)] <- 'Unknown baseline--COVID-19 prophylactic anticoagulation'
     # ccc19x$der_thromboprophy[which((ccc19x$c19_anticoag_reason___1 == 1|
     #                                   ccc19x$c19_anticoag_reason_fu___1 == 1) &
     #                                  is.na(ccc19x$der_ac_baseline))] <- 'Missing baseline--COVID-19 prophylactic anticoagulation'
     # 
     # #COVID-19 treatment
     # ccc19x$der_thromboprophy[which((ccc19x$c19_anticoag_reason___2a == 1|
     #                                   ccc19x$c19_anticoag_reason___2c == 1|
     #                                   ccc19x$c19_anticoag_reason_fu___2a == 1|
     #                                   ccc19x$c19_anticoag_reason_fu___2c == 1) &
     #                                  ccc19x$der_thromboprophy == 'Baseline prophylactic anticoagulation')] <- 'Baseline prophy--COVID-19 therapeutic anticoagulation'
     # ccc19x$der_thromboprophy[which((ccc19x$c19_anticoag_reason___2a == 1|
     #                                   ccc19x$c19_anticoag_reason___2c == 1|
     #                                   ccc19x$c19_anticoag_reason_fu___2a == 1|
     #                                   ccc19x$c19_anticoag_reason_fu___2c == 1) &
     #                                  ccc19x$der_thromboprophy == 'Baseline therapeutic anticoagulation')] <- 'Baseline therapeutic--COVID-19 therapeutic anticoagulation'
     # ccc19x$der_thromboprophy[which((ccc19x$c19_anticoag_reason___2a == 1|
     #                                   ccc19x$c19_anticoag_reason___2c == 1|
     #                                   ccc19x$c19_anticoag_reason_fu___2a == 1|
     #                                   ccc19x$c19_anticoag_reason_fu___2c == 1) &
     #                                  ccc19x$der_ac_baseline == 0)] <- 'No baseline--COVID-19 therapeutic anticoagulation'
     # ccc19x$der_thromboprophy[which((ccc19x$c19_anticoag_reason___2a == 1|
     #                                   ccc19x$c19_anticoag_reason___2c == 1|
     #                                   ccc19x$c19_anticoag_reason_fu___2a == 1|
     #                                   ccc19x$c19_anticoag_reason_fu___2c == 1) &
     #                                  ccc19x$der_ac_baseline == 99)] <- 'Unknown baseline--COVID-19 therapeutic anticoagulation'
     # ccc19x$der_thromboprophy[which((ccc19x$c19_anticoag_reason___2a == 1|
     #                                   ccc19x$c19_anticoag_reason___2c == 1|
     #                                   ccc19x$c19_anticoag_reason_fu___2a == 1|
     #                                   ccc19x$c19_anticoag_reason_fu___2c == 1) &
     #                                  is.na(ccc19x$der_ac_baseline))] <- 'Missing baseline--COVID-19 therapeutic anticoagulation'
     # 
     # #None for COVID-19
     # ccc19x$der_thromboprophy[which(ccc19x$c19_anticoag_reason___none == 1 &
     #                                  ccc19x$der_thromboprophy == 'Baseline prophylactic anticoagulation')] <- 'Baseline prophy--no COVID-19 anticoagulation'
     # ccc19x$der_thromboprophy[which(ccc19x$c19_anticoag_reason___none == 1 &
     #                                  ccc19x$der_thromboprophy == 'Baseline therapeutic anticoagulation')] <- 'Baseline therapeutic--no COVID-19 anticoagulation'
     # ccc19x$der_thromboprophy[which(ccc19x$c19_anticoag_reason___none == 1 &
     #                                  ccc19x$der_ac_baseline == 0)] <- 'No baseline--no COVID-19 anticoagulation'
     # ccc19x$der_thromboprophy[which(ccc19x$c19_anticoag_reason___none == 1 &
     #                                  ccc19x$der_ac_baseline == 99)] <- 'Unknown baseline--no COVID-19 anticoagulation'
     # ccc19x$der_thromboprophy[which(ccc19x$c19_anticoag_reason___none == 1 &
     #                                  is.na(ccc19x$der_ac_baseline))] <- 'Missing baseline--no COVID-19 anticoagulation'
     # 
     # #Unknown for COVID-19
     # ccc19x$der_thromboprophy[which(ccc19x$c19_anticoag_reason___unk == 1 &
     #                                  ccc19x$der_thromboprophy == 'Baseline prophylactic anticoagulation')] <- 'Baseline prophy--unknown COVID-19 anticoagulation'
     # ccc19x$der_thromboprophy[which(ccc19x$c19_anticoag_reason___unk == 1 &
     #                                  ccc19x$der_thromboprophy == 'Baseline therapeutic anticoagulation')] <- 'Baseline therapeutic--unknown COVID-19 anticoagulation'
     # ccc19x$der_thromboprophy[which(ccc19x$c19_anticoag_reason___unk == 1 &
     #                                  ccc19x$der_ac_baseline == 0)] <- 'No baseline--unknown COVID-19 anticoagulation'
     # ccc19x$der_thromboprophy[which(ccc19x$c19_anticoag_reason___unk == 1 &
     #                                  ccc19x$der_ac_baseline == 99)] <- 'Unknown baseline--unknown COVID-19 anticoagulation'
     # ccc19x$der_thromboprophy[which(ccc19x$c19_anticoag_reason___unk == 1 &
     #                                  is.na(ccc19x$der_ac_baseline))] <- 'Missing baseline--unknown COVID-19 anticoagulation'
     # 
     # #Missing for COVID-19 but not for baseline
     # ccc19x$der_thromboprophy[which(ccc19x$der_thromboprophy == 'Baseline prophylactic anticoagulation')] <- 'Baseline prophy--missing COVID-19 anticoagulation'
     # ccc19x$der_thromboprophy[which(ccc19x$der_thromboprophy == 'Baseline therapeutic anticoagulation')] <- 'Baseline therapeutic--missing COVID-19 anticoagulation'
     # 
     # ccc19x$der_thromboprophy <- factor(ccc19x$der_thromboprophy)
     # summary(ccc19x$der_thromboprophy[ccc19x$redcap_repeat_instrument == ''])
     
     # ######################
     # #zDep18. Modified Khorana
     # ######################
     # {
     #   ccc19x$der_VTE_risk <- NA
     #   
     #   #Create dummy variables if the data set does not have cancer_type_3, 4, 5
     #   if(is.null(ccc19x$cancer_type_3))
     #   {
     #     ccc19x$cancer_type_3 <- ''
     #     ccc19x$cancer_type_4 <- ''
     #     ccc19x$cancer_type_5 <- ''
     #   }
     #   
     #   ccc19x$der_VTE_risk[which((ccc19x$cancer_type %in% c("C3850","C4911","C3513")| 
     #                                ccc19x$cancer_type_2 %in% c("C3850","C4911", "C3513")| 
     #                                ccc19x$cancer_type_3 %in% c("C3850","C4911", "C3513")| 
     #                                ccc19x$cancer_type_4 %in% c("C3850","C4911", "C3513")| 
     #                                ccc19x$cancer_type_5 %in% c("C3850","C4911", "C3513")))] <- 'High-risk VTE malignancy'
     #   ccc19x$der_VTE_risk[which((ccc19x$cancer_type %in% c("C4917","C2926","C4878","C7431","C9063","C4912","C3708","C7355","C9385","C8851","C3209","C9244","C4341","C3211","C3457","C9357","C4337","C2912","C8504","C27908")| 
     #                                ccc19x$cancer_type_2 %in% c("C4917","C2926","C4878","C7431","C9063","C4912","C3708","C7355","C9385","C8851","C3209","C9244","C4341","C3211","C3457","C9357","C4337","C2912","C8504","C27908")| 
     #                                ccc19x$cancer_type_3 %in% c("C4917","C2926","C4878","C7431","C9063","C4912","C3708","C7355","C9385","C8851","C3209","C9244","C4341","C3211","C3457","C9357","C4337","C2912","C8504","C27908")| 
     #                                ccc19x$cancer_type_4 %in% c("C4917","C2926","C4878","C7431","C9063","C4912","C3708","C7355","C9385","C8851","C3209","C9244","C4341","C3211","C3457","C9357","C4337","C2912","C8504","C27908")| 
     #                                ccc19x$cancer_type_5 %in% c("C4917","C2926","C4878","C7431","C9063","C4912","C3708","C7355","C9385","C8851","C3209","C9244","C4341","C3211","C3457","C9357","C4337","C2912","C8504","C27908")))] <- 'Intermediate-risk VTE malignancy'
     #   ccc19x$der_VTE_risk[which((ccc19x$cancer_type %in% c("C4872","C4863","C9291","C4910","C9330","C7724","C2955","C9382","C4013", "C3871")| 
     #                                ccc19x$cancer_type_2 %in% c("C4872","C4863","C9291","C4910","C9330","C7724","C2955","C9382","C4013", "C3871")| 
     #                                ccc19x$cancer_type_3 %in% c("C4872","C4863","C9291","C4910","C9330","C7724","C2955","C9382","C4013", "C3871")| 
     #                                ccc19x$cancer_type_4 %in% c("C4872","C4863","C9291","C4910","C9330","C7724","C2955","C9382","C4013", "C3871")| 
     #                                ccc19x$cancer_type_5 %in% c("C4872","C4863","C9291","C4910","C9330","C7724","C2955","C9382","C4013", "C3871")))] <- 'Low-risk VTE malignancy'
     #   ccc19x$der_VTE_risk[which((ccc19x$cancer_type %in% c("C3234","C3411","C3099","C3844","C4436","C3267", "C7558", "C9039","C3867","C3555","C3917","C4866","C4815", "C9325", "C3809", "C4906","C9306", "C3868", "C9145","C9312","C4817","C3359","C8538","C3059","C4627","C5111","C132067", "C3270", "C7541","C3224", "C9231","C4819","C2921","C132146","C4039","C3538","C9061","C6389","C4189","OTH","OTH_S")|
     #                                ccc19x$cancer_type_2 %in% c("C3234","C3411","C3099","C3844","C4436","C3267", "C7558", "C9039","C3867","C3555","C3917","C4866","C4815", "C9325", "C3809", "C4906","C9306", "C3868", "C9145","C9312","C4817","C3359","C8538","C3059","C4627","C5111","C132067", "C3270", "C7541","C3224", "C9231","C4819","C2921","C132146","C4039","C3538","C9061","C6389","C4189","OTH","OTH_S")|
     #                                ccc19x$cancer_type_3 %in% c("C3234","C3411","C3099","C3844","C4436","C3267", "C7558", "C9039","C3867","C3555","C3917","C4866","C4815", "C9325", "C3809", "C4906","C9306", "C3868", "C9145","C9312","C4817","C3359","C8538","C3059","C4627","C5111","C132067", "C3270", "C7541","C3224", "C9231","C4819","C2921","C132146","C4039","C3538","C9061","C6389","C4189","OTH","OTH_S")|
     #                                ccc19x$cancer_type_4 %in% c("C3234","C3411","C3099","C3844","C4436","C3267", "C7558", "C9039","C3867","C3555","C3917","C4866","C4815", "C9325", "C3809", "C4906","C9306", "C3868", "C9145","C9312","C4817","C3359","C8538","C3059","C4627","C5111","C132067", "C3270", "C7541","C3224", "C9231","C4819","C2921","C132146","C4039","C3538","C9061","C6389","C4189","OTH","OTH_S")|
     #                                ccc19x$cancer_type_5 %in% c("C3234","C3411","C3099","C3844","C4436","C3267", "C7558", "C9039","C3867","C3555","C3917","C4866","C4815", "C9325", "C3809", "C4906","C9306", "C3868", "C9145","C9312","C4817","C3359","C8538","C3059","C4627","C5111","C132067", "C3270", "C7541","C3224", "C9231","C4819","C2921","C132146","C4039","C3538","C9061","C6389","C4189","OTH","OTH_S")))] <- 'Other solid malignancy'
     #   ccc19x$der_VTE_risk[which((ccc19x$cancer_type %in% c("C3167","C3163","C9308","C3247","C3171","C4345","C3106","C3174","C3242","C4665","C3819","C27134","C9300","OTH_H")| 
     #                                ccc19x$cancer_type_2 %in% c("C3167","C3163","C9308","C3247","C3171","C4345","C3106","C3174","C3242","C4665","C3819","C27134","C9300","OTH_H")| 
     #                                ccc19x$cancer_type_3 %in% c("C3167","C3163","C9308","C3247","C3171","C4345","C3106","C3174","C3242","C4665","C3819","C27134","C9300","OTH_H")| 
     #                                ccc19x$cancer_type_4 %in% c("C3167","C3163","C9308","C3247","C3171","C4345","C3106","C3174","C3242","C4665","C3819","C27134","C9300","OTH_H")| 
     #                                ccc19x$cancer_type_5 %in% c("C3167","C3163","C9308","C3247","C3171","C4345","C3106","C3174","C3242","C4665","C3819","C27134","C9300","OTH_H")))] <- 'Other heme malignancy'
     #   ccc19x$der_VTE_risk <- factor(ccc19x$der_VTE_risk)
     #   ccc19x$der_VTE_risk <- relevel(ccc19x$der_VTE_risk, ref = 'Low-risk VTE malignancy')
     #   
     #   temp <- summary(ccc19x$der_VTE_risk[ccc19x$redcap_repeat_instrument == ''])
     #   temp.var.log <- data.frame(name = 'der_VTE_risk',
     #                              timestamp = Sys.time(),
     #                              values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
     #                              stringsAsFactors = F)
     #   var.log <- rbind(var.log, temp.var.log)
     # }
     
     # #zDep19. Metastatic disease to lung
     # ccc19x$der_met_lung <- NA
     # 
     # #Yes
     # ccc19x$der_met_lung[which(ccc19x$mets_sites___1116_1 == 1)] <- 1
     # 
     # #No
     # ccc19x$der_met_lung[which(ccc19x$cancer_status == 1 & is.na(ccc19x$der_met_lung))] <- 0
     # ccc19x$der_met_lung[which(ccc19x$mets_yn == 0 & is.na(ccc19x$der_met_lung))] <- 0
     # ccc19x$der_met_lung[which(ccc19x$mets_yn == 1 & ccc19x$mets_sites___1116_1 == 0)] <- 0
     # 
     # #Unknown
     # ccc19x$der_met_lung[which(ccc19x$mets_yn == 99 & is.na(ccc19x$der_met_lung))] <- 99
     # ccc19x$der_met_lung[which(ccc19x$mets_sites___99 == 1 & is.na(ccc19x$der_met_lung))] <- 99
     # 
     # ccc19x$der_met_lung <- as.factor(ccc19x$der_met_lung)
     # 
     # temp <- summary(ccc19x$der_met_lung[ccc19x$redcap_repeat_instrument == ''])
     # temp.var.log <- data.frame(name = 'der_met_lung',
     #                            timestamp = Sys.time(),
     #                            values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
     #                            stringsAsFactors = F)
     # var.log <- rbind(var.log, temp.var.log)
     
     # ##zDep20. Derived dead/alive variable (0=alive, 1=dead, 99=unknown)
     # #Deprecated because it does not use the vital status indicators
     # ccc19x$der_deadbinary_old <- NA
     # 
     # #Alive on primary form
     # ccc19x$der_deadbinary_old[which(ccc19x$current_status %in% 1:8 |
     #                                   ccc19x$current_status_retro %in% c("1", "1b") |
     #                                   ccc19x$current_status_v2 %in% c("1", "1b", "2")) ] <- 0
     # 
     # #Dead on primary form
     # ccc19x$der_deadbinary_old[which(ccc19x$current_status_retro == 3 |
     #                                   ccc19x$current_status_v2 == 3 |
     #                                   ccc19x$current_status == 9)] <- 1
     # 
     # #Unknown on primary form
     # ccc19x$der_deadbinary_old[which(ccc19x$current_status_retro == 99)] <- 99
     # 
     # #Alive on followup form
     # temp.ref <- which(ccc19x$covid_19_status_fu %in% c('1', '1b', '2') |
     #                     ccc19x$fu_reason %in% 1:2)
     # 
     # ccc19x$der_deadbinary_old[temp.ref] <- 0
     # 
     # #Dead on followup form
     # temp.ref <- which(ccc19x$covid_19_status_fu ==3 |
     #                     ccc19x$current_status_fu == 9 |
     #                     ccc19x$fu_reason == 3)
     # 
     # ccc19x$der_deadbinary_old[temp.ref] <- 1
     # 
     # #Unknown on followup form
     # temp.ref <- which(ccc19x$covid_19_status_fu == 99)
     # ccc19x$der_deadbinary_old[temp.ref] <- 99
     # 
     # #Reconcile death status for each patient
     # temp <- unique(ccc19x$record_id)
     # for(i in 1:length(temp))
     # {
     #   temp.ref <- which(ccc19x$record_id == temp[i])
     #   temp2 <- ccc19x$der_deadbinary_old[temp.ref]
     #   temp2 <- temp2[!is.na(temp2)]
     #   if(length(temp2) > 0)
     #   {
     #     if(any(temp2 == 1)) ccc19x$der_deadbinary_old[temp.ref] <- 1
     #     if(!any(temp2 == 1) & any(temp2 == 0)) ccc19x$der_deadbinary_old[temp.ref] <- 0
     #     if(!any(temp2 == 1) & !any(temp2 == 0) & any(temp2 == 99)) ccc19x$der_deadbinary_old[temp.ref] <- 99
     #   }
     # }
     # 
     # #Factor
     # ccc19x$der_deadbinary_old <- as.factor(ccc19x$der_deadbinary_old)

     # temp <- summary(ccc19x$der_deadbinary_old[ccc19x$redcap_repeat_instrument == ''])
     # temp.var.log <- data.frame(name = 'der_deadbinary_old',
     #                            timestamp = Sys.time(),
     #                            values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
     #                            stringsAsFactors = F)
     # var.log <- rbind(var.log, temp.var.log)
     
     ##########################
     #zDep21. COVID-19 vaccination
     ##########################
     # ccc19x$der_vax <- NA
     # 
     # #No
     # ccc19x$der_vax[which(ccc19x$sars_vax == 0)] <- 'Unvaccinated'
     # 
     # #Fully vaccinated
     # ccc19x$der_vax[which((ccc19x$sars_vax_which == 4 & ccc19x$sars_vax_when %in% 2:4)|
     #                        (ccc19x$sars_vax_which %in% c('1b', '2b') & ccc19x$sars_vax_when %in% 3:4)|
     #                        (ccc19x$sars_vax_which %in% c('3b') & ccc19x$sars_vax_when %in% 2:4))
     #                  ] <- 'Fully vaccinated'
     # 
     # ccc19x$der_vax[which(ccc19x$sars_vax_when == 1|
     #                        (ccc19x$sars_vax_when %in% 2:4 & is.na(ccc19x$der_vax)))] <- 'Partially vaccinated'
     # 
     # # #Partial early (0-4 weeks)
     # # ccc19x$der_vax[which(ccc19x$sars_vax_when == 1)] <- 'Partially vaccinated, early (0-4 wks)'
     # # 
     # # #Partial late (4+ weeks)
     # # ccc19x$der_vax[which(ccc19x$sars_vax_when %in% 2:4 & is.na(ccc19x$der_vax))] <- 'Partially vaccinated, late (4+ weeks)'
     # # 
     # #After COVID-19
     # ccc19x$der_vax[which(ccc19x$sars_vax_when == 88)] <- 'After COVID-19'
     # 
     # #Unknown
     # ccc19x$der_vax[which(ccc19x$sars_vax == 99|
     #                        ccc19x$sars_vax_when == 99)] <- 'Unknown'
     # 
     # ccc19x$der_vax <- factor(ccc19x$der_vax)
     # 
     # temp <- summary(ccc19x$der_vax[ccc19x$redcap_repeat_instrument == ''])
     # temp.var.log <- data.frame(name = 'der_vax',
     #                            timestamp = Sys.time(),
     #                            values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
     #                            stringsAsFactors = F)
     # var.log <- rbind(var.log, temp.var.log)
     # 
     
     # #zDep22 Death within 90 days, with default is not dead at 90 days
     # ccc19x$der_dead90 <- 0
     # 
     # temp.ref <- which(ccc19x$der_deadbinary == 1 & ccc19x$redcap_repeat_instrument == '')
     # 
     # #1. Calculated time to death is <= 90 days
     # temp.diff <- difftime(ccc19x$meta_righttime, ccc19x$meta_lefttime_lb, units = 'days')
     # temp.ref2 <- which(temp.diff[temp.ref] <= 90)
     # temp <- ccc19x$record_id[temp.ref[temp.ref2]]
     # ccc19x$der_dead90[which(ccc19x$record_id %in% temp)] <- 1
     # 
     # #2. 90-day mortality flag is set (baseline)
     # temp.ref2 <- which(ccc19x$mortality_90[temp.ref] == 0)
     # temp <- ccc19x$record_id[temp.ref[temp.ref2]]
     # ccc19x$der_dead90[which(ccc19x$record_id %in% temp)] <- 1
     # 
     # #3. 90-day mortality flag is set (follow-up)
     # temp.ref2 <- which(ccc19x$d90_vital_status[temp.ref] == 1)
     # temp <- ccc19x$record_id[temp.ref[temp.ref2]]
     # ccc19x$der_dead90[which(ccc19x$record_id %in% temp)] <- 1
     # 
     # #4. 90-day follow-up form is filled out as death
     # temp <- ccc19x$record_id[which(ccc19x$fu_weeks == 90 & (
     #   ccc19x$fu_reason == 3 |
     #     ccc19x$covid_19_status_fu == 3 |
     #     ccc19x$current_status_fu == 9 ))]
     # ccc19x$der_dead90[which(ccc19x$record_id %in% temp)] <- 1
     # 
     # #5. Follow-up form filled out as other and timing < 13 weeks
     # temp <- ccc19x$record_id[which(ccc19x$timing_of_report_weeks < 13 & (
     #   ccc19x$fu_reason == 3 |
     #     ccc19x$covid_19_status_fu == 3 |
     #     ccc19x$current_status_fu == 9 ))]
     # ccc19x$der_dead90[which(ccc19x$record_id %in% temp)] <- 1
     # 
     # #6. Days to death <= 90
     # temp <- ccc19x$record_id[which(ccc19x$der_days_to_death_combined <= 90)]
     # ccc19x$der_dead90[which(ccc19x$record_id %in% temp)] <- 1
     # 
     # #7. Rescind status if days to death > 90
     # temp <- ccc19x$record_id[which(ccc19x$der_days_to_death_combined > 90)]
     # ccc19x$der_dead90[which(ccc19x$record_id %in% temp)] <- 0
     # 
     # #8. Declare unknown if days to death cannot be calculated and mortality flag not set
     # temp <- ccc19x$record_id[which(ccc19x$der_deadbinary == 1 & ccc19x$der_dead90 == 0 &
     #                                  (is.na(ccc19x$mortality_90)|ccc19x$mortality_90 == 99) & 
     #                                  (is.na(ccc19x$d90_vital_status)|ccc19x$d90_vital_status == 99) & #Mortality flags
     #                                  (is.na(ccc19x$der_days_to_death_combined) | ccc19x$der_days_to_death_combined == 9999))]
     # flag <- rep(T, length(temp))
     # for(i in 1:length(temp))
     # {
     #   temp.ref <- which(ccc19x$record_id == temp[i])
     #   temp2 <- c(ccc19x$hosp_los[temp.ref],
     #              ccc19x$hosp_los_2[temp.ref],
     #              ccc19x$hosp_los_fu[temp.ref],
     #              ccc19x$hosp_los_fu_2[temp.ref],
     #              ccc19x$icu_los[temp.ref],
     #              ccc19x$icu_los_fu[temp.ref])
     #   temp2 <- temp2[!is.na(temp2)]
     #   temp3 <- ccc19x$mortality_90[temp.ref] == 1
     #   temp3 <- temp3[!is.na(temp3)]
     #   if(length(temp2) > 0)
     #   {
     #     temp2 <- sum(temp2)
     #     if(temp2 > 90) flag[i] <- F
     #   }
     #   if(length(temp3) > 0)
     #     if(any(temp3)) flag[i] <- F
     # }
     # temp <- temp[flag]
     # 
     # ccc19x$der_dead90[which(ccc19x$record_id %in% temp)] <- 99
     # 
     # #9. Recover some patients with unknown or missing days to death
     # #Estimate days to death for patients with missing/unknown days and retrospective reporting (baseline form only)
     # #Estimate as the maximum length of time possible based on the interval
     # temp <- ccc19x$record_id[which(ccc19x$der_dead90 %in% c(0,99) &
     #                                  (ccc19x$der_days_to_death == 9999|is.na(ccc19x$der_days_to_death)) &
     #                                  ccc19x$current_status_retro == 3)]
     # if(length(temp) > 0)
     # {
     #   for(i in 1:length(temp))
     #   {
     #     temp.ref <- which(ccc19x$record_id == temp[i])
     #     temp2 <- ccc19x$covid_19_dx_interval[temp.ref]
     #     temp2 <- temp2[!is.na(temp2)]
     #     if(temp2 %in% 1:5) ccc19x$der_dead90[temp.ref] <- 1
     #   }
     # }
     # 
     # #10. Rescind unknown status if 180-day follow-up form is filled out as death and is not the first f/u form
     # temp <- ccc19x$record_id[which(ccc19x$fu_weeks %in% c(180) & 
     #                                  ccc19x$redcap_repeat_instance > 1 &
     #                                  (ccc19x$fu_reason == 3 |
     #                                     ccc19x$covid_19_status_fu == 3 |
     #                                     ccc19x$current_status_fu == 9 ) &
     #                                  ccc19x$der_dead90 == 99)]
     # ccc19x$der_dead90[which(ccc19x$record_id %in% temp)] <- 0
     # 
     # ccc19x$der_dead90 <- as.factor(ccc19x$der_dead90)
     # 
     # temp <- summary(ccc19x$der_dead90[ccc19x$redcap_repeat_instrument == ''])
     # temp.var.log <- data.frame(name = 'der_dead90',
     #                            timestamp = Sys.time(),
     #                            values = paste(paste(names(temp), temp, sep = ': '), collapse = '; '),
     #                            stringsAsFactors = F)
     # var.log <- rbind(var.log, temp.var.log)
     
     # #zDep23. Dead within 180 days (default is alive)
     # ccc19x$der_dead180 <- 0
     # 
     # temp.ref <- which(ccc19x$der_deadbinary == 1 & ccc19x$redcap_repeat_instrument == '')
     # 
     # #1. Calculated time to death is <= 180 days
     # temp.diff <- difftime(ccc19x$meta_righttime, ccc19x$meta_lefttime_lb, units = 'days')
     # temp.ref2 <- which(temp.diff[temp.ref] <= 180)
     # temp <- ccc19x$record_id[temp.ref[temp.ref2]]
     # ccc19x$der_dead180[which(ccc19x$record_id %in% temp)] <- 1
     # 
     # #2. 180-day mortality flag is set (baseline)
     # temp.ref2 <- which(ccc19x$mortality_180[temp.ref] == 0)
     # temp <- ccc19x$record_id[temp.ref[temp.ref2]]
     # ccc19x$der_dead180[which(ccc19x$record_id %in% temp)] <- 1
     # 
     # #3. 180-day mortality flag is set (follow-up)
     # temp.ref2 <- which(ccc19x$d180_vital_status[temp.ref] == 1)
     # temp <- ccc19x$record_id[temp.ref[temp.ref2]]
     # ccc19x$der_dead180[which(ccc19x$record_id %in% temp)] <- 1
     # 
     # #4. 180-day follow-up form is filled out as death
     # temp <- ccc19x$record_id[which(ccc19x$fu_weeks == 180 & (
     #   ccc19x$fu_reason == 3 |
     #     ccc19x$covid_19_status_fu == 3 |
     #     ccc19x$current_status_fu == 9 ))]
     # ccc19x$der_dead180[which(ccc19x$record_id %in% temp)] <- 1
     # 
     # #5. Follow-up form filled out as other and timing < 26 weeks
     # temp <- ccc19x$record_id[which(ccc19x$timing_of_report_weeks < 26 & (
     #   ccc19x$fu_reason == 3 |
     #     ccc19x$covid_19_status_fu == 3 |
     #     ccc19x$current_status_fu == 9 ))]
     # ccc19x$der_dead180[which(ccc19x$record_id %in% temp)] <- 1
     # 
     # #6. Days to death <= 180
     # temp <- ccc19x$record_id[which(ccc19x$der_days_to_death_combined <= 180)]
     # ccc19x$der_dead180[which(ccc19x$record_id %in% temp)] <- 1
     # 
     # #7. Rescind status if days to death > 180
     # temp <- ccc19x$record_id[which(ccc19x$der_days_to_death_combined > 180)]
     # ccc19x$der_dead180[which(ccc19x$record_id %in% temp)] <- 0
     # 
     # #8. Declare unknown if days to death cannot be calculated and mortality flag not set
     # temp <- ccc19x$record_id[which(ccc19x$der_deadbinary == 1 & ccc19x$der_dead180 == 0 &
     #                                  (is.na(ccc19x$mortality_180)|ccc19x$mortality_180 == 99) & 
     #                                  (is.na(ccc19x$d180_vital_status)|ccc19x$d180_vital_status == 99) & #Mortality flags
     #                                  (is.na(ccc19x$der_days_to_death_combined) | ccc19x$der_days_to_death_combined == 9999))]
     # flag <- rep(T, length(temp))
     # for(i in 1:length(temp))
     # {
     #   temp.ref <- which(ccc19x$record_id == temp[i])
     #   temp2 <- c(ccc19x$hosp_los[temp.ref],
     #              ccc19x$hosp_los_2[temp.ref],
     #              ccc19x$hosp_los_fu[temp.ref],
     #              ccc19x$hosp_los_fu_2[temp.ref],
     #              ccc19x$icu_los[temp.ref],
     #              ccc19x$icu_los_fu[temp.ref])
     #   temp2 <- temp2[!is.na(temp2)]
     #   temp3 <- ccc19x$mortality_180[temp.ref] == 1
     #   temp3 <- temp3[!is.na(temp3)]
     #   if(length(temp2) > 0)
     #   {
     #     temp2 <- sum(temp2)
     #     if(temp2 > 180) flag[i] <- F
     #   }
     #   if(length(temp3) > 0)
     #     if(any(temp3)) flag[i] <- F
     # }
     # temp <- temp[flag]
     # 
     # ccc19x$der_dead180[which(ccc19x$record_id %in% temp)] <- 99
     # 
     # #9. Recover some patients with unknown or missing days to death
     # #Estimate days to death for patients with missing/unknown days and retrospective reporting (baseline form only)
     # #Estimate as the maximum length of time possible based on the interval
     # temp <- ccc19x$record_id[which(ccc19x$der_dead180 %in% c(0,99) &
     #                                  (ccc19x$der_days_to_death == 9999|is.na(ccc19x$der_days_to_death)) &
     #                                  ccc19x$current_status_retro == 3)]
     # if(length(temp) > 0)
     # {
     #   for(i in 1:length(temp))
     #   {
     #     temp.ref <- which(ccc19x$record_id == temp[i])
     #     temp2 <- ccc19x$covid_19_dx_interval[temp.ref]
     #     temp2 <- temp2[!is.na(temp2)]
     #     if(temp2 %in% 1:6) ccc19x$der_dead180[temp.ref] <- 1
     #   }
     # }
     # 
     # ccc19x$der_dead180 <- as.factor(ccc19x$der_dead180)
     # summary(ccc19x$der_dead180[ccc19x$redcap_repeat_instrument == ''])
     
   }
}

#Condensed output for troubleshooting
temp.ref <- grep(colnames(ccc19x), pattern = '^der_')
out <- data.frame(variable = colnames(ccc19x)[temp.ref], values = '', stringsAsFactors = F)
for(i in 1:length(temp.ref))
{
  if(is.factor(ccc19x[,temp.ref[i]]))
  {
    temp <- summary(ccc19x[ccc19x$redcap_repeat_instrument == '',temp.ref[i]])
    out$values[i] <- paste(paste(names(temp), temp, sep = ': '), collapse = '; ')
  } else if(is.integer(ccc19x[,temp.ref[i]]))
  {
    temp <- summary(factor(ccc19x[ccc19x$redcap_repeat_instrument == '',temp.ref[i]]))
    out$values[i] <- paste(paste(names(temp), temp, sep = ': '), collapse = '; ')
  } else out$values[i] <- 'Not a factor or an integer variable'
}
write.csv(out, file = paste(stamp, '.summary of derived variables results.csv', sep = ''), row.names = F)

#Save here
save(ccc19x, stamp, var.log, file = paste(stamp, '.',
                              suffix,
                              ' saved on ',
                              gsub(Sys.time(), pattern = ':', replacement = '-'),
                              '.RData', sep = ''))
