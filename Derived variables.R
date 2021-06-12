###Code to create derived variables
setwd("~/Box Sync/CCC19 data")

#Create an object that is a copy of the original data
ccc19x <- foo

#Define the desired suffix for the save function
suffix <- 'data with derived variables for analysis'

##DERIVED VARIABLES to recode:
{
  #########
  #Outcomes
  #########
  {
    
    "deadbinary"
    ##O1. Derived dead/alive variable (0=alive, 1=dead, 99=unknown)
    ccc19x$der_deadbinary <- NA
    
    #Alive on primary form
    ccc19x$der_deadbinary[which(ccc19x$current_status %in% 1:8 |
                                  ccc19x$current_status_retro %in% c("1", "1b") | 
                                  ccc19x$current_status_v2 %in% c("1", "1b", "2")) ] <- 0
    
    #Dead on primary form
    ccc19x$der_deadbinary[which(ccc19x$current_status_retro == 3 | 
                                  ccc19x$current_status_v2 == 3 | 
                                  ccc19x$current_status == 9)] <- 1
    
    #Unknown on primary form
    ccc19x$der_deadbinary[which(ccc19x$current_status_retro == 99)] <- 99
    
    #Alive on followup form
    temp.ref <- which(ccc19x$covid_19_status_fu %in% c('1','1b','2') | 
                        ccc19x$fu_reason %in% 1:2)
    
    ccc19x$der_deadbinary[temp.ref] <- 0
    
    #Dead on followup form
    temp.ref <- which(ccc19x$covid_19_status_fu ==3 | 
                        ccc19x$current_status_fu == 9 |
                        ccc19x$fu_reason == 3)
    
    ccc19x$der_deadbinary[temp.ref] <- 1
    
    #Unknown on followup form
    temp.ref <- which(ccc19x$covid_19_status_fu == 99)
    ccc19x$der_deadbinary[temp.ref] <- 99
    
    #Reconcile death status for each patient
    temp <- unique(ccc19x$record_id)
    for(i in 1:length(temp))
    {
      temp.ref <- which(ccc19x$record_id == temp[i])
      temp2 <- ccc19x$der_deadbinary[temp.ref]
      temp2 <- temp2[!is.na(temp2)]
      if(length(temp2) > 0)
      {
        if(any(temp2 == 1)) ccc19x$der_deadbinary[temp.ref] <- 1
        if(!any(temp2 == 1) & any(temp2 == 0)) ccc19x$der_deadbinary[temp.ref] <- 0
        if(!any(temp2 == 1) & !any(temp2 == 0) & any(temp2 == 99)) ccc19x$der_deadbinary[temp.ref] <- 99
      }
    }
    
    #Factor
    ccc19x$der_deadbinary <- as.factor(ccc19x$der_deadbinary)
    
    summary(ccc19x$der_deadbinary[ccc19x$redcap_repeat_instrument == ''])
    
    "hosp"
    #O2. Hospitalization
    ccc19x$der_hosp <- NA
    
    #Initial form
    
    #No
    ccc19x$der_hosp[which(ccc19x$hosp_status == 0| 
                            ccc19x$current_status %in% c(1,3)| #Outpatient or ER - new COVID-19 diagnosis
                            ccc19x$worst_status_clinical %in% c(0:3)|
                            ccc19x$worst_complications_severity___0 == 1)] <- 0
    
    #Yes
    ccc19x$der_hosp[which(ccc19x$hosp_status %in% c(1:3) | 
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
    summary(ccc19x$der_hosp[ccc19x$redcap_repeat_instrument == ''])
    
    #O2a. Hospitalization within first 30 days
    ccc19x$der_hosp_bl <- NA
    
    #Initial form
    
    #No
    ccc19x$der_hosp_bl[which(ccc19x$hosp_status == 0| 
                            ccc19x$current_status %in% c(1,3)| #Outpatient or ER - new COVID-19 diagnosis
                            ccc19x$worst_status_clinical %in% c(0:3)|
                              ccc19x$worst_complications_severity___0 == 1)] <- 0
    
    #Yes
    ccc19x$der_hosp_bl[which(ccc19x$hosp_status %in% c(1:3) | 
                               ccc19x$current_status %in% c(5:8)|
                               ccc19x$c19_anticoag_reason___3 == 1| #Can only be true if patient was hospitalized
                               ccc19x$worst_status_clinical %in% c(5:8)| 
                               ccc19x$labs == '2a'| #Labs drawn at time of hospitalization
                               ccc19x$current_status_clinical %in% c(4:8))] <- 1
    
    #Interventions that could only happen in a hospital
    ccc19x$der_hosp_bl[which(ccc19x$resp_failure_tx %in% 2:6 |
                               ccc19x$resp_failure_tx_fu %in% 2:6)] <- 1
    
    #Unknown
    ccc19x$der_hosp_bl[which((ccc19x$hosp_status == 99 |
                             ccc19x$worst_status_clinical == 99) & 
                            is.na(ccc19x$der_hosp_bl))] <- 99
    
    #Followup ONLY if less than or equal to 30 days and for hospitalization
    temp <- ccc19x$record_id[which(ccc19x$fu_reason == 1 & 
                                     ((ccc19x$fu_weeks == 'OTH' & ccc19x$timing_of_report_weeks <= 4)|
                                        ccc19x$fu_weeks == 30))]
    ccc19x$der_hosp_bl[which(ccc19x$record_id %in% temp & ccc19x$redcap_repeat_instrument == '')] <- 1
    
    #Factor
    ccc19x$der_hosp_bl <- as.factor(ccc19x$der_hosp_bl)
    summary(ccc19x$der_hosp_bl[ccc19x$redcap_repeat_instrument == ''])
    
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
    ccc19x$der_ICU[which((ccc19x$hosp_status == 99 |
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
    summary(ccc19x$der_ICU[ccc19x$redcap_repeat_instrument == ''])
    
    #O3a. Direct admission to ICU at baseline
    ccc19x$der_ICU_direct <- ccc19x$der_ICU
    ccc19x$der_ICU_direct[which(ccc19x$hosp_status != 3 & ccc19x$der_ICU_direct == 1)] <- 0
    summary(ccc19x$der_ICU_direct[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_mv[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_ICU_mv_pressors[ccc19x$redcap_repeat_instrument == ''])
    
    "recovered"                           
    #O5. Derived recovery variable
    ccc19x$der_recovered <- NA
    
    #initial form
    ccc19x$der_recovered[which(ccc19x$current_status_v2 %in% c("1", "1b") |
                                 ccc19x$current_status_retro %in% c("1", "1b"))] <- 1
    ccc19x$der_recovered[which(is.na(ccc19x$der_recovered))] <- 0
    
    #followup form
    ccc19x$furecovered <- NA
    ccc19x$furecovered[which(ccc19x$covid_19_status_fu %in% c("1", "1b"))] <- 1
    furecovereds <- ccc19x$record_id[which(ccc19x$furecovered == 1)]
    ccc19x$der_recovered[which(ccc19x$record_id %in% furecovereds)] <- 1
    rm(furecovereds)
    
    #Factor
    ccc19x$der_recovered <- as.factor(ccc19x$der_recovered)
    
    length(unique(ccc19x$record_id))
    
    #O6. Combined days to death
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
    
    summary(ccc19x$der_days_to_death_combined[which(ccc19x$der_deadbinary == 1)])
    
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
    summary(ccc19x$der_o2_ever[ccc19x$redcap_repeat_instrument == ''])
    
    
    #O8a. Severe composite outcome - mechanical ventilation, severe illness requiring hospitalization, intensive care unit (ICU) requirement, or death
    ccc19x$der_severe <- NA
    
    #Present
    ccc19x$der_severe[which(ccc19x$der_deadbinary == 1)] <- 1
    ccc19x$der_severe[which(ccc19x$der_mv == 1)] <- 1
    ccc19x$der_severe[which(ccc19x$der_ICU == 1)] <- 1
    ccc19x$der_severe[which(ccc19x$severity_of_covid_19_v2 == 3)] <- 1
    ccc19x$der_severe[which(ccc19x$current_status_clinical %in% 6:8)] <- 1
    ccc19x$der_severe[which(ccc19x$worst_status_clinical %in% 6:8)] <- 1
    ccc19x$der_severe[which(ccc19x$current_status_clinical %in% 6:8)] <- 1
    
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
    summary(ccc19x$der_severe[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_severe2[ccc19x$redcap_repeat_instrument == ''])
    
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
    #ccc19x$der_severe3 <- relevel(ccc19x$der_severe3, ref = '1')
    summary(ccc19x$der_severe3[ccc19x$redcap_repeat_instrument == ''])
   
    
    
  }
  print('Outcomes completed')
  
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
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk','c19_complications_pulm___unk'))
      for(i in which(is.na(ccc19x$der_PE_comp) & ccc19x$redcap_repeat_instrument == ''))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_PE_comp[i] <- 99
      
      #Followup
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk','c19_complications_pulm_fu___unk'))
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
      summary(ccc19x$der_PE_comp[ccc19x$redcap_repeat_instrument == ''])
      
      #Comp01a. PE complications within 90 days (3 months)
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
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk','c19_complications_pulm___unk'))
      for(i in which(is.na(ccc19x$der_PE_comp_within_3mo) & ccc19x$redcap_repeat_instrument == ''))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_PE_comp_within_3mo[i] <- 99
      
      #Followup
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk','c19_complications_pulm_fu___unk'))
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
      summary(ccc19x$der_PE_comp_within_3mo[ccc19x$redcap_repeat_instrument == ''])
      
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
      summary(ccc19x$der_SVT_comp[ccc19x$redcap_repeat_instrument == ''])
      
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
      summary(ccc19x$der_DVT_comp[ccc19x$redcap_repeat_instrument == ''])
      
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
      summary(ccc19x$der_DVT_comp_within_3mo[ccc19x$redcap_repeat_instrument == ''])
      
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
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk','c19_complications_other___unk'))
      for(i in which(is.na(ccc19x$der_thrombosis_NOS_comp) & ccc19x$redcap_repeat_instrument == ''))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_thrombosis_NOS_comp[i] <- 99
      
      #Followup
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk','c19_complications_other_fu___unk'))
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
      summary(ccc19x$der_thrombosis_NOS_comp[ccc19x$redcap_repeat_instrument == ''])
      
      #Comp04a. thrombosis_NOS complications within 90 days (3 months)
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
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card___unk','c19_complications_other___unk'))
      for(i in which(is.na(ccc19x$der_thrombosis_NOS_comp_within_3mo) & ccc19x$redcap_repeat_instrument == ''))
        if(all(ccc19x[i,temp.ref] == 1)) ccc19x$der_thrombosis_NOS_comp_within_3mo[i] <- 99
      
      #Followup
      temp.ref <- which(colnames(ccc19x) %in% c('c19_complications_card_fu___unk','c19_complications_other_fu___unk'))
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
      temp.ref <- which(colnames(ccc19x) %in% c('der_PE_comp','der_DVT_comp','der_thrombosis_NOS_comp'))
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
      summary(ccc19x$der_VTE_comp[ccc19x$redcap_repeat_instrument == ''])
      
      #Comp05a. Combined VTE indicator (excluding SVT and thrombosis NOS)
      ccc19x$der_VTE_comp_v2 <- NA
      
      #Any complication
      temp.ref <- which(colnames(ccc19x) %in% c('der_PE_comp','der_DVT_comp'))
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
      summary(ccc19x$der_VTE_comp_v2[ccc19x$redcap_repeat_instrument == ''])
      
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
      summary(ccc19x$der_VTE_comp_within_3mo[ccc19x$redcap_repeat_instrument == ''])
      
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
      summary(ccc19x$der_ATE_comp[ccc19x$redcap_repeat_instrument == ''])
      
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
      summary(ccc19x$der_ATE_comp_within_3mo[ccc19x$redcap_repeat_instrument == ''])
      
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
      summary(ccc19x$der_stroke_comp[ccc19x$redcap_repeat_instrument == ''])
      
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
      summary(ccc19x$der_stroke_comp_within_3mo[ccc19x$redcap_repeat_instrument == ''])
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
    
    #Comp10. Worst severity of complications 
    ccc19x$der_worst <- NA
    
    #Serious
    temp <- ccc19x$record_id[which(ccc19x$severity_of_covid_19_v2 == 3|
                                     ccc19x$complications_severity___3 == 1|
                                     ccc19x$worst_complications_severity___3 == 1|
                                     ccc19x$complications_severity_fu___3 == 1)]
    ccc19x$der_worst[ccc19x$record_id %in% temp] <- 3
    
    #Moderate
    temp <- ccc19x$record_id[which(ccc19x$severity_of_covid_19_v2 == 2|
                                     (ccc19x$complications_severity___2 == 1 & ccc19x$complications_severity___3 == 0)|
                                     (ccc19x$worst_complications_severity___2 == 1 & ccc19x$worst_complications_severity___3 == 0)|
                                     (ccc19x$complications_severity_fu___2 == 1 & ccc19x$complications_severity_fu___3 == 0))] 
    ccc19x$der_worst[ccc19x$record_id %in% temp & is.na(ccc19x$der_worst)] <- 2
    
    #Mild
    temp <- ccc19x$record_id[which(ccc19x$severity_of_covid_19_v2 == 1|
                                     (ccc19x$complications_severity___1 == 1 & ccc19x$complications_severity___2 == 0 & ccc19x$complications_severity___3 == 0)|
                                     (ccc19x$worst_complications_severity___1 == 1 & ccc19x$worst_complications_severity___2 == 0 & ccc19x$worst_complications_severity___3 == 0)|
                                     (ccc19x$complications_severity_fu___1 == 1 & ccc19x$complications_severity_fu___2 == 0 & ccc19x$complications_severity_fu___3 == 0))] 
    ccc19x$der_worst[ccc19x$record_id %in% temp & is.na(ccc19x$der_worst)] <- 1
    
    #Other/unknown
    temp <- ccc19x$record_id[which(ccc19x$severity_of_covid_19_v2 == 99|
                                     ((ccc19x$complications_severity___oth ==1|ccc19x$complications_severity___99 ==1) & 
                                        ccc19x$complications_severity___1 == 0 & ccc19x$complications_severity___2 == 0 & ccc19x$complications_severity___3 == 0) |
                                     ((ccc19x$worst_complications_severity___oth ==1|ccc19x$worst_complications_severity___99 ==1) & 
                                        ccc19x$worst_complications_severity___1 == 0 & ccc19x$worst_complications_severity___2 == 0 & ccc19x$worst_complications_severity___3 == 0) |
                                     ((ccc19x$complications_severity_fu___oth ==1|ccc19x$complications_severity_fu___99 ==1) & 
                                        ccc19x$complications_severity_fu___1 == 0 & ccc19x$complications_severity_fu___2 == 0 & ccc19x$complications_severity_fu___3 == 0))]
    ccc19x$der_worst[ccc19x$record_id %in% temp & is.na(ccc19x$der_worst)] <- 99
    
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
    summary(ccc19x$der_MOF_comp[ccc19x$redcap_repeat_instrument == ''])
    
    
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
    summary(ccc19x$der_sepsis_comp[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_bleeding_comp[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_DIC_comp[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_resp_failure_comp[ccc19x$redcap_repeat_instrument == ''])
    
    
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
    summary(ccc19x$der_pneumonitis_comp[ccc19x$redcap_repeat_instrument == ''])
    
    
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
    summary(ccc19x$der_pneumonia_comp[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_ARDS_comp[ccc19x$redcap_repeat_instrument == ''])
    
    
    
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
    summary(ccc19x$der_pleural_eff_comp[ccc19x$redcap_repeat_instrument == ''])
    
    
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
    summary(ccc19x$der_empyema_comp[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_hotn_comp[ccc19x$redcap_repeat_instrument == ''])
    
    
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
    summary(ccc19x$der_MI_comp[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_card_isch_comp[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_AFib_comp[ccc19x$redcap_repeat_instrument == ''])
    
    
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
    summary(ccc19x$der_VF_comp[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_arry_oth_comp[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_CMY_comp[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_CHF_comp[ccc19x$redcap_repeat_instrument == ''])
    
    
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
    summary(ccc19x$der_AHI_comp[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_ascites_comp[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_BO_comp[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_bowelPerf_comp[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_ileus_comp[ccc19x$redcap_repeat_instrument == ''])
    
    
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
    summary(ccc19x$der_peritonitis_comp[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_AKI_comp[ccc19x$redcap_repeat_instrument == ''])
    
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
    ccc19x$der_coinfection_viral[which(ccc19x$coinfection_yn == 0 & is.na(ccc19x$der_coinfection_viral))] <- 0
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(any(ccc19x[i,temp.ref] == 1) & is.na(ccc19x$der_coinfection_viral[i])) ccc19x$der_coinfection_viral[i] <- 0 
    
    #Unknown
    ccc19x$der_coinfection_viral[which((ccc19x$coinfection_yn == 99|ccc19x$coinfection___unk == 1) &
                                         is.na(ccc19x$der_coinfection_viral))] <- 99
    
    ccc19x$der_coinfection_viral <- factor(ccc19x$der_coinfection_viral)
    summary(ccc19x$der_coinfection_viral[ccc19x$redcap_repeat_instrument == ''])
    
    #Comp38 Bacterial co-infection within +/- 2 weeks of COVID-19 diagnosis
    ccc19x$der_coinfection_bacterial <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'coinfection___') & !grepl(colnames(ccc19x), pattern = 'unk|none|442376007'))
    
    #Yes
    ccc19x$der_coinfection_bacterial[which(ccc19x$coinfection___409822003 == 1|
                                             ccc19x$coinfection___8745002 == 1|
                                             ccc19x$coinfection___233607000 == 1|
                                             ccc19x$coinfection___81325006 == 1)] <- 1
    
    #No
    ccc19x$der_coinfection_bacterial[which(ccc19x$coinfection_yn == 0 & is.na(ccc19x$der_coinfection_bacterial))] <- 0
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(any(ccc19x[i,temp.ref] == 1) & is.na(ccc19x$der_coinfection_bacterial[i])) ccc19x$der_coinfection_bacterial[i] <- 0 
    
    #Unknown
    ccc19x$der_coinfection_bacterial[which((ccc19x$coinfection_yn == 99|ccc19x$coinfection___unk == 1) &
                                             is.na(ccc19x$der_coinfection_bacterial))] <- 99
    
    ccc19x$der_coinfection_bacterial <- factor(ccc19x$der_coinfection_bacterial)
    summary(ccc19x$der_coinfection_bacterial[ccc19x$redcap_repeat_instrument == ''])
    
    #Comp38a Gram Positive Bacterial co-infection within +/- 2 weeks of COVID-19 diagnosis
    ccc19x$der_coinfection_bact_gram_pos <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'coinfection___') & !grepl(colnames(ccc19x), pattern = 'unk|none|442376007'))
    
    #Yes
    ccc19x$der_coinfection_bact_gram_pos[which(ccc19x$coinfection___8745002 == 1|
                                                 ccc19x$coinfection___233607000 == 1)] <- 1
    
    #No
    ccc19x$der_coinfection_bact_gram_pos[which(ccc19x$coinfection_yn == 0 & is.na(ccc19x$der_coinfection_bact_gram_pos))] <- 0
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
    ccc19x$der_coinfection_bact_gram_neg[which(ccc19x$coinfection_yn == 0 & is.na(ccc19x$der_coinfection_bact_gram_neg))] <- 0
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(any(ccc19x[i,temp.ref] == 1) & is.na(ccc19x$der_coinfection_bact_gram_neg[i])) ccc19x$der_coinfection_bact_gram_neg[i] <- 0 
    
    #Unknown
    ccc19x$der_coinfection_bact_gram_neg[which((ccc19x$coinfection_yn == 99|ccc19x$coinfection___unk == 1) &
                                                 is.na(ccc19x$der_coinfection_bact_gram_neg))] <- 99
    
    ccc19x$der_coinfection_bact_gram_neg <- factor(ccc19x$der_coinfection_bact_gram_neg)
    summary(ccc19x$der_coinfection_bact_gram_neg[ccc19x$redcap_repeat_instrument == ''])
    
    #Comp39 Fungal co-infection within +/- 2 weeks of COVID-19 diagnosis
    ccc19x$der_coinfection_fungal <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'coinfection___') & !grepl(colnames(ccc19x), pattern = 'unk|none|442376007'))
    
    #Yes
    ccc19x$der_coinfection_fungal[which(ccc19x$coinfection___414561005 == 1|
                                          ccc19x$coinfection___2429008 == 1|
                                          ccc19x$coinfection___709601002 == 1)] <- 1
    
    #No
    ccc19x$der_coinfection_fungal[which(ccc19x$coinfection_yn == 0 & is.na(ccc19x$der_coinfection_fungal))] <- 0
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(any(ccc19x[i,temp.ref] == 1) & is.na(ccc19x$der_coinfection_fungal[i])) ccc19x$der_coinfection_fungal[i] <- 0 
    
    #Unknown
    ccc19x$der_coinfection_fungal[which((ccc19x$coinfection_yn == 99|ccc19x$coinfection___unk == 1) &
                                          is.na(ccc19x$der_coinfection_fungal))] <- 99
    
    ccc19x$der_coinfection_fungal <- factor(ccc19x$der_coinfection_fungal)
    summary(ccc19x$der_coinfection_fungal[ccc19x$redcap_repeat_instrument == ''])
    
    #Comp40 Any co-infection 
    ccc19x$der_coinfection_any <- NA
    
    #Yes
    ccc19x$der_coinfection_any[which(ccc19x$der_coinfection_bacterial == 1|
                                       ccc19x$der_coinfection_fungal == 1|
                                       ccc19x$der_coinfection_viral == 1)] <- 1
    
    #No
    ccc19x$der_coinfection_any[which(ccc19x$der_coinfection_bacterial == 0 &
                                       ccc19x$der_coinfection_fungal == 0 &
                                       ccc19x$der_coinfection_viral == 0)] <- 0
    
    #Unknown
    ccc19x$der_coinfection_any[which((ccc19x$der_coinfection_bacterial == 99|
                                        ccc19x$der_coinfection_fungal == 99|
                                        ccc19x$der_coinfection_viral == 99) & is.na(ccc19x$der_coinfection_any))] <- 99
    
    ccc19x$der_coinfection_any <- factor(ccc19x$der_coinfection_any)
    summary(ccc19x$der_coinfection_any[ccc19x$redcap_repeat_instrument == ''])
    
    
  
    
    rm(master_comp)
    }
  print('Complications completed')
  
  ##################
  #Time measurements
  ##################
  {
    #Data fix
    ccc19x$ts_5 <- gsub(ccc19x$ts_5, pattern = '/20 ', replacement = '/2020 ')
    
    #T1 & T2. Time of last known followup (if alive) or to death (if dead) in days
    ccc19x$meta_lefttime <- as.POSIXlt("2099-12-31 00:00:00 CDT")
    ccc19x$meta_righttime <- as.POSIXlt("2099-12-31 00:00:00 CDT")
    ccc19x$meta_righttime[ccc19x$ts_0 != ''] <- as.POSIXct(ccc19x$ts_0[ccc19x$ts_0 != ''])
    
    #First initial form
    temp.ref <- which(ccc19x$covid_19_dx_interval == 1)
    ccc19x$meta_lefttime[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 7*24*60*60
    
    temp.ref <- which(ccc19x$covid_19_dx_interval == 2)
    ccc19x$meta_lefttime[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 14*24*60*60
    
    temp.ref <- which(ccc19x$covid_19_dx_interval == 3)
    ccc19x$meta_lefttime[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 28*24*60*60
    
    temp.ref <- which(ccc19x$covid_19_dx_interval == 4)
    ccc19x$meta_lefttime[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 56*24*60*60
    
    temp.ref <- which(ccc19x$covid_19_dx_interval == 5)
    ccc19x$meta_lefttime[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 84*24*60*60
    
    temp.ref <- which(ccc19x$covid_19_dx_interval == 6)
    ccc19x$meta_lefttime[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 180*24*60*60
    
    temp.ref <- which(ccc19x$covid_19_dx_interval == 8)
    ccc19x$meta_lefttime[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 270*24*60*60
    
    temp.ref <- which(ccc19x$covid_19_dx_interval == 9)
    ccc19x$meta_lefttime[temp.ref] <- ccc19x$meta_righttime[temp.ref] - 360*24*60*60
    
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
    
    #T3. Median f/u
    ccc19x$der_days_fu <- NA
    pts <- unique(ccc19x$record_id)
    
    #Middle of follow-up intervals (from covid_19_dx_interval)
    fu <- c(7/2, (7+14)/2, (14+28)/2, (28+56)/2, (56+84)/2, (90+180)/2, 180, (180+270)/2, (270+360)/2, 365)
    
    for(i in 1:length(pts))
    {
      temp.ref <- which(ccc19x$record_id == pts[i])
      #Check that f/u form has been completed, remove otherwise
      temp <- ccc19x$followup_complete[temp.ref]
      temp.ref <- temp.ref[which(is.na(temp) | temp == 2)]
      if(length(temp.ref) == 1) #No follow-up forms
      {
        #Check if days to death has data, if so use it
        if(!is.na(ccc19x$der_days_to_death_combined[temp.ref])) 
        {
          if(ccc19x$der_days_to_death_combined[temp.ref] != 9999) 
          {
            ccc19x$der_days_fu[temp.ref] <- ccc19x$der_days_to_death_combined[temp.ref]
          } else #Default is the median of the time interval of diagnosis
            ccc19x$der_days_fu[temp.ref] <- fu[ccc19x$covid_19_dx_interval[temp.ref]]
        } else
        {
          #Default is the median of the time interval of diagnosis
          ccc19x$der_days_fu[temp.ref] <- fu[ccc19x$covid_19_dx_interval[temp.ref]]
          
          # #Check that LOS aren't longer than this
          # if(!is.na(ccc19x$der_days_fu[temp.ref]))
          # {
          #   temp <- ccc19x[temp.ref,c('hosp_los','hosp_los_2','icu_los')]
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
        #Check if days to death has data, if so use it
        if(length(temp) == 1) 
        {
          if(temp != 9999) 
          {
            ccc19x$der_days_fu[temp.ref] <- temp
          }
        } else
        {
          temp <- ccc19x$fu_weeks[temp.ref]
          if(any(temp == 'OTH'))
          {
            temp[temp == 'OTH'] <- 7*ccc19x$timing_of_report_weeks[temp.ref[which(temp == 'OTH')]]
          }
          if(length(temp[temp != '']) > 0) ccc19x$der_days_fu[temp.ref] <- max(as.numeric(temp[temp != '']))
        }
        
        #If patient is deceased and days are missing, check the mortality variables for floor adjustment
        temp <- any(ccc19x$der_deadbinary[temp.ref] == 1) & any(ccc19x$der_days_fu[temp.ref] > 30) & 
          any(ccc19x$d30_vital_status[temp.ref] == 1) & any(is.na(ccc19x$der_days_to_death_combined[temp.ref])|ccc19x$der_days_to_death_combined[temp.ref] == 9999)
        if(!is.na(temp) & temp) ccc19x$der_days_fu[temp.ref] <- 30
        
        temp <- any(ccc19x$der_deadbinary[temp.ref] == 1) & any(ccc19x$der_days_fu[temp.ref] > 90) & 
          any(ccc19x$d90_vital_status[temp.ref] == 1) & any(is.na(ccc19x$der_days_to_death_combined[temp.ref])|ccc19x$der_days_to_death_combined[temp.ref] == 9999)
        if(!is.na(temp) & temp) ccc19x$der_days_fu[temp.ref] <- 90
        
        temp <- any(ccc19x$der_deadbinary[temp.ref] == 1) & any(ccc19x$der_days_fu[temp.ref] > 180) & 
          any(ccc19x$d180_vital_status[temp.ref] == 1) & any(is.na(ccc19x$der_days_to_death_combined[temp.ref])|ccc19x$der_days_to_death_combined[temp.ref] == 9999)
        if(!is.na(temp) & temp) ccc19x$der_days_fu[temp.ref] <- 180
      }
    }
    
    summary(ccc19x$der_days_fu[ccc19x$redcap_repeat_instrument == ''])
    
    #T4 & T5 Median f/u in days anchored to actual dates
    ccc19x$meta_lefttime2 <- as.POSIXlt("2099-12-31 00:00:00 CDT")
    ccc19x$meta_righttime2 <- as.POSIXlt("2099-12-31 00:00:00 CDT")
    ccc19x$meta_righttime2[ccc19x$ts_0 != ''] <- as.POSIXct(ccc19x$ts_0[ccc19x$ts_0 != ''])
    
    #First initial form
    temp.ref <- which(ccc19x$covid_19_dx_interval == 1)
    ccc19x$meta_lefttime2[temp.ref] <- ccc19x$meta_righttime2[temp.ref] - 7*24*60*60/2
    
    temp.ref <- which(ccc19x$covid_19_dx_interval == 2)
    ccc19x$meta_lefttime2[temp.ref] <- ccc19x$meta_righttime2[temp.ref] - (7+14)*24*60*60/2
    
    temp.ref <- which(ccc19x$covid_19_dx_interval == 3)
    ccc19x$meta_lefttime2[temp.ref] <- ccc19x$meta_righttime2[temp.ref] - (14+28)*24*60*60/2
    
    temp.ref <- which(ccc19x$covid_19_dx_interval == 4)
    ccc19x$meta_lefttime2[temp.ref] <- ccc19x$meta_righttime2[temp.ref] - (28+56)*24*60*60/2
    
    temp.ref <- which(ccc19x$covid_19_dx_interval == 5)
    ccc19x$meta_lefttime2[temp.ref] <- ccc19x$meta_righttime2[temp.ref] - (56+84)*24*60*60/2
    
    temp.ref <- which(ccc19x$covid_19_dx_interval == 6)
    ccc19x$meta_lefttime2[temp.ref] <- ccc19x$meta_righttime2[temp.ref] - (90+180)*24*60*60/2
    
    temp.ref <- which(ccc19x$covid_19_dx_interval == 8)
    ccc19x$meta_lefttime2[temp.ref] <- ccc19x$meta_righttime2[temp.ref] - (180+270)*24*60*60/2
    
    temp.ref <- which(ccc19x$covid_19_dx_interval == 9)
    ccc19x$meta_lefttime2[temp.ref] <- ccc19x$meta_righttime2[temp.ref] - (270+360)*24*60*60/2
    
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
        ccc19x$meta_righttime2[temp.ref] <- temp.time[1]
      }
    }
    
    #T6. 30-day follow-up available (0 = no; 1 = yes; 99 = unknown)
    ccc19x$der_d30 <- NA 
    
    #30-day question filled out
    ccc19x$der_d30[which(ccc19x$mortality == 1)] <- 1
    ccc19x$der_d30[which(ccc19x$mortality %in% c(0,88))] <- 0
    ccc19x$der_d30[which(ccc19x$mortality == 99)] <- 99
    
    #30-day or 90-day f/u form filled out
    temp <- ccc19x$record_id[which(ccc19x$fu_weeks %in% c(30,90))]
    ccc19x$der_d30[which(ccc19x$record_id %in% temp)] <- 1
    
    #Other f/u form with weeks > 4 filled out
    temp <- ccc19x$record_id[which(ccc19x$fu_weeks == 'OTH' & ccc19x$timing_of_report_weeks > 4)]
    ccc19x$der_d30[which(ccc19x$record_id %in% temp)] <- 1
    
    #T7 & T8 Calculated time from diagnosis has to be at least 30 days
    ccc19x$meta_lefttime3 <- as.POSIXlt("2099-12-31 00:00:00 CDT")
    ccc19x$meta_righttime3 <- as.POSIXlt("2099-12-31 00:00:00 CDT")
    ccc19x$meta_righttime3[ccc19x$ts_0 != ''] <- as.POSIXct(ccc19x$ts_0[ccc19x$ts_0 != ''])
    
    #First initial form
    temp.ref <- which(ccc19x$covid_19_dx_interval == 1)
    ccc19x$meta_lefttime3[temp.ref] <- ccc19x$meta_righttime3[temp.ref] - 0*24*60*60
    
    temp.ref <- which(ccc19x$covid_19_dx_interval == 2)
    ccc19x$meta_lefttime3[temp.ref] <- ccc19x$meta_righttime3[temp.ref] - 7*24*60*60
    
    temp.ref <- which(ccc19x$covid_19_dx_interval == 3)
    ccc19x$meta_lefttime3[temp.ref] <- ccc19x$meta_righttime3[temp.ref] - 14*24*60*60
    
    temp.ref <- which(ccc19x$covid_19_dx_interval == 4)
    ccc19x$meta_lefttime3[temp.ref] <- ccc19x$meta_righttime3[temp.ref] - 28*24*60*60
    
    temp.ref <- which(ccc19x$covid_19_dx_interval == 5)
    ccc19x$meta_lefttime3[temp.ref] <- ccc19x$meta_righttime3[temp.ref] - 56*24*60*60
    
    temp.ref <- which(ccc19x$covid_19_dx_interval == 6)
    ccc19x$meta_lefttime3[temp.ref] <- ccc19x$meta_righttime3[temp.ref] - 90*24*60*60
    
    temp.ref <- which(ccc19x$covid_19_dx_interval %in% 7:8)
    ccc19x$meta_lefttime3[temp.ref] <- ccc19x$meta_righttime3[temp.ref] - 180*24*60*60
    
    temp.ref <- which(ccc19x$covid_19_dx_interval == 9)
    ccc19x$meta_lefttime3[temp.ref] <- ccc19x$meta_righttime3[temp.ref] - 270*24*60*60
    
    temp.ref <- which(ccc19x$covid_19_dx_interval == 10)
    ccc19x$meta_lefttime3[temp.ref] <- ccc19x$meta_righttime3[temp.ref] - 360*24*60*60
    
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
        ccc19x$meta_righttime3[temp.ref] <- temp.time[1]
      }
    }
    
    temp.diff <- difftime(ccc19x$meta_righttime3, ccc19x$meta_lefttime3, units = 'days')
    temp <- ccc19x$record_id[which(as.numeric(temp.diff) >= 30)]
    
    ccc19x$der_d30[which(ccc19x$record_id %in% temp)] <- 1
    
    ccc19x$der_d30 <- factor(ccc19x$der_d30)
    summary(ccc19x$der_d30[ccc19x$redcap_repeat_instrument == ''])
    
    #O18. Dead within 30 days
    
    #Default is the mortality variable
    # ccc19x$der_dead30 <- ccc19x$mortality
    # ccc19x$der_dead30[which(ccc19x$der_dead30 == 88)] <- NA
    
    #Default is not dead at 30 days
    ccc19x$der_dead30 <- 0
    
    temp.ref <- which(ccc19x$der_deadbinary == 1 & ccc19x$redcap_repeat_instrument == '')
    
    #1. Calculated time to death is <= 30 days
    temp.diff <- difftime(ccc19x$meta_righttime, ccc19x$meta_lefttime, units = 'days')
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
    temp <- ccc19x$record_id[which(ccc19x$der_days_to_death_combined > 30)]
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
                                     (ccc19x$der_days_to_death == 9999|is.na(ccc19x$der_days_to_death)) &
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
    temp <- ccc19x$record_id[which(ccc19x$fu_weeks %in% c(90,180) & 
                                     ccc19x$redcap_repeat_instance > 1 &
                                     (ccc19x$fu_reason == 3 |
                                        ccc19x$covid_19_status_fu == 3 |
                                        ccc19x$current_status_fu == 9 ) &
                                     ccc19x$der_dead30 == 99)]
    ccc19x$der_dead30[which(ccc19x$record_id %in% temp)] <- 0
    
    ccc19x$der_dead30 <- as.factor(ccc19x$der_dead30)
    summary(ccc19x$der_dead30[ccc19x$redcap_repeat_instrument == ''])
    
    #Alternate variable that does NOT default to alive at 30 days
    ccc19x$der_dead30a <- NA
    
    temp.ref <- which(ccc19x$der_deadbinary == 1 & ccc19x$redcap_repeat_instrument == '')
    
    #0. Median f/u time is > 30 days or alive on a follow-up form
    temp.ref2 <- which(ccc19x$der_dead30a[which(ccc19x$der_days_fu > 30)])
    temp <- ccc19x$record_id[temp.ref2]
    ccc19x$der_dead30a[which(ccc19x$record_id %in% temp)] <- 0
    
    #Alive on followup form
    temp.ref2 <- which((ccc19x$covid_19_status_fu %in% c('1','1b','2') | 
                          ccc19x$fu_reason %in% 1:2) &
                         (ccc19x$fu_weeks %in% c(30,90,180) | ccc19x$timing_of_report_weeks > 4))
    temp <- ccc19x$record_id[temp.ref2]
    ccc19x$der_dead30a[which(ccc19x$record_id %in% temp)] <- 0
    
    #1. Calculated time to death is <= 30 days
    temp.diff <- difftime(ccc19x$meta_righttime, ccc19x$meta_lefttime, units = 'days')
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
    temp <- ccc19x$record_id[which(ccc19x$der_days_to_death_combined > 30)]
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
    
    #10. Rescind unknown status if 90-day or 180-day follow-up form is filled out as death and is not the first f/u form
    temp <- ccc19x$record_id[which(ccc19x$fu_weeks %in% c(90,180) & 
                                     ccc19x$redcap_repeat_instance > 1 &
                                     (ccc19x$fu_reason == 3 |
                                        ccc19x$covid_19_status_fu == 3 |
                                        ccc19x$current_status_fu == 9 ) &
                                     ccc19x$der_dead30a == 99)]
    ccc19x$der_dead30a[which(ccc19x$record_id %in% temp)] <- 0
    
    ccc19x$der_dead30a <- as.factor(ccc19x$der_dead30a)
    summary(ccc19x$der_dead30a[ccc19x$redcap_repeat_instrument == ''])
    
    #O24. Dead within 90 days
    
    #Default is not dead at 90 days
    ccc19x$der_dead90 <- 0
    
    temp.ref <- which(ccc19x$der_deadbinary == 1 & ccc19x$redcap_repeat_instrument == '')
    
    #1. Calculated time to death is <= 90 days
    temp.diff <- difftime(ccc19x$meta_righttime, ccc19x$meta_lefttime, units = 'days')
    temp.ref2 <- which(temp.diff[temp.ref] <= 90)
    temp <- ccc19x$record_id[temp.ref[temp.ref2]]
    ccc19x$der_dead90[which(ccc19x$record_id %in% temp)] <- 1
    
    #2. 90-day mortality flag is set (baseline)
    temp.ref2 <- which(ccc19x$mortality_90[temp.ref] == 0)
    temp <- ccc19x$record_id[temp.ref[temp.ref2]]
    ccc19x$der_dead90[which(ccc19x$record_id %in% temp)] <- 1
    
    #3. 90-day mortality flag is set (follow-up)
    temp.ref2 <- which(ccc19x$d90_vital_status[temp.ref] == 1)
    temp <- ccc19x$record_id[temp.ref[temp.ref2]]
    ccc19x$der_dead90[which(ccc19x$record_id %in% temp)] <- 1
    
    #4. 90-day follow-up form is filled out as death
    temp <- ccc19x$record_id[which(ccc19x$fu_weeks == 90 & (
      ccc19x$fu_reason == 3 |
        ccc19x$covid_19_status_fu == 3 |
        ccc19x$current_status_fu == 9 ))]
    ccc19x$der_dead90[which(ccc19x$record_id %in% temp)] <- 1
    
    #5. Follow-up form filled out as other and timing < 13 weeks
    temp <- ccc19x$record_id[which(ccc19x$timing_of_report_weeks < 13 & (
      ccc19x$fu_reason == 3 |
        ccc19x$covid_19_status_fu == 3 |
        ccc19x$current_status_fu == 9 ))]
    ccc19x$der_dead90[which(ccc19x$record_id %in% temp)] <- 1
    
    #6. Days to death <= 90
    temp <- ccc19x$record_id[which(ccc19x$der_days_to_death_combined <= 90)]
    ccc19x$der_dead90[which(ccc19x$record_id %in% temp)] <- 1
    
    #7. Rescind status if days to death > 90
    temp <- ccc19x$record_id[which(ccc19x$der_days_to_death_combined > 90)]
    ccc19x$der_dead90[which(ccc19x$record_id %in% temp)] <- 0
    
    #8. Declare unknown if days to death cannot be calculated and mortality flag not set
    temp <- ccc19x$record_id[which(ccc19x$der_deadbinary == 1 & ccc19x$der_dead90 == 0 &
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
    
    ccc19x$der_dead90[which(ccc19x$record_id %in% temp)] <- 99
    
    #9. Recover some patients with unknown or missing days to death
    #Estimate days to death for patients with missing/unknown days and retrospective reporting (baseline form only)
    #Estimate as the maximum length of time possible based on the interval
    temp <- ccc19x$record_id[which(ccc19x$der_dead90 %in% c(0,99) &
                                     (ccc19x$der_days_to_death == 9999|is.na(ccc19x$der_days_to_death)) &
                                     ccc19x$current_status_retro == 3)]
    if(length(temp) > 0)
    {
      for(i in 1:length(temp))
      {
        temp.ref <- which(ccc19x$record_id == temp[i])
        temp2 <- ccc19x$covid_19_dx_interval[temp.ref]
        temp2 <- temp2[!is.na(temp2)]
        if(temp2 %in% 1:5) ccc19x$der_dead90[temp.ref] <- 1
      }
    }
    
    #10. Rescind unknown status if 180-day follow-up form is filled out as death and is not the first f/u form
    temp <- ccc19x$record_id[which(ccc19x$fu_weeks %in% c(180) & 
                                     ccc19x$redcap_repeat_instance > 1 &
                                     (ccc19x$fu_reason == 3 |
                                        ccc19x$covid_19_status_fu == 3 |
                                        ccc19x$current_status_fu == 9 ) &
                                     ccc19x$der_dead90 == 99)]
    ccc19x$der_dead90[which(ccc19x$record_id %in% temp)] <- 0
    
    ccc19x$der_dead90 <- as.factor(ccc19x$der_dead90)
    summary(ccc19x$der_dead90[ccc19x$redcap_repeat_instrument == ''])
    
    #O25. Dead within 180 days
    
    #Default is not dead at 180 days
    ccc19x$der_dead180 <- 0
    
    temp.ref <- which(ccc19x$der_deadbinary == 1 & ccc19x$redcap_repeat_instrument == '')
    
    #1. Calculated time to death is <= 180 days
    temp.diff <- difftime(ccc19x$meta_righttime, ccc19x$meta_lefttime, units = 'days')
    temp.ref2 <- which(temp.diff[temp.ref] <= 180)
    temp <- ccc19x$record_id[temp.ref[temp.ref2]]
    ccc19x$der_dead180[which(ccc19x$record_id %in% temp)] <- 1
    
    #2. 180-day mortality flag is set (baseline)
    temp.ref2 <- which(ccc19x$mortality_180[temp.ref] == 0)
    temp <- ccc19x$record_id[temp.ref[temp.ref2]]
    ccc19x$der_dead180[which(ccc19x$record_id %in% temp)] <- 1
    
    #3. 180-day mortality flag is set (follow-up)
    temp.ref2 <- which(ccc19x$d180_vital_status[temp.ref] == 1)
    temp <- ccc19x$record_id[temp.ref[temp.ref2]]
    ccc19x$der_dead180[which(ccc19x$record_id %in% temp)] <- 1
    
    #4. 180-day follow-up form is filled out as death
    temp <- ccc19x$record_id[which(ccc19x$fu_weeks == 180 & (
      ccc19x$fu_reason == 3 |
        ccc19x$covid_19_status_fu == 3 |
        ccc19x$current_status_fu == 9 ))]
    ccc19x$der_dead180[which(ccc19x$record_id %in% temp)] <- 1
    
    #5. Follow-up form filled out as other and timing < 26 weeks
    temp <- ccc19x$record_id[which(ccc19x$timing_of_report_weeks < 26 & (
      ccc19x$fu_reason == 3 |
        ccc19x$covid_19_status_fu == 3 |
        ccc19x$current_status_fu == 9 ))]
    ccc19x$der_dead180[which(ccc19x$record_id %in% temp)] <- 1
    
    #6. Days to death <= 180
    temp <- ccc19x$record_id[which(ccc19x$der_days_to_death_combined <= 180)]
    ccc19x$der_dead180[which(ccc19x$record_id %in% temp)] <- 1
    
    #7. Rescind status if days to death > 180
    temp <- ccc19x$record_id[which(ccc19x$der_days_to_death_combined > 180)]
    ccc19x$der_dead180[which(ccc19x$record_id %in% temp)] <- 0
    
    #8. Declare unknown if days to death cannot be calculated and mortality flag not set
    temp <- ccc19x$record_id[which(ccc19x$der_deadbinary == 1 & ccc19x$der_dead180 == 0 &
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
    
    ccc19x$der_dead180[which(ccc19x$record_id %in% temp)] <- 99
    
    #9. Recover some patients with unknown or missing days to death
    #Estimate days to death for patients with missing/unknown days and retrospective reporting (baseline form only)
    #Estimate as the maximum length of time possible based on the interval
    temp <- ccc19x$record_id[which(ccc19x$der_dead180 %in% c(0,99) &
                                     (ccc19x$der_days_to_death == 9999|is.na(ccc19x$der_days_to_death)) &
                                     ccc19x$current_status_retro == 3)]
    if(length(temp) > 0)
    {
      for(i in 1:length(temp))
      {
        temp.ref <- which(ccc19x$record_id == temp[i])
        temp2 <- ccc19x$covid_19_dx_interval[temp.ref]
        temp2 <- temp2[!is.na(temp2)]
        if(temp2 %in% 1:6) ccc19x$der_dead180[temp.ref] <- 1
      }
    }
    
    ccc19x$der_dead180 <- as.factor(ccc19x$der_dead180)
    summary(ccc19x$der_dead180[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_composite_hosp_death[ccc19x$redcap_repeat_instrument == ''])
    
    #T9 Month and year of diagnosis, accounting for interval bounds
    temp.ref <- which(ccc19x$redcap_repeat_instrument == '')
    
    #First, extract month and year from the POSIXlt objects
    x1 <- months(ccc19x$meta_lefttime[temp.ref], abbreviate = T)
    xm <- months(ccc19x$meta_lefttime2[temp.ref], abbreviate = T)
    x2 <- months(ccc19x$meta_lefttime3[temp.ref], abbreviate = T)
    y1 <- format(ccc19x$meta_lefttime[temp.ref], format = '%Y')
    ym <- format(ccc19x$meta_lefttime2[temp.ref], format = '%Y')
    y2 <- format(ccc19x$meta_lefttime3[temp.ref], format = '%Y')
    
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
    
    ccc19x$der_month_dx <- NA
    temp.ref2 <- which(x1 == x2 & y1 == y2 & y1 != 2099)
    
    ccc19x$der_month_dx[temp.ref[temp.ref2]] <- paste(y1[temp.ref2], '-', x1[temp.ref2], sep = '')
    ccc19x$der_month_dx <- factor(ccc19x$der_month_dx)
    summary(ccc19x$der_month_dx[ccc19x$redcap_repeat_instrument == ''])
    
    #T09a Month and year of diagnosis, using the right side of the interval as the anchor
    ccc19x$der_month_rt_dx <- NA
    ccc19x$der_month_rt_dx[temp.ref] <- paste(y2, '-', x2, sep = '')
    ccc19x$der_month_rt_dx <- factor(ccc19x$der_month_rt_dx)
    summary(ccc19x$der_month_rt_dx[ccc19x$redcap_repeat_instrument == ''])
    
    #T10 Quarter and year of diagnosis, accounting for interval bounds
    ccc19x$der_quarter_dx <- NA
    
    #Q1
    temp.ref2 <- which(x1 %in% c('January','February','March') & 
                         x2 %in% c('January','February','March') & y1 == y2 & y1 != 2099)
    ccc19x$der_quarter_dx[temp.ref[temp.ref2]] <- paste('Q1', y1[temp.ref2])
    
    #Q2
    temp.ref2 <- which(x1 %in% c('April','May','June') & 
                         x2 %in% c('April','May','June') & y1 == y2 & y1 != 2099)
    ccc19x$der_quarter_dx[temp.ref[temp.ref2]] <- paste('Q2', y1[temp.ref2])
    
    #Q3
    temp.ref2 <- which(x1 %in% c('July','August','September') & 
                         x2 %in% c('July','August','September') & y1 == y2 & y1 != 2099)
    ccc19x$der_quarter_dx[temp.ref[temp.ref2]] <- paste('Q3', y1[temp.ref2])
    
    #Q4
    temp.ref2 <- which(x1 %in% c('October','November','December') & 
                         x2 %in% c('October','November','December') & y1 == y2 & y1 != 2099)
    ccc19x$der_quarter_dx[temp.ref[temp.ref2]] <- paste('Q4', y1[temp.ref2])
    
    ccc19x$der_quarter_dx <- factor(ccc19x$der_quarter_dx)
    summary(ccc19x$der_quarter_dx[ccc19x$redcap_repeat_instrument == ''])
    
    #T10a Quarter and year of diagnosis, using the right side of the interval as the anchor
    ccc19x$der_quarter_rt_dx <- NA
    
    #Q1
    temp.ref2 <- which(x2 %in% c('January','February','March') & y2 != 2099)
    ccc19x$der_quarter_rt_dx[temp.ref[temp.ref2]] <- paste('Q1', y2[temp.ref2])
    
    #Q2
    temp.ref2 <- which(x2 %in% c('April','May','June') & y2 != 2099)
    ccc19x$der_quarter_rt_dx[temp.ref[temp.ref2]] <- paste('Q2', y2[temp.ref2])
    
    #Q3
    temp.ref2 <- which(x2 %in% c('July','August','September') & y2 != 2099)
    ccc19x$der_quarter_rt_dx[temp.ref[temp.ref2]] <- paste('Q3', y2[temp.ref2])
    
    #Q4
    temp.ref2 <- which(x2 %in% c('October','November','December') & y2 != 2099)
    
    ccc19x$der_quarter_rt_dx[temp.ref[temp.ref2]] <- paste('Q4', y2[temp.ref2])
    ccc19x$der_quarter_rt_dx <- factor(ccc19x$der_quarter_rt_dx)
    summary(ccc19x$der_quarter_rt_dx[ccc19x$redcap_repeat_instrument == ''])
    
    #T11 Trimester and year of diagnosis, using the right side of the interval as the anchor
    ccc19x$der_tri_rt_dx <- NA
    
    #T1
    temp.ref2 <- which(x2 %in% c('January','February','March','April') & y2 != 2099)
    ccc19x$der_tri_rt_dx[temp.ref[temp.ref2]] <- paste('T1', y2[temp.ref2])
    
    #T2
    temp.ref2 <- which(x2 %in% c('May','June','July','August') & y2 != 2099)
    ccc19x$der_tri_rt_dx[temp.ref[temp.ref2]] <- paste('T2', y2[temp.ref2])
    
    #T3
    temp.ref2 <- which(x2 %in% c('September','October','November','December') & y2 != 2099)
    
    ccc19x$der_tri_rt_dx[temp.ref[temp.ref2]] <- paste('T3', y2[temp.ref2])
    ccc19x$der_tri_rt_dx <- factor(ccc19x$der_tri_rt_dx)
    summary(ccc19x$der_tri_rt_dx[ccc19x$redcap_repeat_instrument == ''])
    
    #T12 Hemi-year of diagnosis, accounting for interval bounds
    ccc19x$der_hemi_dx <- NA
    
    #H1
    temp.ref2 <- which(x1 %in% c('January','February','March','April','May','June') & 
                         x2 %in% c('January','February','March','April','May','June') & y1 == y2 & y1 != 2099)
    ccc19x$der_hemi_dx[temp.ref[temp.ref2]] <- paste('H1', y1[temp.ref2])
    
    #H2
    temp.ref2 <- which(x1 %in% c('July','August','September','October','November','December') & 
                         x2 %in% c('July','August','September','October','November','December') & y1 == y2 & y1 != 2099)
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
    
    ccc19x$der_cancer_tx_timing_v2 <- as.factor(ccc19x$der_cancer_tx_timing_v2)
    summary(ccc19x$der_cancer_tx_timing_v2[ccc19x$redcap_repeat_instrument == ''])
    
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
    
    ccc19x$der_cancer_tx_timing_v3 <- as.factor(ccc19x$der_cancer_tx_timing_v3)
    summary(ccc19x$der_cancer_tx_timing_v3[ccc19x$redcap_repeat_instrument == ''])
    
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
      
      ccc19x$der_ordinal_v1 <- as.integer(ccc19x$der_ordinal_v1)
      summary(factor(ccc19x$der_ordinal_v1[ccc19x$redcap_repeat_instrument == '']))
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
      #Only declare unknown if patient not know to be alive at 30, 90, or 180 days
      ccc19x$der_ordinal_v1a[which(ccc19x$der_deadbinary == 99 &
                                     !(ccc19x$der_dead30 == 0|ccc19x$der_dead90 == 0|ccc19x$der_dead180 == 0))] <- 99
      
      length(ccc19x$der_ordinal_v1a[which(ccc19x$der_deadbinary == 99 & ccc19x$der_dead30 == 99)])
      
      ccc19x$der_ordinal_v1a <- as.integer(ccc19x$der_ordinal_v1a)
      summary(factor(ccc19x$der_ordinal_v1a[ccc19x$redcap_repeat_instrument == '']))
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
    
    #O23. ordinal_v2 (0 = never hospitalized; 1 = hospitalized; 2 = required O2; 3 = ICU; 4 = vent; 5 = death)
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
      
      ccc19x$der_ordinal_v2 <- as.integer(ccc19x$der_ordinal_v2)
      summary(factor(ccc19x$der_ordinal_v2[ccc19x$redcap_repeat_instrument == '']))
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
    
    ccc19x$der_ordinal_v3 <- as.integer(ccc19x$der_ordinal_v3)
    summary(factor(ccc19x$der_ordinal_v3[ccc19x$redcap_repeat_instrument == '']))
    
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
    
    ccc19x$der_ordinal_v3a <- as.integer(ccc19x$der_ordinal_v3a)
    summary(factor(ccc19x$der_ordinal_v3a[ccc19x$redcap_repeat_instrument == '']))
    
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
    
    ccc19x$der_ordinal_v4 <- as.integer(ccc19x$der_ordinal_v4)
    summary(factor(ccc19x$der_ordinal_v4[ccc19x$redcap_repeat_instrument == '']))
  }
  print('Ordinal outcomes completed')
  
  ###########
  #Treatments - takes some time, not as much as complications
  ###########
  {
    "hca" 
    #Rx1. Derived variable for hydroxychloroquine/azithro exposure used for TREATMENT of COVID-19
    ccc19x$der_hca <- NA
    
    #Non-trial
    #Neither, baseline
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment___unk|19_treatment___rxcui_18631|19_treatment___rxcui_5521'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(ccc19x$covid_19_treatment___rxcui_18631[i] == 0 &
         ccc19x$covid_19_treatment___rxcui_5521[i] == 0 &
         any(ccc19x[i,temp.ref])) ccc19x$der_hca[i] <- 'Neither HCQ nor AZ'
    
    #Neither, followup
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk|19_treatment_fu___rxcui_18631|19_treatment_fu___rxcui_5521'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(ccc19x$covid_19_treatment_fu___rxcui_18631[i] == 0 &
         ccc19x$covid_19_treatment_fu___rxcui_5521[i] == 0 &
         any(ccc19x[i,temp.ref])) ccc19x$der_hca[i] <- 'Neither HCQ nor AZ'
    
    ccc19x$der_hca[which(ccc19x$covid_19_treatment___rxcui_5521 == 1 & ccc19x$covid_19_treatment___rxcui_18631 == 0)] <- 'HCQ alone'
    ccc19x$der_hca[which(ccc19x$covid_19_treatment_fu___rxcui_5521 == 1 & ccc19x$covid_19_treatment_fu___rxcui_18631 == 0)] <- 'HCQ alone'
    
    ccc19x$der_hca[which(ccc19x$covid_19_treatment___rxcui_5521 == 0 & ccc19x$covid_19_treatment___rxcui_18631 == 1)] <- 'AZ alone'
    ccc19x$der_hca[which(ccc19x$covid_19_treatment_fu___rxcui_5521 == 0 & ccc19x$covid_19_treatment_fu___rxcui_18631 == 1)] <- 'AZ alone'
    
    ccc19x$der_hca[which(ccc19x$covid_19_treatment___rxcui_5521 == 1 & ccc19x$covid_19_treatment___rxcui_18631 == 1)] <- 'AZ+HCQ'
    ccc19x$der_hca[which(ccc19x$covid_19_treatment_fu___rxcui_5521 == 1 & ccc19x$covid_19_treatment_fu___rxcui_18631 == 1)] <- 'AZ+HCQ'
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(ccc19x$covid_19_treatment___unk[i] == 1 &
         all(ccc19x[i,temp.ref] == 0 )) ccc19x$der_hca[i] <- 'Unknown'
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(ccc19x$covid_19_treatment_fu___unk[i] == 1 &
         all(ccc19x[i,temp.ref] == 0 )) ccc19x$der_hca[i] <- 'Unknown'
    
    #Trial baseline
    trial.ref <- which(ccc19x$covid_19_treatment_trial == 1)
    for(i in 1:length(trial.ref))
    {
      temp <- ccc19x[trial.ref[i],c('covid_19_trial_tx___rxcui_5521','covid_19_trial_tx___rxcui_18631')]
      if(temp[1] == 1 & temp[2] == 0) temp <- 'HCQ alone' else
        if(temp[1] == 0 & temp[2] == 1) temp <- 'AZ alone' else
          if(temp[1] == 1 & temp[2] == 1) temp <- 'AZ+HCQ' else temp <- 'Neither HCQ nor AZ'
          
          if(ccc19x$der_hca[trial.ref[i]] == 'HCQ alone' & temp == 'AZ alone' |
             ccc19x$der_hca[trial.ref[i]] == 'AZ alone' & temp == 'HCQ alone' |
             ccc19x$der_hca[trial.ref[i]] == 'AZ+HCQ') ccc19x$der_hca[trial.ref[i]] <- 'AZ+HCQ' 
          
    }
    
    #Trial f/u
    trial.ref <- which(ccc19x$covid_19_treatment_trial_fu == 1)
    if(length(trial.ref) > 0)
    {
      for(i in 1:length(trial.ref))
      {
        temp <- ccc19x[trial.ref[i],c('covid_19_trial_tx_fu___rxcui_5521','covid_19_trial_tx_fu___rxcui_18631')]
        if(temp[1] != 0 & temp[2] != 0)
        {
        if(temp[1] == 1 & temp[2] == 0) temp <- 'HCQ alone' else
          if(temp[1] == 0 & temp[2] == 1) temp <- 'AZ alone' else
            if(temp[1] == 1 & temp[2] == 1) temp <- 'AZ+HCQ' else temp <- 'Neither HCQ nor AZ'
            
            if(ccc19x$der_hca[trial.ref[i]] == 'HCQ alone' & temp == 'AZ alone' |
               ccc19x$der_hca[trial.ref[i]] == 'AZ alone' & temp == 'HCQ alone' |
               ccc19x$der_hca[trial.ref[i]] == 'AZ+HCQ') ccc19x$der_hca[trial.ref[i]] <- 'AZ+HCQ'
        }
      }         
    }
    
    #Resolve discrepancies between baseline and f/u
    temp.ref <- which(ccc19x$redcap_repeat_instrument == 'followup')
    for(i in temp.ref)
    {
      temp <- ccc19x$der_hca[ccc19x$record_id == ccc19x$record_id[i]]
      temp <- temp[!is.na(temp)]
      if(length(temp) > 0)
      {
        if((any(temp == 'AZ alone') & any(temp=='HCQ alone'))|any(temp == 'AZ+HCQ')) ccc19x$der_hca[ccc19x$record_id == ccc19x$record_id[i]] <- 'AZ+HCQ'
        else if(any(temp %in% c('Neither HCQ nor AZ','Unknown')) & any(!temp %in% c('Neither HCQ nor AZ','Unknown'))) ccc19x$der_hca[ccc19x$record_id == ccc19x$record_id[i]] <- unique(temp[!temp %in% c('Neither HCQ nor AZ','Unknown')])
      }
    }
    
    #Factor
    ccc19x$der_hca <- as.factor(ccc19x$der_hca)
    ccc19x$der_hca <- relevel(ccc19x$der_hca, ref = 'Neither HCQ nor AZ')
    summary(ccc19x$der_hca[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_hcq[ccc19x$redcap_repeat_instrument == ''])
    
    #Rx4. High-dose steroids as COVID-19 treatment ever (dose > 20 mg/d)
    ccc19x$der_steroids_hd_c19 <- NA
    ccc19x$der_steroids_hd_c19[which(ccc19x$steroid_specific %in% 2:3|
                                ccc19x$steroid_specific_fu %in% 2:3)] <- 1
    
    #Never or less than 20 mg/d
    ccc19x$der_steroids_hd_c19[which((ccc19x$covid_19_treatment___ho_45523 == 1 & ccc19x$steroid_specific %in% c('1','1a','1b') & is.na(ccc19x$der_steroids_hd_c19))|
                                (ccc19x$covid_19_treatment_fu___ho_45523 == 1 & ccc19x$steroid_specific_fu %in% c('1','1a','1b') & is.na(ccc19x$der_steroids_hd_c19))|
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
    summary(ccc19x$der_steroids_hd_c19[ccc19x$redcap_repeat_instrument == ''])
    
    #Rx4a. Steroids ever used for TREATMENT of COVID-19
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
    summary(ccc19x$der_steroids_c19[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_azithro[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_plasma[ccc19x$redcap_repeat_instrument == ''])
    
    #Rx16. Monoclonal antibody ever used for treatment of COVID-19
    ccc19x$der_monoclonals <- NA
    ccc19x$der_monoclonals[which(ccc19x$covid_19_treatment___db_15718 == 1|
                                   ccc19x$covid_19_treatment___bam_et == 1|
                                   ccc19x$covid_19_treatment___regen_cov2 == 1|
                                   ccc19x$covid_19_trial_tx___db_15718 == 1|
                                   ccc19x$covid_19_trial_tx___bam_et == 1|
                                   ccc19x$covid_19_trial_tx___regen_cov2 == 1|
                                   ccc19x$covid_19_treatment_fu___db_15718 == 1|
                                   ccc19x$covid_19_treatment_fu___bam_et == 1|
                                   ccc19x$covid_19_treatment_fu___regen_cov2 == 1|
                                   ccc19x$covid_19_trial_tx_fu___db_15718 == 1|
                                   ccc19x$covid_19_trial_tx_fu___bam_et == 1|
                                   ccc19x$covid_19_trial_tx_fu___regen_cov2 == 1)] <- 1
    
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
    summary(ccc19x$der_monoclonals[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_toci[ccc19x$redcap_repeat_instrument == ''])
    
    #Rx7. COVID-19 treatments other than HCQ, AZ, high-dose steroids, remdesivir, or toci (excluding oyxgen, anticoag, ECMO, CRRT)
    ccc19x$der_other_tx_c19 <- NA
    
    #Build the exclusion list
    x <- c('b01a','n02ba', #Anticoag, Aspirin, APA
           '233573008','714749008', #ECMO, CRRT
           'rxcui_5521','omop4873974','rxcui_18631', #HCQ, Rem, AZ
           'ho_44995','rxcui_260101', #Antivirals NOS, oseltamivir
           'ho_45523', #Steroids
           '612865','l04ac07', #Toci
           'atc_c10aa', #Statins
           '19_treatment___oth','19_treatment_fu___oth','trial_tx___oth','tx_fu___oth', #"Other" treatments (usually antibiotics)
           '19_treatment___unk','19_treatment_fu___unk','trial_tx___unk','tx_fu___unk',
           'treatment___none','treatment_fu___none')
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
    
    summary(factor(ccc19x$der_other_tx_c19))
    
    #Unknown
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$covid_19_treatment___unk[i] == 1) ccc19x$der_other_tx_c19[i] <- 99
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$covid_19_treatment_fu___unk[i] == 1) ccc19x$der_other_tx_c19[i] <- 99
    
    summary(factor(ccc19x$der_other_tx_c19))
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_other_tx_c19[i] <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_other_tx_c19[i] <- NA
    
    summary(factor(ccc19x$der_other_tx_c19))
    
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
    summary(ccc19x$der_other_tx_c19[ccc19x$redcap_repeat_instrument == ''])
    
    #Rx7a. COVID-19 treatments other than HCQ, steroids, or remdesivir
    ccc19x$der_other_tx_c19_v2 <- NA
    
    #Build the exclusion list
    x <- c('b01a','n02ba', #Anticoag, Aspirin, APA
           '233573008','714749008', #ECMO, CRRT
           'rxcui_5521','omop4873974', #HCQ, Rem
           'ho_44995','rxcui_260101', #Antivirals NOS, oseltamivir
           'ho_45523', #Steroids
           'atc_c10aa', #Statins
           '19_treatment___oth','19_treatment_fu___oth','trial_tx___oth','tx_fu___oth', #"Other" treatments (usually antibiotics)
           '19_treatment___unk','19_treatment_fu___unk','trial_tx___unk','tx_fu___unk',
           'treatment___none','treatment_fu___none')
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
    summary(ccc19x$der_other_tx_c19_v2[ccc19x$redcap_repeat_instrument == ''])
    
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
    
    #Unknown baseline or treatment
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___|19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = 'concomitant_meds___unk|19_treatment___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_statins_baseline[i] == 0 | is.na(ccc19x$der_statins_baseline[i]))) ccc19x$der_statins_baseline[i] <- 99
    
    #Unknown f/u treatment
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_statins_baseline[i] == 0 | is.na(ccc19x$der_statins_baseline[i]))) ccc19x$der_statins_baseline[i] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___|concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_statins_baseline[i] != 1|is.na(ccc19x$der_statins_baseline[i]))) ccc19x$der_statins_baseline[i] <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_statins_baseline[i] != 1|is.na(ccc19x$der_statins_baseline[i]))) ccc19x$der_statins_baseline[i] <- NA
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_statins_baseline[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_statins_baseline[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_statins_baseline[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_statins_baseline[temp.ref] <- 0
      }
    }
    
    ccc19x$der_statins_baseline <- factor(ccc19x$der_statins_baseline)
    summary(ccc19x$der_statins_baseline[ccc19x$redcap_repeat_instrument == ''])
    
    #Rx22. Beta blockers (BB) at baseline
    ccc19x$der_BB_baseline <- NA
    ccc19x$der_BB_baseline[which(ccc19x$concomitant_meds___c07a == 1)] <- 1
    
    #Never
    ccc19x$der_BB_baseline[which(ccc19x$concomitant_meds___c07a == 0)] <- 0
    
    #Unknown baseline or treatment
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___|19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = 'concomitant_meds___unk|19_treatment___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_BB_baseline[i] == 0 | is.na(ccc19x$der_BB_baseline[i]))) ccc19x$der_BB_baseline[i] <- 99
    
    #Unknown f/u treatment
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_BB_baseline[i] == 0 | is.na(ccc19x$der_BB_baseline[i]))) ccc19x$der_BB_baseline[i] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___|concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_BB_baseline[i] != 1|is.na(ccc19x$der_BB_baseline[i]))) ccc19x$der_BB_baseline[i] <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_BB_baseline[i] != 1|is.na(ccc19x$der_BB_baseline[i]))) ccc19x$der_BB_baseline[i] <- NA
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_BB_baseline[temp.ref]
      temp <- as.numeric(unique(temp[!is.na(temp)]))
      if(length(temp) > 0)
      {
        if(any(temp == 1)) ccc19x$der_BB_baseline[temp.ref] <- 1
        if(!any(temp == 1) & any(temp == 99)) ccc19x$der_BB_baseline[temp.ref] <- 99
        if(!any(temp == 1) & !any(temp == 99) & any(temp == 0)) ccc19x$der_BB_baseline[temp.ref] <- 0
      }
    }
    
    ccc19x$der_BB_baseline <- factor(ccc19x$der_BB_baseline)
    summary(ccc19x$der_BB_baseline[ccc19x$redcap_repeat_instrument == ''])
    
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
    
    #Rx10. Low-dose steroids ever (dose up to 20 mg/d)
    ccc19x$der_steroids_ld <- NA
    ccc19x$der_steroids_ld[which(ccc19x$steroid_specific_2 %in% c('1','1a','1b')|
                                   ccc19x$steroid_specific %in% c('1','1a','1b')|
                                   ccc19x$steroid_specific_fu %in% c('1','1a','1b'))] <- 1
    
    #Never
    ccc19x$der_steroids_ld[which((ccc19x$concomitant_meds___h02 == 0 & is.na(ccc19x$der_steroids_ld))|
                                   (ccc19x$covid_19_treatment___ho_45523 == 0 & is.na(ccc19x$der_steroids_ld))|
                                   (ccc19x$covid_19_treatment_fu___ho_45523 == 0 & is.na(ccc19x$der_steroids_ld)))] <- 0
    
    #Unknown
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
                                   ccc19x$steroid_specific_fu == 99 & is.na(ccc19x$der_steroids_ld))] <- 99
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_steroids_ld[i] == 0) ccc19x$der_steroids_ld[i] <- NA
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_steroids_ld[i] == 0) ccc19x$der_steroids_ld[i] <- NA
    
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
    summary(ccc19x$der_steroids_ld[ccc19x$redcap_repeat_instrument == ''])
    
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
    
    summary(factor(ccc19x$der_ac_apa))
    
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
    
    summary(factor(ccc19x$der_ac_apa))
    
    #Unknown baseline or treatment
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___|19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = 'concomitant_meds___unk|19_treatment___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_ac_apa[i] == 0 | is.na(ccc19x$der_ac_apa[i]))) ccc19x$der_ac_apa[i] <- 99
    
    summary(factor(ccc19x$der_ac_apa))
    
    #Unknown f/u treatment
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_ac_apa[i] == 0 | is.na(ccc19x$der_ac_apa[i]))) ccc19x$der_ac_apa[i] <- 99
    
    summary(factor(ccc19x$der_ac_apa))
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___|concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_ac_apa[i] != 1|is.na(ccc19x$der_ac_apa[i]))) ccc19x$der_ac_apa[i] <- NA
    
    summary(factor(ccc19x$der_ac_apa))
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_ac_apa[i] != 1|is.na(ccc19x$der_ac_apa[i]))) ccc19x$der_ac_apa[i] <- NA
    
    summary(factor(ccc19x$der_ac_apa))
    
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
    summary(ccc19x$der_ac_apa[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_ac_apa_baseline[ccc19x$redcap_repeat_instrument == ''])
    
    #Rx13a. Anticoagulation at baseline
    ccc19x$der_ac_baseline <- NA
    ccc19x$der_ac_baseline[which(ccc19x$concomitant_meds___b01a == 1)] <- 1
    
    summary(factor(ccc19x$der_ac_baseline))
    
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
    summary(ccc19x$der_ac_baseline[ccc19x$redcap_repeat_instrument == ''])
    
    #Rx13b. APA at baseline
    ccc19x$der_apa_baseline <- NA
    ccc19x$der_apa_baseline[which(ccc19x$concomitant_meds___n02ba == 1|
                                    ccc19x$concomitant_meds___b01ac == 1)] <- 1
    
    summary(factor(ccc19x$der_apa_baseline))
    
    #Unexposed
    ccc19x$der_apa_baseline[which(is.na(ccc19x$der_apa_baseline) & ccc19x$concomitant_meds___n02ba == 0 &
                                    ccc19x$concomitant_meds___b01ac == 0 &
                                    ccc19x$concomitant_meds___unk == 0)] <- 0
    
    summary(factor(ccc19x$der_apa_baseline))
    
    #Unknown baseline
    ccc19x$der_apa_baseline[which(ccc19x$concomitant_meds___unk == 1 & is.na(ccc19x$der_apa_baseline))] <- 99
    
    summary(factor(ccc19x$der_apa_baseline))
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0)) ccc19x$der_apa_baseline[i] <- NA
    
    summary(factor(ccc19x$der_apa_baseline))
    
    ccc19x$der_apa_baseline <- factor(ccc19x$der_apa_baseline)
    summary(ccc19x$der_apa_baseline[ccc19x$redcap_repeat_instrument == ''])
    
    #Rx23. Thromboprophylaxis, including consideration of baseline anticoagulation
    #Does not yet account for discrepancies in baseline and follow-up forms
    ccc19x$der_thromboprophy <- NA
    
    #Baseline prophylaxis
    ccc19x$der_thromboprophy[which(ccc19x$bl_anticoag_reason == 360271000)] <- 'Baseline prophylactic anticoagulation'
    
    #Baseline therapeutic
    ccc19x$der_thromboprophy[which(ccc19x$bl_anticoag_reason == 262202000)] <- 'Baseline therapeutic anticoagulation'
    
    #COVID-19 prophylaxis
    ccc19x$der_thromboprophy[which((ccc19x$c19_anticoag_reason___1 == 1|
                                      ccc19x$c19_anticoag_reason_fu___1 == 1) &
                                     ccc19x$der_thromboprophy == 'Baseline prophylactic anticoagulation')] <- 'Baseline prophy--COVID-19 prophylactic anticoagulation'
    ccc19x$der_thromboprophy[which((ccc19x$c19_anticoag_reason___1 == 1|
                                      ccc19x$c19_anticoag_reason_fu___1 == 1) &
                                     ccc19x$der_thromboprophy == 'Baseline therapeutic anticoagulation')] <- 'Baseline therapeutic--COVID-19 prophylactic anticoagulation'
    ccc19x$der_thromboprophy[which((ccc19x$c19_anticoag_reason___1 == 1|
                                      ccc19x$c19_anticoag_reason_fu___1 == 1) &
                                     ccc19x$der_ac_baseline == 0)] <- 'No baseline--COVID-19 prophylactic anticoagulation'
    ccc19x$der_thromboprophy[which((ccc19x$c19_anticoag_reason___1 == 1|
                                      ccc19x$c19_anticoag_reason_fu___1 == 1) &
                                     ccc19x$der_ac_baseline == 99)] <- 'Unknown baseline--COVID-19 prophylactic anticoagulation'
    ccc19x$der_thromboprophy[which((ccc19x$c19_anticoag_reason___1 == 1|
                                      ccc19x$c19_anticoag_reason_fu___1 == 1) &
                                     is.na(ccc19x$der_ac_baseline))] <- 'Missing baseline--COVID-19 prophylactic anticoagulation'
    
    #COVID-19 treatment
    ccc19x$der_thromboprophy[which((ccc19x$c19_anticoag_reason___2a == 1|
                                      ccc19x$c19_anticoag_reason___2c == 1|
                                      ccc19x$c19_anticoag_reason_fu___2a == 1|
                                      ccc19x$c19_anticoag_reason_fu___2c == 1) &
                                     ccc19x$der_thromboprophy == 'Baseline prophylactic anticoagulation')] <- 'Baseline prophy--COVID-19 therapeutic anticoagulation'
    ccc19x$der_thromboprophy[which((ccc19x$c19_anticoag_reason___2a == 1|
                                      ccc19x$c19_anticoag_reason___2c == 1|
                                      ccc19x$c19_anticoag_reason_fu___2a == 1|
                                      ccc19x$c19_anticoag_reason_fu___2c == 1) &
                                     ccc19x$der_thromboprophy == 'Baseline therapeutic anticoagulation')] <- 'Baseline therapeutic--COVID-19 therapeutic anticoagulation'
    ccc19x$der_thromboprophy[which((ccc19x$c19_anticoag_reason___2a == 1|
                                      ccc19x$c19_anticoag_reason___2c == 1|
                                      ccc19x$c19_anticoag_reason_fu___2a == 1|
                                      ccc19x$c19_anticoag_reason_fu___2c == 1) &
                                     ccc19x$der_ac_baseline == 0)] <- 'No baseline--COVID-19 therapeutic anticoagulation'
    ccc19x$der_thromboprophy[which((ccc19x$c19_anticoag_reason___2a == 1|
                                      ccc19x$c19_anticoag_reason___2c == 1|
                                      ccc19x$c19_anticoag_reason_fu___2a == 1|
                                      ccc19x$c19_anticoag_reason_fu___2c == 1) &
                                     ccc19x$der_ac_baseline == 99)] <- 'Unknown baseline--COVID-19 therapeutic anticoagulation'
    ccc19x$der_thromboprophy[which((ccc19x$c19_anticoag_reason___2a == 1|
                                      ccc19x$c19_anticoag_reason___2c == 1|
                                      ccc19x$c19_anticoag_reason_fu___2a == 1|
                                      ccc19x$c19_anticoag_reason_fu___2c == 1) &
                                     is.na(ccc19x$der_ac_baseline))] <- 'Missing baseline--COVID-19 therapeutic anticoagulation'
    
    #None for COVID-19
    ccc19x$der_thromboprophy[which(ccc19x$c19_anticoag_reason___none == 1 &
                                     ccc19x$der_thromboprophy == 'Baseline prophylactic anticoagulation')] <- 'Baseline prophy--no COVID-19 anticoagulation'
    ccc19x$der_thromboprophy[which(ccc19x$c19_anticoag_reason___none == 1 &
                                     ccc19x$der_thromboprophy == 'Baseline therapeutic anticoagulation')] <- 'Baseline therapeutic--no COVID-19 anticoagulation'
    ccc19x$der_thromboprophy[which(ccc19x$c19_anticoag_reason___none == 1 &
                                     ccc19x$der_ac_baseline == 0)] <- 'No baseline--no COVID-19 anticoagulation'
    ccc19x$der_thromboprophy[which(ccc19x$c19_anticoag_reason___none == 1 &
                                     ccc19x$der_ac_baseline == 99)] <- 'Unknown baseline--no COVID-19 anticoagulation'
    ccc19x$der_thromboprophy[which(ccc19x$c19_anticoag_reason___none == 1 &
                                     is.na(ccc19x$der_ac_baseline))] <- 'Missing baseline--no COVID-19 anticoagulation'
    
    #Unknown for COVID-19
    ccc19x$der_thromboprophy[which(ccc19x$c19_anticoag_reason___unk == 1 &
                                     ccc19x$der_thromboprophy == 'Baseline prophylactic anticoagulation')] <- 'Baseline prophy--unknown COVID-19 anticoagulation'
    ccc19x$der_thromboprophy[which(ccc19x$c19_anticoag_reason___unk == 1 &
                                     ccc19x$der_thromboprophy == 'Baseline therapeutic anticoagulation')] <- 'Baseline therapeutic--unknown COVID-19 anticoagulation'
    ccc19x$der_thromboprophy[which(ccc19x$c19_anticoag_reason___unk == 1 &
                                     ccc19x$der_ac_baseline == 0)] <- 'No baseline--unknown COVID-19 anticoagulation'
    ccc19x$der_thromboprophy[which(ccc19x$c19_anticoag_reason___unk == 1 &
                                     ccc19x$der_ac_baseline == 99)] <- 'Unknown baseline--unknown COVID-19 anticoagulation'
    ccc19x$der_thromboprophy[which(ccc19x$c19_anticoag_reason___unk == 1 &
                                     is.na(ccc19x$der_ac_baseline))] <- 'Missing baseline--unknown COVID-19 anticoagulation'
    
    #Missing for COVID-19 but not for baseline
    ccc19x$der_thromboprophy[which(ccc19x$der_thromboprophy == 'Baseline prophylactic anticoagulation')] <- 'Baseline prophy--missing COVID-19 anticoagulation'
    ccc19x$der_thromboprophy[which(ccc19x$der_thromboprophy == 'Baseline therapeutic anticoagulation')] <- 'Baseline therapeutic--missing COVID-19 anticoagulation'
    
    ccc19x$der_thromboprophy <- factor(ccc19x$der_thromboprophy)
    summary(ccc19x$der_thromboprophy[ccc19x$redcap_repeat_instrument == ''])
    
    #Rx17. ACEi at baseline
    ccc19x$der_acei_bl <- NA
    ccc19x$der_acei_bl[which(ccc19x$concomitant_meds___c09a == 1)] <- 1
    
    summary(factor(ccc19x$der_acei_bl))
    
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
    summary(ccc19x$der_acei_bl[ccc19x$redcap_repeat_instrument == ''])
    
    ######################
    #Rx18. ARB at baseline
    ######################
    ccc19x$der_arb_bl <- NA
    ccc19x$der_arb_bl[which(ccc19x$concomitant_meds___c09c == 1)] <- 1
    
    summary(factor(ccc19x$der_arb_bl))
    
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
    summary(ccc19x$der_arb_bl[ccc19x$redcap_repeat_instrument == ''])
    
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
    ccc19x$der_steroids_hd_bl[which((ccc19x$steroid_specific_2 %in% c(1,'1a','1b') |
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
    
    #Low-dose
    ccc19x$der_steroids_bl[which(ccc19x$steroid_specific_2 %in% c(1,'1a','1b'))] <- 'Low-dose'
    
    #High-dose
    ccc19x$der_steroids_bl[which(ccc19x$steroid_specific_2 %in% 2:3)] <- 'High-dose'
    
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
    
    #Rx12. Aspirin or APA ever (baseline or treatment for COVID-19)
    ccc19x$der_as_apa <- NA
    ccc19x$der_as_apa[which(ccc19x$concomitant_meds___n02ba == 1|
                              ccc19x$concomitant_meds___b01ac == 1|
                              ccc19x$covid_19_treatment___n02ba == 1|
                              ccc19x$covid_19_treatment___b01ac == 1|
                              ccc19x$covid_19_treatment_fu___n02ba == 1|
                              ccc19x$covid_19_treatment_fu___b01ac == 1
    )] <- 1
    
    summary(factor(ccc19x$der_as_apa[ccc19x$redcap_repeat_instrument == '']))
    
    #Never
    ccc19x$der_as_apa[which(is.na(ccc19x$der_as_apa) & ccc19x$concomitant_meds___n02ba == 0 &
                              ccc19x$concomitant_meds___b01ac == 0 &
                              ccc19x$covid_19_treatment___n02ba == 0 &
                              ccc19x$covid_19_treatment___b01ac == 0 &
                              (is.na(ccc19x$covid_19_treatment_fu___n02ba) | ccc19x$covid_19_treatment_fu___n02ba == 0) &
                              (is.na(ccc19x$covid_19_treatment_fu___b01ac) | ccc19x$covid_19_treatment_fu___b01ac == 0))] <- 0
    
    summary(factor(ccc19x$der_as_apa[ccc19x$redcap_repeat_instrument == '']))
    
    #Unknown baseline or treatment
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___|19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = 'concomitant_meds___unk|19_treatment___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_as_apa[i] == 0 | is.na(ccc19x$der_as_apa[i]))) ccc19x$der_as_apa[i] <- 99
    
    summary(factor(ccc19x$der_as_apa[ccc19x$redcap_repeat_instrument == '']))
    
    #Unknown f/u treatment
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_as_apa[i] == 0 | is.na(ccc19x$der_as_apa[i]))) ccc19x$der_as_apa[i] <- 99
    
    summary(factor(ccc19x$der_as_apa[ccc19x$redcap_repeat_instrument == '']))
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___|concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_as_apa[i] != 1|is.na(ccc19x$der_as_apa[i]))) ccc19x$der_as_apa[i] <- NA
    
    summary(factor(ccc19x$der_as_apa[ccc19x$redcap_repeat_instrument == '']))
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_as_apa[i] != 1|is.na(ccc19x$der_as_apa[i]))) ccc19x$der_as_apa[i] <- NA
    
    summary(factor(ccc19x$der_as_apa[ccc19x$redcap_repeat_instrument == '']))
    
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
    
    summary(factor(ccc19x$der_ac[ccc19x$redcap_repeat_instrument == '']))
    
    #Never
    ccc19x$der_ac[which(is.na(ccc19x$der_ac) & 
                          ccc19x$concomitant_meds___b01a == 0 &
                          ccc19x$covid_19_treatment___b01a == 0 &
                          (is.na(ccc19x$covid_19_treatment_fu___b01a) | ccc19x$covid_19_treatment_fu___b01a == 0))] <- 0
    
    summary(factor(ccc19x$der_ac[ccc19x$redcap_repeat_instrument == '']))
    
    #Unknown baseline or treatment
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'concomitant_meds___|19_treatment___') & 
                        !grepl(colnames(ccc19x), pattern = 'concomitant_meds___unk|19_treatment___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_ac[i] == 0 | is.na(ccc19x$der_ac[i]))) ccc19x$der_ac[i] <- 99
    
    summary(factor(ccc19x$der_ac[ccc19x$redcap_repeat_instrument == '']))
    
    #Unknown f/u treatment
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___') & 
                        !grepl(colnames(ccc19x), pattern = '19_treatment_fu___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_ac[i] == 0 | is.na(ccc19x$der_ac[i]))) ccc19x$der_ac[i] <- 99
    
    summary(factor(ccc19x$der_ac[ccc19x$redcap_repeat_instrument == '']))
    
    #Missing
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment___|concomitant_meds___'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_ac[i] != 1|is.na(ccc19x$der_ac[i]))) ccc19x$der_ac[i] <- NA
    
    summary(factor(ccc19x$der_ac[ccc19x$redcap_repeat_instrument == '']))
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = '19_treatment_fu___'))
    for(i in which(ccc19x$redcap_repeat_instrument == 'followup'))
      if(all(ccc19x[i,temp.ref] == 0) & (ccc19x$der_ac[i] != 1|is.na(ccc19x$der_ac[i]))) ccc19x$der_ac[i] <- NA
    
    summary(factor(ccc19x$der_ac[ccc19x$redcap_repeat_instrument == '']))
    
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
    summary(ccc19x$der_ac[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_rem[ccc19x$redcap_repeat_instrument == ''])
  }
  print('Treatments completed')
  
  #############
  #Demographics
  #############
  {
    #D1. age with estimation for categoricals
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
    
    #D1a. Age re-categorized
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
    
    #D1b. Age re-categorized v2
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
    
    "sex"
    #D2. Sex
    #recode other/prefer not to say as missing
    ccc19x$der_sex <- ccc19x$gender
    ccc19x$der_sex[which(ccc19x$der_sex == 0)] <- 'Female'
    ccc19x$der_sex[which(ccc19x$der_sex == 1)] <- 'Male'
    ccc19x$der_sex[which(ccc19x$der_sex %in% c(2:3))] <- NA
    
    #Factor
    ccc19x$der_sex <- as.factor(ccc19x$der_sex)
    summary(ccc19x$der_sex[ccc19x$redcap_repeat_instrument == ''])
    
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
    
    summary(ccc19x$der_smoking[ccc19x$redcap_repeat_instrument == ''])
    
    #D03b. Derived variable for smoking status collapsing the current/former smoker variables
    ccc19x$der_smoking2 <- NA
    ccc19x$der_smoking2[which(ccc19x$smoking_status == 3)] <- 'Never'
    ccc19x$der_smoking2[which(ccc19x$smoking_status %in% c("1","2", "2b", "2c", "2d", "2a"))] <- "Current or Former"
    ccc19x$der_smoking2[which(ccc19x$smoking_status == 99)] <- 'Unknown'
    
    #Factor
    ccc19x$der_smoking2 <- as.factor(ccc19x$der_smoking2)
    ccc19x$der_smoking2 <- relevel(ccc19x$der_smoking2, ref = 'Never')
    
    summary(ccc19x$der_smoking2[ccc19x$redcap_repeat_instrument == ''])
    
    ###############
    #Race/Ethnicity
    ###############
    
    #D04a. Derived variable for race/ethnicity
    ccc19x$der_race <- NA
    
    ccc19x$der_race[which(ccc19x$race___2054_5 == 1 & ccc19x$race___2106_3 == 0 &
                            ccc19x$ethnicity %in% c('2186-5','UNK'))] <- "Non-Hispanic Black"
    ccc19x$der_race[which(ccc19x$race___2106_3 == 1 & ccc19x$race___2054_5 == 0 &
                            ccc19x$ethnicity %in% c('2186-5','UNK'))] <- "Non-Hispanic White"
    
    ccc19x$der_race[which((ccc19x$race___1002_5 == 1|ccc19x$race___2028_9 == 1|
                             ccc19x$race___2076_8 ==1|ccc19x$race___2131_1 == 1|
                             ccc19x$race___unk == 1) & ccc19x$race___2106_3 == 0 &
                            ccc19x$race___2054_5 == 0)] <- 'Other'
    
    #Overwrite "Other" race with Hispanic ethnicity
    ccc19x$der_race[which(ccc19x$ethnicity == "2135-2")] <- "Hispanic"
    
    #Factor
    ccc19x$der_race <- as.factor(ccc19x$der_race)
    ccc19x$der_race <- relevel(ccc19x$der_race, ref = 'Non-Hispanic White')
    summary(ccc19x$der_race[ccc19x$redcap_repeat_instrument == ''])
    
    #D04b. Collapse all but NHW
    ccc19x$der_race_collapsed <- ccc19x$der_race
    ccc19x$der_race_collapsed[ccc19x$der_race_collapsed %in% c('Hispanic','Non-Hispanic Black')] <- 'Other'
    ccc19x$der_race_collapsed <- droplevels(ccc19x$der_race_collapsed)
    
    summary(ccc19x$der_race_collapsed[ccc19x$redcap_repeat_instrument == ''])
    
    #D04c. Derived variable for race/ethnicity including Asian
    ccc19x$der_race_v2 <- NA
    
    ccc19x$der_race_v2[which(ccc19x$race___2054_5 == 1 & ccc19x$race___2106_3 == 0 &
                               ccc19x$ethnicity %in% c('2186-5','UNK'))] <- "Non-Hispanic Black"
    ccc19x$der_race_v2[which(ccc19x$race___2106_3 == 1 & ccc19x$race___2054_5 == 0 &
                               ccc19x$ethnicity %in% c('2186-5','UNK'))] <- "Non-Hispanic White"
    
    #Overwrite the preceding if AAPI
    ccc19x$der_race_v2[which((ccc19x$race___2028_9 == 1|ccc19x$race___2076_8 ==1) &
                               ccc19x$ethnicity %in% c('2186-5','UNK'))] <- 'Non-Hispanic AAPI'
    
    #Other
    ccc19x$der_race_v2[which((ccc19x$race___1002_5 == 1|ccc19x$race___2131_1 == 1|
                                ccc19x$race___unk == 1) & !ccc19x$der_race_v2 %in% c('Non-Hispanic Black',
                                                                                     'Non-Hispanic White',
                                                                                     'Non-Hispanic AAPI'))] <- 'Other'
    
    #Overwrite "Other" with Hispanic ethnicity
    ccc19x$der_race_v2[which(ccc19x$ethnicity == "2135-2")] <- "Hispanic"
    
    #Factor
    ccc19x$der_race_v2 <- as.factor(ccc19x$der_race_v2)
    summary(ccc19x$der_race_v2[ccc19x$redcap_repeat_instrument == ''])
    
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
                               (ccc19x$recent_surgery == 1 & ccc19x$surgery_timing %in% c('2','3'))|
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
    summary(ccc19x$der_VTE_baseline[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_region_v2[ccc19x$redcap_repeat_instrument == ''])
    
    #D14b. Region with US collapsed
    ccc19x$der_region_v3 <- as.character(ccc19x$der_region)
    ccc19x$der_region_v3[ccc19x$country_of_patient_residen != 1 & ccc19x$redcap_repeat_instrument == ''] <- 'Non-US'
    ccc19x$der_region_v3[ccc19x$country_of_patient_residen == 1 & ccc19x$redcap_repeat_instrument == ''] <- 'US'
    
    #Factor
    ccc19x$der_region_v3 <- as.factor(ccc19x$der_region_v3)
    summary(ccc19x$der_region_v3[ccc19x$redcap_repeat_instrument == ''])
    
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
    
    summary(ccc19x$der_division[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_insurance[ccc19x$redcap_repeat_instrument == ''])
    
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
      cc19x$der_bmi <- ccc19x$bmi
      
      #Records with height/weight recorded, no BMI
      temp.ref <- which(is.na(ccc19x$der_bmi) & grepl(ccc19x$height, pattern = '[0-9]') & grepl(ccc19x$weight, pattern = '[0-9]'))
      
      #removing rows that are missing height or weight
      temp <- ccc19x[temp.ref,c('height','weight')]
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
      temp$height[which(temp$height == '5 feet 1 inch')] <- '61 inches'
      temp$height[which(temp$height == '5 feet 3 inches')] <- '63 inches'
      temp$height[which(temp$height == '5 feet 4 inches')] <- '64 inches'
      temp$height[which(temp$height == '5 feet 6 inches')] <- '66 inches'
      temp$height[which(temp$height == '5 feet 7 inches')] <- '67 inches'
      temp$height[which(temp$height == '5 feet 8 inches')] <- '68 inches'
      temp$height[which(temp$height == '5 feet 9 inches')] <- '69 inches'
      temp$height[which(temp$height == '5 feet 10 inches')] <- '70 inches'
      temp$height[which(temp$height == '5 feet 11 inches')] <- '71 inches'
      temp$height[which(temp$height == '6 feet')] <- '72 inches'
      temp$height[which(temp$height == '6 feet 2 inches')] <- '74 inches'
      
      #converted height strings into double values and put them in a new column
      temp$mheight <- temp$height
      temp$mheight <- gsub(temp$mheight, pattern = 'cm|[mM]| |in|inches', replacement = '', ignore.case = T)
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
      
      summary(ccc19x$der_bmi[ccc19x$redcap_repeat_instrument == ''])
    }
   
    "obesity"
    ##C02a. derived variable coding the obesity status (binary)
    {
    ccc19x$der_obesity <- NA
    
    ccc19x$der_obesity[which(ccc19x$significant_comorbidities___414916001 == 1 |
                               ccc19x$significant_comorbidities___238136002 == 1)] <- 1
    
    #Records with numeric BMI recorded or derived as >= 30
    ccc19x$der_obesity[which(ccc19x$der_bmi >= 30)] <- 1
    
    #Records with numeric BMI recorded or derived as < 30 (do not overwrite, for now)
    ccc19x$der_obesity[which(ccc19x$der_bmi < 30 & is.na(ccc19x$der_obesity))] <- 0
    
    #Not specified (map to Not obese for now)
    ccc19x$der_obesity[which(ccc19x$significant_comorbidities___238136002 == 0 &
                               ccc19x$significant_comorbidities___414916001 == 0 &
                               is.na(ccc19x$der_obesity))] <- 0
    
    #Revert "not obese" to NA if all the significant comorbidities are unchecked and BMI data not available
    temp.ref <- grep(colnames(ccc19x), pattern = 'significant_comorbidities___')
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_obesity[i] == 0) ccc19x$der_obesity[i] <- NA
    
    #Unknown
    ccc19x$der_obesity[which(ccc19x$significant_comorbidities___unk == 1 & is.na(ccc19x$der_obesity))] <- 99
    
    #Factor
    ccc19x$der_obesity <- as.factor(ccc19x$der_obesity)
    summary(ccc19x$der_obesity[ccc19x$redcap_repeat_instrument == ''])
    }
    
    ##C02b. derived variable coding the morbid obesity status (binary)
    {
    ccc19x$der_morbid_obesity <- NA
    
    ccc19x$der_morbid_obesity[which(ccc19x$significant_comorbidities___238136002 == 1)] <- 1
    
    #Records with numeric BMI recorded or derived as >= 40
    ccc19x$der_morbid_obesity[which(ccc19x$der_bmi >= 40)] <- 1
    
    #Records with numeric BMI recorded or derived as < 40 (do not overwrite, for now)
    ccc19x$der_morbid_obesity[which(ccc19x$der_bmi < 40 & is.na(ccc19x$der_morbid_obesity))] <- 0
    
    #Not specified
    ccc19x$der_morbid_obesity[which(ccc19x$significant_comorbidities___238136002 == 0 &
                                      is.na(ccc19x$der_morbid_obesity))] <- 0
    
    #Revert "not obese" to NA if all the significant comorbidities are unchecked and BMI data not available
    temp.ref <- grep(colnames(ccc19x), pattern = 'significant_comorbidities___')
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_morbid_obesity[i] == 0) ccc19x$der_morbid_obesity[i] <- NA
    
    #Unknown
    ccc19x$der_morbid_obesity[which(ccc19x$significant_comorbidities___unk == 1 & is.na(ccc19x$der_morbid_obesity))] <- 99
    
    #Factor
    ccc19x$der_morbid_obesity <- as.factor(ccc19x$der_morbid_obesity)
    summary(ccc19x$der_morbid_obesity[ccc19x$redcap_repeat_instrument == ''])
    }
    
    ##C02c. derived variable coding the morbid obesity status (binary, cutoff BMI 35)
    {
    ccc19x$der_morbid_obesity_v2 <- NA
    
    ccc19x$der_morbid_obesity_v2[which(ccc19x$significant_comorbidities___238136002 == 1)] <- 1
    
    #Records with numeric BMI recorded or derived as >= 35
    ccc19x$der_morbid_obesity_v2[which(ccc19x$der_bmi >= 35)] <- 1
    
    #Records with numeric BMI recorded or derived as < 35 (do not overwrite, for now)
    ccc19x$der_morbid_obesity_v2[which(ccc19x$der_bmi < 35 & is.na(ccc19x$der_morbid_obesity_v2))] <- 0
    
    #Not specified
    ccc19x$der_morbid_obesity_v2[which(ccc19x$significant_comorbidities___238136002 == 0 &
                                         is.na(ccc19x$der_morbid_obesity_v2))] <- 0
    
    #Revert "not obese" to NA if all the significant comorbidities are unchecked and BMI data not available
    temp.ref <- grep(colnames(ccc19x), pattern = 'significant_comorbidities___')
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$der_morbid_obesity_v2[i] == 0) ccc19x$der_morbid_obesity_v2[i] <- NA
    
    #Unknown
    ccc19x$der_morbid_obesity_v2[which(ccc19x$significant_comorbidities___unk == 1 & is.na(ccc19x$der_morbid_obesity_v2))] <- 99
    
    #Factor
    ccc19x$der_morbid_obesity_v2 <- as.factor(ccc19x$der_morbid_obesity_v2)
    summary(ccc19x$der_morbid_obesity_v2[ccc19x$redcap_repeat_instrument == ''])
    }
    
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
      summary(ccc19x$der_bmi_cat[ccc19x$redcap_repeat_instrument == ''])
    }
    
    #C03. Number of comorbidities (just factor)
    ccc19x$der_comorbid_no <- factor(ccc19x$comorbid_no)
    summary(ccc19x$der_comorbid_no)
    
    #C03a. Simplified # of comorbidities
    ccc19x$der_comorbid_no_collapsed <- as.character(ccc19x$der_comorbid_no)
    ccc19x$der_comorbid_no_collapsed[which(ccc19x$der_comorbid_no %in% c('2','3','4'))] <- 2
    ccc19x$der_comorbid_no_collapsed <- factor(ccc19x$der_comorbid_no_collapsed)
    summary(ccc19x$der_comorbid_no_collapsed[ccc19x$redcap_repeat_instrument == ''])
    
    #C03b. Simplified # of comorbidities 2
    ccc19x$der_comorbid_no_collapsed2 <- as.character(ccc19x$der_comorbid_no)
    ccc19x$der_comorbid_no_collapsed2[which(ccc19x$der_comorbid_no %in% c('1','2'))] <- '1 to 2'
    ccc19x$der_comorbid_no_collapsed2[which(ccc19x$der_comorbid_no %in% c('3','4'))] <- '3+'
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
    summary(ccc19x$der_dm2[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_autoimmune[ccc19x$redcap_repeat_instrument == ''])
    
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
    
    temp.ref <- which(grepl(colnames(ccc19x), pattern = 'significant_comorbidities') &
                        !grepl(colnames(ccc19x), pattern = 'significant_comorbidities___unk'))
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      if(all(ccc19x[i,temp.ref] == 0) & ccc19x$significant_comorbidities___unk[i] == 1) ccc19x$der_pulm[i] <- 99
    }
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_pulm[temp.ref]
      temp2 <- ccc19x$der_pulm[temp.ref][2:length(temp.ref)]
      temp2 <- temp2[!is.na(temp2)]
      if(length(temp[!is.na(temp)]) > 0)
      {
        if(any(temp[!is.na(temp)] == 1)) ccc19x$der_pulm[temp.ref] <- 1
        if(length(temp[2:length(temp)][!is.na(temp[2:length(temp)])]) > 0)
        {
          if((is.na(temp[1])|temp[1] == 0) & all(temp2 == 0) & !any(temp[!is.na(temp)] == 1)) ccc19x$der_pulm[temp.ref] <- 0
          if((is.na(temp[1])|temp[1] == 0) & any(temp2 == 99) & !any(temp[!is.na(temp)] == 1)) ccc19x$der_pulm[temp.ref] <- 99
        }
      }
    }
    
    ccc19x$der_pulm <- factor(ccc19x$der_pulm)
    summary(ccc19x$der_pulm[ccc19x$redcap_repeat_instrument == ''])
    
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
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_card[temp.ref]
      temp2 <- ccc19x$der_card[temp.ref][2:length(temp.ref)]
      temp2 <- temp2[!is.na(temp2)]
      if(length(temp[!is.na(temp)]) > 0)
      {
        if(any(temp[!is.na(temp)] == 1)) ccc19x$der_card[temp.ref] <- 1
        if(length(temp[2:length(temp)][!is.na(temp[2:length(temp)])]) > 0)
        {
          if((is.na(temp[1])|temp[1] == 0) & all(temp2 == 0) & !any(temp[!is.na(temp)] == 1)) ccc19x$der_card[temp.ref] <- 0
          if((is.na(temp[1])|temp[1] == 0) & any(temp2 == 99) & !any(temp[!is.na(temp)] == 1)) ccc19x$der_card[temp.ref] <- 99
        }
      }
    }
    
    ccc19x$der_card <- factor(ccc19x$der_card)
    summary(ccc19x$der_card[ccc19x$redcap_repeat_instrument == ''])
    
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
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_card_v2[temp.ref]
      temp2 <- ccc19x$der_card_v2[temp.ref][2:length(temp.ref)]
      temp2 <- temp2[!is.na(temp2)]
      if(length(temp[!is.na(temp)]) > 0)
      {
        if(any(temp[!is.na(temp)] == 1)) ccc19x$der_card_v2[temp.ref] <- 1
        if(length(temp[2:length(temp)][!is.na(temp[2:length(temp)])]) > 0)
        {
          if((is.na(temp[1])|temp[1] == 0) & all(temp2 == 0) & !any(temp[!is.na(temp)] == 1)) ccc19x$der_card_v2[temp.ref] <- 0
          if((is.na(temp[1])|temp[1] == 0) & any(temp2 == 99) & !any(temp[!is.na(temp)] == 1)) ccc19x$der_card_v2[temp.ref] <- 99
        }
      }
    }
    
    ccc19x$der_card_v2 <- factor(ccc19x$der_card_v2)
    summary(ccc19x$der_card_v2[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_CAD_bl[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_CHF_bl[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_PVD_bl[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_CVA_bl[ccc19x$redcap_repeat_instrument == ''])
    
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
    
    #Merge baseline and followup if discrepancy
    for(i in unique(ccc19x$record_id[which(ccc19x$redcap_repeat_instrument == 'followup')]))
    {
      temp.ref <- which(ccc19x$record_id == i)
      temp <- ccc19x$der_renal[temp.ref]
      temp2 <- ccc19x$der_renal[temp.ref][2:length(temp.ref)]
      temp2 <- temp2[!is.na(temp2)]
      if(length(temp[!is.na(temp)]) > 0)
      {
        if(any(temp[!is.na(temp)] == 1)) ccc19x$der_renal[temp.ref] <- 1
        if(length(temp[2:length(temp)][!is.na(temp[2:length(temp)])]) > 0)
        {
          if((is.na(temp[1])|temp[1] == 0) & all(temp2 == 0) & !any(temp[!is.na(temp)] == 1)) ccc19x$der_renal[temp.ref] <- 0
          if((is.na(temp[1])|temp[1] == 0) & any(temp2 == 99) & !any(temp[!is.na(temp)] == 1)) ccc19x$der_renal[temp.ref] <- 99
        }
      }
    }
    
    ccc19x$der_renal <- factor(ccc19x$der_renal)
    summary(ccc19x$der_renal[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_htn[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_hld[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_dementia[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_ccc19cci[ccc19x$redcap_repeat_instrument == ''])
    
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
    temp[which(ccc19x$der_sex == 'Male' & ccc19x$der_age >= 55)] <- temp[which(ccc19x$der_sex == 'Male' & ccc19x$der_age >= 55)] + 1
    temp[which(ccc19x$der_sex == 'Female' & ccc19x$der_age >= 60)] <- temp[which(ccc19x$der_sex == 'Female' & ccc19x$der_age >= 60)] + 1
    #Obesity
    temp[which(ccc19x$der_obesity == 'Obese')] <- temp[which(ccc19x$der_obesity == 'Obese')] + 1
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
    summary(ccc19x$der_CVD_risk[ccc19x$redcap_repeat_instrument == ''])
    
    #########################
    #C13a. CVD risk factor v2 
    #########################
    ccc19x$der_CVD_risk_v2 <- NA
    
    #Two or more risk factors
    temp <- rep(0, nrow(ccc19x))
    
    #Sex and age
    temp[which(ccc19x$der_sex == 'Male' & ccc19x$der_age >= 55)] <- temp[which(ccc19x$der_sex == 'Male' & ccc19x$der_age >= 55)] + 1
    temp[which(ccc19x$der_sex == 'Female' & ccc19x$der_age >= 60)] <- temp[which(ccc19x$der_sex == 'Female' & ccc19x$der_age >= 60)] + 1
    
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
    ccc19x$der_CVD_risk_v2[which(ccc19x$der_sex == 'Male' & ccc19x$der_age < 55)] <- 0
    ccc19x$der_CVD_risk_v2[which(ccc19x$der_sex == 'Female' & ccc19x$der_age < 60)] <- 0
    
    #Unknowns
    ccc19x$der_CVD_risk_v2[which((ccc19x$significant_comorbidities___38341003 == 0 & ccc19x$significant_comorbidities___unk == 1)|
                                   (ccc19x$significant_comorbidities___55822004 == 0 & ccc19x$significant_comorbidities___unk == 1)|
                                   ccc19x$der_dm2 == 99|
                                   ccc19x$der_smoking == 'Unknown')
    ] <- 'Unknown'
    
    #Already has CVD
    ccc19x$der_CVD_risk_v2[which(ccc19x$der_card_v2 == 1)] <- 'CVD already present'
    
    ccc19x$der_CVD_risk_v2 <- factor(ccc19x$der_CVD_risk_v2)
    summary(ccc19x$der_CVD_risk_v2[ccc19x$redcap_repeat_instrument == ''])
    
    }
  print('Comorbidities completed')
  
  #############
  #Cancer types
  #############
  {
    
    #Dx1-9. Binary indicators for heme types
    ccc19x$der_HemeNOS <- 0
    ccc19x$der_HemeNOS[which(ccc19x$cancer_type %in% c("C27134", "C9300","OTH_H")|
                               ccc19x$cancer_type_2 %in% c("C27134", "C9300","OTH_H")|
                               ccc19x$cancer_type_3 %in% c("C27134", "C9300","OTH_H")|
                               ccc19x$cancer_type_4 %in% c("C27134", "C9300","OTH_H")|
                               ccc19x$cancer_type_5 %in% c("C27134", "C9300","OTH_H"))] <- 1
    ccc19x$der_HemeNOS <- factor(ccc19x$der_HemeNOS)
    summary(ccc19x$der_HemeNOS[ccc19x$redcap_repeat_instrument == ''])
    
    #B-cell malignancies + acute lymphoblastic leukemia + aggressive NHL + indolent NHL and excluding plasma cell neoplasms
    ccc19x$der_Bcell <- 0
    ccc19x$der_Bcell[which(ccc19x$cancer_type %in% c("C3167","C8851","C9357","C9244","C4337","C2912","C8504","C3209","C3163","C4341","C9308")|
                                   ccc19x$cancer_type_2 %in% c("C3167","C8851","C9357","C9244","C4337","C2912","C8504","C3209","C3163","C4341","C9308")|
                                   ccc19x$cancer_type_3 %in% c("C3167","C8851","C9357","C9244","C4337","C2912","C8504","C3209","C3163","C4341","C9308")|
                                   ccc19x$cancer_type_4 %in% c("C3167","C8851","C9357","C9244","C4337","C2912","C8504","C3209","C3163","C4341","C9308")|
                                   ccc19x$cancer_type_5 %in% c("C3167","C8851","C9357","C9244","C4337","C2912","C8504","C3209","C3163","C4341","C9308"))] <- 1
    ccc19x$der_Bcell <- factor(ccc19x$der_Bcell)
    summary(ccc19x$der_Bcell[ccc19x$redcap_repeat_instrument == ''])
    
    ccc19x$der_Lymph_HGNHL <- 0
    ccc19x$der_Lymph_HGNHL[which(ccc19x$cancer_type %in% c("C8851","C9244","C2912")|
                                   ccc19x$cancer_type_2 %in% c("C8851","C9244","C2912")|
                                   ccc19x$cancer_type_3 %in% c("C8851","C9244","C2912")|
                                   ccc19x$cancer_type_4 %in% c("C8851","C9244","C2912")|
                                   ccc19x$cancer_type_5 %in% c("C8851","C9244","C2912"))] <- 1
    ccc19x$der_Lymph_HGNHL <- factor(ccc19x$der_Lymph_HGNHL)
    summary(ccc19x$der_Lymph_HGNHL[ccc19x$redcap_repeat_instrument == ''])
    
    ccc19x$der_Lymph_LGNHL <- 0
    ccc19x$der_Lymph_LGNHL[which(ccc19x$cancer_type %in% c("C3209", "C3163", "C4341", "C4337", "C8504")|
                                   ccc19x$cancer_type_2 %in% c("C3209", "C3163", "C4341", "C4337", "C8504")|
                                   ccc19x$cancer_type_3 %in% c("C3209", "C3163", "C4341", "C4337", "C8504")|
                                   ccc19x$cancer_type_4 %in% c("C3209", "C3163", "C4341", "C4337", "C8504")|
                                   ccc19x$cancer_type_5 %in% c("C3209", "C3163", "C4341", "C4337", "C8504"))] <- 1
    ccc19x$der_Lymph_LGNHL <- factor(ccc19x$der_Lymph_LGNHL)
    summary(ccc19x$der_Lymph_LGNHL[ccc19x$redcap_repeat_instrument == ''])
    
    ccc19x$der_ALL <- 0
    ccc19x$der_ALL[which(ccc19x$cancer_type %in% c("C3167")|
                           ccc19x$cancer_type_2 %in% c("C3167")|
                           ccc19x$cancer_type_3 %in% c("C3167")|
                           ccc19x$cancer_type_4 %in% c("C3167")|
                           ccc19x$cancer_type_5 %in% c("C3167"))] <- 1
    ccc19x$der_ALL <- factor(ccc19x$der_ALL)
    summary(ccc19x$der_ALL[ccc19x$redcap_repeat_instrument == ''])
    
    ccc19x$der_CLL <- 0
    ccc19x$der_CLL[which(ccc19x$cancer_type %in% c("C3163")|
                           ccc19x$cancer_type_2 %in% c("C3163")|
                           ccc19x$cancer_type_3 %in% c("C3163")|
                           ccc19x$cancer_type_4 %in% c("C3163")|
                           ccc19x$cancer_type_5 %in% c("C3163"))] <- 1
    ccc19x$der_CLL <- factor(ccc19x$der_CLL)
    summary(ccc19x$der_CLL[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_Lymph_Other[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_PCDs[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_Lymph[ccc19x$redcap_repeat_instrument == ''])
    
    #Myeloid malignancy
    ccc19x$der_Myeloid <- 0
    ccc19x$der_Myeloid[which(ccc19x$cancer_type %in% c("C3171","C4345","C3174","C3247")|
                             ccc19x$cancer_type_2 %in% c("C3171","C4345","C3174","C3247")|
                             ccc19x$cancer_type_3 %in% c("C3171","C4345","C3174","C3247")|
                             ccc19x$cancer_type_4 %in% c("C3171","C4345","C3174","C3247")|
                             ccc19x$cancer_type_5 %in% c("C3171","C4345","C3174","C3247"))] <- 1
    ccc19x$der_Myeloid <- factor(ccc19x$der_Myeloid)
    summary(ccc19x$der_Myeloid[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca8 Heme indicator
    ccc19x$der_heme <- NA
    ccc19x$der_heme[ccc19x$redcap_repeat_instrument == ''] <- 0
    ccc19x$der_heme[which(ccc19x$der_HemeNOS == 1|
                            ccc19x$der_Lymph == 1|
                            ccc19x$der_Myeloid == 1|
                            ccc19x$der_PCDs == 1)] <- 1
    
    ccc19x$der_heme <- factor(ccc19x$der_heme)
    summary(ccc19x$der_heme[ccc19x$redcap_repeat_instrument == ''])
    
    #Dx10- Solid tumor binary indicators
    ccc19x$der_Breast <- 0
    ccc19x$der_Breast[which(ccc19x$cancer_type %in% c("C4872")|
                              ccc19x$cancer_type_2 %in% c("C4872")|
                              ccc19x$cancer_type_3 %in% c("C4872")|
                              ccc19x$cancer_type_4 %in% c("C4872")|
                              ccc19x$cancer_type_5 %in% c("C4872"))] <- 1
    ccc19x$der_Breast <- factor(ccc19x$der_Breast)
    summary(ccc19x$der_Breast[ccc19x$redcap_repeat_instrument == ''])
    
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
    
    #Bladder
    ccc19x$der_Bladder <- 0
    ccc19x$der_Bladder[which(ccc19x$cancer_type %in% c("C4912")|
                                ccc19x$cancer_type_2 %in% c("C4912")|
                               ccc19x$cancer_type_3 %in% c("C4912")|
                               ccc19x$cancer_type_4 %in% c("C4912")|
                               ccc19x$cancer_type_5 %in% c("C4912"))] <- 1
    ccc19x$der_Bladder <- factor(ccc19x$der_Bladder)
    summary(ccc19x$der_Bladder[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_Other_GU[ccc19x$redcap_repeat_instrument == ''])
    
    #Thoracic
    ccc19x$der_Thoracic <- 0
    ccc19x$der_Thoracic[which(ccc19x$cancer_type %in% c("C4917", "C2926", "C4878", "C3234","C3411")|
                                ccc19x$cancer_type_2 %in% c("C4917", "C2926", "C4878", "C3234","C3411")|
                                ccc19x$cancer_type_3 %in% c("C4917", "C2926", "C4878", "C3234","C3411")|
                                ccc19x$cancer_type_4 %in% c("C4917", "C2926", "C4878", "C3234","C3411")|
                                ccc19x$cancer_type_5 %in% c("C4917", "C2926", "C4878", "C3234","C3411"))] <- 1
    ccc19x$der_Thoracic <- factor(ccc19x$der_Thoracic)
    summary(ccc19x$der_Thoracic[ccc19x$redcap_repeat_instrument == ''])
    
    #GI cancers
    
    #Lower GI
    ccc19x$der_LowerGI <- 0
    ccc19x$der_LowerGI[which(ccc19x$cancer_type %in% c("C9291","C4910","C9330","C7724","C2955","C9382")|
                               ccc19x$cancer_type_2 %in% c("C9291","C4910","C9330","C7724","C2955","C9382")|
                               ccc19x$cancer_type_3 %in% c("C9291","C4910","C9330","C7724","C2955","C9382")|
                               ccc19x$cancer_type_4 %in% c("C9291","C4910","C9330","C7724","C2955","C9382")|
                               ccc19x$cancer_type_5 %in% c("C9291","C4910","C9330","C7724","C2955","C9382"))] <- 1
    ccc19x$der_LowerGI <- factor(ccc19x$der_LowerGI)
    summary(ccc19x$der_LowerGI[ccc19x$redcap_repeat_instrument == ''])
    
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
    
    #GI overall
    ccc19x$der_GI <- 0
    ccc19x$der_GI[which(ccc19x$der_LowerGI == 1|ccc19x$der_UpperGI == 1)] <- 1
    ccc19x$der_GI <- factor(ccc19x$der_GI)
    summary(ccc19x$der_GI[ccc19x$redcap_repeat_instrument == ''])
    
    #Gyn
    ccc19x$der_Gyn <- 0
    ccc19x$der_Gyn[which(ccc19x$cancer_type %in% c("C7558", "C9039", "C7431","C3867","C3555","C3917","C4866")|
                           ccc19x$cancer_type_2 %in% c("C7558", "C9039", "C7431","C3867","C3555","C3917","C4866")|
                           ccc19x$cancer_type_3 %in% c("C7558", "C9039", "C7431","C3867","C3555","C3917","C4866")|
                           ccc19x$cancer_type_4 %in% c("C7558", "C9039", "C7431","C3867","C3555","C3917","C4866")|
                           ccc19x$cancer_type_5 %in% c("C7558", "C9039", "C7431","C3867","C3555","C3917","C4866"))] <- 1
    ccc19x$der_Gyn <- factor(ccc19x$der_Gyn)
    summary(ccc19x$der_Gyn[ccc19x$redcap_repeat_instrument == ''])
    
    #Endocrine
    ccc19x$der_Endo <- 0
    ccc19x$der_Endo[which(ccc19x$cancer_type %in% c("C4815", "C9325", "C3809", "C4906")|
                            ccc19x$cancer_type_2 %in% c("C4815", "C9325", "C3809", "C4906")|
                            ccc19x$cancer_type_3 %in% c("C4815", "C9325", "C3809", "C4906")|
                            ccc19x$cancer_type_4 %in% c("C4815", "C9325", "C3809", "C4906")|
                            ccc19x$cancer_type_5 %in% c("C4815", "C9325", "C3809", "C4906"))] <- 1
    ccc19x$der_Endo <- factor(ccc19x$der_Endo)
    summary(ccc19x$der_Endo[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_Derm[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_HN[ccc19x$redcap_repeat_instrument == ''])
    
    #Sarcoma
    ccc19x$der_Sarcoma <- 0
    ccc19x$der_Sarcoma[which(ccc19x$cancer_type %in% c("C9306", "C3868", "C9145","C9312","C4817","C3359","C8538")|
                               ccc19x$cancer_type_2 %in% c("C9306", "C3868", "C9145","C9312","C4817","C3359","C8538")|
                               ccc19x$cancer_type_3 %in% c("C9306", "C3868", "C9145","C9312","C4817","C3359","C8538")|
                               ccc19x$cancer_type_4 %in% c("C9306", "C3868", "C9145","C9312","C4817","C3359","C8538")|
                               ccc19x$cancer_type_5 %in% c("C9306", "C3868", "C9145","C9312","C4817","C3359","C8538"))] <- 1
    ccc19x$der_Sarcoma <- factor(ccc19x$der_Sarcoma)
    summary(ccc19x$der_Sarcoma[ccc19x$redcap_repeat_instrument == ''])
    
    #Neuro
    ccc19x$der_Neuro <- 0
    ccc19x$der_Neuro[which(ccc19x$cancer_type %in% c("C3059","C4627","C5111","C132067", "C3270", "C7541")|
                             ccc19x$cancer_type_2 %in% c("C3059","C4627","C5111","C132067", "C3270", "C7541")|
                             ccc19x$cancer_type_3 %in% c("C3059","C4627","C5111","C132067", "C3270", "C7541")|
                             ccc19x$cancer_type_4 %in% c("C3059","C4627","C5111","C132067", "C3270", "C7541")|
                             ccc19x$cancer_type_5 %in% c("C3059","C4627","C5111","C132067", "C3270", "C7541"))] <- 1
    ccc19x$der_Neuro <- factor(ccc19x$der_Neuro)
    summary(ccc19x$der_Neuro[ccc19x$redcap_repeat_instrument == ''])
    
    #Solid tumors NOS (including unassigned "Other" cancers)
    ccc19x$der_SolidNOS <- 0
    ccc19x$der_SolidNOS[which(ccc19x$cancer_type %in% c("C132146","C3538","C3708","C4039","OTH_S","OTH")|
                                ccc19x$cancer_type_2 %in% c("C132146","C3538","C3708","C4039","OTH_S","OTH")|
                                ccc19x$cancer_type_3 %in% c("C132146","C3538","C3708","C4039","OTH_S","OTH")|
                                ccc19x$cancer_type_4 %in% c("C132146","C3538","C3708","C4039","OTH_S","OTH")|
                                ccc19x$cancer_type_5 %in% c("C132146","C3538","C3708","C4039","OTH_S","OTH"))] <- 1
    ccc19x$der_SolidNOS <- factor(ccc19x$der_SolidNOS)
    summary(ccc19x$der_SolidNOS[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_solid[ccc19x$redcap_repeat_instrument == ''])
    
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
    
    #Ca02. Line of active therapy
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
    summary(ccc19x$der_txline[ccc19x$redcap_repeat_instrument == ''])
    
    ############################################
    #Ca10. Any cancer treatment in past 3 months
    ############################################
    ccc19x$der_anytx <- NA
    
    ccc19x$der_anytx[which((ccc19x$on_treatment == 1 & ccc19x$recent_treatment %in% 1:3)|
                             ccc19x$hx_treatment == 1)] <- 1
    
    ccc19x$der_anytx[which((ccc19x$on_treatment == 0 & ccc19x$hx_treatment != 1)|
                             (ccc19x$on_treatment == 1 & ccc19x$recent_treatment %in% 88 & ccc19x$hx_treatment != 1)|
                             ccc19x$recent_treatment == 98)] <- 0
    
    ccc19x$der_anytx[which((ccc19x$on_treatment == 99 | 
                              ccc19x$recent_treatment == 99 |
                              ccc19x$hx_treatment == 99 |
                              (ccc19x$on_treatment == 1 & is.na(ccc19x$recent_treatment))) &
                             is.na(ccc19x$der_anytx))] <- 99
    
    ccc19x$der_anytx <- factor(ccc19x$der_anytx)
    summary(ccc19x$der_anytx[ccc19x$redcap_repeat_instrument == ''])
    
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
    
    #####################################
    #Ca10a. Any cytotoxic within 3 months
    #####################################
    ccc19x$der_any_cyto <- ccc19x$der_anytx
    ccc19x$der_any_cyto[which(ccc19x$der_any_cyto == 1 & 
                                ((ccc19x$treatment_modality___685 == 0 & ccc19x$treatment_modality___45186 == 0)|
                                   (ccc19x$treatment_modality___694 == 0 &
                                      ccc19x$treatment_modality___45186 == 1 & ccc19x$transplant_cellular_therapy %in% c(10,2,3,4,5,6))))] <- 0
    summary(ccc19x$der_any_cyto[ccc19x$redcap_repeat_instrument == ''])
    
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
    ccc19x$der_immunosuppressed_v2[which(ccc19x$der_any_cyto == 1)] <- 1
    
    #Unknown
    ccc19x$der_immunosuppressed_v2[which((ccc19x$concomitant_meds___unk == 1|ccc19x$significant_comorbidities___unk == 1) &
                                           is.na(ccc19x$der_immunosuppressed_v2))] <- 99
    
    ccc19x$der_immunosuppressed_v2 <- factor(ccc19x$der_immunosuppressed_v2)
    summary(ccc19x$der_immunosuppressed_v2[ccc19x$redcap_repeat_instrument == ''])
    
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
    
    
    #Ca10b. Any immunotherapy within 3 months
    ccc19x$der_any_immuno <- ccc19x$der_anytx
    ccc19x$der_any_immuno[which(ccc19x$der_any_immuno == 1 & 
                                  ((ccc19x$treatment_modality___694 == 0 & ccc19x$treatment_modality___45186 == 0)|
                                     (ccc19x$treatment_modality___694 == 0 &
                                        ccc19x$treatment_modality___45186 == 1 & ccc19x$transplant_cellular_therapy == 1)))] <- 0
    summary(ccc19x$der_any_immuno[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca10c. Any targeted therapy within 3 months
    ccc19x$der_any_targeted <- ccc19x$der_anytx
    ccc19x$der_any_targeted[which(ccc19x$der_any_targeted == 1 & ccc19x$treatment_modality___58229 == 0)] <- 0
    summary(ccc19x$der_any_targeted[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca10d. Any endocrine therapy within 3 months
    ccc19x$der_any_endo <- ccc19x$der_anytx
    ccc19x$der_any_endo[which(ccc19x$der_any_endo == 1 & ccc19x$treatment_modality___691 == 0)] <- 0
    summary(ccc19x$der_any_endo[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca10e. Any radiation therapy within 3 months
    ccc19x$der_any_rt <- ccc19x$der_anytx
    ccc19x$der_any_rt[which(ccc19x$der_any_rt == 1 & ccc19x$treatment_modality___695 == 0)] <- 0
    summary(ccc19x$der_any_rt[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca10f. Any cancer surgery within 3 months
    ccc19x$der_any_ca_surgery <- ccc19x$der_anytx
    ccc19x$der_any_ca_surgery[which(ccc19x$der_any_ca_surgery == 1 & ccc19x$treatment_modality___14051 == 0)] <- 0
    summary(ccc19x$der_any_ca_surgery[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca10f1. Any ***surgery*** within 30 days
    ccc19x$der_any_surgery_1mo <- ccc19x$der_anytx_4wk
    ccc19x$der_any_surgery_1mo[which(ccc19x$der_any_surgery_1mo == 1 & ccc19x$treatment_modality___14051 == 0)] <- 0
    ccc19x$der_any_surgery_1mo[which(ccc19x$surgery_timing == 1)] <- 1
    ccc19x$der_any_surgery_1mo[which(ccc19x$surgery_timing %in% 2:3 &
                                       (is.na(ccc19x$der_any_surgery_1mo)|ccc19x$der_any_surgery_1mo == 99))] <- 0
    ccc19x$der_any_surgery_1mo[which(ccc19x$surgery_timing == 'UNK' & ccc19x$der_any_surgery_1mo == 0)] <- 99
    summary(ccc19x$der_any_surgery_1mo[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca10g. Any other therapy within 3 months
    ccc19x$der_any_other <- ccc19x$der_anytx
    ccc19x$der_any_other[which(ccc19x$der_any_other == 1 & 
                                 ccc19x$treatment_modality___45215 == 0 &
                                 ccc19x$treatment_modality___oth == 0)] <- 0
    summary(ccc19x$der_any_other[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca10h. Any targeted therapy or ICI within 3 months
    ccc19x$der_any_targeted_ici <- ccc19x$der_anytx
    ccc19x$der_any_targeted_ici[which(ccc19x$der_any_targeted_ici == 1 & 
                                        (ccc19x$treatment_modality___58229 == 1|
                                           ccc19x$what_immunotherapy %in% c('45838','45446',
                                                                            '45170','45838-45446')))] <- 1
    ccc19x$der_any_targeted_ici[which(ccc19x$der_any_targeted_ici == 1 & (ccc19x$treatment_modality___58229 == 0|
                                                                            !ccc19x$what_immunotherapy %in% c('45838','45446',
                                                                                                             '45170','45838-45446')))] <- 0
    
    ccc19x$der_any_targeted_ici <- factor(ccc19x$der_any_targeted_ici)
    summary(ccc19x$der_any_targeted_ici[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca10i. Any transplant or cellular therapy within 3 months
    ccc19x$der_any_sct_cellular <- ccc19x$der_anytx
    ccc19x$der_any_sct_cellular[which(ccc19x$der_any_sct_cellular == 1 & ccc19x$treatment_modality___45186 == 0)] <- 0
    ccc19x$der_any_sct_cellular[which(ccc19x$transplant_cellular_timing %in% c(0,3,4))] <- 0
    ccc19x$der_any_sct_cellular[which(ccc19x$transplant_cellular_timing %in% 1:2)] <- 1
    ccc19x$der_any_sct_cellular[which(ccc19x$transplant_cellular_timing == 99)] <- 99
    
    ccc19x$der_any_sct_cellular <- factor(ccc19x$der_any_sct_cellular)
    summary(ccc19x$der_any_sct_cellular[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca10j. Any local therapy within 3 months (surgery or radiation)
    ccc19x$der_any_local <- ccc19x$der_any_ca_surgery
    ccc19x$der_any_local[which(ccc19x$der_any_rt == 1)] <- 1
    summary(ccc19x$der_any_local[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca10k. Any systemic therapy within 3 months
    ccc19x$der_any_systemic <- ccc19x$der_any_cyto
    ccc19x$der_any_systemic[which(ccc19x$der_any_endo == 1|
                                      ccc19x$der_any_immuno == 1|
                                      ccc19x$der_any_targeted == 1)] <- 1
    summary(ccc19x$der_any_systemic[ccc19x$redcap_repeat_instrument == ''])
    
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
    
    #Ca10a1. Any cytotoxic chemotherapy within 12 months
    ccc19x$der_any_cyto_12mo <- ccc19x$der_any_systemic_v2
    ccc19x$der_any_cyto_12mo[which(ccc19x$der_any_cyto_12mo == 1 & ccc19x$treatment_modality___685 == 0)] <- 0
    summary(ccc19x$der_any_cyto_12mo[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca10c1. Any targeted therapy within 12 months
    ccc19x$der_any_targeted_12mo <- ccc19x$der_any_systemic_v2
    ccc19x$der_any_targeted_12mo[which(ccc19x$der_any_targeted_12mo == 1 & ccc19x$treatment_modality___58229 == 0)] <- 0
    summary(ccc19x$der_any_targeted_12mo[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca10d1. Any endocrine therapy within 12 months
    ccc19x$der_any_endo_12mo <- ccc19x$der_any_systemic_v2
    ccc19x$der_any_endo_12mo[which(ccc19x$der_any_endo_12mo == 1 & ccc19x$treatment_modality___691 == 0)] <- 0
    summary(ccc19x$der_any_endo_12mo[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_allo365[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca13. Autologous transplant within 100 days (default is no)
    ccc19x$der_auto100 <- 0
    ccc19x$der_auto100[which(ccc19x$transplant_cellular_therapy == 1 &
                               ccc19x$transplant_cellular_timing %in% 1:2)] <- 1
    ccc19x$der_auto100[which(ccc19x$transplant_cellular_therapy == 1 &
                               ccc19x$transplant_cellular_timing == 99)] <- 99
    ccc19x$der_auto100[which(ccc19x$transplant_cellular_therapy == 1 &
                               is.na(ccc19x$transplant_cellular_timing))] <- NA
    
    ccc19x$der_auto100 <- factor(ccc19x$der_auto100)
    summary(ccc19x$der_auto100[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_hct_recent[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca3a. ecogcat
    #categorical ecog variable, lumping 1 = 0/1, 2 = 2, and 3 = 3/4, 4 = unknown
    ccc19x$der_ecogcat <- NA
    ccc19x$der_ecogcat[which(ccc19x$ecog_status %in% c(0,1))] <- '0 or 1'
    ccc19x$der_ecogcat[which(ccc19x$ecog_status == 2)] <- 2
    ccc19x$der_ecogcat[which(ccc19x$ecog_status %in% c(3,4))] <- '3 or 4'
    ccc19x$der_ecogcat[which(ccc19x$ecog_status %in% 88:99)] <- 'Unknown'
    
    #Factor
    ccc19x$der_ecogcat <- as.factor(ccc19x$der_ecogcat)
    summary(ccc19x$der_ecogcat)
    
    #Ca3b. ECOG 0, 1, 2+
    ccc19x$der_ecogcat2 <- ccc19x$ecog_status
    ccc19x$der_ecogcat2[which(ccc19x$der_ecogcat2 %in% 2:4)] <- '2+'
    ccc19x$der_ecogcat2[which(ccc19x$der_ecogcat2 %in% 88:99)] <- 'Unknown'
    ccc19x$der_ecogcat2 <- factor(ccc19x$der_ecogcat2)
    summary(ccc19x$der_ecogcat2[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_cancer_status[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca07b. Cancer status without combining stable and responding
    ccc19x$der_cancer_status_v2 <- ccc19x$cancer_status
    ccc19x$der_cancer_status_v2[which(ccc19x$der_cancer_status_v2 == 1)] <- '0 - Remission/NED'
    ccc19x$der_cancer_status_v2[which(ccc19x$der_cancer_status_v2 == 2)] <- '1 - Active, responding'
    ccc19x$der_cancer_status_v2[which(ccc19x$der_cancer_status_v2 == 3)] <- '2 - Active, stable'
    ccc19x$der_cancer_status_v2[which(ccc19x$der_cancer_status_v2 == 4)] <- '3 - Active, progressing'
    ccc19x$der_cancer_status_v2[which(ccc19x$der_cancer_status_v2 %in% c(5,99))] <- '99 - Unknown'
    
    #Factor
    ccc19x$der_cancer_status_v2 <- as.factor(ccc19x$der_cancer_status_v2)
    summary(ccc19x$der_cancer_status_v2[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca07c. Cancer status without combining active unknown and unknown
    ccc19x$der_cancer_status_v3 <- ccc19x$cancer_status
    ccc19x$der_cancer_status_v3[which(ccc19x$der_cancer_status_v3 == 1)] <- '0 - Remission/NED'
    ccc19x$der_cancer_status_v3[which(ccc19x$der_cancer_status_v3 %in% c(2,3))] <- '1- Active, stable/responding'
    ccc19x$der_cancer_status_v3[which(ccc19x$der_cancer_status_v3 == 4)] <- '2 - Active, progressing'
    ccc19x$der_cancer_status_v3[which(ccc19x$der_cancer_status_v3 == 5)] <- '2-99 - Active, unknown status'
    ccc19x$der_cancer_status_v3[which(ccc19x$der_cancer_status_v3 == 99)] <- '99 - Unknown'
    
    #Factor
    ccc19x$der_cancer_status_v3 <- as.factor(ccc19x$der_cancer_status_v3)
    summary(ccc19x$der_cancer_status_v3[ccc19x$redcap_repeat_instrument == ''])
    
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
    summary(ccc19x$der_cancer_status_v4[ccc19x$redcap_repeat_instrument == ''])
    
    ############################################################
    #Ca07e. Cancer progressing at the time of COVID-19 diagnosis
    ccc19x$der_cancer_prog_bl <- NA
    
    #No
    ccc19x$der_cancer_prog_bl[which(ccc19x$cancer_status %in% 1:3)] <- 0
    
    #Yes
    ccc19x$der_cancer_prog_bl[which(ccc19x$cancer_status == 4)] <- 1
    
    #Unknown status
    ccc19x$der_cancer_prog_bl[which(ccc19x$cancer_status %in% c(5,99))] <- 99
    
    #Factor
    ccc19x$der_cancer_prog_bl <- as.factor(ccc19x$der_cancer_prog_bl)
    summary(ccc19x$der_cancer_prog_bl[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca19. Metastatic status (only applicable to solid tumors)
    ccc19x$der_metastatic <- NA
    
    #Yes
    ccc19x$der_metastatic[which(ccc19x$stage %in% c(4,'764-7'))] <- 1
    ccc19x$der_metastatic[which(ccc19x$mets_yn == 1)] <- 1
  
    #No
    ccc19x$der_metastatic[which(ccc19x$mets_yn == 0)] <- 0
    ccc19x$der_metastatic[which(ccc19x$cancer_status == 1)] <- 0
    
    #Unknown
    ccc19x$der_metastatic[which(ccc19x$cancer_status == 99 & is.na(ccc19x$der_metastatic))] <- 99
    ccc19x$der_metastatic[which(ccc19x$stage == 99 & is.na(ccc19x$der_metastatic))] <- 99
    ccc19x$der_metastatic[which(ccc19x$mets_yn == 99 & is.na(ccc19x$der_metastatic))] <- 99
    
    ccc19x$der_metastatic <- as.factor(ccc19x$der_metastatic)
    summary(ccc19x$der_metastatic[which(ccc19x$der_solid == 1)])
    
    #Ca19a. Metastatic disease to lung
    ccc19x$der_met_lung <- NA
    
    #Yes
    ccc19x$der_met_lung[which(ccc19x$mets_sites___1116_1 == 1)] <- 1
    
    #No
    ccc19x$der_met_lung[which(ccc19x$cancer_status == 1 & is.na(ccc19x$der_met_lung))] <- 0
    ccc19x$der_met_lung[which(ccc19x$mets_yn == 0 & is.na(ccc19x$der_met_lung))] <- 0
    ccc19x$der_met_lung[which(ccc19x$mets_yn == 1 & ccc19x$mets_sites___1116_1 == 0)] <- 0
    
    #Unknown
    ccc19x$der_met_lung[which(ccc19x$mets_yn == 99 & is.na(ccc19x$der_met_lung))] <- 99
    ccc19x$der_met_lung[which(ccc19x$mets_sites___99 == 1 & is.na(ccc19x$der_met_lung))] <- 99
    
    ccc19x$der_met_lung <- as.factor(ccc19x$der_met_lung)
    summary(ccc19x$der_met_lung[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca20. Stage at cancer diagnosis, simplified
    ccc19x$der_stage <- NA
    
    #Localized
    ccc19x$der_stage[which(ccc19x$stage %in% c(1:3,'764-1'))] <- 'Localized'
    
    #Disseminated
    ccc19x$der_stage[which(ccc19x$stage %in% c(4,'764-7'))] <- 'Disseminated'
    
    #Unknown
    ccc19x$der_stage[which(ccc19x$stage == 99)] <- 'Unknown'
    
    ccc19x$der_stage <- as.factor(ccc19x$der_stage)
    summary(ccc19x$der_stage[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca21. Active versus inactive cancer
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
    summary(ccc19x$der_active[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca4. Number of anti-cancer drugs
    
    #Load the curated file
    drugs <- read.csv(file = 'Mapping - medications/CCC19-ca-drugs-2021-06-10.csv', header = T, stringsAsFactors = F)
    drugs <- drugs[order(drugs$record_id),]
    
    #Just keep the rows with drug information
    drugs <- drugs[drugs$drug1 != '',]
    
    for(i in 3:ncol(drugs)) drugs[,i] <- trimws(drugs[,i])
    
    ccc19x$der_no_drugs <- NA
    
    #Count the drugs
    drugs.ref <- which(colnames(drugs) == 'drug1'):which(colnames(drugs) == 'drug12')
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
    drugs <- drugs[,c('record_id','drug1','drug2','drug3','drug4','drug5','drug6','drug7','drug8')]
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
      if(length(temp.ref) == 1) ccc19x[temp.ref,c('drug1','drug2','drug3','drug4','drug5','drug6','drug7','drug8')] <- drugs[i,2:9]
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
     
    #Ca24: Regimen match - this will only work if the HemOnc ontology is in the workspace
    ccc19x$der_regimen <- NA
    for(i in which(!is.na(ccc19x$drug1)))
    {
      #Get list of drugs
      temp <- ccc19x[i,c('drug1','drug2','drug3','drug4','drug5','drug6','drug7')]
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
        ccc19x$der_regimen[i] <- out
      } else ccc19x$der_regimen[i] <- 'No match'      
    }
    
    #Ca14. 1st generation ARA (only fill for prostate cancer patients, for now)
    ccc19x$der_ARA_1st_gen <- NA
    
    temp.ref <- which(ccc19x$der_Prostate == 1)
    ccc19x$der_ARA_1st_gen[temp.ref] <- 0
    ccc19x$der_ARA_1st_gen[which(ccc19x$prostate_tx___83008 == 1|ccc19x$prostate_tx___4508 == 1|ccc19x$prostate_tx___31805 == 1)] <- 1
    ccc19x$der_ARA_1st_gen[which(ccc19x$drug1 %in% c('Bicalutamide','Flutamide','Nilutamide')|
                                   ccc19x$drug2 %in% c('Bicalutamide','Flutamide','Nilutamide')|
                                   ccc19x$drug3 %in% c('Bicalutamide','Flutamide','Nilutamide')|
                                   ccc19x$drug4 %in% c('Bicalutamide','Flutamide','Nilutamide')|
                                   ccc19x$drug5 %in% c('Bicalutamide','Flutamide','Nilutamide')|
                                   ccc19x$drug6 %in% c('Bicalutamide','Flutamide','Nilutamide')|
                                   ccc19x$drug7 %in% c('Bicalutamide','Flutamide','Nilutamide'))] <- 1
    
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
    ccc19x$der_ARA_2nd_gen[which(ccc19x$drug1 %in% c('Apalutamide','Enzalutamide','Darolutamide')|
                                   ccc19x$drug2 %in% c('Apalutamide','Enzalutamide','Darolutamide')|
                                   ccc19x$drug3 %in% c('Apalutamide','Enzalutamide','Darolutamide')|
                                   ccc19x$drug4 %in% c('Apalutamide','Enzalutamide','Darolutamide')|
                                   ccc19x$drug5 %in% c('Apalutamide','Enzalutamide','Darolutamide')|
                                   ccc19x$drug6 %in% c('Apalutamide','Enzalutamide','Darolutamide')|
                                   ccc19x$drug7 %in% c('Apalutamide','Enzalutamide','Darolutamide'))] <- 1
    
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
    ccc19x$der_chemo_prca[which(ccc19x$drug1 %in% c('Cabazitaxel','Carboplatin','Docetaxel')|
                                  ccc19x$drug2 %in% c('Cabazitaxel','Carboplatin','Docetaxel')|
                                  ccc19x$drug3 %in% c('Cabazitaxel','Carboplatin','Docetaxel')|
                                  ccc19x$drug4 %in% c('Cabazitaxel','Carboplatin','Docetaxel')|
                                  ccc19x$drug5 %in% c('Cabazitaxel','Carboplatin','Docetaxel')|
                                  ccc19x$drug6 %in% c('Cabazitaxel','Carboplatin','Docetaxel')|
                                  ccc19x$drug7 %in% c('Cabazitaxel','Carboplatin','Docetaxel'))] <- 1
    
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
    
    ccc19x$der_adt[which((ccc19x$drug1 %in% c('ADT','Leuprolide','Degarelix','Goserelin','Histrelin','Triptorelin')|
                            ccc19x$drug2 %in% c('ADT','Leuprolide','Degarelix','Goserelin','Histrelin','Triptorelin')|
                            ccc19x$drug3 %in% c('ADT','Leuprolide','Degarelix','Goserelin','Histrelin','Triptorelin')|
                            ccc19x$drug4 %in% c('ADT','Leuprolide','Degarelix','Goserelin','Histrelin','Triptorelin')|
                            ccc19x$drug5 %in% c('ADT','Leuprolide','Degarelix','Goserelin','Histrelin','Triptorelin')|
                            ccc19x$drug6 %in% c('ADT','Leuprolide','Degarelix','Goserelin','Histrelin','Triptorelin')|
                            ccc19x$drug7 %in% c('ADT','Leuprolide','Degarelix','Goserelin','Histrelin','Triptorelin'))
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
    ccc19x$der_adt[which(ccc19x$prostate_tx___unk == 1 & is.na(ccc19x$der_adt))] <- 99
    
    ccc19x$der_adt <- factor(ccc19x$der_adt)
    summary(ccc19x$der_adt[ccc19x$der_Prostate == 1])
    
    #Ca4a: CD20 drugs
    ccc19x$der_cd20 <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1','drug2','drug3','drug4','drug5','drug6','drug7')] %in%
             c('Rituximab','Obinutuzumab','Ofatumumab','Ublituximab','Veltuzumab'))) ccc19x$der_cd20[temp.ref[i]] <- 1 else
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
      if(any(ccc19x[temp.ref[i],c('drug1','drug2','drug3','drug4','drug5','drug6','drug7')] %in%
             c('Rituximab','Obinutuzumab','Ofatumumab','Ublituximab','Veltuzumab'))) ccc19x$der_cd20_12mo[temp.ref[i]] <- 1 
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
      if(any(ccc19x[temp.ref[i],c('drug1','drug2','drug3','drug4','drug5','drug6','drug7')] %in%
             c('Rituximab','Obinutuzumab','Ofatumumab','Ublituximab','Veltuzumab'))) ccc19x$der_cd20_3mo[temp.ref[i]] <- 1 
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
      if(any(ccc19x[i,c('drug1','drug2','drug3','drug4','drug5','drug6','drug7')] %in%
             c('Prednisone','Prednisolone','Methylprednisolone','Dexamethasone','Hydrocortisone'))) ccc19x$der_cd20_cyto_12mo[i] <- 2
    
    ccc19x$der_cd20_cyto_12mo[which(ccc19x$der_cd20_12mo == 0|ccc19x$der_any_cyto_12mo == 0)] <- 0
    ccc19x$der_cd20_cyto_12mo[which(ccc19x$der_cd20_12mo == 99|ccc19x$der_any_cyto_12mo == 99)] <- 99
    
    ccc19x$der_cd20_cyto_12mo <- factor(ccc19x$der_cd20_cyto_12mo)
    summary(ccc19x$der_cd20_cyto_12mo[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca4m: CD38 drugs
    ccc19x$der_cd38 <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1','drug2','drug3','drug4','drug5','drug6','drug7')] %in%
             c('Daratumumab','Isatuximab'))) ccc19x$der_cd38[temp.ref[i]] <- 1 else
               ccc19x$der_cd38[temp.ref[i]] <- 0
    }
    
    ccc19x$der_cd38 <- factor(ccc19x$der_cd38)
    summary(ccc19x$der_cd38[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca4b: BTKi
    ccc19x$der_btki <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1','drug2','drug3','drug4','drug5','drug6','drug7')] %in%
             c('Ibrutinib','Acalabrutinib','LOXO-305'))) ccc19x$der_btki[temp.ref[i]] <- 1 else
               ccc19x$der_btki[temp.ref[i]] <- 0
    }
    
    ccc19x$der_btki <- factor(ccc19x$der_btki)
    summary(ccc19x$der_btki[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca4c: Venetoclax
    ccc19x$der_venet <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1','drug2','drug3','drug4','drug5','drug6','drug7')] %in%
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
      if(any(ccc19x[temp.ref[i],c('drug1','drug2','drug3','drug4','drug5','drug6','drug7')] %in%
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
      if(any(ccc19x[temp.ref[i],c('drug1','drug2','drug3','drug4','drug5','drug6','drug7')] %in%
             c('Letrozole','Exemestane','Anastrozole','Aromatase inhibitor'))) ccc19x$der_ai[temp.ref[i]] <- 1 else
               ccc19x$der_ai[temp.ref[i]] <- 0
    }
    
    ccc19x$der_ai <- factor(ccc19x$der_ai)
    summary(ccc19x$der_ai[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca4f: VEGF inhibitor
    ccc19x$der_vegfi <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1','drug2','drug3','drug4','drug5','drug6','drug7')] %in%
             c('Axitinib','Bevacizumab','Cabozantinib','Lenvatinib','Pazopanib',
               'Sorafenib','Sunitinib','Vandetanib','Ramucirumab'))) ccc19x$der_vegfi[temp.ref[i]] <- 1 else
               ccc19x$der_vegfi[temp.ref[i]] <- 0
    }
    
    ccc19x$der_vegfi <- factor(ccc19x$der_vegfi)
    summary(ccc19x$der_vegfi[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca4g: TKI inhibitor (incomplete list, possibly)
    ccc19x$der_tki <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1','drug2','drug3','drug4','drug5','drug6','drug7')] %in%
             c('Afatinib','Alectinib','Axitinib','Brigatinib','Cabozantinib','Ceritinib',
               'Crizotinib','Entrectinib','Erlotinib','Gefitinib','Gilteritinib','Lapatinib',
               'Lenvatinib','Lorlatinib','Neratinib','Nilotinib','Niraparib','Osimertinib',
               'Pazopanib','Ponatinib','Regorafenib','Ripretinib','Selpercatinib','Sorafenib',
               'Sunitinib','Acalabrutinib','Bosutinib','Dasatinib','Fedratinib','Ibrutinib',
               'Imatinib','Midostaurin','Pexidartinib','Pralsetinib','Ruxolitinib','Tucatinib',
               'Vandetinib'))) ccc19x$der_tki[temp.ref[i]] <- 1 else
                 ccc19x$der_tki[temp.ref[i]] <- 0
    }
    
    ccc19x$der_tki <- factor(ccc19x$der_tki)
    summary(ccc19x$der_tki[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca4h: CDK4/6 inhibitor
    ccc19x$der_cdk46i <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1','drug2','drug3','drug4','drug5','drug6','drug7')] %in%
             c('Abemaciclib','Palbociclib','Ribociclib'))) ccc19x$der_cdk46i[temp.ref[i]] <- 1 else
               ccc19x$der_cdk46i[temp.ref[i]] <- 0
    }
    
    ccc19x$der_cdk46i <- factor(ccc19x$der_cdk46i)
    summary(ccc19x$der_cdk46i[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca4i: GNRH agonists and antagonists
    ccc19x$der_gnrh <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1','drug2','drug3','drug4','drug5','drug6','drug7')] %in%
             c('ADT','Goserelin','Histrelin','Leuprolide','Triptorelin',
               'Abarelix','Degarelix','Relugolix'))) ccc19x$der_gnrh[temp.ref[i]] <- 1 else
                 ccc19x$der_gnrh[temp.ref[i]] <- 0
    }
    
    ccc19x$der_gnrh <- factor(ccc19x$der_gnrh)
    summary(ccc19x$der_gnrh[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca4j: Oral anti-androgen
    ccc19x$der_oral_antiandrogen <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1','drug2','drug3','drug4','drug5','drug6','drug7')] %in%
             c('Abiraterone','Apalutamide','Darolutamide','Enzalutmide',
               'Flutamide','Nilutamide','Bicalutamide'))) ccc19x$der_oral_antiandrogen[temp.ref[i]] <- 1 else
                 ccc19x$der_oral_antiandrogen[temp.ref[i]] <- 0
    }
    
    ccc19x$der_oral_antiandrogen <- factor(ccc19x$der_oral_antiandrogen)
    summary(ccc19x$der_oral_antiandrogen[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca4k: PD-1/PD-L1 antibodies
    ccc19x$der_pd1_l1 <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1','drug2','drug3','drug4','drug5','drug6','drug7')] %in%
             c('Nivolumab','Pembrolizumab','Atezolizumab','Avelumab',
               'Durvalumab','Cemiplimab'))) ccc19x$der_pd1_l1[temp.ref[i]] <- 1 else
                 ccc19x$der_pd1_l1[temp.ref[i]] <- 0
    }
    
    ccc19x$der_pd1_l1 <- factor(ccc19x$der_pd1_l1)
    summary(ccc19x$der_pd1_l1[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca4l: Anti-HER2
    ccc19x$der_her2 <- NA
    
    temp.ref <- which(!is.na(ccc19x$drug1) & ccc19x$redcap_repeat_instrument == '')
    for(i in 1:length(temp.ref))
    {
      if(any(ccc19x[temp.ref[i],c('drug1','drug2','drug3','drug4','drug5','drug6','drug7')] %in%
             c('Lapatinib','Neratinib','Tucatinib','Trastuzumab','Margetuximab',
               'Trastuzumab emtansine','Trastuzumab deruxtecan','Pertuzumab'))) ccc19x$der_her2[temp.ref[i]] <- 1 else
                 ccc19x$der_her2[temp.ref[i]] <- 0
    }
    
    ccc19x$der_her2 <- factor(ccc19x$der_her2)
    summary(ccc19x$der_her2[ccc19x$redcap_repeat_instrument == ''])
    
    #Ca6: Center type
    sites <- read.csv(file = '~/Box Sync/CCC19 data/Institution list.csv', header = T, stringsAsFactors = F)
    
    #ccc19x institutions, first
    temp <- unique(ccc19x$ccc19_institution)
    temp <- temp[temp != '']
    
    ccc19x$der_site_type <- NA
    for(i in 1:length(temp))
    {
      temp.ref <- which(ccc19x$ccc19_institution == temp[i])
      ccc19x$der_site_type[temp.ref] <- sites$Center.Type[sites$Center.ID == temp[i]]
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
    ccc19x$der_site_type[which(ccc19x$der_site_type %in% c('NCI-CC','NCI-CCC'))] <- 'TCC'
    
    #Factor
    ccc19x$der_site_type <- as.factor(ccc19x$der_site_type)
    summary(ccc19x$der_site_type)
    
  }
  print('Cancer treatment and related variables completed')
  
  ##################
  #Laboratory values
  ##################
  {
    #L1. Neutrophil:Lympocyte ratio, using categorical values
    ccc19x$der_nlr_cat <- NA
    
    #High
    ccc19x$der_nlr_cat[which(ccc19x$anc_range == 'HI' & ccc19x$alc_range %in% c('LO','WNL'))] <- 'HI'
    ccc19x$der_nlr_cat[which(ccc19x$anc_range == 'WNL' & ccc19x$alc_range %in% c('LO'))] <- 'HI'
    
    #Low
    ccc19x$der_nlr_cat[which(ccc19x$anc_range == 'WNL' & ccc19x$alc_range %in% c('HI'))] <- 'LO'
    ccc19x$der_nlr_cat[which(ccc19x$anc_range == 'LO' & ccc19x$alc_range %in% c('WNL','HI'))] <- 'LO'
    
    #Neither high nor low
    ccc19x$der_nlr_cat[which(ccc19x$anc_range == 'WNL' & ccc19x$alc_range %in% c('WNL'))] <- 'Neither'
    ccc19x$der_nlr_cat[which(ccc19x$anc_range == 'LO' & ccc19x$alc_range %in% c('LO'))] <- 'Neither'
    ccc19x$der_nlr_cat[which(ccc19x$anc_range == 'HI' & ccc19x$alc_range %in% c('HI'))] <- 'Neither'
    
    #Not tested
    ccc19x$der_nlr_cat[which((ccc19x$anc_range == 'NT' & ccc19x$alc_range %in% c('NT'))|
                               ccc19x$labs == 3)] <- 'Not drawn/Not available'
    
    ccc19x$der_nlr_cat <- factor(ccc19x$der_nlr_cat)
    summary(ccc19x$der_nlr_cat[ccc19x$redcap_repeat_instrument == ''])
    
    #L2. D-Dimer
    ccc19x$der_ddimer <- NA
    ccc19x$der_ddimer[which(ccc19x$ddimer == 0)] <- 'Normal'
    ccc19x$der_ddimer[which(ccc19x$ddimer == 1)] <- 'Abnormal'
    ccc19x$der_ddimer[which(ccc19x$ddimer == 99)] <- 'Unknown'
    ccc19x$der_ddimer[which(ccc19x$labs == 3|ccc19x$ddimer == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_ddimer <- factor(ccc19x$der_ddimer)
    ccc19x$der_ddimer <- relevel(ccc19x$der_ddimer, ref = 'Normal')
    summary(ccc19x$der_ddimer[ccc19x$redcap_repeat_instrument == ''])
    
    #L3. Fibrinogen
    ccc19x$der_fibrinogen <- NA
    ccc19x$der_fibrinogen[which(ccc19x$fibrinogen == 0)] <- 'Normal'
    ccc19x$der_fibrinogen[which(ccc19x$fibrinogen == 1)] <- 'Abnormal'
    ccc19x$der_fibrinogen[which(ccc19x$fibrinogen == 99)] <- 'Unknown'
    ccc19x$der_fibrinogen[which(ccc19x$labs == 3|ccc19x$fibrinogen == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_fibrinogen <- factor(ccc19x$der_fibrinogen)
    ccc19x$der_fibrinogen <- relevel(ccc19x$der_fibrinogen, ref = 'Normal')
    summary(ccc19x$der_fibrinogen[ccc19x$redcap_repeat_instrument == ''])
    
    #L4. PT
    ccc19x$der_pt <- NA
    ccc19x$der_pt[which(ccc19x$pt == 0)] <- 'Normal'
    ccc19x$der_pt[which(ccc19x$pt == 1)] <- 'Abnormal'
    ccc19x$der_pt[which(ccc19x$pt == 99)] <- 'Unknown'
    ccc19x$der_pt[which(ccc19x$labs == 3|ccc19x$pt == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_pt <- factor(ccc19x$der_pt)
    ccc19x$der_pt <- relevel(ccc19x$der_pt, ref = 'Normal')
    summary(ccc19x$der_pt[ccc19x$redcap_repeat_instrument == ''])
    
    #L5. aPTT
    ccc19x$der_aptt <- NA
    ccc19x$der_aptt[which(ccc19x$aptt == 0)] <- 'Normal'
    ccc19x$der_aptt[which(ccc19x$aptt == 1)] <- 'Abnormal'
    ccc19x$der_aptt[which(ccc19x$aptt == 99)] <- 'Unknown'
    ccc19x$der_aptt[which(ccc19x$labs == 3|ccc19x$aptt == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_aptt <- factor(ccc19x$der_aptt)
    ccc19x$der_aptt <- relevel(ccc19x$der_aptt, ref = 'Normal')
    summary(ccc19x$der_aptt[ccc19x$redcap_repeat_instrument == ''])
    
    #L6. High-sensitivity troponin
    ccc19x$der_hs_trop <- NA
    ccc19x$der_hs_trop[which(ccc19x$hs_trop == 0)] <- 'Normal'
    ccc19x$der_hs_trop[which(ccc19x$hs_trop == 1)] <- 'Abnormal'
    ccc19x$der_hs_trop[which(ccc19x$hs_trop == 99)] <- 'Unknown'
    ccc19x$der_hs_trop[which(ccc19x$labs == 3|ccc19x$hs_trop == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_hs_trop <- factor(ccc19x$der_hs_trop)
    ccc19x$der_hs_trop <- relevel(ccc19x$der_hs_trop, ref = 'Normal')
    summary(ccc19x$der_hs_trop[ccc19x$redcap_repeat_instrument == ''])
    
    #L7. BNP
    ccc19x$der_bnp <- NA
    ccc19x$der_bnp[which(ccc19x$bnp == 0)] <- 'Normal'
    ccc19x$der_bnp[which(ccc19x$bnp == 1)] <- 'Abnormal'
    ccc19x$der_bnp[which(ccc19x$bnp == 99)] <- 'Unknown'
    ccc19x$der_bnp[which(ccc19x$labs == 3|ccc19x$bnp == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_bnp <- factor(ccc19x$der_bnp)
    ccc19x$der_bnp <- relevel(ccc19x$der_bnp, ref = 'Normal')
    summary(ccc19x$der_bnp[ccc19x$redcap_repeat_instrument == ''])
    
    #L8. crp
    ccc19x$der_crp <- NA
    ccc19x$der_crp[which(ccc19x$crp == 0)] <- 'Normal'
    ccc19x$der_crp[which(ccc19x$crp == 1)] <- 'Abnormal'
    ccc19x$der_crp[which(ccc19x$crp == 99)] <- 'Unknown'
    ccc19x$der_crp[which(ccc19x$labs == 3|ccc19x$crp == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_crp <- factor(ccc19x$der_crp)
    ccc19x$der_crp <- relevel(ccc19x$der_crp, ref = 'Normal')
    summary(ccc19x$der_crp[ccc19x$redcap_repeat_instrument == ''])
    
    #L9. ldh
    ccc19x$der_ldh <- NA
    ccc19x$der_ldh[which(ccc19x$ldh == 0)] <- 'Normal'
    ccc19x$der_ldh[which(ccc19x$ldh == 1)] <- 'Abnormal'
    ccc19x$der_ldh[which(ccc19x$ldh == 99)] <- 'Unknown'
    ccc19x$der_ldh[which(ccc19x$labs == 3|ccc19x$ldh == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_ldh <- factor(ccc19x$der_ldh)
    ccc19x$der_ldh <- relevel(ccc19x$der_ldh, ref = 'Normal')
    summary(ccc19x$der_ldh[ccc19x$redcap_repeat_instrument == ''])
    
    #L10. il6
    ccc19x$der_il6 <- NA
    ccc19x$der_il6[which(ccc19x$il6 == 0)] <- 'Normal'
    ccc19x$der_il6[which(ccc19x$il6 == 1)] <- 'Abnormal'
    ccc19x$der_il6[which(ccc19x$il6 == 99)] <- 'Unknown'
    ccc19x$der_il6[which(ccc19x$labs == 3|ccc19x$il6 == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_il6 <- factor(ccc19x$der_il6)
    ccc19x$der_il6 <- relevel(ccc19x$der_il6, ref = 'Normal')
    summary(ccc19x$der_il6[ccc19x$redcap_repeat_instrument == ''])
    
    #L11. creatinine
    ccc19x$der_creat <- NA
    ccc19x$der_creat[which(ccc19x$creat == 0)] <- 'Normal'
    ccc19x$der_creat[which(ccc19x$creat == 1)] <- 'Abnormal'
    ccc19x$der_creat[which(ccc19x$creat == 99)] <- 'Unknown'
    ccc19x$der_creat[which(ccc19x$labs == 3|ccc19x$creat == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_creat <- factor(ccc19x$der_creat)
    ccc19x$der_creat <- relevel(ccc19x$der_creat, ref = 'Normal')
    summary(ccc19x$der_creat[ccc19x$redcap_repeat_instrument == ''])
    
    #L12. wbc
    ccc19x$der_wbc <- NA
    ccc19x$der_wbc[which(ccc19x$wbc_range == 'WNL')] <- 'Normal'
    ccc19x$der_wbc[which(ccc19x$wbc_range == 'LO')] <- 'Low'
    ccc19x$der_wbc[which(ccc19x$wbc_range == 'HI')] <- 'High'
    ccc19x$der_wbc[which(ccc19x$wbc_range == 99)] <- 'Unknown'
    ccc19x$der_wbc[which(ccc19x$labs == 3|ccc19x$wbc_range == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_wbc <- factor(ccc19x$der_wbc)
    ccc19x$der_wbc <- relevel(ccc19x$der_wbc, ref = 'Normal')
    summary(ccc19x$der_wbc[ccc19x$redcap_repeat_instrument == ''])
    
    #L13. hgb
    ccc19x$der_hgb <- NA
    ccc19x$der_hgb[which(ccc19x$hgb_range == 'WNL')] <- 'Normal'
    ccc19x$der_hgb[which(ccc19x$hgb_range == 'LO')] <- 'Low'
    ccc19x$der_hgb[which(ccc19x$hgb_range == 'HI')] <- 'High'
    ccc19x$der_hgb[which(ccc19x$hgb_range == 99)] <- 'Unknown'
    ccc19x$der_hgb[which(ccc19x$labs == 3|ccc19x$hgb_range == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_hgb <- factor(ccc19x$der_hgb)
    ccc19x$der_hgb <- relevel(ccc19x$der_hgb, ref = 'Normal')
    summary(ccc19x$der_hgb[ccc19x$redcap_repeat_instrument == ''])
    
    #L14. plt
    ccc19x$der_plt <- NA
    ccc19x$der_plt[which(ccc19x$plt_range == 'WNL')] <- 'Normal'
    ccc19x$der_plt[which(ccc19x$plt_range == 'LO')] <- 'Low'
    ccc19x$der_plt[which(ccc19x$plt_range == 'HI')] <- 'High'
    ccc19x$der_plt[which(ccc19x$plt_range == 99)] <- 'Unknown'
    ccc19x$der_plt[which(ccc19x$labs == 3|ccc19x$plt_range == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_plt <- factor(ccc19x$der_plt)
    ccc19x$der_plt <- relevel(ccc19x$der_plt, ref = 'Normal')
    summary(ccc19x$der_plt[ccc19x$redcap_repeat_instrument == ''])
    
    #L15. alc
    ccc19x$der_alc <- NA
    ccc19x$der_alc[which(ccc19x$alc_range == 'WNL')] <- 'Normal'
    ccc19x$der_alc[which(ccc19x$alc_range == 'LO')] <- 'Low'
    ccc19x$der_alc[which(ccc19x$alc_range == 'HI')] <- 'High'
    ccc19x$der_alc[which(ccc19x$alc_range == 99)] <- 'Unknown'
    ccc19x$der_alc[which(ccc19x$labs == 3|ccc19x$alc_range == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_alc <- factor(ccc19x$der_alc)
    ccc19x$der_alc <- relevel(ccc19x$der_alc, ref = 'Normal')
    summary(ccc19x$der_alc[ccc19x$redcap_repeat_instrument == ''])
    
    #L15a. lymphopenia
    ccc19x$der_lymphopenia <- NA
    ccc19x$der_lymphopenia[which(ccc19x$alc_range %in% c('WNL','HI'))] <- 'Not lymphopenic'
    ccc19x$der_lymphopenia[which(ccc19x$alc_range == 'LO')] <- 'Lymphopenic'
    ccc19x$der_lymphopenia[which(ccc19x$alc_range == 99)] <- 'Unknown'
    ccc19x$der_lymphopenia[which(ccc19x$labs == 3|ccc19x$alc_range == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_lymphopenia <- factor(ccc19x$der_lymphopenia)
    ccc19x$der_lymphopenia <- relevel(ccc19x$der_lymphopenia, ref = 'Not lymphopenic')
    summary(ccc19x$der_lymphopenia[ccc19x$redcap_repeat_instrument == ''])
    
    #L16. anc
    ccc19x$der_anc <- NA
    ccc19x$der_anc[which(ccc19x$anc_range == 'WNL')] <- 'Normal'
    ccc19x$der_anc[which(ccc19x$anc_range == 'LO')] <- 'Low'
    ccc19x$der_anc[which(ccc19x$anc_range == 'HI')] <- 'High'
    ccc19x$der_anc[which(ccc19x$anc_range == 99)] <- 'Unknown'
    ccc19x$der_anc[which(ccc19x$labs == 3|ccc19x$anc_range == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_anc <- factor(ccc19x$der_anc)
    ccc19x$der_anc <- relevel(ccc19x$der_anc, ref = 'Normal')
    summary(ccc19x$der_anc[ccc19x$redcap_repeat_instrument == ''])
    
    #L17. Troponin
    ccc19x$der_tni <- NA
    ccc19x$der_tni[which(ccc19x$tni == 0)] <- 'Normal'
    ccc19x$der_tni[which(ccc19x$tni == 1)] <- 'Abnormal'
    ccc19x$der_tni[which(ccc19x$tni == 99)] <- 'Unknown'
    ccc19x$der_tni[which(ccc19x$labs == 3|ccc19x$tni == 'NT')] <- 'Not drawn/Not available'
    ccc19x$der_tni <- factor(ccc19x$der_tni)
    ccc19x$der_tni <- relevel(ccc19x$der_tni, ref = 'Normal')
    summary(ccc19x$der_tni[ccc19x$redcap_repeat_instrument == ''])
    
    #L18. Combined lymphopenia and neutropenia
    ccc19x$der_lnpenia <- NA
    ccc19x$der_lnpenia[which(ccc19x$anc_range == 'LO')] <- 'Neutropenia'
    ccc19x$der_lnpenia[which(ccc19x$alc_range == 'LO')] <- 'Lymphopenia'
    ccc19x$der_lnpenia[which(ccc19x$alc_range == 'LO' & ccc19x$anc_range == 'LO')] <- 'Both'
    ccc19x$der_lnpenia[which(ccc19x$alc_range %in% c('WNL','HI') & ccc19x$anc_range %in% c('WNL','HI'))] <- 'Neither'
    
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
    summary(ccc19x$der_trop_combined[ccc19x$redcap_repeat_instrument == ''])
    
  
    
    #L20. WBC value transformation
    
    #creating the transformed row
    ccc19x$transformed_wbc <- ccc19x$wbc_numeric
    
    #Determine the units (cells/uL or cells*10^9/L) for any wbc with a decimal value, greater than 500, or less than 100 (proper units for this variable is cells*10^9/L, as stated on REDCAP variable description)
    
    ccc19x[which(ccc19x[,"transformed_wbc"] > 500), "transformed_wbc"] <- ccc19x[which(ccc19x[,"transformed_wbc"] > 500), "transformed_wbc"]/1000
    
    summary(ccc19x$wbc_numeric[ccc19x$redcap_repeat_instrument == ''])
    summary(ccc19x$transformed_wbc[ccc19x$redcap_repeat_instrument == ''])
    boxplot(ccc19x$transformed_wbc)
    
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
    
    #L26. Creatinine transformation
    
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
    ccc19x$der_imwg[which(ccc19x$der_age <= 75)] <- 0
    ccc19x$der_imwg[which(ccc19x$der_age > 75 & ccc19x$der_age <= 80)] <- 1
    ccc19x$der_imwg[which(ccc19x$der_age > 80)] <- 2
    
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
    summary(ccc19x$der_imwg[ccc19x$redcap_repeat_instrument == ''])
    
    #X2a. Modified IMWG frailty index
    #Patients will be categorized as nonfrail (0-1 pt), prefrail/intermediate (2-3 pts) or frail (4-6 pts)
    ccc19x$der_imwg_mod <- NA
    
    #Age
    ccc19x$der_imwg_mod[which(ccc19x$der_age <= 75)] <- 0
    ccc19x$der_imwg_mod[which(ccc19x$der_age > 75 & ccc19x$der_age <= 80)] <- 1
    ccc19x$der_imwg_mod[which(ccc19x$der_age > 80)] <- 2
    
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
    summary(ccc19x$der_imwg_mod[ccc19x$redcap_repeat_instrument == ''])
    
    #####################
    #X3. Modified Khorana
    #####################
    ccc19x$der_VTE_risk <- NA
    ccc19x$der_VTE_risk[which((ccc19x$cancer_type %in% c("C3850","C4911","C3513")| 
                                 ccc19x$cancer_type_2 %in% c("C3850","C4911", "C3513")| 
                                 ccc19x$cancer_type_3 %in% c("C3850","C4911", "C3513")| 
                                 ccc19x$cancer_type_4 %in% c("C3850","C4911", "C3513")| 
                                 ccc19x$cancer_type_5 %in% c("C3850","C4911", "C3513")))] <- 'High-risk VTE malignancy'
    ccc19x$der_VTE_risk[which((ccc19x$cancer_type %in% c("C4917","C2926","C4878","C7431","C9063","C4912","C3708","C7355","C9385","C8851","C3209","C9244","C4341","C3211","C9357","C4337","C2912","C8504","C27908")| 
                                 ccc19x$cancer_type_2 %in% c("C4917","C2926","C4878","C7431","C9063","C4912","C3708","C7355","C9385","C8851","C3209","C9244","C4341","C3211","C9357","C4337","C2912","C8504","C27908")| 
                                 ccc19x$cancer_type_3 %in% c("C4917","C2926","C4878","C7431","C9063","C4912","C3708","C7355","C9385","C8851","C3209","C9244","C4341","C3211","C9357","C4337","C2912","C8504","C27908")| 
                                 ccc19x$cancer_type_4 %in% c("C4917","C2926","C4878","C7431","C9063","C4912","C3708","C7355","C9385","C8851","C3209","C9244","C4341","C3211","C9357","C4337","C2912","C8504","C27908")| 
                                 ccc19x$cancer_type_5 %in% c("C4917","C2926","C4878","C7431","C9063","C4912","C3708","C7355","C9385","C8851","C3209","C9244","C4341","C3211","C9357","C4337","C2912","C8504","C27908")))] <- 'Intermediate-risk VTE malignancy'
    ccc19x$der_VTE_risk[which((ccc19x$cancer_type %in% c("C4872","C4863","C9291","C4910","C9330","C7724","C2955","C9382","C4013", "C3871")| 
                                 ccc19x$cancer_type_2 %in% c("C4872","C4863","C9291","C4910","C9330","C7724","C2955","C9382","C4013", "C3871")| 
                                 ccc19x$cancer_type_3 %in% c("C4872","C4863","C9291","C4910","C9330","C7724","C2955","C9382","C4013", "C3871")| 
                                 ccc19x$cancer_type_4 %in% c("C4872","C4863","C9291","C4910","C9330","C7724","C2955","C9382","C4013", "C3871")| 
                                 ccc19x$cancer_type_5 %in% c("C4872","C4863","C9291","C4910","C9330","C7724","C2955","C9382","C4013", "C3871")))] <- 'Low-risk VTE malignancy'
    ccc19x$der_VTE_risk[which((ccc19x$cancer_type %in% c("C3234","C3411","C3099","C3844","C4436","C3267", "C7558", "C9039","C3867","C3555","C3917","C4866","C4815", "C9325", "C3809", "C4906","C9306", "C3868", "C9145","C9312","C4817","C3359","C8538","C3059","C4627","C5111","C132067", "C3270", "C7541","C3224", "C9231","C4819","C2921","C132146","C4039","C3538","C9061","C6389","C4189","OTH","OTH_S")|
                                 ccc19x$cancer_type_2 %in% c("C3234","C3411","C3099","C3844","C4436","C3267", "C7558", "C9039","C3867","C3555","C3917","C4866","C4815", "C9325", "C3809", "C4906","C9306", "C3868", "C9145","C9312","C4817","C3359","C8538","C3059","C4627","C5111","C132067", "C3270", "C7541","C3224", "C9231","C4819","C2921","C132146","C4039","C3538","C9061","C6389","C4189","OTH","OTH_S")|
                                 ccc19x$cancer_type_3 %in% c("C3234","C3411","C3099","C3844","C4436","C3267", "C7558", "C9039","C3867","C3555","C3917","C4866","C4815", "C9325", "C3809", "C4906","C9306", "C3868", "C9145","C9312","C4817","C3359","C8538","C3059","C4627","C5111","C132067", "C3270", "C7541","C3224", "C9231","C4819","C2921","C132146","C4039","C3538","C9061","C6389","C4189","OTH","OTH_S")|
                                 ccc19x$cancer_type_4 %in% c("C3234","C3411","C3099","C3844","C4436","C3267", "C7558", "C9039","C3867","C3555","C3917","C4866","C4815", "C9325", "C3809", "C4906","C9306", "C3868", "C9145","C9312","C4817","C3359","C8538","C3059","C4627","C5111","C132067", "C3270", "C7541","C3224", "C9231","C4819","C2921","C132146","C4039","C3538","C9061","C6389","C4189","OTH","OTH_S")|
                                 ccc19x$cancer_type_5 %in% c("C3234","C3411","C3099","C3844","C4436","C3267", "C7558", "C9039","C3867","C3555","C3917","C4866","C4815", "C9325", "C3809", "C4906","C9306", "C3868", "C9145","C9312","C4817","C3359","C8538","C3059","C4627","C5111","C132067", "C3270", "C7541","C3224", "C9231","C4819","C2921","C132146","C4039","C3538","C9061","C6389","C4189","OTH","OTH_S")))] <- 'Other solid malignancy'
    ccc19x$der_VTE_risk[which((ccc19x$cancer_type %in% c("C3167","C3163","C9308","C3247","C3171","C4345","C3106","C3174","C3242","C4665","C3819","C27134","C9300","OTH_H")| 
                                 ccc19x$cancer_type_2 %in% c("C3167","C3163","C9308","C3247","C3171","C4345","C3106","C3174","C3242","C4665","C3819","C27134","C9300","OTH_H")| 
                                 ccc19x$cancer_type_3 %in% c("C3167","C3163","C9308","C3247","C3171","C4345","C3106","C3174","C3242","C4665","C3819","C27134","C9300","OTH_H")| 
                                 ccc19x$cancer_type_4 %in% c("C3167","C3163","C9308","C3247","C3171","C4345","C3106","C3174","C3242","C4665","C3819","C27134","C9300","OTH_H")| 
                                 ccc19x$cancer_type_5 %in% c("C3167","C3163","C9308","C3247","C3171","C4345","C3106","C3174","C3242","C4665","C3819","C27134","C9300","OTH_H")))] <- 'Other heme malignancy'
    ccc19x$der_VTE_risk <- factor(ccc19x$der_VTE_risk)
    ccc19x$der_VTE_risk <- relevel(ccc19x$der_VTE_risk, ref = 'Low-risk VTE malignancy')
    summary(ccc19x$der_VTE_risk[ccc19x$redcap_repeat_instrument == ''])
    
    #X6a-c. Due dates for follow-up forms, assuming right-sided diagnosis (i.e., at least)
    ccc19x$meta_30d_due <- NA
    ccc19x$meta_90d_due <- NA
    ccc19x$meta_180d_due <- NA
    
    temp.ref <- which(ccc19x$redcap_repeat_instrument == '')
    ccc19x$meta_30d_due[temp.ref] <- as.character(as.POSIXct(ccc19x$meta_lefttime3[temp.ref]) + 30*24*60*60)
    ccc19x$meta_30d_due[temp.ref] <- gsub(ccc19x$meta_30d_due[temp.ref], pattern = ' [0-9]{2}:[0-9]{2}:[0-9]{2}', replacement = '')
    ccc19x$meta_90d_due[temp.ref] <- as.character(as.POSIXct(ccc19x$meta_lefttime3[temp.ref]) + 90*24*60*60)
    ccc19x$meta_90d_due[temp.ref] <- gsub(ccc19x$meta_90d_due[temp.ref], pattern = ' [0-9]{2}:[0-9]{2}:[0-9]{2}', replacement = '')
    ccc19x$meta_180d_due[temp.ref] <- as.character(as.POSIXct(ccc19x$meta_lefttime3[temp.ref]) + 180*24*60*60)
    ccc19x$meta_180d_due[temp.ref] <- gsub(ccc19x$meta_180d_due[temp.ref], pattern = ' [0-9]{2}:[0-9]{2}:[0-9]{2}', replacement = '')
    
    
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
    
    #NA - patient is deceased prior to the due date
    temp.ref <- which(ccc19x$der_days_to_death_combined %in% 0:9998 & ccc19x$redcap_repeat_instrument == '')
    ccc19x$der_30d_complete[temp.ref][which(ccc19x$der_days_to_death_combined[temp.ref] < 30 & is.na(ccc19x$der_30d_complete[temp.ref]))] <- "N/A - Patient died before form was due"
    ccc19x$der_90d_complete[temp.ref][which(ccc19x$der_days_to_death_combined[temp.ref] < 90 & is.na(ccc19x$der_90d_complete[temp.ref]))] <- "N/A - Patient died before form was due"
    ccc19x$der_180d_complete[temp.ref][which(ccc19x$der_days_to_death_combined[temp.ref] < 180 & is.na(ccc19x$der_180d_complete[temp.ref]))] <- "N/A - Patient died before form was due"
    
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
    summary(ccc19x$der_30d_complete[ccc19x$redcap_repeat_instrument == ''])
    summary(ccc19x$der_90d_complete[ccc19x$redcap_repeat_instrument == ''])
    summary(ccc19x$der_180d_complete[ccc19x$redcap_repeat_instrument == ''])
    
    ##############################################
    #X4. Quality score and X5. Enumerated problems
    ##############################################
    
    #Calculate a quality score for each case
    ccc19x$meta_quality <- 0
    ccc19x$meta_problems <- ''
    
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
    threshold <- 105
    abline(v = threshold)
    
    ccc19x$meta_quality[which(ccc19x$missing > threshold)] <- ccc19x$meta_quality[which(ccc19x$missing > threshold)] + 5
    ccc19x$meta_problems[which(ccc19x$missing > threshold)] <- paste(ccc19x$meta_problems[which(ccc19x$missing > threshold)],
                                                             '; High levels of baseline missingness', sep = '')
    
    #Large number of unknowns
    dict.unk <- dict[grep(dict$Choices..Calculations..OR.Slider.Labels, pattern = 'Unknown'),]
    
    u1 <- dict.unk$name[dict.unk$Field.Type == 'radio']
    temp <- dict.unk$name[dict.unk$Field.Type == 'checkbox']
    u2 <- colnames(ccc19x)[colnames(ccc19x) %in% c(paste(temp,'___unk',sep = ''),
                                                   paste(temp,'___99', sep = ''))]
    
    ccc19x$unknown <- 0
    for(i in which(ccc19x$redcap_repeat_instrument == ''))
    {
      ccc19x$unknown[i] <- length(which(ccc19x[i,u1] %in% c('UNK','99')))
      ccc19x$unknown[i] <- ccc19x$unknown[i] + length(which(ccc19x[i,u2] == 1))
    }
    
    ccc19x$meta_quality[which(ccc19x$unknown >= 20)] <- ccc19x$meta_quality[which(ccc19x$unknown >= 20)] + 5
    ccc19x$meta_problems[which(ccc19x$unknown >= 20)] <- paste(ccc19x$meta_problems[which(ccc19x$unknown >= 20)],
                                                              '; Large number of unknowns', sep = '')
    
    ##################
    #Moderate problems
    ##################
    
    #Cancer status missing
    temp.ref <- which(is.na(ccc19x$der_cancer_status) &
                        ccc19x$redcap_repeat_instrument == '')
    ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 3
    ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                           '; Cancer status missing', sep = '')
    
    #ECOG status missing
    temp.ref <- which(is.na(ccc19x$der_ecogcat) &
                        ccc19x$redcap_repeat_instrument == '')
    ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 3
    ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                           '; ECOG PS missing', sep = '')
    
    #Death status missing/unk
    temp.ref <- which((ccc19x$der_deadbinary == 99|is.na(ccc19x$der_deadbinary)) &
                        ccc19x$redcap_repeat_instrument == '')
    ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 3
    ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                           '; Death status missing or unknown', sep = '')
    
    #Baseline COVID-19 severity missing/unk
    temp.ref <- which((ccc19x$severity_of_covid_19_v2 == 99|is.na(ccc19x$severity_of_covid_19_v2)) &
                        ccc19x$redcap_repeat_instrument == '')
    ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 3
    ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                           '; Baseline COVID-19 severity missing or unknown', sep = '')
    
    #30-day f/u is 60+ days overdue (if applicable and not superseded by 90-day f/u)
    temp.diff <- difftime(Sys.time(), ccc19x$meta_lefttime3, units = 'days')
    temp <- as.numeric(temp.diff)
    
    temp.ref <- which(temp >= 90 & ccc19x$der_days_fu < 30 & ccc19x$der_30d_complete == 'No')
    ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 3
    ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                           '; 30-day f/u is at least 60 days overdue', sep = '')
    
    #90-day f/u is 60+ days overdue (if applicable)
    temp.diff <- difftime(Sys.time(), ccc19x$meta_lefttime3, units = 'days')
    temp <- as.numeric(temp.diff)
    
    temp.ref <- which(temp >= 150 & ccc19x$der_days_fu < 90 & ccc19x$der_90d_complete == 'No')
    ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 0 #No penalty, yet
    ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                           '; 90-day f/u is at least 60 days overdue', sep = '')
    
    ###############
    #Minor problems
    ###############
    
    #ADT (prostate)
    temp.ref <- which((ccc19x$adt == 99|is.na(ccc19x$adt)) &
                        !ccc19x$hx_treatment %in% c(3,88) &
                        (ccc19x$cancer_type == 'C4863'|
                           ccc19x$cancer_type_2 == 'C4863'|
                           ccc19x$cancer_type_3 == 'C4863'|
                           ccc19x$cancer_type_4 == 'C4863'|
                           ccc19x$cancer_type_5 == 'C4863'))
    ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 1
    ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                           '; ADT missing or unknown', sep = '')
    
    #Biomarkers (breast)
    temp.ref <- which(((ccc19x$breast_biomarkers___er == 0 & ccc19x$breast_biomarkers___her2 == 0 &
                          ccc19x$breast_biomarkers___tnbc == 0) | (ccc19x$breast_biomarkers___99 == 1)) &
                        (ccc19x$cancer_type == 'C4872'|
                           ccc19x$cancer_type_2 == 'C4872'|
                           ccc19x$cancer_type_3 == 'C4872'|
                           ccc19x$cancer_type_4 == 'C4872'|
                           ccc19x$cancer_type_5 == 'C4872'))
    ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 1
    ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                           '; Breast cancer biomarkers missing or unknown', sep = '')
    
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
    
    #Mets status unknown, unless patient has stage IV/disseminated cancer with active disease
    temp.ref <- which((ccc19x$mets_yn == 99|is.na(ccc19x$mets_yn)) &
                        ccc19x$cancer_status != '1' &
                        !(ccc19x$cancer_status %in% 2:5 & ccc19x$stage %in% c('4','764-7')) &
                        ccc19x$redcap_repeat_instrument == '')
    ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 1
    ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                           '; Metastatic status missing or unknown', sep = '')
    
    #ECOG status unknown
    temp.ref <- which(ccc19x$der_ecogcat == 'Unknown' &
                        ccc19x$redcap_repeat_instrument == '')
    ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 1
    ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                           '; ECOG PS unknown', sep = '')
    
    #ICU status missing/unk
    temp.ref <- which((ccc19x$der_ICU == 99|is.na(ccc19x$der_ICU)) &
                        ccc19x$redcap_repeat_instrument == '')
    ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 1
    ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                           '; ICU status missing or unknown', sep = '')
    
    #Hospital status missing/unk
    temp.ref <- which((ccc19x$der_hosp == 99|is.na(ccc19x$der_hosp)) &
                        ccc19x$redcap_repeat_instrument == '')
    ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 1
    ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                           '; Hospital status missing or unknown', sep = '')
    
    #Intubation status missing/unk
    temp.ref <- which((ccc19x$der_mv == 99|is.na(ccc19x$der_mv)) &
                        ccc19x$redcap_repeat_instrument == '')
    ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 1
    ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                           '; Intubation status missing or unknown', sep = '')
    
    #O2 status missing/unk
    temp.ref <- which((ccc19x$der_o2_ever == 99|is.na(ccc19x$der_o2_ever)) &
                        ccc19x$redcap_repeat_instrument == '')
    ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 1
    ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                           '; O2 requirement missing or unknown', sep = '')
    
    #Days to death missing or 9999
    temp.ref <- which(ccc19x$der_deadbinary == 1 & 
                        (ccc19x$der_days_to_death_combined == 9999|is.na(ccc19x$der_days_to_death_combined)) &
                        ccc19x$redcap_repeat_instrument == '')
    ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 1
    ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                           '; Days to death missing or unknown', sep = '')
    
    #30-day f/u is 30+ days overdue (if applicable and not superseded by 90-day f/u)
    temp.diff <- difftime(Sys.time(), ccc19x$meta_lefttime3, units = 'days')
    temp <- as.numeric(temp.diff)
    
    temp.ref <- which(temp >= 60 & temp < 90 & ccc19x$der_days_fu < 30 & ccc19x$der_30d_complete == 'No')
    ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 1
    ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                           '; 30-day f/u is at least 30 days overdue', sep = '')
    
    #90-day f/u is 30+ days overdue (if applicable)
    temp.diff <- difftime(Sys.time(), ccc19x$meta_lefttime3, units = 'days')
    temp <- as.numeric(temp.diff)
    
    temp.ref <- which(temp >= 120 & temp < 150 & ccc19x$der_days_fu < 90 & ccc19x$der_90d_complete == 'No')
    ccc19x$meta_quality[temp.ref] <- ccc19x$meta_quality[temp.ref] + 0 #No penalty, yet
    ccc19x$meta_problems[temp.ref] <- paste(ccc19x$meta_problems[temp.ref],
                                           '; 90-day f/u is at least 30 days overdue', sep = '')
    
    #Remove leading semicolon
    ccc19x$meta_problems <- gsub(ccc19x$meta_problems, pattern = '^; ', replacement = '')
    
    #######################
    #X07. Breast biomarkers
    #######################
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
    summary(ccc19x$der_breast_biomarkers[ccc19x$der_Breast == 1])
    
    #############
    #X08. Gleason
    #############
    ccc19x$der_gleason <- ccc19x$gleason
    ccc19x$der_gleason[which(ccc19x$der_gleason %in% c('X7','X8','X9'))] <- 'Unknown'
    ccc19x$der_gleason[ccc19x$der_gleason == ''] <- NA
    ccc19x$der_gleason <- factor(ccc19x$der_gleason)
    summary(ccc19x$der_gleason[ccc19x$der_Prostate == 1])
    
    #X08a. Gleason with grouped categories
    ccc19x$der_gleason_v2 <- as.character(ccc19x$der_gleason)
    ccc19x$der_gleason_v2[which(ccc19x$der_gleason_v2 %in% c('03','04','05'))] <- 'Less than 6'
    ccc19x$der_gleason_v2 <- factor(ccc19x$der_gleason_v2)
    summary(ccc19x$der_gleason_v2[ccc19x$der_Prostate == 1])
    
    ####################
    #X09. Cytokine storm
    ####################
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
    summary(ccc19x$der_cytokine_storm[ccc19x$redcap_repeat_instrument == ''])
    
    #####################################
    #X10. Lower respiratory tract disease
    #####################################
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
    
    ###############
    #X11. PASC (v1)
    ###############
    ccc19x$der_pasc <- NA
    
    #No - patient marked as fully recovered within 90 days
    
    #Baseline
    ccc19x$der_pasc[which((ccc19x$current_status_v2 == 1|ccc19x$current_status_retro == 1) &
                            ccc19x$covid_19_dx_interval %in% 1:5)] <- 0
    
    #Follow-up
    ccc19x$der_pasc[which(ccc19x$covid_19_status_fu == 1 &
                            (ccc19x$fu_weeks %in% c(30,90)|
                              ccc19x$timing_of_report_weeks <= 13))] <- 0
    
    #Yes - patient marked as having recovered with complications within 90 days
    
    #Baseline
    ccc19x$der_pasc[which((ccc19x$current_status_v2 == '1b'|ccc19x$current_status_retro == '1b') &
                            ccc19x$covid_19_dx_interval %in% 1:5)] <- 1
    
    #Follow-up
    ccc19x$der_pasc[which(ccc19x$covid_19_status_fu == '1b' &
                            (ccc19x$fu_weeks %in% c(30,90)|
                               ccc19x$timing_of_report_weeks <= 13))] <- 1
    
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
    summary(ccc19x$der_pasc[ccc19x$redcap_repeat_instrument == ''])
    
    ############################
    #X12. SARS-CoV-2 vaccination
    ############################
    ccc19x$der_vax <- NA
    
    #No
    ccc19x$der_vax[which(ccc19x$sars_vax == 0)] <- 'Unvaccinated'
    
    #Fully vaccinated
    ccc19x$der_vax[which((ccc19x$sars_vax_which == 4 & ccc19x$sars_vax_when %in% 2:4)|
                           (ccc19x$sars_vax_which %in% c('1b','2b') & ccc19x$sars_vax_when %in% 3:4)|
                           (ccc19x$sars_vax_which %in% c('3b') & ccc19x$sars_vax_when %in% 2:4))
                     ] <- 'Fully vaccinated'
    
    ccc19x$der_vax[which(ccc19x$sars_vax_when == 1|
                           (ccc19x$sars_vax_when %in% 2:4 & is.na(ccc19x$der_vax)))] <- 'Partially vaccinated'
    
    # #Partial early (0-4 weeks)
    # ccc19x$der_vax[which(ccc19x$sars_vax_when == 1)] <- 'Partially vaccinated, early (0-4 wks)'
    # 
    # #Partial late (4+ weeks)
    # ccc19x$der_vax[which(ccc19x$sars_vax_when %in% 2:4 & is.na(ccc19x$der_vax))] <- 'Partially vaccinated, late (4+ weeks)'
    # 
    #After COVID-19
    ccc19x$der_vax[which(ccc19x$sars_vax_when == 88)] <- 'After COVID-19'
    
    #Unknown
    ccc19x$der_vax[which(ccc19x$sars_vax == 99|
                           ccc19x$sars_vax_when == 99)] <- 'Unknown'
    
    ccc19x$der_vax <- factor(ccc19x$der_vax)
    summary(ccc19x$der_vax[ccc19x$redcap_repeat_instrument == ''])
    
  }
  print('Other derived variables completed')
  
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
write.csv(out, file = paste(Sys.time(),'.summary of derived variables results.csv', sep = ''), row.names = F)

#Save here
save(ccc19x, file = paste(Sys.time(),'.',suffix,'.RData', sep = ''))