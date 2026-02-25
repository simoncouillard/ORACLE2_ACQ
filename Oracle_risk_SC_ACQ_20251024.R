# Load packages for data manipulation
library(tidyr)       # Data tidying
library(dplyr)       # Data manipulation
library(purrr)       # Functional programming
library(stringr)     # String manipulation
library(magrittr)    # Pipes (%>%)
library(lubridate)   # Date and time manipulation
library(readxl)      # Read Excel files
library(tibble)      # Enhanced data frames

# Load packages for statistical analysis
library(MASS)        # Statistical functions
library(mice)        # Multiple imputations
library(rms)         # Regression modeling
library(MuMIn)       # Model selection
library(boot)        # Bootstrap functions
library(AICcmodavg)  # AIC model comparison
library(caret)       # Machine learning
library(gam)         # Generalized additive models
library(rstatix)     # Statistical tests
library(metafor)
library(lme4)    # For glmer.nb with random effects
library(pROC)    # For C-statistic

# Load packages for survival analysis
library(survival)        # Survival analysis
library(ggsurvfit)       # ggplot2 extensions for survival plots
library(tidycmprsk)      # Competing risks analysis

# Load packages for visualization
library(ggplot2)         # Data visualization
library(ggpubr)          # Publication-ready plots
library(ggvenn)          # Venn diagrams
library(cowplot)         # Plot grid layouts
library(pheatmap)        # Heatmaps
library(cowplot)         # For better plot alignment
library(patchwork)
library(dcurves)
library(foreach)
library(knitr)


#library(ComplexHeatmap)  # Advanced heatmaps
library(circlize)        # Circular plots
library(scales)          # Scaling functions
library(forplo)
library(patchwork)


# Load packages for reporting and tables
library(gtsummary)       # Summary tables
library(tableone)        # Create Table 1
library(table1)          # Advanced Table 1
library(Gmisc, quietly = TRUE)  # Table summaries
library(glue)            # String interpolation
library(knitr)           # Dynamic reports

# Load packages for model selection
#library(glmulti)         # Multi-model inference
library(leaps)           # Subset regression

# Other utilities
#library(rJava)           # Java integration
library(writexl)         # Write to Excel
library(parallel)
library(foreach)
library(doParallel)
#======================================================================================================================================
# Define Calculator_OSI function
FEV1_intercept <- 1.726723
FEV1_beta1 <- -0.009392698
FEV1_beta2 <- 2.439627e-05
FEV1_beta3 <- -3.141919e-08
Tif_intercept <- 2.149472
Tif_beta1 <- -0.01628198
Tif_beta2 <- 5.707598e-05
Tif_beta3 <- -9.203903e-08
Calculator_OSI <- function(FEV1_PCT, Tif_PCT) {
  OSI <- 
    ((FEV1_intercept + 
        FEV1_beta1 * FEV1_PCT + 
        FEV1_beta2 * FEV1_PCT^2 + 
        FEV1_beta3 * FEV1_PCT^3) - 1) +
    ((Tif_intercept + 
        Tif_beta1 * Tif_PCT + 
        Tif_beta2 * Tif_PCT^2 + 
        Tif_beta3 * Tif_PCT^3) - 1) +
    1
  
  OSI <- round(OSI, 3)
  return(OSI)
}
#======================================================================================================================================
#======================================================================================================================================
#======================================================================================================================================
#PRELIMINARY PART
#Import the DATA from the file 

##Selecting the working directory
setwd("~/Med - PhD/ORACLE2 - Rstuff")

## Importing the original data (will be used to create table 1)
col_types <- c("guess", "guess", "text", "text", "numeric", "text", "numeric", "numeric", "numeric", "text", "numeric", "text", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "numeric", "text", "numeric", "text", "numeric", "numeric", "numeric", "text", "numeric", "text", "numeric", "numeric", "text", "text", "text", "text", "numeric", "numeric", "text", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "text", "numeric", "text", "numeric", "numeric", "text", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric")
data_original_imported <- suppressWarnings(read_excel("data_ORACLE_original_20240429.xlsx", col_types = col_types))
data_original <-data_original_imported
## Importing the imputated data without the systematically missing data (d)= Only imputation of of all missing data
col_types <-c("guess", "text" ,"text","text", "text", "text", "numeric", "numeric", "numeric", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "text", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text" )
Data_imputated_all_imported <- suppressWarnings(read_excel("data_ORACLE_imp_20250102_joined.xlsx", col_types = col_types))
data_imputated<-Data_imputated_all_imported 
## Importing the imputated data with the systematically missing data not replaced (NR) = Only imputation of non-systematically missing
col_types <-c("guess","text","text","text","text","text","numeric","text","numeric","text","text","text","text","numeric","numeric","text","text","numeric","text","text","text","text","text","text","text","text","numeric","numeric","numeric","text","numeric","text","text","text","text","numeric","text","text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","text","numeric","text","numeric","numeric","text","numeric","numeric","text","text","text","text","text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","text")
Data_imputated_without_systematically_missing_imported<-suppressWarnings(read_excel("data_ORACLE_imp_sysREMOVED_20250102_joined.xlsx", col_types = col_types))
data_imputated_without_systematically_missing<- Data_imputated_without_systematically_missing_imported

#currentpath<-"~/Med - PhD/ORACLE2 - Rstuff"
#======================================================================================================================================
#TRANSFORMATION of Values

#Putting NA for the missing value
data_original[data_original == "NA"] <- NA
data_imputated[data_imputated== "NA"] <- NA
data_imputated_without_systematically_missing[data_imputated_without_systematically_missing == "NA"] <- NA

#Transform values to put them in the the good units (#please verify if these lines are still necessary)
#Modify the values of the stratos trials to put the reversibility in % for all the studies instead of decimal
data_original<- data_original %>% 
  mutate(FEV1_reversibility_percent_postBD_real = case_when(
    Enrolled_Trial_name=="STRATOS_1" ~ FEV1_PCT_reversibility_postBD*100,
    Enrolled_Trial_name=="STRATOS_2" ~ FEV1_PCT_reversibility_postBD*100,
    TRUE ~ FEV1_PCT_reversibility_postBD))
#Modify the values of the Captain study to put adherence in trial in percentage everywhere
data_original<- data_original %>% 
  mutate(Adherence_InTrial_quantity_real = case_when(
    Enrolled_Trial_name=="CAPTAIN" ~ Adherence_InTrial_quantity*100,
    TRUE ~ Adherence_InTrial_quantity))
#======================================================================================================================================

#Creation of a unique dataframe which contain original value, imputated and imputated_only_not_systematically_missing
data_original_main<-data_original[,c("Sequential_number","Enrolled_Trial_name","Treatment_arm","Age","Gender_0Female_1Male","BMI","Ethnicity","Country","Region","Treatment_step","Any_severe_attack_previous_12m_0no_1yes","Any_attack_or_hospitalization_previous_12_months","Number_severe_attack_previous_12m","Number_hospitalisations_for_asthma_previous_12_months","Number_hospitalizations_previous_12m_1Yes_0No","Previous_ICU_0no_1yes_9999notknown","Previous_Intubation_0no_1yes_9999notknown","Previous_ICU_or_intubation_0no_1yes","Smoking_0never_1ex_2current","Pack_years","Psychiatric_disease_0no_1yes_9999notknown","Atopy_history_0no_1yes_9999notknown","Eczema_0no_1yes_9999notknown","AllergicRhinitis__0no_1yes_9999notknown","Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown","Chronic_Rhinosinusitis_0no_1yes_9999notknown","Nasal_polyposis_0no_1yes_9999notknown","Previous_nasal_polypectomy_0no_1yes_9999notknown","ICS_DOSE_CLASS","LABA_prescribed_0no_1yes","LAMA_prescribed__0no_1yes","maintenance_OCS_prescribed__0no_1yes","Theophylline_prescribed__0no_1yes","Intranasal_seroid_prescribed__0no_1yes","FEV1_predicted_L","FVC_predicted_L","FEV1_preBD_L_Baseline","FEV1_preBD_PCT_Baseline","FVC_preBD_L_Baseline","FEV1_postBD_L_Baseline","FEV1_postBD_PCT_Baseline","FVC_postBD_L_Baseline","FVC_postBD_PCT_Baseline","FEV1_PCT_reversibility_postBD","FEV1_FVC_ratio","ACQ_baseline_score_mean","ACT_baseline_score","Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced","FeNO_baseline_ppb","Total_IgE","Follow_up_duration_days_nozero","Number_severe_asthma_attacks_during_followup","Time_to_First_attack","Time_to_2n_attack","Time_to_3n_attack","Time_to_4n_attack","Time_to_5n_attack","End_FollowUp_Reason","FEV1PREBD_L_52W","FEV1PREBD_PCT_52W","FEV1POSTBD_L_52W","FEV1POSTBD_PCT_52W","FEV1_reversibility_percent_postBD_real")]
data_imputated_all<-data_imputated[,c("Sequential_number",".imp","Age","Gender_0Female_1Male","BMI","Any_severe_attack_previous_12m_0no_1yes","Number_severe_attack_previous_12m_con","Number_hospitalisations_for_asthma_previous_12_months_con","Previous_ICU_or_intubation_0no_1yes","Smoking_0never_1ex_2current","Pack_years","Atopy_history_0no_1yes_9999notknown","Eczema_0no_1yes_9999notknown","AllergicRhinitis__0no_1yes_9999notknown","Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown","Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown","Chronic_Rhinosinusitis_0no_1yes_9999notknown","Nasal_polyposis_0no_1yes_9999notknown","Previous_nasal_polypectomy_0no_1yes_9999notknown","FEV1_preBD_L_Baseline","FEV1_preBD_PCT_Baseline","FVC_preBD_L_Baseline","FEV1_postBD_L_Baseline","FEV1_postBD_PCT_Baseline","FVC_postBD_L_Baseline","FEV1_PCT_reversibility_postBD","FEV1_FVC_ratio","ACQ_baseline_score_mean","Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced","FeNO_baseline_ppb","Total_IgE")]
colnames(data_imputated_all)[-c(1:2)] <- paste0(colnames(data_imputated_all)[-c(1:2)], "_imputated")
colnames(data_imputated_all) <- make.unique(colnames(data_imputated_all))

data_imputated_no_systematically_missing<-data_imputated_without_systematically_missing[,c("Sequential_number",".imp","Age","Gender_0Female_1Male","BMI","Any_severe_attack_previous_12m_0no_1yes","Number_severe_attack_previous_12m_con","Number_hospitalisations_for_asthma_previous_12_months_con","Previous_ICU_or_intubation_0no_1yes","Smoking_0never_1ex_2current","Pack_years","Atopy_history_0no_1yes_9999notknown","Eczema_0no_1yes_9999notknown","AllergicRhinitis__0no_1yes_9999notknown","Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown","Chronic_Rhinosinusitis_0no_1yes_9999notknown","Nasal_polyposis_0no_1yes_9999notknown","Previous_nasal_polypectomy_0no_1yes_9999notknown","FEV1_preBD_L_Baseline","FEV1_preBD_PCT_Baseline","FVC_preBD_L_Baseline","FEV1_postBD_L_Baseline","FEV1_postBD_PCT_Baseline","FVC_postBD_L_Baseline","FEV1_PCT_reversibility_postBD","FEV1_FVC_ratio","ACQ_baseline_score_mean","Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced","FeNO_baseline_ppb","Total_IgE")]
colnames(data_imputated_no_systematically_missing)[-c(1:2)] <- paste0(colnames(data_imputated_no_systematically_missing)[-c(1:2)], "_imputated_no_systematically_missing")

#combining the datasets
merged_data_imputated <- merge(data_imputated_all, data_imputated_no_systematically_missing, 
                               by = c("Sequential_number", ".imp"), 
                               all = TRUE)

All_data <- merge(data_original, merged_data_imputated, 
                  by = c("Sequential_number"), 
                  all = TRUE)
#======================================================================================================================================
#======================================================================================================================================
#======================================================================================================================================
#======================================================================================================================================
#PART A : CREATION of TABLE 1  

##Selecting the data_original which contain the value without any imputation
data_original_table<-All_data

###Labeling the differents variable with a clear name for table 1 and their respective units
####Age
label(data_original_table$Age) <- "Age"
####Sex
data_original_table$Gender_0Female_1Male <- factor(data_original_table$Gender_0Female_1Male, levels=c(0,1), labels=c("Female", "Male"))
label(data_original_table$Gender_0Female_1Male) <- "Sex, n(%)"
####Ethnicity
data_original_table$Ethnicity <- factor(data_original_table$Ethnicity, levels=c("American_Indian_or_Alaska_Native","Asian","Black_or_African_American","Maori","Multiple","Native_Hawaiian_or_other_Pacific_Islander","Other","White"))
label(data_original_table$Ethnicity) <- "Ethnicity, n(%)"
####Region
data_original_table$Region<-factor(data_original$Region,levels=c("Asia","Europe","North_America","Oceania","South_Africa","South_America"))
data_original_table$Region <- factor(data_original_table$Region, levels=c("Asia","Europe","North_America","Oceania","South_Africa","South_America"), labels=c("Asia","Europe","North_America","Oceania","South_Africa","South_America"))
label(data_original_table$Region) <- "Region, n(%)"
####BMI
label(data_original_table$BMI) <- "Body Mass Index"
units(data_original_table$BMI) <- "kg/m2"
####Treatment step
label(data_original_table$Treatment_step) <- "Treatment step, n(%)"
####Blood eosinophils (BEC)
label(data_original_table$Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced) <- "Blood eosinophils"
units(data_original_table$Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced) <- "x10^9 cells/L"
####FENO
label(data_original_table$FeNO_baseline_ppb) <- "FeNO"
units(data_original_table$FeNO_baseline_ppb) <- "ppb"
####IgE
label(data_original_table$Total_IgE) <- "Total IgE"
units(data_original_table$Total_IgE) <- "ng/mL"
####FEV1
label(data_original_table$FEV1_preBD_PCT_Baseline) <- "FEV1"
units(data_original_table$FEV1_preBD_PCT_Baseline) <- "% of predicted"
####FEV1/FVC
label(data_original_table$FEV1_FVC_ratio) <- "FEV1/FVC"
####Previous ICU or intubation
data_original_table$Previous_ICU_or_intubation_0no_1yes <- factor(data_original_table$Previous_ICU_or_intubation_0no_1yes, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Previous_ICU_or_intubation_0no_1yes) <- "Previous ICU or intubation, n(%)"
####Previous ICU
data_original_table$Previous_ICU_0no_1yes_9999notknown <- factor(data_original_table$Previous_ICU_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Previous_ICU_0no_1yes_9999notknown) <- "Previous ICU, n(%)"
####Previous intubation
data_original_table$Previous_Intubation_0no_1yes_9999notknown <- factor(data_original_table$Previous_Intubation_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Previous_Intubation_0no_1yes_9999notknown) <- "Previous intubation, n(%)"
####Atopy history
data_original_table$Atopy_history_0no_1yes_9999notknown <- factor(data_original_table$Atopy_history_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Atopy_history_0no_1yes_9999notknown) <- "Atopy history, n(%)"
####Allergy testing positive
data_original_table$Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown <- factor(data_original_table$Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown) <- "Allergy testing positive, n(%)"
####Eczema
data_original_table$Eczema_0no_1yes_9999notknown <- factor(data_original_table$Eczema_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Eczema_0no_1yes_9999notknown) <- "Eczema, n(%)"
####Allergic rhinitis
data_original_table$AllergicRhinitis__0no_1yes_9999notknown <- factor(data_original_table$AllergicRhinitis__0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$AllergicRhinitis__0no_1yes_9999notknown) <- "Allergic rhinitis, n(%)"
####Chronic rhinosinusitis
data_original_table$Chronic_Rhinosinusitis_0no_1yes_9999notknown <- factor(data_original_table$Chronic_Rhinosinusitis_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Chronic_Rhinosinusitis_0no_1yes_9999notknown) <- "Chronic rhinosinusitis, n(%)"
####Nasal polyposis
data_original_table$Nasal_polyposis_0no_1yes_9999notknown <- factor(data_original_table$Nasal_polyposis_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Nasal_polyposis_0no_1yes_9999notknown) <- "Nasal polyposis, n(%)"
####Previous nasal polypectomy
data_original_table$Previous_nasal_polypectomy_0no_1yes_9999notknown <- factor(data_original_table$Previous_nasal_polypectomy_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Previous_nasal_polypectomy_0no_1yes_9999notknown) <- "Previous nasal polypectomy, n(%)"
####ACQ-5
label(data_original_table$ACQ_baseline_score_mean) <- "ACQ-5"
####Psychiatric disease
data_original_table$Psychiatric_disease_0no_1yes_9999notknown <- factor(data_original_table$Psychiatric_disease_0no_1yes_9999notknown, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Psychiatric_disease_0no_1yes_9999notknown) <- "Psychiatric disease, n(%)"
####Smoking history
data_original_table$Smoking_0never_1ex_2current <- factor(data_original_table$Smoking_0never_1ex_2current, levels=c(0,1,2), labels=c("Never smoked","Ex-smoker", "Current smoker"))
label(data_original_table$Smoking_0never_1ex_2current) <- "Smoking history, n(%)"
####On ICS
data_original_table$Any_ICS_prescribed_0no_1yes <- factor(data_original$Any_ICS_prescribed_0no_1yes, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Any_ICS_prescribed_0no_1yes) <- "On ICS, n(%)"
####ICS Dose
data_original_table$ICS_DOSE_CLASS <- factor(data_original_table$ICS_DOSE_CLASS, levels=c("0","Low","Medium","High"))
label(data_original_table$ICS_DOSE_CLASS) <- "ICS Dose, n(%)"
####On SABA
data_original_table$SABA_prescribed__0no_1yes <- factor(data_original_table$SABA_prescribed__0no_1yes, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$SABA_prescribed__0no_1yes) <- "On SABA, n(%)"
####On SABA actuation
label(data_original_table$SABA_actuations_per_day_average_PreTrial) <- "SABA actuations per day pre trial"
label(data_original_table$SABA_actuations_per_day_average_InTrial) <- "SABA actuations per day in trial"
####On mOCS
data_original_table$maintenance_OCS_prescribed__0no_1yes <- factor(data_original_table$maintenance_OCS_prescribed__0no_1yes, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$maintenance_OCS_prescribed__0no_1yes) <- "On mOCS, n(%)"
####On intranasal ICS
data_original_table$Intranasal_seroid_prescribed__0no_1yes <- factor(data_original_table$Intranasal_seroid_prescribed__0no_1yes, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Intranasal_seroid_prescribed__0no_1yes) <- "On intranasal ICS, n(%)"
####On LABA
data_original_table$LABA_prescribed_0no_1yes <- factor(data_original_table$LABA_prescribed_0no_1yes, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$LABA_prescribed_0no_1yes) <- "On LABA, n(%)"
####On Montelukast
data_original_table$Montelukast_prescribed__0no_1yes <- factor(data_original_table$Montelukast_prescribed__0no_1yes, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Montelukast_prescribed__0no_1yes) <- "On Montelukast, n(%)"
####On LAMA
data_original_table$LAMA_prescribed__0no_1yes <- factor(data_original_table$LAMA_prescribed__0no_1yes, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$LAMA_prescribed__0no_1yes) <- "On LAMA, n(%)"
####Adherence
label(data_original_table$Adherence_PreTrial_quantity) <- "Adherence pre-trial"
label(data_original_table$Adherence_InTrial_quantity) <- "Adherence in trial"
####Attack history: severe exacerbation or hospitalisation in past 12 months
data_original_table$Any_attack_or_hospitalization_previous_12_months <- factor(data_original_table$Any_attack_or_hospitalization_previous_12_months, levels=c(1,0), labels=c("Yes", "No"))
label(data_original_table$Any_attack_or_hospitalization_previous_12_months) <- "Attack history: severe exacerbation or hospitalisation in past 12 months, n(%)"
####Attack history: Number of severe exacerbations in past 12 months
label(data_original_table$Number_severe_attack_previous_12m) <- "Attack history: Number of severe exacerbations in past 12 months, n(%)"
####Attack history: Number of hospitalisation in past 12 months
label(data_original_table$Number_hospitalisations_for_asthma_previous_12_months) <- "Attack history: Number of hospitalisation in past 12 months"
####Follow-up duration (days)
label(data_original_table$Follow_up_duration_days_nozero) <- "Follow-up duration (days)"

##Identify the data which the only available data is in categories (for age and BMI)
data_original_table<- data_original_table %>% 
  mutate(Age_format = case_when(
    Age_cat=="NA" ~ NA,
    !is.na(Age) ~ "Continuous",
    TRUE ~ "In categories"))
data_original_table$Age_format <- factor(data_original_table$Age_format, levels=c("Continuous","In categories"))

data_original_table<- data_original_table %>% 
  mutate(BMI_format = case_when(
    BMI_cat=="NA" ~ NA,
    !is.na(BMI) ~ "Continuous",
    TRUE ~ "In categories"))
data_original_table$BMI_format <- factor(data_original_table$BMI_format, levels=c("Continuous","In categories"))

##Create the groups for the data analyzed by group (FEV1, ACQ-5, In trial severe exacerbations)
###Group of FEV1
data_original_table$FEV1_PCT_reversibility_postBD_by_group<-cut(data_original_table$FEV1_reversibility_percent_postBD_real,breaks = c(-1000,12,1000),labels=c("<12%","⩾12%"))
label(data_original_table$FEV1_PCT_reversibility_postBD_by_group) <- "FEV1 reversibility (by group)"
###Group of ACQ-5
data_original_table$ACQ_baseline_score_mean_by_group<-cut(data_original_table$ACQ_baseline_score_mean,breaks = c(-1000,1.5,1000),labels=c("<1.5","⩾1.5%"))
label(data_original_table$ACQ_baseline_score_mean_by_group) <- "ACQ-5 (by group)"

###Group of In trial severe exacerbations
data_original_table$Exacerbations_during_follow_up_by_group<-cut(data_original_table$Number_severe_asthma_attacks_during_followup,breaks = c(-1000,0.9,1000),labels=c("0","⩾1"))
label(data_original_table$Exacerbations_during_follow_up_by_group) <- "In trial severe exacerbations, n(%)"

#CREATING of CRSwNP
data_original_table<- data_original_table %>% 
  mutate(CRSwNP=case_when(
    #Identifying as "Yes" for patients with NP
    Nasal_polyposis_0no_1yes_9999notknown=="Yes"~ "Yes",
    #Identifying as "Yes" for patients with polypectomy history
    Previous_nasal_polypectomy_0no_1yes_9999notknown =="Yes"~ "Yes",
    #Identifying as "No" for patients without NP
    Nasal_polyposis_0no_1yes_9999notknown=="No" ~ "No",
    #Identifying as NA for patients that NP was not assessed
    is.na(Nasal_polyposis_0no_1yes_9999notknown) ~ NA,
    TRUE ~ NA
  ))
data_original_table$CRSwNP <- factor(data_original_table$CRSwNP, levels=c("Yes","No"), labels=c("Yes","No"))

##CRSsNP (No CRSwNP + No history of nasal polypectomy + CRS=="Yes")
data_original_table<- data_original_table %>% 
  mutate(CRSsNP=case_when(
    #Identifying as NA for patients that nasal polyposis was not assessed
    is.na(CRSwNP)~ NA,
    #Identifying as NA for patients that Chronic_Rhinosinusitis was not assessed
    is.na(Chronic_Rhinosinusitis_0no_1yes_9999notknown) ~ NA,
    #Identifying as "Yes" for patients with CRSwNP
    CRSwNP=="Yes"~ "No",
    #Identifying as "No" for patients without Chronic rhinosinusitis
    Chronic_Rhinosinusitis_0no_1yes_9999notknown=="No" ~ "No",
    #Identifying as "Yes" for patients with Chronic rhinosinusitis
    Chronic_Rhinosinusitis_0no_1yes_9999notknown=="Yes" ~ "Yes",
    TRUE ~ NA
  ))
data_original_table$CRSsNP <- factor(data_original_table$CRSsNP, levels=c("Yes","No"), labels=c("Yes","No"))


# Create function for continuous and categorical variables
my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x, ), digits = 3), c("", "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD), "Median (IQR)"=sprintf(paste("%s (%s - %s)"), MEDIAN, Q1, Q3),"Geo. mean (GSD)"=sprintf("%s (&plusmn; %s)", GMEAN, GSD),"Geo. mean (IQR)"=sprintf(paste("%s (%s - %s)"), GMEAN, Q1, Q3), "Range"=sprintf("%s - %s", MIN, MAX)))}

my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y, sprintf("%d (%0.0f%%)", FREQ, PCT))))}
colnames(data_original_table)
# Create the tables 1 by trial
Table1_by_trial <- table1(~ Age+ Age_format + Gender_0Female_1Male +BMI+BMI_imputated+BMI_imputated_no_systematically_missing+BMI_format +Smoking_0never_1ex_2current+ Ethnicity +Region +Atopy_history_0no_1yes_9999notknown+Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown+Eczema_0no_1yes_9999notknown+AllergicRhinitis__0no_1yes_9999notknown+Chronic_Rhinosinusitis_0no_1yes_9999notknown+Nasal_polyposis_0no_1yes_9999notknown+Previous_nasal_polypectomy_0no_1yes_9999notknown+CRSsNP+CRSwNP+Psychiatric_disease_0no_1yes_9999notknown+Any_ICS_prescribed_0no_1yes+ICS_DOSE_CLASS+SABA_prescribed__0no_1yes+SABA_actuations_per_day_average_PreTrial+SABA_actuations_per_day_average_InTrial+maintenance_OCS_prescribed__0no_1yes+Intranasal_seroid_prescribed__0no_1yes+LABA_prescribed_0no_1yes+LAMA_prescribed__0no_1yes+Montelukast_prescribed__0no_1yes+Adherence_PreTrial_quantity+Adherence_InTrial_quantity_real+ Treatment_step+ACQ_baseline_score_mean+ACQ_baseline_score_mean_by_group+Any_attack_or_hospitalization_previous_12_months+Number_severe_attack_previous_12m+Number_hospitalisations_for_asthma_previous_12_months+Previous_ICU_or_intubation_0no_1yes+Previous_ICU_0no_1yes_9999notknown+Previous_Intubation_0no_1yes_9999notknown+FEV1_preBD_PCT_Baseline+FEV1_FVC_ratio+FEV1_reversibility_percent_postBD_real+FEV1_PCT_reversibility_postBD_by_group+Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced+FeNO_baseline_ppb+Total_IgE+Total_IgE_imputated+Total_IgE_imputated_no_systematically_missing+Follow_up_duration_days_nozero+Exacerbations_during_follow_up_by_group  # add the variables you want
                          | Enrolled_Trial_name, # the | means  "by"
                          overall=c(left="Total"), # add overall= if you want the total number to be included
                          data=subset(data_original_table,.imp==1),
                          render.continuous=my.render.cont, 
                          render.categorical=my.render.cat)

# Other information for required Table 1:follow up duration and number asthma attack total and by study, 
## Total follow-up and number of severe asthma attack
table_total <- data.frame(c(sum(data_original_table$Follow_up_duration_years_nozero, na.rm=TRUE),sum(data_original_table$Number_severe_asthma_attacks_during_followup, na.rm=TRUE)))
rownames(table_total)<-c("Follow up duration (year)","Nb of asthma attack during follow-up")
colnames(table_total)<-"Total"

##Follow-up duration in year
sum(data_original$Follow_up_duration_years_nozero, na.rm=TRUE)
table_sum_follow_up<- aggregate(data_original_table$Follow_up_duration_years_nozero, by=list(Category=data_original_table$Enrolled_Trial_name), FUN=sum)
table_sum_follow_up<- t(table_sum_follow_up)
colnames(table_sum_follow_up)<-table_sum_follow_up[1,]
table_sum_follow_up<-table_sum_follow_up[2,]

##Number of severe asthma attack
table_sum_asthma_attack<- aggregate(data_original_table$Number_severe_asthma_attacks_during_followup, by=list(Category=data_original_table$Enrolled_Trial_name), FUN=sum)
table_sum_asthma_attack<- t(table_sum_asthma_attack)
colnames(table_sum_asthma_attack)<-table_sum_asthma_attack[1,]
table_sum_asthma_attack<-table_sum_asthma_attack[2,]
##Table with the total information
table_sum_follow_up_and_asthma_attack <- rbind(table_sum_follow_up,table_sum_asthma_attack)
rownames(table_sum_follow_up_and_asthma_attack)<- c("Follow up duration (year)","Nb of asthma attack during follow-up")
table_sum_follow_up_and_asthma_attack <- cbind(table_total,table_sum_follow_up_and_asthma_attack)
table_sum_follow_up_and_asthma_attack
#Export the table for follow-up duration and asthma attack sum
write_xlsx(table_sum_follow_up_and_asthma_attack,"table_sum_follow_up_and_attack.xlsx")
#======================================================================================================================================
#======================================================================================================================================
#======================================================================================================================================
#======================================================================================================================================
#PART C = PREPARING THE IMPUTATED DATASET (identification of variable, calculation of variables and creation of category)
## Identify the categorical data
Data_Oracle<-All_data %>% 
  #Gender
  mutate(Gender_0Female_1Male= case_when(Gender_0Female_1Male ==0 ~ "Female", Gender_0Female_1Male ==1 ~ "Male",TRUE ~ NA)) %>% 
  mutate(Gender_0Female_1Male_imputated= case_when(Gender_0Female_1Male_imputated ==0 ~ "Female", Gender_0Female_1Male_imputated ==1 ~ "Male",TRUE ~ NA)) %>% 
  mutate(Gender_0Female_1Male_imputated_no_systematically_missing= case_when(Gender_0Female_1Male_imputated_no_systematically_missing ==0 ~ "Female", Gender_0Female_1Male_imputated_no_systematically_missing ==1 ~ "Male",TRUE ~ NA)) %>% 
  #Smoking
  mutate(Smoking_0never_1ex_2current= case_when(Smoking_0never_1ex_2current ==0 ~ "Never", Smoking_0never_1ex_2current ==1 ~ "Yes (current or ex)",Smoking_0never_1ex_2current ==2 ~ "Yes (current or ex)",TRUE ~ NA)) %>% 
  mutate(Smoking_0never_1ex_2current_imputated= case_when(Smoking_0never_1ex_2current_imputated ==0 ~ "Never", Smoking_0never_1ex_2current_imputated ==1 ~ "Yes (current or ex)",Smoking_0never_1ex_2current_imputated ==2 ~ "Yes (current or ex)",TRUE ~ NA)) %>% 
  mutate(Smoking_0never_1ex_2current_imputated_no_systematically_missing= case_when(Smoking_0never_1ex_2current_imputated_no_systematically_missing ==0 ~ "Never", Smoking_0never_1ex_2current_imputated_no_systematically_missing ==1 ~ "Yes (current or ex)",Smoking_0never_1ex_2current_imputated_no_systematically_missing ==2 ~ "Yes (current or ex)",TRUE ~ NA)) %>% 
  #Atopy
  mutate(Atopy_history_0no_1yes_9999notknown= case_when(Atopy_history_0no_1yes_9999notknown ==0 ~ "No", Atopy_history_0no_1yes_9999notknown ==1 ~ "Yes",TRUE ~ NA)) %>%
  mutate(Atopy_history_0no_1yes_9999notknown_imputated= case_when(Atopy_history_0no_1yes_9999notknown_imputated ==0 ~ "No", Atopy_history_0no_1yes_9999notknown_imputated ==1 ~ "Yes",TRUE ~ NA)) %>%
  mutate(Atopy_history_0no_1yes_9999notknown_imputated_no_systematically_missing= case_when(Atopy_history_0no_1yes_9999notknown_imputated_no_systematically_missing ==0 ~ "No", Atopy_history_0no_1yes_9999notknown_imputated_no_systematically_missing ==1 ~ "Yes",TRUE ~ NA)) %>%
  #Airborne_allergen_sensitisation
  mutate(Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown= case_when(Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown ==0 ~ "No", Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown ==1 ~ "Yes",TRUE ~ NA)) %>% 
  mutate(Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown_imputated= case_when(Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown_imputated ==0 ~ "No", Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown_imputated ==1 ~ "Yes",TRUE ~ NA)) %>% 
  mutate(Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown_imputated_no_systematically_missing= case_when(Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown_imputated_no_systematically_missing ==0 ~ "No", Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown_imputated_no_systematically_missing ==1 ~ "Yes",TRUE ~ NA)) %>% 
  #Eczema
  mutate(Eczema_0no_1yes_9999notknown= case_when(Eczema_0no_1yes_9999notknown ==0 ~ "No", Eczema_0no_1yes_9999notknown ==1 ~ "Yes",TRUE ~ NA)) %>%
  mutate(Eczema_0no_1yes_9999notknown_imputated= case_when(Eczema_0no_1yes_9999notknown_imputated ==0 ~ "No", Eczema_0no_1yes_9999notknown_imputated ==1 ~ "Yes",TRUE ~ NA)) %>%
  mutate(Eczema_0no_1yes_9999notknown_imputated_no_systematically_missing= case_when(Eczema_0no_1yes_9999notknown_imputated_no_systematically_missing ==0 ~ "No", Eczema_0no_1yes_9999notknown_imputated_no_systematically_missing ==1 ~ "Yes",TRUE ~ NA)) %>%
  #Allergic rhinitis
  mutate(AllergicRhinitis__0no_1yes_9999notknown= case_when(AllergicRhinitis__0no_1yes_9999notknown ==0 ~ "No", AllergicRhinitis__0no_1yes_9999notknown ==1 ~ "Yes",TRUE ~ NA)) %>% 
  mutate(AllergicRhinitis__0no_1yes_9999notknown_imputated= case_when(AllergicRhinitis__0no_1yes_9999notknown_imputated ==0 ~ "No", AllergicRhinitis__0no_1yes_9999notknown_imputated ==1 ~ "Yes",TRUE ~ NA)) %>% 
  mutate(AllergicRhinitis__0no_1yes_9999notknown_imputated_no_systematically_missing= case_when(AllergicRhinitis__0no_1yes_9999notknown_imputated_no_systematically_missing ==0 ~ "No", AllergicRhinitis__0no_1yes_9999notknown_imputated_no_systematically_missing ==1 ~ "Yes",TRUE ~ NA)) %>% 
  #Chronic rhinosinusitis
  mutate(Chronic_Rhinosinusitis_0no_1yes_9999notknown= case_when(Chronic_Rhinosinusitis_0no_1yes_9999notknown ==0 ~ "No", Chronic_Rhinosinusitis_0no_1yes_9999notknown ==1 ~ "Yes",TRUE ~ NA)) %>% 
  mutate(Chronic_Rhinosinusitis_0no_1yes_9999notknown_imputated= case_when(Chronic_Rhinosinusitis_0no_1yes_9999notknown_imputated ==0 ~ "No", Chronic_Rhinosinusitis_0no_1yes_9999notknown_imputated ==1 ~ "Yes",TRUE ~ NA)) %>% 
  mutate(Chronic_Rhinosinusitis_0no_1yes_9999notknown_imputated_no_systematically_missing= case_when(Chronic_Rhinosinusitis_0no_1yes_9999notknown_imputated_no_systematically_missing ==0 ~ "No", Chronic_Rhinosinusitis_0no_1yes_9999notknown_imputated_no_systematically_missing ==1 ~ "Yes",TRUE ~ NA)) %>% 
  #Nasal polyposis
  mutate(Nasal_polyposis_0no_1yes_9999notknown= case_when(Nasal_polyposis_0no_1yes_9999notknown ==0 ~ "No", Nasal_polyposis_0no_1yes_9999notknown ==1 ~ "Yes",TRUE ~ NA)) %>% 
  mutate(Nasal_polyposis_0no_1yes_9999notknown_imputated= case_when(Nasal_polyposis_0no_1yes_9999notknown_imputated ==0 ~ "No", Nasal_polyposis_0no_1yes_9999notknown_imputated ==1 ~ "Yes",TRUE ~ NA)) %>% 
  mutate(Nasal_polyposis_0no_1yes_9999notknown_imputated_no_systematically_missing= case_when(Nasal_polyposis_0no_1yes_9999notknown_imputated_no_systematically_missing ==0 ~ "No", Nasal_polyposis_0no_1yes_9999notknown_imputated_no_systematically_missing ==1 ~ "Yes",TRUE ~ NA)) 


colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Gender_0Female_1Male" )]<- "Gender"
Data_Oracle$Gender<-as.factor(Data_Oracle$Gender)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Gender_0Female_1Male_imputated" )]<- "Gender_imputated"
Data_Oracle$Gender_imputated<-as.factor(Data_Oracle$Gender_imputated)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Gender_0Female_1Male_imputated_no_systematically_missing" )]<- "Gender_imputated_no_systematically_missing"
Data_Oracle$Gender_imputated_no_systematically_missing<-as.factor(Data_Oracle$Gender_imputated_no_systematically_missing)

colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Smoking_0never_1ex_2current" )]<- "Smoking_Statut"
Data_Oracle$Smoking_Statut<-as.factor(Data_Oracle$Smoking_Statut)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Smoking_0never_1ex_2current_imputated" )]<- "Smoking_Statut_imputated"
Data_Oracle$Smoking_Statut_imputated<-as.factor(Data_Oracle$Smoking_Statut_imputated)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Smoking_0never_1ex_2current_imputated_no_systematically_missing" )]<- "Smoking_Statut_imputated_no_systematically_missing"
Data_Oracle$Smoking_Statut_imputated_no_systematically_missing<-as.factor(Data_Oracle$Smoking_Statut_imputated_no_systematically_missing)

colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Atopy_history_0no_1yes_9999notknown" )]<- "Atopy_history"
Data_Oracle$Atopy_history<-as.factor(Data_Oracle$Atopy_history)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Atopy_history_0no_1yes_9999notknown_imputated" )]<- "Atopy_history_imputated"
Data_Oracle$Atopy_history_imputated<-as.factor(Data_Oracle$Atopy_history_imputated)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Atopy_history_0no_1yes_9999notknown_imputated_no_systematically_missing" )]<- "Atopy_history_imputated_no_systematically_missing"
Data_Oracle$Atopy_history_imputated_no_systematically_missing<-as.factor(Data_Oracle$Atopy_history_imputated_no_systematically_missing)

colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown" )]<- "Airborne_allergen_sensibilisation"
Data_Oracle$Airborne_allergen_sensibilisation<-as.factor(Data_Oracle$Airborne_allergen_sensibilisation)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown_imputated" )]<- "Airborne_allergen_sensibilisation_imputated"
Data_Oracle$Airborne_allergen_sensibilisation_imputated<-as.factor(Data_Oracle$Airborne_allergen_sensibilisation_imputated)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown_imputated_no_systematically_missing" )]<- "Airborne_allergen_sensibilisation_imputated_no_systematically_missing"
Data_Oracle$Airborne_allergen_sensibilisation_imputated_no_systematically_missing<-as.factor(Data_Oracle$Airborne_allergen_sensibilisation_imputated_no_systematically_missing)

colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Eczema_0no_1yes_9999notknown" )]<- "Eczema"
Data_Oracle$Eczema<-as.factor(Data_Oracle$Eczema)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Eczema_0no_1yes_9999notknown_imputated" )]<- "Eczema_imputated"
Data_Oracle$Eczema_imputated<-as.factor(Data_Oracle$Eczema_imputated)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Eczema_0no_1yes_9999notknown_imputated_no_systematically_missing" )]<- "Eczema_imputated_no_systematically_missing"
Data_Oracle$Eczema_imputated_no_systematically_missing<-as.factor(Data_Oracle$Eczema_imputated_no_systematically_missing)

colnames(Data_Oracle)[which(colnames(Data_Oracle)=="AllergicRhinitis__0no_1yes_9999notknown" )]<- "Allergic_rhinitis"
Data_Oracle$Allergic_rhinitis<-as.factor(Data_Oracle$Allergic_rhinitis)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="AllergicRhinitis__0no_1yes_9999notknown_imputated" )]<- "Allergic_rhinitis_imputated"
Data_Oracle$Allergic_rhinitis_imputated<-as.factor(Data_Oracle$Allergic_rhinitis_imputated)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="AllergicRhinitis__0no_1yes_9999notknown_imputated_no_systematically_missing" )]<- "Allergic_rhinitis_imputated_no_systematically_missing"
Data_Oracle$Allergic_rhinitis_imputated_no_systematically_missing<-as.factor(Data_Oracle$Allergic_rhinitis_imputated_no_systematically_missing)

colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Chronic_Rhinosinusitis_0no_1yes_9999notknown" )]<- "Chronic_rhinosinusitis"
Data_Oracle$Chronic_rhinosinusitis<-as.factor(Data_Oracle$Chronic_rhinosinusitis)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Chronic_Rhinosinusitis_0no_1yes_9999notknown_imputated" )]<- "Chronic_rhinosinusitis_imputated"
Data_Oracle$Chronic_rhinosinusitis_imputated<-as.factor(Data_Oracle$Chronic_rhinosinusitis_imputated)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Chronic_Rhinosinusitis_0no_1yes_9999notknown_imputated_no_systematically_missing" )]<- "Chronic_rhinosinusitis_imputated_no_systematically_missing"
Data_Oracle$Chronic_rhinosinusitis_imputated_no_systematically_missing<-as.factor(Data_Oracle$Chronic_rhinosinusitis_imputated_no_systematically_missing)

colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Nasal_polyposis_0no_1yes_9999notknown" )]<- "Nasal_polyposis"
Data_Oracle$Nasal_polyposis<-as.factor(Data_Oracle$Nasal_polyposis)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Nasal_polyposis_0no_1yes_9999notknown_imputated" )]<- "Nasal_polyposis_imputated"
Data_Oracle$Nasal_polyposis_imputated<-as.factor(Data_Oracle$Nasal_polyposis_imputated)
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Nasal_polyposis_0no_1yes_9999notknown_imputated_no_systematically_missing" )]<- "Nasal_polyposis_imputated_no_systematically_missing"
Data_Oracle$Nasal_polyposis_imputated_no_systematically_missing<-as.factor(Data_Oracle$Nasal_polyposis_imputated_no_systematically_missing)

## Modify the name of columns for clear names
Data_Oracle<-Data_Oracle %>% 
  mutate(Eosinophils_Log=log10(Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced))
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced_imputated" )]<- "Eosinophils_Log_imputated"
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced_imputated_no_systematically_missing" )]<- "Eosinophils_Log_imputated_no_systematically_missing"

Data_Oracle<-Data_Oracle %>% 
  mutate(FeNO_Log=log10(FeNO_baseline_ppb))
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="FeNO_baseline_ppb_imputated" )]<- "FeNO_Log_imputated"
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="FeNO_baseline_ppb_imputated_no_systematically_missing" )]<- "FeNO_Log_imputated_no_systematically_missing"

Data_Oracle<-Data_Oracle %>% 
  mutate(IgE_Log=log10(Total_IgE))
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Total_IgE_imputated")]<- "IgE_Log_imputated"
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Total_IgE_imputated_no_systematically_missing")]<- "IgE_Log_imputated_no_systematically_missing"

colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Number_severe_attack_previous_12m_con"  )]<- "Attack_12mo_Nb"
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Number_severe_attack_previous_12m_con_imputated"  )]<- "Attack_12mo_Nb_imputated"
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Number_severe_attack_previous_12m_con_imputated_no_systematically_missing"  )]<- "Attack_12mo_Nb_imputated_no_systematically_missing"

colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Number_hospitalisations_for_asthma_previous_12_months_con"  )]<- "Hospitalisations_12mo_Nb"
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Number_hospitalisations_for_asthma_previous_12_months_con_imputated"  )]<- "Hospitalisations_12mo_Nb_imputated"
colnames(Data_Oracle)[which( colnames(Data_Oracle)=="Number_hospitalisations_for_asthma_previous_12_months_con_imputated_no_systematically_missing"  )]<- "Hospitalisations_12mo_Nb_imputated_no_systematically_missing"
#======================================================================================================================================
#Calculation of the continuous variable 
##Calculation of predicted spirometric parameters according to %FEV1 or %FVC when the FEV1_predicted_L was not given
Data_Oracle<-Data_Oracle%>% 
  mutate(FEV1_predicted_L= case_when(
    is.na(FEV1_predicted_L) & !is.na(FEV1_preBD_L_Baseline) & !is.na(FEV1_preBD_PCT_Baseline) ~ (100*FEV1_preBD_L_Baseline)/FEV1_preBD_PCT_Baseline,
    is.na(FEV1_predicted_L) & !is.na(FEV1_postBD_L_Baseline) & !is.na(FEV1_postBD_PCT_Baseline) ~ (100*FEV1_postBD_L_Baseline)/FEV1_postBD_PCT_Baseline,
    TRUE ~ FEV1_predicted_L
  )) %>% 
  mutate(FVC_predicted_L= case_when(
    is.na(FVC_predicted_L) & !is.na(FVC_preBD_L_Baseline) & !is.na(FVC_preBD_PCT_Baseline) ~ (100*FVC_preBD_L_Baseline)/FVC_preBD_PCT_Baseline,
    is.na(FVC_predicted_L) & !is.na(FVC_postBD_L_Baseline) & !is.na(FVC_preBD_PCT_Baseline) ~ (100*FVC_postBD_L_Baseline)/FVC_preBD_PCT_Baseline,
    TRUE ~ FVC_predicted_L
  )) 

## Calculation FEV1/FVC
Data_Oracle<-Data_Oracle%>% 
  mutate(Tiffeneau=FEV1_preBD_L_Baseline/FVC_preBD_L_Baseline)%>% 
  mutate(Tiffeneau_imputated=FEV1_preBD_L_Baseline_imputated/FVC_preBD_L_Baseline_imputated)%>% 
  mutate(Tiffeneau_imputated_no_systematically_missing=FEV1_preBD_L_Baseline_imputated_no_systematically_missing/FVC_preBD_L_Baseline_imputated_no_systematically_missing)

## Calculate the value in absolute for eosinophils, FeNO and IgE
Data_Oracle<-Data_Oracle %>% 
  mutate(Blood_Eos_baseline_x10_9_cells_per_L_imputated=10^Eosinophils_Log_imputated) %>% 
  mutate(Blood_Eos_baseline_x10_9_cells_per_L_imputated_no_systematically_missing=10^Eosinophils_Log_imputated_no_systematically_missing) %>% 
  
  mutate(FeNO_baseline_ppb_imputated=10^FeNO_Log_imputated)%>% 
  mutate(FeNO_baseline_ppb_imputated_no_systematically_missing=10^FeNO_Log_imputated_no_systematically_missing)%>% 
  
  mutate(Total_IgE_imputated=10^IgE_Log_imputated) %>% 
  mutate(Total_IgE_imputated_no_systematically_missing=10^IgE_Log_imputated_no_systematically_missing)

##Put the Follow up duration in days
Data_Oracle<-Data_Oracle %>% 
  mutate(Follow_up_duration_days=Data_Oracle$Follow_up_duration_days_nozero)
#======================================================================================================================================
## Calculation of the parameters by a definite change
Data_Oracle<-Data_Oracle%>% 
  #Age per 10 year increase
  mutate(Age_per_10=Age/10) %>%
  mutate(Age_per_10_imputated=Age_imputated/10) %>%
  mutate(Age_per_10_imputated_no_systematically_missing=Age_imputated_no_systematically_missing/10) %>%
  #BMI per 5 increase
  mutate(BMI_per_5=BMI/5) %>% 
  mutate(BMI_per_5_imputated=BMI_imputated/5) %>% 
  mutate(BMI_per_5_imputated_no_systematically_missing=BMI_imputated_no_systematically_missing/5) %>% 
  #FEV1 per 10% decrease
  mutate(FEV1_preBD_per10_Baseline=-FEV1_preBD_PCT_Baseline/10) %>%
  mutate(FEV1_preBD_per10_Baseline_imputated=-FEV1_preBD_PCT_Baseline_imputated/10) %>%
  mutate(FEV1_preBD_per10_Baseline_imputated_no_systematically_missing=-FEV1_preBD_PCT_Baseline_imputated_no_systematically_missing/10) %>%
  #Reversibility per 10%
  mutate(FEV1_per10_reversibilityBD= FEV1_PCT_reversibility_postBD/10) %>% 
  mutate(FEV1_per10_reversibilityBD_imputated= FEV1_PCT_reversibility_postBD_imputated/10) %>% 
  mutate(FEV1_per10_reversibilityBD_imputated_no_systematically_missing= FEV1_PCT_reversibility_postBD_imputated_no_systematically_missing/10)

### Calculate the value standardize by percentile 25 and 75 
FeNO_delta_75_25<- quantile(Data_Oracle$FeNO_Log_imputated)[4]-quantile(Data_Oracle$FeNO_Log_imputated)[2]   # Display quantiles for FeNO in the first dataset
BEC_delta_75_25<- quantile(Data_Oracle$Eosinophils_Log_imputated)[4]-quantile(Data_Oracle$Eosinophils_Log_imputated)[2]   # Display quantiles for FeNO in the first dataset

### Scaling FeNO based on the 25th and 75th percentiles (method by Prof. Frank Harrell)
Data_Oracle <- Data_Oracle %>%
  mutate(FeNO_p_imputated = FeNO_Log_imputated / FeNO_delta_75_25)  # Scale log FeNO by dividing by 0.47 (between the 25th and 75th percentiles)
Data_Oracle <- Data_Oracle %>%
  mutate(BEC_p_imputated = Eosinophils_Log_imputated / BEC_delta_75_25)  # Scale log FeNO by dividing by 0.47 (between the 25th and 75th percentiles)
#======================================================================================================================================
#Creation of new categoricals data
##Create the CRsNP factor (CRS without NP)

#CREATING of CRSwNP
Data_Oracle <- Data_Oracle  %>% 
  mutate(CRSwNP=case_when(
    #Identifying as "Yes" for patients with NP
    Nasal_polyposis=="Yes"~ "Yes",
    #Identifying as "Yes" for patients with polypectomy history
    Previous_nasal_polypectomy_0no_1yes_9999notknown =="Yes"~ "Yes",
    #Identifying as "No" for patients without NP
    Nasal_polyposis=="No" ~ "No",
    #Identifying as NA for patients that NP was not assessed
    is.na(Nasal_polyposis) ~ NA,
    TRUE ~ NA
  )) %>% 
  mutate(CRSwNP_imputated=case_when(
    #Identifying as "Yes" for patients with NP
    Nasal_polyposis_imputated=="Yes"~ "Yes",
    #Identifying as "Yes" for patients with polypectomy history
    Previous_nasal_polypectomy_0no_1yes_9999notknown_imputated =="Yes"~ "Yes",
    #Identifying as "No" for patients without NP
    Nasal_polyposis_imputated=="No" ~ "No",
    #Identifying as NA for patients that NP was not assessed
    is.na(Nasal_polyposis_imputated) ~ NA,
    TRUE ~ NA
  )) %>% 
  mutate(CRSwNP_imputated_no_systematically_missing=case_when(
    #Identifying as "Yes" for patients with NP
    Nasal_polyposis_imputated_no_systematically_missing=="Yes"~ "Yes",
    #Identifying as "Yes" for patients with polypectomy history
    Previous_nasal_polypectomy_0no_1yes_9999notknown_imputated_no_systematically_missing =="Yes"~ "Yes",
    #Identifying as "No" for patients without NP
    Nasal_polyposis_imputated_no_systematically_missing=="No" ~ "No",
    #Identifying as NA for patients that NP was not assessed
    is.na(Nasal_polyposis_imputated_no_systematically_missing) ~ NA,
    TRUE ~ NA
  ))

Data_Oracle$CRSwNP <- factor(Data_Oracle$CRSwNP, levels=c("No","Yes"), labels=c("No","Yes"))
Data_Oracle$CRSwNP_imputated <- factor(Data_Oracle$CRSwNP_imputated, levels=c("No","Yes"), labels=c("No","Yes"))
Data_Oracle$CRSwNP_imputated_no_systematically_missing <- factor(Data_Oracle$CRSwNP_imputated_no_systematically_missing, levels=c("No","Yes"), labels=c("No","Yes"))

##CRSsNP (No CRSwNP + No history of nasal polypectomy + CRS=="Yes")
Data_Oracle<- Data_Oracle %>% 
  mutate(CRSsNP=case_when(
    #Identifying as NA for patients that nasal polyposis was not assessed
    is.na(CRSwNP)~ NA,
    #Identifying as NA for patients that Chronic_Rhinosinusitis was not assessed
    is.na(Chronic_rhinosinusitis) ~ NA,
    #Identifying as "Yes" for patients with CRSwNP
    CRSwNP=="Yes"~ "No",
    #Identifying as "No" for patients without Chronic rhinosinusitis
    Chronic_rhinosinusitis=="No" ~ "No",
    #Identifying as "Yes" for patients with Chronic rhinosinusitis
    Chronic_rhinosinusitis=="Yes" ~ "Yes",
    TRUE ~ NA
  ))%>% 
  mutate(CRSsNP_imputated=case_when(
    #Identifying as NA for patients that nasal polyposis was not assessed
    is.na(CRSwNP_imputated)~ NA,
    #Identifying as NA for patients that Chronic_Rhinosinusitis was not assessed
    is.na(Chronic_rhinosinusitis_imputated) ~ NA,
    #Identifying as "Yes" for patients with CRSwNP
    CRSwNP_imputated=="Yes"~ "No",
    #Identifying as "No" for patients without Chronic rhinosinusitis
    Chronic_rhinosinusitis_imputated=="No" ~ "No",
    #Identifying as "Yes" for patients with Chronic rhinosinusitis
    Chronic_rhinosinusitis_imputated=="Yes" ~ "Yes",
    TRUE ~ NA
  ))%>% 
  mutate(CRSsNP_imputated_no_systematically_missing=case_when(
    #Identifying as NA for patients that nasal polyposis was not assessed
    is.na(CRSwNP_imputated_no_systematically_missing)~ NA,
    #Identifying as NA for patients that Chronic_Rhinosinusitis was not assessed
    is.na(Chronic_rhinosinusitis_imputated_no_systematically_missing) ~ NA,
    #Identifying as "Yes" for patients with CRSwNP
    CRSwNP_imputated_no_systematically_missing=="Yes"~ "No",
    #Identifying as "No" for patients without Chronic rhinosinusitis
    Chronic_rhinosinusitis_imputated_no_systematically_missing=="No" ~ "No",
    #Identifying as "Yes" for patients with Chronic rhinosinusitis
    Chronic_rhinosinusitis_imputated_no_systematically_missing=="Yes" ~ "Yes",
    TRUE ~ NA
  ))
Data_Oracle$CRSsNP <- factor(Data_Oracle$CRSsNP, levels=c("No","Yes"), labels=c("No","Yes"))
Data_Oracle$CRSsNP_imputated <- factor(Data_Oracle$CRSsNP_imputated, levels=c("No","Yes"), labels=c("No","Yes"))
Data_Oracle$CRSsNP_imputated_no_systematically_missing <- factor(Data_Oracle$CRSsNP_imputated_no_systematically_missing, levels=c("No","Yes"), labels=c("No","Yes"))

#================================================================================================================================================================================================================================================================================================================================================================================================
Table1_test_by_trial <- table1(~ Chronic_rhinosinusitis+Chronic_rhinosinusitis_imputated_no_systematically_missing+Nasal_polyposis+Nasal_polyposis_imputated_no_systematically_missing+CRSsNP+CRSsNP_imputated_no_systematically_missing+CRSwNP+CRSwNP_imputated_no_systematically_missing
                               | Enrolled_Trial_name, # the | means  "by"
                               overall=c(left="Total"), # add overall= if you want the total number to be included
                               data=subset(Data_Oracle,.imp==1),
                               render.continuous=my.render.cont, 
                               render.categorical=my.render.cat)

##Category by inflammatory marker
Data_Oracle$Eosinophils_by_group <- cut(
  Data_Oracle$Blood_Eos_baseline_x10_9_cells_per_L,
  breaks = c(0, 0.15, 0.3, Inf), # Use Inf for the last group to include all larger values
  labels = c('<0.15', '0.15-0.3', '>=0.3'),
  right = FALSE # Make intervals left-closed and right-open
)
Data_Oracle$Eosinophils_by_group_imputated <- cut(
  Data_Oracle$Blood_Eos_baseline_x10_9_cells_per_L_imputated,
  breaks = c(0, 0.15, 0.3, Inf), # Use Inf for the last group to include all larger values
  labels = c('<0.15', '0.15-0.3', '>=0.3'),
  right = FALSE # Make intervals left-closed and right-open
)

Data_Oracle$Eosinophils_by_group_imputated_no_systematically_missing <- cut(
  Data_Oracle$Blood_Eos_baseline_x10_9_cells_per_L_imputated_no_systematically_missing,
  breaks = c(0, 0.15, 0.3, Inf), # Use Inf for the last group to include all larger values
  labels = c('<0.15', '0.15-0.3', '>=0.3'),
  right = FALSE # Make intervals left-closed and right-open
)

Data_Oracle$FeNO_baseline_by_group <- cut(
  Data_Oracle$FeNO_baseline_ppb,
  breaks = c(0, 25, 50, Inf), # Replace 100000 with Inf to ensure >=50 includes 50 and beyond
  labels = c('<25', '25-<50', '>=50'),
  right = TRUE # Ensure intervals are right-closed
)
Data_Oracle$FeNO_baseline_by_group_imputated <- cut(
  Data_Oracle$FeNO_baseline_ppb_imputated,
  breaks = c(0, 25, 50, Inf), # Replace 100000 with Inf to ensure >=50 includes 50 and beyond
  labels = c('<25', '25-<50', '>=50'),
  right = TRUE # Ensure intervals are right-closed
)

Data_Oracle$FeNO_baseline_by_group_imputated_no_systematically_missing <- cut(
  Data_Oracle$FeNO_baseline_ppb_imputated_no_systematically_missing,
  breaks = c(0, 25, 50, Inf), # Replace 100000 with Inf to ensure >=50 includes 50 and beyond
  labels = c('<25', '25-<50', '>=50'),
  right = TRUE # Ensure intervals are right-closed
)


Data_Oracle$IgE_by_group<-cut(Data_Oracle$Total_IgE,breaks = c(0,150, 600,100000),labels=c('<150', '150-600', '>600'))
Data_Oracle$IgE_by_group_imputated<-cut(Data_Oracle$Total_IgE_imputated,breaks = c(0,150, 600,100000),labels=c('<150', '150-600', '>600'))
Data_Oracle$IgE_by_group_imputated_no_systematically_missing<-cut(Data_Oracle$Total_IgE_imputated_no_systematically_missing,breaks = c(0,150, 600,100000),labels=c('<150', '150-600', '>600'))


##Category by lung function
Data_Oracle$FEV1_preBD_Baseline_by_group<-cut(Data_Oracle$FEV1_preBD_PCT_Baseline,breaks = c(0,50,60,70,100000),right = FALSE,labels=c('<50%',"50-<60%",'60-<70%',">=70%"))

Data_Oracle$FEV1_preBD_Baseline_by_group_imputated<-cut(Data_Oracle$FEV1_preBD_PCT_Baseline_imputated,breaks = c(0,50,60,70,100000),labels=c('<50%',"50-<60%",'60-<70%',">=70%"))
Data_Oracle$FEV1_preBD_Baseline_by_group_imputated_no_systematically_missing<-cut(Data_Oracle$FEV1_preBD_PCT_Baseline_imputated_no_systematically_missing,breaks = c(0,50,60,70,100000),labels=c('<50%',"50-<60%",'60-<70%',">=70%"))

Data_Oracle$FEV1_preBD_Baseline_by_group <- factor(Data_Oracle$FEV1_preBD_Baseline_by_group, levels = c(">=70%","60-<70%","50-<60%","<50%"))
Data_Oracle$FEV1_preBD_Baseline_by_group_imputated <- factor(Data_Oracle$FEV1_preBD_Baseline_by_group_imputated, levels = c(">=70%","60-<70%","50-<60%","<50%"))
Data_Oracle$FEV1_preBD_Baseline_by_group_imputated_no_systematically_missing <- factor(Data_Oracle$FEV1_preBD_Baseline_by_group_imputated_no_systematically_missing, levels = c(">=70%","60-<70%","50-<60%","<50%"))

##Category by GINA_treatment_step
Data_Oracle<-Data_Oracle %>% 
  mutate(Treatment_step= case_when(Treatment_step=="1"~"Step 1",Treatment_step=="2"~"Step 2",Treatment_step=="3"~"Step 3",Treatment_step=="4"~"Step 4",Treatment_step=="5"~"Step 5",TRUE~Treatment_step))

Data_Oracle<-Data_Oracle %>% 
  mutate(Treatment_step_1and2= case_when(Treatment_step=="Step 1"~"Step 1-2",Treatment_step=="Step 2"~"Step 1-2",TRUE~Treatment_step))
Data_Oracle$Treatment_step_1and2<-as.factor(Data_Oracle$Treatment_step_1and2)

Data_Oracle$Treatment_step <- factor(Data_Oracle$Treatment_step, levels = c("Step 3","Step 1","Step 2","Step 4","Step 5"))
Data_Oracle$Treatment_step_1and2 <- factor(Data_Oracle$Treatment_step_1and2, levels = c("Step 4","Step 1-2","Step 3","Step 5"))

##Create a treatment step_1-2_vs_3-4-5
Data_Oracle <- Data_Oracle %>%
  mutate(Treatment_step_1_2vs3_5 = factor(case_when(
    Treatment_step %in% c("Step 1", "Step 2") ~ "Step 1-2",
    Treatment_step %in% c("Step 3", "Step 4", "Step 5") ~ "Step 3-5",
    TRUE ~ Treatment_step  # Keep other values as they are
  )))


##Category by ACQ-5
Data_Oracle$ACQ5_by_group<-cut(Data_Oracle$ACQ_baseline_score_mean,breaks = c(-10,1.5,3,100000),labels=c("<1.5","1.5-3",">3"))
Data_Oracle$ACQ5_by_group_imputated<-cut(Data_Oracle$ACQ_baseline_score_mean_imputated,breaks = c(-10,1.5,3,100000),labels=c("<1.5","1.5-3",">3"))
Data_Oracle$ACQ5_by_group_imputated_no_systematically_missing<-cut(Data_Oracle$ACQ_baseline_score_mean_imputated_no_systematically_missing,breaks = c(-10,1.5,3,100000),labels=c("<1.5","1.5-3",">3"))

##Category by BMI
Data_Oracle$BMI_by_group<-cut(Data_Oracle$BMI,breaks = c(-10,25,30,35,100000),labels=c("<25","25-30","30-35",">35"))
Data_Oracle$BMI_by_group_imputated<-cut(Data_Oracle$BMI_imputated,breaks = c(-10,25,30,35,100000),labels=c("<25","25-30","30-35",">35"))
Data_Oracle$BMI_by_group_imputated_no_systematically_missing<-cut(Data_Oracle$BMI_imputated_no_systematically_missing,breaks = c(-10,25,30,35,100000),labels=c("<25","25-30","30-35",">35"))


##Category by Age group
Data_Oracle$Age_by_group_imputated<-cut(Data_Oracle$Age_imputated,breaks = c(-10,40,50,60,100000),labels=c("<40","40-50","50-60",">60"))

#Category of ICS DOSE_CLASS
Data_Oracle$ICS_DOSE_CLASS <- factor(Data_Oracle$ICS_DOSE_CLASS, 
                                     levels = c("0", "Low", "Medium", "High"))
Data_Oracle$ICS_DOSE_CLASS <- relevel(Data_Oracle$ICS_DOSE_CLASS, ref = "High")
Data_Oracle$ICS_DOSE_NUMERIC <- as.numeric(factor(Data_Oracle$ICS_DOSE_CLASS, 
                                                  levels = c("0", "Low", "Medium", "High"))) - 1

Data_Oracle$ICS_DOSE_CLASS_0_or_Low_combine <- factor(ifelse(Data_Oracle$ICS_DOSE_CLASS %in% c("0", "Low"), 
                                                             "0 or Low", 
                                                             as.character(Data_Oracle$ICS_DOSE_CLASS)),
                                                      levels = c("0 or Low", "Medium", "High"))
#=================================================================================================================================================================================================================================================
#=================================================================================================================================================================================================================================================
#=================================================================================================================================================================================================================================================
########## DENSITY PLOTM OF ACQ5 IN ALL TRIALS

# Code to create multipanel density plots of ACQ baseline scores per trial


# Get all unique trial names
all_trials <- unique(Data_Oracle$Enrolled_Trial_name)
# Sort alphabetically for consistent display
all_trials <- sort(all_trials)

# Function to create a single density plot for a trial
create_density_plot <- function(trial_name, data) {
  data_subset <- data %>% 
    filter(Enrolled_Trial_name == trial_name, .imp == 1) # Using only the first imputation for visualization
  
  ggplot(data_subset, aes(x = ACQ_baseline_score_mean_imputated)) +
    geom_density(fill = "lightblue", alpha = 0.7) +
    scale_x_continuous(breaks = seq(0, 6, by = 1), limits = c(0, 6)) +
    labs(title = trial_name,
         x = "ACQ Baseline Score",
         y = "Density") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray", fill = NA, linewidth = 0.5)
    )
}

# Create lists to hold plots
plot_list <- list()

# Create a density plot for each trial
for (trial in all_trials) {
  plot_list[[trial]] <- create_density_plot(trial, Data_Oracle)
}

# Figure 1: Trials 1-6
fig1_trials <- all_trials[1:6]
fig1 <- (plot_list[[fig1_trials[1]]] + plot_list[[fig1_trials[2]]] + plot_list[[fig1_trials[3]]]) /
  (plot_list[[fig1_trials[4]]] + plot_list[[fig1_trials[5]]] + plot_list[[fig1_trials[6]]]) +
  plot_annotation(title = "ACQ Baseline Score Distribution (Trials 1-6)",
                  theme = theme(plot.title = element_text(size = 16, face = "bold")))
ggsave("ACQ_density_fig1.png", fig1, width = 12, height = 8, dpi = 300)

# Figure 2: Trials 7-12
fig2_trials <- all_trials[7:12]
fig2 <- (plot_list[[fig2_trials[1]]] + plot_list[[fig2_trials[2]]] + plot_list[[fig2_trials[3]]]) /
  (plot_list[[fig2_trials[4]]] + plot_list[[fig2_trials[5]]] + plot_list[[fig2_trials[6]]]) +
  plot_annotation(title = "ACQ Baseline Score Distribution (Trials 7-12)",
                  theme = theme(plot.title = element_text(size = 16, face = "bold")))
ggsave("ACQ_density_fig2.png", fig2, width = 12, height = 8, dpi = 300)

# Figure 3: Trials 13-18
fig3_trials <- all_trials[13:18]
fig3 <- (plot_list[[fig3_trials[1]]] + plot_list[[fig3_trials[2]]] + plot_list[[fig3_trials[3]]]) /
  (plot_list[[fig3_trials[4]]] + plot_list[[fig3_trials[5]]] + plot_list[[fig3_trials[6]]]) +
  plot_annotation(title = "ACQ Baseline Score Distribution (Trials 13-18)",
                  theme = theme(plot.title = element_text(size = 16, face = "bold")))
ggsave("ACQ_density_fig3.png", fig3, width = 12, height = 8, dpi = 300)

# Figure 4: Trials 19-22 (or however many are remaining)
remaining_trials <- all_trials[19:length(all_trials)]

# Create a layout based on the remaining number
if (length(remaining_trials) <= 3) {
  fig4 <- wrap_plots(lapply(remaining_trials, function(t) plot_list[[t]]), ncol = 3)
} else if (length(remaining_trials) == 4) {
  fig4 <- (plot_list[[remaining_trials[1]]] + plot_list[[remaining_trials[2]]]) /
    (plot_list[[remaining_trials[3]]] + plot_list[[remaining_trials[4]]])
} else {
  # For 5-6 remaining trials
  fig4 <- (plot_list[[remaining_trials[1]]] + plot_list[[remaining_trials[2]]] + plot_list[[remaining_trials[3]]]) /
    (plot_list[[remaining_trials[4]]] + 
       if(length(remaining_trials) >= 5) plot_list[[remaining_trials[5]]] else plot_spacer() + 
       if(length(remaining_trials) >= 6) plot_list[[remaining_trials[6]]] else plot_spacer())
}

fig4 <- fig4 + plot_annotation(title = paste0("ACQ Baseline Score Distribution (Trials 19-", length(all_trials), ")"),
                               theme = theme(plot.title = element_text(size = 16, face = "bold")))
ggsave("ACQ_density_fig4.png", fig4, width = 12, height = 8, dpi = 300)

# Optional: Create a single combined figure with all plots (very dense)
# all_plots <- wrap_plots(plot_list, ncol = 5)
# ggsave("ACQ_density_all_trials.png", all_plots, width = 20, height = 16, dpi = 300)

# Print the list of all trials for reference
print(all_trials)



#######################################################
# MAKE NEW DATASET OF TRIALS RECRUITING WITHOUT ACQ cutoffs

# Create two new subsets of Data_Oracle based on specific trials


# List of trials to include
exclude_trials <- c("AZISAST", "Novel_START", "PRACTICAL", "PACT", "DREAM")

# Create Data_Oracle_ACQANY by including only the specified trials
Data_Oracle_ACQANY <- Data_Oracle %>%
  filter(Enrolled_Trial_name %in% exclude_trials)

# Print summary information
cat("Original Data_Oracle dataset dimensions: ", nrow(Data_Oracle), " rows x ", ncol(Data_Oracle), " columns\n")
cat("Data_Oracle_ACQANY dataset dimensions: ", nrow(Data_Oracle_ACQANY), " rows x ", ncol(Data_Oracle_ACQANY), " columns\n")


cat("\nTrials in Data_Oracle_ACQANY:\n")
print(sort(unique(Data_Oracle_ACQANY$Enrolled_Trial_name)))


#######################################################
#make new dataset only with trials that give ACQ per item

selected_trials <- c("AZISAST", "BENRAP2B", "CAPTAIN", "DREAM", "LUTE", "MILLY", 
                     "NAVIGATOR", "Novel_START", "PACT", "PATHWAY", "PRACTICAL", 
                     "STRATOS_1", "STRATOS_2", "VERSE")

## Create new dataset with only selected trials
Data_OracleACQitems <- Data_Oracle %>%
  filter(Enrolled_Trial_name %in% selected_trials)


#######################################################
# CREATE TABLE 1 COMPARING THREE DATASETS
# NOW INCLUDING EXTENDED MODEL COVARIATES
#######################################################

# Add a grouping variable to each dataset to identify the source
Data_Oracle_temp <- Data_Oracle %>%
  mutate(dataset_group = "Full ORACLE2")

Data_Oracle_ACQANY_temp <- Data_Oracle_ACQANY %>%
  mutate(dataset_group = "Any ACQ subset")

Data_OracleACQitems_temp <- Data_OracleACQitems %>%
  mutate(dataset_group = "ACQ Items subset")

# Combine all three datasets
Data_Combined <- bind_rows(
  Data_Oracle_temp,
  Data_Oracle_ACQANY_temp,
  Data_OracleACQitems_temp
)

# Make dataset_group a factor with desired order
Data_Combined$dataset_group <- factor(Data_Combined$dataset_group,
                                      levels = c("Full ORACLE2", 
                                                 "Any ACQ subset", 
                                                 "ACQ Items subset"))

# Filter to imputation 1 only
data_combined_imp1 <- subset(Data_Combined, .imp == 1)

# Compute p-values using LOGGED values before transforming
p_values_table <- print(CreateTableOne(vars = c("FeNO_Log_imputated", "Eosinophils_Log_imputated"), 
                                       strata = "dataset_group", 
                                       data = data_combined_imp1, 
                                       test = TRUE),
                        quote = FALSE, printToggle = FALSE, smd = FALSE)

# Extract p-values correctly
p_feno <- p_values_table[rownames(p_values_table) == "FeNO_Log_imputated", "p"]
p_eos <- p_values_table[rownames(p_values_table) == "Eosinophils_Log_imputated", "p"]

# Apply antilog transformation
data_combined_imp1$FeNO_Antilog <- 10^(data_combined_imp1$FeNO_Log_imputated)
data_combined_imp1$Eosinophils_Antilog <- 10^(data_combined_imp1$Eosinophils_Log_imputated)

# Calculate total followup years by group
group_summary <- data_combined_imp1 %>%
  group_by(dataset_group) %>%
  summarise(
    Group_Total_Attacks = sum(Number_severe_asthma_attacks_during_followup, na.rm = TRUE),
    total_followup_years = sum(Follow_up_duration_years, na.rm = TRUE),
    n_patients = n(),
    unadjusted_annual_rate = Group_Total_Attacks / total_followup_years
  )

# Print group summary with unadjusted annual rates
cat("\nUnadjusted Annualized Asthma Attack Rates by Dataset Group:\n")
print(group_summary)

# Add group-level variables to the data frame
data_combined_imp1 <- data_combined_imp1 %>%
  group_by(dataset_group) %>%
  mutate(
    Group_Total_Attacks = sum(Number_severe_asthma_attacks_during_followup, na.rm = TRUE),
    total_followup_years = sum(Follow_up_duration_years, na.rm = TRUE),
    Group_Unadjusted_Annual_Rate = Group_Total_Attacks / total_followup_years
  ) %>%
  ungroup()

# Create a categorical variable for Enrolled_Trial_name for Table 1
data_combined_imp1$Enrolled_Trial_Category <- factor(data_combined_imp1$Enrolled_Trial_name)

# Define variables for the table - NOW INCLUDING EXTENDED MODEL COVARIATES
vars_to_compare <- c("Age_imputated",
                     "Gender_imputated",
                     "BMI_imputated",
                     "Smoking_Statut_imputated",
                     "Treatment_step",
                     "Number_severe_attack_previous_12m",
                     "FEV1_preBD_PCT_Baseline_imputated",
                     "FEV1_reversibility_percent_postBD_real",
                     "FEV1_FVC_ratio_imputated",
                     "ACQ_baseline_score_mean_imputated", 
                     "Eosinophils_Antilog",
                     "FeNO_Antilog",
                     "CRSsNP_imputated",
                     "CRSwNP_imputated",
                     "Allergic_rhinitis_imputated",
                     "Enrolled_Trial_Category",  
                     "Group_Total_Attacks",  
                     "total_followup_years",
                     "Group_Unadjusted_Annual_Rate")

# Specify categorical variables (for proper display)
cat_vars <- c("Gender_imputated", 
              "Smoking_Statut_imputated",
              "Treatment_step",
              "CRSsNP_imputated",
              "CRSwNP_imputated",
              "Allergic_rhinitis_imputated",
              "Enrolled_Trial_Category")

# Specify non-normal variables to display as median [IQR]
nonnormal_vars <- c("Eosinophils_Antilog", "FeNO_Antilog")

# Create table without the overall column
table1 <- CreateTableOne(vars = vars_to_compare, 
                         strata = "dataset_group", 
                         data = data_combined_imp1,
                         factorVars = cat_vars,
                         addOverall = FALSE)

# Convert table to a modifiable format with nonnormal specification
table1_summary <- print(table1, 
                        nonnormal = nonnormal_vars,
                        showAllLevels = TRUE, 
                        quote = FALSE, 
                        pDigits = 3, 
                        printToggle = FALSE)

# Manually insert the p-values for FeNO and Eosinophils
if ("FeNO_Antilog" %in% rownames(table1_summary)) {
  table1_summary["FeNO_Antilog", "p"] <- p_feno
}
if ("Eosinophils_Antilog" %in% rownames(table1_summary)) {
  table1_summary["Eosinophils_Antilog", "p"] <- p_eos
}

# Print markdown version
cat("\n=== TABLE 1: PATIENT CHARACTERISTICS (EXTENDED MODEL COVARIATES) ===\n\n")
kable(table1_summary, format = "markdown", 
      caption = "Table 1. Comparison of patient characteristics across three dataset groups (including extended model covariates)")

# Convert to HTML format
htmlTable(table1_summary)

# Save as an HTML file
writeLines(htmlTable(table1_summary), "table1_three_groups_extended_imputation1.html")

# Export as CSV for Excel
write.csv(table1_summary, "table1_three_groups_extended_imputation1.csv")

# Create a simpler chi-square test for treatment steps 3-5
treatment_data <- data_combined_imp1 %>%
  filter(as.numeric(as.character(Treatment_step)) >= 3)

# Create contingency table and run chi-square test
if(nrow(treatment_data) > 0) {
  treatment_table <- table(treatment_data$dataset_group, treatment_data$Treatment_step)
  treatment_chi_square <- chisq.test(treatment_table)
  cat("\nChi-Square Test for Treatment Steps 3+ Distribution:\n")
  print(treatment_chi_square)
}

# Calculate FeNO_Antilog and Eosinophils_Antilog medians and IQRs by group
data_combined_imp1 %>%
  group_by(dataset_group) %>%
  summarise(
    Eosinophils_Antilog_median = median(Eosinophils_Antilog, na.rm = TRUE),
    Eosinophils_Antilog_IQR = IQR(Eosinophils_Antilog, na.rm = TRUE),
    FeNO_Antilog_median = median(FeNO_Antilog, na.rm = TRUE),
    FeNO_Antilog_IQR = IQR(FeNO_Antilog, na.rm = TRUE)
  ) %>%
  mutate(
    Eosinophils_Antilog = paste0(Eosinophils_Antilog_median, " [", 
                                 round(Eosinophils_Antilog_median - Eosinophils_Antilog_IQR / 2, 2), " - ", 
                                 round(Eosinophils_Antilog_median + Eosinophils_Antilog_IQR / 2, 2), "]"),
    FeNO_Antilog = paste0(FeNO_Antilog_median, " [", 
                          round(FeNO_Antilog_median - FeNO_Antilog_IQR / 2, 2), " - ", 
                          round(FeNO_Antilog_median + FeNO_Antilog_IQR / 2, 2), "]")
  ) %>%
  select(dataset_group, Eosinophils_Antilog, FeNO_Antilog)

# Compute the median and IQR for FeNO_Antilog and Eosinophils_Antilog in Overall group
overall_stats <- data_combined_imp1 %>%
  summarise(
    Eosinophils_Antilog_median = median(Eosinophils_Antilog, na.rm = TRUE),
    Eosinophils_Antilog_IQR = IQR(Eosinophils_Antilog, na.rm = TRUE),
    FeNO_Antilog_median = median(FeNO_Antilog, na.rm = TRUE),
    FeNO_Antilog_IQR = IQR(FeNO_Antilog, na.rm = TRUE)
  )

# Print the results as median [Q1 - Q3]
Eos_result <- paste0(overall_stats$Eosinophils_Antilog_median, " [", 
                     round(overall_stats$Eosinophils_Antilog_median - overall_stats$Eosinophils_Antilog_IQR / 2, 2), " - ", 
                     round(overall_stats$Eosinophils_Antilog_median + overall_stats$Eosinophils_Antilog_IQR / 2, 2), "]")

FeNO_result <- paste0(overall_stats$FeNO_Antilog_median, " [", 
                      round(overall_stats$FeNO_Antilog_median - overall_stats$FeNO_Antilog_IQR / 2, 2), " - ", 
                      round(overall_stats$FeNO_Antilog_median + overall_stats$FeNO_Antilog_IQR / 2, 2), "]")

cat("\nOverall Statistics:\n")
cat("Eosinophils_Antilog: ", Eos_result, "\n")
cat("FeNO_Antilog: ", FeNO_result, "\n")

# Tabulate the counts of Enrolled Trial for each group
enrolled_trial_table <- table(data_combined_imp1$dataset_group, 
                              data_combined_imp1$Enrolled_Trial_name)

# Convert the table to a dataframe for better formatting
enrolled_trial_df <- as.data.frame(enrolled_trial_table)

# Add proportions to the dataframe
enrolled_trial_df$Proportion <- with(enrolled_trial_df, Freq / ave(Freq, Var1, FUN = sum))

# Format the table using kable for basic display
kable(enrolled_trial_df, 
      format = "html", 
      col.names = c("Dataset Group", "Enrolled Trial", "Count", "Proportion"),
      caption = "Counts and proportions of enrolled trials by dataset group.")

# Run a Chi-square test to compare the distributions between the groups
chi_square_result <- chisq.test(enrolled_trial_table)

# Print the Chi-square test results
cat("\nChi-Square Test Result for Trial Distribution:\n")
print(chi_square_result)

cat("\n=== TABLE 1 FOR THREE GROUPS (EXTENDED) COMPLETED ===\n")

#######################################################

#######################################################
# SPLINE CURVE AND DENSITY PLOTS FOR ACQ5 SCORE
# Alternative approach using prediction grids
# WITH EXTENDED ADJUSTMENT STRATEGY
#######################################################

# Filter to imputation 1 for both datasets
data_oracle_imp1 <- subset(Data_Oracle, .imp == 1)
data_acqany_imp1 <- subset(Data_Oracle_ACQANY, .imp == 1)

# DEFINE COMMON REFERENCE VALUES (from Full ORACLE2)

ref_any_severe_attack <- names(sort(table(data_oracle_imp1$Any_severe_attack_previous_12m_0no_1yes_imputated), decreasing = TRUE))[1]
ref_treatment_step <- names(sort(table(data_oracle_imp1$Treatment_step), decreasing = TRUE))[1]
ref_fev1 <- mean(data_oracle_imp1$FEV1_preBD_PCT_Baseline_imputated, na.rm = TRUE)
ref_eos <- mean(data_oracle_imp1$Eosinophils_Log_imputated, na.rm = TRUE)
ref_feno <- mean(data_oracle_imp1$FeNO_Log_imputated, na.rm = TRUE)
ref_bmi <- mean(data_oracle_imp1$BMI_imputated, na.rm = TRUE)
ref_gender <- names(sort(table(data_oracle_imp1$Gender_imputated), decreasing = TRUE))[1]
ref_smoking <- names(sort(table(data_oracle_imp1$Smoking_Statut_imputated), decreasing = TRUE))[1]
ref_fev1fvc <- mean(data_oracle_imp1$FEV1_FVC_ratio_imputated, na.rm = TRUE)
ref_crssnp <- names(sort(table(data_oracle_imp1$CRSsNP_imputated), decreasing = TRUE))[1]
ref_crswnp <- names(sort(table(data_oracle_imp1$CRSwNP_imputated), decreasing = TRUE))[1]
ref_allergic_rhinitis <- names(sort(table(data_oracle_imp1$Allergic_rhinitis_imputated), decreasing = TRUE))[1]

# Print reference values for transparency
cat("\nReference values used for predictions (EXTENDED MODEL):\n")
cat("Any severe attack previous 12m:", ref_any_severe_attack, "\n")
cat("Treatment Step:", ref_treatment_step, "\n")
cat("FEV1 % predicted:", ref_fev1, "\n")
cat("Log Eosinophils:", ref_eos, "\n")
cat("Log FeNO:", ref_feno, "\n")
cat("BMI:", ref_bmi, "\n")
cat("Gender:", ref_gender, "\n")
cat("Smoking status:", ref_smoking, "\n")
cat("FEV1/FVC ratio:", ref_fev1fvc, "\n")
cat("CRSsNP:", ref_crssnp, "\n")
cat("CRSwNP:", ref_crswnp, "\n")
cat("Allergic rhinitis:", ref_allergic_rhinitis, "\n\n")

#######################################################
# MODEL 1: Full ORACLE2 dataset - EXTENDED ADJUSTMENT
#######################################################

model_acq_full <- glm.nb(
  Number_severe_asthma_attacks_during_followup ~ 
    rcs(ACQ_baseline_score_mean_imputated, 3) +
    as.factor(Any_severe_attack_previous_12m_0no_1yes_imputated) +
    as.factor(Treatment_step) + 
    FEV1_preBD_PCT_Baseline_imputated + 
    Eosinophils_Log_imputated*FeNO_Log_imputated + 
    BMI_imputated +
    as.factor(Gender_imputated) +
    as.factor(Smoking_Statut_imputated) +
    FEV1_FVC_ratio_imputated +
    as.factor(CRSsNP_imputated) +
    as.factor(CRSwNP_imputated) +
    as.factor(Allergic_rhinitis_imputated) +
    offset(log(Follow_up_duration_days)) + 
    as.factor(Enrolled_Trial_name),
  data = data_oracle_imp1
)

# Summary of the model
summary(model_acq_full)

# Get reference trial for full dataset
ref_trial_full <- names(sort(table(data_oracle_imp1$Enrolled_Trial_name), decreasing = TRUE))[1]

# Create a prediction grid for ACQ scores using COMMON reference values
pred_grid_full <- expand.grid(
  ACQ_baseline_score_mean_imputated = seq(0, 5, by = 0.1),
  Any_severe_attack_previous_12m_0no_1yes_imputated = factor(ref_any_severe_attack, levels = levels(factor(data_oracle_imp1$Any_severe_attack_previous_12m_0no_1yes_imputated))),
  Treatment_step = factor(ref_treatment_step, levels = levels(factor(data_oracle_imp1$Treatment_step))),
  FEV1_preBD_PCT_Baseline_imputated = ref_fev1,
  Eosinophils_Log_imputated = ref_eos,
  FeNO_Log_imputated = ref_feno,
  BMI_imputated = ref_bmi,
  Gender_imputated = factor(ref_gender, levels = levels(factor(data_oracle_imp1$Gender_imputated))),
  Smoking_Statut_imputated = factor(ref_smoking, levels = levels(factor(data_oracle_imp1$Smoking_Statut_imputated))),
  FEV1_FVC_ratio_imputated = ref_fev1fvc,
  CRSsNP_imputated = factor(ref_crssnp, levels = levels(factor(data_oracle_imp1$CRSsNP_imputated))),
  CRSwNP_imputated = factor(ref_crswnp, levels = levels(factor(data_oracle_imp1$CRSwNP_imputated))),
  Allergic_rhinitis_imputated = factor(ref_allergic_rhinitis, levels = levels(factor(data_oracle_imp1$Allergic_rhinitis_imputated))),
  Follow_up_duration_days = 365.25,  # Annualized rate (1 year)
  Enrolled_Trial_name = ref_trial_full
)

# Generate predictions with confidence intervals
pred_grid_full$predicted_rate <- predict(model_acq_full, newdata = pred_grid_full, type = "response")

# Get standard errors for confidence intervals
pred_link <- predict(model_acq_full, newdata = pred_grid_full, type = "link", se.fit = TRUE)
pred_grid_full$lower_ci <- exp(pred_link$fit - 1.96 * pred_link$se.fit)
pred_grid_full$upper_ci <- exp(pred_link$fit + 1.96 * pred_link$se.fit)

#######################################################
# MODEL 2: Any ACQ subset - EXTENDED ADJUSTMENT
#######################################################

model_acq_subset <- glm.nb(
  Number_severe_asthma_attacks_during_followup ~ 
    rcs(ACQ_baseline_score_mean_imputated, 3) +
    as.factor(Any_severe_attack_previous_12m_0no_1yes_imputated) +
    as.factor(Treatment_step) + 
    FEV1_preBD_PCT_Baseline_imputated + 
    Eosinophils_Log_imputated*FeNO_Log_imputated + 
    BMI_imputated +
    as.factor(Gender_imputated) +
    as.factor(Smoking_Statut_imputated) +
    FEV1_FVC_ratio_imputated +
    as.factor(CRSsNP_imputated) +
    as.factor(CRSwNP_imputated) +
    as.factor(Allergic_rhinitis_imputated) +
    offset(log(Follow_up_duration_days)) + 
    as.factor(Enrolled_Trial_name),
  data = data_acqany_imp1
)

# Summary of the model
summary(model_acq_subset)

# Get reference trial for subset (pick one that exists in the subset)
ref_trial_subset <- names(sort(table(data_acqany_imp1$Enrolled_Trial_name), decreasing = TRUE))[1]

# Create prediction grid for ACQ subset using SAME COMMON reference values
pred_grid_subset <- expand.grid(
  ACQ_baseline_score_mean_imputated = seq(0, 5, by = 0.1),
  Any_severe_attack_previous_12m_0no_1yes_imputated = factor(ref_any_severe_attack, levels = levels(factor(data_acqany_imp1$Any_severe_attack_previous_12m_0no_1yes_imputated))),
  Treatment_step = factor(ref_treatment_step, levels = levels(factor(data_acqany_imp1$Treatment_step))),
  FEV1_preBD_PCT_Baseline_imputated = ref_fev1,
  Eosinophils_Log_imputated = ref_eos,
  FeNO_Log_imputated = ref_feno,
  BMI_imputated = ref_bmi,
  Gender_imputated = factor(ref_gender, levels = levels(factor(data_acqany_imp1$Gender_imputated))),
  Smoking_Statut_imputated = factor(ref_smoking, levels = levels(factor(data_acqany_imp1$Smoking_Statut_imputated))),
  FEV1_FVC_ratio_imputated = ref_fev1fvc,
  CRSsNP_imputated = factor(ref_crssnp, levels = levels(factor(data_acqany_imp1$CRSsNP_imputated))),
  CRSwNP_imputated = factor(ref_crswnp, levels = levels(factor(data_acqany_imp1$CRSwNP_imputated))),
  Allergic_rhinitis_imputated = factor(ref_allergic_rhinitis, levels = levels(factor(data_acqany_imp1$Allergic_rhinitis_imputated))),
  Follow_up_duration_days = 365.25,
  Enrolled_Trial_name = ref_trial_subset
)

# Generate predictions with confidence intervals
pred_grid_subset$predicted_rate <- predict(model_acq_subset, newdata = pred_grid_subset, type = "response")

pred_link_subset <- predict(model_acq_subset, newdata = pred_grid_subset, type = "link", se.fit = TRUE)
pred_grid_subset$lower_ci <- exp(pred_link_subset$fit - 1.96 * pred_link_subset$se.fit)
pred_grid_subset$upper_ci <- exp(pred_link_subset$fit + 1.96 * pred_link_subset$se.fit)

# Set y-axis limits with maximum of 3.5
y_min <- 0
y_max <- 3.5
y_limits <- c(y_min, y_max)

#######################################################
# PLOT 1A: Spline curve for Full ORACLE2
#######################################################

spline_acq_full <- ggplot(pred_grid_full, aes(x = ACQ_baseline_score_mean_imputated, y = predicted_rate)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "lightblue", alpha = 0.3) +
  geom_line(color = "blue", linewidth = 1) +
  xlab("Baseline ACQ-5 Score") +
  ylab("Estimated annualised\nsevere asthma attack rate") +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1), expand = c(0, 0)) +
  scale_y_continuous(limits = y_limits, breaks = seq(0, 3.5, by = 0.5), oob = scales::squish) +
  coord_cartesian(xlim = c(0, 5), expand = FALSE) +
  theme_classic() +
  theme(
    axis.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 12),
    axis.title.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  ) +
  labs(title = "Full ORACLE2\n(n=6513)")

#######################################################
# PLOT 1B: Spline curve for Any ACQ subset
#######################################################

spline_acq_subset <- ggplot(pred_grid_subset, aes(x = ACQ_baseline_score_mean_imputated, y = predicted_rate)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "lightcoral", alpha = 0.3) +
  geom_line(color = "red3", linewidth = 1) +
  xlab("Baseline ACQ-5 Score") +
  ylab("Estimated annualised\nsevere asthma attack rate") +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1), expand = c(0, 0)) +
  scale_y_continuous(limits = y_limits, breaks = seq(0, 3.5, by = 0.5), oob = scales::squish) +
  coord_cartesian(xlim = c(0, 5), expand = FALSE) +
  theme_classic() +
  theme(
    axis.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 12),
    axis.title.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  ) +
  labs(title = "Any ACQ subset\n(n=750)")

#######################################################
# PLOT 2A: Density plot for Full ORACLE2
#######################################################

density_acq_full <- ggplot(data_oracle_imp1, aes(x = ACQ_baseline_score_mean_imputated)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1), expand = c(0, 0)) +
  coord_cartesian(xlim = c(0, 5), expand = FALSE) +
  labs(x = "Baseline ACQ-5 Score", y = "Probability density") +
  theme_classic() +
  theme(
    axis.text.x = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 12),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  )

#######################################################
# PLOT 2B: Density plot for Any ACQ subset
#######################################################

density_acq_subset <- ggplot(data_acqany_imp1, aes(x = ACQ_baseline_score_mean_imputated)) +
  geom_density(fill = "lightcoral", alpha = 0.5) +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1), expand = c(0, 0)) +
  coord_cartesian(xlim = c(0, 5), expand = FALSE) +
  labs(x = "Baseline ACQ-5 Score", y = "Probability density") +
  theme_classic() +
  theme(
    axis.text.x = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 12),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  )

#######################################################
# COMBINE INTO 2x2 GRID WITH PROPER ALIGNMENT
#######################################################

# Create left column (Full ORACLE2)
left_column <- plot_grid(spline_acq_full, density_acq_full, 
                         ncol = 1, 
                         rel_heights = c(2, 0.5),
                         align = "v", 
                         axis = "lr")

# Create right column (Any ACQ subset)
right_column <- plot_grid(spline_acq_subset, density_acq_subset, 
                          ncol = 1, 
                          rel_heights = c(2, 0.5),
                          align = "v", 
                          axis = "lr")

# Combine both columns
combined_plot <- plot_grid(left_column, right_column, 
                           ncol = 2,
                           align = "h",
                           axis = "tb")

# Save outputs
ggsave("ACQ5_spline_density_2x2_grid_extended.png", combined_plot, 
       width = 12, height = 10, units = "in", dpi = 600)

ggsave("ACQ5_spline_density_2x2_grid_extended.pdf", combined_plot, 
       width = 12, height = 10, units = "in", device = "pdf")

cat("\n=== ACQ5 SPLINE AND DENSITY PLOTS COMPLETED (EXTENDED ADJUSTMENT) ===\n")

################################################################################
# ACQ-5 ASSOCIATION WITH ASTHMA EXACERBATIONS
# Negative binomial regression using Rubin's rules for multiple imputation
# Univariable and Multivariable analyses
################################################################################


################################################################################
# 1. DEFINE MODEL FORMULAS
################################################################################

# Univariable model: ACQ + trial adjustment only
formula_univariable <- as.formula(
  Number_severe_asthma_attacks_during_followup ~
    ACQ_baseline_score_mean_imputated +
    offset(log(Follow_up_duration_days)) + 
    as.factor(Enrolled_Trial_name)
)

# Multivariable model: ACQ + full adjustment
formula_multivariable <- as.formula(
  Number_severe_asthma_attacks_during_followup ~
    ACQ_baseline_score_mean_imputated +
    as.factor(Any_severe_attack_previous_12m_0no_1yes_imputated) +
    as.factor(Treatment_step) + 
    FEV1_preBD_PCT_Baseline_imputated + 
    Eosinophils_Log_imputated * FeNO_Log_imputated + 
    BMI_imputated +
    as.factor(Gender_imputated) +
    as.factor(Smoking_Statut_imputated) +
    FEV1_FVC_ratio_imputated +
    as.factor(CRSsNP_imputated) +
    as.factor(CRSwNP_imputated) +
    as.factor(Allergic_rhinitis_imputated) +
    offset(log(Follow_up_duration_days)) + 
    as.factor(Enrolled_Trial_name)
)

################################################################################
# 2. HELPER FUNCTION TO FIT MODELS AND POOL RESULTS
################################################################################

fit_and_pool_acq <- function(data, formula_obj, model_name) {
  
  cat(sprintf("\n=== Fitting %s ===\n", model_name))
  
  # Fit model across all imputations
  model_list <- NULL
  for (i in 1:10) {
    model_list[[i]] <- glm.nb(
      formula = formula_obj,
      data = subset(data, .imp == i)
    )
  }
  
  # Pool results using Rubin's rules
  pooled_results <- summary(pool(model_list), conf.int = TRUE, exp = TRUE)
  
  # Extract ACQ-5 results
  acq_result <- pooled_results[pooled_results$term == "ACQ_baseline_score_mean_imputated", ]
  
  # Calculate RR per 1 unit
  rr_1unit <- acq_result$estimate
  ci_lower_1unit <- acq_result$`2.5 %`
  ci_upper_1unit <- acq_result$`97.5 %`
  p_value <- acq_result$p.value
  
  # Calculate RR per 0.5 unit = (RR per 1 unit)^0.5
  rr_0.5unit <- rr_1unit^0.5
  ci_lower_0.5unit <- ci_lower_1unit^0.5
  ci_upper_0.5unit <- ci_upper_1unit^0.5
  
  # Return results
  return(list(
    rr_1unit = rr_1unit,
    ci_lower_1unit = ci_lower_1unit,
    ci_upper_1unit = ci_upper_1unit,
    rr_0.5unit = rr_0.5unit,
    ci_lower_0.5unit = ci_lower_0.5unit,
    ci_upper_0.5unit = ci_upper_0.5unit,
    p_value = p_value,
    pooled_results = pooled_results
  ))
}

################################################################################
# 3. HELPER FUNCTION TO PRINT RESULTS
################################################################################

print_acq_results <- function(results, cohort_name, model_type) {
  
  cat(sprintf("\n=== %s - %s ===\n", cohort_name, model_type))
  
  cat("\nACQ-5 adjusted Rate Ratio per 1-unit increment [95% CI]:\n")
  cat(sprintf("  aRR = %.3f [%.3f - %.3f], p = %.4f\n", 
              results$rr_1unit, 
              results$ci_lower_1unit, 
              results$ci_upper_1unit,
              results$p_value))
  
  cat("\nACQ-5 adjusted Rate Ratio per 0.5-unit increment [95% CI]:\n")
  cat(sprintf("  aRR = %.3f [%.3f - %.3f]\n", 
              results$rr_0.5unit, 
              results$ci_lower_0.5unit, 
              results$ci_upper_0.5unit))
}

################################################################################
# 4. FULL ORACLE2 COHORT - UNIVARIABLE ANALYSIS
################################################################################

results_full_univar <- fit_and_pool_acq(
  data = Data_Oracle,
  formula_obj = formula_univariable,
  model_name = "Full ORACLE2 - Univariable"
)

print_acq_results(
  results = results_full_univar,
  cohort_name = "FULL ORACLE2 COHORT",
  model_type = "UNIVARIABLE (ACQ + Trial)"
)

################################################################################
# 5. FULL ORACLE2 COHORT - MULTIVARIABLE ANALYSIS
################################################################################

results_full_multivar <- fit_and_pool_acq(
  data = Data_Oracle,
  formula_obj = formula_multivariable,
  model_name = "Full ORACLE2 - Multivariable"
)

print_acq_results(
  results = results_full_multivar,
  cohort_name = "FULL ORACLE2 COHORT",
  model_type = "MULTIVARIABLE (Extended Adjustment)"
)

################################################################################
# 6. ANY ACQ SUBSET COHORT - UNIVARIABLE ANALYSIS
################################################################################

results_subset_univar <- fit_and_pool_acq(
  data = Data_Oracle_ACQANY,
  formula_obj = formula_univariable,
  model_name = "Any ACQ Subset - Univariable"
)

print_acq_results(
  results = results_subset_univar,
  cohort_name = "ANY ACQ SUBSET COHORT",
  model_type = "UNIVARIABLE (ACQ + Trial)"
)

################################################################################
# 7. ANY ACQ SUBSET COHORT - MULTIVARIABLE ANALYSIS
################################################################################

results_subset_multivar <- fit_and_pool_acq(
  data = Data_Oracle_ACQANY,
  formula_obj = formula_multivariable,
  model_name = "Any ACQ Subset - Multivariable"
)

print_acq_results(
  results = results_subset_multivar,
  cohort_name = "ANY ACQ SUBSET COHORT",
  model_type = "MULTIVARIABLE (Extended Adjustment)"
)

################################################################################
# 8. CALCULATE ACQ-5 INCREMENT FOR TARGET RATE RATIOS
################################################################################

calculate_acq_increment <- function(results, target_rr, cohort_name) {
  
  # Formula: increment = log(target_RR) / log(RR_per_1unit)
  increment <- log(target_rr) / log(results$rr_1unit)
  
  # Note: upper CI of RR gives lower bound of increment, and vice versa
  increment_lower <- log(target_rr) / log(results$ci_upper_1unit)
  increment_upper <- log(target_rr) / log(results$ci_lower_1unit)
  
  cat(sprintf("\n%s:\n", cohort_name))
  cat(sprintf("  ACQ-5 increment for aRR = %.1f: %.3f [%.3f - %.3f]\n", 
              target_rr, increment, increment_lower, increment_upper))
  
  return(list(
    increment = increment,
    increment_lower = increment_lower,
    increment_upper = increment_upper
  ))
}

cat("\n")
cat("================================================================================\n")
cat("ACQ-5 INCREMENT NEEDED FOR TARGET RATE RATIOS\n")
cat("================================================================================\n")

# Target aRR = 1.2
cat("\n--- ACQ-5 INCREMENT FOR aRR = 1.2 ---\n")

increment_results <- list()

increment_results$full_univar_1.2 <- calculate_acq_increment(
  results = results_full_univar,
  target_rr = 1.2,
  cohort_name = "Full ORACLE2 (Univariable)"
)

increment_results$full_multivar_1.2 <- calculate_acq_increment(
  results = results_full_multivar,
  target_rr = 1.2,
  cohort_name = "Full ORACLE2 (Multivariable)"
)

increment_results$subset_univar_1.2 <- calculate_acq_increment(
  results = results_subset_univar,
  target_rr = 1.2,
  cohort_name = "Any ACQ Subset (Univariable)"
)

increment_results$subset_multivar_1.2 <- calculate_acq_increment(
  results = results_subset_multivar,
  target_rr = 1.2,
  cohort_name = "Any ACQ Subset (Multivariable)"
)

# Target aRR = 1.4
cat("\n--- ACQ-5 INCREMENT FOR aRR = 1.4 ---\n")

increment_results$full_univar_1.4 <- calculate_acq_increment(
  results = results_full_univar,
  target_rr = 1.4,
  cohort_name = "Full ORACLE2 (Univariable)"
)

increment_results$full_multivar_1.4 <- calculate_acq_increment(
  results = results_full_multivar,
  target_rr = 1.4,
  cohort_name = "Full ORACLE2 (Multivariable)"
)

increment_results$subset_univar_1.4 <- calculate_acq_increment(
  results = results_subset_univar,
  target_rr = 1.4,
  cohort_name = "Any ACQ Subset (Univariable)"
)

increment_results$subset_multivar_1.4 <- calculate_acq_increment(
  results = results_subset_multivar,
  target_rr = 1.4,
  cohort_name = "Any ACQ Subset (Multivariable)"
)

################################################################################
# 8B. INTERQUARTILE RANGE (IQR) ANALYSIS
################################################################################

cat("\n")
cat("================================================================================\n")
cat("ACQ-5 RATE RATIOS PER INTERQUARTILE RANGE (IQR)\n")
cat("================================================================================\n")

# Calculate IQR for Full ORACLE2 cohort (using first imputation for reference)
acq_quantiles_full <- quantile(subset(Data_Oracle, .imp == 1)$ACQ_baseline_score_mean_imputated, 
                               probs = c(0.25, 0.75), na.rm = TRUE)
iqr_full <- acq_quantiles_full[2] - acq_quantiles_full[1]

cat("\nFull ORACLE2 cohort:\n")
cat(sprintf("  25th percentile: %.3f\n", acq_quantiles_full[1]))
cat(sprintf("  75th percentile: %.3f\n", acq_quantiles_full[2]))
cat(sprintf("  IQR range: %.3f\n", iqr_full))

# Calculate IQR for Any ACQ subset (using first imputation for reference)
acq_quantiles_subset <- quantile(subset(Data_Oracle_ACQANY, .imp == 1)$ACQ_baseline_score_mean_imputated, 
                                 probs = c(0.25, 0.75), na.rm = TRUE)
iqr_subset <- acq_quantiles_subset[2] - acq_quantiles_subset[1]

cat("\nAny ACQ Subset cohort:\n")
cat(sprintf("  25th percentile: %.3f\n", acq_quantiles_subset[1]))
cat(sprintf("  75th percentile: %.3f\n", acq_quantiles_subset[2]))
cat(sprintf("  IQR range: %.3f\n", iqr_subset))

# Create IQR-scaled ACQ variable for Full ORACLE2
Data_Oracle$ACQ_IQR_scaled <- Data_Oracle$ACQ_baseline_score_mean_imputated / iqr_full

# Create IQR-scaled ACQ variable for Any ACQ subset
Data_Oracle_ACQANY$ACQ_IQR_scaled <- Data_Oracle_ACQANY$ACQ_baseline_score_mean_imputated / iqr_subset

# Univariable model formula for IQR
formula_univariable_iqr <- as.formula(
  Number_severe_asthma_attacks_during_followup ~
    ACQ_IQR_scaled +
    offset(log(Follow_up_duration_days)) + 
    as.factor(Enrolled_Trial_name)
)

# Multivariable model formula for IQR
formula_multivariable_iqr <- as.formula(
  Number_severe_asthma_attacks_during_followup ~
    ACQ_IQR_scaled +
    as.factor(Any_severe_attack_previous_12m_0no_1yes_imputated) +
    as.factor(Treatment_step) + 
    FEV1_preBD_PCT_Baseline_imputated + 
    Eosinophils_Log_imputated * FeNO_Log_imputated + 
    BMI_imputated +
    as.factor(Gender_imputated) +
    as.factor(Smoking_Statut_imputated) +
    FEV1_FVC_ratio_imputated +
    as.factor(CRSsNP_imputated) +
    as.factor(CRSwNP_imputated) +
    as.factor(Allergic_rhinitis_imputated) +
    offset(log(Follow_up_duration_days)) + 
    as.factor(Enrolled_Trial_name)
)

# Helper function for IQR models
fit_and_pool_acq_iqr <- function(data, formula_obj, model_name) {
  
  cat(sprintf("\n=== Fitting %s (IQR-scaled) ===\n", model_name))
  
  # Fit model across all imputations
  model_list <- NULL
  for (i in 1:10) {
    model_list[[i]] <- glm.nb(
      formula = formula_obj,
      data = subset(data, .imp == i)
    )
  }
  
  # Pool results using Rubin's rules
  pooled_results <- summary(pool(model_list), conf.int = TRUE, exp = TRUE)
  
  # Extract ACQ-5 IQR-scaled results
  acq_result <- pooled_results[pooled_results$term == "ACQ_IQR_scaled", ]
  
  # RR per IQR
  rr_iqr <- acq_result$estimate
  ci_lower_iqr <- acq_result$`2.5 %`
  ci_upper_iqr <- acq_result$`97.5 %`
  p_value <- acq_result$p.value
  
  # Return results
  return(list(
    rr_iqr = rr_iqr,
    ci_lower_iqr = ci_lower_iqr,
    ci_upper_iqr = ci_upper_iqr,
    p_value = p_value,
    pooled_results = pooled_results
  ))
}

# Fit IQR models for Full ORACLE2
results_full_univar_iqr <- fit_and_pool_acq_iqr(
  data = Data_Oracle,
  formula_obj = formula_univariable_iqr,
  model_name = "Full ORACLE2 - Univariable IQR"
)

results_full_multivar_iqr <- fit_and_pool_acq_iqr(
  data = Data_Oracle,
  formula_obj = formula_multivariable_iqr,
  model_name = "Full ORACLE2 - Multivariable IQR"
)

# Fit IQR models for Any ACQ subset
results_subset_univar_iqr <- fit_and_pool_acq_iqr(
  data = Data_Oracle_ACQANY,
  formula_obj = formula_univariable_iqr,
  model_name = "Any ACQ Subset - Univariable IQR"
)

results_subset_multivar_iqr <- fit_and_pool_acq_iqr(
  data = Data_Oracle_ACQANY,
  formula_obj = formula_multivariable_iqr,
  model_name = "Any ACQ Subset - Multivariable IQR"
)

# Print IQR results
cat("\n=== FULL ORACLE2 COHORT - IQR RESULTS ===\n")
cat(sprintf("\nUNIVARIABLE - aRR per IQR (%.3f): %.3f [%.3f - %.3f], p = %.4f\n",
            iqr_full,
            results_full_univar_iqr$rr_iqr,
            results_full_univar_iqr$ci_lower_iqr,
            results_full_univar_iqr$ci_upper_iqr,
            results_full_univar_iqr$p_value))

cat(sprintf("\nMULTIVARIABLE - aRR per IQR (%.3f): %.3f [%.3f - %.3f], p = %.4f\n",
            iqr_full,
            results_full_multivar_iqr$rr_iqr,
            results_full_multivar_iqr$ci_lower_iqr,
            results_full_multivar_iqr$ci_upper_iqr,
            results_full_multivar_iqr$p_value))

cat("\n=== ANY ACQ SUBSET COHORT - IQR RESULTS ===\n")
cat(sprintf("\nUNIVARIABLE - aRR per IQR (%.3f): %.3f [%.3f - %.3f], p = %.4f\n",
            iqr_subset,
            results_subset_univar_iqr$rr_iqr,
            results_subset_univar_iqr$ci_lower_iqr,
            results_subset_univar_iqr$ci_upper_iqr,
            results_subset_univar_iqr$p_value))

cat(sprintf("\nMULTIVARIABLE - aRR per IQR (%.3f): %.3f [%.3f - %.3f], p = %.4f\n",
            iqr_subset,
            results_subset_multivar_iqr$rr_iqr,
            results_subset_multivar_iqr$ci_lower_iqr,
            results_subset_multivar_iqr$ci_upper_iqr,
            results_subset_multivar_iqr$p_value))

################################################################################
# 8B. CREATE TABLE FOR TARGET RR INCREMENTS
################################################################################

cat("\n")
cat("================================================================================\n")
cat("TABLE: ACQ-5 INCREMENT FOR TARGET RATE RATIOS\n")
cat("================================================================================\n")

increment_table <- data.frame(
  Cohort = rep(c("Full ORACLE2", "Any ACQ Subset"), each = 2),
  Model = rep(c("Univariable", "Multivariable"), 2),
  
  `ACQ increment for aRR = 1.2` = c(
    sprintf("%.3f [%.3f-%.3f]",
            increment_results$full_univar_1.2$increment,
            increment_results$full_univar_1.2$increment_lower,
            increment_results$full_univar_1.2$increment_upper),
    sprintf("%.3f [%.3f-%.3f]",
            increment_results$full_multivar_1.2$increment,
            increment_results$full_multivar_1.2$increment_lower,
            increment_results$full_multivar_1.2$increment_upper),
    sprintf("%.3f [%.3f-%.3f]",
            increment_results$subset_univar_1.2$increment,
            increment_results$subset_univar_1.2$increment_lower,
            increment_results$subset_univar_1.2$increment_upper),
    sprintf("%.3f [%.3f-%.3f]",
            increment_results$subset_multivar_1.2$increment,
            increment_results$subset_multivar_1.2$increment_lower,
            increment_results$subset_multivar_1.2$increment_upper)
  ),
  
  `ACQ increment for aRR = 1.4` = c(
    sprintf("%.3f [%.3f-%.3f]",
            increment_results$full_univar_1.4$increment,
            increment_results$full_univar_1.4$increment_lower,
            increment_results$full_univar_1.4$increment_upper),
    sprintf("%.3f [%.3f-%.3f]",
            increment_results$full_multivar_1.4$increment,
            increment_results$full_multivar_1.4$increment_lower,
            increment_results$full_multivar_1.4$increment_upper),
    sprintf("%.3f [%.3f-%.3f]",
            increment_results$subset_univar_1.4$increment,
            increment_results$subset_univar_1.4$increment_lower,
            increment_results$subset_univar_1.4$increment_upper),
    sprintf("%.3f [%.3f-%.3f]",
            increment_results$subset_multivar_1.4$increment,
            increment_results$subset_multivar_1.4$increment_lower,
            increment_results$subset_multivar_1.4$increment_upper)
  ),
  
  check.names = FALSE,
  stringsAsFactors = FALSE
)

print(increment_table, row.names = FALSE)

# Save increment table
write.csv(increment_table, "acq_increment_for_target_RR.csv", row.names = FALSE)
cat("\nIncrement table saved to: acq_increment_for_target_RR.csv\n")

# Create HTML version
html_increment <- "acq_increment_for_target_RR.html"
sink(html_increment)
cat("<!DOCTYPE html>\n<html>\n<head>\n")
cat("<style>\n")
cat("table { border-collapse: collapse; width: 100%; font-family: Arial, sans-serif; margin: 20px 0; }\n")
cat("th, td { border: 1px solid #ddd; padding: 12px; text-align: center; }\n")
cat("th { background-color: #2196F3; color: white; }\n")
cat("td:nth-child(1), td:nth-child(2) { text-align: left; }\n")
cat("tr:nth-child(even) { background-color: #f2f2f2; }\n")
cat(".univar-row { background-color: #e3f2fd; }\n")
cat(".multivar-row { background-color: #fff3e0; }\n")
cat("</style>\n")
cat("</head>\n<body>\n")
cat("<h2>ACQ-5 Increment Needed for Target Rate Ratios</h2>\n")
cat("<table>\n")

# Header
cat("<tr>")
for(col_name in names(increment_table)) {
  cat(sprintf("<th>%s</th>", col_name))
}
cat("</tr>\n")

# Data rows
for(i in 1:nrow(increment_table)) {
  if(increment_table$Model[i] == "Univariable") {
    cat("<tr class='univar-row'>")
  } else {
    cat("<tr class='multivar-row'>")
  }
  
  for(j in 1:ncol(increment_table)) {
    cat(sprintf("<td>%s</td>", increment_table[i,j]))
  }
  cat("</tr>\n")
}

cat("</table>\n")
cat("<h3>Interpretation:</h3>\n")
cat("<p>Values indicate how much ACQ-5 needs to increase to achieve the target adjusted rate ratio (aRR) for exacerbations.</p>\n")
cat("<p>For example, if ACQ increment for aRR = 1.2 is 0.5, this means increasing ACQ-5 by 0.5 points is associated with a 20% increase in exacerbation rate.</p>\n")
cat("<h3>Notes:</h3>\n")
cat("<ul>\n")
cat("<li><strong>Univariable:</strong> ACQ + trial adjustment only</li>\n")
cat("<li><strong>Multivariable:</strong> ACQ + full covariate adjustment</li>\n")
cat("<li>Values shown as point estimate [95% CI]</li>\n")
cat("</ul>\n")
cat("</body>\n</html>\n")
sink()

cat("HTML increment table saved to: acq_increment_for_target_RR.html\n")

################################################################################
# 9. CREATE SUMMARY TABLE
################################################################################

cat("\n")
cat("================================================================================\n")
cat("SUMMARY TABLE: ACQ-5 ASSOCIATION WITH EXACERBATIONS\n")
cat("================================================================================\n")

summary_table <- data.frame(
  Cohort = rep(c("Full ORACLE2", "Any ACQ Subset"), each = 2),
  Model = rep(c("Univariable", "Multivariable"), 2),
  
  `aRR per 1 unit` = c(
    sprintf("%.3f [%.3f-%.3f]", 
            results_full_univar$rr_1unit, 
            results_full_univar$ci_lower_1unit, 
            results_full_univar$ci_upper_1unit),
    sprintf("%.3f [%.3f-%.3f]", 
            results_full_multivar$rr_1unit, 
            results_full_multivar$ci_lower_1unit, 
            results_full_multivar$ci_upper_1unit),
    sprintf("%.3f [%.3f-%.3f]", 
            results_subset_univar$rr_1unit, 
            results_subset_univar$ci_lower_1unit, 
            results_subset_univar$ci_upper_1unit),
    sprintf("%.3f [%.3f-%.3f]", 
            results_subset_multivar$rr_1unit, 
            results_subset_multivar$ci_lower_1unit, 
            results_subset_multivar$ci_upper_1unit)
  ),
  
  `aRR per 0.5 unit` = c(
    sprintf("%.3f [%.3f-%.3f]", 
            results_full_univar$rr_0.5unit, 
            results_full_univar$ci_lower_0.5unit, 
            results_full_univar$ci_upper_0.5unit),
    sprintf("%.3f [%.3f-%.3f]", 
            results_full_multivar$rr_0.5unit, 
            results_full_multivar$ci_lower_0.5unit, 
            results_full_multivar$ci_upper_0.5unit),
    sprintf("%.3f [%.3f-%.3f]", 
            results_subset_univar$rr_0.5unit, 
            results_subset_univar$ci_lower_0.5unit, 
            results_subset_univar$ci_upper_0.5unit),
    sprintf("%.3f [%.3f-%.3f]", 
            results_subset_multivar$rr_0.5unit, 
            results_subset_multivar$ci_lower_0.5unit, 
            results_subset_multivar$ci_upper_0.5unit)
  ),
  
  `aRR per IQR` = c(
    sprintf("%.3f [%.3f-%.3f]", 
            results_full_univar_iqr$rr_iqr, 
            results_full_univar_iqr$ci_lower_iqr, 
            results_full_univar_iqr$ci_upper_iqr),
    sprintf("%.3f [%.3f-%.3f]", 
            results_full_multivar_iqr$rr_iqr, 
            results_full_multivar_iqr$ci_lower_iqr, 
            results_full_multivar_iqr$ci_upper_iqr),
    sprintf("%.3f [%.3f-%.3f]", 
            results_subset_univar_iqr$rr_iqr, 
            results_subset_univar_iqr$ci_lower_iqr, 
            results_subset_univar_iqr$ci_upper_iqr),
    sprintf("%.3f [%.3f-%.3f]", 
            results_subset_multivar_iqr$rr_iqr, 
            results_subset_multivar_iqr$ci_lower_iqr, 
            results_subset_multivar_iqr$ci_upper_iqr)
  ),
  
  `P-value` = c(
    sprintf("%.4f", results_full_univar$p_value),
    sprintf("%.4f", results_full_multivar$p_value),
    sprintf("%.4f", results_subset_univar$p_value),
    sprintf("%.4f", results_subset_multivar$p_value)
  ),
  
  check.names = FALSE,
  stringsAsFactors = FALSE
)

print(summary_table, row.names = FALSE)

# Save summary table
write.csv(summary_table, "acq_exacerbation_association_summary.csv", row.names = FALSE)
cat("\nSummary table saved to: acq_exacerbation_association_summary.csv\n")

################################################################################
# 10. CREATE HTML SUMMARY TABLE
################################################################################

html_file <- "acq_exacerbation_association_summary.html"
sink(html_file)
cat("<!DOCTYPE html>\n<html>\n<head>\n")
cat("<style>\n")
cat("table { border-collapse: collapse; width: 100%; font-family: Arial, sans-serif; margin: 20px 0; }\n")
cat("th, td { border: 1px solid #ddd; padding: 12px; text-align: center; }\n")
cat("th { background-color: #4CAF50; color: white; }\n")
cat("td:nth-child(1), td:nth-child(2) { text-align: left; }\n")
cat("tr:nth-child(even) { background-color: #f2f2f2; }\n")
cat(".univar-row { background-color: #e3f2fd; }\n")
cat(".multivar-row { background-color: #fff3e0; }\n")
cat("</style>\n")
cat("</head>\n<body>\n")
cat("<h2>ACQ-5 Association with Asthma Exacerbations</h2>\n")
cat("<table>\n")

# Header
cat("<tr>")
for(col_name in names(summary_table)) {
  cat(sprintf("<th>%s</th>", col_name))
}
cat("</tr>\n")

# Data rows
for(i in 1:nrow(summary_table)) {
  if(summary_table$Model[i] == "Univariable") {
    cat("<tr class='univar-row'>")
  } else {
    cat("<tr class='multivar-row'>")
  }
  
  for(j in 1:ncol(summary_table)) {
    cat(sprintf("<td>%s</td>", summary_table[i,j]))
  }
  cat("</tr>\n")
}

cat("</table>\n")
cat("<h3>Notes:</h3>\n")
cat("<ul>\n")
cat("<li><strong>Univariable:</strong> ACQ + trial adjustment only</li>\n")
cat("<li><strong>Multivariable:</strong> ACQ + attack history + treatment step + lung function + biomarkers + demographics + comorbidities + trial adjustment</li>\n")
cat("<li><strong>aRR:</strong> Adjusted rate ratio from negative binomial regression</li>\n")
cat("<li><strong>IQR (Interquartile Range):</strong> Rate ratio per increase from 25th to 75th percentile of ACQ-5</li>\n")
cat(sprintf("<li><strong>Full ORACLE2 IQR:</strong> %.3f (25th: %.3f, 75th: %.3f)</li>\n", 
            iqr_full, acq_quantiles_full[1], acq_quantiles_full[2]))
cat(sprintf("<li><strong>Any ACQ Subset IQR:</strong> %.3f (25th: %.3f, 75th: %.3f)</li>\n", 
            iqr_subset, acq_quantiles_subset[1], acq_quantiles_subset[2]))
cat("<li>Results pooled across 10 multiply imputed datasets using Rubin's rules</li>\n")
cat("</ul>\n")
cat("</body>\n</html>\n")
sink()

cat("HTML table saved to: acq_exacerbation_association_summary.html\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("\nFiles saved:\n")
cat("  - acq_exacerbation_association_summary.csv\n")
cat("  - acq_exacerbation_association_summary.html\n")
cat("  - acq_increment_for_target_RR.csv\n")
cat("  - acq_increment_for_target_RR.html\n")


#######################################################
# 2. Ordinal classification: <0.75 (REF), 0.75-<1.5, 1.5-<2.0, 2.0-<2.5, 2.5-<3.0, 3.0-<4.0, >=4.0
# WITH EXTENDED ADJUSTMENT STRATEGY
#######################################################

cat("\n=== ORDINAL CLASSIFICATION ANALYSIS (EXTENDED ADJUSTMENT) ===\n")
cat("ACQ-5 categories: <0.75 (reference), 0.75-<1.5, 1.5-<2.0, 2.0-<2.5, 2.5-<3.0, 3.0-<4.0, >=4.0\n")
cat("Adjusted for: Previous attacks, treatment step, FEV1%, eos, FeNO, BMI, gender, smoking, FEV1/FVC, CRSsNP, CRSwNP, allergic rhinitis\n\n")

# Initialize results data frame for ordinal analysis INCLUDING reference category
ordinal_results <- data.frame(
  Category = c("<0.75 (REF)",
               "0.75 - <1.5 vs <0.75", 
               "1.5 - <2.0 vs <0.75",
               "2.0 - <2.5 vs <0.75",
               "2.5 - <3.0 vs <0.75",
               ">=3.0 vs <0.75"),
  Full_ORACLE2_n = NA,
  Full_ORACLE2_pct = NA,
  Full_ORACLE2_aRR = NA,
  Full_ORACLE2_Lower_CI = NA,
  Full_ORACLE2_Upper_CI = NA,
  Full_ORACLE2_p_value = NA,
  Any_ACQ_Subset_n = NA,
  Any_ACQ_Subset_pct = NA,
  Any_ACQ_Subset_aRR = NA,
  Any_ACQ_Subset_Lower_CI = NA,
  Any_ACQ_Subset_Upper_CI = NA,
  Any_ACQ_Subset_p_value = NA
)

#######################################################
# Full ORACLE2 dataset - Ordinal with Extended Adjustment
#######################################################

# Create ordinal variable for each imputation
Data_Oracle_ordinal <- Data_Oracle %>%
  mutate(ACQ_ordinal = case_when(
    ACQ_baseline_score_mean_imputated < 0.75 ~ "cat1_<0.75",
    ACQ_baseline_score_mean_imputated >= 0.75 & ACQ_baseline_score_mean_imputated < 1.5 ~ "cat2_0.75-<1.5",
    ACQ_baseline_score_mean_imputated >= 1.5 & ACQ_baseline_score_mean_imputated < 2.0 ~ "cat3_1.5-<2.0",
    ACQ_baseline_score_mean_imputated >= 2.0 & ACQ_baseline_score_mean_imputated < 2.5 ~ "cat4_2.0-<2.5",
    ACQ_baseline_score_mean_imputated >= 2.5 & ACQ_baseline_score_mean_imputated < 3.0 ~ "cat5_2.5-<3.0",
    ACQ_baseline_score_mean_imputated >= 3.0 ~ "cat6_>=3.0"
  )) %>%
  mutate(ACQ_ordinal = factor(ACQ_ordinal, levels = c("cat1_<0.75", "cat2_0.75-<1.5", 
                                                      "cat3_1.5-<2.0", "cat4_2.0-<2.5",
                                                      "cat5_2.5-<3.0", "cat6_>=3.0")))

# Calculate sample sizes and percentages for Full ORACLE2 (using imputation 1)
ordinal_counts_full <- table(subset(Data_Oracle_ordinal, .imp == 1)$ACQ_ordinal)
ordinal_pct_full <- prop.table(ordinal_counts_full) * 100

ordinal_results$Full_ORACLE2_n[1] <- as.numeric(ordinal_counts_full["cat1_<0.75"])
ordinal_results$Full_ORACLE2_pct[1] <- as.numeric(ordinal_pct_full["cat1_<0.75"])
ordinal_results$Full_ORACLE2_n[2] <- as.numeric(ordinal_counts_full["cat2_0.75-<1.5"])
ordinal_results$Full_ORACLE2_pct[2] <- as.numeric(ordinal_pct_full["cat2_0.75-<1.5"])
ordinal_results$Full_ORACLE2_n[3] <- as.numeric(ordinal_counts_full["cat3_1.5-<2.0"])
ordinal_results$Full_ORACLE2_pct[3] <- as.numeric(ordinal_pct_full["cat3_1.5-<2.0"])
ordinal_results$Full_ORACLE2_n[4] <- as.numeric(ordinal_counts_full["cat4_2.0-<2.5"])
ordinal_results$Full_ORACLE2_pct[4] <- as.numeric(ordinal_pct_full["cat4_2.0-<2.5"])
ordinal_results$Full_ORACLE2_n[5] <- as.numeric(ordinal_counts_full["cat5_2.5-<3.0"])
ordinal_results$Full_ORACLE2_pct[5] <- as.numeric(ordinal_pct_full["cat5_2.5-<3.0"])
ordinal_results$Full_ORACLE2_n[6] <- as.numeric(ordinal_counts_full["cat6_>=3.0"])
ordinal_results$Full_ORACLE2_pct[6] <- as.numeric(ordinal_pct_full["cat6_>=3.0"])

# Set reference category values (aRR = 1.0)
ordinal_results$Full_ORACLE2_aRR[1] <- 1.0
ordinal_results$Full_ORACLE2_Lower_CI[1] <- NA
ordinal_results$Full_ORACLE2_Upper_CI[1] <- NA
ordinal_results$Full_ORACLE2_p_value[1] <- NA

# Run models across all 10 imputations with EXTENDED ADJUSTMENT
res_comb_ordinal_full <- NULL

for (i in 1:10){
  res_comb_ordinal_full[[i]] <- glm.nb(
    Number_severe_asthma_attacks_during_followup ~
      ACQ_ordinal +
      as.factor(Any_severe_attack_previous_12m_0no_1yes_imputated) +
      as.factor(Treatment_step) + 
      FEV1_preBD_PCT_Baseline_imputated + 
      Eosinophils_Log_imputated*FeNO_Log_imputated + 
      BMI_imputated +
      as.factor(Gender_imputated) +
      as.factor(Smoking_Statut_imputated) +
      FEV1_FVC_ratio_imputated +
      as.factor(CRSsNP_imputated) +
      as.factor(CRSwNP_imputated) +
      as.factor(Allergic_rhinitis_imputated) +
      offset(log(Follow_up_duration_days)) + 
      as.factor(Enrolled_Trial_name), 
    data = subset(Data_Oracle_ordinal, .imp == i)
  )
}

# Pool results
res_pool_ordinal_full <- summary(pool(res_comb_ordinal_full), conf.int = TRUE, exp = TRUE)

# Extract results for ordinal categories
acq_ordinal_cat2_full <- res_pool_ordinal_full[res_pool_ordinal_full$term == "ACQ_ordinalcat2_0.75-<1.5", ]
acq_ordinal_cat3_full <- res_pool_ordinal_full[res_pool_ordinal_full$term == "ACQ_ordinalcat3_1.5-<2.0", ]
acq_ordinal_cat4_full <- res_pool_ordinal_full[res_pool_ordinal_full$term == "ACQ_ordinalcat4_2.0-<2.5", ]
acq_ordinal_cat5_full <- res_pool_ordinal_full[res_pool_ordinal_full$term == "ACQ_ordinalcat5_2.5-<3.0", ]
acq_ordinal_cat6_full <- res_pool_ordinal_full[res_pool_ordinal_full$term == "ACQ_ordinalcat6_>=3.0", ]

# Store results (starting from row 2, as row 1 is reference)
ordinal_results$Full_ORACLE2_aRR[2] <- acq_ordinal_cat2_full$estimate
ordinal_results$Full_ORACLE2_Lower_CI[2] <- acq_ordinal_cat2_full$`2.5 %`
ordinal_results$Full_ORACLE2_Upper_CI[2] <- acq_ordinal_cat2_full$`97.5 %`
ordinal_results$Full_ORACLE2_p_value[2] <- acq_ordinal_cat2_full$p.value

ordinal_results$Full_ORACLE2_aRR[3] <- acq_ordinal_cat3_full$estimate
ordinal_results$Full_ORACLE2_Lower_CI[3] <- acq_ordinal_cat3_full$`2.5 %`
ordinal_results$Full_ORACLE2_Upper_CI[3] <- acq_ordinal_cat3_full$`97.5 %`
ordinal_results$Full_ORACLE2_p_value[3] <- acq_ordinal_cat3_full$p.value

ordinal_results$Full_ORACLE2_aRR[4] <- acq_ordinal_cat4_full$estimate
ordinal_results$Full_ORACLE2_Lower_CI[4] <- acq_ordinal_cat4_full$`2.5 %`
ordinal_results$Full_ORACLE2_Upper_CI[4] <- acq_ordinal_cat4_full$`97.5 %`
ordinal_results$Full_ORACLE2_p_value[4] <- acq_ordinal_cat4_full$p.value

ordinal_results$Full_ORACLE2_aRR[5] <- acq_ordinal_cat5_full$estimate
ordinal_results$Full_ORACLE2_Lower_CI[5] <- acq_ordinal_cat5_full$`2.5 %`
ordinal_results$Full_ORACLE2_Upper_CI[5] <- acq_ordinal_cat5_full$`97.5 %`
ordinal_results$Full_ORACLE2_p_value[5] <- acq_ordinal_cat5_full$p.value

ordinal_results$Full_ORACLE2_aRR[6] <- acq_ordinal_cat6_full$estimate
ordinal_results$Full_ORACLE2_Lower_CI[6] <- acq_ordinal_cat6_full$`2.5 %`
ordinal_results$Full_ORACLE2_Upper_CI[6] <- acq_ordinal_cat6_full$`97.5 %`
ordinal_results$Full_ORACLE2_p_value[6] <- acq_ordinal_cat6_full$p.value


#######################################################
# Any ACQ subset - Ordinal with Extended Adjustment
#######################################################

# Create ordinal variable for each imputation
Data_Oracle_ACQANY_ordinal <- Data_Oracle_ACQANY %>%
  mutate(ACQ_ordinal = case_when(
    ACQ_baseline_score_mean_imputated < 0.75 ~ "cat1_<0.75",
    ACQ_baseline_score_mean_imputated >= 0.75 & ACQ_baseline_score_mean_imputated < 1.5 ~ "cat2_0.75-<1.5",
    ACQ_baseline_score_mean_imputated >= 1.5 & ACQ_baseline_score_mean_imputated < 2.0 ~ "cat3_1.5-<2.0",
    ACQ_baseline_score_mean_imputated >= 2.0 & ACQ_baseline_score_mean_imputated < 2.5 ~ "cat4_2.0-<2.5",
    ACQ_baseline_score_mean_imputated >= 2.5 & ACQ_baseline_score_mean_imputated < 3.0 ~ "cat5_2.5-<3.0",
    ACQ_baseline_score_mean_imputated >= 3.0 ~ "cat6_>=3.0"
  )) %>%
  mutate(ACQ_ordinal = factor(ACQ_ordinal, levels = c("cat1_<0.75", "cat2_0.75-<1.5", 
                                                      "cat3_1.5-<2.0", "cat4_2.0-<2.5",
                                                      "cat5_2.5-<3.0", "cat6_>=3.0")))

# Calculate sample sizes and percentages for Any ACQ subset (using imputation 1)
ordinal_counts_subset <- table(subset(Data_Oracle_ACQANY_ordinal, .imp == 1)$ACQ_ordinal)
ordinal_pct_subset <- prop.table(ordinal_counts_subset) * 100

ordinal_results$Any_ACQ_Subset_n[1] <- as.numeric(ordinal_counts_subset["cat1_<0.75"])
ordinal_results$Any_ACQ_Subset_pct[1] <- as.numeric(ordinal_pct_subset["cat1_<0.75"])
ordinal_results$Any_ACQ_Subset_n[2] <- as.numeric(ordinal_counts_subset["cat2_0.75-<1.5"])
ordinal_results$Any_ACQ_Subset_pct[2] <- as.numeric(ordinal_pct_subset["cat2_0.75-<1.5"])
ordinal_results$Any_ACQ_Subset_n[3] <- as.numeric(ordinal_counts_subset["cat3_1.5-<2.0"])
ordinal_results$Any_ACQ_Subset_pct[3] <- as.numeric(ordinal_pct_subset["cat3_1.5-<2.0"])
ordinal_results$Any_ACQ_Subset_n[4] <- as.numeric(ordinal_counts_subset["cat4_2.0-<2.5"])
ordinal_results$Any_ACQ_Subset_pct[4] <- as.numeric(ordinal_pct_subset["cat4_2.0-<2.5"])
ordinal_results$Any_ACQ_Subset_n[5] <- as.numeric(ordinal_counts_subset["cat5_2.5-<3.0"])
ordinal_results$Any_ACQ_Subset_pct[5] <- as.numeric(ordinal_pct_subset["cat5_2.5-<3.0"])
ordinal_results$Any_ACQ_Subset_n[6] <- as.numeric(ordinal_counts_subset["cat6_>=3.0"])
ordinal_results$Any_ACQ_Subset_pct[6] <- as.numeric(ordinal_pct_subset["cat6_>=3.0"])


# Set reference category values (aRR = 1.0)
ordinal_results$Any_ACQ_Subset_aRR[1] <- 1.0
ordinal_results$Any_ACQ_Subset_Lower_CI[1] <- NA
ordinal_results$Any_ACQ_Subset_Upper_CI[1] <- NA
ordinal_results$Any_ACQ_Subset_p_value[1] <- NA

# Run models across all 10 imputations with EXTENDED ADJUSTMENT
res_comb_ordinal_subset <- NULL

for (i in 1:10){
  res_comb_ordinal_subset[[i]] <- glm.nb(
    Number_severe_asthma_attacks_during_followup ~
      ACQ_ordinal +
      as.factor(Any_severe_attack_previous_12m_0no_1yes_imputated) +
      as.factor(Treatment_step) + 
      FEV1_preBD_PCT_Baseline_imputated + 
      Eosinophils_Log_imputated*FeNO_Log_imputated + 
      BMI_imputated +
      as.factor(Gender_imputated) +
      as.factor(Smoking_Statut_imputated) +
      FEV1_FVC_ratio_imputated +
      as.factor(CRSsNP_imputated) +
      as.factor(CRSwNP_imputated) +
      as.factor(Allergic_rhinitis_imputated) +
      offset(log(Follow_up_duration_days)) + 
      as.factor(Enrolled_Trial_name), 
    data = subset(Data_Oracle_ACQANY_ordinal, .imp == i)
  )
}

# Pool results
res_pool_ordinal_subset <- summary(pool(res_comb_ordinal_subset), conf.int = TRUE, exp = TRUE)

# Extract results for ordinal categories
acq_ordinal_cat2_subset <- res_pool_ordinal_subset[res_pool_ordinal_subset$term == "ACQ_ordinalcat2_0.75-<1.5", ]
acq_ordinal_cat3_subset <- res_pool_ordinal_subset[res_pool_ordinal_subset$term == "ACQ_ordinalcat3_1.5-<2.0", ]
acq_ordinal_cat4_subset <- res_pool_ordinal_subset[res_pool_ordinal_subset$term == "ACQ_ordinalcat4_2.0-<2.5", ]
acq_ordinal_cat5_subset <- res_pool_ordinal_subset[res_pool_ordinal_subset$term == "ACQ_ordinalcat5_2.5-<3.0", ]
acq_ordinal_cat6_subset <- res_pool_ordinal_subset[res_pool_ordinal_subset$term == "ACQ_ordinalcat6_>=3.0", ]

# Store results (starting from row 2, as row 1 is reference)
ordinal_results$Any_ACQ_Subset_aRR[2] <- acq_ordinal_cat2_subset$estimate
ordinal_results$Any_ACQ_Subset_Lower_CI[2] <- acq_ordinal_cat2_subset$`2.5 %`
ordinal_results$Any_ACQ_Subset_Upper_CI[2] <- acq_ordinal_cat2_subset$`97.5 %`
ordinal_results$Any_ACQ_Subset_p_value[2] <- acq_ordinal_cat2_subset$p.value

ordinal_results$Any_ACQ_Subset_aRR[3] <- acq_ordinal_cat3_subset$estimate
ordinal_results$Any_ACQ_Subset_Lower_CI[3] <- acq_ordinal_cat3_subset$`2.5 %`
ordinal_results$Any_ACQ_Subset_Upper_CI[3] <- acq_ordinal_cat3_subset$`97.5 %`
ordinal_results$Any_ACQ_Subset_p_value[3] <- acq_ordinal_cat3_subset$p.value

ordinal_results$Any_ACQ_Subset_aRR[4] <- acq_ordinal_cat4_subset$estimate
ordinal_results$Any_ACQ_Subset_Lower_CI[4] <- acq_ordinal_cat4_subset$`2.5 %`
ordinal_results$Any_ACQ_Subset_Upper_CI[4] <- acq_ordinal_cat4_subset$`97.5 %`
ordinal_results$Any_ACQ_Subset_p_value[4] <- acq_ordinal_cat4_subset$p.value

ordinal_results$Any_ACQ_Subset_aRR[5] <- acq_ordinal_cat5_subset$estimate
ordinal_results$Any_ACQ_Subset_Lower_CI[5] <- acq_ordinal_cat5_subset$`2.5 %`
ordinal_results$Any_ACQ_Subset_Upper_CI[5] <- acq_ordinal_cat5_subset$`97.5 %`
ordinal_results$Any_ACQ_Subset_p_value[5] <- acq_ordinal_cat5_subset$p.value

ordinal_results$Any_ACQ_Subset_aRR[6] <- acq_ordinal_cat6_subset$estimate
ordinal_results$Any_ACQ_Subset_Lower_CI[6] <- acq_ordinal_cat6_subset$`2.5 %`
ordinal_results$Any_ACQ_Subset_Upper_CI[6] <- acq_ordinal_cat6_subset$`97.5 %`
ordinal_results$Any_ACQ_Subset_p_value[6] <- acq_ordinal_cat6_subset$p.value

# Format results for display with n (%)
ordinal_results_display <- ordinal_results %>%
  mutate(
    Full_ORACLE2_n_pct = sprintf("%d (%.1f%%)", Full_ORACLE2_n, Full_ORACLE2_pct),
    Full_ORACLE2_Result = ifelse(is.na(Full_ORACLE2_Lower_CI),
                                 "1.0 [REF]",
                                 sprintf("%.3f [%.3f - %.3f]", 
                                         Full_ORACLE2_aRR, 
                                         Full_ORACLE2_Lower_CI, 
                                         Full_ORACLE2_Upper_CI)),
    Full_ORACLE2_p = ifelse(is.na(Full_ORACLE2_p_value),
                            "-",
                            sprintf("%.4f", Full_ORACLE2_p_value)),
    Any_ACQ_Subset_n_pct = sprintf("%d (%.1f%%)", Any_ACQ_Subset_n, Any_ACQ_Subset_pct),
    Any_ACQ_Subset_Result = ifelse(is.na(Any_ACQ_Subset_Lower_CI),
                                   "1.0 [REF]",
                                   sprintf("%.3f [%.3f - %.3f]", 
                                           Any_ACQ_Subset_aRR, 
                                           Any_ACQ_Subset_Lower_CI, 
                                           Any_ACQ_Subset_Upper_CI)),
    Any_ACQ_Subset_p = ifelse(is.na(Any_ACQ_Subset_p_value),
                              "-",
                              sprintf("%.4f", Any_ACQ_Subset_p_value))
  ) %>%
  select(Category, Full_ORACLE2_n_pct, Full_ORACLE2_Result, Full_ORACLE2_p, 
         Any_ACQ_Subset_n_pct, Any_ACQ_Subset_Result, Any_ACQ_Subset_p)

# Print ordinal results table
cat("\n=== ORDINAL CLASSIFICATION RESULTS TABLE (EXTENDED ADJUSTMENT) ===\n")
print(kable(ordinal_results_display, 
            format = "markdown",
            col.names = c("ACQ-5 Category", 
                          "n (%)", "Full ORACLE2 aRR [95% CI]", "p-value",
                          "n (%)", "Any ACQ Subset aRR [95% CI]", "p-value"),
            caption = "Adjusted rate ratios for ACQ-5 ordinal classification (reference: <0.75) - Extended adjustment"))

# Save ordinal results as CSV
write.csv(ordinal_results, 
          "ACQ5_ordinal_results_extended.csv", 
          row.names = FALSE)

# Save formatted ordinal table as HTML
writeLines(
  kable(ordinal_results_display, 
        format = "html",
        col.names = c("ACQ-5 Category", 
                      "n (%)", "Full ORACLE2 aRR [95% CI]", "p-value",
                      "n (%)", "Any ACQ Subset aRR [95% CI]", "p-value"),
        caption = "Adjusted rate ratios for ACQ-5 ordinal classification (reference: <0.75) - Extended adjustment"),
  "ACQ5_ordinal_results_extended.html"
)

# Print sample sizes for ordinal categories
cat("\n=== SAMPLE SIZES FOR ORDINAL CATEGORIES ===\n")

cat("\nFull ORACLE2 (imputation 1):\n")
print(ordinal_counts_full)
cat("\nProportions:\n")
print(ordinal_pct_full)

cat("\nAny ACQ Subset (imputation 1):\n")
print(ordinal_counts_subset)
cat("\nProportions:\n")
print(ordinal_pct_subset)

cat("\n=== ADDITIONAL ACQ-5 ANALYSES COMPLETED ===\n")
cat("Results saved to:\n")
cat("  - ACQ5_ordinal_results_extended.csv\n")
cat("  - ACQ5_ordinal_results_extended.html\n")



########################
############################################
# Prognostic value of the distinct ACQ item
# WITH PUBLICATION-READY FIGURES
#############################################

# Load required library for high-quality figures
library(metafor)

###Creation UNIVARIABLE
res_comb = NULL
R2_values<-c()
AICC_values<-c()
for (i in 1:10){
  res_comb[[i]] =glm.nb(Number_severe_asthma_attacks_during_followup ~
                          ACQ_baseline_score_item1_sleepawakenings+
                          ACQ_baseline_score_item2_morningsymptoms+
                          ACQ_baseline_score_item3_activitylimitation+
                          ACQ_baseline_score_item4_dyspnea+
                          ACQ_baseline_score_item5_wheezing+
                          offset(log(Follow_up_duration_days)) + 
                          as.factor(Enrolled_Trial_name), 
                        data = subset(Data_OracleACQitems, .imp == i))
  AICC_values<-c(AICC_values,AICc(res_comb[[i]]))
  R2_values<-c(R2_values,1 - (summary(res_comb[[i]])$deviance / summary(res_comb[[i]])$null.deviance))
}
res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
res_pool 
AICC_mean<-mean(AICC_values)
R2_mean<-mean(R2_values)
res_pool
#selectrateratios except enrolled trial name and intercept
res_pool_no_trialname <- res_pool[!startsWith(as.character(res_pool$term), "as.factor(Enrolled_Trial_name"), ]
res_pool_no_trialname <- res_pool[!grepl("Enrolled_Trial_name|Intercept", as.character(res_pool$term)), ]

#makeforestplot
forest_data <- data.frame(
  name = gsub(".*_item([0-9]).*?([a-zA-Z]+)$", "Item \\1: \\2", 
              as.character(res_pool_no_trialname$term)),
  estimate = res_pool_no_trialname$estimate,
  ci.lb = res_pool_no_trialname[, "2.5 %"],
  ci.ub = res_pool_no_trialname[, "97.5 %"]
)

# Export PNG version
png("ACQ_items_univariable_forest.png", width = 10, height = 8, units = "in", res = 600)
par(mar = c(7, 4, 4, 2))
forest(
  x = forest_data$estimate,
  ci.lb = forest_data$ci.lb,
  ci.ub = forest_data$ci.ub,
  slab = forest_data$name,
  refline = 1,
  header = "ACQ-5 Items: Univariable Analysis",
  xlab = "Rate Ratio",
  alim = c(0, 2),
  at = c(0, 0.5, 1, 1.5, 2),
  ilab.xpos = -1,
  cex = 1.0,
  cex.lab = 1.2,
  cex.axis = 1.0,
  psize = 1.2
)
mtext(paste0("Model Performance: R² = ", round(R2_mean, 3),
             "  |  AICc = ", round(AICC_mean, 1)),
      side = 1, line = 5, cex = 1.0)
dev.off()

# Export PDF version
pdf("ACQ_items_univariable_forest.pdf", width = 10, height = 8)
par(mar = c(7, 4, 4, 2))
forest(
  x = forest_data$estimate,
  ci.lb = forest_data$ci.lb,
  ci.ub = forest_data$ci.ub,
  slab = forest_data$name,
  refline = 1,
  header = "ACQ-5 Items: Univariable Analysis",
  xlab = "Rate Ratio",
  alim = c(0, 2),
  at = c(0, 0.5, 1, 1.5, 2),
  ilab.xpos = -1,
  cex = 1.0,
  cex.lab = 1.2,
  cex.axis = 1.0,
  psize = 1.2
)
mtext(paste0("Model Performance: R² = ", round(R2_mean, 3),
             "  |  AICc = ", round(AICC_mean, 1)),
      side = 1, line = 5, cex = 1.0)
dev.off()

cat("\nFigure saved: ACQ_items_univariable_forest.png and .pdf\n")

###Creation UNIVARIABLE WITH ALL ITEM INTERACTIONS
res_comb = NULL
R2_values<-c()
AICC_values<-c()
for (i in 1:10){
  res_comb[[i]] =glm.nb(Number_severe_asthma_attacks_during_followup ~
                          ACQ_baseline_score_item1_sleepawakenings*
                          ACQ_baseline_score_item2_morningsymptoms*
                          ACQ_baseline_score_item3_activitylimitation*
                          ACQ_baseline_score_item4_dyspnea*
                          ACQ_baseline_score_item5_wheezing+
                          offset(log(Follow_up_duration_days)) + 
                          as.factor(Enrolled_Trial_name), 
                        data = subset(Data_OracleACQitems, .imp == i))
  AICC_values<-c(AICC_values,AICc(res_comb[[i]]))
  R2_values<-c(R2_values,1 - (summary(res_comb[[i]])$deviance / summary(res_comb[[i]])$null.deviance))
}
res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
res_pool 
AICC_mean<-mean(AICC_values)
R2_mean<-mean(R2_values)
res_pool
#selectrateratios except enrolled trial name and intercept
res_pool_no_trialname <- res_pool[!startsWith(as.character(res_pool$term), "as.factor(Enrolled_Trial_name"), ]
res_pool_no_trialname <- res_pool[!grepl("Enrolled_Trial_name|Intercept", as.character(res_pool$term)), ]

# Function to clean a single ACQ term
clean_single_term <- function(term) {
  item_num <- gsub(".*item([0-9])_([a-zA-Z]+).*", "\\1", term)
  symptom_type <- gsub(".*item[0-9]_([a-zA-Z]+).*", "\\1", term)
  return(paste0("I", item_num, ":", symptom_type))
}

# Function to clean interaction terms
clean_interaction_term <- function(term) {
  parts <- strsplit(as.character(term), ":")[[1]]
  cleaned_parts <- sapply(parts, clean_single_term)
  return(paste(cleaned_parts, collapse = " × "))
}

# Create the forest plot with interaction labels
forest_data <- data.frame(
  name = sapply(as.character(res_pool_no_trialname$term), clean_interaction_term),
  estimate = res_pool_no_trialname$estimate,
  ci.lb = res_pool_no_trialname[, "2.5 %"],
  ci.ub = res_pool_no_trialname[, "97.5 %"]
)

# Export PNG version
png("ACQ_items_all_interactions_forest.png", width = 14, height = 16, units = "in", res = 600)
par(mar = c(7, 12, 4, 2))
forest(
  x = forest_data$estimate,
  ci.lb = forest_data$ci.lb,
  ci.ub = forest_data$ci.ub,
  slab = forest_data$name,
  refline = 1,
  header = "ACQ-5 Items: All Possible Interactions",
  xlab = "Rate Ratio",
  alim = c(0, 2),
  at = c(0, 0.5, 1, 1.5, 2),
  ilab.xpos = -1,
  cex = 0.9,
  cex.lab = 1.2,
  cex.axis = 1.0,
  psize = 1.2
)
mtext(paste0("Model Performance: R² = ", round(R2_mean, 3),
             "  |  AICc = ", round(AICC_mean, 1)),
      side = 1, line = 5, cex = 1.0)
dev.off()

# Export PDF version
pdf("ACQ_items_all_interactions_forest.pdf", width = 14, height = 16)
par(mar = c(7, 12, 4, 2))
forest(
  x = forest_data$estimate,
  ci.lb = forest_data$ci.lb,
  ci.ub = forest_data$ci.ub,
  slab = forest_data$name,
  refline = 1,
  header = "ACQ-5 Items: All Possible Interactions",
  xlab = "Rate Ratio",
  alim = c(0, 2),
  at = c(0, 0.5, 1, 1.5, 2),
  ilab.xpos = -1,
  cex = 0.9,
  cex.lab = 1.2,
  cex.axis = 1.0,
  psize = 1.2
)
mtext(paste0("Model Performance: R² = ", round(R2_mean, 3),
             "  |  AICc = ", round(AICC_mean, 1)),
      side = 1, line = 5, cex = 1.0)
dev.off()

cat("\nFigure saved: ACQ_items_all_interactions_forest.png and .pdf\n")

###CREATE UNIVARIABLE WITH ITERATIVE INTERACTION TESTING
# Create list of all possible 2-way interactions
items <- 1:5
interactions <- combn(items, 2)
n_interactions <- ncol(interactions)

# Define the symptom types for each item
symptom_types <- c("sleepawakenings", "morningsymptoms", "activitylimitation", "dyspnea", "wheezing")

# Initialize list to store interaction results
all_interactions <- NULL

# Loop through each interaction
for(j in 1:n_interactions) {
  res_comb <- NULL
  
  # Get item numbers and their corresponding symptom types
  item1 <- interactions[1,j]
  item2 <- interactions[2,j]
  symptom1 <- symptom_types[item1]
  symptom2 <- symptom_types[item2]
  
  formula_str <- paste0("Number_severe_asthma_attacks_during_followup ~ ",
                        "ACQ_baseline_score_item1_sleepawakenings + ",
                        "ACQ_baseline_score_item2_morningsymptoms + ",
                        "ACQ_baseline_score_item3_activitylimitation + ",
                        "ACQ_baseline_score_item4_dyspnea + ",
                        "ACQ_baseline_score_item5_wheezing + ",
                        "ACQ_baseline_score_item", item1, "_", symptom1, ":",
                        "ACQ_baseline_score_item", item2, "_", symptom2, " + ",
                        "offset(log(Follow_up_duration_days)) + as.factor(Enrolled_Trial_name)")
  
  # Run imputed models
  for(i in 1:10) {
    res_comb[[i]] <- glm.nb(as.formula(formula_str), 
                            data = subset(Data_OracleACQitems, .imp == i))
  }
  
  # Pool results
  res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
  
  # Extract interaction term
  interaction_row <- res_pool[grep(":", res_pool$term), ]
  
  # Store result
  all_interactions <- rbind(all_interactions, interaction_row)
}

# Create forest data
forest_data <- data.frame(
  name = as.character(all_interactions$term),
  estimate = all_interactions$estimate,
  ci.lb = all_interactions[, "2.5 %"],
  ci.ub = all_interactions[, "97.5 %"]
)

# Clean up the interaction names for display
forest_data$name <- gsub("ACQ_baseline_score_", "", forest_data$name)
forest_data$name <- gsub("_sleepawakenings", "", forest_data$name)
forest_data$name <- gsub("_morningsymptoms", "", forest_data$name)
forest_data$name <- gsub("_activitylimitation", "", forest_data$name)
forest_data$name <- gsub("_dyspnea", "", forest_data$name)
forest_data$name <- gsub("_wheezing", "", forest_data$name)
forest_data$name <- gsub("item", "Item ", forest_data$name)
forest_data$name <- gsub(":", " × Item ", forest_data$name)

# Determine significant results
significant <- (forest_data$ci.lb > 1) | (forest_data$ci.ub < 1)
n_sig <- sum(significant)

# Export PNG version - WITHOUT meta-analysis pooling
png("ACQ_items_two_way_interactions_forest.png", width = 12, height = 10, units = "in", res = 600)
par(mar = c(7, 12, 4, 2))
forest(
  x = forest_data$estimate,
  ci.lb = forest_data$ci.lb,
  ci.ub = forest_data$ci.ub,
  slab = forest_data$name,
  refline = 1,
  header = "ACQ-5 Items: Two-Way Interactions",
  xlab = "Rate Ratio",
  alim = c(0.9, 1.1),
  at = seq(0.9, 1.1, by = 0.05),
  cex = 0.9,
  cex.lab = 1.2,
  cex.axis = 1.0,
  psize = 1.2
)
mtext(paste0("Number of significant interactions: ", n_sig, " out of ", n_interactions), 
      side = 1, line = 5, cex = 1.0)
dev.off()

# Export PDF version
pdf("ACQ_items_two_way_interactions_forest.pdf", width = 12, height = 10)
par(mar = c(7, 12, 4, 2))
forest(
  x = forest_data$estimate,
  ci.lb = forest_data$ci.lb,
  ci.ub = forest_data$ci.ub,
  slab = forest_data$name,
  refline = 1,
  header = "ACQ-5 Items: Two-Way Interactions",
  xlab = "Rate Ratio",
  alim = c(0.9, 1.1),
  at = seq(0.9, 1.1, by = 0.05),
  cex = 0.9,
  cex.lab = 1.2,
  cex.axis = 1.0,
  psize = 1.2
)
mtext(paste0("Number of significant interactions: ", n_sig, " out of ", n_interactions), 
      side = 1, line = 5, cex = 1.0)
dev.off()

cat("\nFigure saved: ACQ_items_two_way_interactions_forest.png and .pdf\n")

#######################################################
# FIGURE 2: ACQ PROGNOSTIC VALUE - MEAN SCORE & ITEMS
# 2x2 Panel: Top = Mean ACQ (25% height) | Bottom = Individual Items
# Left = Univariable | Right = Multivariable
# Publication-ready format
#######################################################

cat("\n=== CREATING FIGURE 2: ACQ PROGNOSTIC VALUE (2x2 PANEL) ===\n")

#######################################################
# PART A: ACQ MEAN SCORE - UNIVARIABLE
#######################################################
cat("Running univariable models for ACQ mean score...\n")

res_comb_mean_univ <- list()
for (i in 1:10) {
  res_comb_mean_univ[[i]] <- glm.nb(
    Number_severe_asthma_attacks_during_followup ~
      ACQ_baseline_score_mean_imputated +
      offset(log(Follow_up_duration_days)) +
      as.factor(Enrolled_Trial_name),
    data = subset(Data_OracleACQitems, .imp == i)
  )
}

res_pool_mean_univ <- summary(pool(res_comb_mean_univ), conf.int = TRUE, exp = TRUE)
res_pool_mean_univ_acq <- res_pool_mean_univ[grep("ACQ_baseline_score_mean", res_pool_mean_univ$term), ]

#######################################################
# PART B: ACQ MEAN SCORE - MULTIVARIABLE
#######################################################
cat("Running multivariable models for ACQ mean score...\n")

res_comb_mean_multi <- list()
for (i in 1:10) {
  res_comb_mean_multi[[i]] <- glm.nb(
    Number_severe_asthma_attacks_during_followup ~
      ACQ_baseline_score_mean_imputated +
      as.factor(Any_severe_attack_previous_12m_0no_1yes_imputated) +
      as.factor(Treatment_step) +
      FEV1_preBD_PCT_Baseline_imputated +
      Eosinophils_Log_imputated*FeNO_Log_imputated +
      BMI_imputated +
      as.factor(Gender_imputated) +
      as.factor(Smoking_Statut_imputated) +
      FEV1_FVC_ratio_imputated +
      as.factor(CRSsNP_imputated) +
      as.factor(CRSwNP_imputated) +
      as.factor(Allergic_rhinitis_imputated) +
      offset(log(Follow_up_duration_days)) +
      as.factor(Enrolled_Trial_name),
    data = subset(Data_OracleACQitems, .imp == i)
  )
}

res_pool_mean_multi <- summary(pool(res_comb_mean_multi), conf.int = TRUE, exp = TRUE)
res_pool_mean_multi_acq <- res_pool_mean_multi[grep("ACQ_baseline_score_mean", res_pool_mean_multi$term), ]

#######################################################
# PART C: ACQ ITEMS - UNIVARIABLE
#######################################################
cat("Running univariable models for 5 ACQ items...\n")

res_comb_univ <- list()
for (i in 1:10) {
  res_comb_univ[[i]] <- glm.nb(
    Number_severe_asthma_attacks_during_followup ~
      ACQ_baseline_score_item1_sleepawakenings +
      ACQ_baseline_score_item2_morningsymptoms +
      ACQ_baseline_score_item3_activitylimitation +
      ACQ_baseline_score_item4_dyspnea +
      ACQ_baseline_score_item5_wheezing +
      offset(log(Follow_up_duration_days)) +
      as.factor(Enrolled_Trial_name),
    data = subset(Data_OracleACQitems, .imp == i)
  )
}

res_pool_univ <- summary(pool(res_comb_univ), conf.int = TRUE, exp = TRUE)
res_pool_univ_items <- res_pool_univ[grep("ACQ_baseline_score_item", res_pool_univ$term), ]

#######################################################
# PART D: ACQ ITEMS - MULTIVARIABLE
#######################################################
cat("Running multivariable models for 5 ACQ items...\n")

res_comb_multi <- list()
for (i in 1:10) {
  res_comb_multi[[i]] <- glm.nb(
    Number_severe_asthma_attacks_during_followup ~
      ACQ_baseline_score_item1_sleepawakenings +
      ACQ_baseline_score_item2_morningsymptoms +
      ACQ_baseline_score_item3_activitylimitation +
      ACQ_baseline_score_item4_dyspnea +
      ACQ_baseline_score_item5_wheezing +
      as.factor(Any_severe_attack_previous_12m_0no_1yes_imputated) +
      as.factor(Treatment_step) +
      FEV1_preBD_PCT_Baseline_imputated +
      Eosinophils_Log_imputated*FeNO_Log_imputated +
      BMI_imputated +
      as.factor(Gender_imputated) +
      as.factor(Smoking_Statut_imputated) +
      FEV1_FVC_ratio_imputated +
      as.factor(CRSsNP_imputated) +
      as.factor(CRSwNP_imputated) +
      as.factor(Allergic_rhinitis_imputated) +
      offset(log(Follow_up_duration_days)) +
      as.factor(Enrolled_Trial_name),
    data = subset(Data_OracleACQitems, .imp == i)
  )
}

res_pool_multi <- summary(pool(res_comb_multi), conf.int = TRUE, exp = TRUE)
res_pool_multi_items <- res_pool_multi[grep("ACQ_baseline_score_item", res_pool_multi$term), ]

#######################################################
# PREPARE DATA FOR PLOTTING
#######################################################

# ACQ Mean Score - Univariable
forest_data_mean_univ <- data.frame(
  name = "ACQ-5 mean score",
  estimate = res_pool_mean_univ_acq$estimate,
  ci.lb = res_pool_mean_univ_acq[, "2.5 %"],
  ci.ub = res_pool_mean_univ_acq[, "97.5 %"],
  pval = res_pool_mean_univ_acq$p.value
)

# ACQ Mean Score - Multivariable
forest_data_mean_multi <- data.frame(
  name = "ACQ-5 mean score",
  estimate = res_pool_mean_multi_acq$estimate,
  ci.lb = res_pool_mean_multi_acq[, "2.5 %"],
  ci.ub = res_pool_mean_multi_acq[, "97.5 %"],
  pval = res_pool_mean_multi_acq$p.value
)

# ACQ Items - Univariable
forest_data_univ <- data.frame(
  name = as.character(res_pool_univ_items$term),
  estimate = res_pool_univ_items$estimate,
  ci.lb = res_pool_univ_items[, "2.5 %"],
  ci.ub = res_pool_univ_items[, "97.5 %"],
  pval = res_pool_univ_items$p.value
)

# ACQ Items - Multivariable
forest_data_multi <- data.frame(
  name = as.character(res_pool_multi_items$term),
  estimate = res_pool_multi_items$estimate,
  ci.lb = res_pool_multi_items[, "2.5 %"],
  ci.ub = res_pool_multi_items[, "97.5 %"],
  pval = res_pool_multi_items$p.value
)

# Clean names for items
clean_names <- function(names) {
  names <- gsub("ACQ_baseline_score_item1_sleepawakenings", "Sleep awakenings", names)
  names <- gsub("ACQ_baseline_score_item2_morningsymptoms", "Morning symptoms", names)
  names <- gsub("ACQ_baseline_score_item3_activitylimitation", "Activity limitation", names)
  names <- gsub("ACQ_baseline_score_item4_dyspnea", "Dyspnea", names)
  names <- gsub("ACQ_baseline_score_item5_wheezing", "Wheezing", names)
  return(names)
}

forest_data_univ$name <- clean_names(forest_data_univ$name)
forest_data_multi$name <- clean_names(forest_data_multi$name)

# Reverse order so they plot top to bottom
forest_data_univ <- forest_data_univ[5:1, ]
forest_data_multi <- forest_data_multi[5:1, ]

# Calculate RR per 0.5-unit increment (RR^0.5) - THESE WILL BE PLOTTED
# Mean score - univariable
forest_data_mean_univ$estimate_0.5 <- forest_data_mean_univ$estimate^0.5
forest_data_mean_univ$ci.lb_0.5 <- forest_data_mean_univ$ci.lb^0.5
forest_data_mean_univ$ci.ub_0.5 <- forest_data_mean_univ$ci.ub^0.5

# Mean score - multivariable
forest_data_mean_multi$estimate_0.5 <- forest_data_mean_multi$estimate^0.5
forest_data_mean_multi$ci.lb_0.5 <- forest_data_mean_multi$ci.lb^0.5
forest_data_mean_multi$ci.ub_0.5 <- forest_data_mean_multi$ci.ub^0.5

# Items - univariable
forest_data_univ$estimate_0.5 <- forest_data_univ$estimate^0.5
forest_data_univ$ci.lb_0.5 <- forest_data_univ$ci.lb^0.5
forest_data_univ$ci.ub_0.5 <- forest_data_univ$ci.ub^0.5

# Items - multivariable
forest_data_multi$estimate_0.5 <- forest_data_multi$estimate^0.5
forest_data_multi$ci.lb_0.5 <- forest_data_multi$ci.lb^0.5
forest_data_multi$ci.ub_0.5 <- forest_data_multi$ci.ub^0.5

# Determine significance
sig_mean_univ <- forest_data_mean_univ$pval < 0.05
sig_mean_multi <- forest_data_mean_multi$pval < 0.05
sig_univ <- forest_data_univ$pval < 0.05
sig_multi <- forest_data_multi$pval < 0.05

# Labels
labels_for_items <- forest_data_univ$name

# X-axis limits for log scale
x_min <- 0.95
x_max <- 1.20
log_breaks <- c(0.95, 1.00, 1.05, 1.10, 1.15, 1.20)

#######################################################
# CREATE 2x2 PANEL FIGURE - PNG VERSION
#######################################################

png("Figure2_ACQ_mean_and_items_2x2.png", width = 18, height = 10, units = "in", res = 600)

# Set up 2x2 layout with equal plot areas and space between columns
# Left panels: mar = c(., 20, ., 5) = 25 total horizontal margin
# Right panels: mar = c(., 5, ., 2) = 7 total horizontal margin
# Margin difference = 18 lines
# Space between columns = 5 (right of left) + 5 (left of right) = 10 lines
layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE), 
       widths = c(1.5, 1), 
       heights = c(0.25, 1))

#######################################################
# PANEL A (TOP LEFT): ACQ MEAN SCORE - UNIVARIABLE
#######################################################
par(mar = c(1, 20, 4, 5))

yi_mean <- 1
plot(NA, xlim = log(c(x_min, x_max)), ylim = c(0.5, 1.5),
     xlab = "", ylab = "",
     axes = FALSE, frame.plot = FALSE, xaxs = "i", yaxs = "i")

# NO X-AXIS for top panels
abline(v = log(1), lty = 2, col = "gray50", lwd = 1)

# Y-axis label with increased font size (1.25) - closer to plot
text(log(x_min) - 0.06, yi_mean, forest_data_mean_univ$name, pos = 2, xpd = TRUE, cex = 1.25, font = 1)

# CI and point - using 0.5-unit increment values
segments(log(forest_data_mean_univ$ci.lb_0.5), yi_mean,
         log(forest_data_mean_univ$ci.ub_0.5), yi_mean, lwd = 2)
if (sig_mean_univ) {
  points(log(forest_data_mean_univ$estimate_0.5), yi_mean, pch = 18, cex = 2.5, col = "red")
} else {
  points(log(forest_data_mean_univ$estimate_0.5), yi_mean, pch = 18, cex = 2.5, col = "black")
}

mtext("A. ACQ Mean Score - Univariable", side = 3, line = 2.5, cex = 1.1, font = 2, adj = 0)
mtext("(adjusted for trial only)", side = 3, line = 1, cex = 0.85, adj = 0)

#######################################################
# PANEL B (TOP RIGHT): ACQ MEAN SCORE - MULTIVARIABLE
#######################################################
par(mar = c(1, 5, 4, 2))

plot(NA, xlim = log(c(x_min, x_max)), ylim = c(0.5, 1.5),
     xlab = "", ylab = "",
     axes = FALSE, frame.plot = FALSE, xaxs = "i", yaxs = "i")

# NO X-AXIS for top panels
abline(v = log(1), lty = 2, col = "gray50", lwd = 1)

# CI and point - using 0.5-unit increment values
segments(log(forest_data_mean_multi$ci.lb_0.5), yi_mean,
         log(forest_data_mean_multi$ci.ub_0.5), yi_mean, lwd = 2)
if (sig_mean_multi) {
  points(log(forest_data_mean_multi$estimate_0.5), yi_mean, pch = 18, cex = 2.5, col = "red")
} else {
  points(log(forest_data_mean_multi$estimate_0.5), yi_mean, pch = 18, cex = 2.5, col = "black")
}

mtext("B. ACQ Mean Score - Multivariable", side = 3, line = 2.5, cex = 1.1, font = 2, adj = 0)
mtext("(adjusted for biomarkers + clinical factors)", side = 3, line = 1, cex = 0.85, adj = 0)

#######################################################
# PANEL C (BOTTOM LEFT): ACQ ITEMS - UNIVARIABLE
#######################################################
par(mar = c(5, 20, 3, 5))

yi <- 5:1
plot(NA, xlim = log(c(x_min, x_max)), ylim = c(0.5, 5.5),
     xlab = "", ylab = "",
     axes = FALSE, frame.plot = FALSE, xaxs = "i", yaxs = "i")

# X-axis with increased font size (1.25)
axis(1, at = log(log_breaks), labels = log_breaks, cex.axis = 1.25)
mtext("Rate Ratio per 0.5-point increment", side = 1, line = 3, cex = 1.25)
abline(v = log(1), lty = 2, col = "gray50", lwd = 1)

# Y-axis labels with increased font size (1.25) - closer to plot
for (i in 1:5) {
  text(log(x_min) - 0.06, yi[i], labels_for_items[i], pos = 2, xpd = TRUE, cex = 1.25, font = 1)
}

# CI and points - using 0.5-unit increment values
for (i in 1:5) {
  segments(log(forest_data_univ$ci.lb_0.5[i]), yi[i],
           log(forest_data_univ$ci.ub_0.5[i]), yi[i], lwd = 2)
  if (sig_univ[i]) {
    points(log(forest_data_univ$estimate_0.5[i]), yi[i], pch = 18, cex = 2, col = "red")
  } else {
    points(log(forest_data_univ$estimate_0.5[i]), yi[i], pch = 18, cex = 2, col = "black")
  }
}

mtext("C. ACQ Items - Univariable", side = 3, line = 1.5, cex = 1.1, font = 2, adj = 0)

#######################################################
# PANEL D (BOTTOM RIGHT): ACQ ITEMS - MULTIVARIABLE
#######################################################
par(mar = c(5, 5, 3, 2))

plot(NA, xlim = log(c(x_min, x_max)), ylim = c(0.5, 5.5),
     xlab = "", ylab = "",
     axes = FALSE, frame.plot = FALSE, xaxs = "i", yaxs = "i")

# X-axis with increased font size (1.25)
axis(1, at = log(log_breaks), labels = log_breaks, cex.axis = 1.25)
mtext("Rate Ratio per 0.5-point increment", side = 1, line = 3, cex = 1.25)
abline(v = log(1), lty = 2, col = "gray50", lwd = 1)

# CI and points - using 0.5-unit increment values
for (i in 1:5) {
  segments(log(forest_data_multi$ci.lb_0.5[i]), yi[i],
           log(forest_data_multi$ci.ub_0.5[i]), yi[i], lwd = 2)
  if (sig_multi[i]) {
    points(log(forest_data_multi$estimate_0.5[i]), yi[i], pch = 18, cex = 2, col = "red")
  } else {
    points(log(forest_data_multi$estimate_0.5[i]), yi[i], pch = 18, cex = 2, col = "black")
  }
}

mtext("D. ACQ Items - Multivariable", side = 3, line = 1.5, cex = 1.1, font = 2, adj = 0)

dev.off()

#######################################################
# CREATE 2x2 PANEL FIGURE - PDF VERSION
#######################################################

pdf("Figure2_ACQ_mean_and_items_2x2.pdf", width = 18, height = 10)

layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE), 
       widths = c(1.5, 1), 
       heights = c(0.25, 1))

# PANEL A
par(mar = c(1, 20, 4, 5))
yi_mean <- 1
plot(NA, xlim = log(c(x_min, x_max)), ylim = c(0.5, 1.5),
     xlab = "", ylab = "",
     axes = FALSE, frame.plot = FALSE, xaxs = "i", yaxs = "i")
abline(v = log(1), lty = 2, col = "gray50", lwd = 1)
text(log(x_min) - 0.06, yi_mean, forest_data_mean_univ$name, pos = 2, xpd = TRUE, cex = 1.25, font = 1)
segments(log(forest_data_mean_univ$ci.lb_0.5), yi_mean,
         log(forest_data_mean_univ$ci.ub_0.5), yi_mean, lwd = 2)
if (sig_mean_univ) {
  points(log(forest_data_mean_univ$estimate_0.5), yi_mean, pch = 18, cex = 2.5, col = "red")
} else {
  points(log(forest_data_mean_univ$estimate_0.5), yi_mean, pch = 18, cex = 2.5, col = "black")
}
mtext("A. ACQ Mean Score - Univariable", side = 3, line = 2.5, cex = 1.1, font = 2, adj = 0)
mtext("(adjusted for trial only)", side = 3, line = 1, cex = 0.85, adj = 0)

# PANEL B
par(mar = c(1, 5, 4, 2))
plot(NA, xlim = log(c(x_min, x_max)), ylim = c(0.5, 1.5),
     xlab = "", ylab = "",
     axes = FALSE, frame.plot = FALSE, xaxs = "i", yaxs = "i")
abline(v = log(1), lty = 2, col = "gray50", lwd = 1)
segments(log(forest_data_mean_multi$ci.lb_0.5), yi_mean,
         log(forest_data_mean_multi$ci.ub_0.5), yi_mean, lwd = 2)
if (sig_mean_multi) {
  points(log(forest_data_mean_multi$estimate_0.5), yi_mean, pch = 18, cex = 2.5, col = "red")
} else {
  points(log(forest_data_mean_multi$estimate_0.5), yi_mean, pch = 18, cex = 2.5, col = "black")
}
mtext("B. ACQ Mean Score - Multivariable", side = 3, line = 2.5, cex = 1.1, font = 2, adj = 0)
mtext("(adjusted for biomarkers + clinical factors)", side = 3, line = 1, cex = 0.85, adj = 0)

# PANEL C
par(mar = c(5, 20, 3, 5))
yi <- 5:1
plot(NA, xlim = log(c(x_min, x_max)), ylim = c(0.5, 5.5),
     xlab = "", ylab = "",
     axes = FALSE, frame.plot = FALSE, xaxs = "i", yaxs = "i")
axis(1, at = log(log_breaks), labels = log_breaks, cex.axis = 1.25)
mtext("Rate Ratio per 0.5-point increment", side = 1, line = 3, cex = 1.25)
abline(v = log(1), lty = 2, col = "gray50", lwd = 1)
for (i in 1:5) {
  text(log(x_min) - 0.06, yi[i], labels_for_items[i], pos = 2, xpd = TRUE, cex = 1.25, font = 1)
}
for (i in 1:5) {
  segments(log(forest_data_univ$ci.lb_0.5[i]), yi[i],
           log(forest_data_univ$ci.ub_0.5[i]), yi[i], lwd = 2)
  if (sig_univ[i]) {
    points(log(forest_data_univ$estimate_0.5[i]), yi[i], pch = 18, cex = 2, col = "red")
  } else {
    points(log(forest_data_univ$estimate_0.5[i]), yi[i], pch = 18, cex = 2, col = "black")
  }
}
mtext("C. ACQ Items - Univariable", side = 3, line = 1.5, cex = 1.1, font = 2, adj = 0)

# PANEL D
par(mar = c(5, 5, 3, 2))
plot(NA, xlim = log(c(x_min, x_max)), ylim = c(0.5, 5.5),
     xlab = "", ylab = "",
     axes = FALSE, frame.plot = FALSE, xaxs = "i", yaxs = "i")
axis(1, at = log(log_breaks), labels = log_breaks, cex.axis = 1.25)
mtext("Rate Ratio per 0.5-point increment", side = 1, line = 3, cex = 1.25)
abline(v = log(1), lty = 2, col = "gray50", lwd = 1)
for (i in 1:5) {
  segments(log(forest_data_multi$ci.lb_0.5[i]), yi[i],
           log(forest_data_multi$ci.ub_0.5[i]), yi[i], lwd = 2)
  if (sig_multi[i]) {
    points(log(forest_data_multi$estimate_0.5[i]), yi[i], pch = 18, cex = 2, col = "red")
  } else {
    points(log(forest_data_multi$estimate_0.5[i]), yi[i], pch = 18, cex = 2, col = "black")
  }
}
mtext("D. ACQ Items - Multivariable", side = 3, line = 1.5, cex = 1.1, font = 2, adj = 0)

dev.off()

cat("\nFigure 2 saved: Figure2_ACQ_mean_and_items_2x2.png and .pdf\n")
cat("2x2 panel figure: Top = ACQ mean score (25% height), Bottom = ACQ items\n")
cat("                  Left = Univariable, Right = Multivariable\n")
cat("Forest plots display rate ratios per 0.5-point increment in ACQ scores\n")
cat("ALL analyses use Data_OracleACQitems dataset\n")
cat("X-axis on log scale ranging from 0.95 to 1.20 with equal dimensions for left and right panels\n")

#######################################################
# PRINT SUMMARY STATISTICS
#######################################################

cat("\n=== FIGURE 2 SUMMARY ===\n")

cat("\nACQ MEAN SCORE - UNIVARIABLE:\n")
cat("Per 1-unit increment:\n")
print(data.frame(
  Variable = forest_data_mean_univ$name,
  RR = round(forest_data_mean_univ$estimate, 3),
  CI_lower = round(forest_data_mean_univ$ci.lb, 3),
  CI_upper = round(forest_data_mean_univ$ci.ub, 3),
  P_value = formatC(forest_data_mean_univ$pval, format = "e", digits = 2),
  Significant = sig_mean_univ
))
cat("Per 0.5-unit increment:\n")
print(data.frame(
  Variable = forest_data_mean_univ$name,
  RR = round(forest_data_mean_univ$estimate_0.5, 3),
  CI_lower = round(forest_data_mean_univ$ci.lb_0.5, 3),
  CI_upper = round(forest_data_mean_univ$ci.ub_0.5, 3)
))

cat("\nACQ MEAN SCORE - MULTIVARIABLE:\n")
cat("Per 1-unit increment:\n")
print(data.frame(
  Variable = forest_data_mean_multi$name,
  RR = round(forest_data_mean_multi$estimate, 3),
  CI_lower = round(forest_data_mean_multi$ci.lb, 3),
  CI_upper = round(forest_data_mean_multi$ci.ub, 3),
  P_value = formatC(forest_data_mean_multi$pval, format = "e", digits = 2),
  Significant = sig_mean_multi
))
cat("Per 0.5-unit increment:\n")
print(data.frame(
  Variable = forest_data_mean_multi$name,
  RR = round(forest_data_mean_multi$estimate_0.5, 3),
  CI_lower = round(forest_data_mean_multi$ci.lb_0.5, 3),
  CI_upper = round(forest_data_mean_multi$ci.ub_0.5, 3)
))

cat("\nACQ ITEMS - UNIVARIABLE:\n")
cat("Per 1-unit increment:\n")
print(data.frame(
  Item = forest_data_univ$name,
  RR = round(forest_data_univ$estimate, 3),
  CI_lower = round(forest_data_univ$ci.lb, 3),
  CI_upper = round(forest_data_univ$ci.ub, 3),
  P_value = round(forest_data_univ$pval, 4),
  Significant = sig_univ
))
cat("Per 0.5-unit increment:\n")
print(data.frame(
  Item = forest_data_univ$name,
  RR = round(forest_data_univ$estimate_0.5, 3),
  CI_lower = round(forest_data_univ$ci.lb_0.5, 3),
  CI_upper = round(forest_data_univ$ci.ub_0.5, 3)
))

cat("\nACQ ITEMS - MULTIVARIABLE:\n")
cat("Per 1-unit increment:\n")
print(data.frame(
  Item = forest_data_multi$name,
  RR = round(forest_data_multi$estimate, 3),
  CI_lower = round(forest_data_multi$ci.lb, 3),
  CI_upper = round(forest_data_multi$ci.ub, 3),
  P_value = round(forest_data_multi$pval, 4),
  Significant = sig_multi
))
cat("Per 0.5-unit increment:\n")
print(data.frame(
  Item = forest_data_multi$name,
  RR = round(forest_data_multi$estimate_0.5, 3),
  CI_lower = round(forest_data_multi$ci.lb_0.5, 3),
  CI_upper = round(forest_data_multi$ci.ub_0.5, 3)
))

#######################################################
# END OF FIGURE 2 SECTION
#######################################################

###Creation MULTIVARIABLE dyspnea and wheezing only
res_comb = NULL
R2_values<-c()
AICC_values<-c()
for (i in 1:10){
  res_comb[[i]] =glm.nb(Number_severe_asthma_attacks_during_followup ~
                          ACQ_baseline_score_item4_dyspnea +
                          ACQ_baseline_score_item5_wheezing +
                          as.factor(Any_severe_attack_previous_12m_0no_1yes_imputated) +
                          as.factor(Treatment_step) + 
                          FEV1_preBD_PCT_Baseline_imputated + 
                          Eosinophils_Log_imputated*FeNO_Log_imputated + 
                          BMI_imputated +
                          as.factor(Gender_imputated) +
                          as.factor(Smoking_Statut_imputated) +
                          FEV1_FVC_ratio_imputated +
                          as.factor(CRSsNP_imputated) +
                          as.factor(CRSwNP_imputated) +
                          as.factor(Allergic_rhinitis_imputated) +
                          offset(log(Follow_up_duration_days)) + 
                          as.factor(Enrolled_Trial_name), 
                        data = subset(Data_OracleACQitems, .imp == i))
  AICC_values<-c(AICC_values,AICc(res_comb[[i]]))
  R2_values<-c(R2_values,1 - (summary(res_comb[[i]])$deviance / summary(res_comb[[i]])$null.deviance))
}
res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
res_pool 
AICC_mean<-mean(AICC_values)
R2_mean<-mean(R2_values)
res_pool
#selectrateratios except enrolled trial name
res_pool_no_trialname <- res_pool[!startsWith(as.character(res_pool$term), "as.factor(Enrolled_Trial_name"), ]
res_pool_no_trialname <- res_pool[!grepl("Enrolled_Trial_name|Intercept", as.character(res_pool$term)), ]

# Create the forest plot with careful spacing
forest_data <- data.frame(
  name = as.character(res_pool_no_trialname$term),
  estimate = res_pool_no_trialname$estimate,
  ci.lb = res_pool_no_trialname[, "2.5 %"],
  ci.ub = res_pool_no_trialname[, "97.5 %"]
)

# Clean names for better readability
forest_data$name <- gsub("ACQ_baseline_score_item4_dyspnea", "Item 4: Dyspnea", forest_data$name)
forest_data$name <- gsub("ACQ_baseline_score_item5_wheezing", "Item 5: Wheezing", forest_data$name)
forest_data$name <- gsub("as.factor\\(|\\).*", "", forest_data$name)
forest_data$name <- gsub("_imputated", "", forest_data$name)
forest_data$name <- substr(forest_data$name, 1, 40)

# Export PNG version
png("ACQ_items_dyspnea_wheezing_multivariable_forest.png", width = 12, height = 10, units = "in", res = 600)
par(mar = c(7, 10, 4, 2))
forest(
  x = forest_data$estimate,
  ci.lb = forest_data$ci.lb,
  ci.ub = forest_data$ci.ub,
  slab = forest_data$name,
  refline = 1,
  header = "Dyspnea & Wheezing: Multivariable Analysis",
  xlab = "Rate Ratio",
  alim = c(0, 2),
  at = c(0, 0.5, 1, 1.5, 2),
  ilab.xpos = -1,
  cex = 0.9,
  cex.lab = 1.2,
  cex.axis = 1.0,
  psize = 1.2
)
mtext(paste0("Model Performance: R² = ", round(R2_mean, 3),
             "  |  AICc = ", round(AICC_mean, 1)),
      side = 1, line = 5, cex = 1.0)
dev.off()

# Export PDF version
pdf("ACQ_items_dyspnea_wheezing_multivariable_forest.pdf", width = 12, height = 10)
par(mar = c(7, 10, 4, 2))
forest(
  x = forest_data$estimate,
  ci.lb = forest_data$ci.lb,
  ci.ub = forest_data$ci.ub,
  slab = forest_data$name,
  refline = 1,
  header = "Dyspnea & Wheezing: Multivariable Analysis",
  xlab = "Rate Ratio",
  alim = c(0, 2),
  at = c(0, 0.5, 1, 1.5, 2),
  ilab.xpos = -1,
  cex = 0.9,
  cex.lab = 1.2,
  cex.axis = 1.0,
  psize = 1.2
)
mtext(paste0("Model Performance: R² = ", round(R2_mean, 3),
             "  |  AICc = ", round(AICC_mean, 1)),
      side = 1, line = 5, cex = 1.0)
dev.off()

cat("\nFigure saved: ACQ_items_dyspnea_wheezing_multivariable_forest.png and .pdf\n")

###Creation MULTIVARIABLE ACQ baseline, only selected trials - WITH EXTENDED ADJUSTMENT
res_comb = NULL
R2_values<-c()
AICC_values<-c()
for (i in 1:10){
  res_comb[[i]] =glm.nb(Number_severe_asthma_attacks_during_followup ~
                          ACQ_baseline_score_mean_imputated +
                          as.factor(Any_severe_attack_previous_12m_0no_1yes_imputated) +
                          as.factor(Treatment_step) + 
                          FEV1_preBD_PCT_Baseline_imputated + 
                          Eosinophils_Log_imputated*FeNO_Log_imputated + 
                          BMI_imputated +
                          as.factor(Gender_imputated) +
                          as.factor(Smoking_Statut_imputated) +
                          FEV1_FVC_ratio_imputated +
                          as.factor(CRSsNP_imputated) +
                          as.factor(CRSwNP_imputated) +
                          as.factor(Allergic_rhinitis_imputated) +
                          offset(log(Follow_up_duration_days)) + 
                          as.factor(Enrolled_Trial_name), 
                        data = subset(Data_OracleACQitems, .imp == i))
  AICC_values<-c(AICC_values,AICc(res_comb[[i]]))
  R2_values<-c(R2_values,1 - (summary(res_comb[[i]])$deviance / summary(res_comb[[i]])$null.deviance))
}
res_pool <- summary(pool(res_comb), conf.int = TRUE,exp=TRUE)
res_pool 
AICC_mean<-mean(AICC_values)
R2_mean<-mean(R2_values)
res_pool
#selectrateratios except enrolled trial name
res_pool_no_trialname <- res_pool[!startsWith(as.character(res_pool$term), "as.factor(Enrolled_Trial_name"), ]
res_pool_no_trialname <- res_pool[!grepl("Enrolled_Trial_name|Intercept", as.character(res_pool$term)), ]

# Create the forest plot with careful spacing
forest_data <- data.frame(
  name = as.character(res_pool_no_trialname$term),
  estimate = res_pool_no_trialname$estimate,
  ci.lb = res_pool_no_trialname[, "2.5 %"],
  ci.ub = res_pool_no_trialname[, "97.5 %"]
)

# Clean names for better readability
forest_data$name <- gsub("ACQ_baseline_score_mean_imputated", "ACQ-5 Score", forest_data$name)
forest_data$name <- gsub("as.factor\\(|\\).*", "", forest_data$name)
forest_data$name <- gsub("_imputated", "", forest_data$name)
forest_data$name <- gsub("_", " ", forest_data$name)
forest_data$name <- substr(forest_data$name, 1, 40)

# Export PNG version
png("ACQ_baseline_extended_multivariable_forest.png", width = 12, height = 10, units = "in", res = 600)
par(mar = c(7, 10, 4, 2))
forest(
  x = forest_data$estimate,
  ci.lb = forest_data$ci.lb,
  ci.ub = forest_data$ci.ub,
  slab = forest_data$name,
  refline = 1,
  header = "ACQ-5 Baseline: Extended Multivariable Model",
  xlab = "Rate Ratio",
  alim = c(0, 2),
  at = c(0, 0.5, 1, 1.5, 2),
  ilab.xpos = -1,
  cex = 0.9,
  cex.lab = 1.2,
  cex.axis = 1.0,
  psize = 1.2
)
mtext(paste0("Model Performance: R² = ", round(R2_mean, 3),
             "  |  AICc = ", round(AICC_mean, 1)),
      side = 1, line = 5, cex = 1.0)
dev.off()

# Export PDF version
pdf("ACQ_baseline_extended_multivariable_forest.pdf", width = 12, height = 10)
par(mar = c(7, 10, 4, 2))
forest(
  x = forest_data$estimate,
  ci.lb = forest_data$ci.lb,
  ci.ub = forest_data$ci.ub,
  slab = forest_data$name,
  refline = 1,
  header = "ACQ-5 Baseline: Extended Multivariable Model",
  xlab = "Rate Ratio",
  alim = c(0, 2),
  at = c(0, 0.5, 1, 1.5, 2),
  ilab.xpos = -1,
  cex = 0.9,
  cex.lab = 1.2,
  cex.axis = 1.0,
  psize = 1.2
)
mtext(paste0("Model Performance: R² = ", round(R2_mean, 3),
             "  |  AICc = ", round(AICC_mean, 1)),
      side = 1, line = 5, cex = 1.0)
dev.off()

cat("\nFigure saved: ACQ_baseline_extended_multivariable_forest.png and .pdf\n")

cat("\n=== ALL ACQ ITEM FIGURES EXPORTED ===\n")
cat("PNG files (600 dpi) and PDF files saved for:\n")
cat("  1. ACQ_items_univariable_forest\n")
cat("  2. ACQ_items_all_interactions_forest\n")
cat("  3. ACQ_items_two_way_interactions_forest\n")
cat("  4. ACQ_items_dyspnea_wheezing_multivariable_forest\n")
cat("  5. ACQ_baseline_extended_multivariable_forest\n")
cat("  6. Figure2_ACQ_items_univariable_multivariable (NEW PUBLICATION-READY)\n")





################################################################################
# ASTHMA EXACERBATION PREDICTION MODEL COMPARISON
# Comparing contribution of ACQ, attack history, and biomarkers
# Using negative binomial mixed models with trial as random intercept
################################################################################

################################################################################
# 1. DATA PREPARATION: SCALE CONTINUOUS PREDICTORS
################################################################################

cat("\n=== SCALING CONTINUOUS PREDICTORS ===\n")
cat("Scaling improves convergence and reduces computation time...\n")

# Scale continuous predictors within each imputation
for (imp in 1:10) {
  data_subset <- Data_Oracle[Data_Oracle$.imp == imp, ]
  
  # Scale all continuous predictors (mean=0, sd=1)
  Data_Oracle[Data_Oracle$.imp == imp, "ACQ_baseline_score_mean_imputated_scaled"] <- 
    as.numeric(scale(data_subset$ACQ_baseline_score_mean_imputated))
  
  Data_Oracle[Data_Oracle$.imp == imp, "FEV1_preBD_PCT_Baseline_imputated_scaled"] <- 
    as.numeric(scale(data_subset$FEV1_preBD_PCT_Baseline_imputated))
  
  Data_Oracle[Data_Oracle$.imp == imp, "Eosinophils_Log_imputated_scaled"] <- 
    as.numeric(scale(data_subset$Eosinophils_Log_imputated))
  
  Data_Oracle[Data_Oracle$.imp == imp, "FeNO_Log_imputated_scaled"] <- 
    as.numeric(scale(data_subset$FeNO_Log_imputated))
  
  Data_Oracle[Data_Oracle$.imp == imp, "BMI_imputated_scaled"] <- 
    as.numeric(scale(data_subset$BMI_imputated))
  
  Data_Oracle[Data_Oracle$.imp == imp, "FEV1_FVC_ratio_imputated_scaled"] <- 
    as.numeric(scale(data_subset$FEV1_FVC_ratio_imputated))
}

cat("Scaling complete.\n")

# Pre-subset imputed datasets for efficiency
cat("Pre-subsetting imputed datasets...\n")
Data_Oracle_list <- lapply(1:10, function(i) subset(Data_Oracle, .imp == i))

################################################################################
# 2. HELPER FUNCTIONS FOR METRICS CALCULATION
################################################################################

# Function to calculate C-statistic for predicting any vs no attacks
calculate_c_statistic <- function(model, data) {
  # Get predictions (fixed effects only for generalizability)
  pred <- predict(model, type = "response", re.form = NA)
  
  # Get observed values
  obs <- data$Number_severe_asthma_attacks_during_followup
  
  # Remove any NA values
  complete_cases <- complete.cases(pred, obs)
  pred <- pred[complete_cases]
  obs <- obs[complete_cases]
  
  # Binary outcome: any attack vs no attack
  binary_outcome <- ifelse(obs > 0, 1, 0)
  
  # Calculate C-statistic if we have both outcomes
  if(length(unique(binary_outcome)) > 1) {
    roc_obj <- roc(binary_outcome, pred, quiet = TRUE)
    c_statistic <- as.numeric(auc(roc_obj))
  } else {
    c_statistic <- NA
  }
  
  return(c_statistic)
}

# Function to calculate marginal R² (variance explained by fixed effects)
calculate_R2_marginal <- function(model) {
  null_var <- var(model@resp$y)
  fixed_var <- var(predict(model, re.form = NA))
  R2_marginal <- fixed_var / null_var
  return(R2_marginal)
}

################################################################################
# 3. SET UP PARALLEL PROCESSING
################################################################################

cl <- makeCluster(7)
registerDoParallel(cl)
cat("\n=== PARALLEL PROCESSING INITIALIZED (7 CORES) ===\n")

################################################################################
# 4. MODEL DEFINITIONS AND FITTING
################################################################################

# Initialize results storage
all_results <- list()

#-------------------------------------------------------------------------------
# MODEL 1: FULL MODEL (REFERENCE)
#-------------------------------------------------------------------------------

cat("\n=== Fitting Model 1: Full Model (Reference) ===\n")

results_model1 <- foreach(i = 1:10, 
                          .packages = c("lme4", "MASS", "pROC")) %dopar% {
                            
                            data_i <- Data_Oracle_list[[i]]
                            
                            model <- glmer.nb(
                              Number_severe_asthma_attacks_during_followup ~
                                ACQ_baseline_score_mean_imputated_scaled +
                                as.factor(Any_severe_attack_previous_12m_0no_1yes_imputated) +
                                as.factor(Treatment_step) +
                                FEV1_preBD_PCT_Baseline_imputated_scaled +
                                Eosinophils_Log_imputated_scaled * FeNO_Log_imputated_scaled +
                                BMI_imputated_scaled +
                                as.factor(Gender_imputated) +
                                as.factor(Smoking_Statut_imputated) +
                                FEV1_FVC_ratio_imputated_scaled +
                                as.factor(CRSsNP_imputated) +
                                as.factor(CRSwNP_imputated) +
                                as.factor(Allergic_rhinitis_imputated) +
                                offset(log(Follow_up_duration_days)) +
                                (1 | Enrolled_Trial_name),
                              data = data_i,
                              control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl = list(maxfun = 1e5))
                            )
                            
                            list(
                              model = model,
                              r2 = calculate_R2_marginal(model),
                              c_statistic = calculate_c_statistic(model, data_i)
                            )
                          }

all_results$model1 <- results_model1
cat(sprintf("Model 1 complete - R²: %.3f (±%.3f), C-stat: %.3f (±%.3f)\n",
            mean(sapply(results_model1, function(x) x$r2), na.rm = TRUE),
            sd(sapply(results_model1, function(x) x$r2), na.rm = TRUE),
            mean(sapply(results_model1, function(x) x$c_statistic), na.rm = TRUE),
            sd(sapply(results_model1, function(x) x$c_statistic), na.rm = TRUE)))

#-------------------------------------------------------------------------------
# MODEL 2: ACQ ONLY
#-------------------------------------------------------------------------------

cat("\n=== Fitting Model 2: ACQ Only ===\n")

results_model2 <- foreach(i = 1:10, 
                          .packages = c("lme4", "MASS", "pROC")) %dopar% {
                            
                            data_i <- Data_Oracle_list[[i]]
                            
                            model <- glmer.nb(
                              Number_severe_asthma_attacks_during_followup ~
                                ACQ_baseline_score_mean_imputated_scaled +
                                offset(log(Follow_up_duration_days)) +
                                (1 | Enrolled_Trial_name),
                              data = data_i,
                              control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl = list(maxfun = 1e5))
                            )
                            
                            list(
                              model = model,
                              r2 = calculate_R2_marginal(model),
                              c_statistic = calculate_c_statistic(model, data_i)
                            )
                          }

all_results$model2 <- results_model2
cat(sprintf("Model 2 complete - R²: %.3f (±%.3f), C-stat: %.3f (±%.3f)\n",
            mean(sapply(results_model2, function(x) x$r2), na.rm = TRUE),
            sd(sapply(results_model2, function(x) x$r2), na.rm = TRUE),
            mean(sapply(results_model2, function(x) x$c_statistic), na.rm = TRUE),
            sd(sapply(results_model2, function(x) x$c_statistic), na.rm = TRUE)))

#-------------------------------------------------------------------------------
# MODEL 3: FULL WITHOUT ACQ
#-------------------------------------------------------------------------------

cat("\n=== Fitting Model 3: Full Model Without ACQ ===\n")

results_model3 <- foreach(i = 1:10, 
                          .packages = c("lme4", "MASS", "pROC")) %dopar% {
                            
                            data_i <- Data_Oracle_list[[i]]
                            
                            model <- glmer.nb(
                              Number_severe_asthma_attacks_during_followup ~
                                as.factor(Any_severe_attack_previous_12m_0no_1yes_imputated) +
                                as.factor(Treatment_step) +
                                FEV1_preBD_PCT_Baseline_imputated_scaled +
                                Eosinophils_Log_imputated_scaled * FeNO_Log_imputated_scaled +
                                BMI_imputated_scaled +
                                as.factor(Gender_imputated) +
                                as.factor(Smoking_Statut_imputated) +
                                FEV1_FVC_ratio_imputated_scaled +
                                as.factor(CRSsNP_imputated) +
                                as.factor(CRSwNP_imputated) +
                                as.factor(Allergic_rhinitis_imputated) +
                                offset(log(Follow_up_duration_days)) +
                                (1 | Enrolled_Trial_name),
                              data = data_i,
                              control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl = list(maxfun = 1e5))
                            )
                            
                            list(
                              model = model,
                              r2 = calculate_R2_marginal(model),
                              c_statistic = calculate_c_statistic(model, data_i)
                            )
                          }

all_results$model3 <- results_model3
cat(sprintf("Model 3 complete - R²: %.3f (±%.3f), C-stat: %.3f (±%.3f)\n",
            mean(sapply(results_model3, function(x) x$r2), na.rm = TRUE),
            sd(sapply(results_model3, function(x) x$r2), na.rm = TRUE),
            mean(sapply(results_model3, function(x) x$c_statistic), na.rm = TRUE),
            sd(sapply(results_model3, function(x) x$c_statistic), na.rm = TRUE)))

#-------------------------------------------------------------------------------
# MODEL 4: ATTACK HISTORY ONLY
#-------------------------------------------------------------------------------

cat("\n=== Fitting Model 4: Attack History Only ===\n")

results_model4 <- foreach(i = 1:10, 
                          .packages = c("lme4", "MASS", "pROC")) %dopar% {
                            
                            data_i <- Data_Oracle_list[[i]]
                            
                            model <- glmer.nb(
                              Number_severe_asthma_attacks_during_followup ~
                                as.factor(Any_severe_attack_previous_12m_0no_1yes_imputated) +
                                offset(log(Follow_up_duration_days)) +
                                (1 | Enrolled_Trial_name),
                              data = data_i,
                              control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl = list(maxfun = 1e5))
                            )
                            
                            list(
                              model = model,
                              r2 = calculate_R2_marginal(model),
                              c_statistic = calculate_c_statistic(model, data_i)
                            )
                          }

all_results$model4 <- results_model4
cat(sprintf("Model 4 complete - R²: %.3f (±%.3f), C-stat: %.3f (±%.3f)\n",
            mean(sapply(results_model4, function(x) x$r2), na.rm = TRUE),
            sd(sapply(results_model4, function(x) x$r2), na.rm = TRUE),
            mean(sapply(results_model4, function(x) x$c_statistic), na.rm = TRUE),
            sd(sapply(results_model4, function(x) x$c_statistic), na.rm = TRUE)))

#-------------------------------------------------------------------------------
# MODEL 5: FULL WITHOUT ATTACK HISTORY
#-------------------------------------------------------------------------------

cat("\n=== Fitting Model 5: Full Model Without Attack History ===\n")

results_model5 <- foreach(i = 1:10, 
                          .packages = c("lme4", "MASS", "pROC")) %dopar% {
                            
                            data_i <- Data_Oracle_list[[i]]
                            
                            model <- glmer.nb(
                              Number_severe_asthma_attacks_during_followup ~
                                ACQ_baseline_score_mean_imputated_scaled +
                                as.factor(Treatment_step) +
                                FEV1_preBD_PCT_Baseline_imputated_scaled +
                                Eosinophils_Log_imputated_scaled * FeNO_Log_imputated_scaled +
                                BMI_imputated_scaled +
                                as.factor(Gender_imputated) +
                                as.factor(Smoking_Statut_imputated) +
                                FEV1_FVC_ratio_imputated_scaled +
                                as.factor(CRSsNP_imputated) +
                                as.factor(CRSwNP_imputated) +
                                as.factor(Allergic_rhinitis_imputated) +
                                offset(log(Follow_up_duration_days)) +
                                (1 | Enrolled_Trial_name),
                              data = data_i,
                              control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl = list(maxfun = 1e5))
                            )
                            
                            list(
                              model = model,
                              r2 = calculate_R2_marginal(model),
                              c_statistic = calculate_c_statistic(model, data_i)
                            )
                          }

all_results$model5 <- results_model5
cat(sprintf("Model 5 complete - R²: %.3f (±%.3f), C-stat: %.3f (±%.3f)\n",
            mean(sapply(results_model5, function(x) x$r2), na.rm = TRUE),
            sd(sapply(results_model5, function(x) x$r2), na.rm = TRUE),
            mean(sapply(results_model5, function(x) x$c_statistic), na.rm = TRUE),
            sd(sapply(results_model5, function(x) x$c_statistic), na.rm = TRUE)))

#-------------------------------------------------------------------------------
# MODEL 6: BIOMARKERS ONLY (EOSINOPHILS × FeNO)
#-------------------------------------------------------------------------------

cat("\n=== Fitting Model 6: Biomarkers Only (Eosinophils × FeNO) ===\n")

results_model6 <- foreach(i = 1:10, 
                          .packages = c("lme4", "MASS", "pROC")) %dopar% {
                            
                            data_i <- Data_Oracle_list[[i]]
                            
                            model <- glmer.nb(
                              Number_severe_asthma_attacks_during_followup ~
                                Eosinophils_Log_imputated_scaled * FeNO_Log_imputated_scaled +
                                offset(log(Follow_up_duration_days)) +
                                (1 | Enrolled_Trial_name),
                              data = data_i,
                              control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl = list(maxfun = 1e5))
                            )
                            
                            list(
                              model = model,
                              r2 = calculate_R2_marginal(model),
                              c_statistic = calculate_c_statistic(model, data_i)
                            )
                          }

all_results$model6 <- results_model6
cat(sprintf("Model 6 complete - R²: %.3f (±%.3f), C-stat: %.3f (±%.3f)\n",
            mean(sapply(results_model6, function(x) x$r2), na.rm = TRUE),
            sd(sapply(results_model6, function(x) x$r2), na.rm = TRUE),
            mean(sapply(results_model6, function(x) x$c_statistic), na.rm = TRUE),
            sd(sapply(results_model6, function(x) x$c_statistic), na.rm = TRUE)))

#-------------------------------------------------------------------------------
# MODEL 7: FULL WITHOUT BIOMARKERS
#-------------------------------------------------------------------------------

cat("\n=== Fitting Model 7: Full Model Without Biomarkers ===\n")

results_model7 <- foreach(i = 1:10, 
                          .packages = c("lme4", "MASS", "pROC")) %dopar% {
                            
                            data_i <- Data_Oracle_list[[i]]
                            
                            model <- glmer.nb(
                              Number_severe_asthma_attacks_during_followup ~
                                ACQ_baseline_score_mean_imputated_scaled +
                                as.factor(Any_severe_attack_previous_12m_0no_1yes_imputated) +
                                as.factor(Treatment_step) +
                                FEV1_preBD_PCT_Baseline_imputated_scaled +
                                BMI_imputated_scaled +
                                as.factor(Gender_imputated) +
                                as.factor(Smoking_Statut_imputated) +
                                FEV1_FVC_ratio_imputated_scaled +
                                as.factor(CRSsNP_imputated) +
                                as.factor(CRSwNP_imputated) +
                                as.factor(Allergic_rhinitis_imputated) +
                                offset(log(Follow_up_duration_days)) +
                                (1 | Enrolled_Trial_name),
                              data = data_i,
                              control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl = list(maxfun = 1e5))
                            )
                            
                            list(
                              model = model,
                              r2 = calculate_R2_marginal(model),
                              c_statistic = calculate_c_statistic(model, data_i)
                            )
                          }

all_results$model7 <- results_model7
cat(sprintf("Model 7 complete - R²: %.3f (±%.3f), C-stat: %.3f (±%.3f)\n",
            mean(sapply(results_model7, function(x) x$r2), na.rm = TRUE),
            sd(sapply(results_model7, function(x) x$r2), na.rm = TRUE),
            mean(sapply(results_model7, function(x) x$c_statistic), na.rm = TRUE),
            sd(sapply(results_model7, function(x) x$c_statistic), na.rm = TRUE)))

# Stop parallel processing
stopCluster(cl)
cat("\n=== PARALLEL PROCESSING STOPPED ===\n")

################################################################################
# 5. COMPILE RESULTS AND CALCULATE DELTAS
################################################################################

# Extract mean and SD for each model
extract_metrics <- function(results) {
  r2_values <- sapply(results, function(x) x$r2)
  c_values <- sapply(results, function(x) x$c_statistic)
  
  data.frame(
    R2_mean = mean(r2_values, na.rm = TRUE),
    R2_sd = sd(r2_values, na.rm = TRUE),
    C_mean = mean(c_values, na.rm = TRUE),
    C_sd = sd(c_values, na.rm = TRUE)
  )
}

metrics <- data.frame(
  Model = paste0("Model ", 1:7),
  do.call(rbind, lapply(all_results, extract_metrics))
)

# Calculate deltas relative to Model 1 (reference)
metrics$R2_delta <- metrics$R2_mean - metrics$R2_mean[1]
metrics$C_delta <- metrics$C_mean - metrics$C_mean[1]

# Save full results
write.csv(metrics, "model_comparison_full_results.csv", row.names = FALSE)
cat("\nFull results saved to: model_comparison_full_results.csv\n")

################################################################################
# 6. CREATE PUBLICATION-READY TABLE
################################################################################

cat("\n=== CREATING PUBLICATION-READY TABLE ===\n")

# Format function
format_metric <- function(mean, sd) {
  sprintf("%.3f", mean)
}

format_delta <- function(delta) {
  sprintf("%.3f", delta)
}

# Create table with requested groupings
pub_table <- data.frame(
  Metric = c("Marginal R²", "C-statistic"),
  
  # Column 1: Model 1 (Reference)
  `Model 1\nFull Model\n(Reference)` = c(
    format_metric(metrics$R2_mean[1], metrics$R2_sd[1]),
    format_metric(metrics$C_mean[1], metrics$C_sd[1])
  ),
  
  # ACQ family
  `Model 2\nACQ Only` = c(
    format_metric(metrics$R2_mean[2], metrics$R2_sd[2]),
    format_metric(metrics$C_mean[2], metrics$C_sd[2])
  ),
  
  `Model 3\nFull Without ACQ` = c(
    format_metric(metrics$R2_mean[3], metrics$R2_sd[3]),
    format_metric(metrics$C_mean[3], metrics$C_sd[3])
  ),
  
  `Δ (Model 3 vs 1)` = c(
    format_delta(metrics$R2_delta[3]),
    format_delta(metrics$C_delta[3])
  ),
  
  # Attack history family
  `Model 4\nAttack History Only` = c(
    format_metric(metrics$R2_mean[4], metrics$R2_sd[4]),
    format_metric(metrics$C_mean[4], metrics$C_sd[4])
  ),
  
  `Model 5\nFull Without\nAttack History` = c(
    format_metric(metrics$R2_mean[5], metrics$R2_sd[5]),
    format_metric(metrics$C_mean[5], metrics$C_sd[5])
  ),
  
  `Δ (Model 5 vs 1)` = c(
    format_delta(metrics$R2_delta[5]),
    format_delta(metrics$C_delta[5])
  ),
  
  # Biomarkers family
  `Model 6\nBiomarkers Only\n(Eos × FeNO)` = c(
    format_metric(metrics$R2_mean[6], metrics$R2_sd[6]),
    format_metric(metrics$C_mean[6], metrics$C_sd[6])
  ),
  
  `Model 7\nFull Without\nBiomarkers` = c(
    format_metric(metrics$R2_mean[7], metrics$R2_sd[7]),
    format_metric(metrics$C_mean[7], metrics$C_sd[7])
  ),
  
  `Δ (Model 7 vs 1)` = c(
    format_delta(metrics$R2_delta[7]),
    format_delta(metrics$C_delta[7])
  ),
  
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# Print table
cat("\n=== PUBLICATION-READY TABLE ===\n")
cat("(Table formatted for easy copying to Excel/Word)\n\n")
print(pub_table, row.names = FALSE)

# Also create a more readable console version
cat("\n\n=== FORMATTED FOR CONSOLE ===\n\n")
cat(sprintf("%-15s", "Metric"))
for(col in names(pub_table)[-1]) {
  cat(sprintf("%-18s", gsub("\n", " ", col)))
}
cat("\n")
cat(paste(rep("-", 15 + 18*10), collapse=""))
cat("\n")
for(i in 1:nrow(pub_table)) {
  cat(sprintf("%-15s", pub_table[i,1]))
  for(j in 2:ncol(pub_table)) {
    cat(sprintf("%-18s", pub_table[i,j]))
  }
  cat("\n")
}

# Save as CSV
write.csv(pub_table, "publication_table_model_comparison.csv", row.names = FALSE)
cat("\nPublication table saved to: publication_table_model_comparison.csv\n")

# Save as HTML
html_file <- "publication_table_model_comparison.html"
sink(html_file)
cat("<!DOCTYPE html>\n<html>\n<head>\n")
cat("<style>\n")
cat("table { border-collapse: collapse; width: 100%; font-family: Arial, sans-serif; }\n")
cat("th, td { border: 1px solid #ddd; padding: 8px; text-align: center; }\n")
cat("th { background-color: #4CAF50; color: white; }\n")
cat("tr:nth-child(even) { background-color: #f2f2f2; }\n")
cat(".delta-col { background-color: #ffffcc; font-weight: bold; }\n")
cat(".reference-col { background-color: #e6f3ff; font-weight: bold; }\n")
cat("</style>\n")
cat("</head>\n<body>\n")
cat("<h2>Comparison of Asthma Exacerbation Prediction Models</h2>\n")
cat("<table>\n")

# Header row
cat("<tr>")
cat("<th>Metric</th>")
for(i in 2:ncol(pub_table)) {
  col_name <- names(pub_table)[i]
  if(grepl("Reference", col_name)) {
    cat(sprintf("<th class='reference-col'>%s</th>", gsub("\n", "<br>", col_name)))
  } else if(grepl("Δ", col_name)) {
    cat(sprintf("<th class='delta-col'>%s</th>", gsub("\n", "<br>", col_name)))
  } else {
    cat(sprintf("<th>%s</th>", gsub("\n", "<br>", col_name)))
  }
}
cat("</tr>\n")

# Data rows
for(i in 1:nrow(pub_table)) {
  cat("<tr>")
  cat(sprintf("<td style='text-align:left; font-weight:bold;'>%s</td>", pub_table[i,1]))
  for(j in 2:ncol(pub_table)) {
    if(grepl("Δ", names(pub_table)[j])) {
      cat(sprintf("<td class='delta-col'>%s</td>", pub_table[i,j]))
    } else if(grepl("Reference", names(pub_table)[j])) {
      cat(sprintf("<td class='reference-col'>%s</td>", pub_table[i,j]))
    } else {
      cat(sprintf("<td>%s</td>", pub_table[i,j]))
    }
  }
  cat("</tr>\n")
}

cat("</table>\n")
cat("<p><em>Note: Values shown as mean across 10 multiply imputed datasets. Δ indicates change compared to Model 1 (Reference).</em></p>\n")
cat("</body>\n</html>\n")
sink()
cat("HTML table saved to: publication_table_model_comparison.html\n")

################################################################################
# 6B. CREATE VERTICAL (TRANSPOSED) TABLE FORMAT
################################################################################

cat("\n=== CREATING VERTICAL TABLE FORMAT ===\n")

# Create vertical table (models as rows, metrics as columns)
vertical_table <- data.frame(
  Model = c(
    "Model 1: Full Model (Reference)",
    "Model 2: ACQ Only",
    "Model 3: Full Without ACQ",
    "Δ (Model 3 vs 1)",
    "Model 4: Attack History Only",
    "Model 5: Full Without Attack History",
    "Δ (Model 5 vs 1)",
    "Model 6: Biomarkers Only (Eos × FeNO)",
    "Model 7: Full Without Biomarkers",
    "Δ (Model 7 vs 1)"
  ),
  
  `Marginal R²` = c(
    format_metric(metrics$R2_mean[1], metrics$R2_sd[1]),
    format_metric(metrics$R2_mean[2], metrics$R2_sd[2]),
    format_metric(metrics$R2_mean[3], metrics$R2_sd[3]),
    format_delta(metrics$R2_delta[3]),
    format_metric(metrics$R2_mean[4], metrics$R2_sd[4]),
    format_metric(metrics$R2_mean[5], metrics$R2_sd[5]),
    format_delta(metrics$R2_delta[5]),
    format_metric(metrics$R2_mean[6], metrics$R2_sd[6]),
    format_metric(metrics$R2_mean[7], metrics$R2_sd[7]),
    format_delta(metrics$R2_delta[7])
  ),
  
  `C-statistic` = c(
    format_metric(metrics$C_mean[1], metrics$C_sd[1]),
    format_metric(metrics$C_mean[2], metrics$C_sd[2]),
    format_metric(metrics$C_mean[3], metrics$C_sd[3]),
    format_delta(metrics$C_delta[3]),
    format_metric(metrics$C_mean[4], metrics$C_sd[4]),
    format_metric(metrics$C_mean[5], metrics$C_sd[5]),
    format_delta(metrics$C_delta[5]),
    format_metric(metrics$C_mean[6], metrics$C_sd[6]),
    format_metric(metrics$C_mean[7], metrics$C_sd[7]),
    format_delta(metrics$C_delta[7])
  ),
  
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# Print vertical table
cat("\n")
print(vertical_table, row.names = FALSE)

# Save vertical table as CSV
write.csv(vertical_table, "publication_table_vertical.csv", row.names = FALSE)
cat("\nVertical table saved to: publication_table_vertical.csv\n")

# Save vertical table as HTML
html_vertical <- "publication_table_vertical.html"
sink(html_vertical)
cat("<!DOCTYPE html>\n<html>\n<head>\n")
cat("<style>\n")
cat("table { border-collapse: collapse; width: 100%; font-family: Arial, sans-serif; }\n")
cat("th, td { border: 1px solid #ddd; padding: 10px; }\n")
cat("th { background-color: #4CAF50; color: white; text-align: center; }\n")
cat("td:first-child { text-align: left; font-weight: 500; }\n")
cat("td:not(:first-child) { text-align: center; }\n")
cat(".reference-row { background-color: #e6f3ff; font-weight: bold; }\n")
cat(".delta-row { background-color: #ffffcc; font-weight: bold; font-style: italic; }\n")
cat(".family-header { background-color: #f0f0f0; font-weight: bold; border-top: 2px solid #333; }\n")
cat("tr:nth-child(even):not(.delta-row):not(.reference-row):not(.family-header) { background-color: #f9f9f9; }\n")
cat("</style>\n")
cat("</head>\n<body>\n")
cat("<h2>Asthma Exacerbation Prediction Models - Vertical Format</h2>\n")
cat("<table>\n")

# Header
cat("<tr>")
for(col_name in names(vertical_table)) {
  cat(sprintf("<th>%s</th>", col_name))
}
cat("</tr>\n")

# Data rows with conditional formatting
for(i in 1:nrow(vertical_table)) {
  model_name <- vertical_table[i,1]
  
  if(grepl("Reference", model_name)) {
    cat("<tr class='reference-row'>")
  } else if(grepl("^Δ", model_name)) {
    cat("<tr class='delta-row'>")
  } else if(grepl("Model [246]:", model_name)) {
    # Add subtle separator before single predictor models
    cat("<tr style='border-top: 1px solid #ccc;'>")
  } else {
    cat("<tr>")
  }
  
  for(j in 1:ncol(vertical_table)) {
    cat(sprintf("<td>%s</td>", vertical_table[i,j]))
  }
  cat("</tr>\n")
}

cat("</table>\n")
cat("<h3>Model Groupings:</h3>\n")
cat("<ul>\n")
cat("<li><strong>Reference Model:</strong> Model 1 (Full model with all predictors)</li>\n")
cat("<li><strong>ACQ Family:</strong> Models 2-3 evaluate ACQ contribution</li>\n")
cat("<li><strong>Attack History Family:</strong> Models 4-5 evaluate attack history contribution</li>\n")
cat("<li><strong>Biomarkers Family:</strong> Models 6-7 evaluate biomarker contribution</li>\n")
cat("</ul>\n")
cat("<p><em>Note: Values shown as mean across 10 multiply imputed datasets. Δ indicates change compared to Model 1.</em></p>\n")
cat("</body>\n</html>\n")
sink()
cat("Vertical HTML table saved to: publication_table_vertical.html\n")

################################################################################
# 7. MODEL DESCRIPTIONS
################################################################################

cat("\n")
cat("================================================================================\n")
cat("MODEL DESCRIPTIONS\n")
cat("================================================================================\n")
cat("\n")
cat("Model 1 (Full Model - Reference):\n")
cat("  ACQ + attack history + treatment step + FEV1% + biomarkers (Eos × FeNO) +\n")
cat("  BMI + sex + smoking + FEV1/FVC + CRSsNP + CRSwNP + allergic rhinitis\n")
cat("  All models adjusted for trial (random intercept)\n")
cat("\n")
cat("Model 2 (ACQ Only):\n")
cat("  ACQ baseline score only\n")
cat("\n")
cat("Model 3 (Full Without ACQ):\n")
cat("  Same as Model 1, excluding ACQ\n")
cat("\n")
cat("Model 4 (Attack History Only):\n")
cat("  Previous severe attacks (binary) only\n")
cat("\n")
cat("Model 5 (Full Without Attack History):\n")
cat("  Same as Model 1, excluding attack history\n")
cat("\n")
cat("Model 6 (Biomarkers Only):\n")
cat("  Blood eosinophils × FeNO interaction only\n")
cat("\n")
cat("Model 7 (Full Without Biomarkers):\n")
cat("  Same as Model 1, excluding biomarkers (Eos × FeNO)\n")
cat("\n")
cat("================================================================================\n")
cat("METRICS\n")
cat("================================================================================\n")
cat("\n")
cat("Marginal R²: Variance in exacerbations explained by fixed effects only\n")
cat("C-statistic: Discrimination for predicting any vs no attacks (binary outcome)\n")
cat("Δ (Delta): Change in metric compared to Model 1 (Full Model)\n")
cat("  - Negative delta = worse performance than full model\n")
cat("  - Models 3, 5, 7 expected to have negative deltas\n")
cat("\n")

################################################################################
# 8. INTERPRETATION SUMMARY
################################################################################

cat("\n")
cat("================================================================================\n")
cat("INTERPRETATION OF DELTAS\n")
cat("================================================================================\n")
cat("\n")

# ACQ contribution
acq_contribution_r2 <- abs(metrics$R2_delta[3])
acq_contribution_c <- abs(metrics$C_delta[3])

cat("ACQ Contribution (Model 1 vs Model 3):\n")
cat(sprintf("  - Removing ACQ decreases R² by: %.4f\n", acq_contribution_r2))
cat(sprintf("  - Removing ACQ decreases C-statistic by: %.4f\n", acq_contribution_c))
cat("\n")

# Attack history contribution
attack_contribution_r2 <- abs(metrics$R2_delta[5])
attack_contribution_c <- abs(metrics$C_delta[5])

cat("Attack History Contribution (Model 1 vs Model 5):\n")
cat(sprintf("  - Removing attack history decreases R² by: %.4f\n", attack_contribution_r2))
cat(sprintf("  - Removing attack history decreases C-statistic by: %.4f\n", attack_contribution_c))
cat("\n")

# Biomarkers contribution
biomarker_contribution_r2 <- abs(metrics$R2_delta[7])
biomarker_contribution_c <- abs(metrics$C_delta[7])

cat("Biomarkers Contribution (Model 1 vs Model 7):\n")
cat(sprintf("  - Removing biomarkers decreases R² by: %.4f\n", biomarker_contribution_r2))
cat(sprintf("  - Removing biomarkers decreases C-statistic by: %.4f\n", biomarker_contribution_c))
cat("\n")

# Relative importance
cat("Relative Importance (based on R² decrease):\n")
contributions <- data.frame(
  Predictor = c("ACQ", "Attack History", "Biomarkers"),
  R2_contribution = c(acq_contribution_r2, attack_contribution_r2, biomarker_contribution_r2)
)
contributions <- contributions[order(-contributions$R2_contribution), ]

for(i in 1:nrow(contributions)) {
  cat(sprintf("  %d. %s: %.4f\n", i, contributions$Predictor[i], contributions$R2_contribution[i]))
}

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("\nFiles saved:\n")
cat("  - model_comparison_full_results.csv (all metrics for all models)\n")
cat("  - publication_table_model_comparison.csv (horizontal format)\n")
cat("  - publication_table_model_comparison.html (horizontal format - HTML)\n")
cat("  - publication_table_vertical.csv (vertical format)\n")
cat("  - publication_table_vertical.html (vertical format - HTML)\n")
