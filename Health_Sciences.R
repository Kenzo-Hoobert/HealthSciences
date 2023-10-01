library(ggplot2)
library(knitr)
library(clipr)
library(scales)
library(maps)
library(plotly)
library(sf)
library(gridExtra)
library(patchwork)
library(ggthemes)
library(tidyverse)
library(cowplot)
library(kableExtra)
library(RColorBrewer)
library(rsample)
library(MASS)
library(caret)
library(doParallel)
library(randomForest)
library(tidymodels)
library(mice)
library(xgboost)
library(yardstick)
library(parsnip)
library(kernlab)
library(ranger)
library(modeltime)
library(vip)
library(dplyr)

# Cleaned dataframe
df <- read_csv("C:\\Users\\datte\\Downloads\\HS_OneHotEncoding (1).csv", col_names = FALSE)
df$X1 <- NULL
names <- as.character(df[1,])
names(df) <- names
df[-1, ]

our_df <- df[c(10, 13:18, 22, 51:218)]
our_df

for (x in c(1:7)){
  for (value in our_df[x]){
    x_nonum <- which(is.na(suppressWarnings(as.numeric(value))))
    print(x_nonum)
  }
}

for (x in c(1:175)){
  our_df[x] <- suppressWarnings(as.numeric(as.character(unlist(our_df[[x]]))))
}

for (x in c(1:175)){
  print(sum(is.na(unlist(our_df[x]))))
}

for (x in c(1:175)){
  print(class(unlist(our_df[x])))
}

our_df <- drop_na(our_df)

# Linear Modeling (Hopefully)
set.seed(123)
splits <- initial_split(our_df %>% drop_na())
train <- training(splits)
test <- testing(splits)

lm <- lm(number_diagnoses ~ time_in_hospital +
            num_lab_procedures +
            num_procedures +
            num_medications +
            number_outpatient +
            number_emergency +
            number_inpatient +
           `Caucasian_2` +
           `AfricanAmerican_2` +
           `?_2` +
           `Other_2` +
           `Asian_2` +
           `Hispanic_2` +
           `Female_3` +
           `Male_3` +
           `Unknown/Invalid_3` +
           `[0-10)_4` +
           `[10-20)_4` +
           `[20-30)_4` +
           `[30-40)_4` +
           `[40-50)_4` +
           `[50-60)_4` +
           `[60-70)_4` +
           `[70-80)_4` +
           `[80-90)_4` +
           `[90-100)_4` +
           `6_6` +
           `1_6` +
           `2_6` +
           `3_6` +
           `4_6` +
           `5_6` +
           `8_6` +
           `7_6` +
           `25_7` +
           `1_7` +
           `3_7` +
           `6_7` +
           `2_7` +
           `5_7` +
           `11_7` +
           `7_7` +
           `10_7` +
           `4_7` +
           `14_7` +
           `18_7` +
           `8_7` +
           `13_7` +
           `12_7` +
           `16_7` +
           `17_7` +
           `22_7` +
           `23_7` +
           `9_7` +
           `20_7` +
           `15_7` +
           `24_7` +
           `28_7` +
           `19_7` +
           `27_7` +
           `1_8` +
           `7_8` +
           `2_8` +
           `4_8` +
           `5_8` +
           `6_8` +
           `20_8` +
           `3_8` +
           `17_8` +
           `8_8` +
           `9_8` +
           `14_8` +
           `10_8` +
           `22_8` +
           `11_8` +
           `25_8` +
           `13_8` +
           `?_10` +
           `MC_10` +
           `MD_10` +
           `HM_10` +
           `UN_10` +
           `BC_10` +
           `SP_10` +
           `CP_10` +
           `SI_10` +
           `DM_10` +
           `CM_10` +
           `CH_10` +
           `PO_10` +
           `WC_10` +
           `OT_10` +
           `OG_10` +
           `MP_10` +
           `FR_10` +
           `Pediatrics-Endocrinology_11` +
           `?_11` +
           `InternalMedicine_11` +
           `Family/GeneralPractice_11` +
           `Cardiology_11` +
           `Surgery-General_11` +
           `Orthopedics_11` +
           `Gastroenterology_11` +
           `Surgery-Cardiovascular/Thoracic_11` +
           `Nephrology_11` +
           `Orthopedics-Reconstructive_11` +
           `Psychiatry_11` +
           `Emergency/Trauma_11` +
           `Pulmonology_11` +
           `Surgery-Neuro_11` +
           `Obsterics&Gynecology-GynecologicOnco_11` +
           `ObstetricsandGynecology_11` +
           `Pediatrics_11` +
           `Hematology/Oncology_11` +
           `Otolaryngology_11` +
           `Surgery-Colon&Rectal_11` +
           `Pediatrics-CriticalCare_11` +
           `Endocrinology_11` +
           `Urology_11` +
           `Psychiatry-Child/Adolescent_11` +
           `Pediatrics-Pulmonology_11` +
           `Neurology_11` +
           `Anesthesiology-Pediatric_11` +
           `Radiology_11` +
           `Pediatrics-Hematology-Oncology_11` +
           `Psychology_11` +
           `Podiatry_11` +
           `Gynecology_11` +
           `Oncology_11` +
           `Pediatrics-Neurology_11` +
           `Surgery-Plastic_11` +
           `Surgery-Thoracic_11` +
           `Surgery-PlasticwithinHeadandNeck_11` +
           `Ophthalmology_11` +
           `Surgery-Pediatric_11` +
           `Pediatrics-EmergencyMedicine_11` +
           `PhysicalMedicineandRehabilitation_11` +
           `InfectiousDiseases_11` +
           `Anesthesiology_11` +
           `Rheumatology_11` +
           `AllergyandImmunology_11` +
           `Surgery-Maxillofacial_11` +
           `Pediatrics-InfectiousDiseases_11` +
           `Pediatrics-AllergyandImmunology_11` +
           `Dentistry_11` +
           `Surgeon_11` +
           `Surgery-Vascular_11` +
           `Osteopath_11` +
           `Psychiatry-Addictive_11` +
           `Surgery-Cardiovascular_11` +
           `PhysicianNotFound_11` +
           `Hematology_11` +
           `Proctology_11` +
           `Obstetrics_11` +
           `SurgicalSpecialty_11` +
           `Radiologist_11` +
           `Pathology_11` +
           `Dermatology_11` +
           `SportsMedicine_11` +
           `Speech_11` +
           `Hospitalist_11` +
           `OutreachServices_11` +
           `Cardiology-Pediatric_11` +
           `Perinatology_11` +
           `Neurophysiology_11` +
           `Endocrinology-Metabolism_11` +
           `DCPTEAM_11` +
           `Resident_11` +
           `No_47` +
           `Ch_47` +
           `No_48` +
           `Yes_48` +
           `NO_49` +
           `>30_49` +
           `<30_49`,
         data = train)

drop_na(df)

lm_preds <- predict(lm, test)

lm_train <- predict(lm, train)

lm_rmse_train <- sqrt(mean((train$lastSoldPricelog - lm_train)^2, na.rm = T))
lm_rmse_train
lm_r2_train <- R2(pred = lm_train, obs = train$lastSoldPricelog, na.rm = T)
lm_r2_train

lm_rmse_train_text <- sqrt(mean((train$lastSoldPricelog - lm_train)^2, na.rm = T))
lm_rmse_train_text
lm_r2_train_text <- R2(pred = lm_train, obs = train$lastSoldPricelog, na.rm = T)
lm_r2_train_text

#bhc results
lm_rmse_basic <- sqrt(mean((test$lastSoldPricelog - lm_preds)^2, na.rm = T))
lm_rmse_basic
lm_r2_basic <- R2(pred = lm_preds, obs = test$lastSoldPricelog, na.rm = T)
lm_r2_basic

#text results
lm_rmse_text <- sqrt(mean((test$lastSoldPricelog - lm_preds)^2, na.rm = T))
lm_rmse_text
lm_r2_text <- R2(pred = lm_preds, obs = test$lastSoldPricelog, na.rm = T)
lm_r2_text

#full results
lm_rmse_full <- sqrt(mean((test$number_diagnoses - lm_preds)^2, na.rm = T))
lm_rmse_full
lm_r2_full <- R2(pred = lm_preds, obs = test$number_diagnoses, na.rm = T)
lm_r2_full

summary(lm)

# ALt method
lm.aic <- stepAIC(lm)
summary(lm.aic)


# Random forest -----------------------------------------------------------------------
rf <- rand_forest(mode = "regression", engine = "randomForest", trees = 500, min_n = 8, mtry = 4) 

rf_wf <- workflow() %>% 
  add_model(rf) %>% 
  add_formula(number_diagnoses ~ time_in_hospital +
                num_lab_procedures +
                num_procedures +
                num_medications +
                number_outpatient +
                number_emergency +
                number_inpatient +
                `Caucasian_2` +
                `AfricanAmerican_2` +
                `?_2` +
                `Other_2` +
                `Asian_2` +
                `Hispanic_2` +
                `Female_3` +
                `Male_3` +
                `Unknown/Invalid_3` +
                `[0-10)_4` +
                `[10-20)_4` +
                `[20-30)_4` +
                `[30-40)_4` +
                `[40-50)_4` +
                `[50-60)_4` +
                `[60-70)_4` +
                `[70-80)_4` +
                `[80-90)_4` +
                `[90-100)_4` +
                `6_6` +
                `1_6` +
                `2_6` +
                `3_6` +
                `4_6` +
                `5_6` +
                `8_6` +
                `7_6` +
                `25_7` +
                `1_7` +
                `3_7` +
                `6_7` +
                `2_7` +
                `5_7` +
                `11_7` +
                `7_7` +
                `10_7` +
                `4_7` +
                `14_7` +
                `18_7` +
                `8_7` +
                `13_7` +
                `12_7` +
                `16_7` +
                `17_7` +
                `22_7` +
                `23_7` +
                `9_7` +
                `20_7` +
                `15_7` +
                `24_7` +
                `28_7` +
                `19_7` +
                `27_7` +
                `1_8` +
                `7_8` +
                `2_8` +
                `4_8` +
                `5_8` +
                `6_8` +
                `20_8` +
                `3_8` +
                `17_8` +
                `8_8` +
                `9_8` +
                `14_8` +
                `10_8` +
                `22_8` +
                `11_8` +
                `25_8` +
                `13_8` +
                `?_10` +
                `MC_10` +
                `MD_10` +
                `HM_10` +
                `UN_10` +
                `BC_10` +
                `SP_10` +
                `CP_10` +
                `SI_10` +
                `DM_10` +
                `CM_10` +
                `CH_10` +
                `PO_10` +
                `WC_10` +
                `OT_10` +
                `OG_10` +
                `MP_10` +
                `FR_10` +
                `Pediatrics-Endocrinology_11` +
                `?_11` +
                `InternalMedicine_11` +
                `Family/GeneralPractice_11` +
                `Cardiology_11` +
                `Surgery-General_11` +
                `Orthopedics_11` +
                `Gastroenterology_11` +
                `Surgery-Cardiovascular/Thoracic_11` +
                `Nephrology_11` +
                `Orthopedics-Reconstructive_11` +
                `Psychiatry_11` +
                `Emergency/Trauma_11` +
                `Pulmonology_11` +
                `Surgery-Neuro_11` +
                `Obsterics&Gynecology-GynecologicOnco_11` +
                `ObstetricsandGynecology_11` +
                `Pediatrics_11` +
                `Hematology/Oncology_11` +
                `Otolaryngology_11` +
                `Surgery-Colon&Rectal_11` +
                `Pediatrics-CriticalCare_11` +
                `Endocrinology_11` +
                `Urology_11` +
                `Psychiatry-Child/Adolescent_11` +
                `Pediatrics-Pulmonology_11` +
                `Neurology_11` +
                `Anesthesiology-Pediatric_11` +
                `Radiology_11` +
                `Pediatrics-Hematology-Oncology_11` +
                `Psychology_11` +
                `Podiatry_11` +
                `Gynecology_11` +
                `Oncology_11` +
                `Pediatrics-Neurology_11` +
                `Surgery-Plastic_11` +
                `Surgery-Thoracic_11` +
                `Surgery-PlasticwithinHeadandNeck_11` +
                `Ophthalmology_11` +
                `Surgery-Pediatric_11` +
                `Pediatrics-EmergencyMedicine_11` +
                `PhysicalMedicineandRehabilitation_11` +
                `InfectiousDiseases_11` +
                `Anesthesiology_11` +
                `Rheumatology_11` +
                `AllergyandImmunology_11` +
                `Surgery-Maxillofacial_11` +
                `Pediatrics-InfectiousDiseases_11` +
                `Pediatrics-AllergyandImmunology_11` +
                `Dentistry_11` +
                `Surgeon_11` +
                `Surgery-Vascular_11` +
                `Osteopath_11` +
                `Psychiatry-Addictive_11` +
                `Surgery-Cardiovascular_11` +
                `PhysicianNotFound_11` +
                `Hematology_11` +
                `Proctology_11` +
                `Obstetrics_11` +
                `SurgicalSpecialty_11` +
                `Radiologist_11` +
                `Pathology_11` +
                `Dermatology_11` +
                `SportsMedicine_11` +
                `Speech_11` +
                `Hospitalist_11` +
                `OutreachServices_11` +
                `Cardiology-Pediatric_11` +
                `Perinatology_11` +
                `Neurophysiology_11` +
                `Endocrinology-Metabolism_11` +
                `DCPTEAM_11` +
                `Resident_11` +
                `No_47` +
                `Ch_47` +
                `No_48` +
                `Yes_48` +
                `NO_49` +
                `>30_49` +
                `<30_49`)

folds <- vfold_cv(train)

rf_tune <- tuneRF(x = train %>% dplyr::select(`time_in_hospital`,
                                              `num_lab_procedures`,
                                              `num_procedures`,
                                              `num_medications`,
                                              `number_outpatient`,
                                              `number_emergency`,
                                              `number_inpatient`,
                                              `Caucasian_2`,
                                              `AfricanAmerican_2`,
                                              `?_2`,
                                              `Other_2`,
                                              `Asian_2`,
                                              `Hispanic_2`,
                                              `Female_3`,
                                              `Male_3`,
                                              `Unknown/Invalid_3`,
                                              `[0-10)_4`,
                                              `[10-20)_4`,
                                              `[20-30)_4`,
                                              `[30-40)_4`,
                                              `[40-50)_4`,
                                              `[50-60)_4`,
                                              `[60-70)_4`,
                                              `[70-80)_4`,
                                              `[80-90)_4`,
                                              `[90-100)_4`,
                                              `6_6`,
                                              `1_6`,
                                              `2_6`,
                                              `3_6`,
                                              `4_6`,
                                              `5_6`,
                                              `8_6`,
                                              `7_6`,
                                              `25_7`,
                                              `1_7`,
                                              `3_7`,
                                              `6_7`,
                                              `2_7`,
                                              `5_7`,
                                              `11_7`,
                                              `7_7`,
                                              `10_7`,
                                              `4_7`,
                                              `14_7`,
                                              `18_7`,
                                              `8_7`,
                                              `13_7`,
                                              `12_7`,
                                              `16_7`,
                                              `17_7`,
                                              `22_7`,
                                              `23_7`,
                                              `9_7`,
                                              `20_7`,
                                              `15_7`,
                                              `24_7`,
                                              `28_7`,
                                              `19_7`,
                                              `27_7`,
                                              `1_8`,
                                              `7_8`,
                                              `2_8`,
                                              `4_8`,
                                              `5_8`,
                                              `6_8`,
                                              `20_8`,
                                              `3_8`,
                                              `17_8`,
                                              `8_8`,
                                              `9_8`,
                                              `14_8`,
                                              `10_8`,
                                              `22_8`,
                                              `11_8`,
                                              `25_8`,
                                              `13_8`,
                                              `?_10`,
                                              `MC_10`,
                                              `MD_10`,
                                              `HM_10`,
                                              `UN_10`,
                                              `BC_10`,
                                              `SP_10`,
                                              `CP_10`,
                                              `SI_10`,
                                              `DM_10`,
                                              `CM_10`,
                                              `CH_10`,
                                              `PO_10`,
                                              `WC_10`,
                                              `OT_10`,
                                              `OG_10`,
                                              `MP_10`,
                                              `FR_10`,
                                              `Pediatrics-Endocrinology_11`,
                                              `?_11`,
                                              `InternalMedicine_11`,
                                              `Family/GeneralPractice_11`,
                                              `Cardiology_11`,
                                              `Surgery-General_11`,
                                              `Orthopedics_11`,
                                              `Gastroenterology_11`,
                                              `Surgery-Cardiovascular/Thoracic_11`,
                                              `Nephrology_11`,
                                              `Orthopedics-Reconstructive_11`,
                                              `Psychiatry_11`,
                                              `Emergency/Trauma_11`,
                                              `Pulmonology_11`,
                                              `Surgery-Neuro_11`,
                                              `Obsterics&Gynecology-GynecologicOnco_11`,
                                              `ObstetricsandGynecology_11`,
                                              `Pediatrics_11`,
                                              `Hematology/Oncology_11`,
                                              `Otolaryngology_11`,
                                              `Surgery-Colon&Rectal_11`,
                                              `Pediatrics-CriticalCare_11`,
                                              `Endocrinology_11`,
                                              `Urology_11`,
                                              `Psychiatry-Child/Adolescent_11`,
                                              `Pediatrics-Pulmonology_11`,
                                              `Neurology_11`,
                                              `Anesthesiology-Pediatric_11`,
                                              `Radiology_11`,
                                              `Pediatrics-Hematology-Oncology_11`,
                                              `Psychology_11`,
                                              `Podiatry_11`,
                                              `Gynecology_11`,
                                              `Oncology_11`,
                                              `Pediatrics-Neurology_11`,
                                              `Surgery-Plastic_11`,
                                              `Surgery-Thoracic_11`,
                                              `Surgery-PlasticwithinHeadandNeck_11`,
                                              `Ophthalmology_11`,
                                              `Surgery-Pediatric_11`,
                                              `Pediatrics-EmergencyMedicine_11`,
                                              `PhysicalMedicineandRehabilitation_11`,
                                              `InfectiousDiseases_11`,
                                              `Anesthesiology_11`,
                                              `Rheumatology_11`,
                                              `AllergyandImmunology_11`,
                                              `Surgery-Maxillofacial_11`,
                                              `Pediatrics-InfectiousDiseases_11`,
                                              `Pediatrics-AllergyandImmunology_11`,
                                              `Dentistry_11`,
                                              `Surgeon_11`,
                                              `Surgery-Vascular_11`,
                                              `Osteopath_11`,
                                              `Psychiatry-Addictive_11`,
                                              `Surgery-Cardiovascular_11`,
                                              `PhysicianNotFound_11`,
                                              `Hematology_11`,
                                              `Proctology_11`,
                                              `Obstetrics_11`,
                                              `SurgicalSpecialty_11`,
                                              `Radiologist_11`,
                                              `Pathology_11`,
                                              `Dermatology_11`,
                                              `SportsMedicine_11`,
                                              `Speech_11`,
                                              `Hospitalist_11`,
                                              `OutreachServices_11`,
                                              `Cardiology-Pediatric_11`,
                                              `Perinatology_11`,
                                              `Neurophysiology_11`,
                                              `Endocrinology-Metabolism_11`,
                                              `DCPTEAM_11`,
                                              `Resident_11`,
                                              `No_47`,
                                              `Ch_47`,
                                              `No_48`,
                                              `Yes_48`,
                                              `NO_49`,
                                              `>30_49`,
                                              `<30_49`),
                  y = train$number_diagnoses, mtryStart = 1, ntreeTry = 30, stepFactor = 2,
                  improve = .001, doBest = T, plot = T, trace = T)
rf_tune

rf_wf <- rf_wf %>% fit(train)

rf_preds <- predict(rf_wf, test) %>% 
  unlist()

test %>% 
  ggplot(aes(x = number_diagnoses))+
  geom_abline(slope = 0.2, intercept = 5.5, aes(linetype= 1))+
  geom_point(aes(y = rf_preds)) +
  labs(x = "Number Diagnoses", y = "prediction", title = "Random Forest")

# Create the variable importance plot
rf_model <- extract_fit_parsnip(rf_wf)
vip(rf_model, num_features = 15 ,geom = "point") +
  labs(title = "Variable Importance Plot")

rf_rmse_full <- RMSE(pred = rf_preds, obs = test$number_diagnoses)
rf_rmse_full
rf_r2_full <- R2(pred = rf_preds, obs = test$number_diagnoses)
rf_r2_full




