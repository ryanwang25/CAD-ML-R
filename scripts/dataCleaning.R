library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)
library(foreign)
library(tidyverse)
library(reshape2)
library(rsample)
library(caret)
library(survey)


##### DEMOGRAPHICS #####

demographics <- read.csv("data/data.csv")

demog_vec <- c("Gender (Male: 1, Female :2)" = "RIAGENDR", 
                                "Age at screening (yrs)" = "RIDAGEYR", 
                                "Race" = "RIDRETH3",
                                "Country of birth" = "DMDBORN4",
                                "interview weight" = "WTINTPRP",
                                "psu" = "SDMVPSU",
                                "strata" = "SDMVSTRA"
  
)

existing_demog_vec <- demog_vec[demog_vec %in% names(demographics)]

if (length(existing_demog_vec) >0) {
  demographics <- rename(demographics, !!!existing_demog_vec)
}

demog_exclude <- c("SDDSRVYR", "RIDSTATR", "RIDAGEMN", "RIDRETH1", "RIDEXMON", "RIDEXPRG", 
                                   "SIALANG", "SIAPROXY", "FIALANG", "SIAINTRP", "FIAPROXY", "MIALANG", "MIAPROXY",
                                   "MIAINTRP", "AIALANGA", "WTMECPRP", "INDFMPIR",
                                   "DMDYRUSZ", "DMDMARTZ", "DMDEDUC2", "FIAINTRP")

existing_demog_exclude <- intersect(demog_exclude, 
                                           names(demographics))
demographics <- select(demographics, -all_of(existing_demog_exclude))



demographics <- clean_names(demographics, "snake")



demographics <- filter(demographics, country_of_birth < 77)


##### HDL CHOLESTEROL ######

hdl_cholesterol <- read.xport("ryan/datasets/P_HDL.XPT")

hdl_cholesterol <- hdl_cholesterol |> rename("Direct HDL-Cholesterol (mg/dL)" = "LBDHDD") |>
  select(-LBDHDDSI)

hdl_cholesterol <- clean_names(hdl_cholesterol, "snake")


##### LDL CHOLESTEROL #####

ldl <- read.xport("ryan/datasets/P_TRIGLY.XPT") # subsample weights

ldl_vec <- c("Fasting Subsample Weight" = "WTSAFPRP", "Triglyceride (mg/dL)" = "LBXTR",
             "LDL-Cholesterol (mg/dL)" = "LBDLDL")

existing_ldl_vec <- ldl_vec[ldl_vec %in%
                              names(ldl)]
if (length(existing_ldl_vec) > 0) {
  ldl <- rename(ldl, !!!existing_ldl_vec)
}


ldl_exclude <- c("LBDLDLSI", "LBDLDLM", "LBDLDMSI", 
                  "LBDLDLN", "LBDLDNSI")


existing_ldl_exclude <- intersect(ldl_exclude, names(ldl))

ldl <- select(ldl, -all_of(existing_ldl_exclude))


ldl <- clean_names(ldl, "snake")


##### EXERCISE ACTIVITY ######

exerciseActivity <- read.xport("ryan/datasets/P_PAQ.XPT")

exercise_vec <- 
  c("days Vigorous Work (weekly)" = "PAQ610",
         "Vigorous Work Activity?" = "PAQ605",
         "Moderate Work Activity?" = "PAQ620",
         "Daily Vigorous Activity (min)" = "PAD615",
         "Days of Moderate Work (weekly)" = "PAQ625",
         "Moderate Work a Day (min)" = "PAD630",
         "Walk or bike?" = "PAQ635",
         "Days Walk or Bike (weekly)" = "PAQ640",
         "Min Sedentary Activities" = "PAD680") 

existing_exercise_vec <- exercise_vec[exercise_vec %in% names(exerciseActivity)]

if (length(existing_exercise_vec) > 0) {
  exerciseActivity <- rename(exerciseActivity, !!!existing_exercise_vec)
}

exercise_exclude <- c("PAQ655", "PAQ670", "PAD675",
"PAQ650", "PAQ665", "PAD660", "PAD645")

existing_exercise_exclude <- intersect(exercise_exclude, names(exerciseActivity))

exerciseActivity <- select(exerciseActivity, -all_of(existing_exercise_exclude))


exerciseActivity <- clean_names(exerciseActivity, "snake")


# Columns to replace NA with 0
cols_replace_na_with_0 <- c("daily_vigorous_activity_min", 
                               "moderate_work_a_day_min", 
                               "days_of_moderate_work_weekly", 
                               "days_vigorous_work_weekly", 
                               "days_walk_or_bike_weekly", 
                               "min_sedentary_activities")

# Columns to replace NA with 2
cols_replace_na_with_2 <- c("vigorous_work_activity", 
                               "moderate_work_activity", 
                               "walk_or_bike")

# Loop for replacing NA with 0
for (col in cols_replace_na_with_0) {
  exerciseActivity[[col]] <- replace_na(exerciseActivity[[col]], 0)
}

# Loop for replacing NA with 2
for (col in cols_replace_na_with_2) {
  exerciseActivity[[col]] <- replace_na(exerciseActivity[[col]], 2)
}


exerciseActivity <- filter(exerciseActivity,
  days_of_moderate_work_weekly < 99 &
    daily_vigorous_activity_min < 7000 &
    days_vigorous_work_weekly < 8 &
    days_walk_or_bike_weekly < 8 &
    min_sedentary_activities < 7000 &
    moderate_work_a_day_min < 7000 &
    moderate_work_activity < 3 &
    vigorous_work_activity < 3 &
    walk_or_bike < 3
)

##### BLOODPRESSURE #####

bloodpressure <- read.xport("ryan/datasets/P_BPXO.XPT")

bp_vec <- c("Systolic (1st reading)" = "BPXOSY1",
                   "Diastolic (1st reading)" = "BPXODI1",
                   "Systolic (2nd reading)" = "BPXOSY2",
                   "Diastolic (2nd reading)" = "BPXODI2",
                   "Systolic (3rd reading)" = "BPXOSY3",
                   "Diastolic (3rd reading)" = "BPXODI3",
                   "Pulse (1st reading)" = "BPXOPLS1",
                   "Pulse (2nd reading)" = "BPXOPLS2",
                   "Pulse (3rd reading)" = "BPXOPLS3")

existing_bp_vec <- bp_vec[bp_vec %in% names(bloodpressure)]

if (length(existing_bp_vec) >0) {
  bloodpressure <- rename(bloodpressure, !!!existing_bp_vec)
}

bp_exclude <- c("BPAOARM", "BPAOCSZ");
existing_bp_exclude <- intersect(bp_exclude, names(bloodpressure))

bloodpressure <- select(bloodpressure, -all_of(existing_bp_exclude))

bloodpressure <- clean_names(bloodpressure, "snake")

column_means <- bloodpressure %>%
  summarise(across(everything(), ~mean(., na.rm = TRUE)))

# Replace NA values with column means
bloodpressure <- bloodpressure %>%
  mutate(across(everything(),~replace_na(., mean(., na.rm = TRUE))))

bloodpressure <- mutate(bloodpressure, 
                        "mean_systolic" = (systolic_1st_reading + systolic_2nd_reading +systolic_3rd_reading)/3,
                        "mean_diastolic" = (diastolic_1st_reading + diastolic_2nd_reading + diastolic_3rd_reading) / 3,
                        "mean_pulse" = (pulse_1st_reading + pulse_2nd_reading + pulse_3rd_reading) / 3)

bloodpressure <- select(bloodpressure,
                        -pulse_1st_reading, -pulse_2nd_reading, -pulse_3rd_reading,
                        -systolic_1st_reading, -systolic_2nd_reading, -systolic_3rd_reading,
                        -diastolic_3rd_reading,-diastolic_2nd_reading, -diastolic_1st_reading)

##### MEDICAL CONDITIONS #####

medicalConditions <- read.xport("ryan/datasets/P_MCQ.XPT")

med_vec <- c("told you have asthma" = "MCQ010",
             "Taking treatment anemia" = "MCQ053",
             "had coronary heart disease?" = "MCQ160C",
             "had angina?" = "MCQ160D",
             "had heart attack" ="MCQ160E",
             "had thyroid problem" = "MCQ160M",
             "had COPD, emphysema, ChB" = "MCQ160P",
             "Close relative had asthma?" = "MCQ300B",
             "Close relative had heart attack?" = "MCQ300A",
             "told to increase exercise?" = "MCQ366B",
             "told to reduce salt?" = "MCQ366C",
             "told to reduce calories?" = "MCQ366D",
             "had congestive heart failure" = "MCQ160B"
  
)


existing_med_vec <- med_vec[med_vec %in% names(medicalConditions)]

if (length(existing_med_vec) > 0) {
  medicalConditions <- rename(medicalConditions, !!!existing_med_vec)
  
}

med_exclude <- c("MCQ025","MCQ035", "MCQ040", "MCQ050",  "AGQ030", "MCQ092", "MCD093", 
                  "MCQ149", "MCQ151", "RHD018", "MCQ160A", "MCQ195", "MCQ160F", "MCD180F",
                  "MCQ160L", "MCQ170L","MCD180L", "MCQ500", "MCQ510A", "MCQ510B", "MCQ510C",
                  "MCQ510D", "MCQ510E", "MCQ510F", "MCQ520", "MCQ530", "MCQ540", "MCQ550", "MCQ560",
                  "MCQ570", "MCQ220", "MCQ230A", "MCQ230B", "MCQ230C", "MCQ230D", "MCQ300C", "MCQ366A",
                  "OSQ230", "MCQ080", "MCQ170M", "MCQ371A", "MCQ371C", "MCQ371D", "MCQ371B", "MCD180M",
                  "MCD180D", "MCD180C", "MCD180B", "MCD180E")
     
existing_med_exclude <- intersect(med_exclude, names(medicalConditions))

medicalConditions <- select(medicalConditions, -all_of(existing_med_exclude))
                 
medicalConditions <- clean_names(medicalConditions, "snake")

med_cols_to_update <- c(
  "close_relative_had_asthma",
  "close_relative_had_heart_attack",
  "had_angina",
  "had_congestive_heart_failure",
  "had_copd_emphysema_ch_b",
  "had_coronary_heart_disease",
  "had_heart_attack",
  "had_thyroid_problem",
  "taking_treatment_anemia",
  "told_to_increase_exercise",
  "told_to_reduce_calories",
  "told_to_reduce_salt",
  "told_you_have_asthma"
)

for (col in med_cols_to_update) {
  medicalConditions[[col]] <- replace_na(medicalConditions[[col]], 2)
}

 medicalConditions <- filter(medicalConditions, if_all(!matches("seqn"), ~. <=3))


##### WEIGHT #####

weight <- read.xport("ryan/datasets/P_WHQ.XPT")

weight <- weight |> filter(WHD010 < 7777 & WHD020 < 7777) |>
  select(SEQN, WHD010, WHD020) |>
  mutate(BMI = 703 * WHD020 / (WHD010 * WHD010)) |>
  select(-WHD010) |> rename("Weight (lbs)" = "WHD020")

weight <- clean_names(weight, "snake")


#### CARDIO HEALTH #####

cardioHealth <- read.xport("ryan/datasets/P_CDQ.XPT")

cardioHealth <- cardioHealth |> 
  select(SEQN, CDQ001, CDQ010) |>
  rename("had discomfort in chest?" = "CDQ001",
         "Shortness of breath stairs?" = "CDQ010")

cardioHealth <- clean_names(cardioHealth, "snake")
cardioHealth <- filter(cardioHealth, shortness_of_breath_stairs != 9 &
                         had_discomfort_in_chest != 9)



##### combine data by SEQN (id) ######
merged_data <- list(ldl, bloodpressure,
                    medicalConditions, weight, exerciseActivity, demographics, cardioHealth) %>% 
  reduce(inner_join, by = 'seqn')


merged_data <- filter(merged_data, is.na(merged_data$had_coronary_heart_disease) == F)


# remove NAs and replace with Mean for all columns and rows
NAtoMean <- merged_data

for (i in 1:ncol(merged_data)) {
  NAtoMean[,i][is.na(NAtoMean[,i])] <- mean(NAtoMean[,i], na.rm = TRUE)
}




