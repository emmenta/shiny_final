library(tidyverse)

Heart_Disease <- read_csv("Heart_Disease.csv")
glimpse(Heart_Disease)

rename(Heart_Disease, 
                      "Chest_paint_type" = "Chest pain type",
                      "FBS_over_120" = "FBS over 120",
                      "EKG_results" = "EKG results",
                      "Max_HR" = "Max HR",
                      "Exercise_angina" = "Exercise angina",
                      "ST_depression" = "ST depression",
                      "Slope_of_ST" = "Slope of ST",
                      "Number_of_vessels_fluro" = "Number of vessels fluro",
                      "Heart_Disease" = "Heart Disease"
       ) -> Heart_Disease


glimpse(Heart_Disease)


Heart_Disease |>
  mutate(Sex = ifelse(Sex == 1, "Male", "Female"),
         Chest_paint_type = ifelse(Chest_paint_type == 1, "typical angina", ifelse(Chest_paint_type == 2, "atypical angina", ifelse(Chest_paint_type == 3, "non-anginal pain", "asymptomatic"))),
         FBS_over_120 = ifelse(FBS_over_120 == 1, "Ture", "False"),
         EKG_results = ifelse(EKG_results == 0, "normal", ifelse(EKG_results == 1, "ST-T abnormality", "Ventricular hypertrophy")),
         Exercise_angina = ifelse(Exercise_angina == 1, "Yes", "No"),
         Slope_of_ST = ifelse(Slope_of_ST == 1, "Upsloping", ifelse(Slope_of_ST == 2, "Flat", "Downslope")),
         Thallium = ifelse(Thallium == 3, "Normal", ifelse(Thallium == 6, "Fixed defect", "Reversable defect"))
         
         ) -> Heart_Disease


glimpse(Heart_Disease)

Heart_Disease |> 
  mutate(
    Sex = factor(Sex),
    Chest_paint_type = factor(Chest_paint_type),
    FBS_over_120 = factor(FBS_over_120),
    EKG_results = factor(EKG_results),
    Exercise_angina = factor(Exercise_angina),
    Slope_of_ST = factor(Slope_of_ST),
    Thallium = factor(Thallium),
    Heart_Disease = factor(Heart_Disease)
  ) -> Heart_Disease

write.csv(Heart_Disease, file = "Heart_Disease_Data.csv")
