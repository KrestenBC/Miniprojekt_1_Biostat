# Load Libraries

library(tidyverse)  # For datamanipulation via dplyr
library(here)       # For filecontrol

# Load the raw data
soldiers <- read_csv2(here("raw_data", "soldiers.csv"))

# Clean the data
soldiers <- soldiers |>
  mutate(
    heightcm = Heightin * 2.54,
    weightkg = weightkg/10,            # weightkg is kg * 10 in the raw data
    BMI = weightkg/(heightcm/100)^2,
    
    # Sex values Male, Female, Man, Woman
    # Merge these values to Male and Female
    
    sex = case_when(
      sex == "Man" ~ "Male",
      sex == "Woman" ~ "Female",
      .default = sex),
    
    
    # Create categories for the level of BMI based on WHO guidelines
    category = case_when(
      BMI < 18.5 ~ "Underweight",
      BMI < 25   ~ "Normal range",
      BMI < 30   ~ "Overweight",
      BMI >= 30  ~ "Obese",
      is.na(BMI) ~ NA_character_,
      .default = "UNCLASSIFIED CHECK CODE!!!"),
    
    # DODRace is difficult to read with numeric values. 
    # Create a race variable thats easier to read
    race = case_when(
      DODRace == 1 ~ "White",
      DODRace == 2 ~ "Black",
      DODRace == 3 ~ "Hispanic",
      DODRace == 4 ~ "Asian",
      DODRace == 5 ~ "Native American",
      DODRace == 6 ~ "Pacific Islander",
      DODRace == 8 ~ "Other",
      is.na(DODRace) ~ NA_character_,
      .default = "UNCLASSIFIED CHECK CODE!!!")
    ) |> 
  
  # Drop Heightin because we have the subjects height in cm now
  select(-Heightin) |> 
  
  # Change the order of the columns
  relocate(subjectid,
           fake_cpr,
           sex,
           age,
           Ethnicity, 
           DODRace,
           race,
           Installation,
           Component,
           Branch,
           heightcm,
           weightkg,
           BMI,
           category)

