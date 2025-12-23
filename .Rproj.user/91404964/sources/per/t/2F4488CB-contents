

library(readr)
library(dplyr)
library(janitor)
library(tidyr)
library(stringr)
library(forcats)

data <- read_csv("MLC Congregation Survey_December 3, 2025_14.35.csv", skip = 1) |>
  slice(-1) |>
  type_convert()

data_names <- data.frame(full_name = names(data)) |>
  mutate(
    clean_name = make_clean_names(full_name)
  )

data <- data |>
  clean_names()



names(data)
new_names <- c(
  "start_date",
  "end_date",
  "response_type",
  "ip_address",
  "progress",
  "duration_in_seconds",
  "finished",
  "recorded_date",
  "response_id",
  "recipient_last_name",
  "recipient_first_name",
  "recipient_email",
  "external_data_reference",
  "location_latitude",
  "location_longitude",
  "distribution_channel",
  "user_language",
  "church_mission_feeling",                  # [18]
  "church_expectations_choice",              # [19]
  "church_expectations_other",               # [20]
  "service_attendance",                      # [21]
  "spiritually_fed",                         # [22]
  "connected_to_community",                  # [23]
  "connection_factors_choice",               # [24]
  "connection_factors_other",                # [25]
  "music_styles_choice",                     # [26]
  "music_styles_other",                      # [27]
  "youth_program_feedback",                  # [28]
  "church_priorities_choice",                # [29]
  "church_priorities_other",                 # [30]
  "age_range",                               # [31]
  "church_association_length",               # [32]
  "additional_thoughts"                      # [33]
)
names(data) <- new_names

data_names$new_name <- new_names

data_names <- data_names |>
  mutate(
    multi_choice = str_detect(new_names, "_choice"),
    open = str_detect(new_names, "_other|_feedback|additional_thoughts")
  )


# Cleaning Data ----------------------------------------------------------

clean <- data |>
  mutate(
    # Creating Factors
    church_mission_feeling = factor(church_mission_feeling, levels = c("Very dissatisfied", "Dissatisfied", "Neither satisfied nor dissatisfied", "Satisfied", "Very Sattisfied")),
    spiritually_fed = factor(spiritually_fed, levels = c("Rarely", "Sometimes", "Usually", "Always")),
    connected_to_community = factor(connected_to_community, levels = c("Rarely", "Sometimes", "Usually", "Always")),
    age_range = factor(age_range, levels = c("Under 20", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")),
    church_association_length = factor(church_association_length, levels = c("Less than 1 year", "1-5 years", "6-10 years", "11-15 years", "16-20 years", "Over 20 years")),
    
    # Replacing "Other"
    # church_expectations_choice = str_replace(church_expectations_choice, "Please Describe", church_expectations_other)
) |>
  select(
    -c(response_type, ip_address, progress, finished, starts_with("recipient"), starts_with("location"), distribution_channel, external_data_reference)
  )

multi_long <- clean |>
  select(
    response_id, age_range, service_attendance, church_association_length, all_of(data_names |> filter(multi_choice == TRUE) |> pull(new_name))
  ) |>
  pivot_longer(cols = ends_with("choice"), names_to = "question", values_to = "response") |>
  separate_longer_delim(response, ",") |>
  group_by(question, response) |>
  mutate(n = n()) |>
  arrange(question, desc(n)) |>
  ungroup()



# Write Data --------------------------------------------------------------

write_rds(clean, "survey_clean.rds")
write_rds(multi_long, "survey_multi_long.rds")



# Data for Dashboard Table -----------------------------------------------

table_data <- clean |>
  select(
    response_id,
    age_range, service_attendance, church_association_length,
    church_mission_feeling, spiritually_fed, connected_to_community,
    church_expectations_choice, church_expectations_other, connection_factors_choice, connection_factors_other,
    music_styles_choice, music_styles_other, church_priorities_choice, church_priorities_other,
    youth_program_feedback, additional_thoughts
  ) |>
  mutate(
    across(
      where(is.character),
      function(str){
        str_replace_all(str, ",", ", ") |>
          str_squish()
      }
    )
  )
names(table_data) <- c(
  "response_id",
  "Age Range", "Primary Service", "Church Association Length",
  "Church Mission Feeling", "Spiritually Fed", "MLC Community Connection",
  "Church Expectations", "Church Expectations (Other)", "Connection Factors", "Connection Factors (Other)",
  "Music Styles", "Music Styles-Other", "Church Priorities", "Church Priorities (Other)",
  "Youth Program Feedback", "Additional Feedback"
)

# names(table_data)[match(data_names[data_names$new_name %in% names(table_data),"new_name"], names(table_data))] <- data_names[data_names$new_name %in% names(table_data),"full_name"]

write_rds(table_data, "table_data.rds")
