# Load libraries ---------------------------------------------------------------

library(here)
library(foreign)
library(readr)
library(dplyr)
library(stringr)

# With thanks to this GitHub gist:
# https://gist.github.com/igorbrigadir/0d03171e3e731b615f55

# Convert SAV to CSV -----------------------------------------------------------

write.table(
  read.spss(
    here("data/world_24nations_25nov2006.sav"), to.data.frame = TRUE),
    file=here("data/reference_data_converted.csv"), quote = FALSE, sep = ",")

# Import the converted CSV data ------------------------------------------------

reference_data_converted <- read.csv(
  here("data/reference_data_converted.csv"),
  header = FALSE, na.strings = "NA", stringsAsFactors = FALSE)

# Tidy the CSV data ------------------------------------------------------------

# Assign column names
first_row <- as.vector(reference_data_converted[1,1:19])
vars <- c("obs_id", first_row)
initial_tidy <- reference_data_converted
colnames(initial_tidy) <- vars

# Delete original column names from first row
initial_tidy <- initial_tidy %>%
  filter(obs_id != "sample")

# Remove empty rows used as visual separators for human readability
initial_tidy %>%
  filter(self1 != "") -> initial_tidy

# Fix up right-shifted data ----------------------------------------------------

# Subset to right-shifted records and tidy
initial_tidy %>%
  filter(!sex %in% c(NA, "male", "female")) %>%
  # Replace values that have shifted over to the incorrect variable
  mutate(
    nation = sample,
    sample = age,
    age = sex,
    sex = self1,
    self1 = self2,
    self2 = self3,
    self3 = self4,
    self4 = self5,
    self5 = self6,
    self6 = self7,
    self7 = self8,
    self8 = self9,
    self9 = self10,
    self10 = agegr_us,
    agegroup = sumscore,
    sumscore = NA_real_) -> reference_data_shifted

# Subset to non-shifted records
initial_tidy %>%
  filter(sex %in% c(NA, "male", "female")) -> reference_data_not_shifted

# Merge subsets back together
aligned_data <- rbind(
  reference_data_not_shifted, reference_data_shifted)

# Tidy up item values ----------------------------------------------------------

# Remove records where the item values are not valid
# Valid values: not at all true, barely true, moderately true, exactly true
aligned_data %>%
  mutate(
    self1_check = grepl("^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$", self1),
    self2_check = grepl("^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$", self2),
    self3_check = grepl("^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$", self3),
    self4_check = grepl("^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$", self4),
    self5_check = grepl("^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$", self5),
    self6_check = grepl("^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$", self6),
    self7_check = grepl("^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$", self7),
    self8_check = grepl("^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$", self8),
    self9_check = grepl("^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$", self9),
    self10_check = grepl("^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$", self10)) %>%
  filter(self1_check == FALSE) %>%
  filter(self2_check == FALSE) %>%
  filter(self3_check == FALSE) %>%
  filter(self4_check == FALSE) %>%
  filter(self5_check == FALSE) %>%
  filter(self6_check == FALSE) %>%
  filter(self7_check == FALSE) %>%
  filter(self8_check == FALSE) %>%
  filter(self9_check == FALSE) %>%
  filter(self10_check == FALSE) %>%
  select(-ends_with("_check")) -> fixed_item_values

# Recalculate summed scores ----------------------------------------------------

fixed_item_values %>%
  mutate(
    self1 = str_trim(self1, side = "both"),
    self2 = str_trim(self2, side = "both"),
    self3 = str_trim(self3, side = "both"),
    self4 = str_trim(self4, side = "both"),
    self5 = str_trim(self5, side = "both"),
    self6 = str_trim(self6, side = "both"),
    self7 = str_trim(self7, side = "both"),
    self8 = str_trim(self8, side = "both"),
    self9 = str_trim(self9, side = "both"),
    self10 = str_trim(self10, side = "both"),
    self1_itemscore = case_when(
      self1 == "not at all true" ~ 1,
      self1 == "barely true"     ~ 2,
      self1 == "moderately true" ~ 3,
      self1 == "exactly true"    ~ 4),
    self2_itemscore = case_when(
      self2 == "not at all true" ~ 1,
      self2 == "barely true"     ~ 2,
      self2 == "moderately true" ~ 3,
      self2 == "exactly true"    ~ 4),
    self3_itemscore = case_when(
      self3 == "not at all true" ~ 1,
      self3 == "barely true"     ~ 2,
      self3 == "moderately true" ~ 3,
      self3 == "exactly true"    ~ 4),
    self4_itemscore = case_when(
      self4 == "not at all true" ~ 1,
      self4 == "barely true"     ~ 2,
      self4 == "moderately true" ~ 3,
      self4 == "exactly true"    ~ 4),
    self5_itemscore = case_when(
      self5 == "not at all true" ~ 1,
      self5 == "barely true"     ~ 2,
      self5 == "moderately true" ~ 3,
      self5 == "exactly true"    ~ 4),
    self6_itemscore = case_when(
      self6 == "not at all true" ~ 1,
      self6 == "barely true"     ~ 2,
      self6 == "moderately true" ~ 3,
      self6 == "exactly true"    ~ 4),
    self7_itemscore = case_when(
      self7 == "not at all true" ~ 1,
      self7 == "barely true"     ~ 2,
      self7 == "moderately true" ~ 3,
      self7 == "exactly true"    ~ 4),
    self8_itemscore = case_when(
      self8 == "not at all true" ~ 1,
      self8 == "barely true"     ~ 2,
      self8 == "moderately true" ~ 3,
      self8 == "exactly true"    ~ 4),
    self9_itemscore = case_when(
      self9 == "not at all true" ~ 1,
      self9 == "barely true"     ~ 2,
      self9 == "moderately true" ~ 3,
      self9 == "exactly true"    ~ 4),
    self10_itemscore = case_when(
      self10 == "not at all true" ~ 1,
      self10 == "barely true"     ~ 2,
      self10 == "moderately true" ~ 3,
      self10 == "exactly true"    ~ 4),
    sumscore_fixed = self1_itemscore + self2_itemscore + self3_itemscore +
      self4_itemscore + self5_itemscore + self6_itemscore + self7_itemscore +
      self8_itemscore + self9_itemscore + self10_itemscore) %>%
  select(-ends_with("itemscore"), -sumscore) -> reference_data_tidy

# Export the tidy CSV data -----------------------------------------------------

write.csv(
  reference_data_tidy,
  here("data/reference_data_tidy.csv"),
  row.names = FALSE, na = "NA")