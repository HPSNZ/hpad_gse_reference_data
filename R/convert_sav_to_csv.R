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
reference_data_tidy <- reference_data_converted
colnames(reference_data_tidy) <- vars

# Delete original column names from first row
reference_data_tidy <- reference_data_tidy %>%
  filter(obs_id != "sample")

# Remove empty rows used as visual separators for human readability
reference_data_tidy %>%
  filter(self1 != "") -> reference_data_tidy

# Fix up right-shifted data ----------------------------------------------------

# Subset to right-shifted records and tidy
reference_data_tidy %>%
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
reference_data_tidy %>%
  filter(sex %in% c(NA, "male", "female")) -> reference_data_not_shifted

# Merge subsets back together
reference_data_tidy <- rbind(
  reference_data_not_shifted, reference_data_shifted)

# Export the tidy CSV data -----------------------------------------------------

write.csv(
  reference_data_tidy,
  here("data/reference_data_tidy.csv"),
  row.names = FALSE, na = "NA")