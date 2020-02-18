# Load libraries ---------------------------------------------------------------

library(here)
library(foreign)
library(readr)
library(dplyr)

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
reference_data_tidy <- reference_data_tidy %>%
  filter(self1 != "")

# Export the tidy CSV data -----------------------------------------------------

write.csv(
  reference_data_tidy,
  here("data/reference_data_tidy.csv"),
  row.names = FALSE, na = "NA")