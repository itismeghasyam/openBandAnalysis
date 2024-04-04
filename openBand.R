#####################################
### OpenBand Prelim Study Summary
#####################################
library(synapser)
library(tidyverse)
synapser::synLogin()

# Find all records later than Oct 31, 2023 - this was the onboarding date
summary.tbl <- synapser::synTableQuery("SELECT * FROM syn22164707 where uploadDate > '2023-10-31'")$asDataFrame()

# Number of participants and records per participant per table
records.tbl <- summary.tbl %>% 
  dplyr::select(healthCode, originalTable, dayInStudy, phoneInfo, createdOn) %>% 
  unique() %>% 
  dplyr::group_by(healthCode) %>% 
  dplyr::mutate(correctedDayInStudy = (dayInStudy - min(dayInStudy) + 1)) %>% 
  dplyr::ungroup() 

# Records per each table
originalTable_records <- records.tbl %>% 
  dplyr::group_by(originalTable) %>% 
  dplyr::count()
knitr::kable(originalTable_records)

# Filter to tables that we need only - Sitting, Cycling, Walking, Sleeping
records.tbl.meta <- records.tbl %>% 
  dplyr::filter(originalTable %in% c('Sitting-v1','Cycling-v1','Walking-v1','Sleeping-v1'))

# number of participants! [Total 5 participants as of now]
print(unique(records.tbl.meta$healthCode))
print(unique(records.tbl.meta$healthCode) %>% length())

# How did participants contribute to acitivites: Records per participant categorized by activity
records.tbl.activity <- records.tbl.meta %>% 
  dplyr::select(healthCode, originalTable, correctedDayInStudy) %>% 
  tidyr::pivot_wider(names_from = originalTable,
                     values_from = correctedDayInStudy)
knitr::kable(records.tbl.activity)
View(records.tbl.activity)
# Looking at the above, we see that after the orientation date of Oct 31, 2023
# - Only 3 participants have records for all activities
# - One participant has records for all activities EXCEPT Walking
# - One participant has just a single record, for Sitting-v1 
