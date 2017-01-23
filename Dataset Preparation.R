# Joseph Carl
# BUAN 5210
# Final Project: CA Transportation


#--------------------------
# DATASET PREPARATION
#--------------------------

# Clear workspace
rm(list=ls(all=TRUE))

# CA transportation to work
  # data source:
  # https://chhs.data.ca.gov/Environment/Transportation-To-Work-2000-2006-2010/wgev-qytt
    # I first filtered the dataset in my web browser using the "Manage" option
    # to remove columns I did not want (for smaller file size) :
    # ind_id, ind_definition, race_eth_code, LL95CI_percent, UL95CI_percent, percent_se,
    # percent_rse, CA_decile, CA_RR, version
  # Then I downloaded the filtered data as "Transportation_To_Work_2000-2006-2010.csv"
  # Save that csv in working directory

# Original data used for creating subset of data called "CAtransit"
# Read in the data
transit <- read.csv("Transportation_To_Work_2000-2006-2010.csv", stringsAsFactors = TRUE) %>%
  # Filter the data to keep only county-level data, and remove Totals (from ethnicity and cartotal)
  # totals can be re-calculated in R later
  filter(geotype == "CO" & race_eth_name != "Total" & mode != "CARTOTAL")

# Save a copy of this smaller dataset to use
write_csv(transit, path = "CAtransit.csv")
  # this writes a copy of the smaller dataset as a csv to the working directory
  # to be used for the analysis


# Get another dataset that just has totals by county instead of broken down by race/ethnicity
transit2 <- read.csv("Transportation_To_Work_2000-2006-2010.csv", stringsAsFactors = TRUE) %>%
  filter(geotype == "CO" & race_eth_name == "Total" & mode != "CARTOTAL")
# Save copy of smaller dataset
write_csv(transit2, path = "CountyTotals.csv")
  # Note: I did not end up using this dataset in the analysis, since I had more than enough to work 
  # with in the CAtransit file

