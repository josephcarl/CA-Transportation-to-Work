# CA-Transportation-to-Work
Visualization of Transportation Mode Usage between 2000 and 2010 in California for final project in BUAN 5210 (Communicating and Visualizing Data) at Seattle University

The dataset for this analysis comes from the California Health and Human Services Agency and provides the number of people in each county and region who use as their main source of commuting to work one of the following transportation modes: working from home ("ATHOME"), bicycling, driving alone ("CAR"), carpooling, taking public transit ("PUBLICTR"), and walking. The data are available for each of California's 58 counties as well as 15 regions, which are groups of counties. The data, taken from the U.S. Census and the American Community Survey, are for years 2000 and 2006-2010, where the 2006-2010 values are an average of those years, as that data was collected in multiple years. These two time points were chosen because the data for these periods were also broken down into 8 racial and ethnic groups: African Americans, American Indian/Alaska Natives (AIAN), Asians, Latinos (of all races), Multiple race, Native Hawaiian or Pacific Islander (NHOPI), Other, and non-Latino White.

Due to the large file size, the csv for the subset of county and regional data used in this analysis ("CATransit.csv") is provided here. For documentation of how that subset was created, see file "Dataset Preparation.R"

The entire analysis is included in the file "CA_Transportation.R", while the main findings are included in the final report (pdf and Rmd).
