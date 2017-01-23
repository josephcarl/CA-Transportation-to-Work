# Joseph Carl
# BUAN 5210
# Final Project: CA Transportation
# Technical Appendix


# RESEARCH QUESTIONS
  # How does the proportion of people driving/carpooling to work compare to the 
    # proportion of people using alternate transportation in different parts of the state 
    # (both county and region)?
  # Do different races/ethnicities use alternative forms of transportation in varying levels, and do those levels differ across the state?
  # Have uses of alternate transportation increased or decreased over time?

# RESEARCH PLAN
# Calculate and visualize the % level of driving/carpooling compared to alternate transportation 
#      by county and region. Is there a difference between urban and rural counties/regions?
# Calculate and visualize the % level of driving/carpooling compared to alternate transportation 
#     by ethnicity. Are certain races/ethnicities more likely to use alternate transportation than others?
# Compare the level of usage (% of population) for each transportation type for 2000 and 2006-2010 
#     to see if there are differences. I want to see if alternate transportation usage has increased, 
#     decreased, or stayed the same over time, and see if there is a difference across counties and ethnicities.
# Because this dataset also has statewide data for the above averages, I can compare counties and regions 
#     to the state average to see which ones are above and below the average level.

# MAIN FINDINGS
  # 1. Change in mode usage by ethnicity and/or region from 2000 to 2006-2010
  # 2. Conditional probability of race/ethnicity given transit mode
  # 3. Alternate transport can be popular in urban, suburban, and rural regions


#--------------------------
# GETTING STARTED
#--------------------------


# Clear workspace
rm(list=ls(all=TRUE))

# Libraries
library(tidyverse)
library(maps)             # for creating county maps of California
library(RColorBrewer)


#this is the data used for this analysis
CAtransit <- read.csv("CAtransit.csv", stringsAsFactors = TRUE)


# Capwords function from toupper() documentation to make strings look better
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


#--------------------------
# VISUALS USED IN FINAL REPORT
#--------------------------

# 1
# Do different races/ethnicities use transport modes differently across CA?
CAtransit %>% 
  group_by(race_eth_name, mode, reportyear) %>% 
  summarise(modepop = sum(pop_mode, na.rm=T), totpop = sum(pop_total, na.rm=T),
            perc = modepop/totpop) %>% 
  ggplot(aes(race_eth_name, perc, fill=mode))+
  geom_bar(stat="identity")+
  facet_grid(reportyear ~.)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(x="Race/Ethnicity",
       y="Percent of Workforce Using Transit Mode", 
       fill="Mode",
       title = "Driving Alone Is Dominant Transportation Mode for All Groups")
# whites appear to drive alone the most, while Latinos drive alone the least and carpool the most
# African Americans appear to use public transit at higher rates than others

# How much did mode usage change over time for each race/ethnicity?
CAtransit %>%  
  group_by(race_eth_name, mode, reportyear) %>% 
  summarise(modepop = sum(pop_mode, na.rm=T), totpop = sum(pop_total, na.rm=T),
            perc = modepop/totpop) %>% 
  select(-(4:5)) %>% 
  spread(reportyear, perc) %>% 
  mutate(percChange = (`2006-2010` - `2000`) / `2000` , 
         percPointChange = (`2006-2010` - `2000`)*100) %>% 
  ggplot(aes(race_eth_name, percPointChange, colour=mode, shape=mode))+
  geom_point(size=3)+
  scale_shape_manual(values = c(15,16,17,18,19,20))+
  geom_hline(yintercept = 0)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(x="Race/Ethnicity",
       y="Percentage Point Change", 
       fill="Mode",
       title = "Carpooling Declines Across All Racial and Ethnic Groups",
       subtitle = "Driving Alone and Working from Home have Biggest Increases from 2000 to 2006-2010")


# 2
# Construct a frequency table of race/ethnicity and transportation mode

race_transit <- CAtransit %>% filter(reportyear=="2006-2010") %>% 
  group_by(race_eth_name, mode) %>% 
  summarise(people = sum(pop_mode, na.rm=T)) 


race_transit_freq <- xtabs(people ~ race_eth_name + mode, race_transit)

# Graphing relative probabilities
transit_probs <- as.data.frame(prop.table(race_transit_freq, 2))
transit_probs$Source <- "Data"

# get statewide % of population for each group
statewide <- CAtransit %>% filter(reportyear=="2006-2010") %>% 
  group_by(race_eth_name) %>% 
  summarise(people = sum(pop_mode, na.rm=T)) %>% 
  mutate(mode = as.factor("State Avg"),
         Freq = people/sum(people),
         Source = as.factor("Statewide")) %>% 
  select(-2)

transit_probs_graph2 <-  bind_rows(transit_probs, statewide)
transit_probs_graph2$mode <- as.factor(transit_probs_graph2$mode)
transit_probs_graph2$Source <- as.factor(transit_probs_graph2$Source)


ggplot(transit_probs_graph2, aes(mode, Freq, fill = race_eth_name)) +
  geom_bar(stat="identity")+
  facet_grid(~ Source, scales = "free_x", space = "free_x")+
  labs(x="Transportation Mode",
       y="% of Users for Each Mode",
       fill="Race/Ethnicity",
       title = "Latinos Disproportionately Use Transit & Carpooling",
       subtitle = "Whites Disproportionately Bike and Work from Home")+
  scale_fill_brewer(palette = "Paired")

# 3
# what is the breakdown (by mode) of alternate transport use by region?
# just using 2006-2010 data for this one

CAtransit %>% filter(reportyear == "2006-2010" & mode != "CAR") %>% 
  group_by(region_name, mode) %>% 
  summarise(modepop = sum(pop_mode, na.rm=T), totpop = sum(pop_total, na.rm=T),
            perc = modepop/totpop*100) %>% 
  ggplot(aes(region_name, perc, fill=mode, label=round(perc,1)))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_hline(yintercept=25.7, linetype="dashed")+
  labs(x="Region", 
       y="% of Workforce",
       fill="Mode",
       title="Most Regions Below State Average for Alternate Transport Use",
       subtitle = "Carpooling is Most Popular Alternative to Driving Alone")+
  geom_text(size=3, position = position_stack(vjust=0.5), color="white")


#--------------------------
# EDA
#--------------------------

# What's the data like?
str(CAtransit)

summary(CAtransit)

# histograms
hist(CAtransit$pop_mode)
hist(CAtransit$pop_total)
hist(CAtransit$percent)

# How are the data organized?
levels(CAtransit$geoname)         # 58 levels, 58 counties in CA
levels(CAtransit$region_name)     # 14 regions
levels(CAtransit$race_eth_name)   # 8 races/ethnicities
  # AIAN: American Indian/Alaska Native
  # NHOPI: Native Hawaiian or Pacific Islander

# Number of counties in each region
CAtransit %>% group_by(region_name) %>% 
  summarise(counties = n_distinct(geoname))

# How many people in the dataset?
CAtransit %>% group_by(reportyear) %>% 
  summarise(people = sum(pop_mode, na.rm=T)) %>%    # 14.4mil in 2000, 16mil in 2006-2010
  ggplot(aes(reportyear, people, fill=reportyear))+
  geom_bar(stat="identity", position = "dodge")+
  ggtitle("Number of People Each Year")

# How many people in each race/ethnicity group?
CAtransit %>% group_by(reportyear, race_eth_name) %>% 
  summarise(people = sum(pop_mode, na.rm=T)) %>%
  ggplot(aes(race_eth_name, people, fill=reportyear))+
  geom_bar(stat="identity", position = "dodge")+
  ggtitle("How many people of each race/ethnicity?")
# white: over 7mil
# Latino: jumped from 3.8MM to 5.4MM
# African Am: ~800M
# Asian: jumped from 1.6MM to 2.2MM
# very low levels of other groups

# How many people for each transportation type?
CAtransit %>% group_by(reportyear, mode_name) %>% 
  summarise(people = sum(pop_mode, na.rm=T)) %>% 
  ggplot(aes(mode_name, people, fill=reportyear))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  ggtitle("How many people used each transportation type?")
# Every category except carpool decreased from 2000 to 2006-2010
  # but this makes sense since the total number of respondents also increased
# this would look good as a line graph because it's time data
CAtransit %>% group_by(reportyear, mode_name) %>% 
  summarise(people = sum(pop_mode, na.rm=T)) %>% 
  ggplot(aes(reportyear, people, group=mode_name, colour=mode_name))+
  geom_point()+
  geom_line()+
  ggtitle("How many people used each transportation type?")
# big jump in driving alone, slight decrease in carpooling
# looks like slight uptick in working at home, other categories fairly flat


# How many people in each region?
CAtransit %>% group_by(reportyear, region_name) %>% 
  summarise(people = sum(pop_mode, na.rm=T)) %>% 
  ggplot(aes(region_name, people, fill=reportyear))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  ggtitle("How many people in each region?")
# largest area by far is SoCal, over 7MM people, 
    # includes all of LA metro counties plus Imperial county
# followed by Bay Area with over 3MM people
# smaller regions of San Diego, San Joaquin Valley, and Sacramento, ~.5MM each
    # this could be a good breakdown of big city/medium/small or rural

# How many people in each county?
popbyCounty2006 <- CAtransit %>% filter(reportyear == "2006-2010") %>% 
  group_by(tolower(geoname)) %>% 
  summarise(people = sum(pop_mode, na.rm=T))
names(popbyCounty2006) <- c("subregion", "people")

popbyCounty2000 <- CAtransit %>% filter(reportyear == "2000") %>% 
  group_by(tolower(geoname)) %>% 
  summarise(people = sum(pop_mode, na.rm=T))
names(popbyCounty2000) <- c("subregion", "people")


# How closely related are # of people for each commute vs total people?
ggplot(CAtransit, aes(pop_total, pop_mode, colour = mode))+
  geom_point()+
  facet_grid(reportyear ~ .)    # view each year separately
  # car is still dominant, and each mode appears in almost perfect line -- 
  # little variation in proportions?
# Look at each mode separately
ggplot(CAtransit, aes(pop_total, pop_mode, colour = mode))+
  geom_point()+
  facet_wrap(~mode)
  # a little easier to see the variation in this graph, the plots no longer look quite
  # as tight linearly, particularly in large population counties

# each transport type - this would look good as a proportional bar graph, 
      # using percentage instead of # of people


# How does the proportion of people driving/carpooling to work compare to the proportion of people 
    # using alternate transportation in different parts of the state (both county and region)?
# start with aggregate usage of each transportation mode

CAtransit %>% group_by(reportyear, mode) %>% 
  summarise(modepop = sum(pop_mode, na.rm=T), totpop = sum(pop_total, na.rm=T),
            perc = modepop/totpop) %>% 
  ggplot(aes(reportyear, perc, fill=mode))+
  geom_bar(stat="identity")+
  coord_flip()
  # not much change in the aggregate. car is dominant at over 70% of commuters
  # car usage increased slightly statewide from 2000 to 2006-2010, carpool declined slightly
  # and working from home increased slightly

# but we can use this code to get the state averages
CAstateModeRates <- CAtransit %>% group_by(reportyear, mode) %>% 
  summarise(modepop = sum(pop_mode, na.rm=T), totpop = sum(pop_total, na.rm=T),
            perc = modepop/totpop)

# CAbyCounty %>% group_by(reportyear, geoname, mode) %>% 
#   summarise(pplPerCounty = sum(pop_total/6-sum(is.na(pop_total))) )
#   summarise(modepop = sum(pop_mode, na.rm=T), totpop = sum(pop_total, na.rm=T),
#             perc = modepop/totpop)

# What was the percent change of each mode of transport?
CAtransit %>% group_by(mode, reportyear) %>% 
  summarise(modepop = sum(pop_mode, na.rm=T), totpop = sum(pop_total, na.rm=T),
            perc = modepop/totpop) %>% 
  select(-(3:4)) %>%                                      # get rid of totals and keep %
  spread(reportyear, perc) %>% 
  mutate(percChange = (`2006-2010` - `2000`) / `2000` , 
         percPointChange = (`2006-2010` - `2000`)*100) %>% 
  ggplot(aes(mode, percPointChange, fill=mode))+
  geom_bar(stat="identity")+
  ggtitle("Percentage Point Change in Transport Mode Usage Statewide")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
  

# How does the use of alternate transportation (including carpooling) compare to car usage?
CAtransit %>% group_by(mode, reportyear) %>% 
  summarise(modepop = sum(pop_mode, na.rm=T), totpop = sum(pop_total, na.rm=T),
            perc = modepop/totpop) %>% 
  select(-(3:4)) %>% 
  spread(reportyear, perc) %>% 
  mutate(alt = ifelse(mode=="CAR", "Driving Alone", "Alternate Transport")) %>% 
  group_by(alt) %>% 
  summarise(year2000 = sum(`2000`), year2006.2010 = sum(`2006-2010`)) %>% 
  gather("year", "percent", 2:3) %>% 
  ggplot(aes(year, percent, fill=alt))+
  geom_bar(stat="identity")+
  ggtitle("Slight Decline in Use of Alternate Transportation\nfrom 2000 to 2006-2010")

# Does the breakdown of transport mode usage vary across regions?
CAtransit %>%  
  group_by(region_name, mode, reportyear) %>% 
  summarise(modepop = sum(pop_mode, na.rm=T), totpop = sum(pop_total, na.rm=T),
            perc = modepop/totpop) %>% 
  ggplot(aes(region_name, perc, fill=mode))+
  geom_bar(stat="identity")+
  facet_grid(reportyear ~.)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
# car is still mostly dominant across CA, hard to see minor changes in levels of this graph
# try calculating the % change of each mode in each region
CAtransit %>%  
  group_by(region_name, mode, reportyear) %>% 
  summarise(modepop = sum(pop_mode, na.rm=T), totpop = sum(pop_total, na.rm=T),
            perc = modepop/totpop) %>% 
  select(-(4:5)) %>% 
  spread(reportyear, perc) %>% 
  mutate(percChange = (`2006-2010` - `2000`) / `2000` , 
         percPointChange = (`2006-2010` - `2000`)*100) %>% 
  ggplot(aes(region_name, percPointChange, colour=mode, shape=mode))+
  geom_point(size=3)+
  scale_shape_manual(values = c(15,16,17,18,19,20))+
  geom_hline(yintercept = 0)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(
    title = "Carpooling Declines Across All Regions",
    subtitle = "Driving Alone and Working from Home have Modest Increases")

# Do different races/ethnicities use transport modes differently across CA?
CAtransit %>% 
  group_by(race_eth_name, mode, reportyear) %>% 
  summarise(modepop = sum(pop_mode, na.rm=T), totpop = sum(pop_total, na.rm=T),
            perc = modepop/totpop) %>% 
  ggplot(aes(race_eth_name, perc, fill=mode))+
  geom_bar(stat="identity")+
  facet_grid(reportyear ~.)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(x="Race/Ethnicity",
       y="Percent of Workforce", 
       fill="Mode",
       title = "Driving Alone Is Dominant Transportation Mode for All Groups")
# whites appear to drive alone the most, while Latinos drive alone the least and carpool the most
# African Americans appear to use public transit at higher rates than others


# How much did mode usage change over time for each race/ethnicity?
CAtransit %>%  
  group_by(race_eth_name, mode, reportyear) %>% 
  summarise(modepop = sum(pop_mode, na.rm=T), totpop = sum(pop_total, na.rm=T),
            perc = modepop/totpop) %>% 
  select(-(4:5)) %>% 
  spread(reportyear, perc) %>% 
  mutate(percChange = (`2006-2010` - `2000`) / `2000` , 
         percPointChange = (`2006-2010` - `2000`)*100) %>% 
  ggplot(aes(race_eth_name, percPointChange, colour=mode, shape=mode))+
  geom_point(size=3)+
  scale_shape_manual(values = c(15,16,17,18,19,20))+
  geom_hline(yintercept = 0)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(x="Race/Ethnicity",
       y="Percentage Point Change", 
       fill="Mode",
       title = "Carpooling Declines Across All Racial/Ethnic Groups",
       subtitle = "Driving Alone and Working from Home have Biggest Increases from 2000 to 2006-2010")


# How does the % driving alone vary across counties? (maybe not much based on it not varying much by region)
UseByCounty <- CAtransit %>% 
  group_by(tolower(geoname), mode, reportyear) %>% 
  summarise(modepop = sum(pop_mode, na.rm=T), totpop = sum(pop_total, na.rm=T),
            perc = modepop/totpop) %>% 
  select(-(4:5)) %>% 
  spread(reportyear, perc)
names(UseByCounty) <- c("subregion", "mode", "perc2000", "perc2006.2010")


# Which counties are the biggest bicyclists?
CAtransit %>% 
  group_by(tolower(geoname), mode, reportyear) %>% 
  summarise(modepop = sum(pop_mode, na.rm=T), totpop = sum(pop_total, na.rm=T),
            perc = modepop/totpop*100) %>% 
  select(-(4:5)) %>% 
  spread(reportyear, perc) %>% 
  filter(mode=="BICYCLE") %>% 
  arrange(desc(`2006-2010`)) %>% 
  filter(`2006-2010` > 1.5) %>%     # gets counties with > 1.5% of workforce biking
  gather("year", "perc", 3:4) %>% 
  ggplot(aes(perc, reorder(capwords(`tolower(geoname)`), perc), color=year))+
  geom_point(size=3)+
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey60", linetype = "dashed"))
# Yolo county over 7% (Home of Davis, Bike Capital of America)
# Santa Barbara and Inyo both over 3%


# Which counties are using transit at the highest % of population?
UseByCounty %>% group_by(mode) %>% 
  top_n(12, perc2006.2010) %>% 
  arrange(desc(perc2006.2010)) %>% 
  filter(mode=="PUBLICTR") %>% 
  gather("year", "percent", 3:4) %>% 
  ggplot(aes(percent*100, reorder(capwords(subregion), percent), color=year)) +
  geom_point(size=3)+
  geom_vline(xintercept = 5.13, color = "red") +      # vertical line for state average
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey60", linetype = "dashed"))+
  labs(y= "County",
       title = "Transit Usage Highest in Areas with Biggest Transit Systems",
       subtitle = "San Francisco's Transit Usage Far Surpasses Other Counties")+
  scale_x_continuous("Percent of Workforce", limits = c(0,35), breaks = seq(0,35,5))


# Which counties are utilizing alternate transport (not driving alone) at the highest rates?
CAtransit %>% group_by(geoname, mode, reportyear) %>% 
  summarise(modepop = sum(pop_mode, na.rm=T), totpop = sum(pop_total, na.rm=T),
            perc = modepop/totpop) %>% 
  select(-(4:5)) %>% 
  filter(mode == "CAR") %>% 
  mutate(AltPerc = (1-perc)*100) %>% 
  select(-4) %>% 
  spread(reportyear, AltPerc) %>% 
  arrange(desc(`2006-2010`)) %>% 
  filter(`2006-2010` > 30) %>%          # select counties where >30% uses alternate transport
  gather("year", "percent", 3:4) %>% 
  ggplot(aes(percent, reorder(capwords(as.character(geoname)), percent), color = year)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 27.0, color = "lightseagreen") +    # statewide average in 2006-2010
  geom_vline(xintercept = 28.2, color = "brown1") +           # statewide average for 2000
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey60", linetype = "dashed"))+
  labs(x= "Percent of Workforce",
       y= "County",
       color = "Year",
       title = "Alternate Transport Usage High in Both Urban and Rural Counties",
       subtitle = "Statewide Average for Alternate Transport is 27.0%")
  
# Which races/ethnicities are utilizing alternate transport (not driving alone) at the highest rates?
CAtransit %>% group_by(race_eth_name, mode, reportyear) %>% 
  summarise(modepop = sum(pop_mode, na.rm=T), totpop = sum(pop_total, na.rm=T),
            perc = modepop/totpop) %>% 
  select(-(4:5)) %>% 
  filter(mode == "CAR") %>% 
  mutate(AltPerc = (1-perc)*100) %>% 
  select(-4) %>% 
  spread(reportyear, AltPerc) %>% 
  arrange(desc(`2006-2010`)) %>% 
  gather("year", "percent", 3:4) %>% 
  ggplot(aes(percent, reorder(race_eth_name, percent), color = year)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 27.0, color = "lightseagreen") +    # statewide average in 2006.2010
  geom_vline(xintercept = 28.2, color = "brown1") +           # statewide average for 2000
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey60", linetype = "dashed"))+
  labs(x= "Percent of Workforce",
       y= "Race/Ethnicity",
       color = "Year",
       title = "Alternate Transport Use by Race/Ethnicity",
       subtitle = "Latinos Well Above State Average, Whites well Below")


# what is the breakdown (by mode) of alternate transport use by region?
  # just using 2006-2010 data for this one
CAtransit %>% filter(reportyear == "2006-2010" & mode != "CAR") %>% 
  group_by(region_name, mode) %>% 
  summarise(modepop = sum(pop_mode, na.rm=T), totpop = sum(pop_total, na.rm=T),
            perc = modepop/totpop*100) %>% 
  ggplot(aes(region_name, perc, fill=mode, label=round(perc,1)))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  geom_hline(yintercept=25.7, linetype="dashed")+
  labs(x="Region", 
       y="% of Workforce",
       fill="Mode",
       title="Most Regions Below State Average for Alternate Transport Use",
       subtitle = "Carpooling is Most Popular Alternative to Driving Alone")+
  geom_text(size=3, position = position_stack(vjust=0.5), color="white")
  



#--------------------------
# CONDITIONAL PROBABILITIES
#--------------------------

# Construct a frequency table of race/ethnicity and transportation mode

race_transit <- CAtransit %>% filter(reportyear=="2006-2010") %>% 
  group_by(race_eth_name, mode) %>% 
  summarise(people = sum(pop_mode, na.rm=T)) 

# heat map of race/ethnicity and transport mode

race_transit_freq <- xtabs(people ~ race_eth_name + mode, race_transit)

# Is mode of transportation independent of race/ethnicity?
# Do a chi-squared test
chisq.test(race_transit_freq)
# p-value > 0.5, reject Ho, mode of transportation depends on race/ethnicity

# Graphing relative probabilities
transit_probs <- as.data.frame(prop.table(race_transit_freq, 2))
transit_probs$Source <- "Data"

# get statewide % of population for each group
statewide <- CAtransit %>% filter(reportyear=="2006-2010") %>% 
  group_by(race_eth_name) %>% 
  summarise(people = sum(pop_mode, na.rm=T)) %>% 
  mutate(mode = as.factor("State Avg"),
         Freq = people/sum(people),
         Source = as.factor("Statewide")) %>% 
  select(-2)

transit_probs_graph2 <-  bind_rows(transit_probs, statewide)
transit_probs_graph2$mode <- as.factor(transit_probs_graph2$mode)
transit_probs_graph2$Source <- as.factor(transit_probs_graph2$Source)

statewide2 <- CAtransit %>% filter(reportyear=="2006-2010") %>% 
  group_by(race_eth_name) %>% 
  summarise(people = sum(pop_mode, na.rm=T)) %>% 
  mutate(perc = people/sum(people)) %>% 
  select(-2)

transit_probs_graph <- inner_join(transit_probs, statewide2, by="race_eth_name") %>% 
  mutate(diff = Freq - perc)

# Plot conditional probabilities
ggplot(transit_probs_graph, aes(race_eth_name, mode, fill = diff)) +
  geom_raster()+
  geom_text(aes(label = round(Freq*100, 2)), size = 4.5)+
  theme_minimal()+
  scale_fill_gradient2(low = "darkred", mid = "lightgrey", high = "blue", 
                       midpoint = 0,
                      breaks = c(0,20,40,60))+
  labs(x="Race/Ethnicity",
       y="Transportation Mode",
       fill="Over/Under-Represented",
       title = "Conditional Probability of Race Given Transport Mode", 
       subtitle = "Latinos and Asians Use Transit and Carpooling at Disproportionately High Levels")

ggplot(transit_probs_graph2, aes(mode, Freq, fill = race_eth_name)) +
  geom_bar(stat="identity")+
  facet_grid(~ Source, scales = "free_x", space = "free_x")+
  labs(x="Transportation Mode",
       y="% of Users for Each Mode",
       fill="Race/Ethnicity",
       title = "Latinos Disproportionately Use Transit & Carpooling",
       subtitle = "Whites Disproportionately Bike and Work from Home")+
  scale_fill_brewer(palette = "Paired")
  
class(race_transit_freq)

addmargins(race_transit_freq)

race_transit_prop <- prop.table(race_transit_freq)
round(race_transit_prop, 6)

round(addmargins(race_transit_prop), 6)

# Relative probabilities
  # Ex: What's the probability that someone takes public transit given that they're white?
  # P(publictr|white) = P(publictr,white)/P(white)
prop.table(race_transit_freq,1)     # 3.21%
  # Ex: What's the probability that someone who works from home is Latino?
  # P(Latino | wfh) = P(Latino,wfh)/P(wfh)
prop.table(race_transit_freq,2)     # 19.2%
  # interesting findings: Latinos are disproportionately more likely to carpool

#--------------------------
# MAP DATA
#--------------------------

# Maps created with the help of:
  # http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

ca_df <- map_data("state") %>% 
  subset(region == "california")
ca_counties <- map_data("county") %>% 
  subset(region == "california")

ca_base <- ggplot(ca_df, aes(x=long, y=lat, group=group))+
  coord_fixed(1.3)+
  geom_polygon(color = "black", fill = "gray")

ca_base +
  geom_polygon(data = ca_counties, fill = NA, color = "white") +
  geom_polygon(color="black", fill=NA)

# plot 2006-2010 people by county
ca_co_ppl2006 <- inner_join(ca_counties, popbyCounty2006, by = "subregion")
ca_co_ppl2000 <- inner_join(ca_counties, popbyCounty2000, by = "subregion")

# Following theme courtesy of:
# http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

# Map of number of people in each county in 2006-2010
ca_base +
  geom_polygon(data = ca_co_ppl2006, aes(fill=people), color="white")+
  geom_polygon(color="black", fill=NA)+
  theme_bw()+
  ditch_the_axes+
  scale_fill_gradient(low = "white", high = "darkred",
                      trans = "log10")+
  ggtitle("Number of commuters in 2006-2010")

ca_base +
  geom_polygon(data = ca_co_ppl2000, aes(fill=people), color="white")+
  geom_polygon(color="black", fill=NA)+
  theme_bw()+
  ditch_the_axes+
  scale_fill_gradient(low = "white", high = "darkred",
                      trans = "log10")+
  ggtitle("Number of commuters in 2000")



# Map the county level transport use data
CountyDatatoMap <- inner_join(ca_counties, UseByCounty, by = "subregion")

# Driving alone in 2000 and 2006.2010
ca_base +
  geom_polygon(data = filter(CountyDatatoMap, mode=="CAR") %>% select(-9),
               aes(fill=perc2000*100), color="white") +
  geom_polygon(color="black", fill=NA)+
  theme_bw()+
  ditch_the_axes+
  labs(fill="Percent",
       title = "% of Drive Alone commuters in 2000",
       subtitle = "Outside of San Francisco, Car Remains King in CA")+
  scale_fill_gradient(low = "white", 
                       high = "darkred" ) 
car2006 <- ca_base +
  geom_polygon(data = filter(CountyDatatoMap, mode=="CAR") %>% select(-8),
               aes(fill=perc2006.2010*100), color="white") +
  geom_polygon(color="black", fill=NA)+
  theme_bw()+
  ditch_the_axes+
  labs(fill="Percent",
       title = "Driving Alone")+
  scale_fill_gradient(low = "white", 
                      high = "darkred" ) 

# title = "% of Drive Alone commuters in 2006-2010",
# subtitle = "Outside of San Francisco, Car Remains King in CA"
car2006

# As for carpooling
ca_base +
  geom_polygon(data = filter(CountyDatatoMap, mode=="CARPOOL") %>% select(-9),
               aes(fill=perc2000*100), color="white") +
  geom_polygon(color="black", fill=NA)+
  theme_bw()+
  ditch_the_axes+
  labs(fill="Percent")+
  scale_fill_gradient(low = "white", 
                      high = "darkred" ) +
  ggtitle("% of Workforce who Carpooled in 2000")
carpool2006 <- ca_base +
  geom_polygon(data = filter(CountyDatatoMap, mode=="CARPOOL") %>% select(-8),
               aes(fill=perc2006.2010*100), color="white") +
  geom_polygon(color="black", fill=NA)+
  theme_bw()+
  ditch_the_axes+
  labs(fill="Percent",
       title="Carpool")+
  scale_fill_gradient(low = "white", 
                      high = "darkred" )
  
# ggtitle("% of Workforce who Carpooled in 2006-2010")
carpool2006


# Bicycling
ca_base +
  geom_polygon(data = filter(CountyDatatoMap, mode=="BICYCLE") %>% select(-9),
               aes(fill=perc2000*100), color="white") +
  geom_polygon(color="black", fill=NA)+
  theme_bw()+
  ditch_the_axes+
  labs(fill="Percent")+
  scale_fill_gradient(low = "white", 
                      high = "darkred" ) +
  ggtitle("% of Workforce who Biked in 2000")
bike2006 <- ca_base +
  geom_polygon(data = filter(CountyDatatoMap, mode=="BICYCLE") %>% select(-8),
               aes(fill=perc2006.2010*100), color="white") +
  geom_polygon(color="black", fill=NA)+
  theme_bw()+
  ditch_the_axes+
  labs(fill="Percent",
       title="Bicycle")+
  scale_fill_gradient(low = "white", 
                      high = "darkred" ) 
# title = "% of Workforce who Biked in 2006-2010",
# subtitle = "Yolo Remains Bike Capital, Santa Barbara and Inyo both over 3%")+

bike2006

# How do the counties vary for public transit usage?
# Map public transit use
ca_base +
  geom_polygon(data = filter(CountyDatatoMap, mode=="PUBLICTR") %>% select(-9),
               aes(fill=perc2000*100), color="white") +
  geom_polygon(color="black", fill=NA)+
  theme_bw()+
  ditch_the_axes+
  labs(fill="Percent",
       title = "% of Workforce who Took Public Transit in 2000",
       subtitle = "San Francisco Leads at 32%, Bay Area and LA Near 10%")+
  scale_fill_gradient(low = "white", 
                      high = "darkred",
                      breaks = c(0,5,10,20,30),
                      trans="sqrt")                     # Transformed scale to sqrt bc of San Francisco
transit2006 <- ca_base +
  geom_polygon(data = filter(CountyDatatoMap, mode=="PUBLICTR") %>% select(-8),
               aes(fill=perc2006.2010*100), color="white") +
  geom_polygon(color="black", fill=NA)+
  theme_bw()+
  ditch_the_axes+
  labs(fill="Percent",
       title = "Public Transit") +
  scale_fill_gradient(low = "white", 
                      high = "darkred",
                      breaks = c(0,5,10,20,30),
                      trans = "sqrt")                   # Transformed scale to sqrt bc of San Francisco

# % of Workforce who Took Public Transit in 2006-2010
# San Francisco Leads at 32%, Bay Area and LA Near 10%
transit2006

# What about working from home? (guessing more prominent in urban counties)
UseByCounty %>% group_by(mode) %>% 
  top_n(10, perc2000) %>% 
  arrange(desc(perc2000)) %>% 
  filter(mode=="ATHOME")
UseByCounty %>% group_by(mode) %>% 
  top_n(10, perc2006.2010) %>% 
  arrange(desc(perc2006.2010)) %>% 
  filter(mode=="ATHOME")  
  # looks like it's mostly rural counties actually
ca_base +
  geom_polygon(data = filter(CountyDatatoMap, mode=="ATHOME") %>% select(-9),
               aes(fill=perc2000*100), color="white") +
  geom_polygon(color="black", fill=NA)+
  theme_bw()+
  ditch_the_axes+
  labs(fill="Percent",
       title = "% of Workforce who Worked from Home in 2000") +
  scale_fill_gradient(low = "white", 
                      high = "darkred") 
athome2006 <- ca_base +
  geom_polygon(data = filter(CountyDatatoMap, mode=="ATHOME") %>% select(-8),
               aes(fill=perc2006.2010*100), color="white") +
  geom_polygon(color="black", fill=NA)+
  theme_bw()+
  ditch_the_axes+
  labs(fill="Percent",
       title = "Work from Home") +
  scale_fill_gradient(low = "white", 
                      high = "darkred",
                      breaks = c(0,5,10,20),
                      trans = "sqrt") 
# % of Workforce who Worked from Home in 2006-2010
athome2006

# What about walking to work? (probably not too common outside SF)
UseByCounty %>% group_by(mode) %>% 
  top_n(10, perc2000) %>% 
  arrange(desc(perc2000)) %>% 
  filter(mode=="WALK")
UseByCounty %>% group_by(mode) %>% 
  top_n(10, perc2006.2010) %>% 
  arrange(desc(perc2006.2010)) %>% 
  filter(mode=="WALK")  
  # actually, it's a bunch of rural counties and San Francisco. Maybe people living in small towns
  # walk to work?
ca_base +
  geom_polygon(data = filter(CountyDatatoMap, mode=="WALK") %>% select(-9),
               aes(fill=perc2000*100), color="white") +
  geom_polygon(color="black", fill=NA)+
  theme_bw()+
  ditch_the_axes+
  labs(fill="Percent",
       title = "% of Workforce who Walked to Work in 2000") +
  scale_fill_gradient(low = "white", 
                      high = "darkred",
                      trans = "sqrt") 
walk2006 <- ca_base +
  geom_polygon(data = filter(CountyDatatoMap, mode=="WALK") %>% select(-8),
               aes(fill=perc2006.2010*100), color="white") +
  geom_polygon(color="black", fill=NA)+
  theme_bw()+
  ditch_the_axes+
  labs(fill="Percent",
       title="Walk") +
  scale_fill_gradient(low = "white", 
                      high = "darkred",
                      trans = "sqrt") 
# % of Workforce who Walked to Work in 2006-2010
walk2006
