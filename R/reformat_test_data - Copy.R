#

#Load packages
library(here)
library(tidyverse)

#read the recorded data
daphnia <- read.csv(here("input","daphnia_reproduction_copper.csv"))

#convert D,M,Y columns to date
daphnia$date_manual <- as.Date(paste(daphnia$year,daphnia$month,daphnia$day, sep = "-"))

#ensure date is stored as date
daphnia$date <- as.Date(daphnia$date)

#reorder chronologically
daphnia <- daphnia[order(daphnia$date),]

#make a list of unique test jar identifiers
daphnia$test <- paste(daphnia$acc_temp, daphnia$test_temp, daphnia$conc, daphnia$rep)

#and make a list of those jar tests
tests <- unique(daphnia$test)

#remove any duplicate rows
daphnia <-  unique(daphnia)

#and save the file
write.csv(daphnia, here("output","daphnia_reproduction_copper_incomplete.csv"), row.names = FALSE)

#The next loop calculates the number of days between broods and which brood number the observation is

#for each test jar
for(i in 1:length(tests)){
  #filter observations for that jar
  group <- filter(daphnia, test == tests[i])
  #and only consider offspring
  group <- filter(group, obs == "start" | obs == "neonates" | obs == "ephippia")

  #then for each offspring observation
  #if there is more than one
  if(length(group[,1]) > 1){
    for(j in 2:length(group[,1])){
  #then the time to brood is the date of that brood minus the previous or start date
      daphnia[daphnia$test == tests[i] & daphnia$obs == "start" | daphnia$test == tests[i] & daphnia$obs == "neonates" | daphnia$test == tests[i] & daphnia$obs == "ephippia",][j,]$time_brood <- group[j,]$date - group[j - 1,]$date
  #if tha time to brood is one (ie. <24 apart)
      if(      daphnia[daphnia$test == tests[i] & daphnia$obs == "start" | daphnia$test == tests[i] & daphnia$obs == "neonates" | daphnia$test == tests[i] & daphnia$obs == "ephippia",][j,]$time_brood == 1){
        #then assume they are the same brood
        daphnia[daphnia$test == tests[i] & daphnia$obs == "start" | daphnia$test == tests[i] & daphnia$obs == "neonates" | daphnia$test == tests[i] & daphnia$obs == "ephippia",][j,]$brood_number <- j - 2
      } else{
        #otherwise increase the brood count by one
        daphnia[daphnia$test == tests[i] & daphnia$obs == "start" | daphnia$test == tests[i] & daphnia$obs == "neonates" | daphnia$test == tests[i] & daphnia$obs == "ephippia",][j,]$brood_number <- j - 1
      }
    }
  }
}

#save the data
write.csv(daphnia, here("output","daphnia_reproduction_copper.csv"), row.names = FALSE)
