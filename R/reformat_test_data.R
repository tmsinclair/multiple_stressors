#Load packages
library(here)
library(tidyverse)

#read the recorded data
daphnia <- read.csv(here("input","daphnia_reproduction_copper_3.csv"))

#convert D,M,Y columns to date
daphnia$date_manual <- as.Date(paste(daphnia$year,daphnia$month,daphnia$day, sep = "-"))

#ensure date is stored as date
daphnia$date <- as.Date(daphnia$date)

#and save the file
write.csv(daphnia, here("output","daphnia_reproduction_copper_incomplete.csv"), row.names = FALSE)
#
concs <- c(0,6,9,13,20,30,45,60)

for(i in length(concs):1){
  daphnia$conc[daphnia$conc == i-1] <- concs[i]
}

start <- c()
concs_start <- c(98,0,6,9,13,20,30,45,60)

for(j in 2:(length(concs_start))){
  daphnia[daphnia$obs == "s" & daphnia$conc == concs_start[j-1],]$conc <- concs_start[j]
  if(j == length(concs_start)){
    break
  }
  start <- rbind(start, daphnia[daphnia$obs == "s",])
}
length(start[,1])
daphnia <- rbind(daphnia,start)

finish <- c()
concs_fin <- c(99,0,6,9,13,20,30,45,60)
for(k in 2:length(concs_fin)){
  daphnia[daphnia$obs == "f" & daphnia$conc == concs_fin[k-1],]$conc <- concs_fin[k]
  if(k == length(concs_fin)){
    break
  }
  finish <- rbind(finish, daphnia[daphnia$obs == "f",])
}
daphnia <- rbind(daphnia,finish)

finish[finish$acc_temp == 3 & finish$test_temp == 1,]
daphnia[daphnia$acc_temp == 25 & daphnia$test_temp == 15 & daphnia$rep == 3,]
temps <- c(15,20,25)

for(o in 1:3){
  daphnia$acc_temp[daphnia$acc_temp == o] <- temps[o]
  daphnia$test_temp[daphnia$test_temp == o] <- temps[o]
}

#reorder chronologically
daphnia <- daphnia[order(daphnia$date),]

#make a list of unique test jar identifiers
daphnia$test <- paste(daphnia$acc_temp, daphnia$test_temp, daphnia$rep, daphnia$conc)

#and make a list of those jar tests
tests <- unique(daphnia$test)

#remove any duplicate rows
daphnia <-  unique(daphnia)

daphnia[daphnia$rep == 5,]

for(m in 1:length(tests)){
  if(length(daphnia[daphnia$test == tests[m] & daphnia$obs == "s",]$obs) > 1){
    print(tests[m])
  }
}


#The next loop calculates the number of days between broods and which brood number the observation is
m <- 297
#for each test jar
for(m in 1:length(tests)){
  #filter observations for that jar
  group <- daphnia[daphnia$test == tests[m],]
  #and only consider offspring
  group <- group[group$obs == "s" | group$obs == "n" | group$obs == "e",]

  #then for each offspring observation
  #if there is more than one
  if(length(group[,1]) > 1){
    for(n in 2:(length(group[,1]))){
  #then the time to brood is the date of that brood minus the previous or start date
      daphnia[daphnia$test == tests[m] & daphnia$obs == "s" | daphnia$test == tests[m] & daphnia$obs == "n" | daphnia$test == tests[m] & daphnia$obs == "e",][n,]$time_brood <- group[n,]$date - group[n - 1,]$date
  #if tha time to brood is one (ie. <24 apart)
      if(      daphnia[daphnia$test == tests[m] & daphnia$obs == "s" | daphnia$test == tests[m] & daphnia$obs == "n" | daphnia$test == tests[m] & daphnia$obs == "e",][n,]$time_brood == 1){
        #then assume they are the same brood
        daphnia[daphnia$test == tests[m] & daphnia$obs == "s" | daphnia$test == tests[m] & daphnia$obs == "n" | daphnia$test == tests[m] & daphnia$obs == "e",][n,]$brood_number <- n - 2
      } else{
        #otherwise increase the brood count by one
          daphnia[daphnia$test == tests[m] & daphnia$obs == "s" | daphnia$test == tests[m] & daphnia$obs == "n" | daphnia$test == tests[m] & daphnia$obs == "e",][n,]$brood_number <- n - 1
      }
    }
  }
}

#and to fill in a time for those that don't reproduce'
p <- 248
for(p in 1:length(tests)){
  #filter observations for that jar
  group <- daphnia[daphnia$test == tests[p],]
  #and only look for groups
  if(!any(any(group$obs == "d") | any(group$obs == "e") |any(group$obs == "n")) == TRUE){
   end <- group[group$obs == "f",]
   end$obs <- "n"
   end$count <- 0
   end$time_brood <- end$date - group[group$obs == "s",]$date
   end$brood_number <- 1
   daphnia <- rbind(daphnia, end)
  }
}

#save the data
write.csv(daphnia, here("output","daphnia_reproduction_copper.csv"), row.names = FALSE)
