#Load packages
library(here)
library(ggplot2)

#read the recorded data
daphnia <- read.csv(here("output","daphnia_reproduction_copper.csv"))

daphnia[daphnia$test == odd[1] | daphnia$test == odd[2] |daphnia$test == odd[3] |daphnia$test == odd[4] |daphnia$test == odd[5],]

daphnia <- daphnia[daphnia$brood_number == 1 & !is.na(daphnia$brood_number) & daphnia$obs != "s",]

daphnia$temp <- paste(daphnia$acc_temp, daphnia$test_temp)

daphnia$acc_temp <- as.factor(daphnia$acc_temp)
daphnia$test_temp <- as.factor(daphnia$test_temp)

tests <- daphnia$test

daphnia[daphnia$temp == "25 15",]

for(i in 1:length(tests)){
  if(length(daphnia[daphnia$test ==  tests[i] & daphnia$obs == "n",1]) > 1){
    print(i)
    print(length(daphnia$day))
    print(daphnia[daphnia$test ==  tests[i] & daphnia$obs == "n",])
    print(1)
    brood_count <- sum(daphnia[daphnia$test ==  tests[i] & daphnia$obs == "n",]$count)
    print(2)
    daphnia[daphnia$test ==  tests[i] & daphnia$obs == "n",][1,]$count <- brood_count
    print(3)
    daphnia <- daphnia[daphnia$test !=  tests[i] | daphnia$time_brood != 1,]
  }
}

odd <- daphnia[daphnia$time_brood < 7 & !is.na(daphnia$time_brood),]$test

ggplot(data = daphnia, aes(x = conc, y = time_brood, col = test_temp))+
  geom_point(size = 4, alpha = 0.9)+
  geom_smooth(method='lm',formula=y~x, se = FALSE)+
  scale_color_manual(values = c("#224499","#AA33AA","#FF3333"))+
  #scale_color_manual(values = c("#991100","#CC4411","#FF6655","#007733","#33CC44","#88FF55","#004477","#4444BB","#8844FF"))+
  facet_wrap(~acc_temp)

ggplot(data = daphnia, aes(x = conc, y = count, col = test_temp))+
  geom_point(size = 4, alpha = 0.7)+
  geom_smooth(method='lm',formula=y~x, se = FALSE)+
  scale_color_manual(values = c("#224499","#AA33AA","#FF3333"))+
  #scale_color_manual(values = c("#991100","#CC4411","#FF6655","#007733","#33CC44","#88FF55","#004477","#4444BB","#8844FF"))+
  facet_wrap(~acc_temp)

time <- ggplot(data = daphnia, aes(x = log(conc+1), y = time_brood, col = test_temp))+
  geom_point(size = 4, alpha = 0.9)+
  geom_smooth(method='lm',formula=y~x, se = FALSE)+
  scale_color_manual(values = c("#224499","#AA33AA","#FF3333"))+
  labs(title = "Effect of copper and temperature on the time to daphnia first", x = "log[Cu]", y = "Time (days")+
  #scale_color_manual(values = c("#991100","#CC4411","#FF6655","#007733","#33CC44","#88FF55","#004477","#4444BB","#8844FF"))+
  facet_wrap(~acc_temp)

neo <- ggplot(data = daphnia, aes(x = log(conc+1), y = count, col = test_temp))+
  geom_point(size = 4, alpha = 0.9)+
  geom_smooth(method='lm',formula=y~x, se = FALSE)+
  scale_color_manual(values = c("#224499","#AA33AA","#FF3333"))+
  labs(title = "Effect of copper and temperature on the number of first brood daphnia offspring", x = "log[Cu]", y = "Neonates")+
  #scale_color_manual(values = c("#991100","#CC4411","#FF6655","#007733","#33CC44","#88FF55","#004477","#4444BB","#8844FF"))+
  facet_wrap(~acc_temp)

ggsave(here("figures","brood_1_offspring.jpg"), neo)
ggsave(here("figures","brood_1_time.jpg"), time)
