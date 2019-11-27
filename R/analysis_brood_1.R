#Load packages
library(here)
library(ggplot2)

#read the recorded data
daphnia <- read.csv(here("output","daphnia_reproduction_copper.csv"))

daphnia <- daphnia[setdiff(rownames(daphnia),
                rownames(daphnia[daphnia$time_brood < 28 & daphnia$count == 0,])),]

daphnia <- daphnia[daphnia$brood_number == 1 & !is.na(daphnia$brood_number) & daphnia$obs != "s",]

daphnia <- daphnia[setdiff(rownames(daphnia),
                           rownames(daphnia[daphnia$conc == 0 & daphnia$count == 0,])),]

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

daph <- daphnia[setdiff(rownames(daphnia),
                        rownames(daphnia[daphnia$time_brood <31 & daphnia$count == 0 & daphnia$test_temp == 15,])),]
daph <- daph %>%
  group_by(acc_temp, test_temp, conc)%>%
  summarise(
    n = length(count),
    count_sd = sd(count),
    count = mean(count),
    time_brood_sd = sd(time_brood),
    time_brood = mean(time_brood)
  )

time <- ggplot(data = daphnia[setdiff(rownames(daphnia),
                                      rownames(daphnia[daphnia$time_brood <31 & daphnia$count == 0 & daphnia$test_temp == 15,])),], aes(x = log(conc+1), y = time_brood, col = test_temp))+
  geom_point(data = daph, aes(x = log(conc+1), y = time_brood, col = test_temp), size = 4, alpha = 0.75)+
  geom_errorbar(data = daph, mapping=aes(x = log(conc+1), ymin=time_brood-time_brood_sd, ymax=time_brood+time_brood_sd, color= test_temp), width=0.2, size=1)+
  geom_smooth(method='lm',formula=y~x, se = FALSE,lwd = 1.5)+
  scale_color_manual(values = c("#224499","#AA33AA","#FF3333"))+
  labs(title = "Acclimatised temperature (degrees centigrade)", x = "log[Cu]", y = "Time (days)")+
  #scale_color_manual(values = c("#991100","#CC4411","#FF6655","#007733","#33CC44","#88FF55","#004477","#4444BB","#8844FF"))+
  facet_wrap(~acc_temp)

neo <- ggplot(data = daphnia, aes(x = log(conc+1), y = count, col = test_temp))+
  geom_point(data = daph, aes(x = log(conc+1), y = count, col = test_temp),size = 4, alpha = 0.75)+
  geom_errorbar(data = daph, mapping=aes(x = log(conc+1), ymin=count-count_sd, ymax=count+count_sd, color= test_temp), width=0.2, size=1)+
  geom_smooth(method='lm',formula=y~x, se = FALSE,lwd = 1.5)+
  scale_color_manual(values = c("#224499","#AA33AA","#FF3333"))+
  labs(title = "Acclimatised temperature (degrees centigrade)", x = "log[Cu]", y = "Neonates")+
  #scale_color_manual(values = c("#991100","#CC4411","#FF6655","#007733","#33CC44","#88FF55","#004477","#4444BB","#8844FF"))+
  facet_wrap(~acc_temp)

ggsave(here("figures","brood_1_offspring.jpg"), neo, width = 5, height = 3)
ggsave(here("figures","brood_1_time.jpg"), time, width = 5, height = 3)

daphnia$acc_temp <- as.numeric(daphnia$acc_temp)
daphnia$test_temp <- as.numeric(daphnia$test_temp)

anova_time_brood <- aov(daphnia$time_brood ~ daphnia$acc_temp*daphnia$test_temp*daphnia$conc)
summary(anova_time_brood)

anova_time_brood <- glm(daphnia$time_brood ~ daphnia$acc_temp*daphnia$conc*daphnia$test_temp)
summary(anova_time_brood)

anova_time_brood2 <- glm(daphnia$time_brood ~ daphnia$acc_temp*daphnia$conc+daphnia$test_temp)
summary(anova_time_brood)

anova(anova_time_brood, anova_time_brood2, test="LRT")

anova_time_brood <- aov(daphnia$time_brood ~ daphnia$acc_temp*daphnia$conc+daphnia$test_temp)
summary(anova_time_brood)

anova_count <- aov(daphnia$count ~ daphnia$acc_temp*daphnia$test_temp*daphnia$conc)
summary(anova_count)

anova_count <- glm(daphnia$count ~ daphnia$acc_temp*daphnia$conc+daphnia$test_temp)
summary(anova_count)

anova_count <- aov(daphnia$count ~ daphnia$acc_temp*daphnia$conc+daphnia$test_temp)
summary(anova_count)

daph_0 <- daphnia %>%
  filter(count > 0)%>%
  filter(!is.na(time_brood))%>%
  filter(conc == 0)%>%
  filter(brood_number == 1)%>%
  group_by(acc_temp, test_temp)%>%
  summarise(
    n = length(count),
    count_sd = sd(count),
    count = mean(count),
    time_brood_sd = sd(time_brood),
    time_brood = mean(time_brood)
    )

daph_0$acc_temp <- as.character(daph_0$acc_temp)
daph_0$test_temp <- as.numeric(daph_0$test_temp)

daphnia$acc_temp <- as.character(daphnia$acc_temp)
daphnia$test_temp <- as.numeric(daphnia$test_temp)

time <- ggplot(data = daphnia %>%
                 filter(count > 0)%>%
                 filter(!is.na(time_brood))%>%
                 filter(conc == 0)%>%
                 filter(brood_number == 1), aes(x = test_temp, y = time_brood, col = acc_temp))+
  geom_point(data = daph_0, aes(x = test_temp, y = time_brood, col = acc_temp), size = 4, alpha = 0.75)+
  geom_errorbar(data = daph_0, mapping=aes(x=test_temp, ymin=time_brood-time_brood_sd, ymax=time_brood+time_brood_sd, color= acc_temp), width=0.2, size=1.5)+
  geom_smooth(method='lm',formula=y~x, se = FALSE,lwd = 1.5)+
  scale_color_manual(values = c("#1199BB","#22CC66","#CCBB22"))+
  labs(title = "A", x = "Test Temperature", y = "Time (days)")

time

neo <- ggplot(data = daphnia %>%
                filter(count > 0)%>%
                filter(!is.na(time_brood))%>%
                filter(conc == 0)%>%
                filter(brood_number == 1), aes(x = test_temp, y = count, col = acc_temp))+
  geom_point(data = daph_0, aes(x = test_temp, y = count, col = acc_temp),size = 4, alpha = 0.75)+
  geom_errorbar(data = daph_0, mapping=aes(x=test_temp, ymin=count-count_sd, ymax=count+count_sd, color= acc_temp), width=0.2, size=1.5)+
  geom_smooth(method='lm',formula=y~x, se = FALSE,lwd = 1.5)+
  scale_color_manual(values = c("#1199BB","#22CC66","#CCBB22"))+
  labs(title = "B", x = "Test Temperature", y = "Neonates")

neo

ggsave(here("figures","control_offspring.jpg"), neo, width = 5, height = 3)
ggsave(here("figures","control_time.jpg"), time, width = 5, height = 3)
