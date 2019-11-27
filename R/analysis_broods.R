#Load packages
library(here)
library(ggplot2)

#read the recorded data
daphnia <- read.csv(here("output","daphnia_reproduction_copper.csv"))

daphnia <- daphnia[!is.na(daphnia$brood_number),]

daphnia <- daphnia[daphnia$conc != "60" | daphnia$brood_number != "2",]

daphnia$temp <- paste(daphnia$acc_temp, daphnia$test_temp)

daphnia$acc_temp <- as.factor(daphnia$acc_temp)
daphnia$test_temp <- as.factor(daphnia$test_temp)
daphnia$conc <- as.factor(daphnia$conc)

tests <- daphnia$test

daphnia[daphnia$temp == "25 15",]

daphnia$brood_number

ggplot(data = daphnia, aes(x = brood_number, y = time_brood, col = test_temp))+
  geom_point(size = 4, alpha = 0.9)+
  scale_color_manual(values = c("#224499","#AA33AA","#FF3333"))+
  #scale_color_manual(values = c("#991100","#CC4411","#FF6655","#007733","#33CC44","#88FF55","#004477","#4444BB","#8844FF"))+
  facet_wrap(~acc_temp)

ggplot(data = daphnia, aes(x = brood_number, y = count, col = test_temp))+
  geom_point(size = 4, alpha = 0.7)+
  scale_color_manual(values = c("#224499","#AA33AA","#FF3333"))+
  #scale_color_manual(values = c("#991100","#CC4411","#FF6655","#007733","#33CC44","#88FF55","#004477","#4444BB","#8844FF"))+
  facet_wrap(~acc_temp)

time <- ggplot(data = daphnia, aes(x = brood_number, y = time_brood, col = test_temp))+
  geom_point(size = 4, alpha = 0.9)+
  scale_color_manual(values = c("#224499","#AA33AA","#FF3333"))+
  labs(title = "Effect temperature on time to next brood", x = "Brood number", y = "Time (days")+
  #scale_color_manual(values = c("#991100","#CC4411","#FF6655","#007733","#33CC44","#88FF55","#004477","#4444BB","#8844FF"))+
  facet_wrap(~acc_temp)

neo <- ggplot(data = daphnia, aes(x = brood_number, y = count, col = test_temp))+
  geom_point(size = 4, alpha = 0.9)+
  geom_smooth(method='lm',formula=y~x, se = FALSE)+
  scale_color_manual(values = c("#224499","#AA33AA","#FF3333"))+
  labs(title = "Effect temperature on number of offspring over multiple broods", x = "Brood number", y = "Neonates")+
  #scale_color_manual(values = c("#991100","#CC4411","#FF6655","#007733","#33CC44","#88FF55","#004477","#4444BB","#8844FF"))+
  facet_wrap(~acc_temp)

ggsave(here("figures","brood_offspring_end.jpg"), neo)
ggsave(here("figures","brood_time_end.jpg"), time)

anova_time_brood <- aov(daphnia$time_brood ~ daphnia$acc_temp*daphnia$test_temp*daphnia$conc*daphnia$brood_number)
summary(anova_time_brood)

anova_count <- aov(daphnia$count ~ daphnia$acc_temp*daphnia$test_temp*daphnia$conc*daphnia$brood_number)
summary(anova_count)
str(anova_count)

time <- ggplot(data = daphnia, aes(x = brood_number, y = time_brood, col = test_temp))+
  geom_point(size = 4, alpha = 0.9)+
  #scale_color_manual(values = c("#224499","#AA33AA","#FF3333"))+
  labs(title = "Effect temperature on time to next brood", x = "Brood number", y = "Time (days")+
  #scale_color_manual(values = c("#991100","#CC4411","#FF6655","#007733","#33CC44","#88FF55","#004477","#4444BB","#8844FF"))+
  facet_wrap(~conc)

ggsave(here("figures","brood_time_conc.jpg"), time)

neo <- ggplot(data = daphnia, aes(x = brood_number, y = count, col = conc))+
  geom_point(size = 4, alpha = 0.9)+
  geom_smooth(method='lm',formula=y~x, se = FALSE)+
  #scale_color_manual(values = c("#224499","#AA33AA","#FF3333"))+
  labs(title = "Effect temperature on number of offspring over multiple broods", x = "Brood number", y = "Neonates")+
  #scale_color_manual(values = c("#991100","#CC4411","#FF6655","#007733","#33CC44","#88FF55","#004477","#4444BB","#8844FF"))+
  facet_wrap(~test_temp)

ggsave(here("figures","broods_offspring_conc.jpg"), neo)

###
#plots of conc on x axis
###

time <- ggplot(data = daphnia, aes(x = conc, y = time_brood, col = test_temp))+
  geom_point(size = 4, alpha = 0.9)+
  #scale_color_manual(values = c("#224499","#AA33AA","#FF3333"))+
  labs(title = "Effect temperature on time to next brood", x = "Brood number", y = "Time (days")+
  #scale_color_manual(values = c("#991100","#CC4411","#FF6655","#007733","#33CC44","#88FF55","#004477","#4444BB","#8844FF"))+
  facet_wrap(~conc)

ggsave(here("figures","brood_time_conc.jpg"), time)

neo <- ggplot(data = daphnia, aes(x = conc, y = count, col = conc))+
  geom_point(size = 4, alpha = 0.9)+
  geom_smooth(method='lm',formula=y~x, se = FALSE)+
  #scale_color_manual(values = c("#224499","#AA33AA","#FF3333"))+
  labs(title = "Effect temperature on number of offspring over multiple broods", x = "Brood number", y = "Neonates")+
  #scale_color_manual(values = c("#991100","#CC4411","#FF6655","#007733","#33CC44","#88FF55","#004477","#4444BB","#8844FF"))+
  facet_wrap(~test_temp)

ggsave(here("figures","broods_offspring_conc.jpg"), neo)
