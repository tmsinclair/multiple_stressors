library(tidyverse)
library(lme4)
library(here)
#Creating dummy data

offspring <- c(25,21,20,17,18,12,13,9,6,2,
               37,33,28,26,21,20,16,11,8,3,
               40,35,33,26,22,21,14,13,7,2,
               48,39,34,29,24,20,18,13,5,1,
               50,53,48,33,36,24,18,11,6,2,
               55,51,48,40,35,22,14,13,8,3,
               80,70,60,50,40,30,20,10,0,0,
               70,61,53,46,31,26,20,17,7,3,
               99,90,84,79,72,69,67,56,50,0)

offspring <- c(25,21,20,17,18,12,13,9,6,2,
               37,33,28,26,21,20,16,11,8,3,
               50,40,37,30,27,21,14,13,7,2,
               35,33,28,24,19,17,13,10,5,1,
               50,53,48,33,36,24,18,11,6,2,
               65,63,59,45,40,38,28,20,11,3,
               50,47,46,40,30,27,20,16,8,4,
               70,75,60,50,46,35,25,17,7,3,
               99,84,73,58,52,40,31,29,12,2)

conc <- rep(c(0,1,3,7,15,31,62,125,250,500),9)
line <- c(rep(15,30),rep(20,30),rep(25,30))
test_temp <- rep(c(rep(15,10),rep(20,10),rep(25,10)),3)

results <- tibble(conc,line,test_temp,offspring)

results$line <- as.character(line)
results$test_temp <- as.character(test_temp)

results$log_conc <- log10(results$conc + 1)

plot <- ggplot(data = results, aes(x = log(conc), y = offspring, col = test_temp))+
  geom_point(size = 4)+
  facet_wrap(~line)+
  geom_smooth(method = 'lm', se=FALSE)

ggsave(file = here("figures","offspring_conc.jpeg"), plot= plot)

results %>%
  group_by(line, test_temp)%>%
  summarize(
    gradient = lm(offspring ~ log_conc)$`coefficients`[2],
    intercept = lm(offspring ~ log_conc)$`coefficients`[1],
  )

lapply()

lm <- lm(offspring ~ log_conc, results)

newx = seq(min(results$log_conc),max(results$log_conc),by = (max(results$log_conc)+min(results$log_conc))/20)

conf_interval <- predict(lm, newdata = data.frame(log_conc = newx), interval=c("confidence"),
                         level = 0.95)
results

plot <- ggplot(data = results, aes(x = log(conc), y = offspring, col = test_temp, shape = line))+
  geom_point(size = 4, alpha = 0.7)+
  geom_smooth(method = 'lm', se=FALSE)

ggsave(file = here("figures","offspring_log_conc_one_graph.jpeg"), plot= plot)

lmer(offspring ~ log_conc + line + test_temp + (log_conc | line) + (log_conc | test_temp) + (line | test_temp), data=results)

lm <- lm(offspring ~ log_conc*line*test_temp, data=results)

summary(lm)
anova(lm)
TukeyHSD(aov(lm))
plot(TukeyHSD(aov(lm)))

conc <- rep(c(0,1,3,7,15,31,62,125,250,500),27)
line <- rep(c(rep(15,30),rep(20,30),rep(25,30)),3)
test_temp <- rep(c(rep(15,10),rep(20,10),rep(25,10)),9)

offspring2 <- offspring + sample(-8:8, 90, replace = TRUE)
offspring3 <- offspring + sample(-8:8, 90, replace = TRUE)

offspring2[offspring2<0] <- 0
offspring3[offspring3<0] <- 0

offspring_rep <- c(offspring,offspring2,offspring3)

results <- tibble(conc,line,test_temp,offspring_rep)
results$log_conc <- log10(conc+1)
results_sum$mean
results_sum <- results %>%
  group_by(conc,line,test_temp)%>%
  summarise(
    mean = mean(offspring_rep),
    sd = sd(offspring_rep),
    se = sd(offspring_rep/1.96),
    min = min(offspring_rep),
    max = max(offspring_rep))
    
results_sum$line <-  as.character(results_sum$line)
results_sum$test_temp <-  as.character(results_sum$test_temp)

ggplot(data = results_sum, aes(x = conc, y = mean, col = test_temp))+
  geom_point(size = 4, alpha = 0.7)+
  geom_errorbar(mapping=aes(ymin=mean + se, ymax=mean - se), width=0.1, size=1)+
facet_wrap(~line)

plot <- ggplot(data = results_sum, aes(x = log(conc), y = mean, col = test_temp))+
  geom_point(size = 4, alpha = 0.7)+
  geom_errorbar(mapping=aes(ymin=mean + se, ymax=mean - se), width=0.1, size=1)+
  geom_smooth(method = 'lm', se=TRUE)+
facet_wrap(~line)

ggsave(file = here("figures","offspring_log_conc_CI.jpeg"), plot= plot)

results_ec50 <- results %>%
    group_by(line,test_temp)%>%
  summarise(
    min = min(offspring_rep),
    max = max(offspring_rep),
    grad = as.numeric(lm(offspring_rep ~ log_conc)$`coefficients`[2]),
    intercept = lm(offspring_rep ~ log_conc)$`coefficients`[1],
    ec50 = predict(lm(log_conc ~ offspring_rep, data=results),newdata = data.frame(offspring_rep = (max(offspring_rep)+min(offspring_rep))/4),  interval='confidence')[1],
    lwr = predict(lm(log_conc ~ offspring_rep, data=results),newdata = data.frame(offspring_rep = (max(offspring_rep)+min(offspring_rep))/4),  interval='confidence')[2],
    upr = predict(lm(log_conc ~ offspring_rep, data=results),newdata = data.frame(offspring_rep = (max(offspring_rep)+min(offspring_rep))/4),  interval='confidence')[3],
    n = n())

results_ec50$line <-  as.character(results_ec50$line)

plot <- ggplot(data = results_ec50, aes(x = test_temp, y = ec50, col = line))+
  geom_point(size = 4, alpha = 0.7)+
  geom_errorbar(mapping=aes(ymin= lwr, ymax= upr), width=0.1, size=1)+
  geom_smooth(method = 'lm', se=FALSE)

ggsave(file = here("figures","ec50.jpeg"), plot= plot)

lm <- lm(ec50 ~ line*test_temp, data=results_ec50)
summary(lm)
anova(lm)
TukeyHSD(aov(lm))
plot <- plot(TukeyHSD(aov(lm)))
ggsave(file = here("figures","ec50_tukey.jpeg"), plot= plot)
