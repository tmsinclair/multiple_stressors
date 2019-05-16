#dummy data

#equations
dydx <- c(rep(c(27,37,50),3),
  21,31,42,
  27,37,47,
  35,45,55,
  23,37,60,
  27,37,50,
  33,37,41)

#concentrations
conc <- log10(c(0,1,3,7,15,31,62,125,250,500)+1)
no_daph <- function(x){
  x*log10(c(0,1,3,7,15,31,62,125,250,500)+1)
}


library(tidyverse)
line <- rep(c(rep(15,3),rep(20,3),rep(25,3)),3)
test_temp <- rep(c(15,20,25),9)
hypoth <- rep(c(rep("none", 9), rep("ec50", 9), rep("grad_ec50", 9)), 10)
conc <- c(0,1,3,7,15,31,62,125,250,500)

daphs <- lapply(dydx,no_daph)

daphs <- data.frame(matrix(unlist(daphs), nrow=27, byrow=T))

daphs <- cbind(line,test_temp,hypoth,daphs)

daphs <- gather(daphs, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, key = "conc", value = "offspring")

for(i in 1:length(unique(daphs$conc))){
daphs$conc[daphs$conc == unique(daphs$conc)[i]] <- conc[i]
}

daphs$line <- as.factor(daphs$line)
daphs$test_temp <- as.factor(daphs$test_temp)
daphs$conc <- as.numeric(daphs$conc)
daphs$log_conc <- log10(daphs$conc +1)

ggplot(data = daphs[daphs$hypoth == "none",], aes(x = log10(conc), y = offspring, col = hypoth, shape = test_temp))+
  geom_point(size = 4, alpha = 0.3)+
  facet_wrap(~line)

daphs2 <- daphs
daphs2$offspring <- daphs2$offspring + sample(-5:5, length(daphs2$offspring), replace = TRUE)
daphs <- rbind(daphs, daphs2)
daphs2 <- daphs
daphs2$offspring <- daphs2$offspring + sample(-5:5, length(daphs2$offspring), replace = TRUE)
daphs <- rbind(daphs, daphs2)

ggplot(data = daphs[daphs$hypoth == "none",], aes(x = log10(conc), y = offspring, col = hypoth, shape = test_temp))+
  geom_point(size = 4, alpha = 0.3)+
  facet_wrap(~line)

lm <- lm(offspring ~ log_conc*line*test_temp, data=daphs[daphs$hypoth == "none",])

summary(lm)
anova(lm)
TukeyHSD(aov(lm))
plot(TukeyHSD(aov(lm)))

daphs$log_conc <- log10(daphs$conc +1)

daphs$run <- c(rep("a", length(daphs$line)/4), rep("b", length(daphs$line)/4), rep("c", length(daphs$line)/4), rep("d", length(daphs$line)/4))

results_ec50 <- daphs %>%
  group_by(line,test_temp, hypoth, run)%>%
  summarise(
    min = min(offspring),
    max = max(offspring),
    grad = as.numeric(lm(offspring ~ log_conc)$`coefficients`[2]),
    intercept = lm(offspring ~ log_conc)$`coefficients`[1],
    ec50 = predict(lm(log_conc ~ offspring, data=daphs),newdata = data.frame(offspring = (max(offspring)+min(offspring))/4),  interval='confidence')[1],
    lwr = predict(lm(log_conc ~ offspring, data=daphs),newdata = data.frame(offspring = (max(offspring)+min(offspring))/4),  interval='confidence')[2],
    upr = predict(lm(log_conc ~ offspring, data=daphs),newdata = data.frame(offspring = (max(offspring)+min(offspring))/4),  interval='confidence')[3],
    n = n())

results_ec50$line <-  as.character(results_ec50$line)

plot <- ggplot(data = results_ec50, aes(x = test_temp, y = ec50, col = line))+
  geom_point(size = 4, alpha = 0.4)+
  geom_errorbar(mapping=aes(ymin= lwr, ymax= upr), width=0.1, size=1)+
  geom_smooth(method = 'lm', se=FALSE)+
  facet_wrap(~hypoth)
plot

lm <- lm(ec50 ~ line*test_temp, data=results_ec50[results_ec50$hypoth == "ec50",])
summary(lm)
anova(lm)
TukeyHSD(aov(lm))
plot <- plot(TukeyHSD(aov(lm)))
plot
ggsave(file = here("figures","ec50_tukey.jpeg"), plot= plot)
