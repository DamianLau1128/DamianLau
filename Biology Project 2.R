library(readxl)
library(ggplot2)
library(sjPlot)
library(ggpubr)
library(lme4)
library(lmerTest) #LMM Model
setwd("/Users/damianlau/Desktop/BIOL2102/Assignment 2")
data1 <- read_excel("3036217922.xlsx", sheet = "Dataset_1")
data2 <- read_excel("3036217922.xlsx", sheet = "Dataset_2")

# Q1 : Multivariate ANCOVA
fitq1a <- aov(Offspring_Survival_Rate ~ Average_egg_volume * Mother_Size
              + Clutch_size, data = data2)
summary(fitq1a)
fitq1b <- aov(Offspring_Survival_Rate ~ Average_egg_volume + Mother_Size
              + Clutch_size, data = data2)
summary(fitq1b)
#Compare two model (AIC)
AIC(fitq1a)
AIC(fitq1b)
# Maternal affect a offspring survival
# Clutch_size is board line significant
# Mother size size does not show a strong effects
model1 <- lm(Offspring_Survival_Rate ~ Average_egg_volume + Clutch_size
             + Mother_Size, data = data2)
summary(model1)
# egg volume is the most significant

#Visualize Q1
ggplot(data2, aes(Average_egg_volume, Offspring_Survival_Rate,
                  color = Clutch_size, size = Mother_Size)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Offspring Survival vs Egg Volume",
       x = "Average egg volume (µm³)", 
       y = "Offspring survival rate",
      color = "Clutch_size", 
      size = "Mother_Size") +
  theme_classic() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5)) +
  theme(axis.title = element_text(size = 11, face = "bold"))

#Assumption Q1
par(mfrow = c(2,2)) # make a four panel plot window
plot(model1)
shapiro.test(resid(model1))

#Q2 : Type-II ANOVA
fit1 <- aov(Offspring_Survival_Rate ~ Infection_type + Mother_type 
            + Cohort, data = data2)
summary(fit1)
fit2 <- aov(Offspring_Survival_Rate ~ Infection_type * Mother_type 
            + Cohort, data = data2)
summary(fit2)
#Compare two model (AIC)
AIC(fit1)
AIC(fit2)

# Infection type is the primary source
# because it has the highest F value and lowest p-value
model2 <- aov(Offspring_Survival_Rate ~ Infection_type + Mother_type
              + Cohort, data = data2)
summary(model2)
aov(model2)
#test Random Factor
table(data2$Mother_ID)

#Visualize Q2
ggplot(data2, aes(Infection_type, Offspring_Survival_Rate, fill = Mother_type)) +
  geom_boxplot() +
  labs(title = "Offspring Survival vs Infection Type", 
       x = "Infection type", 
       y = "Offspring survival rate") +
  theme_bw() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5)) +
  theme(axis.title = element_text(size = 11, face = "bold"))

#Assumption Q2
par(mfrow = c(2,2)) # make a four panel plot window
plot(model2)
shapiro.test(resid(model2))

#Q3 : Two-way ANOVA

fit3 <-  aov(Average_egg_volume ~ Infection_type + Mother_type
             + Cohort , data = data2)
summary(fit3)
fit4 <- aov(Average_egg_volume ~ Infection_type * Mother_type
            + Cohort , data = data2)
summary(fit4)
#Compare two model (AIC)
AIC(fit3)
AIC(fit4)

# Mother type and infection type affects the egg volume
#fits our assumption
# The high f value can explain most of the random variation 
#(more than the relatively lower p-value one)
model3 <- aov(Average_egg_volume ~ Infection_type * Mother_type + Cohort
              , data = data2)
summary(model3)

#Visulaise Q3
ggplot(data1, aes(x = Infection_type, y = Individual_Egg_volume,
                  fill = Mother_type)) +
  geom_boxplot() +
  labs(title = "Egg Volume Variation by Infection and Mother Type",
       x = "Infection Type", 
       y = "Average Egg Volume (µm³)",
       fill = "Mother Type") +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

#Assumption Q3
par(mfrow = c(2,2)) # make a four panel plot window
plot(model3)
shapiro.test(resid(model3))


# Combine all three assumption models into one 2x6 panel
par(mfrow = c(2, 6))  # 2 rows, 6 columns

# Q1 Assumptions
plot(model1, which = 1, main = "Q1: Resids vs Fitted")
plot(model1, which = 2, main = "Q1: QQ Plot")
plot(model1, which = 3, main = "Q1: Scale-Location")
plot(model1, which = 4, main = "Q1: Leverage")

# Q2 Assumptions  
plot(model2, which = 1, main = "Q2: Resids vs Fitted")
plot(model2, which = 2, main = "Q2: QQ Plot")
plot(model2, which = 3, main = "Q2: Scale-Location")
plot(model2, which = 4, main = "Q2: Leverage")

# Q3 Assumptions
plot(model3, which = 1, main = "Q3: Resids vs Fitted")
plot(model3, which = 2, main = "Q3: QQ Plot")
plot(model3, which = 3, main = "Q3: Scale-Location")
plot(model3, which = 4, main = "Q3: Leverage")

par(mfrow = c(1, 1))  # Reset panel


#Q4a : Linear Mixed Model (Developing Time)
fit5 <- lmer(Development_time ~ Infection_type * Offspring_type + Cohort
             + (1 | Mother_ID), data = data1)
summary(fit5)
fit6 <- lmer(Development_time ~ Infection_type + Offspring_type + Cohort
             + (1 | Mother_ID), data = data1)
summary(fit6)
#Compare two model (AIC)
AIC(fit5)
AIC(fit6)
#The Lower AIC value, the more we want to use

#Q4b : Linear Mixed Model (Longevity)
fit7 <- lmer(Offspring_Adult_size ~ Infection_type * Offspring_type + Cohort
             + (1 | Mother_ID), 
             data = data1)
summary(fit7)
fit8 <- lmer(Offspring_Adult_size ~ Infection_type + Offspring_type + Cohort
             + (1 | Mother_ID), 
             data = data1)
summary(fit8)
#Compare two model (AIC)
AIC(fit7)
AIC(fit8)

#Q4c : Linear Mixed Model (Size)
fit9 <- lmer(Offspring_adult_longevity ~ Infection_type * Offspring_type + Cohort
             + (1 | Mother_ID), 
             data = data1)
summary(fit9)
fit10 <- lmer(Offspring_adult_longevity ~ Infection_type + Offspring_type + Cohort
              + (1 | Mother_ID), 
             data = data1)
summary(fit10)
#Compare two model (AIC)
AIC(fit9)
AIC(fit10)

#Model for 4a : LMM (Developing Time)
model4a <- lmer(Development_time ~ Infection_type + Offspring_type + Cohort
     + (1 | Mother_ID), data = data1)
summary(model4a)

#Visualize 4a
plot4a <- ggplot(data1, aes(x = Infection_type, y = Development_time, 
                            fill = Offspring_type)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  labs(x = "Infection Type", 
       y = "Development Time",
       fill = "Offspring Type",
       title = "Development Time vs Offspring") +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

#Assumption Q4a
par(mfrow = c(2,2)) # make a four panel plot window
plot(model4a)
shapiro.test(resid(model4a))

#Model for 4b : LMM (Size)
model4b <- lmer(Offspring_Adult_size ~ Infection_type + Offspring_type + Cohort
     + (1 | Mother_ID), data = data1)
summary(model4b)

#Visualize 4b
plot4b <- ggplot(data1, aes(x = Infection_type, y = Offspring_Adult_size,
                            fill = Offspring_type)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  labs(x = "Infection Type", 
       y = "Size",
       fill = "Offspring Type",
       title = "Size vs Offspring") +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

#Assumption Q4b
par(mfrow = c(2,2)) # make a four panel plot window
plot(model4b)
shapiro.test(resid(model4b))

#Model for 4c : LMM (Longevity)
model4c <- lmer(Offspring_adult_longevity ~ Infection_type + Offspring_type
                + Cohort + (1 | Mother_ID), data = data1)
summary(model4c)

#Visualize 4c
plot4c <- ggplot(data1, aes(x = Infection_type, y = Offspring_adult_longevity,
                            fill = Offspring_type)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  labs(x = "Infection Type", 
       y = "Longevity (days)",
       fill = "Offspring Type",
       title = "Longevity vs Offspring") +
  theme_bw(base_size = 16) +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

#Assumption Q4c
par(mfrow = c(2,2)) # make a four panel plot window
plot(model4c)
shapiro.test(resid(model4c))

#Combine the three visualized graph
# Arrange all three plots in one page
combined_plot <- ggarrange(plot4a, plot4b, plot4c,
                           ncol = 2, nrow = 2,  # Creates a 2x2 grid (4 spots, but we only use 3)
                           common.legend = TRUE,  # Use one legend for all plots
                           legend = "bottom")     # Position legend at bottom

# Display the combined plot
combined_plot

#Combine the assumption for Q4
par(mfrow = c(3, 4))  # 3 rows (one for each model), 4 columns (diagnostic plots)

# Q4a: Development Time Model
# Residuals vs Fitted
plot(fitted(model4a), resid(model4a), 
     main = "Q4a: Dev Time - Resids vs Fitted",
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")

# Q-Q Plot
qqnorm(resid(model4a), main = "Q4a: Dev Time - QQ Plot")
qqline(resid(model4a))

# Scale-Location
plot(fitted(model4a), sqrt(abs(resid(model4a))), 
     main = "Q4a: Dev Time - Scale-Location", 
     xlab = "Fitted", ylab = "sqrt(|Residuals|)")

# Histogram of residuals
hist(resid(model4a), main = "Q4a: Dev Time - Residuals", 
     xlab = "Residuals")

# Q4b: Adult Size Model
# Residuals vs Fitted
plot(fitted(model4b), resid(model4b), 
     main = "Q4b: Adult Size - Resids vs Fitted",
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")

# Q-Q Plot
qqnorm(resid(model4b), main = "Q4b: Adult Size - QQ Plot")
qqline(resid(model4b))

# Scale-Location
plot(fitted(model4b), sqrt(abs(resid(model4b))), 
     main = "Q4b: Adult Size - Scale-Location", 
     xlab = "Fitted", ylab = "sqrt(|Residuals|)")

# Histogram of residuals
hist(resid(model4b), main = "Q4b: Adult Size - Residuals", 
     xlab = "Residuals")

# Q4c: Longevity Model
# Residuals vs Fitted
plot(fitted(model4c), resid(model4c), 
     main = "Q4c: Longevity - Resids vs Fitted",
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")

# Q-Q Plot
qqnorm(resid(model4c), main = "Q4c: Longevity - QQ Plot")
qqline(resid(model4c))

# Scale-Location
plot(fitted(model4c), sqrt(abs(resid(model4c))), 
     main = "Q4c: Longevity - Scale-Location", 
     xlab = "Fitted", ylab = "sqrt(|Residuals|)")

# Histogram of residuals
hist(resid(model4c), main = "Q4c: Longevity - Residuals", 
     xlab = "Residuals")

par(mfrow = c(1, 1))  # Reset panel