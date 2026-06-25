library(ggplot2)
library(sjPlot)
setwd("/Users/damianlau/Desktop/BIOL2102/Assignment 1")
mountains <- read.csv("3036217922.csv")
str(mountains)

#'Not shaded' = Baseline
## Wide range of Residual values , means catechins production is not only depending on shade (Minor)
###Estimate value = negative -> less catechin production in shaded area
Estimates <- lm(catechins ~ shade, data = subset(mountains, shade =="Not Shaded"|shade=="Shaded"))
summary(Estimates)
#I got positive estimated value -> opposite of my hypothesis -> We need a multivariable model to prevent misleading

# Modelling the residue vs your measuring value
model <- lm(catechins ~ shade * elevation + mountain, data = mountains)
#Estimate value of shaded area & elevation are both negative value respectively -> "highly significant"
## Also means both affects the catechins production
# have shade decrease more than no shade
#high altitude produce less catechins than low altitude
###Interacton of both : High latitude, means lower reduction of catechins production

#Summary Graph
summary(model)

#Test Normality
mean(model$residuals)

# QQ-plot Function to show the Normality
ggplot(model, aes(sample = .resid)) +
  geom_qq_line(colour = "#FF2288") +
  geom_qq(size = 2, alpha = 0.5) + 
  ylab("Sample Quantiles") +
  xlab("Theoretical Quantiles") +
  theme_classic() +
  ggtitle("Normal Q-Q Plot") +
  theme (plot.title = element_text(hjust = 0.5, face = "bold"))

# Test for Normality
shapiro.test(model$residuals)

#Fitted vs. Residuals (Homoscedasticity)
ggplot(model, aes(x = .fitted, y = .resid)) +
  geom_point(size = 3, alpha = 0.3) + 
  geom_hline(yintercept = 0, size = 2) +
  ylab("Rsiduals") +
  xlab("Fitted values") +
  theme_classic() +
  ggtitle("Residuals vs Fitted Values (Homoscedasticity)") +
  theme (plot.title = element_text(hjust = 0.5, face = "bold"))

#The association of shaded and altitude affecting catechin production in different mountain
## plotting the model
plot_model(model, 
           type = "pred", #prediction
           terms = c("elevation", "mountain", "shade"),
           mdrt.values = "quart", #setting determines which values of your continuous variables are used for plotting predictions.
           show.data = TRUE, ## Show data points
           legend.title = "Mountain") +
  labs(title = "Elevation and Shaded Effects on Catechin Production", # Label the title ad x,y axis
       subtitle = "Interactive effects across three mountains",
       x = "Elevation", 
       y = "Catechin Concentration",
       color = "Mountain",
       linetype = "Shade Treatment") +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom",  # or "top", "right"
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2)
  )