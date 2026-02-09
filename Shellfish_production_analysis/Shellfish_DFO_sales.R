#Plotting BC shellfish sales

#data from: https://www.dfo-mpo.gc.ca/stats/aqua/aqua-prod-eng.htm

compilation_of_2019_21_seafood_production_data_for_posting_in_bcdc_xlsx <- read_excel("~/Documents/compilation-of-2019-21-seafood-production-data-for-posting-in-bcdc-xlsx.xlsx", 
                                                                                     +     sheet = "R2")

Data <- compilation_of_2019_21_seafood_production_data_for_posting_in_bcdc_xlsx

head(Data)

#Packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggsci)
library(viridis)
#install.packages("ggpmisc")
library(ggpmisc)
#library
#install.packages("dplyr")
library(dplyr)
#install.packages("datarium")
library("datarium")
#install.packages("tidyverse")
library(tidyverse)
#install.packages("ggpubr")
library(ggpubr)
library(rstatix)
#install.packages("ggResidpanel")
library(ggResidpanel)
#install.packages("DHARMa")
library(DHARMa)
#install.packages("lme4")
library(lme4)
#install.packages("fitdistrplus")
library(fitdistrplus)
library(ggplot2)
#install.packages("hrbrthemes")
library(hrbrthemes)
library(tidyr)
#install.packages("viridis")
library(viridis)
library(car)
#install.packages("agricolae")
library(agricolae)
#install.packages("mgcv")
library(mgcv)
#install.packages("glmmTMB")
library(glmmTMB)
#install.packages("mgcViz")
library(mgcViz)

#Add custom theme
theme.marissa <- function() {
  theme_classic(base_size = 14) +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 16))
}

theme_set(theme.marissa())

# Reshape data to long format
Data_long <- Data %>%
  pivot_longer(cols = -`Species/Product`, names_to = "Year", values_to = "Tonnes")

Data_long <- Data_long %>%
  filter(!is.na(`Species/Product`))

# Create the shaded line graph

#Remove extra info not needed
Data_long$`Species/Product` <- factor(Data_long$`Species/Product`, 
                                      levels = c("Oysters", "Clams", "Mussels", "Scallops", "Other"))

#make year numeric
Data_long$Year <- as.numeric(Data_long$Year)

ggplot(Data_long, aes(x = Year, y = Tonnes, color = `Species/Product`, fill = `Species/Product`, group = `Species/Product`)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = 0, ymax = Tonnes), alpha = 0.7) +
  scale_fill_npg() +  
  scale_color_npg() +  
  labs(title = "", x = "Year", y = "Production (Tonnes)") +
  scale_x_continuous(breaks = seq(min(Data_long$Year), max(Data_long$Year), by = 3)) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),          # Increase x-axis label size
    axis.text.y = element_text(size = 14, face = "bold"),                                  # Increase y-axis label size
    axis.title.x = element_text(size = 16, face = "bold"),                  # Increase x-axis title size and make bold
    axis.title.y = element_text(size = 16, face = "bold"),                  # Increase y-axis title size and make bold
    axis.line = element_line(size = 1),                                   # Make axis lines bolder
    panel.grid = element_blank(),
    legend.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size=16, face = "bold")
  ) 
#+ geom_vline(xintercept = "1996", linetype = "dotted", color = "black", size = 0.6)


#Add regression line
Data_total <- Data_long %>%
  group_by(Year) %>%
  summarize(Total_Tonnes = sum(Tonnes, na.rm = TRUE))

# Create the plot with the regression line
ggplot(Data_long, aes(x = Year, y = Tonnes, color = `Species/Product`, fill = `Species/Product`, group = `Species/Product`)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = 0, ymax = Tonnes), alpha = 0.6) +
  geom_smooth(method = "gam", se = FALSE, linetype = "dashed", size = 1.2) +  # Add individual regression lines
  scale_fill_npg() +  
  scale_color_npg() +  
  labs(title = "", x = "Year", y = "Production (Tonnes)") +
  scale_x_continuous(breaks = seq(min(Data_long$Year), max(Data_long$Year), by = 3)) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),          # Increase x-axis label size
    axis.text.y = element_text(size = 14, face = "bold"),                                  # Increase y-axis label size
    axis.title.x = element_text(size = 16, face = "bold"),                  # Increase x-axis title size and make bold
    axis.title.y = element_text(size = 16, face = "bold"),                  # Increase y-axis title size and make bold
    axis.line = element_line(size = 1),                                   # Make axis lines bolder
    panel.grid = element_blank(),
    legend.text = element_text(size = 16, face = "bold"),
    legend.title = element_text(size=16, face = "bold")
  ) 


#Add regression formulas
ggplot(Data_long, aes(x = Year, y = Tonnes, color = `Species/Product`, fill = `Species/Product`, group = `Species/Product`)) +
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = Tonnes), alpha = 0.5) +
  geom_smooth(method = "gam", se = FALSE, linetype = "dashed", size = 1.2) +
  stat_poly_eq(
    aes(label = paste("bold(", ..eq.label.., ")", "~~~", "bold(", ..rr.label.., ")")),
    formula = y ~ x,
    parse = TRUE,
    size = 4.4,                   
    label.x.npc = "left",
    label.y.npc = "top"
  ) +
  scale_fill_npg() +
  scale_color_npg() +
  labs(title = "", x = "Year", y = "Production (Tonnes)") +
  scale_x_continuous(breaks = seq(min(Data_long$Year), max(Data_long$Year), by = 3)) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),          # Increase x-axis label size
    axis.text.y = element_text(size = 14, face = "bold"),                                  # Increase y-axis label size
    axis.title.x = element_text(size = 16, face = "bold"),                  # Increase x-axis title size and make bold
    axis.title.y = element_text(size = 16, face = "bold"),                  # Increase y-axis title size and make bold
    axis.line = element_line(size = 1),                                   # Make axis lines bolder
    panel.grid = element_blank(),
    legend.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size=16, face = "bold")
  ) 

# Make sure Year is numeric (this is crucial!)

ggplot(Data_long, aes(x = Year, y = Tonnes, color = `Species/Product`, fill = `Species/Product`, group = `Species/Product`)) +
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = Tonnes), alpha = 0.5) +
  geom_smooth(method = "gam", se = FALSE, linetype = "dashed", size = 1.5, alpha = 1) +
  stat_poly_eq(
    aes(label = paste("bold(", ..eq.label.., ")", "~~~", "bold(", ..rr.label.., ")")),
    formula = y ~ x,
    parse = TRUE,
    size = 4.4,
    label.x.npc = "left",
    label.y.npc = "top"
  ) +
  scale_x_continuous(
    breaks = seq(min(Data_long$Year), max(Data_long$Year), by = 2)  # change 'by' to 1, 2, or 5 as needed
  ) +
  scale_fill_npg() +
  scale_color_npg() +
  labs(title = "", x = "Year", y = "Production (Tonnes)") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.line = element_line(size = 0.5),
    panel.grid = element_blank(),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold")
  )


#Run model and test fit
install.packages("gamm4")
library(gamm4)


Data_long$Product <- Data_long$`Species/Product`

Data_long_oyster <- Data_long %>%
  filter(Product %in% c("Oysters"))

Mod4 <- gam(Tonnes ~ s(Year, by = Product, k = 3) + Product + Year, 
              data = Data_long)
summary(Mod4)
plot(Mod4)

simulation_output <- simulateResiduals(Mod4) 
plot(simulation_output)

#Analyze each product seperately with a gam
#Oysters GAM ----
lm_oyster <- lm(Tonnes ~ Year, data = Data_long_oyster)
summary(lm_oyster)
plot(lm_oyster)

gam_oyster <- gam(Tonnes ~ s(Year, k = 3), data = Data_long_oyster)  # k = smoothness
summary(gam_oyster)
plot(gam_oyster, shade = TRUE)

AIC(lm_oyster, gam_oyster)
par(mfrow = c(2, 2))
gam.check(gam_oyster)

simulation_output <- simulateResiduals(gam_oyster) 
plot(simulation_output)


#Clams GAM ----
Data_long_clams <- Data_long %>%
  filter(Product %in% c("Clams"))

lm_clams <- lm((Tonnes) ~ Year, data = Data_long_clams)
summary(lm_clams)
plot(lm_clams)
qqnorm(residuals(lm_clams))
plot(fitted(lm_clams)~residuals(lm_clams))
resid_panel(lm_clams)

gam_clams <- gam(Tonnes ~ s(Year, k = 9),
                 data = Data_long_clams,
                 family = tw())

summary(gam_clams)
plot(gam_clams, shade = TRUE)

AIC(lm_clams, gam_clams)
par(mfrow = c(2, 2))
gam.check(gam_clams)

simulation_output <- simulateResiduals(gam_clams)
plot(simulation_output)

install.packages("gratia")
library(gratia)
appraise(gam_clams)  # from gratia package
draw(gam_clams)

concurvity(gam_clams, full = TRUE) #no concurvity

#Mussels GAM ----
Data_long_mus <- Data_long %>%
  filter(Product %in% c("Mussels"))

lm_mus <- lm((Tonnes) ~ Year, data = Data_long_mus)
summary(lm_mus)
plot(lm_mus)
qqnorm(residuals(lm_mus))
plot(fitted(lm_mus)~residuals(lm_mus))
resid_panel(lm_mus)

gam_mus <- gam(Tonnes ~ s(Year, k = 8),
                 data = Data_long_mus,
               family = tw())

summary(gam_mus)
plot(gam_mus, shade = TRUE)

AIC(lm_mus, gam_mus)
par(mfrow = c(2, 2))
gam.check(gam_mus)

simulation_output <- simulateResiduals(gam_mus)
plot(simulation_output)

concurvity(gam_mus, full = TRUE)


#Scallops GAM ----
Data_long_scal <- Data_long %>%
  filter(Product %in% c("Scallops"))

lm_scal <- lm((Tonnes) ~ Year, data = Data_long_scal)
summary(lm_scal)
plot(lm_scal)
resid_panel(lm_scal)

gam_scal <- gam((Tonnes) ~ s(Year, k = 9),
               data = Data_long_scal,
               family = tw())

summary(gam_scal)
plot(gam_scal, shade = TRUE)

AIC(lm_scal, gam_scal)
par(mfrow = c(2, 2))
gam.check(gam_scal)

simulation_output <- simulateResiduals(gam_scal)
plot(simulation_output)

concurvity(gam_mus, full = TRUE)

Data_long_scal <- Data_long_scal %>%
  mutate(Predicted = predict(gam_scal, type = "response"))

ggplot(Data_long_scal, aes(x = Year, y = Tonnes)) +
  geom_point(color = "gray50") +
  geom_line(aes(y = Predicted), color = "blue", size = 1.2) +
  labs(title = "Observed vs GAM-predicted Scallop Production",
       y = "Tonnes")
range(Data_long_scal$Predicted)

#Find year things changed for oyster production
#Change pt analysis ----
library(segmented)

# Fit segmented (piecewise linear) model
seg_mod <- segmented(lm_oyster, seg.Z = ~Year)
summary(seg_mod)
plot(seg_mod)
AIC(seg_mod, seg_mod_log)

#Calc confindence intervals
confint(seg_mod)

par(mfrow = c(2, 2))
plot(seg_mod$model)

par(mfrow = c(2, 2))

# Residuals vs Fitted
plot(fitted(seg_mod), residuals(seg_mod), 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# Q-Q Plot
qqnorm(residuals(seg_mod), main = "Normal Q-Q")
qqline(residuals(seg_mod), col = "red")

# Scale-Location (Spread vs Fitted)
plot(fitted(seg_mod), sqrt(abs(residuals(seg_mod))), 
     xlab = "Fitted values", ylab = "Sqrt(|Residuals|)",
     main = "Scale-Location")
abline(h = 0, col = "red")

#May not fit, try log transformation
seg_mod_log <- segmented(lm(log(Tonnes) ~ Year, data = Data_long_oyster), seg.Z = ~Year)
summary(seg_mod_log)
plot(seg_mod_log)
par(mfrow = c(2, 2))

# Residuals vs Fitted
plot(fitted(seg_mod_log), residuals(seg_mod_log), 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# Q-Q Plot
qqnorm(residuals(seg_mod_log), main = "Normal Q-Q")
qqline(residuals(seg_mod_log), col = "red")

# Scale-Location (Spread vs Fitted)
plot(fitted(seg_mod_log), sqrt(abs(residuals(seg_mod_log))), 
     xlab = "Fitted values", ylab = "Sqrt(|Residuals|)",
     main = "Scale-Location")
abline(h = 0, col = "red")

#Log not much better, untransformed may be fine









#Run analysis from 2004 to 2023 for oysters only
Data_long$Year <- as.numeric(Data_long$Year)

Data_long_time <- Data_long %>%
  filter(Product == "Oysters", Year >= 2004, Year <= 2023)


ggplot(Data_long_time, aes(x = Year, y = Tonnes, color = `Species/Product`, fill = `Species/Product`, group = `Species/Product`)) +
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = Tonnes), alpha = 0.5) +
  geom_smooth(method = "gam", se = FALSE, linetype = "dashed", size = 1.5, alpha = 1) +
  stat_poly_eq(
    aes(label = paste("bold(", ..eq.label.., ")", "~~~", "bold(", ..rr.label.., ")")),
    formula = y ~ x,
    parse = TRUE,
    size = 4.4,
    label.x.npc = "left",
    label.y.npc = "top"
  ) +
  scale_x_continuous(
    breaks = seq(min(Data_long$Year), max(Data_long$Year), by = 2)  # change 'by' to 1, 2, or 5 as needed
  ) +
  scale_fill_npg() +
  scale_color_npg() +
  labs(title = "", x = "Year", y = "Production (Tonnes)") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.line = element_line(size = 0.5),
    panel.grid = element_blank(),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold")
  )

#Linear trend may not be capturing data - does not look like a linear increase in production in later years (after 2010)
#Try using a GAM to capture wiggles in data


str(Data_long)
Data_long$Year <- as.numeric(Data_long$Year)

#Analyze each product seperately
#Look at oysters


Data_long$Product <- Data_long$`Species/Product`
Data_long_oysters <- Data_long %>%
  filter(Product == "Oysters")

model_gam <- gam(Tonnes ~ s(Year, by=Product, k = 10), data = Data_long_oysters)
summary(model_gam) 
plot(model_gam, pages = 1, shade = TRUE)
gam.check(model_gam) 
concurvity(model_gam, full = TRUE)
AIC(model_gam)

#Dharma Model check fit
simulation_output <- simulateResiduals(model_gam) 
plot(simulation_output)

#Check for outliers
# Extract residuals
Data_long_oysters$resid <- residuals(model_gam)

# Identify outliers in residuals (e.g., using 2 standard deviations)
threshold <- 2 * sd(Data_long_oysters$resid)

outliers_resid <- Data_long_oysters %>%
  filter(abs(resid) > threshold)

outliers_resid

# Remove the rows in `outliers_resid` from `Data_long_oysters` and try GAM again
Data_no_outliers <- anti_join(Data_long_oysters, outliers_resid)
model_gam <- gam(Tonnes ~ s(Year, by=Product, k = 10), data = Data_no_outliers)
summary(model_gam) 
plot(model_gam, pages = 1, shade = TRUE)
simulation_output <- simulateResiduals(model_gam) 
plot(simulation_output) #Fits now, so outliers causing deviations

#Since outliers are true data points, will keep. 

vis.gam(model_gam, 
        type = 'response', 
        plot.type = 'contour', 
        color = "heat",         
        contour.col = "black")  
