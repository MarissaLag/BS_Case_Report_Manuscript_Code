#BS review MHW data
#Analysis of temperature trends in Baynes Sound (BS) British Columbia
#Written by Marissa Wright-LaGreca (Github: MarissaLag)

#Resources used:
#https://cran.r-project.org/web/packages/itsadug/vignettes/acf.html #autocorrelation checks
#https://timeseriesreasoning.com/contents/generalized-least-squares/ #GLS/autocorrelation reasoning

#Packages ----
install.packages(c(
  "readxl",
  "dplyr",
  "ggplot2",
  "car",
  "glmmTMB",
  "lme4",
  "emmeans",
  "ggeffects",
  "DHARMa",
  "lubridate",
  "gridExtra",
  "MuMIn",
  "lubridate",
  "MASS",
  "sandwich",
  "lmtest"
))

library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(glmmTMB)
library(lme4)
library(emmeans)
library(ggeffects)
library(ggplot2)
library(DHARMa)
library(lubridate)
library(gridExtra)
library(MuMIn)
library(lubridate)
library(MASS)
library(sandwich)  # For HAC standard errors
library(lmtest)    # For coeftest

#Load data ----
MHW_raw_data <- read_excel("~/Documents/PhD/Marine_heatwave_BS_review/MHW_raw_data.xlsx", 
                             +     sheet = "MHW_R")
Data <- MHW_raw_data
str(Data)
Data$MHW <- as.numeric(Data$MHW)

#Set theme ----
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

#set seed
set.seed(2376)

#MHW Frequency model ----
#Data wrangling
Data_MHW <- Data %>%
  arrange(Year, Month, Day) %>%
  mutate(MHW = as.numeric(MHW),
         new_event = ifelse(MHW == 1 & lag(MHW, default = 0) == 0, 1, 0)) %>%
  group_by(Year) %>%
  summarise(MHW_events = sum(new_event, na.rm = TRUE))

#trying to log transform data - fixed heterosked issues
Data_MHW_log <- Data %>%
  arrange(Year, Month, Day) %>%
  mutate(MHW = as.numeric(MHW),
         new_event = ifelse(MHW == 1 & lag(MHW, default = 0) == 0, 1, 0)) %>%
  group_by(Year) %>%
  summarise(MHW_events = sum(new_event, na.rm = TRUE)) %>%
  mutate(log_MHW_events = log1p(MHW_events))

#Try removing outlier years 2015, 2017,and 2010
#Removing data made no difference to model fit
# Data_filter <- Data_MHW %>%
#   filter(!Year %in% c(2015, 2010, 2017))

# Linear trends - normal but still has fanning issues
lm_freq <- lm(log_MHW_events ~ Year, data = Data_MHW_log)
summary(lm_freq)
par(mfrow = c(2, 2))
plot(lm_freq)

#Checking for autocorrelation - autocorrelation not present
durbinWatsonTest(lm_freq)  # values near 2 = no autocorrelation
acf(residuals(lm_freq))    # visually check lag-1 autocorrelation

#Use locally estimated scatterplot smoothing (loess) to visualize trends (better than gams for smaller datasets)
#loess is not inferential (i.e. give p values)
loess_fit <- loess(log_MHW_events ~ Year, data = Data_MHW_log, span = 0.75)
summary(loess_fit)

Data_MHW_log$loess_pred <- predict(loess_fit)

ggplot(Data_MHW_log, aes(x = Year, y = log_MHW_events)) +
  geom_point() +
  geom_line(aes(y = loess_pred), color = "blue", size = 1.2)

ggplot(Data_MHW_log, aes(Year, log_MHW_events)) +
  geom_point(size = 2) +
  geom_line(aes(y = predict(loess_fit)), color = "blue", size = 1.2, linetype = "dashed") +
  geom_abline(intercept = coef(gls_freq)[1], slope = coef(gls_freq)[2],
              color = "red", size = 1) +
  labs(y = "log(MHW frequency)", title = "Observed data with LOESS and GLS trends")

#Loess shows the data may be non-linear - may have to use a GAM
gam_fit <- gam(log_MHW_events ~ s(Year), data = Data_MHW_log)
summary(gam_fit) #signif edf = 1, indicates data is roughly linear, gam not needed
par(mfrow = c(2, 2))
gam.check(gam_fit)

#Trying a negative binomial glm with heteroskedacity & autocorrelation (HAC) correction
# Compare different models
glm_poisson <- glm(MHW_events ~ Year, data = Data_MHW, family = poisson)
glm_nb <- glm.nb(MHW_events ~ Year, data = Data_MHW)

# AIC comparison
aic_comparison <- AIC(glm_poisson, glm_nb)
aic_comparison$delta_AIC <- aic_comparison$AIC - min(aic_comparison$AIC)
print(aic_comparison) #nbGLM much better

###Final frequency model - NBglm ----
glm_nb <- glm.nb(MHW_events ~ Year, data = Data_MHW) #without HAC correction
summary(glm_nb)
par(mfrow = c(2, 2))
plot(glm_nb)

# Get HAC-corrected results
results_hac <- coeftest(glm_nb, vcov = vcovHAC(glm_nb))
print(results_hac) 

# Compare standard vs HAC standard errors
cat("\nStandard Error Comparison:\n")
cat("Standard NB SE:", summary(glm_nb)$coefficients["Year", "Std. Error"], "\n")
cat("HAC-corrected SE:", results_hac["Year", "Std. Error"], "\n") #HAC correction reduced SE by ~9%

# Coefficient and HAC standard error
coef_year <- coef(glm_nb)["Year"]
se_hac <- results_hac["Year", "Std. Error"]
z_value <- results_hac["Year", "z value"]
p_value <- results_hac["Year", "Pr(>|z|)"]

cat("\n=== FINAL RESULTS ===\n")
cat("Coefficient:", round(coef_year, 5), "\n")
cat("SE (HAC):", round(se_hac, 5), "\n")
cat("z-value:", round(z_value, 3), "\n")
cat("p-value:", format.pval(p_value, digits = 3), "\n")

# Calculate annual % increase
annual_increase <- (exp(coef_year) - 1) * 100
cat("\nAnnual increase:", round(annual_increase, 2), "%\n")

# 95% Confidence Interval
ci_lower <- coef_year - 1.96 * se_hac
ci_upper <- coef_year + 1.96 * se_hac
cat("95% CI: [", round(ci_lower, 4), ",", round(ci_upper, 4), "]\n")
cat("95% CI (% per year): [", 
    round((exp(ci_lower)-1)*100, 2), "%,", 
    round((exp(ci_upper)-1)*100, 2), "%]\n")

# Calculate dispersion parameter
Data_MHW$fitted_nb <- predict(glm_nb, type = "response")
pearson_chisq <- sum((Data_MHW$MHW_events - Data_MHW$fitted_nb)^2 / Data_MHW$fitted_nb)
df <- nrow(Data_MHW) - length(coef(glm_nb))
dispersion <- pearson_chisq / df #1.56 - still has mild overdispersion but typical for nbglm so acceptable

#Get prediction data
pred_data <- data.frame(Year = seq(1969, 2019, by = 1))
preds <- predict(glm_nb, newdata = pred_data, type = "link", se.fit = TRUE)

pred_data$fit <- exp(preds$fit)
pred_data$lower <- exp(preds$fit - 1.96 * preds$se.fit)
pred_data$upper <- exp(preds$fit + 1.96 * preds$se.fit)

# Plot model predictions
ggplot() +
  geom_ribbon(data = pred_data, 
              aes(x = Year, ymin = lower, ymax = upper),
              fill = "lightblue", alpha = 0.4) +
  geom_line(data = pred_data, 
            aes(x = Year, y = fit),
            color = "blue", size = 1.2) +
  geom_point(data = Data_MHW, 
             aes(x = Year, y = MHW_events),
             size = 2.5, alpha = 0.7) +
  labs(
    x = "Year",
    y = "Number of MHW Events",
    title = "Marine Heatwave Frequency (1969-2019)",
    subtitle = "Negative Binomial GLM with 95% confidence interval"
  )

#look for outliers in nbglm
# Identify the outlier years
Data_MHW$std_resid <- rstandard(glm_nb)
outliers <- Data_MHW[abs(Data_MHW$std_resid) > 2, ]

cat("Outlier observations:\n")
print(outliers[, c("Year", "MHW_events", "fitted_nb", "std_resid")])

# Visualize outliers
plot(Data_MHW$Year, Data_MHW$std_resid,
     xlab = "Year", ylab = "Standardized Residuals",
     main = "Standardized Residuals with Outliers Highlighted",
     ylim = c(-3, max(Data_MHW$std_resid) + 0.5))
abline(h = 0, col = "gray", lwd = 2)
abline(h = c(-2, 2), col = "red", lty = 2)
points(outliers$Year, outliers$std_resid, col = "red", pch = 19, cex = 1.5)
text(outliers$Year, outliers$std_resid, labels = outliers$Year, pos = 3, col = "red")

#pseudo R^2
# Fit null model
glm_nb_null <- glm.nb(MHW_events ~ 1, data = Data_MHW)

#psuedo R^2 with likelihood approach
r.squaredLR(glm_nb)

# Extract Pearson residuals
res <- residuals(glm_nb, type = "pearson")
acf(res, main = "ACF of NB GLM residuals")
sim <- simulateResiduals(glm_nb)
plot(sim)           # general diagnostics
testTemporalAutocorrelation(sim, time = Data_MHW$Year)

#Calculate # of increased MHWs per year during this period
years <- 1969:2019
predicted <- exp(-101.302 + 0.051 * years)
annual_increase <- predicted[2:length(predicted)] - predicted[1:(length(predicted)-1)]
mean_increase <- mean(annual_increase)
mean_increase

###Frequency plot ----
p1 <- ggplot(Data_MHW, aes(x = Year, y = MHW_events)) +
  geom_line(color = "black", size = 1, linewidth = 1.3) +
  geom_point(color = "black", size = 2.7) +
  geom_smooth(method = "glm", color = "black", linetype = "dashed") +
  # stat_regline_equation(
  #   aes(label = ..eq.label..),
  #   label.x = min(Data$Year) + 1,
  #   label.y = max(Data$log_MHW_events, na.rm = TRUE)
  # ) +
  # stat_cor(
  #   aes(label = ..rr.label..),
  #   label.x = min(Data$Year) + 1,
  #   label.y = max(Data$log_MHW_events, na.rm = TRUE) -0.2
  # ) +
  labs(title = "", y = "Mean Frequency", x = "") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),          
    axis.text.y = element_text(size = 14, face = "bold"),                                  
    axis.title.y = element_text(size = 17, face = "bold"),               
    axis.line = element_line(size = 1),                                 
  ) 

#MHW Intensity model ----
#I do not think it is correct to include years with no MHWs for calculating change
#in MHW intensity and duration - as is confounding MHW frequency with duration and intensity
mhw_summary <- Data %>%
  filter(MHW == 1) 

#calculate mean mhw intensity per year
mhw_yearly <- mhw_summary %>%
  filter(!is.na(Intensity)) %>%
  group_by(Year) %>%
  summarise(avg_intensity = mean(Intensity), .groups = "drop")

# Linear trends
lm_intensity <- lm(Intensity ~ Year, data = mhw_summary)
summary(lm_intensity)
par(mfrow = c(2, 2))
plot(lm_intensity)

#Checking for autocorrelation - autocorrelation not present, not needed for intensity
durbinWatsonTest(lm_intensity)  # values near 2 = no autocorrelation
acf(residuals(lm_intensity))    # visually check lag-1 autocorrelation

#may be non linear - test with GAM - does not improve model fit
gam_intensity <- gam(Intensity ~ s(Year, k =3), data = mhw_summary)
summary(gam_intensity)
gam.check(gam_intensity)


###Intensity plot ----
p2 <- ggplot(mhw_yearly, aes(x = Year, y = avg_intensity)) +
  geom_line(color = "black", size =1, linewidth = 1.3) +
  geom_point(color = "black", size = 2.7) +
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +
  # stat_regline_equation(
  #   label.x = min(mhw_summary_complete$Year) + 1,
  #   label.y = max(mhw_summary_complete$avg_intensity, na.rm = TRUE) + 1,
  #   aes(label = ..eq.label..)
  # ) +
  # stat_cor(
  #   label.x = min(mhw_summary_complete$Year) + 1,
  #   label.y = max(mhw_summary_complete$avg_intensity, na.rm = TRUE) + 0.7,
  #   aes(label = ..rr.label..)
  # ) +
  labs(title = "", y = "Mean Intensity (°C)", x = "Year") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),          
    axis.text.y = element_text(size = 14, face = "bold"),                                  
    axis.title.y = element_text(size = 17, face = "bold"),  
    axis.title.x = element_text(size = 17, face = "bold"), 
    axis.line = element_line(size = 1),                                 
  ) 

#MHW Duration model ----
#calculate mean mhw intensity per year
mhw_yearly_duration <- mhw_summary %>%
  filter(!is.na(Duration)) %>%
  group_by(Year) %>%
  summarise(avg_duration = mean(Duration), .groups = "drop")

# Linear trends
lm_duration <- lm(Duration ~ Year, data = mhw_summary)
summary(lm_duration)
par(mfrow = c(2, 2))
plot(lm_duration)

#Checking for autocorrelation - autocorrelation not present, not needed for intensity
durbinWatsonTest(lm_duration)  # values near 2 = no autocorrelation
acf(residuals(lm_duration))    # visually check lag-1 autocorrelation

# Fit GLM with Gamma family and log link
glm_duration <- glm(Duration ~ Year,
                    data = mhw_summary,
                    family = Gamma(link = "log"))

# Summary
summary(glm_duration)
# Check residuals
par(mfrow = c(2,2))
plot(glm_duration, which = 1:4)  # Residuals vs Fitted, Normal Q-Q, Scale-Location, Cook's distance

# Optional: check for overdispersion
dispersion <- sum(residuals(glm_duration, type = "deviance")^2) / glm_duration$df.residual
dispersion  # should be ~1, if >1 indicates overdispersion


# Fit GAM with smooth Year term
gam_duration <- gam(Duration ~ s(Year, k = 10),
                    data = mhw_summary,
                    family = Gamma(link = "log"))

# Summary
summary(gam_duration)
gam.check(gam_duration)
plot(gam_duration, se = TRUE, shade = TRUE,
     main = "MHW Duration over Years (GAM, Gamma)")

#Runnning lm with log transformation
# Calculate log-transformed average duration
mhw_yearly_duration <- mhw_summary %>%
  filter(!is.na(Duration)) %>%
  group_by(Year) %>%
  summarise(avg_duration = mean(Duration), .groups = "drop") %>%
  mutate(log_duration = log(avg_duration))  # natural log

# Fit linear model on log-transformed duration
lm_log_duration <- lm(log_duration ~ Year, data = mhw_yearly_duration)
summary(lm_log_duration)
par(mfrow = c(2,2))
plot(lm_log_duration)

###duration plot ----
p3 <- ggplot(mhw_yearly_duration, aes(x = Year, y = avg_duration)) +
  geom_line(color = "black", size = 1, linewidth = 1.3) +
  geom_point(color = "black", size = 2.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  # stat_regline_equation(
  #   label.x = min(mhw_summary$Year) + 1,
  #   label.y = max(mhw_summary$avg_duration, na.rm = TRUE) - 0.5,
  #   aes(label = ..eq.label..)
  # ) +
  # stat_cor(
  #   label.x = min(mhw_summary$Year) + 1,
  #   label.y = max(mhw_summary$avg_duration, na.rm = TRUE) - 1,
  #   aes(label = ..rr.label..)
  # ) +
  labs(title = "", y = "Mean Duration (days)", x = "Year") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),          
    axis.text.y = element_text(size = 14, face = "bold"),                                  
    axis.title.y = element_text(size = 17, face = "bold"),
    axis.title.x = element_text(size = 17, face = "bold"),
    axis.line = element_line(size = 1),                                 
  ) 


#El Nino Duration & Intensity ----
#Test if duration or intensity is longer during el nino years

#Label el nino years
el_nino_years <- c(1972, 1982, 1987, 1991, 1997, 2002, 2009, 2015)

mhw_summary_events_only <- mhw_summary %>%
  mutate(ElNino = ifelse(Year %in% el_nino_years, "ElNino", "Neutral/Other"))

ggplot(mhw_summary_events_only, aes(x = ElNino, y = avg_duration)) +
  geom_boxplot(fill = "grey") +
  geom_jitter(width = 0.1) +
  geom_text(aes(label = Year), 
            position = position_jitter(width = 0.1, height = 0), 
            size = 3, vjust = -0.5) +
  labs(title = "MHW Duration during El Niño vs Non–El Niño Years",
       x = "", y = "Average Duration (days)")

t.test(avg_duration ~ ElNino, data = mhw_summary_events_only)

lm_duration_elnino <- lm(avg_duration ~ ElNino, data = mhw_summary_events_only)
summary(lm_duration_elnino)

#years seem too broad to classify as el nino or la nina - use months instead
oni <- read.csv(
  "https://psl.noaa.gov/data/correlation/oni.csv",
  stringsAsFactors = FALSE
)
oni

oni <- oni %>%
  mutate(
    Year = year(Date),
    Month = month(Date)
  )

Data2 <- Data %>%
  left_join(oni, by = c("Year", "Month"))

Data2$ONI <- Data2$ONI.from.CPC..missing.value..99.9.https...psl.noaa.gov.data.timeseries.month.

Data2 <- Data2 %>%
  mutate(
    ENSO = case_when(
      ONI >= 0.5  ~ "ElNino",
      ONI <= -0.5 ~ "LaNina",
      TRUE        ~ "Neutral"
    )
  )

mhws <- Data2 %>% filter(MHW == 1)


mhws <- Data2 %>% 
  filter(MHW == 1) %>%
  group_by(Year) %>%  # or group by heatwave event ID if you have one
  arrange(Date) %>%
  slice(1) %>%  # Take first occurrence = start of heatwave
  ungroup()

###Duration ENSO model ----
phase_summary <- mhws %>%
  group_by(ENSO) %>%
  summarise(mean_duration = mean(Duration, na.rm = TRUE),
            n_events = n())
print(phase_summary)

lm_dur_elnino <- lm(Duration ~ ENSO, data = mhws)
summary(lm_dur_elnino)

par(mfrow = c(2, 2))
plot(lm_dur_elnino)

#try glm with log link dist'd
glm_dur_elnino <- glm(Duration ~ ENSO,
                      data = mhws,
                      family = Gamma(link = "log"))

summary(glm_dur_elnino)
par(mfrow = c(2,2))
plot(glm_dur_elnino)
simulation_output <- simulateResiduals(glm_dur_elnino) 
plot(simulation_output) #still deviation detected, but based on other plots, I think this is minor

#try log transformation
mhws$log_Duration <- log(mhws$Duration)
lm_log <- lm(log_Duration ~ ENSO, data = mhws)
summary(lm_log)
par(mfrow=c(2,2))
plot(lm_log)

#glm model has best fit - using

#Pairwise test
emm <- emmeans(glm_dur_elnino, ~ ENSO)
pairs(emm, adjust = "tukey")

#mean and SD
mhws %>%
  filter(ENSO == "Neutral") %>%
  summarise(
    mean = mean(Duration, na.rm = TRUE),
    sd = sd(Duration, na.rm = TRUE)
  )

###ENSO duration plot ----
ggplot(mhws, aes(x = ENSO, y = Duration)) +
  geom_boxplot(fill = "grey") +
  geom_text(aes(label = Year), 
            position = position_jitter(width = 0.1, height = 0), 
            size = 3, vjust = -0.5) +
  labs(title = "MHW Duration by ENSO Phase", x = "", y = "Duration (days)")

###Intensity ENSO model ----
phase_summary <- mhws %>%
  group_by(ENSO) %>%
  summarise(mean_intensity = mean(Intensity, na.rm = TRUE),
            n_events = n())
print(phase_summary)

mhws$ENSO <- factor(mhws$ENSO) 
mhws$ENSO <- relevel(mhws$ENSO, ref = "Neutral")
lm_int_elnino <- lm(Intensity ~ ENSO, data = mhws)
summary(lm_int_elnino)
par(mfrow=c(2,2))
plot(lm_int_elnino) #not a good fit

#try glm with log link dist'd - use
glm_int <- glm(Intensity ~ ENSO,
               data = mhws,
               family = Gamma(link = "log"))
summary(glm_int)
par(mfrow=c(2,2))
plot(glm_int) #better fit
simulation_output <- simulateResiduals(glm_int) 
plot(simulation_output) #no deviations detected

#Pairwise test
emm <- emmeans(glm_dur_elnino, ~ ENSO)
pairs(emm, adjust = "tukey")

#mean and SD
mhws %>%
  filter(ENSO == "Neutral") %>%
  summarise(
    mean = mean(Intensity, na.rm = TRUE),
    sd = sd(Intensity, na.rm = TRUE)
  )

###ENSO intensity plot ----
ggplot(mhws, aes(x = ENSO, y = Intensity)) +
  geom_boxplot(fill = "grey") +
  geom_text(aes(label = Year), 
            position = position_jitter(width = 0.1, height = 0), 
            size = 3, vjust = -0.5) +
  labs(title = "MHW Intensity by ENSO Phase", x = "", y = "Average Intensity (°C)")


#SST model ----
#Import data
ChromeDailySalTemp_R <- read_excel("Documents/PhD/Marine_heatwave_BS_review/MHW_raw_data.xlsx", 
                                   +     sheet = "ChromeDailySalTemp_R")

#Data wrangling
sst_clean <-  ChromeDailySalTemp_R %>%
  filter(`Temperature(C)` != 99.9, Year >= 1969, Year <= 2018)

sst_yearly <- sst_clean %>%
  group_by(Year) %>%
  summarise(mean_sst = mean(`Temperature(C)`, na.rm = TRUE))

#model
lm_SST <- lm(mean_sst ~ Year, data = sst_yearly)
summary(lm_SST)

#Check fit
par(mfrow = c(2, 2))
plot(lm_SST)

#Check for temporal autocorrelation (TA)
sim <- simulateResiduals(lm_SST)
testTemporalAutocorrelation(sim, time = sst_yearly$Year) #significant TA

#Correct TA 
gls_model <- gls(mean_sst ~ Year, 
                 data = sst_yearly,
                 correlation = corAR1(form = ~ Year))
summary(gls_model)
coef(gls_model) #Same as ls with HAC correction
res <- residuals(gls_model, type = "normalized")  # normalized residuals
acf(res, main = "ACF of GLS residuals") #looks good

#or use HAC after running lm
coeftest(lm_SST, vcov = vcovHAC(lm_SST))

###SST plot ----
p4 <- ggplot(sst_yearly, aes(x = Year, y = mean_sst)) +
  geom_line(color = "black", size = 1, linewidth = 1.3) +
  geom_point(color = "black", size = 2.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  # stat_regline_equation(
  #   label.x = min(sst_yearly$Year) + 1,
  #   label.y = max(sst_yearly$mean_sst, na.rm = TRUE),
  #   aes(label = ..eq.label..)
  # ) +
  # stat_cor(
  #   label.x = min(sst_yearly$Year) + 1,
  #   label.y = max(sst_yearly$mean_sst, na.rm = TRUE) - 0.2,
  #   aes(label = ..rr.label..)
  # ) +
  labs(title = "",
       y = "Mean SST (°C)",
       x = "") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),          
    axis.text.y = element_text(size = 14, face = "bold"),                                  
    axis.title.y = element_text(size = 17, face = "bold"),               
    axis.line = element_line(size = 1),                                 
  ) 

#Arrange MHW plots ----
grid.arrange(p4, p1, p2, p3, ncol = 2)

#Calculate Heating Rate ----

#99.9 indicates missing value so remove
Data <- Data %>%
  mutate(`Temperature(C)` = na_if(`Temperature(C)`, 99.9))

# Step 1: Create a proper Date column
Data <- Data %>%
  mutate(Date = make_date(Year, Month, Day))

#Filter for 1969 to 2018
Data_filtered <- Data %>%
  filter(Year >= 1969 & Year <= 2018)

#Arrange by date and calculate daily heating rate
Data_heating <- Data_filtered %>%
  arrange(Date) %>%
  mutate(HeatingRate = `Temperature(C)` - lag(`Temperature(C)`))

#Look at vibrio outbreak (2015)
Data_2015 <- Data_heating %>%
  filter(year(Date) == 2014)

# Define highlight range
highlight_start <- as.Date("2014-05-11")
highlight_end <- as.Date("2014-06-12")

#plot
ggplot(Data_2015, aes(x = Date, y = HeatingRate)) +
  annotate("rect", xmin = as.Date("2014-05-11"), xmax = as.Date("2014-06-12"),
           ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.3) +
  geom_line(color = "firebrick") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Daily Heating Rate in 2015 with Highlight",
       x = "Date",
       y = "Heating Rate (°C/day)")

# Filter for May and June from 1969 to 2018, then summarize
avg_heating_rate_summer <- Data_heating %>%
  filter(Year >= 1969, Year <= 2018, Month %in% c(5, 6)) %>%
  summarise(mean_heating_rate = mean(HeatingRate, na.rm = TRUE))

print(avg_heating_rate_summer)

heating_rate_by_year <- Data_heating %>%
  filter(Year >= 1969, Year <= 2018, Month %in% c(5, 6)) %>%
  group_by(Year) %>%
  summarise(mean_heating_rate = mean(HeatingRate, na.rm = TRUE))

print(heating_rate_by_year)

ggplot(heating_rate_by_year, aes(x = Year, y = mean_heating_rate)) +
  geom_line(color = "black", size = 1) +
  geom_point(color = "black", size = 2) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "black") +
  stat_regline_equation(
    label.x = min(heating_rate_by_year$Year) + 1,
    label.y = max(heating_rate_by_year$mean_heating_rate, na.rm = TRUE),
    aes(label = ..eq.label..)
  ) +
  stat_cor(
    label.x = min(heating_rate_by_year$Year) + 1,
    label.y = max(heating_rate_by_year$mean_heating_rate, na.rm = TRUE) - 0.2,
    aes(label = ..rr.label..)
  ) +
  geom_vline(xintercept = 2015, color = "red", linetype = "solid", linewidth = 1) + 
  labs(
    title = "Average May–June Heating Rate by Year (1969–2018)",
    x = "Year",
    y = "Mean Heating Rate (Delta°C/day)"
  )

#Season MHW intensity & duration ----
#Data
MHW_raw_data <- read_excel("Documents/PhD/Marine_heatwave_BS_review/MHW_raw_data.xlsx", 
                           +     sheet = "MHW_Season_R")
Data <- MHW_raw_data

str(Data)

# Set a scaling factor so Intensity can be shown on a similar scale
scale_factor <- max(Data$Duration) / max(Data$Intensity)

# Factor Season to preserve order
Data$Season <- factor(Data$Season, levels = c("Spring", "Summer", "Autumn", "Winter"))

# Scale Intensity to match Duration for dual y-axis plotting
scale_factor <- max(Data$Duration) / max(Data$Intensity)

ggplot(Data, aes(x = Season)) +
  # Duration line and points
  geom_line(aes(y = Duration, group = 1), color = "blue", size = 1) +
  geom_point(aes(y = Duration), color = "blue", size = 3) +
  geom_errorbar(aes(ymin = Duration - Duration_SD, ymax = Duration + Duration_SD), 
                width = 0.1, color = "blue")
  
  # Intensity line and points (scaled)
  geom_line(aes(y = Intensity * scale_factor, group = 1), color = "red", size = 1, linetype = "dashed") +
  geom_point(aes(y = Intensity * scale_factor), color = "red", size = 3, shape = 17) +
  geom_errorbar(aes(ymin = (Intensity - Intensity_SD) * scale_factor, 
                    ymax = (Intensity + Intensity_SD) * scale_factor), 
                width = 0.1, color = "red") +
  
  # y-axes
  scale_y_continuous(
    name = "Duration (days)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Intensity (°C)")
  ) +
  theme(
    axis.title.y.left = element_text(color = "blue", size = 14),
    axis.title.y.right = element_text(color = "red", size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  labs(
    x = "Season",
    title = ""
  )

#Add frequency on graph too
Data$Season <- factor(Data$Season, levels = c("Spring", "Summer", "Autumn", "Winter"))

# Scaling factor for second y-axis
scale_factor <- max(Data$Duration) / max(Data$Intensity)

ggplot(Data, aes(x = Season)) +
  # Duration
  geom_line(aes(y = Duration, group = 1), color = "blue", size = 1) +
  geom_point(aes(y = Duration, size = Frequency), color = "blue") +
  geom_errorbar(aes(ymin = Duration - Duration_SD, ymax = Duration + Duration_SD), width = 0.1, color = "blue") +
  
  # Intensity (scaled)
  geom_line(aes(y = Intensity * scale_factor, group = 1), color = "red", size = 1, linetype = "dashed") +
  geom_point(aes(y = Intensity * scale_factor, size = Frequency), color = "red", shape = 17) +
  geom_errorbar(aes(ymin = (Intensity - Intensity_SD) * scale_factor, 
                    ymax = (Intensity + Intensity_SD) * scale_factor), width = 0.1, color = "red") +
  
  # y-axes
  scale_y_continuous(
    name = "MHW Duration (days)",
    sec.axis = sec_axis(~ . / scale_factor, name = "MHW Intensity (°C)")
  ) +
  scale_size_continuous(range = c(3, 10), name = "MHW Frequency") +
  
  theme(
    axis.title.y.left = element_text(color = "blue", face = "plain"),
    axis.title.y.right = element_text(color = "red", face = "plain"),
    legend.position = "right"
  ) +
  labs(
    x = "",
    title = ""
  )
