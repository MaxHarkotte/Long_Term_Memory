# Sawangjit et al. (2021): 1 week remote novel object recognition (NOR) memory task
# Mixed model statistical analyses
# By: Max Harkotte
# Contact: maximilian.harkotte@gmail.com
# Last update: January 2021

rm(list = ls()) # clear workspace
cat("\014") # clear console

# 0 - Load packages -------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(psych)
library(lme4)
library(effects)
library(sjPlot)
library(ggpubr)
library(rstatix)
library(lmerTest)

# 1 - Source file ---------------------------------------------------------
dataPath <- "Y:/Max/1wk_NOR_new/Behavior/Data"
setwd(dataPath)

# 2 - Read in data --------------------------------------------------------

# Test wide contains Discrimination Ratios (DR) for three time intervals
# of the memory test: 0-1 min (DR1), 0-3 min (DR3), 0-5 min (DR5) (Minute)
# Between Factor: Sleep or Wake during Post-Encoding Interval (PostInterval)
# Mixed Factor: Muscimol or Saline infusion in the dorsal hippocampus during
# Post-Encoding Interval (Drug)

Test_wide <-
  read.csv2(
    "Data_raw.csv",
    header = TRUE,
    sep = ",",
    stringsAsFactors = TRUE
  )

for (i in 5:(length(Test_wide))) {
  Test_wide[, i]  <- as.numeric(as.character(Test_wide[, i]))
}

# Wide to long data format
Test_long <-
  pivot_longer(Test_wide,
               cols = 5:7 ,
               names_to = "Minute",
               values_to = "DR")

Test_long$Minute <- as.factor(Test_long$Minute)

Test_long <- subset(Test_long, Test_long$Minute != "DR5")

# 3 - Data exploration -----------------------------------------------------

# Data summary
Test_long %>%
  group_by(PostInterval, Drug, Minute) %>%
  get_summary_stats(DR, type = "mean_sd")

# Visualization
bxp_interval <- ggboxplot(
  Test_long,
  x = "Minute",
  y = "DR",
  color = "Drug",
  palette = "jco",
  add = "jitter",
  shape = "Drug",
  facet.by = "PostInterval"
)
bxp_interval

bxp_min <- ggboxplot(
  Test_long,
  x = "PostInterval",
  y = "DR",
  color = "Drug",
  palette = "jco",
  add = "jitter",
  shape = "Drug",
  facet.by = "Minute"
)
bxp_min

# Outliers?
Test_long %>%
  group_by(PostInterval, Drug, Minute) %>%
  identify_outliers(DR)

# Normality assumption using Shapiro-Wilk test
Test_long %>%
  group_by(PostInterval, Drug, Minute) %>%
  shapiro_test(DR)

# Normality assumption using QQ plots
ggqqplot(Test_long, "DR", ggtheme = theme_bw()) +
  facet_grid(PostInterval ~ Drug, labeller = "label_both")

# Homogeneity of variance assumption using Levene's test at each level of the within factor
Test_long %>%
  group_by(Minute) %>%
  levene_test(DR ~ PostInterval * Drug)

Test_long %>%
  group_by(Minute, PostInterval) %>%
  levene_test(DR ~ Drug)

Test_long %>%
  group_by(Minute, Drug) %>%
  levene_test(DR ~ PostInterval)

# 4 - Statistics ----------------------------------------------------------
# Mixed linear models -----------------------------------------------------

# Three way mixed model without interaction term
basic_hlm <-
  lmer(DR ~ Minute + PostInterval + Drug + (1 | animal),
       data = Test_long,
       REML = FALSE)
summary(basic_hlm)

# Adding PostInterval*Drug interaction to basic_hlm
Drug_Sleep_interaction_hlm <-
  lmer(DR ~ Minute + PostInterval * Drug + (1 | animal),
       data = Test_long,
       REML = FALSE)
summary(Drug_Sleep_interaction_hlm)

# Adding Minute*PostInterval interaction to basic_hlm
Minute_Sleep_interaction_hlm <-
  lmer(DR ~ Minute * PostInterval + Drug + (1 | animal),
       data = Test_long,
       REML = FALSE)
summary(Minute_Sleep_interaction_hlm)

# Include all possible interaction of fixed effects
Overall_interaction_hlm <-
  lmer(DR ~ Minute * PostInterval * Drug + (1 | animal),
       data = Test_long,
       REML = FALSE)
summary(Overall_interaction_hlm)

# Remove factor Minute
interaction_hlm <-
  lmer(DR ~ PostInterval * Drug + (1 | animal),
       data = Test_long,
       REML = FALSE)
summary(Overall_interaction_hlm)

# Hierarchical test of models
anova(basic_hlm, Drug_Sleep_interaction_hlm)
anova(basic_hlm, Minute_Sleep_interaction_hlm)
anova(Drug_Sleep_interaction_hlm, Overall_interaction_hlm)

# Check assumptions of selected model
plot_model(Drug_Sleep_interaction_hlm, type = 'diag')
tab_model(Drug_Sleep_interaction_hlm)

# Post-hoc tests for Post-Interval*Drug Interaction ------------------------
library(lmerTest)
two_way_hlm <-
  lmer(DR ~ PostInterval * Drug + (1 | animal),
       data = Test_long,
       REML = FALSE)
anova(two_way_hlm)

# Multiple pairwise comparisons -------------------------------------------

## DR 1
DR1_Sleep_Mus <- Test_long[which(
  Test_long$Minute == 'DR1' &  Test_long$PostInterval == 'Sleep' &
    Test_long$Drug == "Muscimol"
), ]

DR1_Sleep_Sal <- Test_long[which(
  Test_long$Minute == 'DR1'
  &
    Test_long$PostInterval == 'Sleep' & Test_long$Drug == "Saline"
), ]

DR1_Wake_Mus <- Test_long[which(
  Test_long$Minute == 'DR1'
  &
    Test_long$PostInterval == 'Wake' & Test_long$Drug == "Muscimol"
), ]

DR1_Wake_Sal <- Test_long[which(
  Test_long$Minute == 'DR1'
  &
    Test_long$PostInterval == 'Wake' & Test_long$Drug == "Saline"
), ]

# Hypothesis 1: Muscimol prevents sleep effects on memory consolidation
t.test(DR1_Sleep_Mus$DR, DR1_Sleep_Sal$DR, alternative = "two.sided", paired = TRUE) 

# Hypothesis 2: Sleep promotes memory consolidation in the absence of muscimol
t.test(DR1_Sleep_Sal$DR, DR1_Wake_Sal$DR, alternative = "greater", paired = FALSE)

# Additional post-hoc t tests
t.test(DR1_Sleep_Mus$DR, DR1_Wake_Mus$DR, alternative = "two.sided", paired = FALSE)
t.test(DR1_Sleep_Mus$DR, DR1_Wake_Sal$DR, alternative = "two.sided", paired = FALSE)
t.test(DR1_Wake_Mus$DR, DR1_Wake_Sal$DR, alternative = "two.sided", paired = FALSE)

# 5 - Report --------------------------------------------------------------
