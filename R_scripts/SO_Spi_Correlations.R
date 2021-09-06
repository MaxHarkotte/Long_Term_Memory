# Sawangjit et al. (2021): 1 week remote novel object recognition (NOR) memory task
# Correlation analysis for slow oscillations/spindle properties (Mean duration, Mean power, Number)
# By: Max Harkotte
# Contact: maximilian.harkotte@gmail.com
# Last update: January 2021

rm(list = ls()) # clear workspace
cat("\014") # clear console

# 0 - Load packages -------------------------------------------------------
library(tidyverse)
library(ggpubr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(R.matlab)
library(readxl)

# 1 - Source file ---------------------------------------------------------
dataPath <- "Y:/Max/1wk_NOR_new/Behavior/"
setwd(dataPath)

# 2 - Read in data --------------------------------------------------------
coupling_after_trough <- read.csv2(paste0(dataPath,"Data/Spindels_after_SOd_EEGLeft.csv"), sep = ",")
coupling_window <- read.csv2(paste0(dataPath,"Data/Coupling_window_EEGLeft.csv"), sep = ",")
coupling_SO <- read.csv2(paste0(dataPath,"Data/Coupling_SO_EEGLeft.csv"), sep = ",")

coupling_SO$Percent_SO_Coupled <- round(as.numeric(as.character(coupling_SO$Percent_SO_Coupled)), digits = 2)
coupling_SO$Percent_Spi_Coupled <- round(as.numeric(as.character(coupling_SO$Percent_Spi_Coupled)), digits = 2)
coupling_SO$Mean_Angle <- round(as.numeric(as.character(coupling_SO$Mean_Angle)), digits = 2)
coupling_SO$Median_Angle <- round(as.numeric(as.character(coupling_SO$Median_Angle)), digits = 2)
coupling_SO$DR_1 <- as.numeric(as.character(coupling_SO$DR_1))
coupling_SO$DR_3 <- as.numeric(as.character(coupling_SO$DR_3))
coupling_SO$DR_5 <- as.numeric(as.character(coupling_SO$DR_5))
coupling_SO$dRear_1 <- as.numeric(as.character(coupling_SO$dRear_1))
coupling_SO$dRear_3 <- as.numeric(as.character(coupling_SO$dRear_3))
coupling_SO$dRear_5 <- as.numeric(as.character(coupling_SO$dRear_5))

# Coupling after trough ---------------------------------------------------
coupling_after_trough$DR_1 = as.numeric(as.character(coupling_after_trough$DR_1))

mean(coupling_after_trough$Number_Events)
sd(coupling_after_trough$Number_Events)

ggscatter(
  coupling_after_trough,
  x = "DR_1",
  y = "Number_Events",
  title = "Overall number of coupled events  (EEG left)",
  color = "black",
  shape = 21,
  size = 3,
  # Points color, shape and size
  add = "reg.line",
  # Add regressin line
  add.params = list(color = "blue", fill = "lightgray"),
  # Customize reg. line
  conf.int = TRUE,
  # Add confidence interval
  cor.coef = TRUE,
  # Add correlation coefficient. see ?stat_cor
  cor.coeff.args = list(
    method = "spearman",
    label.x = -0.1,
    label.sep = "\n"
  )
)

mus <- subset(coupling_after_trough, coupling_after_trough$Drug == "MUS")
mean(mus$Number_Events)
sd(mus$Number_Events)

ggscatter(
  mus,
  x = "DR_1",
  y = "Number_Events",
  title = "Muscimol (EEG left)",
  color = "black",
  shape = 21,
  size = 3,
  # Points color, shape and size
  add = "reg.line",
  # Add regressin line
  add.params = list(color = "blue", fill = "lightgray"),
  # Customize reg. line
  conf.int = TRUE,
  # Add confidence interval
  cor.coef = TRUE,
  # Add correlation coefficient. see ?stat_cor
  cor.coeff.args = list(
    method = "spearman",
    label.x = -0.1,
    label.sep = "\n"
  )
)

sal <- subset(coupling_after_trough, coupling_after_trough$Drug == "SAL")
mean(sal$Number_Events)
sd(sal$Number_Events)

ggscatter(
  sal,
  x = "DR_1",
  y = "Number_Events",
  title = "Saline (EEG left)",
  color = "black",
  shape = 21,
  size = 3,
  # Points color, shape and size
  add = "reg.line",
  # Add regressin line
  add.params = list(color = "blue", fill = "lightgray"),
  # Customize reg. line
  conf.int = TRUE,
  # Add confidence interval
  cor.coef = TRUE,
  # Add correlation coefficient. see ?stat_cor
  cor.coeff.args = list(
    method = "spearman",
    label.x = -0.1,
    label.sep = "\n"
  )
)


sal[nrow(sal)+1,] <- NA # make same length (data missing for Saline for animal 5)
sal$Animal[is.na(sal$Animal)] <- "05-P10-S1"
sal <- sal[order(sal$Animal),]
mus <- mus[order(mus$Animal),]
t.test(mus$Number_Events, sal$Number_Events, paired = TRUE)

# Coupling window ---------------------------------------------------------
coupling_window$DR_1 = as.numeric(as.character(coupling_window$DR_1))

mean(coupling_window$Number_Events)
sd(coupling_window$Number_Events)

ggscatter(
  coupling_window,
  x = "DR_1",
  y = "Number_Events",
  title = "Overall number of coupled events  (EEG left)",
  color = "black",
  shape = 21,
  size = 3,
  # Points color, shape and size
  add = "reg.line",
  # Add regressin line
  add.params = list(color = "blue", fill = "lightgray"),
  # Customize reg. line
  conf.int = TRUE,
  # Add confidence interval
  cor.coef = TRUE,
  # Add correlation coefficient. see ?stat_cor
  cor.coeff.args = list(
    method = "spearman",
    label.x = -0.1,
    label.sep = "\n"
  )
)

mus <- subset(coupling_window, coupling_window$Drug == "MUS")
mean(mus$Number_Events)
sd(mus$Number_Events)

ggscatter(
  mus,
  x = "DR_1",
  y = "Number_Events",
  title = "Muscimol (EEG left)",
  color = "black",
  shape = 21,
  size = 3,
  # Points color, shape and size
  add = "reg.line",
  # Add regressin line
  add.params = list(color = "blue", fill = "lightgray"),
  # Customize reg. line
  conf.int = TRUE,
  # Add confidence interval
  cor.coef = TRUE,
  # Add correlation coefficient. see ?stat_cor
  cor.coeff.args = list(
    method = "spearman",
    label.x = -0.1,
    label.sep = "\n"
  )
)

sal <- subset(coupling_window, coupling_window$Drug == "SAL")
mean(sal$Number_Events)
sd(sal$Number_Events)

ggscatter(
  sal,
  x = "DR_1",
  y = "Number_Events",
  title = "Saline (EEG left)",
  color = "black",
  shape = 21,
  size = 3,
  # Points color, shape and size
  add = "reg.line",
  # Add regressin line
  add.params = list(color = "blue", fill = "lightgray"),
  # Customize reg. line
  conf.int = TRUE,
  # Add confidence interval
  cor.coef = TRUE,
  # Add correlation coefficient. see ?stat_cor
  cor.coeff.args = list(
    method = "spearman",
    label.x = -0.1,
    label.sep = "\n"
  )
)

sal[nrow(sal)+1,] <- NA # make same length (data missing for Saline for animal 5)
sal$Animal[is.na(sal$Animal)] <- "05-P10-S1"
sal <- sal[order(sal$Animal),]
mus <- mus[order(mus$Animal),]
t.test(mus$Number_Events, sal$Number_Events, paired = TRUE)


# Coupling during SO ------------------------------------------------------

coupling_SO$DR_1 = as.numeric(as.character(coupling_SO$DR_1))

mean(coupling_SO$Number_Events)
sd(coupling_SO$Number_Events)


ggscatter(
  coupling_SO,
  x = "DR_1",
  y = "Number_Events",
  title = "Overall number of coupled events  (EEG right)",
  color = "black",
  shape = 21,
  size = 3,
  # Points color, shape and size
  add = "reg.line",
  # Add regressin line
  add.params = list(color = "blue", fill = "lightgray"),
  # Customize reg. line
  conf.int = TRUE,
  # Add confidence interval
  cor.coef = TRUE,
  # Add correlation coefficient. see ?stat_cor
  cor.coeff.args = list(
    method = "spearman",
    label.x = -0.1,
    label.sep = "\n"
  )
)

mus <- subset(coupling_SO, coupling_SO$Drug == "MUS")

mean(mus$Number_Events)
sd(mus$Number_Events)

ggscatter(
  mus,
  x = "DR_1",
  y = "Number_Events",
  title = "Muscimol - number of coupled events  (EEG right)",
  color = "black",
  shape = 21,
  size = 3,
  # Points color, shape and size
  add = "reg.line",
  # Add regressin line
  add.params = list(color = "blue", fill = "lightgray"),
  # Customize reg. line
  conf.int = TRUE,
  # Add confidence interval
  cor.coef = TRUE,
  # Add correlation coefficient. see ?stat_cor
  cor.coeff.args = list(
    method = "spearman",
    label.x = -0.1,
    label.sep = "\n"
  )
)

sal <- subset(coupling_SO, coupling_SO$Drug == "SAL")
mean(sal$Number_Events)
sd(sal$Number_Events)

ggscatter(
  sal,
  x = "DR_1",
  y = "Number_Events",
  title = "Saline - number of coupled events (EEG right)",
  color = "black",
  shape = 21,
  size = 3,
  # Points color, shape and size
  add = "reg.line",
  # Add regressin line
  add.params = list(color = "blue", fill = "lightgray"),
  # Customize reg. line
  conf.int = TRUE,
  # Add confidence interval
  cor.coef = TRUE,
  # Add correlation coefficient. see ?stat_cor
  cor.coeff.args = list(
    method = "spearman",
    label.x = -0.1,
    label.sep = "\n"
  )
)

sal[nrow(sal)+1,] <- NA # make same length (data missing for Saline for animal 5)
sal$Animal[is.na(sal$Animal)] <- "05-P10-S1"
sal <- sal[order(sal$Animal),]
mus <- mus[order(mus$Animal),]
t.test(mus$Number_Events, sal$Number_Events, paired = TRUE)



write.csv(coupling_SO,
           file.path(dataPath, "Data/Coupling_Behavior_EEGRight.csv"),
           row.names = FALSE)
