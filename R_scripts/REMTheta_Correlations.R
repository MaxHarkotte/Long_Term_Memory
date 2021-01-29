# Sawangjit et al. (2021): 1 week remote novel object recognition (NOR) memory task
# Correlation analysis for spectral analyses (theta power/energy during REM epochs)
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
library(R.matlab)
library(readxl)

# 1 - Source file ---------------------------------------------------------
dataPath <- "Y:/Max/1wk_NOR_new/Behavior/"
setwd(dataPath)

# 2 - Read in data --------------------------------------------------------
behavior <- read_excel(paste0(dataPath,"Data/Behavior info.xlsx"))
behavior$idx <- paste0(substr(behavior$Animal, 1, 2),"_",behavior$Sampling)
behavior$id <- substr(behavior$Animal, 1, 2)

theta <- vector(mode = "list", length = nrow(behavior))

i = 1
while (i < nrow(behavior) + 1) {
  tmp_dat <-
    readMat(paste0("../REMTheta/REMTheta_", behavior$idx[i], ".mat"))
  theta[i] <- list(tmp_dat)
  names(theta)[i] <- behavior$idx[i]
  i = i + 1
}

# Data prep ---------------------------------------------------------------

# EEG LEFT ----------------------------------------------------------------


LEFT <- data.frame(
  Animal = integer(length(theta)),
  Total_REM_Duration = NA,
  Mean_REM_Duration = NA,
  Mean_Pwr = NA,
  Total_Energy = NA
)

i = 1
while (i <= length(theta)) {
  LEFT$Animal[i] <- names(theta)[i]
  LEFT$Total_REM_Duration[i] = theta[[i]][["EEG.Left.sum"]][, 1]
  LEFT$Mean_REM_Duration[i] <- theta[[i]][["EEG.Left.sum"]][, 2]
  LEFT$Mean_Pwr[i] <- theta[[i]][["EEG.Left.sum"]][, 3]
  LEFT$Total_Energy[i] <- theta[[i]][["EEG.Left.sum"]][, 4]
  i = i + 1
}

LEFT <- LEFT %>% separate(Animal, c("Animal", "Sampling"), sep = 2)

LEFT$Sampling[LEFT$Sampling == "_1"] <- 1
LEFT$Sampling[LEFT$Sampling == "_2"] <- 2

LEFT <-
  merge(
    LEFT,
    behavior,
    by.x = c("Animal", "Sampling"),
    by.y = c("id", "Sampling")
  )

LEFT$Animal <- as.factor(LEFT$Animal)
LEFT$Sampling <- as.factor(LEFT$Sampling)
LEFT$Drug <- as.factor(LEFT$Drug)


# Correlations ------------------------------------------------------------
LEFT_muscimol <- subset(LEFT, LEFT$Drug == "MUS")
LEFT_saline <- subset(LEFT, LEFT$Drug == "SAL")
LEFT_saline[nrow(LEFT_saline)+1,] <- NA # make same length (data missing for Saline for animal 5)
LEFT_saline$Animal[is.na(LEFT_saline$Animal)] <- "05"
LEFT_saline <- LEFT_saline[order(LEFT_saline$Animal),]
LEFT_muscimol <- LEFT_muscimol[order(LEFT_muscimol$Animal),]
  
ggscatter(
  LEFT_muscimol,
  x = "DR_1",
  y = "Total_Energy",
  title = "Muscimol infusions (EEG left)",
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
    method = "pearson",
    label.x = -0.1,
    label.sep = "\n"
  )
)

ggscatter(
  LEFT_saline,
  x = "DR_1",
  y = "Total_Energy",
  title = "Saline infusions (EEG left)",
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
    method = "pearson",
    label.x = -0.1,
    label.sep = "\n"
  )
)

ggscatter(
  LEFT_muscimol,
  x = "DR_1",
  y = "Mean_Pwr",
  title = "Muscimol infusions (EEG left)",
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
    method = "pearson",
    label.x = -0.1,
    label.sep = "\n"
  )
)

ggscatter(
  LEFT_saline,
  x = "DR_1",
  y = "Mean_Pwr",
  title = "Saline infusions (EEG left)",
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
    method = "pearson",
    label.x = -0.1,
    label.sep = "\n"
  )
)


# Compare theta power and energy between conditions
# EEG left 

t.test(LEFT_muscimol$Total_Energy, LEFT_saline$Total_Energy, paired = TRUE)
mean(LEFT_muscimol$Total_Energy, na.rm = TRUE)
mean(LEFT_saline$Total_Energy, na.rm = TRUE)

t.test(LEFT_muscimol$Mean_Pwr, LEFT_saline$Mean_Pwr, paired = TRUE)
mean(LEFT_muscimol$Mean_Pwr, na.rm = TRUE)
mean(LEFT_saline$Mean_Pwr, na.rm = TRUE)

t.test(LEFT_muscimol$Mean_REM_Duration, LEFT_saline$Mean_REM_Duration, paired = TRUE)
mean(LEFT_muscimol$Mean_REM_Duration, na.rm = TRUE)
mean(LEFT_saline$Mean_REM_Duration, na.rm = TRUE)

t.test(LEFT_muscimol$Total_REM_Duration, LEFT_saline$Total_REM_Duration, paired = TRUE)
mean(LEFT_muscimol$Total_REM_Duration, na.rm = TRUE)
mean(LEFT_saline$Total_REM_Duration, na.rm = TRUE)



# EEG RIGHT ----------------------------------------------------------------


RIGHT <- data.frame(
  Animal = integer(length(theta)),
  Total_REM_Duration = NA,
  Mean_REM_Duration = NA,
  Mean_Pwr = NA,
  Total_Energy = NA
)

i = 1
while (i <= length(theta)) {
  RIGHT$Animal[i] <- names(theta)[i]
  RIGHT$Total_REM_Duration[i] = theta[[i]][["EEG.Right.sum"]][, 1]
  RIGHT$Mean_REM_Duration[i] <- theta[[i]][["EEG.Right.sum"]][, 2]
  RIGHT$Mean_Pwr[i] <- theta[[i]][["EEG.Right.sum"]][, 3]
  RIGHT$Total_Energy[i] <- theta[[i]][["EEG.Right.sum"]][, 4]
  i = i + 1
}

RIGHT <- RIGHT %>% separate(Animal, c("Animal", "Sampling"), sep = 2)

RIGHT$Sampling[RIGHT$Sampling == "_1"] <- 1
RIGHT$Sampling[RIGHT$Sampling == "_2"] <- 2

RIGHT <-
  merge(
    RIGHT,
    behavior,
    by.x = c("Animal", "Sampling"),
    by.y = c("id", "Sampling")
  )

RIGHT$Animal <- as.factor(RIGHT$Animal)
RIGHT$Sampling <- as.factor(RIGHT$Sampling)
RIGHT$Drug <- as.factor(RIGHT$Drug)


# Correlations ------------------------------------------------------------
RIGHT_muscimol <- subset(RIGHT, RIGHT$Drug == "MUS")
RIGHT_saline <- subset(RIGHT, RIGHT$Drug == "SAL")
RIGHT_saline[nrow(RIGHT_saline)+1,] <- NA # make same length (data missing for Saline for animal 5)
RIGHT_saline$Animal[is.na(RIGHT_saline$Animal)] <- "05"
RIGHT_saline <- RIGHT_saline[order(RIGHT_saline$Animal),]
RIGHT_muscimol <- RIGHT_muscimol[order(RIGHT_muscimol$Animal),]


# RIGHT_saline <- subset(RIGHT_saline, RIGHT_saline$DR_1 > 0)

ggscatter(
  RIGHT_muscimol,
  x = "DR_1",
  y = "Total_Energy",
  title = "Muscimol infusions (EEG right)",
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
    method = "pearson",
    label.x = -0.1,
    label.sep = "\n"
  )
)

ggscatter(
  RIGHT_saline,
  x = "DR_1",
  y = "Total_Energy",
  title = "Saline infusions (EEG right)",
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
    method = "pearson",
    label.x = -0.1,
    label.sep = "\n"
  )
)

ggscatter(
  RIGHT_muscimol,
  x = "DR_1",
  y = "Mean_Pwr",
  title = "Muscimol infusions (EEG right)",
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
    method = "pearson",
    label.x = -0.1,
    label.sep = "\n"
  )
)

ggscatter(
  RIGHT_saline,
  x = "DR_1",
  y = "Mean_Pwr",
  title = "Saline infusions (EEG right)",
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
    method = "pearson",
    label.x = -0.1,
    label.sep = "\n"
  )
)


# Compare theta power and energy between conditions
# EEG RIGHT 

t.test(RIGHT_muscimol$Total_Energy, RIGHT_saline$Total_Energy, paired = TRUE)
mean(RIGHT_muscimol$Total_Energy, na.rm = TRUE)
mean(RIGHT_saline$Total_Energy, na.rm = TRUE)

t.test(RIGHT_muscimol$Mean_Pwr, RIGHT_saline$Mean_Pwr, paired = TRUE)
mean(RIGHT_muscimol$Mean_Pwr, na.rm = TRUE)
mean(RIGHT_saline$Mean_Pwr, na.rm = TRUE)

t.test(RIGHT_muscimol$Mean_REM_Duration, RIGHT_saline$Mean_REM_Duration, paired = TRUE)
mean(RIGHT_muscimol$Mean_REM_Duration, na.rm = TRUE)
mean(RIGHT_saline$Mean_REM_Duration, na.rm = TRUE)

t.test(RIGHT_muscimol$Total_REM_Duration, RIGHT_saline$Total_REM_Duration, paired = TRUE)
mean(RIGHT_muscimol$Total_REM_Duration, na.rm = TRUE)
mean(RIGHT_saline$Total_REM_Duration, na.rm = TRUE)

