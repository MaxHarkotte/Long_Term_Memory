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
behavior <- read_excel(paste0(dataPath,"Data/Behavior info.xlsx"))
behavior$id <- as.factor(substr(behavior$Animal, 1, 2))
behavior$Sampling <- as.factor(behavior$Sampling)

spindles <-
  read.csv2(
    paste0(dataPath, "Data/Spindels_summary_EEGLeft.csv"),
    sep = ",",
    col.names =  c(
      "Animal",
      "Recording",
      "Number_Spi",
      "Densitiy_permin",
      "Mean_Duration",
      "Mean_Power"
    ),
    header = F
  )

spindles$Animal <- as.factor(sprintf("%02d", as.numeric(spindles$Animal)))
spindles$Recording <- as.factor(spindles$Recording)
spindles$Densitiy_permin <- as.numeric(as.character(spindles$Densitiy_permin))
spindles$Mean_Duration <- as.numeric(as.character(spindles$Mean_Duration))
spindles$Mean_Power <- as.numeric(as.character(spindles$Mean_Power))

df_dat <- merge(
    behavior,
    spindles,
    by.x = c("id", "Sampling"),
    by.y = c("Animal", "Recording")
  )

# write.csv(df_dat,
#           file.path(dataPath, "Data/Spindles_Behavior_EEGLeft.csv"),
#           row.names = FALSE)
#   

df_dat <-
  subset(df_dat, df_dat$Animal != "05-P10-S1")

mus <- subset(df_dat, df_dat$Drug == "MUS")
sal <- subset(df_dat, df_dat$Drug == "SAL")
# sal[nrow(sal)+1,] <- NA # make same length (data missing for Saline for animal 5)
# sal$Animal[is.na(sal$Animal)] <- "05-P10-S1"
sal <- sal[order(sal$Animal),]
mus <- mus[order(mus$Animal),]

# Memory -------------------------------------------------------------
# Mean Duration -----------------------------------------------------------
mean(df_dat$Mean_Duration)
sd(df_dat$Mean_Duration)

ggscatter(
  df_dat,
  x = "Mean_Duration",
  y = "DR_1",
  title = "Spindle duration  (EEG right)",
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
    label.y = 1,
    label.sep = "\n"
  )
)

mean(mus$Mean_Duration)
sd(mus$Mean_Duration)

ggscatter(
  mus,
  x = "Mean_Duration",
  y = "DR_1",
  title = "Muscimol - Spindle duration (EEG right)",
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
    label.y = 1,
    label.sep = "\n"
  )
)

mean(sal$Mean_Duration)
sd(sal$Mean_Duration)

ggscatter(
  sal,
  x = "Mean_Duration",
  y = "DR_1",
  title = "Saline - Mean spindle duration (EEG right)",
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
    label.y = 0.1,
    label.sep = "\n"
  )
)+scale_y_continuous(name = "DR",
                    breaks = seq(-0.4, 1, 0.1),
                    limits = c(-0.4, 1)) +
  scale_x_continuous(
    name = "Mean Duration",
    breaks = seq(0.2, 0.8, 0.1),
    limits = c(0.2, 0.8)
  ) 



t.test(mus$Mean_Duration, sal$Mean_Duration, paired = TRUE)



# Mean Power -----------------------------------------------------------
mean(df_dat$Mean_Power)
sd(df_dat$Mean_Power)

ggscatter(
  df_dat,
  x = "Mean_Power",
  y = "DR_1",
  title = "Spindle power  (EEG right)",
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
    label.y = 1,
    label.sep = "\n"
  )
)

mean(mus$Mean_Power)
sd(mus$Mean_Power)

ggscatter(
  mus,
  x = "Mean_Power",
  y = "DR_1",
  title = "Muscimol - Spindle power (EEG right)",
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
    label.y = 1,
    label.sep = "\n"
  )
)

mean(sal$Mean_Power)
sd(sal$Mean_Power)

ggscatter(
  sal,
  x = "Mean_Power",
  y = "DR_1",
  title = "Saline - Mean spindle power (EEG right)",
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
    label.y = 0.1,
    label.sep = "\n"
  )
)+scale_y_continuous(name = "DR",
                     breaks = seq(-0.4, 1, 0.1),
                     limits = c(-0.4, 1)) +
  scale_x_continuous(
    name = "Mean Power",
    breaks = seq(0, 0.06, 0.01),
    limits = c(0, 0.06)
  ) 

t.test(mus$Mean_Power, sal$Mean_Power, paired = TRUE)


# Spindle number -----------------------------------------------------------
mean(df_dat$Number_Spi)
sd(df_dat$Number_Spi)

ggscatter(
  df_dat,
  x = "Number_Spi",
  y = "DR_1",
  add = "reg.line",
  conf.int = TRUE,
  color = "Drug",
  palette = "jco",
) + stat_cor(
  aes(color = Drug),
  method = "spearman",
  label.x = 3,
  label.y = c(1.4, 1.5)
)

library("RVAideMemoire")

spearman.ci(mus$Number_Spi, mus$DR_1, nrep = 1000, conf.level = 0.95)
spearman.ci(sal$Number_Spi, sal$DR_1, nrep = 1000, conf.level = 0.95)

ggscatter(
  df_dat,
  x = "Number_Spi",
  y = "DR_1",
  title = "Spindle number  (EEG right)",
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
    label.y = 1,
    label.sep = "\n"
  )
)

mean(mus$Number_Spi)
sd(mus$Number_Spi)

ggscatter(
  mus,
  x = "Number_Spi",
  y = "DR_1",
  title = "Muscimol - Spindle number (EEG right)",
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
    label.y = 1,
    label.sep = "\n"
  )
)

cor.test(mus$Number_Spi, mus$DR_1, method=c("spearman"))

mean(sal$Number_Spi)
sd(sal$Number_Spi)

ggscatter(
  sal,
  x = "Number_Spi",
  y = "DR_1",
  title = "Saline - Spindle number (EEG left)",
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
    label.y = 0.1,
    label.sep = "\n"
  )
)


t.test(mus$Number_Spi, sal$Number_Spi, paired = TRUE)

# Spindle density -----------------------------------------------------------
mean(df_dat$Densitiy_permin)
sd(df_dat$Densitiy_permin)

ggscatter(
  df_dat,
  x = "Densitiy_permin",
  y = "DR_1",
  title = "Spindle density per min (EEG right)",
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
    label.y = 1,
    label.sep = "\n"
  )
)

mean(mus$Densitiy_permin)
sd(mus$Densitiy_permin)

ggscatter(
  mus,
  x = "Densitiy_permin",
  y = "DR_1",
  title = "Muscimol - Spindle density per min  (EEG right)",
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
    label.y = 1,
    label.sep = "\n"
  )
)


mean(sal$Densitiy_permin)
sd(sal$Densitiy_permin)

ggscatter(
  sal,
  x = "Densitiy_permin",
  y = "DR_1",
  title = "Saline - Spindle density per min  (EEG right)",
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
    label.y = 1,
    label.sep = "\n"
  )
)


t.test(mus$Densitiy_permin, sal$Densitiy_permin, paired = TRUE)


# Rearing -----------------------------------------------------------------
# Mean Duration -----------------------------------------------------------
ggscatter(
  df_dat,
  x = "Mean_Duration",
  y = "dRear_1",
  title = "Spindle duration  (EEG right)",
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
    label.y = 1,
    label.sep = "\n"
  )
)

ggscatter(
  mus,
  x = "Mean_Duration",
  y = "dRear_1",
  title = "Muscimol - Spindle duration (EEG right)",
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
    label.y = 1,
    label.sep = "\n"
  )
)


ggscatter(
  sal,
  x = "Mean_Duration",
  y = "dRear_1",
  title = "Saline - Mean spindle duration (EEG right)",
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
    label.y = 0.1,
    label.sep = "\n"
  )
)


# Mean Power -----------------------------------------------------------
ggscatter(
  df_dat,
  x = "Mean_Power",
  y = "dRear_1",
  title = "Spindle power  (EEG right)",
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
    label.y = 1,
    label.sep = "\n"
  )
)

ggscatter(
  mus,
  x = "Mean_Power",
  y = "dRear_1",
  title = "Muscimol - Spindle power (EEG right)",
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
    label.y = 1,
    label.sep = "\n"
  )
)

ggscatter(
  sal,
  x = "Mean_Power",
  y = "dRear_1",
  title = "Saline - Mean spindle power (EEG right)",
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
    label.y = 0.1,
    label.sep = "\n"
  )
)

# Spindle number -----------------------------------------------------------
ggscatter(
  df_dat,
  x = "Number_Spi",
  y = "dRear_1",
  title = "Spindle number  (EEG left)",
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
    label.y = 1,
    label.sep = "\n"
  )
)

ggscatter(
  mus,
  x = "Number_Spi",
  y = "dRear_1",
  title = "Muscimol - Spindle number (EEG left)",
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
    label.y = 1,
    label.sep = "\n"
  )
)

ggscatter(
  sal,
  x = "Number_Spi",
  y = "dRear_1",
  title = "Saline - Spindle number (EEG right)",
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
    label.y = 0.1,
    label.sep = "\n"
  )
)


# Spindle density -----------------------------------------------------------
ggscatter(
  df_dat,
  x = "Densitiy_permin",
  y = "dRear_1",
  title = "Spindle density per min (EEG right)",
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
    label.y = 1,
    label.sep = "\n"
  )
)

ggscatter(
  mus,
  x = "Densitiy_permin",
  y = "dRear_1",
  title = "Muscimol - Spindle density per min  (EEG right)",
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
    label.y = 1,
    label.sep = "\n"
  )
)

ggscatter(
  sal,
  x = "Densitiy_permin",
  y = "dRear_1",
  title = "Saline - Spindle density per min  (EEG right)",
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
    label.y = 1,
    label.sep = "\n"
  )
)


# Save tables -------------------------------------------------------------

write.csv(df_dat,
           file.path(dataPath, "Data/Spindels_Behavior_EEGLeft_formatted.csv"),
           row.names = FALSE)

