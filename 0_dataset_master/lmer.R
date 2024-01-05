#install.packages("lme4", type = "source")
#install.packages("lmerTest", type = "source")
#install.packages("mediation", type = "source")
#install.packages("devtools") # Devtools is a package which allows to do this.
#devtools::install_github("dustinfife/flexplot")
#detach("package:lmerTest", unload=TRUE)
#install.packages("devtools") ## if not already installed
require(devtools)
install_github("kollerma/robustlmm")
require(robustlmm)

library(tidyverse)
library(sjPlot)
#library(lmerTest)
library(lme4)
library(flexplot)
library(mediation)


setwd("C:/Users/ASuS/OneDrive/Desktop/Work_in_progress/Cognitive_performance/R")
data_raw <- as.data.frame(readxl::read_excel("Experiment_edited_V5b.xlsx", sheet = 1))
summary(data_raw)

#55_NV3vsNV0
data_55_NV3vsNV0 = data_raw[
  (data_raw['Traffic_noise_levels']==55) & 
    ((data_raw['Thermal']=="NV fan 0") | (data_raw['Thermal']=="NV fan 3")),
]
summary(data_55_NV3vsNV0)
direct_DTSET_55_NV3vsNV0 = rlmer(DoubleTrouble_average ~ SET + (SET | subid),  data=data_55_NV3vsNV0)
tab_model(direct_DTSET_55_NV3vsNV0)
visualize(direct_DTSET_55_NV3vsNV0, plot = "model", sample=34)

mediate_DTTS_55_NV3vsNV0 = lmer(ThermalSatisfaction ~ SET + (1 | subid),  data=data_55_NV3vsNV0)
tab_model(mediate_DTTS_55_NV3vsNV0)
full_DTSET_55_NV3vsNV0 = lmer(DoubleTrouble_average ~ ThermalSatisfaction + SET + (1 | subid),  data=data_55_NV3vsNV0)
tab_model(full_DTSET_55_NV3vsNV0)
complete_DTSET_55_NV3vsNV0 = mediate(mediate_DTTS_55_NV3vsNV0, full_DTSET_55_NV3vsNV0, treat ='SET',
                                     mediator = 'ThermalSatisfaction',
                                     boot = FALSE, sims = 1000)
summary(complete_DTSET_55_NV3vsNV0)


#55_NV5vsNV0
data_55_NV5vsNV0 = data_raw[
  (data_raw['Traffic_noise_levels']==55) & 
    ((data_raw['Thermal']=="NV fan 0") | (data_raw['Thermal']=="NV fan 5")),
]
summary(data_55_NV5vsNV0)
direct_DTSET_55_NV5vsNV0 = lmer(DoubleTrouble_average ~ SET + (1 | subid),  data=data_55_NV5vsNV0)
tab_model(direct_DTSET_55_NV5vsNV0)
visualize(direct_DTSET_55_NV5vsNV0, plot = "model", sample=34)

mediate_DTTS_55_NV5vsNV0 = lmer(ThermalSatisfaction ~ SET + (1 | subid),  data=data_55_NV5vsNV0)
tab_model(mediate_DTTS_55_NV5vsNV0)
full_DTSET_55_NV5vsNV0 = lmer(DoubleTrouble_average ~ ThermalSatisfaction + SET + (1 | subid),  data=data_55_NV5vsNV0)
tab_model(full_DTSET_55_NV5vsNV0)
complete_DTSET_55_NV5vsNV0 = mediate(mediate_DTTS_55_NV5vsNV0, full_DTSET_55_NV5vsNV0, treat ='SET',
                                     mediator = 'ThermalSatisfaction',
                                     boot = FALSE, sims = 1000)
summary(complete_DTSET_55_NV5vsNV0)

#55_ACvsNV0
data_55_ACvsNV0 = data_raw[
  (data_raw['Traffic_noise_levels']==55) & 
    ((data_raw['Thermal']=="NV fan 0") | (data_raw['Thermal']=="AC")),
]
summary(data_55_ACvsNV0)
direct_DTSET_55_ACvsNV0 = lmer(DoubleTrouble_average ~ SET + (1 | subid),  data=data_55_ACvsNV0)
tab_model(direct_DTSET_55_ACvsNV0)
visualize(direct_DTSET_55_ACvsNV0, plot = "model", sample=34)

mediate_DTTS_55_ACvsNV0 = lmer(ThermalSatisfaction ~ SET + (1 | subid),  data=data_55_ACvsNV0)
tab_model(mediate_DTTS_55_ACvsNV0)
full_DTSET_55_ACvsNV0 = lmer(DoubleTrouble_average ~ ThermalSatisfaction + SET + (1 | subid),  data=data_55_ACvsNV0)
tab_model(full_DTSET_55_ACvsNV0)
complete_DTSET_55_ACvsNV0 = mediate(mediate_DTTS_55_ACvsNV0, full_DTSET_55_ACvsNV0, treat ='SET',
                                     mediator = 'ThermalSatisfaction',
                                     boot = TRUE, sims = 1000)
summary(complete_DTSET_55_ACvsNV0)

