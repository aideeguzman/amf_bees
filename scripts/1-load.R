## *****************************************************************************
## load ########################################################################
## *****************************************************************************


## *****************************************************************************
## packages ####################################################################
## *****************************************************************************

library(car)
library(cowplot)
library(ggplot2)
library(vegan)
library(lme4)
library(lmerTest)
library(emmeans)
library(stringr)
library(reshape2)
library(ggthemes)
library(optimx)
library(afex)
library(tidyverse)
library(piecewiseSEM)
library(MuMIn)
library(pscl)
library(caret)
library(glmnet)
library(Rcpp)
library(arm)
library(janitor)
library(plotrix)
library(openxlsx)
library(multcomp)
library(broom)
library(ggtext)
library(scales)
library(emoji)



## *****************************************************************************
## data ####################################################################
## *****************************************************************************

df_all <- read.csv("data/df_all.csv") %>%
  mutate(field_measure_date = 
           as.Date(field_measure_date, format = "%Y-%m-%d"))
df_plant_amf_traits <- read.csv("data/df_plant_amf_traits.csv")
df_flower_traits <- read.csv("data/df_flower_traits.csv")
df_bee_visits <- read.csv("data/df_bee_visits.csv") %>%
  mutate(field_measure_date = 
           as.Date(field_measure_date, format = "%Y-%m-%d"))
df_sem <- read.csv("data/df_sem.csv")

## *****************************************************************************
## functions ####################################################################
## *****************************************************************************

source("scripts/functions.R")

## *****************************************************************************
## figure path ####################################################################
## *****************************************************************************

fig.path = "figures"
