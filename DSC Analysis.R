#Data Analysis of DSC
## Date: 05/01/2023

# Load Libraries
library(tidyverse)
library(here)
library(kableExtra)
library(ggpubr)
library (dbplyr)
library(stringr)
library(haven)

# AUX
library(DescTools) # https://search.r-project.org/CRAN/refmans/DescTools/html/AUC.html#:~:text=For%20linear%20interpolation%20the%20AUC,to%20calculate%20a%20numerical%20integral.

# Data Loading ----
## Loading the paths
files <- 
  here::here("Data") %>%
  dir( recursive=TRUE, full.names=TRUE, pattern="\\.csv$")

files

### Function to identify the names of the data ----
names <- 
  here::here("Data") %>%
  dir( recursive=TRUE, pattern="\\.csv$")

DSC<- files %>% map( ~ read.csv2(.))
DSC <- DSC %>% set_names(names) %>% enframe ("Material", "Datos")

### Organising the dataframe ----
DSC <- 
  DSC %>% 
  separate(Material,
           sep = "/",
           into = c("Material","file"))

DSC$file <- NULL

## Unnesting the total dataframe for graphics ----
DSC <-
  DSC %>% 
  unnest(Datos)


# Calculating the Area Under the Curve -----
str(DSC)

## Transforming the column into numeric
DSC$Ts <- as.numeric(DSC$Ts)
DSC$Tr <- as.numeric(DSC$Tr)
DSC$Value <- as.numeric(DSC$Value)

## Changing the colnames
names(DSC) <- c("Material", "File", "Index", "Time", "Ts", "Tr", "Value")

## Graphique

DSC %>% 
  ggplot()+
  aes(x = Time, y = Value, color = Material) +
  geom_line() +
  coord_cartesian(xlim = c(2400 , 2600))
  
facet_wrap( ~ Material, ncol = 1 )

## Calculating the Area Under the Curve

AUC <- DSC %>% 
  filter(Time > 2400, Time <2600) %>% 
  group_by(Material) %>% 
  summarise(
    AUC_Value = AUC( x = Time, y = Value) # Evaluating the AUC but filterint between Time 2400 - 2600
  )



