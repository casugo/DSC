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
  aes(x = Time, )

#Delate columns 2,3 and 5
DSC <<- DSC [, -c(2,3,5)]

DSC$Ts <-
  as.numeric(DSC$Ts)
DSC$Value <-
  as.numeric(DSC$Value)

BB_Hc <- 
  DSC$Material$BB %>%
  ?AUC(DSC$Ts, DSC$Value, From= 100, to= 120, method = "step")


