#Data Analysis of DSC
## Date: 05/01/2023

## Loading the data

# TAsk:
# Find the area under de curve (AUC) of the three materials and for each in the crystallinity
#   (ΔHc) (second set of data) and melting curve (ΔHm) (third set of data).
#   Explantion: when we perform a DSC test you have to heat the material in order to see the melting point, in this case,
#   BB (From -20 to 270 °C), rHDPE (20 - 250 °C) n rPET (20-270) (first set of data) . Then the
#   material is cold down with the inverse tempertures to see its crystallization (second set).
#   Finally, heated again to see the real melting point (third set of data).


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
names(DSC) <- c("Material", "Index", "Time", "Ts", "Tr", "Value")




## Graphique of the Profile
DSC %>% 
  ggplot()+
  aes(x = Time, y = Ts, color = Material) +
  facet_wrap( ~ Material, ncol = 1 ) +
  geom_line() +
  coord_cartesian(xlim = c(1700 , 2000))  


##
DSC %>% 
  ggplot()+
  aes(x = Ts, y = Value, color = Material) +
  facet_wrap( ~ Material, ncol = 1 ) +
  geom_point() +
  geom_line(aes(x = Time, y = Ts)) 
#  coord_cartesian(xlim = c(2400 , 2600)) + 

  
#  CRISTALINIZACION  -------

## Calculating the min and max lines----
Table_Max <- 
  DSC %>% 
  filter(Time > 1000 & Time < 2200) %>% 
  group_by(Material) %>% 
  slice_max(Ts, n = 1) %>% 
  arrange(Material, Time) %>% 
  select(Material, Time) %>% 
  set_names("Material", "Time.t1")

Table_Min <- 
  DSC %>% filter(Time > 2200, Time < 4200 ) %>% 
  group_by(Material) %>% 
  slice_min(Ts, n = 1) %>% 
  arrange(Material, Time) %>% 
  select(Material, Time) %>% 
  set_names("Material", "Time.t2")

Criteria <- 
  Table_Max %>% left_join(Table_Min, by = "Material")

rm(Table_Max, Table_Min)

## Filtering the Database for the Cristalization process  

Cristallization <- 
  DSC %>% 
  group_by(Material) %>%
  inner_join(Criteria) %>% 
  filter(Time > Time.t1 & Time < Time.t2)

## Calculating the Area Under the Curve ----
AUC_cristal <- 
  tibble(
    Material = c("pBC", "rHDPE", "rPET"),
    Temp.1 = c(110, 95, 183),
    Temp.2 = c(125, 121, 210))


AUC_value <- 
  Cristallization %>% 
  group_by(Material) %>%
  inner_join(AUC_cristal, by = "Material") %>% 
  filter(Ts > Temp.1 & Ts < Temp.2 ) 
  

Table_AUC_value <- 
  AUC_value  %>% 
  summarise(
    AUC_Value = AUC( x = Ts, y = Value) # Evaluating the AUC but filterint between each rage from the table AUC_cristal
  )

## Calculating the peak temperature of crystallization

Peak_Crist <-
  DSC%>% filter(Time > 1550, Time < 4100 ) %>% 
  group_by(Material) %>% 
  slice_max(Value, n = 1) %>% 
  arrange(Material, Ts ) %>% 
  select(Material, Ts) %>% 
  set_names("Material",  "Temp.peak")


Table_Peak_Crist <-
  Peak_Crist %>% 
  group_by(Material) %>%
  summarise(
    Peak_Crist = findpeaks ( Value,npeaks=1, sortstr= FALSE ) # Evaluating the AUC but filterint between each rage from the table AUC_cristal
  )

## Graphiques de Cristallization with the calculation of the Area Under the Curve
Cristallization %>% 
  ggplot()+
  aes(x = Ts, y = Value, color = Material) +
  facet_wrap( ~ Material, ncol = 1 ) +
  geom_line() +
  coord_cartesian(xlim = c(95 , 210)) +
  #geom_point(size = 1) 
  geom_polygon(data = AUC_value, 
               aes(x = Ts, 
                   y = Value, 
                   fill = Material),
               ) +
  labs(title = "Cristallization curves",
       #subtitle = "",
       x = "Temperature [°C]",
       y = "Heating flow [W/g]",
       caption = "Made from love") +
  theme_minimal()

ggsave("Cristallization.jpg", width = 7, height = 5, dpi = "print")

####
# Melting -----

## Calculating the min and max lines----


Table_Min <- 
  DSC%>% filter(Time > 2200, Time < 4200 ) %>% 
  group_by(Material) %>% 
  slice_min(Ts, n = 1) %>% 
  arrange(Material, Time) %>% 
  select(Material, Time) %>% 
  set_names("Material", "Time.t3")

Table_Max <- 
  DSC %>% 
  filter(Time > 4000 & Time < 5900) %>% 
  group_by(Material) %>% 
  slice_max(Ts, n = 1) %>% 
  arrange(Material, Time) %>% 
  select(Material, Time) %>% 
  set_names("Material", "Time.t4")

Criteria_Melt<- 
  Table_Min %>% left_join(Table_Max, by = "Material")

rm(Table_Min, Table_Max)

## Filtering the Database for the Melting process  

Melting <- 
  DSC %>% 
  group_by(Material) %>%
  inner_join(Criteria_Melt) %>% 
  filter(Time > Time.t3 & Time < Time.t4)

## Calculating the Area Under the Curve ----
AUC_Melt <- 
  tibble(
    Material = c("pBC", "rHDPE", "rPET"),
    Temp.3 = c(120, 120, 210),
    Temp.4 = c(140, 150, 260))

AUC_value_Melt <- 
  Melting %>% 
  group_by(Material) %>%
  inner_join(AUC_Melt, by = "Material") %>% 
  filter(Ts > Temp.3 & Ts < Temp.4 )

Table_AUC_value_Melt <- 
  AUC_value_Melt  %>% 
  summarise(
    AUC_value_Melt = AUC( x = Ts, y = Value) # Evaluating the AUC but filterint between each rage from the table AUC_Melt
  )

## Calculating the peak temperature of Melting 

Peak_Melt <-
  DSC%>% filter(Time > 3000, Time < 5900 ) %>% 
  group_by(Material) %>% 
  slice_min(Value, n = 1) %>% 
  arrange(Material, Ts ) %>% 
  select(Material, Ts) %>% 
  set_names("Material",  "Temp.peak")


Table_Peak_Melt <-
  Peak_Melt %>% 
  group_by(Material) %>%
  summarise(
    Peak_Melt = findpeaks ( Value,npeaks=1, sortstr= FALSE ) # Evaluating the AUC but filterint between each rage from the table AUC_cristal
  )

## Transforming the column into numeric
Peak_Melt$Temp.peak <- as.numeric(Peak_Melt$Temp.peak)

## Graphiques of Melting  with the calculation of the Area Under the Curve
Melting %>% 
  ggplot()+
  aes(x = Ts, y = Value, color = Material) +
  facet_wrap( ~ Material, ncol = 1 ) +
  geom_line() +
  coord_cartesian(xlim = c(100 , 270)) +
  #geom_point(size = 1) 
  geom_polygon(data = AUC_value_Melt, 
               aes(x = Ts, 
                   y = Value, 
                   fill = Material)+
  geom_text(data = Peak_Melt, 
                  aes(x = c(250, 130, 250), y = c(-1,-3,-1)), 
                  label = Peak_Melt$Temp.peak,
                  vjust = 1,
                  hjust=0),
  )+
  labs(title = "Melting curves",
      # subtitle = "for the Three materials",
       x = "Temperature [°C]",
       y = "Heating flow [W/g]",
       caption = "Made from love") +
  theme_minimal()

ggsave("Melting.jpg", width = 7, height = 5, dpi = "print")
















