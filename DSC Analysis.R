#Data Analysis of DSC
## Date: 05/01/2023

# Load Libraries
library(tidyverse)
library(here)
library(kableExtra)
library(ggpubr)
library (dbplyr)
library(haven)



# Data Loading ----
## Loading the paths
files <- 
  here::here("Data") %>%
  dir( recursive=TRUE, full.names=TRUE, pattern="\\.txt$")


### Function to identify the names of the data ----
names <- 
  here::here("Data") %>%
  dir( recursive=TRUE, pattern="\\.txt$")


## Creating a function to transform the data 
data_trans <- function(path_file, material ){
  
  #  path_file = files[1]; material = c("BB")
  
  table <- read_delim(file = path_file, skip = 2, delim = " ")
  x <-table[1]
  names(x)  <-  c("var_x") 
  y <- names(table)[-1]
  y <- as.numeric(y)
  
  # Making the table
  data <- tibble( x , var_y = y ) %>% 
    #set_names("var_x", "var_y") %>% 
    mutate(Material = material)
  return(data)
}


## Reading teach dataframe -----
### BB
BB1.1 <- data_trans(path_file = files[1], names[1])
BB1.2 <- data_trans(path_file = files[2], names[2])
BB1.3 <- data_trans(path_file = files[3], names[3])
BB2.1 <- data_trans(path_file = files[4], names[4])
BB2.2 <- data_trans(path_file = files[5], names[5])
BB3.3 <- data_trans(path_file = files[6], names[6])

### HDPE
HDPE1.1 <- data_trans(path_file = files[7], names[7])
HDPE1.2 <- data_trans(path_file = files[8], names[8])
HDPE1.3 <- data_trans(path_file = files[9], names[9])
HDPE2.1 <- data_trans(path_file = files[10], names[10])
HDPE2.2 <- data_trans(path_file = files[11], names[11])
HDPE3.3 <- data_trans(path_file = files[12], names[12])

### HDPE
PET1.1 <- data_trans(path_file = files[13], names[13])
PET1.2 <- data_trans(path_file = files[14], names[14])
PET1.3 <- data_trans(path_file = files[15], names[15])
PET2.1 <- data_trans(path_file = files[16],  names[16])
PET2.2 <- data_trans(path_file = files[17], names[17])
PET3.3 <- data_trans(path_file = files[18], names[18])



## Joining together the data------
data <- 
  rbind(
    BB1.1,
    BB1.2,
    BB1.3,
    BB2.1,
    BB2.2,
    BB3.3,
    HDPE1.1,
    HDPE1.2,
    HDPE1.3,
    HDPE2.1,
    HDPE2.2,
    HDPE3.3,
    PET1.1,
    PET1.2,
    PET1.3,
    PET2.1,
    PET2.2,
    PET3.3
  )

## Data

data <- data %>% separate(Material, into = c("Material", "File"), sep = "/")


### Obtaining the Max values

Max_values <- data %>% group_by(File) %>% summarise(Max = max ( var_x))


### Normalizacion
Normalizacion <-
  data %>% left_join(Max_values , by = "File")


### Maing the Normalization vector

Normalizacion <- 
  Normalizacion %>% 
  mutate( var_normal = var_x / Max)



## Making the Graphic
Normalizacion %>% 
  ggplot(aes( x = var_y*(-1), y = Var_normal  , color = File)) + 
  geom_line() +
  facet_wrap( ~ Material , ncol=1) 




## Making a Focus on the Shared Pic
Normalizacion %>% 
  ggplot(aes( x = var_y*(-1), y = var_x , color = File)) + 
  geom_line() +
  facet_wrap( ~ Material , ncol=1) +
  coord_cartesian(
    xlim = c(-3000, -500),
    expand = TRUE,
    default = FALSE,
    clip = "on"
  )


### Hipotesis de Media

Mean_graphs <- 
  Normalizacion %>% 
  group_by(Material, var_y) %>% 
  summarise( Mean_var_normal = mean(var_normal) )



## Making the Graphic joint
Mean_graphs %>% 
  ggplot(aes( x = var_y*(-1), y = Mean_var_normal  , color = Material)) + 
  geom_line() +
  coord_cartesian(xlim = c(-3000, -500)) +
  #coord_cartesian(xlim = c(10,18), ylim=c(0,8)) +
  theme_minimal(base_size = 15, base_family = "Palatino") +
  labs(title="Fourier Transform Infrared Spectrometry",  
       y="Absorbance Units", x="Wavenumber  [cm-1]" ) +
  theme(plot.background = element_rect(fill = "white")) 









