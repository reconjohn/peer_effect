# library and data

# libraries
library(tidyr)
library(dplyr)
library(stringr)
library(readxl)
library(stringdist)
library(tidyverse)
library(openxlsx)
library(readr)
library(scales)
library(sf)
library(stargazer)
library(Hmisc)
library(lubridate)
library(ggpubr)
library(tidycensus)
library(mapview)
library(stargazer)

sf::sf_use_s2(FALSE)
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

load("./data/data.RData") # capa (national), labr (state)
load("C:/Users/yohan/Big_data/US Tract/census.Rdata") #tr.sf, st

# GEM
data <- read_excel("./data/Global-Steel-Plant-Tracker-2023-03-2.xlsx", sheet = 2) %>% 
  filter(Country == "United States") %>% 
  filter(Status == "operating") %>% 
  
  rename(Cap_S = "Nominal crude steel capacity (ttpa)",
         Cap_I = "Nominal iron capacity (ttpa)") %>% 
  
  mutate(Type = 
           ifelse(grepl("BF",`Main production equipment`), "BF_I", 
                  `Main production equipment`),
         Cap_S = ifelse(Cap_S == "N/A", 0, as.numeric(Cap_S)),
         Cap_I = ifelse(Cap_I == "N/A", 0, as.numeric(Cap_I)),
         Cap_T = Cap_S + Cap_I,
         Emp = as.numeric(`Workforce size`)) %>% 
  
  
  separate(Coordinates, c("Lat", "Lon"), ",") %>% 
  st_as_sf(coords = c("Lon", "Lat"), crs = 4269,
           agr = "constant",
           stringsAsFactors = FALSE,
           remove = TRUE)

# flight + GEM
data_f <- read_csv("./data/steel_iron_facility.csv") %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = 4269,
           agr = "constant",
           stringsAsFactors = FALSE,
           remove = TRUE) %>% 
  mutate(Type = ifelse(Product == "BF-BOF", "BF_I", 
                       ifelse(Product %in% c("Coke","Ore"), "Others", 
                              Product))) 
