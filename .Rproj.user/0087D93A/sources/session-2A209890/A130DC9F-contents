# data upload

### Neighbor installations
load("./data/diffusion.Rdata") # dfPV,dfEV, dfPump, s_house
load("./data/diffusionVT.Rdata") # dfVT, v_house

### ACS
load("./data/seattle.Rdata") # seattle_tracts 
load("./data/vermont.Rdata") # temp 

### Technology adoption
load("./data/Seattle_Data.Rdata") # PV, EV, Pump
VPV <- read_csv(file = "./data/Rooftop_solar_VT.csv") %>% 
  filter(`Organization Type` == "Residential") %>% 
  mutate(Year = year(`Installation Date`),
         Month = month(`Installation Date`),
         kW = `Capacity kW`) %>% 
  rename(Lon = Long) %>% 
  dplyr::select(City, `Zip Code`, Lat, Lon, Year, Month, kW)


### DAC 
DAC <- read_csv("https://github.com/reconjohn/disadvantaged_communities/raw/main/results/DAC_s.csv")

### geospatial boundaries
load("C:/Users/yohan/Big_data/US Tract/census.Rdata") #tr.sf