# employment factor comparison 
# based on capacity 
# national
source("./syntax/data.R")

### F1
dis1 <- capa %>% 
  mutate_at(vars(USGS,BLS), funs(./1000)) %>% 
  ggplot(aes(x = USGS, y = BLS)) + 
  geom_point() +
  geom_abline(linetype = 2) +
  geom_smooth(method = lm) +
  labs(x = "Employment from USGS (k)", y = "Employment from BLS (k)",
       title = "a") +
  theme_classic()


dis2 <- capa %>% 
  mutate_at(vars(USGS,BLS), funs(./1000)) %>% 
  gather("key", "value", USGS, BLS) %>% 
  ggplot(aes(x = Year, y = value, color = key)) +
  geom_line(linewidth = 1.3) +
  # geom_vline(xintercept = 2007, linetype = "dotted") +
  labs(y = "Employment (k)", color = "", title = "b") +
  theme_classic() +
  theme(legend.position = "none")

ggsave("./fig/lab_usgs_compare.png", 
       ggarrange(dis1, dis2), width = 10, height = 3)

### F2

# state 
# GEM
employ <- st %>% 
  dplyr::select(State) %>% 
  st_join(data_f) %>% 
  group_by(State) %>% 
  summarise(Emp = sum(Employment, na.rm = T)/1000) %>% 
  mutate_at(c('Emp'), ~na_if(., 0)) %>% # replace 0 with NA
  ggplot() +
  geom_sf(data = st, fill = "gray95", color = "gray60", size = 0.1) +
  geom_sf(aes(fill = Emp)) +
  
  scale_fill_continuous(type = "viridis", limits = c(0,31)) +
  
  # scale_fill_gradientn(viridis,
  #                     limits = c(0,20)) +
  
  
  labs(fill = "Employment (k)",
       title = "a") +
  theme_minimal() +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2700000), 
           ylim = c(-2300000,730000), expand = FALSE, datum = NA)


# GEM + labor
qcewGetIndustryData <- function (year, qtr, industry) {
  url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/industry/INDUSTRY.csv"
  url <- sub("YEAR", year, url, ignore.case=FALSE)
  url <- sub("QTR", tolower(qtr), url, ignore.case=FALSE)
  url <- sub("INDUSTRY", industry, url, ignore.case=FALSE)
  read.csv(url, header = TRUE, sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
}

lab <- st %>% 
  left_join(qcewGetIndustryData("2022", "a", "3311") %>% 
              filter(agglvl_code == 56) %>% 
              mutate(GEOID = substr(area_fips, 1,2)), by = "GEOID") %>% 
  rename(Employ = annual_avg_emplvl) %>% 
  
  dplyr::select(NAME, State, Employ) %>% 
  left_join(data %>% 
              group_by(`Subnational unit (province/state)`) %>% 
              summarise(Emp = sum(Emp)) %>% # GEM employment
              st_drop_geometry(),
            by = c("NAME" = "Subnational unit (province/state)")) # Employ 3311 labor statistics 

# labor statistics 3311
labr <- lab %>% 
  mutate_at(c('Employ'), ~na_if(., 0)) %>%  # replace 0 with NA
  ggplot() +
  geom_sf(data = st, fill = "gray95", color = "gray60", size = 0.1) +
  geom_sf(aes(fill = Employ/1000)) +
  
  scale_fill_continuous(type = "viridis", limits = c(0,31)) + 
  
  labs(fill = "Employment (k)",
       title = "b") +
  theme_minimal() +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2700000), 
           ylim = c(-2300000,730000), expand = FALSE, datum = NA)


# labor vs. GEM
sc <- lab %>% 
  mutate_at(vars(Emp, Employ), funs(./1000)) %>% 
  ggplot(aes(x = Emp, y = Employ)) +
  geom_point() +
  geom_abline(linetype = 2) +
  geom_smooth(method = lm) +
  labs(x = "Employment from GEM (k)", y = "Employment from BLS (k)",
       title = "c") +
  theme_classic()


ggsave("./fig/state_emp.png", ggarrange(ggarrange(employ, labr, nrow = 2), sc, nrow = 1,
                                        widths = c(1.5, 1)), width = 12, height = 6)



### F3
capa <- capa %>% 
  rename(National = BLS)

# state
labr <- labr %>% 
  dplyr::select(-State) %>% 
  rename(State = Employ)


c13 <- capa %>% 
  mutate(National = National/1000) %>% 
  ggplot(aes(x = Steel_cp, y = National))+
  geom_point() +
  # geom_abline(linetype = 2) +
  geom_smooth(method = lm) +
  labs(title = "a", x = "Capacity (MMT)", y = "Employment (k)") +
  theme_classic()


c15 <- capa %>% 
  ggplot(aes(x = Year, y = Steel_cp))+
  geom_point() +
  geom_abline(linetype = 2) +
  geom_smooth(method = lm) +
  labs(title = "b", x = "Year", y = "Capacity (MMT)") +
  theme_classic()


c14 <- capa %>% 
  mutate(TR = National/ Steel_cp) %>% 
  ggplot(aes(x = Year, y = TR))+
  geom_point() +
  geom_abline(linetype = 2) +
  geom_smooth(method = lm) +
  labs(title = "c", x = "Year", y = "Employment/ Capacity") +
  theme_classic()


c23 <- labr %>% 
  mutate(State = State/ 1000) %>% 
  ggplot(aes(x = Steel_cp, y = State))+
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "d", x = "Capacity (MMT)", y = "Employment (k)") +
  theme_classic()


ggsave("./fig/model_compare.png", 
       ggarrange(c13, c15, c14, c23, ncol = 2, nrow = 2), width = 9, height = 7)


