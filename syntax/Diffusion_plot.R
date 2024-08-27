source("./syntax/Lib.R")
source("./syntax/Data.R")

### f1 
### installation trend
dfPV %>% 
  mutate(Tech = "PV (Seattle)") %>% 
  rbind(dfEV %>% 
          mutate(Tech = "EV (Seattle)")) %>% 
  filter(adopter == 1) %>% 
  dplyr::select(Year, Dist, Tech) %>% 
  rbind(dfPump %>% 
          filter(adopter == 1) %>% 
          dplyr::select(Year, Dist) %>% 
          mutate(Tech = "HP (Seattle)")) %>% 
  st_drop_geometry() %>% 
  rbind(dfVT %>% 
          filter(adopter == 1) %>% 
          dplyr::select(Year, Dist) %>% 
          mutate(Tech = "PV (Vermont)") %>% 
          st_drop_geometry()) %>% 
  
  filter(Dist == 50) %>% 
  group_by(Tech, Year) %>% 
  summarise(Count = n()) %>% 
  mutate(Cum = cumsum(Count),
         Tech = factor(Tech, levels = c("PV (Seattle)", "PV (Vermont)", "EV (Seattle)", "HP (Seattle)")),
         Year = as.character(Year),
         Year = substr(Year, 3,4)) %>%
  ggplot(aes(x = Year, y = Cum)) +
  geom_col(aes(fill = "Cumulative")) +
  geom_col(aes(x = Year, y = Count, fill = "Annex")) +
  scale_fill_manual(values = c("Cumulative" = "gray40", "Annex" = "gray70")) +
  labs(y = "Housing buildings with technology", fill = "",
       title = "a") +
  facet_wrap(~Tech, scale = "free", nrow = 1) +
  theme_classic() + 
  theme(legend.position = "bottom")


### mapping
# adding DAC category
f_map <- function(data, boundary, fips, name){
  
  boundary <- boundary %>% 
    st_join(tr.sf %>% 
              rename(GEOID = FIPS) %>% 
              filter(STATE_FIPS == fips) %>%
              dplyr::select(GEOID) %>% 
              left_join(DAC %>% 
                          dplyr::select(GEOID, disadvantaged), by = "GEOID") %>% 
              st_transform(st_crs(boundary)$epsg) %>% 
              dplyr::select(-GEOID),join = st_intersects, largest = TRUE) %>% 
    mutate(disadvantaged = factor(disadvantaged, levels = c(TRUE, FALSE))) 
  
  data %>% 
    filter(Dist == 50) %>% 
    filter(adopter == 1) %>% 
    st_centroid() %>%
    
    ggplot() +
    geom_sf(data = boundary, aes(fill = disadvantaged), color = "gray50", alpha = 0.5) + # border 
    geom_sf(color = "black", alpha = 0.6, size = 0.01) +
    
    scale_fill_manual(values = c("red", "gray100")) +
    
    labs(fill = "Disadvantaged community", title = name) +
    theme_minimal() +
    theme(plot.title = element_text(size=10),        
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          legend.position = "none")
}

spv <- f_map(dfPV, seattle_tracts, "53", "PV (Seattle)")
sev <- f_map(dfEV, seattle_tracts, "53", "EV (Seattle)")
shp <- f_map(dfPump, seattle_tracts, "53", "HP (Seattle)")
vpv <- f_map(dfVT, temp, "50", "PV (Vermont)")

garrange(spv,vpv,sev,shp,
                labels = NULL,
                ncol = 4, nrow = 1,
                common.legend = TRUE, legend = "bottom",
                align = "hv"
                # font.label = list(size = 10, color = "black", face = "bold", 
                #                   family = NULL, position = "top")
)


### f2 
## gap analysis
dd <- rbind(
  rbind(
    dfPV %>% 
      st_drop_geometry() %>% 
      filter(adopter == 1) %>% 
      group_by(Dist) %>% 
      summarise(mean = mean(neighbor)) %>% 
      mutate(tech = "PV (Seattle)"),
    
    dfEV %>% 
      st_drop_geometry() %>% 
      filter(adopter == 1) %>% 
      group_by(Dist) %>% 
      summarise(mean = mean(neighbor)) %>% 
      mutate(tech = "EV (Seattle)"),
    
    dfPump %>% 
      st_drop_geometry() %>% 
      filter(adopter == 1) %>% 
      group_by(Dist) %>% 
      summarise(mean = mean(neighbor)) %>% 
      mutate(tech = "HP (Seattle)"),
    
    dfVT %>% 
      st_drop_geometry() %>% 
      filter(adopter == 1) %>% 
      group_by(Dist) %>% 
      summarise(mean = mean(neighbor)) %>% 
      mutate(tech = "PV (Vermont)")
  ) %>% 
    mutate(adopter = "Y"),
  rbind(
    dfPV %>% 
      st_drop_geometry() %>% 
      filter(adopter == 0) %>% 
      group_by(Dist) %>% 
      summarise(mean = mean(neighbor)) %>% 
      mutate(tech = "PV (Seattle)"),
    
    dfEV %>% 
      st_drop_geometry() %>% 
      filter(adopter == 0) %>% 
      group_by(Dist) %>% 
      summarise(mean = mean(neighbor)) %>% 
      mutate(tech = "EV (Seattle)"),
    
    dfPump %>% 
      st_drop_geometry() %>% 
      filter(adopter == 0) %>% 
      group_by(Dist) %>% 
      summarise(mean = mean(neighbor)) %>% 
      mutate(tech = "HP (Seattle)"),
    
    dfVT %>% 
      st_drop_geometry() %>% 
      filter(adopter == 0) %>% 
      group_by(Dist) %>% 
      summarise(mean = mean(neighbor)) %>% 
      mutate(tech = "PV (Vermont)")
  ) %>% 
    mutate(adopter = "N")
) %>% 
  filter(Dist < 1100) 


dd %>% 
  mutate(adopter = factor(adopter, levels = c("Y","N")),
         tech = factor(tech, levels = c("PV (Seattle)", "PV (Vermont)", "EV (Seattle)", 
                                        "HP (Seattle)")),
         Dist = Dist/100,
         Dist = as.factor(Dist)) %>% 
  
  ggplot() +
  geom_col(position = "dodge", aes(x = Dist, y = mean, fill = adopter)) +
  scale_fill_manual(values = c("gray40", "gray70")) +
  labs(y = "", fill = "Adopter",
       x = "Distance (100m)", title = "a") +
  facet_wrap(~tech, scale = "free", nrow = 1) +
  theme_classic() + 
  theme(legend.position = "none")


dt <- rbind(
  rbind(
    dfPV %>% 
      st_drop_geometry() %>% 
      filter(adopter == 1) %>% 
      group_by(Year) %>% 
      summarise(mean = mean(neighbor)) %>% 
      mutate(tech = "PV (Seattle)"),
    
    dfEV %>% 
      st_drop_geometry() %>% 
      filter(adopter == 1) %>% 
      group_by(Year) %>% 
      summarise(mean = mean(neighbor)) %>% 
      mutate(tech = "EV (Seattle)"),
    
    dfPump %>% 
      st_drop_geometry() %>% 
      filter(adopter == 1) %>% 
      group_by(Year) %>% 
      summarise(mean = mean(neighbor)) %>% 
      mutate(tech = "HP (Seattle)"),
    
    dfVT %>% 
      st_drop_geometry() %>% 
      filter(Dist < 1100) %>% 
      filter(adopter == 1) %>% 
      group_by(Year) %>% 
      summarise(mean = mean(neighbor)) %>% 
      mutate(tech = "PV (Vermont)")
  ) %>% 
    mutate(adopter = "Y"),
  rbind(
    dfPV %>% 
      st_drop_geometry() %>% 
      filter(adopter == 0) %>% 
      group_by(Year) %>% 
      summarise(mean = mean(neighbor)) %>% 
      mutate(tech = "PV (Seattle)"),
    
    dfEV %>% 
      st_drop_geometry() %>% 
      filter(adopter == 0) %>% 
      group_by(Year) %>% 
      summarise(mean = mean(neighbor)) %>% 
      mutate(tech = "EV (Seattle)"),
    
    dfPump %>% 
      st_drop_geometry() %>% 
      filter(adopter == 0) %>% 
      group_by(Year) %>% 
      summarise(mean = mean(neighbor)) %>% 
      mutate(tech = "HP (Seattle)"),
    
    dfVT %>% 
      st_drop_geometry() %>% 
      filter(Dist < 1100) %>% 
      filter(adopter == 0) %>% 
      group_by(Year) %>% 
      summarise(mean = mean(neighbor)) %>% 
      mutate(tech = "PV (Vermont)")
  ) %>% 
    mutate(adopter = "N")
) 

dt %>% 
  mutate(adopter = factor(adopter, levels = c("Y","N")),
         tech = factor(tech, levels = c("PV (Seattle)", "PV (Vermont)", "EV (Seattle)", 
                                        "HP (Seattle)")),
         Year = as.character(Year),
         Year = substr(Year, 3,4)) %>% 
  ggplot() +
  geom_col(position = "dodge", aes(x = Year, y = mean, fill = adopter)) +
  scale_fill_manual(values = c("gray40", "gray70")) +
  labs(y = "", fill = "Adopter",
       x = "Year", title = "b") +
  facet_wrap(~tech, scale = "free", nrow = 1) +
  theme_classic() + 
  theme(legend.position = "none")


# density 
# Seattle: 182758 units, 217.3 square km
# Vermont: 346301 units, 24923.46 square km

# find the average housing unit number per areas
dst <- c(50,100,150,200,250,300,400,500,700,1000)
s_t <- 182758
v_t <- 346301
ad <- c(3202, 725, 4599, 9740) # total adoption in order of PV,EV,HP, VPV
ss <- s_t/217.3*pi*(dst/1000)^2
vv <- v_t/24923.46*pi*(dst/1000)^2

ad[1:3]/s_t
ad[4]/v_t

dff <- dd %>% 
  pivot_wider(names_from = adopter, values_from = mean) %>% 
  mutate(diff = Y - N) %>% 
  mutate(tech = factor(tech, levels = c("PV (Seattle)", "PV (Vermont)", "EV (Seattle)", 
                                        "HP (Seattle)")),
         Dist = Dist/100,
         Dist = as.factor(Dist)) %>%
  mutate(stand = c(rep(ss, 3), vv),
         adpt = c(rep(ad[1:3]/s_t, each = length(dst)),rep(ad[4]/v_t, 10)),
         s_diff = diff/ stand,
         r_dff = s_diff/ adpt)


dff %>% 
  ggplot() + 
  geom_col(position = "dodge", aes(y = diff, x = Dist), fill = "gray50") +
  # geom_point(aes(y = r_dff, x = Dist), color = "red") +
  geom_line(aes(y = log10(s_diff)/6+0.8, x = Dist, group = tech), color = "gray10", alpha = 0.4)+ 
  geom_point(aes(y = log10(s_diff)/6+0.8, x = Dist)) +
  labs(x = "Distance (100m)",
       title = "c") +
  
  facet_wrap(~ tech, nrow = 1) +
  scale_x_discrete(expand = c(0.1, 0)) +
  scale_y_continuous(name = "Number difference between\nadopter vs. non-adopter (gray bar)",
                     sec.axis = sec_axis(~10^((.-0.8)*6), 
                                         breaks = trans_breaks("log10", function(x) 10^x),
                                         labels = trans_format("log10", math_format(10^.x)),
                                         name = "Number difference per\nunit building (black point)")) +
  theme_classic()


### f3
### comparing technologies 
# regardless of distance
# non standardization
ds <- 1000
for(j in 1:4){
  
  if(j == 1){
    df <- dfPV %>% 
      filter(Dist <ds)
    tc <- "PV (Seattle)"
  }else if(j == 2){
    df <- dfEV %>% 
      filter(Dist <ds)
    tc <- "EV (Seattle)"
  }else if(j == 3){
    df <- dfPump %>% 
      filter(Dist <ds)
    tc <- "HP (Seattle)"
  }else{
    df <- dfVT %>% 
      filter(Dist <ds)
    tc <- "PV (Vermont)"
  }
  
  tf <- df %>% 
    st_drop_geometry()
  
  
  fit <- glmer(adopter ~ neighbor + (1| Year)+
                 (1 |Dist), family = "binomial", data = tf)
  
  sims <- 1000
  pe <- fit %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
  peGlm <- pe[1:2]
  vcGlm <- vcov(fit)
  simbetas <- mvrnorm(sims, peGlm, vcGlm) %>% 
    as.data.frame()
  
  
  nei <- simbetas$neighbor
  
  pe <- mean(nei)
  upper <- quantile(nei, probs= 0.95) 
  lower <- quantile(nei, probs= 0.05) 
  
  
  tp <- cbind(pe,upper,lower) %>% 
    as.data.frame() %>% 
    mutate(tech = tc)
  
  if(j == 1){ #only for EV i should be 100
    TP <- tp 
  }else{
    TP <- rbind(tp,TP)
  }
}


TP %>%
  as.data.frame() %>%
  mutate(tech = factor(tech, levels = c("PV (Seattle)", "PV (Vermont)", 
                                        "EV (Seattle)", "HP (Seattle)"))) %>% 
  
  ggplot(aes(color=tech, y=pe, x=tech,ymin=lower, ymax=upper)) + 
  
  geom_errorbar(color = "gray30", width=0.1, alpha=0.9, size=0.7) +
  geom_point(position = "dodge", size = 3, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(title = "a", y = "Installed base coefficient", x = "", 
       color = "", fill = "") +
  
  # coord_cartesian(ylim = c(0.99, 1.06)) +
  theme_classic() + 
  theme(legend.position = "",
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


# no standardizing 
for(j in 1:4){
  
  if(j == 1){
    df <- dfPV
    tc <- "PV (Seattle)"
  }else if(j == 2){
    df <- dfEV
    tc <- "EV (Seattle)"
  }else if(j == 3){
    df <- dfPump
    tc <- "HP (Seattle)"
  }else{
    df <- dfVT
    tc <- "PV (Vermont)"
  }
  
  for(i in c(50,100,150,200,250,300,400,500,700,1000)){
    tf <- df %>% 
      filter(Dist == i)
  
    
    
    fit <- glmer(adopter ~ neighbor + (1| Year), family = "binomial", data = tf)
    
    sims <- 1000
    pe <- fit %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
    peGlm <- pe[1:2]
    vcGlm <- vcov(fit)
    simbetas <- mvrnorm(sims, peGlm, vcGlm) %>% 
      as.data.frame()
    
    
    nei <- simbetas$neighbor
    
    pe <- mean(nei) %>% exp()
    upper <- quantile(nei, probs= 0.95)%>% exp()
    lower <- quantile(nei, probs= 0.05)%>% exp()
    
    
    tp <- cbind(pe,upper,lower) %>% 
      as.data.frame() %>% 
      mutate(rad = i,
             tech = tc)
    
    if(i == 50 & j == 1){ #only for EV i should be 100
      TP <- tp 
    }else{
      TP <- rbind(tp,TP)
    }
    
  }
  
}


TP %>%
  as.data.frame() %>%
  mutate(tech = factor(tech, 
                       levels = c("PV (Seattle)", "PV (Vermont)", "EV (Seattle)", 
                                  "HP (Seattle)"))) %>% 
  ggplot(aes(x = rad, y = pe)) +
  geom_smooth(aes(color = tech),se=FALSE, linewidth = 0.5) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(xintercept  = 505.4,
             linetype="dotdash", size=0.5) +

  labs(title = "b", y = "Installed base odds ratios", x = "Distance (m)", fill = "", color = "") +
  theme_classic() + 
  theme(legend.position = "none",
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"))

ds <- 501
for(j in 1:4){
  
  if(j == 1){
    df <- dfPV %>% 
      filter(Dist<ds)
    tc <- "PV (Seattle)"
  }else if(j == 2){
    df <- dfEV %>% 
      filter(Dist<ds)
    tc <- "EV (Seattle)"
  }else if(j == 3){
    df <- dfPump %>% 
      filter(Dist<ds)
    tc <- "HP (Seattle)"
  }else{
    df <- dfVT %>% 
      filter(Dist<ds)
    tc <- "PV (Vermont)"
  }
  
  tf <- df %>% 
    st_drop_geometry() %>% 
    mutate(Dist = factor(Dist))
  
  
  fit <- glmer(adopter ~ neighbor + 
                 (1 |Dist), family = "binomial", data = tf)
  
  
  x <- 
    tf %>%                    
    dplyr::select(neighbor, Dist) %>% 
    summarize_all(mean) %>%
    uncount(36) %>% # summarize the means for most variables (can't average Species)         # repeat the row 10 times
    mutate(neighbor = 0:35,
           cont = 1,
           Dist = 1
    ) %>% 
    relocate(cont)
  
  
  x <- x %>% 
    as.matrix()

  
  sims <- 1000
  pe <- fit %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
  peGlm <- pe[1:2]
  vcGlm <- vcov(fit)
  simbetas <- mvrnorm(sims, peGlm, vcGlm) %>% 
    as.data.frame() %>% 
    mutate(Dist = ranef(fit)$Dist[[1]][8]) 

  
  
  xbeta <- x %>% 
    as.matrix()%*% t(simbetas)
  
  inverse.logit <- function(xb){
    1/(1+exp(-xb))
  }
  
  prob <-
    inverse.logit(xbeta)
  pe <- apply(prob, 1, mean)
  upper <- apply(prob, 1, quantile, probs= 0.95)
  lower <- apply(prob, 1, quantile, probs= 0.05)
  
  
  tp <- cbind(pe,upper,lower) %>% 
    as.data.frame() %>% 
    mutate(neighbor = 0:35,
           tech = tc)
  
  if(j == 1){ #only for EV i should be 100
    TP <- tp 
  }else{
    TP <- rbind(tp,TP)
  }
}


TP %>%
  as.data.frame() %>%
  mutate(tech = factor(tech, levels = c("PV (Seattle)", "PV (Vermont)", 
                                        "EV (Seattle)", "HP (Seattle)"))) %>% 
  mutate_at(vars(pe,upper,lower), funs(.*100-50)) %>% 
  
  ggplot(aes(x = neighbor, y = pe, ymax = upper, ymin = lower, colour = tech, fill = tech)) +
  geom_line() +
  geom_ribbon(alpha = 0.1, linetype = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +

  labs(y = "Probability of adoption\nrelative to non-adopters (%)", 
       x = "Number of neighbor installations within 500m",
       fill = "",
       color = "",
       title = "c") +
  theme_classic() +
  theme(legend.position = "none")



# standardization
ds <- 1000
for(j in 1:4){
  
  if(j == 1){
    df <- dfPV %>% 
      filter(Dist <ds)
    tc <- "PV (Seattle)"
  }else if(j == 2){
    df <- dfEV %>% 
      filter(Dist <ds)
    tc <- "EV (Seattle)"
  }else if(j == 3){
    df <- dfPump %>% 
      filter(Dist <ds)
    tc <- "HP (Seattle)"
  }else{
    df <- dfVT %>% 
      filter(Dist <ds)
    tc <- "PV (Vermont)"
  }
  
  tf <- df 
  
  if(sum(tf$neighbor) < 2){ # if the number of neighbor is less than 2, then the estimation is spurious.
    next
    
  }else{

    tf <- tf %>%
    mutate_at(vars(neighbor), funs(scale(.) %>% as.numeric()))
  }
  
  
  fit <- glmer(adopter ~ neighbor + (1| Year)+
                 (1 |Dist), family = "binomial", data = tf)
  
  
  sims <- 1000
  pe <- fit %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
  peGlm <- pe[1:2]
  vcGlm <- vcov(fit)
  simbetas <- mvrnorm(sims, peGlm, vcGlm) %>% 
    as.data.frame()
  
  
  nei <- simbetas$neighbor
  
  pe <- mean(nei)
  upper <- quantile(nei, probs= 0.95) 
  lower <- quantile(nei, probs= 0.05) 
  
  
  tp <- cbind(pe,upper,lower) %>% 
    as.data.frame() %>% 
    mutate(tech = tc)
  
  if(j == 1){ #only for EV i should be 100
    TP <- tp 
  }else{
    TP <- rbind(tp,TP)
  }
}


TP %>%
  as.data.frame() %>%
  mutate(tech = factor(tech, levels = c("PV (Seattle)", "PV (Vermont)", 
                                        "EV (Seattle)", "HP (Seattle)"))) %>% 
  
  
  ggplot(aes(color=tech, y=pe, x=tech,ymin=lower, ymax=upper)) + 
  
  geom_errorbar(color = "gray30", width=0.1, alpha=0.9, size=0.7) +
  geom_point(position = "dodge", size = 3, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(title = "d", y = "Installed base coefficient (standardized)", x = "", 
       color = "", fill = "") +
  
  theme_classic() + 
  theme(legend.position = "none",
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# standardizing each loop of tech and distance
for(j in 1:4){
  
  if(j == 1){
    df <- dfPV
    tc <- "PV (Seattle)"
  }else if(j == 2){
    df <- dfEV
    tc <- "EV (Seattle)"
  }else if(j == 3){
    df <- dfPump
    tc <- "HP (Seattle)"
  }else{
    df <- dfVT
    tc <- "PV (Vermont)"
  }
  
  for(i in c(50,100,150,200,250,300,400,500,700,1000)){
    tf <- df %>% 
      filter(Dist == i)
    
    if(sum(tf$neighbor) < 2){ # if the number of neighbor is less than 2, then the estimation is spurious. 
      next
      
    }else{
      tf <- tf %>% 
        # mutate_at(vars(neighbor), funs(rescale(., to=c(0,1)))) 
        mutate_at(vars(neighbor), funs(scale(.) %>% as.numeric()))
    }
  
    
    fit <- glmer(adopter ~ neighbor + (1| Year), family = "binomial", data = tf)
    
    
    sims <- 1000
    pe <- fit %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
    peGlm <- pe[1:2]
    vcGlm <- vcov(fit)
    simbetas <- mvrnorm(sims, peGlm, vcGlm) %>% 
      as.data.frame()
    
    
    nei <- simbetas$neighbor
    
    pe <- mean(nei)%>% exp()
    upper <- quantile(nei, probs= 0.95) %>% exp()
    lower <- quantile(nei, probs= 0.05) %>% exp()
    
    
    tp <- cbind(pe,upper,lower) %>% 
      as.data.frame() %>% 
      mutate(rad = i,
             tech = tc)
    
    if(i == 50 & j == 1){ #only for EV i should be 100
      TP <- tp 
    }else{
      TP <- rbind(tp,TP)
    }
    
  }
  
}


TP %>%
  as.data.frame() %>%
  mutate(tech = factor(tech, 
                       levels = c("PV (Seattle)", "PV (Vermont)", "EV (Seattle)", 
                                  "HP (Seattle)"))) %>% 
  ggplot(aes(x = rad, y = pe)) +
  geom_smooth(aes(color = tech),se=FALSE, alpha = 0.1, linewidth = 0.5) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(xintercept  = c(505.4),
             linetype="dotdash", size=0.5) +

  labs(title = "e", y = "Installed base odd ratios (standardized)", x = "Distance (m)", fill = "", color = "") +
  theme_classic() + 
  theme(legend.position = "none",
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"))


ds <- 1000
for(j in 1:4){
  
  if(j == 1){
    df <- dfPV %>% 
      filter(Dist<ds) 
    tc <- "PV (Seattle)"
  }else if(j == 2){
    df <- dfEV %>% 
      filter(Dist<ds)
    tc <- "EV (Seattle)"
  }else if(j == 3){
    df <- dfPump %>% 
      filter(Dist<ds)
    tc <- "HP (Seattle)"
  }else{
    df <- dfVT %>% 
      filter(Dist<ds)
    tc <- "PV (Vermont)"
  }
  
  tf <- df %>% 
    st_drop_geometry()%>% 
    mutate(Dist = factor(Dist))
  
  if(sum(tf$neighbor) < 2){ # if the number of neighbor is less than 2, then the estimation is spurious.
    next
    
  }else{
    tf <- tf %>%
      mutate_at(vars(neighbor), funs(scale(.) %>% as.numeric()))
  }
  
  fit <- glm(adopter ~ neighbor, family = "binomial", data = tf)
  
  x <- 
    tf %>%                    
    dplyr::select(neighbor) %>% 
    summarize_all(mean) %>%
    uncount(21) %>% # summarize the means for most variables (can't average Species)         # repeat the row 10 times
    mutate(neighbor = seq(-1,1,by=0.1),
           cont = 1
    ) %>% 
    relocate(cont)
  
  
  x <- x %>% 
    as.matrix()
  
  
  sims <- 1000
  pe <- fit %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
  peGlm <- pe[1:2]
  vcGlm <- vcov(fit)
  simbetas <- mvrnorm(sims, peGlm, vcGlm) %>% 
    as.data.frame()
  
  
  xbeta <- x %*% t(simbetas)
  
  inverse.logit <- function(xb){
    1/(1+exp(-xb))
  }
  
  prob <-
    inverse.logit(xbeta)
  
  
  pe <- apply(prob, 1, mean)
  upper <- apply(prob, 1, quantile, probs= 0.95)
  lower <- apply(prob, 1, quantile, probs= 0.05)
  
  
  tp <- cbind(pe,upper,lower) %>% 
    as.data.frame() %>% 
    mutate(neighbor = seq(-1,1,by=0.1),
           tech = tc)
  
  if(j == 1){ #only for EV i should be 100
    TP <- tp 
  }else{
    TP <- rbind(tp,TP)
  }
}


TP %>%
  as.data.frame() %>%
  mutate(tech = factor(tech, levels = c("PV (Seattle)", "PV (Vermont)", 
                                        "EV (Seattle)", "HP (Seattle)"))) %>% 
  mutate_at(vars(pe,upper,lower), funs(.*100-50)) %>%
  # mutate_at(vars(pe,upper,lower), funs(.*100)) %>% 
  
  ggplot(aes(x = neighbor, y = pe, ymax = upper, ymin = lower, colour = tech, fill = tech)) +
  geom_line() +
  geom_ribbon(alpha = 0.05, linetype = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Probability of adoption\nrelative to non-adopters (%)", 
       x = "Standard deviation of neighbor installations",
       fill = "",
       color = "",
       title = "F") +
  theme_classic() +
  theme(legend.position = "none")




### f4
for(j in 1:4){

  if(j == 1){
    df <- dfPV 
    tc <- "PV (Seattle)"
  }else if(j == 2){
    df <- dfEV
    tc <- "EV (Seattle)"
  }else if(j == 3){
    df <- dfPump
    tc <- "HP (Seattle)"
  }else{
    df <- dfVT %>%
      filter(Dist < 1200)
    tc <- "PV (Vermont)"
  }

  for(i in 2013:2019){

    tf <- df %>%
      filter(Year == i)

    if(sum(tf$neighbor) < 2){ # if the number of neighbor is less than 2, then the estimation is spurious.
      next

    }else{
      tf <- tf %>%
        mutate_at(vars(neighbor), funs(scale(.) %>% as.numeric()))
    }
    
    
    fit <- glmer(adopter ~ neighbor + (1| Dist), family = "binomial", data = tf)
    
    sims <- 1000
    pe <- fit %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
    peGlm <- pe[1:2]
    vcGlm <- vcov(fit)
    simbetas <- mvrnorm(sims, peGlm, vcGlm) %>% 
      as.data.frame()


    nei <- simbetas$neighbor

    pe <- mean(nei)
    upper <- quantile(nei, probs= 0.95) 
    lower <- quantile(nei, probs= 0.05) 


    tp <- cbind(pe,upper,lower) %>%
      as.data.frame() %>%
      mutate(year = i,
             tech = tc)

    if(j == 1 & i == 2013){ #only for EV i should be 100
      TP <- tp
    }else{
      TP <- rbind(tp,TP)
    }


  }

}


# non-normalization
for(j in 1:4){
  
  if(j == 1){
    df <- dfPV 
    tc <- "PV (Seattle)"
  }else if(j == 2){
    df <- dfEV
    tc <- "EV (Seattle)"
  }else if(j == 3){
    df <- dfPump
    tc <- "HP (Seattle)"
  }else{
    df <- dfVT %>%
      filter(Dist < 1200)
    tc <- "PV (Vermont)"
  }
  
  for(i in 2013:2019){
    
    tf <- df %>%
      filter(Year == i)

    
    fit <- glmer(adopter ~ neighbor + (1| Dist), family = "binomial", data = tf)
    
    sims <- 1000
    pe <- fit %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
    peGlm <- pe[1:2]
    vcGlm <- vcov(fit)
    simbetas <- mvrnorm(sims, peGlm, vcGlm) %>% 
      as.data.frame()
    
    nei <- simbetas$neighbor
    
    pe <- mean(nei)
    upper <- quantile(nei, probs= 0.95) 
    lower <- quantile(nei, probs= 0.05) 
    
    
    tp <- cbind(pe,upper,lower) %>%
      as.data.frame() %>%
      mutate(year = i,
             tech = tc)
    
    if(j == 1 & i == 2013){ #only for EV i should be 100
      TPn <- tp
    }else{
      TPn <- rbind(tp,TPn)
    }
    
    
  }
  
}

To <- TP %>%
  mutate(norm = "Standardized") %>% 
  rbind(TPn %>% 
          mutate(norm = "Original")) %>% 
  as.data.frame() %>%
  mutate(tech = factor(tech,
                       levels = c("PV (Seattle)", "PV (Vermont)", "EV (Seattle)",
                                  "HP (Seattle)")),
         year = as.character(year),
         year = substr(year, 3,4)) 


To %>% 
  mutate(norm = factor(norm, levels = c("Standardized","Original"))) %>% 
  
  ggplot() +
  geom_point(aes(x = year, y = pe), size = 2) +
  geom_errorbar(aes(x = year, ymin=lower, ymax=upper), color = "gray50", width=0.1, alpha=0.9, size=0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +

  geom_hline(data = To %>% filter(tech == "PV (Seattle)") %>% 
               filter(norm == "Original"), aes(yintercept  = mean(pe)),
             linetype="solid", color = "red", size=0.3, alpha = 0.4) +
  geom_hline(data = To %>% filter(tech == "PV (Vermont)")%>% 
               filter(norm == "Original"), aes(yintercept  = mean(pe)),
             linetype="solid", color = "red", size=0.3, alpha = 0.4) +
  geom_hline(data = To %>% filter(tech == "EV (Seattle)")%>% 
               filter(norm == "Original"), aes(yintercept  = mean(pe)),
             linetype="solid", color = "red", size=0.3, alpha = 0.4) +
  geom_hline(data = To %>% filter(tech == "HP (Seattle)")%>% 
               filter(norm == "Original"), aes(yintercept  = mean(pe)),
             linetype="solid", color = "red", size=0.3, alpha = 0.4) +
  
  geom_hline(data = To %>% filter(tech == "PV (Seattle)") %>% 
               filter(norm == "Standardized"), aes(yintercept  = mean(pe)),
             linetype="solid", color = "red", size=0.3, alpha = 0.4) +
  geom_hline(data = To %>% filter(tech == "PV (Vermont)")%>% 
               filter(norm == "Standardized"), aes(yintercept  = mean(pe)),
             linetype="solid", color = "red", size=0.3, alpha = 0.4) +
  geom_hline(data = To %>% filter(tech == "EV (Seattle)")%>% 
               filter(norm == "Standardized"), aes(yintercept  = mean(pe)),
             linetype="solid", color = "red", size=0.3, alpha = 0.4) +
  geom_hline(data = To %>% filter(tech == "HP (Seattle)")%>% 
               filter(norm == "Standardized"), aes(yintercept  = mean(pe)),
             linetype="solid", color = "red", size=0.3, alpha = 0.4) +

  labs(title = "c", y = "Installed base coefficient", x = "Year", fill = "", color = "") +
  facet_grid(norm~tech, scale = "free", switch = "y") +
  scale_x_discrete(expand = c(0.2, 0)) +
  theme_classic() 



### bass model 
data.pv <- PV %>% 
  group_by(Year) %>% 
  summarise(value = n())

data.ev <- EV %>% 
  group_by(Year) %>% 
  summarise(value = n())

data.pump <- Pump %>% 
  group_by(Year) %>% 
  summarise(value = n())

data.VT <- VPV %>% 
  group_by(Year) %>% 
  summarise(value = n())


# data: year, install; nqtrs: number of times
dff <- function(data, nqtrs){
  Sales <- data$value
  T <- 1:length(Sales)
  
  fit<-nlsLM(Sales ~ M*(((P+Q)^2/P)*exp(-(P+Q)*T))/
               (1+(Q/P)*exp(-(P+Q)*T))^2,
             # add P and Q below
             start=c(list(M=sum(Sales),P=0.08,Q=0.2)))
  
  r <- summary(fit)$coefficient[,1] 
  
  m <- r[[1]]
  p <- r[[2]]
  q <- r[[3]]
  
  t=seq(0,nqtrs)
  
  
  FF = expression(p*(exp((p+q)*t)-1)/(p*exp((p+q)*t)+q))
  ff = D(FF,"t")
  
  new <- eval(ff)*m
  return(new)
}


data.VT %>% 
  mutate(bass = dff(data.VT, 20),
         tech = "PV (Vermont)") %>% 
  rbind(data.pv %>% 
          mutate(bass = dff(data.pv, 16),
                 tech = "PV (Seattle)"),
        data.ev %>% 
          mutate(bass = dff(data.ev, 9),
                 tech = "EV (Seattle)"),
        data.pump %>% 
          mutate(bass = dff(data.pump, 16),
                 tech = "HP (Seattle)")) %>% 
  rename(Bass = bass,
         Adoption = value) %>% 
  filter(Year > 2007) %>% 
  # mutate(Year = substr(Year, 3,4)) %>% 
  mutate(Year = as.character(Year),
         Year = substr(Year, 3,4)) %>%
  rename(Installation = Adoption) %>% 

  gather(key, value, Installation:Bass) %>% 
  mutate(tech = factor(tech, levels = c("PV (Seattle)", "PV (Vermont)", "EV (Seattle)", 
                                        "HP (Seattle)")),
         key = factor(key, levels = c("Installation","Bass"))) %>%
  
  ggplot(aes(x = Year, y = value,lty = key, color = key, group = key)) +
  geom_line() +
  scale_color_manual(values = c("black","red")) +
  
  scale_linetype_manual(values = c(1,5)) +

  facet_wrap(~tech, scale = "free", nrow = 1) +
  labs(y = "Annual installation", x = "Year", color = "", lty = "",
       title = "a") +
  theme_classic() +
  theme(
    axis.line.x = element_line(color="black"),
    axis.line.y = element_line(color="black"),
    legend.position = c(0.95,0.3))



# giving m,p, and q
diff <- function(data){

  
  Sales <- data$value
  T <- 1:length(Sales)
  
  fit<-nlsLM(Sales ~ M*(((P+Q)^2/P)*exp(-(P+Q)*T))/
               (1+(Q/P)*exp(-(P+Q)*T))^2,
             start=c(list(M=sum(Sales),P=0.08,Q=0.2)))
  
  sims <- 1000
  peGlm <- summary(fit)$coefficient[,1] 
  vcGlm <- vcov(fit)
  simbetas <- mvrnorm(sims, peGlm, vcGlm) %>% 
    as.data.frame()
  
  var = c("M","P","Q")
  for(i in 1:3){
    
    nei <- simbetas[[i]]
    
    
    pe <- mean(nei)
    upper <- quantile(nei, probs= 0.95) 
    lower <- quantile(nei, probs= 0.05) 
    
    tp <- cbind(pe,upper,lower) %>% 
      as.data.frame() %>% 
      mutate(var = var[i])
    
    if(i == 1){
      TP <- tp 
    }else{
      TP <- rbind(tp,TP)
    }
    
  }
  
  return(TP)
}

# m,p,q for the total adoption
rbind(diff(data.pv) %>% 
                 mutate(tech = "PV (Seattle)"),
               diff(data.ev) %>% 
                 mutate(tech = "EV (Seattle)"),
               diff(data.pump) %>% 
                 mutate(tech = "HP (Seattle)"),
               diff(data.VT) %>% 
                 mutate(tech = "PV (Vermont)")) %>% 
  as.data.frame() %>% 
  mutate(var = ifelse(var == "M", "Market potential (m)", 
                      ifelse(var == "P", "Innovation (p)","Imitation (q)")),
         var = factor(var, levels = c("Market potential (m)","Innovation (p)",
                                      "Imitation (q)"))) %>% 
  mutate(tech = factor(tech, levels = c("PV (Seattle)","PV (Vermont)", 
                                        "EV (Seattle)","HP (Seattle)"))) %>% 
  filter(var == "Imitation (q)") %>% 
  
  ggplot(aes(color=tech, y=pe, x=tech,ymin=lower, ymax=upper)) + 
  
  geom_errorbar(color = "gray30", width=0.1, alpha=0.9, size=0.7) +
  geom_point(position = "dodge", size = 3, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  
  
  labs(x = "", y = "Bass imitation (q) coefficient",color = "",title = "b") +
  theme_classic() +
  theme(axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        legend.position = "bottom",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

  
### f5
### comparing community
# total count by tech
tech_num <- dfPV %>% 
  filter(adopter == 1) %>% 
  filter(Dist == 50) %>% 
  dplyr::select(GEOID, Unit) %>% 
  mutate(tech = "PV (Seattle)") %>% 
  rbind(
    dfEV %>% 
      filter(adopter == 1) %>% 
      filter(Dist == 50) %>% 
      dplyr::select(GEOID, Unit) %>% 
      mutate(tech = "EV (Seattle)")
  ) %>% 
  rbind(
    dfPump %>% 
      filter(adopter == 1) %>% 
      filter(Dist == 50) %>% 
      dplyr::select(GEOID, Unit) %>% 
      mutate(tech = "HP (Seattle)")
  ) %>% 
  st_drop_geometry() %>% 
  rbind(
    dfVT %>% 
      filter(adopter == 1) %>% 
      filter(Dist == 50) %>% 
      dplyr::select(GEOID, Unit) %>% 
      mutate(tech = "PV (Vermont)") %>% 
      st_drop_geometry()) %>% 
  group_by(GEOID, tech) %>% 
  summarise(count = n(),
            Unit = mean(Unit),
            rate = count/Unit) %>% 
  pivot_wider(names_from = tech, values_from = rate) %>% 
  dplyr::select(-count, -Unit) %>% 
  group_by(GEOID) %>% 
  summarize_all(sum, na.rm = T) 




tab_gen <- function(variable){
  lv <- c("L","M","H")
  num <- 3
  
  
  SEA <- seattle_tracts %>%
    mutate_at(vars(HomeOwn,Income,Edu), funs(rescale(., to=c(0,1)))) %>% 
    mutate(ic = ifelse(get(variable) > quantile(get(variable), 0.8) , "H",
                       ifelse(get(variable) > quantile(get(variable), 0.2), "M","L")),
           ic = factor(ic, levels = lv)) 
  
  VT <- temp %>%
    mutate_at(vars(HomeOwn,Income,Edu), funs(rescale(., to=c(0,1)))) %>% 
    mutate(ic = ifelse(get(variable) > quantile(get(variable), 0.8) , "H",
                       ifelse(get(variable) > quantile(get(variable), 0.2), "M","L")),
           ic = factor(ic, levels = lv)) 
  
  
  fsv <- VT %>% 
    st_drop_geometry() %>% 
    left_join(tech_num %>% 
                dplyr::select(GEOID,`PV (Vermont)`), by = "GEOID") %>% 
    mutate_at(7, ~replace_na(.,0)) %>% 
    
    group_by(ic) %>% 
    summarise(mean = mean(`PV (Vermont)`),
              sd = sd(`PV (Vermont)`)) %>%
    mutate(tech = "PV (Vermont)") %>% 
    rbind(SEA %>% 
            st_drop_geometry() %>% 
            left_join(tech_num %>% 
                        dplyr::select(-`PV (Vermont)`), by = "GEOID") %>% 
            mutate_at(14:16, ~replace_na(.,0)) %>% 
            gather(tech, value, `EV (Seattle)`:`PV (Seattle)`) %>% 
            
            group_by(tech, ic) %>% 
            summarise(mean = mean(value),
                      sd = sd(value)) %>% 
            dplyr::select(ic, mean, sd, tech)) %>% 
    mutate(tech = factor(tech, levels = c("PV (Seattle)","PV (Vermont)",
                                          "EV (Seattle)","HP (Seattle)")),
           var = variable)
  
  
  # box plot 
  ### income and built environment 
  
  tc <- c("PV (Seattle)", "EV (Seattle)", "HP (Seattle)", "PV (Vermont)")
  
  for(j in 1:4){
    
    if(j == 1){
      df <- dfPV %>% 
        filter(Dist < 300) %>% 
        left_join(SEA %>% 
                    st_drop_geometry() %>% 
                    dplyr::select(GEOID, ic), by = "GEOID")
    }else if(j == 2){
      df <- dfEV %>% 
        filter(Dist < 300) %>% 
        left_join(SEA %>% 
                    st_drop_geometry() %>% 
                    dplyr::select(GEOID, ic), by = "GEOID")
    }else if(j == 3){
      df <- dfPump %>% 
        filter(Dist < 300) %>% 
        left_join(SEA %>% 
                    st_drop_geometry() %>% 
                    dplyr::select(GEOID, ic), by = "GEOID")
    }else{
      df <- dfVT %>% 
        filter(Dist < 300) %>% 
        left_join(VT %>% 
                    st_drop_geometry() %>% 
                    dplyr::select(GEOID, ic), by = "GEOID")
    }
    
    
    for(k in 1:num){
      
      tf <- df %>% 
        filter(ic == lv[k])
      
      if(sum(tf$neighbor) < 2){ # if the number of neighbor is less than 2, then the estimation is spurious.
        next
        
      }else{
        tf <- tf %>%
          mutate_at(vars(neighbor), funs(scale(.) %>% as.numeric()))
      }
      
      fit <- glm(adopter ~ neighbor + factor(Year) + factor(Dist), family = "binomial", data = tf)
      # summary(fit)
      
      sims <- 1000
      peGlm <- fit$coefficients
      vcGlm <- vcov(fit)
      simbetas <- mvrnorm(sims, peGlm, vcGlm) %>% 
        as.data.frame()
      
      
      nei <- simbetas$neighbor
      
      pe <- mean(nei)
      upper <- quantile(nei, probs= 0.95) 
      lower <- quantile(nei, probs= 0.05)
      
      
      tp <- cbind(pe,upper,lower) %>% 
        as.data.frame() %>% 
        mutate(cluster = lv[k],
               tech = tc[j],
               var = variable)
      
      if(j == 1 & k == 1){ 
        TP <- tp 
      }else{
        TP <- rbind(tp,TP)
      }
      
    }
    
  }
  
  return(list(TP, fsv))
}


tab_d <- function(){
  lv <- c(TRUE,FALSE)
  num <- 2
  
  seattle_tracts <- seattle_tracts %>% 
    st_join(tr.sf %>% 
              rename(GEOID = FIPS) %>% 
              filter(STATE_FIPS == 53) %>%
              dplyr::select(GEOID) %>% 
              left_join(DAC %>% 
                          rename(DAC = disadvantaged) %>% 
                          dplyr::select(GEOID, DAC), by = "GEOID") %>% 
              st_transform(st_crs(seattle_tracts)$epsg) %>% 
              dplyr::select(-GEOID),join = st_intersects, largest = TRUE) %>% 
    mutate(DAC = factor(DAC, levels = c(TRUE, FALSE))) 
  
  
  temp <- temp %>% 
    st_join(tr.sf %>% 
              rename(GEOID = FIPS) %>% 
              filter(STATE_FIPS == 50) %>%
              dplyr::select(GEOID) %>% 
              left_join(DAC %>% 
                          rename(DAC = disadvantaged) %>% 
                          dplyr::select(GEOID, DAC), by = "GEOID") %>% 
              st_transform(st_crs(temp)$epsg) %>% 
              dplyr::select(-GEOID),join = st_intersects, largest = TRUE) %>% 
    mutate(DAC = factor(DAC, levels = c(TRUE, FALSE))) 
  
  
  fsv <- temp %>% 
    st_drop_geometry() %>% 
    left_join(tech_num %>% 
                dplyr::select(GEOID,`PV (Vermont)`), by = "GEOID") %>% 
    mutate_at(7, ~replace_na(.,0)) %>% 
    
    group_by(DAC) %>% 
    summarise(mean = mean(`PV (Vermont)`),
              sd = sd(`PV (Vermont)`)) %>%
    mutate(tech = "PV (Vermont)") %>% 
    rbind(seattle_tracts %>% 
            st_drop_geometry() %>% 
            left_join(tech_num %>% 
                        dplyr::select(-`PV (Vermont)`), by = "GEOID") %>% 
            mutate_at(14:16, ~replace_na(.,0)) %>% 
            gather(tech, value, `EV (Seattle)`:`PV (Seattle)`) %>% 
            
            group_by(tech, DAC) %>% 
            summarise(mean = mean(value),
                      sd = sd(value)) %>% 
            dplyr::select(DAC, mean, sd, tech)) %>% 
    mutate(tech = factor(tech, levels = c("PV (Seattle)","PV (Vermont)",
                                          "EV (Seattle)","HP (Seattle)")),
           var = "DAC")

  
  tc <- c("PV (Seattle)", "EV (Seattle)", "HP (Seattle)", "PV (Vermont)")
  
  for(j in 1:4){
    
    if(j == 1){
      df <- dfPV %>% 
        filter(Dist < 400) %>% 
        left_join(seattle_tracts %>% 
                    st_drop_geometry() %>% 
                    dplyr::select(GEOID, DAC), by = "GEOID")
    }else if(j == 2){
      df <- dfEV %>% 
        filter(Dist < 400) %>% 
        left_join(seattle_tracts %>% 
                    st_drop_geometry() %>% 
                    dplyr::select(GEOID, DAC), by = "GEOID")
    }else if(j == 3){
      df <- dfPump %>% 
        filter(Dist < 400) %>% 
        left_join(seattle_tracts %>% 
                    st_drop_geometry() %>% 
                    dplyr::select(GEOID, DAC), by = "GEOID")
    }else{
      df <- dfVT %>% 
        filter(Dist < 400) %>% 
        left_join(temp %>% 
                    st_drop_geometry() %>% 
                    dplyr::select(GEOID, DAC), by = "GEOID")
    }
    
    
    for(k in 1:num){
      
      tf <- df %>% 
        filter(DAC == lv[k])
      
      if(sum(tf$neighbor) < 2){ # if the number of neighbor is less than 2, then the estimation is spurious.
        next
        
      }else{
        tf <- tf %>%
          mutate_at(vars(neighbor), funs(scale(.) %>% as.numeric()))
      }
      
      fit <- glm(adopter ~ neighbor + factor(Year) + factor(Dist), family = "binomial", data = tf)
      # summary(fit)
      
      sims <- 1000
      peGlm <- fit$coefficients
      vcGlm <- vcov(fit)
      simbetas <- mvrnorm(sims, peGlm, vcGlm) %>% 
        as.data.frame()
      
      
      nei <- simbetas$neighbor
      
      pe <- mean(nei)
      upper <- quantile(nei, probs= 0.95) 
      lower <- quantile(nei, probs= 0.05)
      
      
      tp <- cbind(pe,upper,lower) %>% 
        as.data.frame() %>% 
        mutate(cluster = lv[k],
               tech = tc[j],
               var = "DAC")
      
      if(j == 1 & k == 1){ 
        TP <- tp 
      }else{
        TP <- rbind(tp,TP)
      }
      
    }
    
  }
  
  return(list(TP, fsv))
}


TP <- rbind(tab_gen("Income")[[1]],
            tab_gen("Edu")[[1]],
            tab_gen("HomeOwn")[[1]],
            tab_d()[[1]])

TP1 <- rbind(tab_gen("Income")[[2]],
             tab_gen("Edu")[[2]],
             tab_gen("HomeOwn")[[2]],
             tab_d()[[2]] %>% 
               rename(ic = "DAC"))



TP2 <- TP %>% 
  left_join(TP1 %>% 
              rename(cluster = ic) %>% 
              mutate(mean = mean * 100) %>% 
              dplyr::select(-sd), by = c("cluster","tech","var")) %>% 
  mutate(cluster = factor(cluster, levels = c("L","M","H",TRUE,FALSE)),
         tech = factor(tech, levels = c("PV (Seattle)", "PV (Vermont)", 
                                        "EV (Seattle)", "HP (Seattle)")),
         var = factor(var, levels = c("HomeOwn","Income","Edu", "DAC")))



TP2 %>% 
  filter(tech == "PV (Seattle)") %>% 
  ggplot() + 
  geom_col(aes(y = mean/5, x = cluster, fill = cluster), position = "dodge", alpha = 0.5) +
  geom_point(aes(y = pe, x = cluster), size = 1.5) +
  geom_errorbar(aes(x=cluster, ymin=lower, ymax=upper), color = "gray30", width=0.1, alpha=0.9, size=0.7) +
  
  
  labs(x = "", fill = "") +
  
  scale_fill_viridis_d(direction = -1) +
  facet_grid(tech ~ var, scales = "free", space = "free", switch = "y") +
  scale_x_discrete(expand = c(0.7, 0)) +
  scale_y_continuous(name = "Coefficient (installed base)",
                     sec.axis = sec_axis(~.*5, name = "Average number of installed housing buildings")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        axis.text.x=element_blank())

TP2 %>% 
  filter(tech == "PV (Vermont)") %>% 
  ggplot() + 
  geom_col(aes(y = mean/40, x = cluster, fill = cluster), position = "dodge", alpha = 0.5) +
  geom_point(aes(y = pe, x = cluster), size = 1.5) +
  geom_errorbar(aes(x=cluster, ymin=lower, ymax=upper), color = "gray30", width=0.1, alpha=0.9, size=0.7) +
  
  labs(x = "", fill = "") +
  
  scale_fill_viridis_d(direction = -1) +
  facet_grid(tech ~ var, scales = "free", space = "free", switch = "y") +
  scale_x_discrete(expand = c(0.7, 0)) +
  scale_y_continuous(name = "Coefficient (installed base)",
                     sec.axis = sec_axis(~.*40, name = "Average number of installed housing buildings")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        axis.text.x=element_blank(),
        strip.text.x = element_blank())

TP2 %>% 
  filter(tech == "EV (Seattle)") %>% 
  ggplot() + 
  geom_col(aes(y = mean*0.7, x = cluster, fill = cluster), position = "dodge", alpha = 0.5) +
  geom_point(aes(y = pe, x = cluster), size = 1.5) +
  geom_errorbar(aes(x=cluster, ymin=lower, ymax=upper), color = "gray30", width=0.1, alpha=0.9, size=0.7) +
  
  labs(x = "", fill = "") +
  
  scale_fill_viridis_d(direction = -1) +
  facet_grid(tech ~ var, scales = "free", space = "free", switch = "y") +
  scale_x_discrete(expand = c(0.7, 0)) +
  scale_y_continuous(name = "Coefficient (installed base)",
                     sec.axis = sec_axis(~./0.7, name = "Average number of installed housing buildings")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        axis.text.x=element_blank(),
        strip.text.x = element_blank())

TP2 %>% 
  filter(tech == "HP (Seattle)") %>% 
  ggplot() + 
  geom_col(aes(y = mean/10, x = cluster, fill = cluster), position = "dodge", alpha = 0.5) +
  geom_point(aes(y = pe, x = cluster), size = 1.5) +
  geom_errorbar(aes(x=cluster, ymin=lower, ymax=upper), color = "gray30", width=0.1, alpha=0.9, size=0.7) +
  
  labs(x = "", fill = "") +
  
  scale_fill_viridis_d(direction = -1) +
  facet_grid(tech ~ var, scales = "free", space = "free", switch = "y") +
  scale_x_discrete(expand = c(0.7, 0)) +
  scale_y_continuous(name = "Coefficient (installed base)",
                     sec.axis = sec_axis(~.*10, name = "Average number of installed housing buildings")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        axis.text.x=element_blank(),
        strip.text.x = element_blank())



# DAC characteristics 
seattle_tracts %>% 
  st_join(tr.sf %>% 
            rename(GEOID = FIPS) %>% 
            filter(STATE_FIPS == 53) %>%
            dplyr::select(GEOID) %>% 
            left_join(DAC %>% 
                        rename(DAC = disadvantaged) %>% 
                        dplyr::select(GEOID, DAC), by = "GEOID") %>% 
            st_transform(st_crs(seattle_tracts)$epsg) %>% 
            dplyr::select(-GEOID),join = st_intersects, largest = TRUE) %>% 
  mutate(DAC = factor(DAC, levels = c(TRUE, FALSE))) %>% 
  st_drop_geometry() %>% 
  mutate_at(vars(HomeOwn,Income,Edu), funs(scale(.) %>% as.numeric())) %>% 
  gather(key, value, HomeOwn,Income,Edu) %>% 
  group_by(DAC, key) %>% 
  summarise(mean = mean(value),
            sd = sd(value)) %>% 
  mutate(area = "Seattle") %>% 
  
  rbind(
    temp %>% 
      st_join(tr.sf %>% 
                rename(GEOID = FIPS) %>% 
                filter(STATE_FIPS == 50) %>%
                dplyr::select(GEOID) %>% 
                left_join(DAC %>% 
                            rename(DAC = disadvantaged) %>% 
                            dplyr::select(GEOID, DAC), by = "GEOID") %>% 
                st_transform(st_crs(temp)$epsg) %>% 
                dplyr::select(-GEOID),join = st_intersects, largest = TRUE) %>% 
      mutate(DAC = factor(DAC, levels = c(TRUE, FALSE))) %>% 
      st_drop_geometry() %>% 
      mutate_at(vars(HomeOwn,Income,Edu), funs(scale(.) %>% as.numeric())) %>% 
      gather(key, value, HomeOwn,Income,Edu) %>% 
      group_by(DAC, key) %>% 
      summarise(mean = mean(value),
                sd = sd(value)) %>% 
      mutate(area = "Vermont") 
  ) %>% 
  mutate(key = factor(key, levels = c("HomeOwn","Income","Edu"))) %>% 
  ggplot() +

  geom_point(aes(y = mean, x = DAC, color = DAC), position = "dodge", size = 3, alpha = 0.5) +
  geom_errorbar(aes(x=DAC, ymin=mean-1.96*sd, ymax=mean+1.96*sd), 
                color = "gray30", width=0.1, alpha=0.9, size=0.7) +
  
  labs(x = "", fill = "", y = "Standardized value\n",
       title = "b") +
  

  scale_color_manual(values = c(viridis(5)[2],  viridis(5)[1])) +
  facet_grid(area~key, scales = "free", switch = "y") +
  scale_x_discrete(expand = c(0.8, 0)) +

  theme_minimal() +
  theme(plot.title = element_text(hjust = -0.06),
        legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        axis.text.x=element_blank())



### f6
### detailed mapping
tr <- 53033010401

s_house %>% 
  ggplot() +
  geom_sf(fill = "gray50", color = NA) + # buildings
  geom_sf(data = seattle_tracts %>% # tract layers
            filter(GEOID == tr), fill = NA, color = "blue") +
  
  labs(fill = "", title = "a") +
  theme_minimal() +
  annotation_scale() +
  
  theme(legend.position = c(0.9,0.3),
        legend.key.size = unit(0.25, 'cm'),
        legend.text=element_text(size=6.5),
        plot.margin = unit(c(0,1,0,0), "cm"),        
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank())


t_d <- dfEV %>% # buffers
  st_transform(4269) %>% 
  filter(adopter == 1) %>% 
  st_centroid() %>% 
  st_join(s_house %>% 
            st_join(dfEV %>% # buildings
                      st_transform(4269) %>% 
                      filter(adopter == 1) %>%
                      filter(GEOID == tr),  join = st_intersects) %>% 
            na.omit() %>% 
            dplyr::select(adopter)) %>% 
  na.omit()


s_house %>% 
  st_join(dfEV %>% # buildings
            st_transform(4269) %>% 
            filter(adopter == 1) %>%
            filter(GEOID == tr),  join = st_intersects) %>% 
  na.omit() %>% 
  ggplot() +
  geom_sf(data = seattle_tracts %>% # tract layers
            filter(GEOID == tr), fill = "gray90", color = "blue", linewidth = 0.5) +
  geom_sf(fill = "gray60", color = NA) + # buildings
  geom_sf(data = dfEV %>% # buffers
            st_transform(4269) %>% 
            filter(adopter == 1) %>% 
            filter(GEOID == tr),
          fill = "red", color = "gray20", alpha = 0.05, size = 0.2) +
  geom_sf(data = t_d,
          color = "blue", shape = 4) +
  
  geom_sf(data = dfEV %>% # buffers
            st_transform(4269) %>% 
            filter(adopter == 1) %>% 
            filter(Dist == 50) %>% 
            filter(GEOID == tr) %>% 
            st_centroid(),
          color = "red", shape = 16, size = 2) +
  
  labs(fill = "", title = "b") +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = -0.05),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank())


tt_d <- dfEV %>% # buffers
  st_transform(4269) %>% 
  filter(adopter == 1) %>% 
  st_centroid() %>% 
  st_join(s_house %>% 
            st_join(dfEV %>% # buildings
                      st_transform(4269) %>% 
                      filter(adopter == 0) %>%
                      filter(GEOID == tr),  join = st_intersects) %>% 
            filter(!is.na(Dist)) %>% 
            dplyr::select(adopter)) %>% 
  na.omit()


s_house %>% 
  st_join(dfEV %>% # buildings
            st_transform(4269) %>% 
            filter(adopter == 0) %>% 
            filter(GEOID == tr),  join = st_intersects) %>% 
  filter(!is.na(Dist)) %>% 
  ggplot() +
  geom_sf(data = seattle_tracts %>% # tract layers
            filter(GEOID == tr), fill = "gray90", color = "blue", linewidth = 0.5) +
  geom_sf(fill = "gray60", color = NA) + # buildings
  
  geom_sf(data = dfEV %>% # buffers
            st_transform(4269) %>% 
            filter(adopter == 0) %>% 
            filter(GEOID == tr),
          fill = "red", color = "gray20", alpha = 0.05, size = 0.2) +
  
  geom_sf(data = tt_d,
          color = "blue", shape = 4) +
  
  geom_sf(data = dfEV %>% # buffers
            st_transform(4269) %>% 
            filter(adopter == 0) %>% 
            filter(Dist == 50) %>% 
            filter(GEOID == tr) %>% 
            st_centroid(),
          color = "red", shape = 16) +
  labs(fill = "", title = "c") +
  theme_minimal() +
  annotation_scale() +
  
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank())

# Vermont
trv <- 5001715700

v_house %>% 
  ggplot() +
  geom_sf(fill = "gray70", color = "gray70", size = 0.1) + # buildings
  geom_sf(data = temp %>% # tract layers
            filter(GEOID == trv), fill = NA, color = "blue") +
  
  labs(fill = "", title = "a\n") +
  theme_minimal() +
  annotation_scale() +
  
  theme(legend.position = c(0.9,0.3),
        legend.key.size = unit(0.25, 'cm'),
        legend.text=element_text(size=6.5),
        plot.margin = unit(c(0,1,0,0), "cm"),        
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank())



t_dv <- dfVT %>% # buffers
  st_transform(4269) %>% 
  filter(adopter == 1) %>% 
  filter(Dist == 50) %>% 
  filter(Year == 2014) %>% 
  st_centroid() %>% 
  st_join(dfVT %>% # buildings
            st_transform(4269) %>% 
            filter(adopter == 1) %>%
            filter(GEOID == trv) %>% 
            filter(Year == 2014) %>% 
            filter(Dist == 7000) %>% 
            dplyr::select(adopter),  join = st_intersects) %>% 
  filter(adopter.y == 1)


v_house %>% 
  
  st_join(dfVT %>% # buildings
            st_transform(4269) %>%
            filter(adopter == 1) %>% 
            filter(GEOID == trv) %>% 
            filter(Year == 2014),  join = st_intersects) %>% 
  na.omit() %>% 
  
  ggplot() +
  geom_sf(data = temp %>% # tract layers
            filter(GEOID == trv), fill = "gray90", color = "blue") +
  geom_sf(fill = "gray90") + # buildings
  geom_sf(data = dfVT %>% # buffers
            st_transform(4269) %>% 
            filter(adopter == 1) %>% 
            filter(GEOID == trv) %>% 
            filter(Year == 2014),
          fill = "red", color = "gray20", alpha = 0.05, size = 0.08) +
  
  geom_sf(data = t_dv,
          color = "blue", shape = 4) +
  
  geom_sf(data = dfVT %>% # point
            st_transform(4269) %>% 
            filter(adopter == 1) %>% 
            filter(Dist == 50) %>% 
            filter(GEOID == trv) %>% 
            filter(Year == 2014) %>% 
            st_centroid(),
          color = "red", shape = 16) +
  labs(fill = "", title = "b") +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = -0.1),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank())


tt_dv <- dfVT %>% # buffers
  st_transform(4269) %>% 
  filter(adopter == 1) %>% 
  filter(Dist == 50) %>% 
  filter(Year == 2014) %>% 
  st_centroid() %>% 
  st_join(dfVT %>% # buildings
            st_transform(4269) %>% 
            filter(adopter == 0) %>%
            filter(GEOID == trv) %>% 
            filter(Year == 2014) %>% 
            filter(Dist == 7000) %>% 
            dplyr::select(adopter),  join = st_intersects) %>% 
  filter(adopter.y == 0)


v_house %>% 
  
  st_join(dfVT %>% # buildings
            st_transform(4269) %>% 
            filter(adopter == 0) %>% 
            filter(GEOID == trv)%>% 
            filter(Year == 2014),  join = st_intersects) %>% 
  filter(!is.na(GEOID)) %>% 
  
  ggplot() +
  geom_sf(data = temp %>% # tract layers
            filter(GEOID == trv), fill = "gray90", color = "blue") +
  geom_sf(fill = "gray90") + # buildings
  geom_sf(data = dfVT %>% # buffers
            st_transform(4269) %>% 
            filter(adopter == 0) %>% 
            filter(GEOID == trv) %>% 
            filter(Year == 2014),
          fill = "red", color = "gray20", alpha = 0.05, size = 0.08) +
  
  geom_sf(data = tt_dv,
          color = "blue", shape = 4) +
  
  geom_sf(data = dfVT %>% # buffers
            st_transform(4269) %>% 
            filter(adopter == 0) %>% 
            filter(Dist == 50) %>% 
            filter(GEOID == trv) %>% 
            filter(Year == 2014) %>% 
            st_centroid(),
          color = "red", shape = 16) +
  labs(fill = "", title = "c") +
  theme_minimal() +
  annotation_scale() +
  
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank())


### f8
# only for solar to see the difference by sampling 
tfr <- d.200 %>% 
  filter(sample == "random") %>% 
  rename(Installed.Base = neighbor) %>% 
  mutate_at(vars(PopDensity:Gini,Installed.Base,Year), funs(scale(.) %>% as.numeric()))

tfg <- d.200 %>% 
  filter(sample == "GEOID") %>% 
  rename(Installed.Base = neighbor) %>% 
  mutate_at(vars(PopDensity:Gini,Installed.Base,Year), funs(scale(.) %>% as.numeric()))

nm <- names(tfr %>% 
              st_drop_geometry() %>% 
              dplyr::select(Installed.Base,Year,PopDensity:Gini))
md <- c("Within tract sampling", "Across tract sampling")
for(i in 1:2){
  
  if(i == 1){tf <- tfg }else{tf <- tfr}
  

  fit <- glm(adopter ~ Year+Installed.Base+
               PopDensity+HomeOwn+SingleFamily+Edu+HomeValue+Income+White+      
               Poverty+Gini,
             family = "binomial", data = tf)
  # summary(fit)
  
  sims <- 1000
  peGlm <- fit$coefficients
  vcGlm <- vcov(fit)
  simbetas <- mvrnorm(sims, peGlm, vcGlm) %>% 
    as.data.frame()
  
  
  for(j in 1:11){
    nei <- simbetas[,nm[j]]
    
    pe <- exp(mean(nei))
    upper <- quantile(nei, probs= 0.95) %>% exp()
    lower <- quantile(nei, probs= 0.05) %>% exp()
    
    
    tp <- cbind(pe,upper,lower) %>% 
      as.data.frame() %>% 
      mutate(var = nm[j],
             model = md[i])
    
    if(i == 1 & j == 1){ 
      TP <- tp 
    }else{
      TP <- rbind(tp,TP)
    }
    
  }
  
}

TP %>%
  as.data.frame() %>%
  mutate(var = factor(var, levels = nm),
         model = factor(model, levels = md)) %>%
  mutate(color = ifelse(upper > 1 & lower > 1, "Y",
                        ifelse(upper < 1 & lower < 1, "Y", "N")),
         color = factor(color, levels = c("Y", "N"))) %>% 
  
  ggplot(aes(y = pe, x = reorder(var,pe), group = var)) +
  geom_errorbar(aes(ymin=lower, ymax=upper),width = 0.1, size = 0.7) +
  geom_point(aes(color = color), size = 2.8) +
  geom_hline(yintercept = 1,linetype = "dotted", size = 1) +
  coord_flip() +
  facet_wrap(~model, scale = "free") +

  labs(x = "", y ="Odds ratio", 
       title = "a") +
  scale_color_manual(values = c("Y" = "red", "N" = "grey"), 
                     # labels = c("N","Y"),
                     name = "Significance") +

  theme_classic() +
  theme(legend.position = "bottom")


d.200 %>% 
  st_drop_geometry() %>% 
  filter(sample != "cluster") %>% 
  mutate(adopter = ifelse(adopter == 0, "N", "Y"),
         adopter = factor(adopter, levels = c("Y", "N")),
         sample = ifelse(sample == "GEOID", "Within tract sampling", "Across tract sampling")) %>% 
  group_by(Year, cluster, adopter, sample) %>% 
  summarise(mean = mean(neighbor)) %>% 
  mutate(sample = factor(sample, levels = c("Within tract sampling", "Across tract sampling"))) %>% 
  ggplot(aes(x = Year, y = mean, color = adopter)) + 
  # geom_line() +
  geom_smooth(aes(fill = adopter),alpha = 0.1, linewidth = 0.5) +
  labs(x = "Year", y= "\nAverage number of \nneighboring installed \nhousing buildings\n",
       color = "Adopter", fill = "Adopter",
       title = "b") +
  annotate("text", x=2012, y=2, label= "Within 200m",
           hjust = 0) +
  scale_x_continuous(breaks = c(2011,2015,2019)) +
  facet_wrap(~sample, scale = "free") +
  theme_classic() +
  theme(legend.position = "bottom")
