###########################################################################
#####                                                                 #####
#####      Script creating figures included in systematic review      #####
#####                                                                 #####
###########################################################################


#### Load packages ####
pacman::p_load(tidyverse, data.table, sf, spdep, brazilmaps, geosphere,
               geobr, sfheaders)
source("functions.R")



#### Read in data ####
## Data extraction ##
df <- fread("data/clean_data_update.csv")


## Countries studied ##
countries <- fread("data/countries_full.csv", fill = T)


## States SP, RJ, ES + MG ##
shp_state <- read_state() 

shp_ill <- shp_state %>%
  filter(abbrev_state %in% c("SP", "RJ", "ES", "MG")) %>%
  # Obtain coordinates of centroid
  mutate(lon = map_dbl(geom, ~st_centroid_within_poly(.x)[[1]]),
         lat = map_dbl(geom, ~st_centroid_within_poly(.x)[[2]])) %>%
  st_as_sf()


## Add capital cities ##
capitals <- data.table(name = c("São Paulo", "Rio de Janeiro", "Vitória", 
                                "Belo Horizonte"),
                       abbrev_state = c("SP", "RJ", "ES", "MG"),
                       lat = c(-23.5505, -22.9068, -20.2976, -19.9167),
                       lon = c(-46.6333, -43.1729, -40.2958, -43.9345),
                       lat1 = c(-23.5505, -22.9068, -20.2976, -19.9167),
                       lon1 = c(-46.6333, -43.1729, -40.2958, -43.9345))
coordinates(capitals) <- ~lat+lon
capitals <- st_as_sf(capitals)
st_crs(capitals) <- st_crs(shp_ill)


## Air travel info ##
gravity_se <- fread("data/air_travel_se.csv")



#### Figure 2: Number of spatial modelling studies published per year by model type ####
## Add 'mixed' category
df$Model_plot <- df$Spatial_model_class
df[grep(" and ", df$Spatial_model_class), ]$Model_plot <- "Mixed"


## Create colour palette
model_class_col <- c("Fixed effect" = "#779FA1",
                     "Random effect" = "#9E606F",
                     "Machine learning" = "#FFD166",
                     "Compartmental" = "#073B4C",
                     "Mixed" = "#06d6a0",
                     "Other" = "#FF6542")



## Re-order model type for plot 
df$Model_plot <- factor(df$Model_plot,
                        levels = c("Fixed effect", "Random effect",
                                   "Machine learning", "Compartmental",
                                   "Mixed", "Other"))


## Histogram
model_class <- ggplot(data = df) +
  geom_histogram(aes(x = Year, fill = Model_plot), colour = "black",
                 binwidth = 1) +
  scale_fill_manual(values = model_class_col, name = "Type of model") +
  theme_light() +
  scale_y_continuous(name = "Frequency", expand = expansion(0)) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15))


## Save plot
ggsave(model_class, filename = "output/model_class.png",
       width = 10, height = 5)



#### Figure 3:Comparison of spatial connectivity using different data sources and assumptions ####
## Map of Southeast Brazil (with centroid and capitals)
southeast_map <- ggplot() +
  #geom_sf(data = shp_state, fill = "white", col = "lightgrey") +
  geom_sf(data = shp_ill, col = "black", fill = "white") +
  geom_point(data = shp_ill, aes(x = lon, y = lat), shape = "x", size = 5) +
  geom_point(data = capitals, aes(x = lon1, y = lat1), size = 2) +
  #geom_text(data = capitals, aes(label = name, x = lon1, y = lat1)) +
  # scale_fill_brewer(name = "State", palette = "Pastel1") +
  coord_sf(xlim = c(-53, -39), ylim = c(-25, -14)) +
  theme_void()


ggsave(southeast_map, filename = "output/southeastmap.png")

## Figure 3A: Neighbourhood-based
# Create matrix of connectivity weights
df_neighb <- data.table(state1 = c(rep("SP", 4), rep("RJ", 4), rep("ES", 4),
                                   rep("MG", 4)),
                        state2 = rep(c("SP", "RJ", "ES", "MG"), 4),
                        neigh = c(NA,1,0,1,1,NA,1,1,0,1,NA,1,1,1,1,NA)) %>%
  mutate(state1 = factor(state1, levels = c("SP", "RJ", "MG", "ES")),
         state2 = factor(state2, levels = c("SP", "RJ", "MG", "ES")))


# Create and save heatplot
heat_neighb <- ggplot(data = df_neighb, aes(x = state1, y = state2, fill = neigh)) +
  geom_raster() +
  geom_text(aes(label = neigh), colour = "black", size = 5) +
  expand_limits(fill = c(0, 1)) +
  scale_fill_gradient2(name = "Weight", low = "white", mid = "#457b9d", 
                       high = "#e63946", midpoint = 0.5,
                       na.value = "white") +
  labs(x = "State", y = "State") +
  coord_equal() +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(size = 15)) 


ggsave(heat_neighb, filename = "output/neighbour_weight.png", 
       height = 5, width = 6.5)



## Figure 3B: Distance-based
# Calculate distance between centroid + weight
df_dist <- mutate(shp_ill, 
                  k = 1)

df_dist <- df_dist %>% 
  full_join(st_drop_geometry(df_dist), by = "k") %>%
  mutate(distkm = distGeo(cbind(lon.x, lat.x), cbind(lon.y, lat.y))/1000,
         decay_dist = ifelse(abbrev_state.x == abbrev_state.y, NA,
                             exp(-distkm/1000)),
         id = paste(abbrev_state.x, abbrev_state.y),
         state1 = factor(abbrev_state.x, levels = c("SP", "RJ", "MG", "ES")),
         state2 = factor(abbrev_state.y, levels = c("SP", "RJ", "MG", "ES"))) %>%
  st_drop_geometry()


# Create and save heatplot
heat_dist <- ggplot(data = df_dist, aes(x = state1, y = state2, fill = decay_dist)) +
  geom_raster() +
  geom_text(aes(label = round(decay_dist, 2)), colour = "black",
            size = 5) +
  expand_limits(fill = c(0, 1)) +
  scale_fill_gradient2(name = "Weight", low = "white", mid = "#457b9d", 
                       high = "#e63946", midpoint = 0.5,
                       na.value = "white") +
  labs(x = "State", y = "State") +
  coord_equal() +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(size = 15)) 


ggsave(heat_dist, filename = "output/heat_dist.png", 
       height = 5, width = 6.5)



## Figure 3C: Human movement data
# Combine data from origin + destination into matrix
df_air1 <- dplyr::select(gravity_se, State_origin, State_dest, Passengers10,
                         Gravity_passenger, Log_passengers, Gravity_model)

df_air2 <- dplyr::select(gravity_se, State_origin, State_dest, Passengers10,
                         Gravity_passenger, Log_passengers, Gravity_model) %>%
  rename(State_dest = State_origin,
         State_origin = State_dest) 

# Add values where origin = destination (set as NA)
df_air3 <- data.table(State_origin = c("SP", "RJ", "ES", "MG"),
                      State_dest = c("SP", "RJ", "ES", "MG"),
                      Passengers10 = rep(NA, 4),
                      Gravity_passenger = rep(NA, 4),
                      Log_passengers = rep(NA, 4),
                      Gravity_model = rep(NA, 4),
                      Passengers1mil = rep(NA, 4),
                      Gravity_1mil = rep(NA, 4))

# Combine into matrix + reduce passengers to per million
df_air <- rbind(df_air1, df_air2) %>%
  mutate(Passengers1mil = Passengers10/10^6,
         Gravity_1mil = Gravity_passenger/10^6) %>%
  rbind(., df_air3) %>%
  mutate(state1 = factor(State_origin, levels = c("SP", "RJ", "MG", "ES")),
         state2 = factor(State_dest, levels = c("SP", "RJ", "MG", "ES")))



# Create and save heatplot
heat_air <- ggplot(data = df_air, aes(x = state1, y = state2, 
                                      fill = Passengers1mil)) +
  geom_raster() +
  geom_text(aes(label = round(Passengers1mil, 2)), size = 5) +
  scale_fill_gradient2(name = "Weight", low = "white", mid = "#457b9d", 
                       high = "#e63946", midpoint = 3,
                       na.value = "white") +
  labs(x = "State", y = "State") +
  coord_equal() +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(size = 15)) 


ggsave(heat_air, filename = "output/heat_air.png", 
       height = 5, width = 6.5)



## Figure 3D: Movement model
heat_gravity <- ggplot(data = df_air, aes(x = state1, y = state2, 
                                          fill = Gravity_1mil)) +
  geom_raster() +
  geom_text(aes(label = round(Gravity_1mil, 2)), size = 5) +
  scale_fill_gradient2(name = "Weight", low = "white", mid = "#457b9d", 
                       high = "#e63946", midpoint = 0.5,
                       na.value = "white") +
  labs(x = "State", y = "State") +
  coord_equal() +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(size = 15)) 


ggsave(heat_gravity, filename = "output/heat_gravity.png", 
       height = 5, width = 6.5)



#### Figure 4: Connectivity assumptions by mosquito species ####
## Create mosquito species variable based on disease
df$mosquito <- ifelse(df$Disease_std %in% c("Chikungunya",
                                            "Chikungunya and dengue",
                                            "Chikungunya and Zika",
                                            "Dengue", 
                                            "Dengue, chikungunya and Zika",
                                            "Yellow fever", "Zika", 
                                            "Zika and dengue"), "Aedes",
                      ifelse(df$Disease_std %in% c("Japanese Encephalitis",
                                                    "West Nile virus",
                                                    "Rift Valley Fever"),
                             "Culex", 
                             ifelse(df$Disease_std == "Malaria", "Anopheles", 
                                    "Error")))


## Create Assumption_results which groups distance, neighbour and shared characteristics togehter
df$Assumption_results <- ifelse(df$Assumption_plot %in% c("Distance", 
                                                          "Neighbours connected", 
                                                          "Shared characteristics") |
                                  df$Mixed_assumption %in% c("Distance and neighbours connected", 
                                                              "Neighbours connected, distance",
                                                              "Shared characteristics and distance",
                                                              "Shared characteristics and neighbours connected"),
                                "Distance-based", df$Assumption_plot)



## Save percentage of studies per species using each assumption 
species_ass_df <- group_by(df, mosquito, Assumption_results) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count)*100,
         Assumption_results = ifelse(Assumption_results == "", "Not given",
                                     Assumption_results))


mosquito_ass_plot <- ggplot(data = species_ass_df) +
  geom_bar(aes(x = mosquito, y = perc, fill = Assumption_results),
           stat = "Identity", colour = "black") +
  scale_x_discrete(name = "Mosquito species",
                   labels = c("Aedes \n(n = 117)", "Anopheles \n(n = 118)",
                              "Culex \n(n = 13)")) +
  scale_fill_manual(values = c("#9E606F", 
                               "#779FA1",
                               "#073B4C", 
                               "#118AB2",   
                               "#FF6542"),
                    name = "Spatial assumption") +
  theme_light() +
  scale_y_continuous(name = "Percentage", expand = expansion(0)) +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 10))


ggsave(mosquito_ass_plot, filename = "output/mosquito_ass.png")



#### Figure 5: Connectivity assumption by model type ####
assumption_by_model <- ggplot(data = df) +
  geom_bar(aes(x = Assumption_results, fill = Model_plot), 
           colour = "black") +
  scale_fill_manual(values = model_class_col, name = "Type of model") +
  scale_x_discrete(labels = c("Distance-based",
                              "Human \nmovement",
                              "Mixed",
                              "Not given",
                              "Vector \nmovement"),
                   name = "Spatial assumption") +
  theme_light() +
  scale_y_continuous(name = "Frequency", expand = expansion(0)) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15))


ggsave(assumption_by_model, filename = "Output/assumption_by_model.png",
       width = 10, height = 7.5)


#### Figure S1: Spatial distribution of studies included in this review ####
## Read in world map data ##
world_map <- map_data("world")


## Convert data into number of studies per country 
countries <- as.data.frame(table(countries$country_map)) %>%
  transmute(name_long = as.character(Var1),
            count = Freq)


## Map showing number of studies per country ##
map_plot <- ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "white", colour = "black", size = 0.1) +
  geom_map(data = countries, map = world_map,
           aes(fill = count, map_id = name_long),
           colour = "black", size = 0.1) +
  #coord_map("rectangular", lat0 = 0, xlim = c(-180, 180), ylim = c(-60, 90)) +
  scale_fill_viridis_c(direction = -1, name = "Number of \nstudies") +
  scale_y_continuous(breaks = c(), name = "") +
  scale_x_continuous(breaks = c(), name = "") +
  theme_minimal() +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 10))


ggsave(map_plot, filename = "output/map_studies.png", width = 10, height = 4)



#### Figure S2: Number of spatial modelling studies published per year by mosquito-borne disease ####
disease_timeplot <- ggplot(data = df) +
  geom_histogram(aes(x = Year, fill = Disease_std), colour = "black", 
                 binwidth = 1) +
  scale_fill_viridis_d(name = "Disease", option = "C") +
  theme_light() +
  scale_y_continuous(name = "Frequency", expand = expansion(0)) +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 10))


ggsave(disease_timeplot, filename = "output/disease_year.png", 
       width = 10, height = 5)