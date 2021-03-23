###########################################################################
#####                                                                 #####
#####             Script creating Supplementary table 1               #####
#####                                                                 #####
###########################################################################

#### Load packages ####
pacman::p_load(tidyverse, data.table)

#### Load data ####
df <- fread("data/full_data.csv")


#### Total for each disease ####
disease_tab <- as.data.frame(addmargins(table(df$Disease_plot)),
                             stringsAsFactors = F)
names(disease_tab) <- c("Disease", "n")

# Table titles
col_names <- disease_tab$Disease

# Create empty row between each summary
break_row <- data.frame(matrix(rep("-", length(col_names)), nrow = 1))
names(break_row) <- col_names

#### Region by disease ####
region_rename <- c(AFRO = "African Region",
                   EMRO = "Eastern Mediterranean Region",
                   EURO = "European Region",
                   PAHO = "Region of the Americas",
                   SEARO = "South-East Asia Region",
                   WPRO = "Western Pacific Region",
                   Multiple = "Multiple regions")

df$region_table <- as.character(region_rename[df$WHO_region])
region_tab <- as.data.frame.matrix(addmargins(table(df$region_table, 
                                                    df$Disease_plot)))

#### Spatial scale by disease ####
df$Spatial_scale_tab <- factor(data$Spatial_scale,        
                               levels = c("Country",
                                          "Country and district",
                                          "District",
                                          "Cluster",
                                          "Patch",
                                          "Individual"))


scale_tab <- as.data.frame.matrix(addmargins(table(df$Spatial_scale_tab, 
                                                   df$Disease_plot)))


#### Model type by disease ####
type_tab <- as.data.frame.matrix(addmargins(table(df$Model_type, 
                                                  df$Disease_plot)))


#### Model class by disease ####
df$model_class_tab <- df$Spatial_model_class
df[grep("and ", df$Spatial_model_class),]$model_class_tab <- "Mixed"
df$model_class_tab <- factor(df$model_class_tab,
                             levels = c("Fixed effect", "Mixed effect",
                                        "Machine learning", "Compartmental",
                                        "Mixed", "Other"))

class_tab <- as.data.frame.matrix(addmargins(table(df$model_class_tab, 
                                                   df$Disease_plot)))

#### Spatial models by disease ####
df$Spatial_model_tab <- factor(df$Spatial_model,
                               levels = c("GLM", "GWR", "GAM", 
                                          "Autoregressive distributed lag model",
                                          "GLMM", "GAMM", "DLNM",
                                          "GLMM and GLM",
                                          "GLMM and GWR", 
                                          "GLMM, GLM and GAM",
                                          "GLMM and GAMM",
                                          "Neural network",
                                          "Boosted regression trees",
                                          "Bayesian network",
                                          "Compartmental",
                                          "Metapopulation",
                                          "Agent-based model",
                                          "GIIM",
                                          "Bespoke",
                                          "Neural network and metapopulation",
                                          "Random forest and GLM"))

model_tab <- as.data.frame.matrix(addmargins(table(df$Spatial_model_tab, 
                                                   df$Disease_plot)))


#### Assumptions by disease ####
df$Assumption_plot <- ifelse(df$Mixed_connectivity_assumption == "Human and vector movement",
                             "Human and vector movement", data$Assumption_plot)

df$Assumption_plot <- factor(df$Assumption_plot,
                             levels = c("Distance-based",
                                        "Human movement",
                                        "Vector movement",
                                        "Human and vector movement",
                                        "Mixed",
                                        "Not given"))

ass_tab <- as.data.frame.matrix(addmargins(table(df$Assumption_plot, 
                                                 df$Disease_plot)))


#### Spatial data by disease ####
df$Spatial_data_standardised <- factor(df$Spatial_data_standardised,
                                       levels = c("Adjacency",
                                                  "Distance",
                                                  "Distance and direction",
                                                  "Coordinates",
                                                  "Mobility data",
                                                  "Mathematical model",
                                                  "Other covariates",
                                                  "Mixed",
                                                  "No data",
                                                  "Not given"))

data_tab <- as.data.frame.matrix(addmargins(table(df$Spatial_data_standardised, 
                                                  df$Disease_plot)))



#### Combine all tables ####
complete_tab <- rbind(region_tab, scale_tab, type_tab, class_tab, model_tab,
                      ass_tab, data_tab)

## create column percentages
i = 0
for(col in col_names) {
  i = i + 1
  total_dis <- disease_tab$n[i]
  complete_tab[paste0(col, "_pct")] <- round((complete_tab[col] / total_dis)*100, 1)
}
names(complete_tab) <- str_replace_all(names(complete_tab), c(" " = ".",
                                                              "," = ""))
complete_tab$end <- "%)"

complete_tab <- complete_tab %>%
  unite(Chikungunya, c(Chikungunya, Chikungunya_pct), 
        sep = " (", remove = TRUE) %>%
  unite(Chikungunya, c(Chikungunya, end), 
        sep = "", remove = F) %>%
  unite(Chikungunya.and.dengue, 
        c(Chikungunya.and.dengue, Chikungunya.and.dengue_pct), 
        sep = " (", remove = TRUE) %>%
  unite(Chikungunya.and.dengue, c(Chikungunya.and.dengue, end), 
        sep = "", remove = F) %>%
  unite(Chikungunya.and.Zika, c(Chikungunya.and.Zika, Chikungunya.and.Zika_pct), 
        sep = " (", remove = TRUE) %>%
  unite(Chikungunya.and.Zika, c(Chikungunya.and.Zika, end), 
        sep = "", remove = F) %>%
  unite(Dengue, c(Dengue, Dengue_pct), 
        sep = " (", remove = TRUE) %>%
  unite(Dengue, c(Dengue, end), 
        sep = "", remove = F) %>%
  unite(Dengue.chikungunya.and.Zika, 
        c(Dengue.chikungunya.and.Zika, Dengue.chikungunya.and.Zika_pct), 
        sep = " (", remove = TRUE) %>%
  unite(Dengue.chikungunya.and.Zika, c(Dengue.chikungunya.and.Zika, end), 
        sep = "", remove = F) %>% 
  unite(Japanese.Encephalitis, c(Japanese.Encephalitis, Japanese.Encephalitis_pct), 
        sep = " (", remove = TRUE) %>%
  unite(Japanese.Encephalitis, c(Japanese.Encephalitis, end), 
        sep = "", remove = F) %>%
  unite(Malaria, c(Malaria, Malaria_pct), 
        sep = " (", remove = TRUE) %>%
  unite(Malaria, c(Malaria, end), 
        sep = "", remove = F) %>%
  unite(Rift.Valley.Fever, c(Rift.Valley.Fever, Rift.Valley.Fever_pct), 
        sep = " (", remove = TRUE) %>%
  unite(Rift.Valley.Fever, c(Rift.Valley.Fever, end), 
        sep = "", remove = F) %>%
  unite(West.Nile.fever, c(West.Nile.fever, West.Nile.fever_pct), 
        sep = " (", remove = TRUE) %>%
  unite(West.Nile.fever, c(West.Nile.fever, end), 
        sep = "", remove = F) %>%
  unite(Yellow.fever, c(Yellow.fever, Yellow.fever_pct), 
        sep = " (", remove = TRUE) %>%
  unite(Yellow.fever, c(Yellow.fever, end), 
        sep = "", remove = F) %>%  
  unite(Zika, c(Zika, Zika_pct), 
        sep = " (", remove = TRUE) %>%
  unite(Zika, c(Zika, end), 
        sep = "", remove = F) %>%
  unite(Zika.and.dengue, c(Zika.and.dengue, Zika.and.dengue_pct), 
        sep = " (", remove = TRUE) %>%
  unite(Zika.and.dengue, c(Zika.and.dengue, end), 
        sep = "", remove = F) %>%
  unite(Sum, c(Sum, Sum_pct), 
        sep = " (", remove = TRUE) %>%
  unite(Sum, c(Sum, end), 
        sep = "", remove = T)

names(complete_tab) <- col_names

  

write.csv(complete_tab, file = "output/general_char_table.csv", row.names = T)

