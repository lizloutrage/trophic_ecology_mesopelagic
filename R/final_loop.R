# Load required libraries ----
library(dplyr)
library(ggplot2)

# load data ----
isotope_data <-  utils::read.csv(here::here("data", "isotopic_data_2021.csv"), sep = ";", header = T, dec = ",")

## Isotope data 
isotope_data_fish <- isotope_data %>%
  # only fish
  filter(taxon == "Fish") %>%
  mutate(species = recode(species, "Cyclothone" = "Cyclothone spp.")) %>%
  arrange(species)

# format final data 
table_iso <- isotope_data_fish %>%
  select(species, station, d15n, d13c) %>%
  rename(habitat = station,
         d15N = d15n,
         d13C = d13c)%>%
  mutate(species= gsub("_"," ", species))%>%
  arrange(habitat, species)

## Biomass data 
trawling_data_evhoe21 <-  utils::read.csv(here::here("data", "trawling_data_evhoe_2021.csv"), sep = ";", header = T, dec = ".")

species_status_biomass <- trawling_data_evhoe21%>%
  # selection of mesopelagic trawl 
  filter(Code_Station%in% c("Z0524", "Z0518", "Z0512", "Z0508", 
                            "Z0503", "Z0497", "Z0492"))%>%
  #deletion when only genus name and genus already sampled in isotope + A. carbo
  filter(!Nom_Scientifique%in%c("Cyclothone braueri","Cyclothone microdon", "Myctophidae",
                                "Aphanopus carbo","Lampanyctus"))%>%
  select(Code_Station, Tot_V_HV, Nom_Scientifique, Code_Espece_Campagne)%>%
  distinct()%>%
  # sum species biomass by depth 
  group_by(Nom_Scientifique, Code_Station)%>%
  mutate(biomass_sp=sum(Tot_V_HV))%>%
  select(-Tot_V_HV)%>%
  distinct()%>%
  # sum of total biomass by station
  group_by(Code_Station)%>%
  mutate(biomass_tot=sum(biomass_sp))%>%
  # relative biomass of each species by station
  mutate(rel_biomass=biomass_sp/biomass_tot*100)%>%
  select(-c(biomass_sp, biomass_tot))%>%
  # selection of species sampled for isotopy in each depth 
  filter(Code_Station== "Z0492"& 
           Nom_Scientifique%in% c("Arctozenus risso", "Argyropelecus olfersii",
                                  "Lampanyctus crocodilus", "Myctophum punctatum",
                                  "Notoscopelus kroyeri", "Xenodermichthys copei")|
           Code_Station== "Z0497"& 
           Nom_Scientifique%in% c("Argyropelecus olfersii", "Lampanyctus crocodilus", 
                                  "Lampanyctus macdonaldi", "Myctophum punctatum",
                                  "Serrivomer beanii", "Xenodermichthys copei")|
           Code_Station== "Z0503"& 
           Nom_Scientifique%in% c("Arctozenus risso","Cyclothone","Lampanyctus crocodilus",
                                  "Serrivomer beanii", "Xenodermichthys copei")|
           Code_Station== "Z0508"& 
           Nom_Scientifique%in% c("Lestidiops sphyrenoides", "Maurolicus muelleri", 
                                  "Myctophum punctatum","Notoscopelus kroyeri",
                                  "Notoscopelus bolini")|
           Code_Station== "Z0512"& 
           Nom_Scientifique%in% c("Arctozenus risso","Argyropelecus olfersii",
                                  "Lampanyctus crocodilus","Notoscopelus kroyeri",
                                  "Xenodermichthys copei")|
           Code_Station== "Z0518"& 
           Nom_Scientifique%in% c("Argyropelecus olfersii","Lampanyctus crocodilus",
                                  "Benthosema glaciale", "Maulisia argipalla",
                                  "Searsia koefoedi", "Serrivomer beanii")|
           Code_Station== "Z0524"& 
           Nom_Scientifique%in% c("Argyropelecus olfersii","Lampanyctus crocodilus",
                                  "Melanostigma atlanticum",  "Serrivomer beanii",
                                  "Xenodermichthys copei"))%>%
  mutate(Species_code= tolower(Code_Espece_Campagne))%>%
  rename(Species_name= Nom_Scientifique,
         Status=Code_Station)%>%
  select(-Code_Espece_Campagne)%>%
  relocate(Status, .after=rel_biomass)%>%
  relocate(Species_code,.after = Species_name)%>%
  arrange(Species_name)

# format final data 
sp_biomass <- species_status_biomass %>%
  rename(habitat = Status, species = Species_name) %>%
  select(-Species_code) %>%
  mutate(species=recode(species, "Cyclothone"="Cyclothone spp."))%>%
  as.data.frame()%>%
  arrange(habitat, species)

# functions ----
source("R/sample_species_biomass.R")
source("R/isotopic_diversity_ind_biomass.R")
source("R/isotopic_diversity_boot_biomass.R")

# Loop for each habitat ----

# List of habitats
habitat_list <- unique(table_iso$habitat)

# Initialize a list to store results for each habitat
results_list <- list()

# Number of bootstrap iterations
nbBoot <- 1000
samp <- 70  

# Initialize a list to store ggplot objects
plot_list <- list()

# Function to generate density plots
generate_density_plot <- function(bootstrapped_data, observed_data, index_name, title) {
  ggplot(data = bootstrapped_data, aes(x = .data[[index_name]], fill = "grey")) +
    geom_density(alpha = 0.5, fill = "grey") +
    geom_vline(xintercept = observed_data,
               color = "black",
               linetype = "dashed") +
    annotate("text",
             x = observed_data + 0.06,
             y = 0.5,
             label = paste("Observed:", round(observed_data, digits = 3)),
             color = "black") +
    labs(title = title) +
    guides(fill = "none") +
    xlab(index_name) +
    theme_minimal()
}

# Create density plots for each index and each habitat
  for (index_name in c("IDiv", "IDis", "IEve", "IUni")) {
    for (habitat_name in habitat_list) {
      
      # Filter the data for the current habitat
      habitat_data <- table_iso %>%
        filter(habitat == habitat_name)
      
      sp_biomass_habitat <- sp_biomass %>%
        filter(habitat == habitat_name)
      
      # Compute observed isotopic diversity indices for the current habitat
      observed_indices <- isotopic_diversity_ind_biomass(habitat_data, alea = TRUE, sp_biomass_habitat)
      
      # Bootstrap the isotopic diversity indices for the current habitat
      bootstrapped_indices <- isotopic_diversity_boot_biomass(habitat_data,
                                                              samp = samp,
                                                              nbBoot = nbBoot,
                                                              sp_biomass_habitat)
      
      # Generate the density plot
      plot_title <- paste("Density Plot for", index_name, "in Habitat", habitat_name)
      density_plot <- generate_density_plot(bootstrapped_data = bootstrapped_indices,
                                            observed_data = observed_indices[[index_name]],
                                            index_name = index_name,
                                            title = plot_title)
      
      # Display the plot
      print(density_plot)
    }
  }
}