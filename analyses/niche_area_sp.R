# libraries & functions ----
library(dplyr)
library(ggplot2)
source("R/niche_area_boot.R")
source("R/niche_area.R")

# data ----
isotope_data <-
  utils::read.csv(
    here::here("data", "isotopic_data_2021.csv"),
    sep = ";",
    header = T,
    dec = ","
  )%>%
  # remove outlier values 
  filter(individual_code!="Arg-olf 42")

# prepare data
isotope_data_fish <- isotope_data %>%
  # only fish
  filter(taxon == "Fish") %>%
  mutate(species = recode(species, "Cyclothone" = "Cyclothone spp.")) %>%
  arrange(species)

# By depth layer
table_iso <- isotope_data_fish %>%
  rename(d15N = d15n,
         d13C = d13c) %>%
  mutate(
    habitat = case_when(
      trawling_depth %in% c(25) ~ "epipelagic",
      trawling_depth %in% c(370, 555) ~ "upper-mesopelagic",
      trawling_depth %in% c(715, 1000) ~ "lower-mesopelagic",
      trawling_depth %in% c(1335) ~ "bathypelagic",
      trawling_depth %in% c(1010) ~ "bottom-proximity"
    )
  ) %>%
  select(species, habitat, d15N, d13C)

# compute analyses ----
# List of habitats
habitat_list <- unique(table_iso$habitat)

# Create an empty data frame to store results
results_df <-
  data.frame(
    habitat = character(0),
    index = character(0),
    value = numeric(0),
    type = character(0)
  )

# Loop through each habitat
for (habitat_name in habitat_list) {
  # Filter the data for the current habitat
  habitat_data <- table_iso %>%
    filter(habitat == habitat_name)
  
  # Compute observed isotopic diversity indices for the current habitat
  observed_indices <- niche_area(habitat_data, alea = TRUE)
  
  # Number of bootstrap iterations
  nbBoot <- 1000
  samp <- 10
  
  # Bootstrap the isotopic diversity indices for the current habitat
  bootstrapped_indices <- niche_area_boot(habitat_data,
                                          samp = nrow(habitat_data),
                                          nbBoot = nbBoot)
  
  # Store observed and bootstrapped values in the results dataframe
  observed_df <-
    data.frame(
      habitat = habitat_name,
      index = "observed",
      value = observed_indices$ELP,
      type = "observed"
    )
  bootstrapped_df <-
    data.frame(
      habitat = habitat_name,
      index = 1:nbBoot,
      value = bootstrapped_indices$ELP,
      type = "bootstrapped"
    )
  
  # Append results for the current habitat to the results data frame
  results_df <- rbind(results_df, observed_df, bootstrapped_df)
}

# Plot results ----
library(ggplot2)

results_df$habitat <- factor(
  results_df$habitat,
  levels =  c(
    "epipelagic",
    "upper-mesopelagic",
    "lower-mesopelagic",
    "bathypelagic",
    "bottom-proximity"
  ),
  labels = c(
    "Epipelagic",
    "Upper-mesopelagic",
    "Lower-mesopelagic",
    "Bathypelagic",
    "Bottom proximity"
  )
)

ggplot(results_df, aes(x = value)) +
  facet_wrap( ~ habitat) +
  theme_light() +
  geom_density(
    data = subset(results_df, type == "bootstrapped"),
    aes(y = ..count..),
    fill = "darkgrey",
    alpha = 0.7,
    color = NA
  ) +
  geom_segment(
    data = subset(results_df, type == "observed"),
    aes(
      x = value,
      xend = value,
      y = 0,
      yend = 21800
    ),
    color = "#045171",
    linetype = "longdash",
    size = 0.8,
    alpha = 0.8
  ) +
  labs(x = "Isotopic niche size", y = "Frequency") +
  theme(
    strip.text.x = element_text(
      size = 12,
      face = "bold",
      color = "gray50"
    ),
    strip.background = element_rect(fill = "white"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  )

ggsave("niches_area_model.png", path = "figures", dpi = 700, height = 6, width = 9)
