#' sum of isotopic niches overlap
#'
#' @param isotope_data_fish : table with species, d15n, d13c and habitat columns
#' @param alea : if true calculate convex hull
#'
#' @examples sum_overlap_std(isotope_data_fish, alea=TRUE)

sum_overlap <- function(isotope_data_fish, alea) {
  
  # preparation of the file
  isotope_data_fish <- data.frame(na.omit(isotope_data_fish))
  d13C <- isotope_data_fish$d13C
  d15N <- isotope_data_fish$d15N
  X <- isotope_data_fish$d13C
  Y <- isotope_data_fish$d15N
  PID <- 1:nrow(isotope_data_fish)
  xydata <- PBSmapping::as.PolyData(cbind(PID, X, Y), projection = NULL, zone = NULL)
  
  # Calculation of ellipse area
  overlap <-
    rKIN::estEllipse(
      data = isotope_data_fish,
      x = "d13C",
      y = "d15N",
      group = "species",
      levels = 40,
      smallSamp = TRUE
    )
  
  # Calculation of overlap
  overlap_matrix <- rKIN::calcOverlap(overlap) %>%
    mutate(across(where(is.numeric), round, 2)) %>%
    tibble::column_to_rownames("OverlapID") %>%
    as.data.frame()
  
  # Sum of all overlaps in the habitat
  OVER <- sum(overlap_matrix)
  
  if (alea == F) {
    lt <- list(OVER = OVER)
  } else {
    lt <- list(OVER = OVER,
               ConvHull = PBSmapping::calcConvexHull(xydata))
  }
  return(lt)
}

#' bootstrap isotopic values in convex hull
#'
#' @param isotope_data_fish: data frame with species, d13C, d15N and habitat columns
#' @param samp: number of samples
#' @param nbBoot: number of bootstrapping isotopic values in the convex hull
#'
#' @examples sum_overlap_boot (isotope_data_fish, samp=10, nbBoot=1000)

sum_overlap_boot <- function(isotope_data_fish, samp, nbBoot) {
  isotope_data_fish <- data.frame(na.omit(isotope_data_fish))
  nbBoot <- nbBoot
  SpNames <- isotope_data_fish$species  
  habNames <- unique(isotope_data_fish$habitat)
  d13C <- isotope_data_fish$d13C
  d15N <- isotope_data_fish$d15N
  d13Cd15N <- cbind(d13C, d15N)
  X <- isotope_data_fish$d13C
  Y <- isotope_data_fish$d15N
  PID <- 1:nrow(isotope_data_fish)
  xydata <- PBSmapping::as.PolyData(cbind(PID, X, Y), projection = NULL, zone = NULL)
  xyCoord <- sp::coordinates(cbind(X, Y))
  
  ## original sampling
  res0 <- sum_overlap(isotope_data_fish, alea = TRUE)
  sres0 <-
    sp::SpatialPolygons(list(sp::Polygons(list(
      sp::Polygon(as.matrix(res0$ConvHull[, 3:4]))
    ), "x")))
  
  ## Boostraping  in the result of the convex hull
  boot_iso <- list()
  Ind <- list()
  
  for (i in 1:nbBoot) {
    set.seed(i)
    sampiS <- sp::spsample(sres0, n = samp, "random")
    boot_iso[[i]] <-
      cbind(data.frame(sampiS),
            rep(paste(habNames), length(sampiS)),
            rep(SpNames, length(sampiS)))
    names(boot_iso[[i]]) <- c("d13C" , "d15N", "habitat", "species")
    Ind[[i]] <- sum_overlap(boot_iso[[i]], alea = FALSE)
  }
  
  res <-
    data.frame(matrix(unlist(Ind), ncol = (unique(
      sapply(Ind, length)
    )), byrow = TRUE))
  colnames(res) <- c("OVER")
  res$habitat <- rep(habNames, nbBoot)
  return(res)
}

null_model_overlap <- function(data) {
  
  # prepare data
isotope_data_fish <- data%>%
  mutate(species = recode(species, "Cyclothone" = "Cyclothone spp.")) %>%
  arrange(species)

# example by depth layer
isotope_data_fish <- isotope_data_fish %>%
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
habitat_list <- unique(isotope_data_fish$habitat)

# Initialize empty data frames for observed and bootstrapped values
observed_df <-
  data.frame(
    habitat = character(0),
    index = character(0),
    value = numeric(0),
    type = character(0)
  )
bootstrapped_df <-
  data.frame(
    habitat = character(0),
    index = character(0),
    value = numeric(0),
    type = character(0)
  )

# Loop through each habitat
for (habitat_name in habitat_list) {
  # Filter the data for the current habitat
  habitat_data <- isotope_data_fish %>%
    filter(habitat == habitat_name)
  
  # Compute observed isotopic diversity indices for the current habitat
  observed_indices <- sum_overlap(habitat_data, alea = TRUE)
  
  # Number of bootstrap iterations
  nbBoot <- 1000
  samp <- 10
  
  # Bootstrap the isotopic diversity indices for the current habitat
  bootstrapped_indices <- sum_overlap_boot(habitat_data,
                                           samp = nrow(habitat_data),
                                           nbBoot = nbBoot)
  
  # a data frame to store the number of species per habitat
  species_count <- habitat_data %>%
    mutate(num_species = n_distinct(species)) %>%
    select(habitat, num_species) %>%
    distinct()
  
  # Create data frames for observed and bootstrapped values
  for (index_name in c("OVER")) {
    # Extract observed value and add to observed_df, standardized by the number of species
    observed_value <- observed_indices[[index_name]]
    habitat_species_count <-
      species_count$num_species[species_count$habitat == habitat_name]
    observed_df <- rbind(
      observed_df,
      data.frame(
        habitat = habitat_name,
        index = index_name,
        value = observed_value / habitat_species_count,
        type = "observed"
      )
    )
    
    # Extract bootstrapped values and add to bootstrapped_df, standardized by the number of species
    bootstrapped_values <- bootstrapped_indices[[index_name]]
    bootstrapped_df <- rbind(
      bootstrapped_df,
      data.frame(
        habitat = habitat_name,
        index = index_name,
        value = bootstrapped_values / habitat_species_count,
        type = "permuted"
      )
    )
  }
}

# plot results ----
bootstrapped_df$habitat <- factor(
  bootstrapped_df$habitat,
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
observed_df$habitat <- factor(
  observed_df$habitat,
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

ggplot(bootstrapped_df, aes(x = value)) +
  facet_wrap( ~ habitat) +
  theme_light() +
  geom_density(aes(y = ..count..), fill = "gray20", alpha = 0.7, color = NA) +
  geom_vline(data = observed_df, aes(xintercept = value), color = "#045171",
             linetype = "longdash", size = 0.8, alpha=0.8) +
  labs(x = "Sum of isotopic niche overlaps", y="Frequency")+
  theme(strip.text.x = element_text(size = 12, face = "bold", color = "gray20"),
        strip.background=element_rect(fill="white"),
        axis.title = element_text(size=12),
        axis.text = element_text(size=12))

ggsave("over_depth_layer.png", path = "figures", dpi = 700, height = 6, width = 9)

}
