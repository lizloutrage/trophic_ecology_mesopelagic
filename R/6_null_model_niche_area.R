#' calculating species ellipse area in a habitat
#'
#' @param table_iso: has to have these column names species, d15N, d13C, habitat
#' @param alea: if true calculate convex hull
#'
#'
#' @examples ellipse_size(table_iso, alea = TRUE)

niche_area <- function(table_iso, alea) {
  table_iso <- data.frame(na.omit(table_iso))
  SpNames <- table_iso$species
  d13C <- table_iso$d13C
  d15N <- table_iso$d15N
  X <- table_iso$d13C
  Y <- table_iso$d15N
  PID <- 1:nrow(table_iso)
  xydata <- PBSmapping::as.PolyData(cbind(PID, X, Y), projection = NULL, zone = NULL)
  xyCoord <- sp::coordinates(cbind(X, Y))
  
  # Calculation ellipse size with rKIN package
  sea <-
    rKIN::estEllipse(
      data = table_iso,
      x = "d13C",
      y = "d15N",
      group = "species",
      levels = 40,
      smallSamp = TRUE
    )
  
  ELP <-  sea$estObj[, c('ShapeArea', 'Group')][[1]]
  
  if (alea == F) {
    lt <- list(ELP = ELP)
  } else {
    lt <-list(ELP = ELP, ConvHull = PBSmapping::calcConvexHull(xydata))
  }
  return(lt)
}

#' Function to bootstrap the ellipse size (inspired from Brind'Amour & Dubois 2013 and  Suchomel et al. 2022)
#'
#' @param table_iso
#' @param samp
#' @param nbBoot
#'
#' @examples  ellipse_boot(table_iso, samp = 10, nbBoot = 100)

niche_area_boot <- function(table_iso, samp, nbBoot) {
  table_iso <- data.frame(na.omit(table_iso))
  nbBoot <- nbBoot
  SpNames <- table_iso$species
  habNames <- unique(table_iso$habitat)
  d13C <- table_iso$d13C
  d15N <- table_iso$d15N
  d13Cd15N <- cbind(d13C, d15N)
  X <- table_iso$d13C
  Y <- table_iso$d15N
  PID <- 1:nrow(table_iso)
  xydata <- PBSmapping::as.PolyData(cbind(PID, X, Y), projection = NULL, zone = NULL)
  xyCoord <- sp::coordinates(cbind(X, Y))
  
  ## original sampling
  res0 <- niche_area(table_iso, alea = T)
  
  ##transform points to spatial points
  sres0 <- sp::SpatialPolygons(list(sp::Polygons(list(
    sp::Polygon(as.matrix(res0$ConvHull[, 3:4]))), "x"))) 
  
  ## Bootstrapping in the result of the convex hull
  boot_iso <- list()
  Ind <- list()
  
  for (i in 1:nbBoot) {
    set.seed(i)
    
    # sample point locations within the polygon, using random sampling method
    sampiS <- sp::spsample(sres0, n = samp, "random")
    boot_iso[[i]] <- cbind(data.frame(sampiS), rep(paste(habNames), length(sampiS)), paste(SpNames))
    names(boot_iso[[i]]) <- c("d13C" , "d15N", "habitat", "species")  
    Ind[[i]] <- niche_area(boot_iso[[i]], alea = F)
  }
  
  res <- data.frame(matrix(unlist(Ind), ncol = (unique(sapply(Ind, length))), byrow = TRUE))
  colnames(res) <- c("ELP")
  res$habitat <- rep(habNames, nbBoot)
  return(res)
  
} 


#' Comparaison of random and real species niche area 
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples

null_model_niche_area <- function(data){
  
  # prepare data
isotope_data_fish <- data %>%
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

ggsave("niches_area_model2.png", path = "figures", dpi = 700, height = 6, width = 9)
  
}

