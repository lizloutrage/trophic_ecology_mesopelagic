#' calculating species ellipse area in a habitat
#'
#' @param isotope_data_fish: has to have these column names species, d15N, d13C, habitat
#' @param alea: if true calculate convex hull
#'
#'
#' @examples ellipse_size(isotope_data_fish, alea = TRUE)

niche_area <- function(isotope_data_fish, alea) {
  isotope_data_fish <- data.frame(na.omit(isotope_data_fish))
  SpNames <- isotope_data_fish$species
  d13C <- isotope_data_fish$d13C
  d15N <- isotope_data_fish$d15N
  X <- isotope_data_fish$d13C
  Y <- isotope_data_fish$d15N
  PID <- 1:nrow(isotope_data_fish)
  xydata <- PBSmapping::as.PolyData(cbind(PID, X, Y), projection = NULL, zone = NULL)
  xyCoord <- sp::coordinates(cbind(X, Y))
  
  # Calculation ellipse size with rKIN package
  sea <-
    rKIN::estEllipse (
      data = isotope_data_fish,
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
#' @param isotope_data_fish
#' @param samp
#' @param nbBoot
#'
#' @examples  ellipse_boot(isotope_data_fish, samp = 10, nbBoot = 100)

niche_area_boot <- function(isotope_data_fish, samp, nbBoot) {
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
  res0 <- niche_area(isotope_data_fish, alea = T)
  
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


#' Comparison of random and real species niche area 
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
  mutate(species = recode(species, "Cyclothone" = "Cyclothone spp.")) %>%
  arrange(species)

# By depth layer
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
  habitat_data <- isotope_data_fish %>%
    filter(habitat == habitat_name)
  
  # Compute observed values for the current habitat
  observed_indices <- niche_area(habitat_data, alea = TRUE)
  
  # Number of bootstrap iterations
  nbBoot <- 10000
  samp <- 10
  
  # Bootstrap values for the current habitat
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
    fill = "gray20",
    alpha = 0.7,
    color = NA
  ) +
  geom_segment(
    data = subset(results_df, type == "observed"),
    aes(
      x = value,
      xend = value,
      y = 0,
      yend = 218000
    ),
    color = "#045171",
    linetype = "longdash",
    size = 0.8,
    alpha = 0.8
  ) +
  labs(x = "Isotopic niche size", y = "Frequency") +
  theme(
    strip.text.x = element_text(
      size = 12.5,
      face = "bold",
      color = "gray20"
    ),
    strip.background = element_rect(fill = "white"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 13)
  )

ggsave("niches_area_model_40.png", path = "figures", dpi = 700, height = 6, width = 9)
  
}

