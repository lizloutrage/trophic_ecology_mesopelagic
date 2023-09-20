#' Weighted bootstrap indices
#'
#'@description
#'Bootstrap function for indices weighted by the relative biomass of species 
#'(inspired by Brind Amour & Dubois 2013 and Suchomel et al. 2022)
#'
#' @param habitat_data isotope data (colnames : species, d15N, d13C, habitat)
#' @param samp number of individuals sampled
#' @param nbBoot number of iterations
#' @param sp_biomass_habitat relative biomass of species in a habitat
#'
#' @return
#' @export
#'
#' @examples

isotopic_diversity_boot_biomass <- function(habitat_data, samp, nbBoot, sp_biomass_habitat) {
  
  # Ensure habitat_data contains required columns and remove NA rows
  habitat_data <- data.frame(na.omit(habitat_data))
  SpNames <- habitat_data$species
  hbnames<- unique(habitat_data$habitat)
  d13C <- habitat_data$d13C
  d15N <- habitat_data$d15N
  X <- habitat_data$d13C
  Y <- habitat_data$d15N
  PID <- 1:nrow(habitat_data)
  xydata <- PBSmapping::as.PolyData(cbind(PID, X, Y), projection = NULL, zone = NULL)
  xyCoord <- sp::coordinates(cbind(X, Y))
  
  # Get unique habitat names
  #habNames <- unique(habitat_data$habitat)
  
  # Initialize lists to store bootstrap results
  boot_iso <- list()
  Ind <- list()
  
  for (i in 1:nbBoot) {
    set.seed(i)
    
    # Sample 'samp' species based on habitat-specific biomass probabilities
    sampled_species_habitat <- sample(
      unique(habitat_data$species), 
      size = samp, 
      replace = TRUE, 
      prob = sp_biomass_habitat$rel_biomass[match(unique(habitat_data$species), sp_biomass_habitat$species)])
    
    # Filter the data for the sampled species within the habitat
    sampled_data_habitat <- habitat_data[
      habitat_data$species %in% sampled_species_habitat , ]
    
    # Compute individual-level weights based on species biomass in the habitat
    weights <- sp_biomass_habitat$rel_biomass[match(sampled_data_habitat$species, sp_biomass_habitat$species)]
    
    # Sample 'samp' individuals from the filtered data
    sampled_data_individuals <- sampled_data_habitat[sample(nrow(sampled_data_habitat), samp, replace = TRUE), ]
    
    # Compute isotopic diversity indices for the sampled data
    Ind[[i]] <- isotopic_diversity_ind_biomass(sampled_data_individuals, sp_biomass_habitat, alea = F)
    
    # Store the sampled data for reference
    boot_iso[[i]] <- sampled_data_individuals
  }
  
  # Create a data frame from the list of indices
  res <- data.frame(matrix(unlist(Ind), ncol = length(Ind[[1]])))
  colnames(res) <- c("IDiv", "IDis", "IEve", "IUni")
  
  # Add a column for habitat names
  res$habitat <- rep(hbnames, nbBoot)
  
  return(res)
}