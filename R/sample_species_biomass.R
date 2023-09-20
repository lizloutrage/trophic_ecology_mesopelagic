#' Randomly sample species based on their relative biomass values
#'
#' @param sp_biomass_habitat relative biomass of species in a habitat
#' @param samp number of samples
#'
#' @return  a subset of the original data set containing the sampled species based on their relative biomass
#'
#' @examples

sample_species_biomass <- function(sp_biomass_habitat, samp) {
  
  sampled_indices <- sample(
    1:nrow(sp_biomass_habitat),
    size = samp,
    replace = TRUE,
    # provides the probability weights for each row in the dataset
    prob = sp_biomass_habitat$rel_Biomass)
  
  # Select sampled species
  sampled_data <- sp_biomass_habitat[sampled_indices,]
  
  return(sampled_data)
}