#' Depth distribution form trawling data
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples

density_distrubtion <- function(data) {
  # density distribution from biomass value
  density_distribution <- data %>%
    #selection of species sampled for stable isotope analysis for this study
    filter(
      species %in% c(
        "Arctozenus risso",
        "Argyropelecus olfersii",
        "Benthosema glaciale",
        "Cyclothone",
        "Lampanyctus crocodilus",
        "Lampanyctus macdonaldi",
        "Lestidiops sphyrenoides",
        "Maulisia argipalla",
        "Maurolicus muelleri",
        "Melanostigma atlanticum",
        "Myctophum punctatum",
        "Notoscopelus bolini",
        "Notoscopelus kroyeri",
        "Searsia koefoedi",
        "Serrivomer beanii",
        "Xenodermichthys copei"
      )
    ) %>%
    mutate(species = recode(species, "Cyclothone" = "Cyclothone spp.")) %>%
    group_by(species) %>%
    # sum total biomass by species (all depth)
    mutate(sum_biomass_sp = sum(biomass_sp)) %>%
    ungroup() %>%
    group_by(trawling_depth, species) %>%
    # % of biomass by depth layer by species 
    mutate(pourcentage_bio = sum(biomass_sp / sum_biomass_sp * 100)) %>%
    # Selection of trawling depth
    select(species, trawling_depth, pourcentage_bio) %>%
    # to have a round number to be able to multiply it afterwards
    mutate(across(pourcentage_bio, round, 0)) %>%
    mutate(n_bio = as.integer(pourcentage_bio)) %>%
    select(-c(pourcentage_bio)) %>%
    tidyr::uncount(n_bio)%>% 
    ungroup() %>% 
    as.data.frame() %>% 
    mutate(species = stringr::str_trim(species)) %>%
    mutate(species = as.character(species))
  
}

#'  calculate median depth by species with trawling data set 2021
#'
#' @param data : density depth distribution
#'
#' @return
#' @export
#'
#' @examples

median_depth_sp <- function(data) {
  data %>%
    group_by(species) %>%
    mutate(median_depth = median(trawling_depth)) %>%
    select(species, median_depth) %>%
    distinct() %>%
    ungroup() %>%
    arrange(species)
}
