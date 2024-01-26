#' Depth distribution form trawling data 
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples

density_distrubtion <- function(data){
  
  # density distribution from biomass value
 density_distribution <- data %>%
  select(Nom_Scientifique, Tot_V_HV, Code_Station) %>%
  #selection of pelagic trawling
  filter(Code_Station %in% c("Z0524", "Z0518", "Z0512", "Z0508",
                             "Z0503", "Z0497", "Z0492")) %>%
  #selection of species sampled for isotope
  filter(
    Nom_Scientifique %in% c(
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
  mutate(Nom_Scientifique = recode(Nom_Scientifique, "Cyclothone" = "Cyclothone spp.")) %>%
  mutate(
    trawling_depth = case_when(
      Code_Station %in% c("Z0508") ~ 25,
      Code_Station %in% c("Z0492") ~ 370,
      Code_Station %in% c("Z0512") ~ 555,
      Code_Station %in% c("Z0503") ~ 715,
      Code_Station %in% c("Z0518") ~ 1000,
      Code_Station %in% c("Z0524") ~ 1010,
      Code_Station %in% c("Z0497") ~ 1335
    )
  ) %>%
  distinct() %>%
  group_by(Nom_Scientifique) %>%
  mutate(sum_sp = sum(Tot_V_HV)) %>%
  ungroup() %>%
  group_by(trawling_depth, Nom_Scientifique) %>%
  mutate(pourcentage_bio = sum(Tot_V_HV / sum_sp * 100)) %>%
  # Selection of trawling depth
  select(Nom_Scientifique, trawling_depth, pourcentage_bio) %>%
  # to have a round number to be able to multiply it afterwards
  mutate(across(pourcentage_bio, round, 0)) %>%
  mutate(n_bio = as.integer(pourcentage_bio)) %>%
  select(-c(pourcentage_bio)) %>%
  tidyr::uncount(n_bio)

}

#'  calculate median depth by species with trawling data set 2021
#'
#' @param data : density depth distribution
#'
#' @return
#' @export
#'
#' @examples
median_depth_sp <- function(data){
  data %>%
  group_by(Nom_Scientifique) %>%
  mutate(median_depth = median(trawling_depth)) %>%
  select(Nom_Scientifique, median_depth) %>%
  distinct() %>%
  ungroup() %>%
  arrange(Nom_Scientifique) %>%
  rename(species = Nom_Scientifique)
}
  

