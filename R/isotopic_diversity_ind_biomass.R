#' Compute isotopic diversity indices weighted by biomass
#'
#'@description
#' This function computes isotopic diversity indices based on stable isotope values according to the si_div script of
#' Sébastien Villéger (article article by Julien Cucherousset & Sébastien Villéger, 2015)
#'
#' @param habitat_data isotope data (colnames : species, d15N, d13C, habitat)
#' @param sp_biomass_habitat relative biomass of species in a habitat
#' @param alea if =T, recover the coordinates of the convex hull
#'
#' @return
#' @export
#'
#' @examples

isotopic_diversity_ind_biomass <- function(habitat_data, sp_biomass_habitat, alea) {
  # preparation of the files
  habitat_data <- data.frame(na.omit(habitat_data))
  SpNames <- habitat_data$species
  habitat_data$habitat <- unique(habitat_data$habitat)
  d13C <- habitat_data$d13C
  d15N <- habitat_data$d15N
  X <- habitat_data$d13C
  Y <- habitat_data$d15N
  PID <- 1:nrow(habitat_data)
  xydata <- PBSmapping::as.PolyData(cbind(PID, X, Y), projection = NULL, zone = NULL)
  xyCoord <- sp::coordinates(cbind(X, Y))
  
  # charge si_div function 
  source("R/si_div.R")
  
  # computing mean Stable Isotope values for each species to match with si_div function 
  data_fish <- habitat_data%>%
    group_by(species) %>%
    mutate(
      sd_d13C = sd(d13C),
      sd_d15N = sd(d15N),
      d13C = mean(d13C),
      d15N = mean(d15N)) %>%
    select(species, d13C, d15N, sd_d13C, sd_d15N, habitat) %>%
    distinct() %>%
    as.data.frame()%>%
    mutate(rel_biomass = sp_biomass_habitat$rel_biomass[match(species, sp_biomass_habitat$species)])%>%
    rename(Status=habitat,
           Species_names=species)
  
  # scaling mean stable isotopes values using function "scale_rge01"
  data_fish_scl <- scaleSI_range01(data_fish)
  
  # computing isotopic diversity of the whole fish assemblage using scaled isotopic values with species relative biomass
  ID_scl_ab <- IDiversity(cons = data_fish_scl, weight = data_fish_scl [, c("rel_biomass")])
  ID_scl_ab_df <- as.data.frame(ID_scl_ab)
  
  # Isotopic diversity indices
  IDiv <- ID_scl_ab_df[10,]
  IDis <- ID_scl_ab_df[11,]
  IEve <- ID_scl_ab_df[12,]
  IUni <- ID_scl_ab_df[13,]
  
  # Standard Ellipses Areas
  # sea <- rKIN::estEllipse(data= habitat_data, x="d13C", y="d15N", group="habitat", levels=40, smallSamp = TRUE)
  # ELP <- sea$estObj[1:2,c('ShapeArea','Group')][1,][[1]]
  
  if (alea == F) {
    lt <- list(
      IDiv = IDiv,
      IDis = IDis,
      IEve = IEve,
      IUni = IUni
    )#, ELP=ELP
  } else {
    lt <-
      list(
        IDiv = IDiv,
        IDis = IDis,
        IEve = IEve,
        IUni = IUni,
        ConvHull = PBSmapping::calcConvexHull(xydata)
      )  #, ELP=ELP
  }
  return(lt)
} 