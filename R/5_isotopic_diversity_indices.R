#' format trawling data to implement in si_div function
#'
#' @param data : trawling data with sp biomass by depth layer and species codes
#'
#' @return : dataframe with match the format for si_div function

sp_status_biomass_format <- function(trawling_data, Species_code) {
  species_status_biomass <- trawling_data %>%
    # group by depth layer (Loutrage et al., 2023)
    mutate(
      Status = case_when(
        station %in% c("Z0508") ~ "epipelagic",
        station %in% c("Z0492", "Z0512") ~ "upper-mesopelagic",
        station %in% c("Z0503", "Z0518") ~ "lower-mesopelagic",
        station %in% c("Z0497") ~ "bathypelagic",
        station %in% c("Z0524") ~ "bottom-proximity"
      )
    ) %>%
    # Calculate proportion of biomass of sampled species  
    # selection of species sampled for isotopy in each depth
    filter(
      Status == "epipelagic" &
        species %in% c(
          "Lestidiops sphyrenoides",
          "Maurolicus muelleri",
          "Myctophum punctatum",
          "Notoscopelus kroyeri",
          "Notoscopelus bolini"
        ) |
        Status == "upper-mesopelagic" &
        species %in% c(
          "Arctozenus risso",
          "Argyropelecus olfersii",
          "Lampanyctus crocodilus",
          "Myctophum punctatum",
          "Notoscopelus kroyeri",
          "Xenodermichthys copei"
        ) |
        Status == "lower-mesopelagic" &
        species %in% c(
          "Arctozenus risso",
          "Argyropelecus olfersii",
          "Cyclothone",
          "Benthosema glaciale",
          "Lampanyctus crocodilus",
          "Maulisia argipalla",
          "Searsia koefoedi",
          "Serrivomer beanii",
          "Xenodermichthys copei"
        ) |
        Status == "bathypelagic" &
        species %in% c(
          "Argyropelecus olfersii",
          "Lampanyctus crocodilus",
          "Lampanyctus macdonaldi",
          "Myctophum punctatum",
          "Serrivomer beanii",
          "Xenodermichthys copei"
        ) |
        Status == "bottom-proximity" &
        species %in% c(
          "Argyropelecus olfersii",
          "Lampanyctus crocodilus",
          "Melanostigma atlanticum",
          "Serrivomer beanii",
          "Xenodermichthys copei"
        )
    ) %>%
    inner_join(Species_code) %>% 
    # create code for each species 
    select(Status,
           biomass_sp,
           species,
           Species_code) %>%
    distinct() %>%
    # sum species biomass by depth layer
    group_by(species, Status) %>%
    mutate(biomass_sp_depth = sum(biomass_sp)) %>%
    select(-biomass_sp) %>%
    distinct() %>%
    # sum of total biomass by depth layer
    group_by(Status) %>%
    mutate(biomass_tot = sum(biomass_sp_depth)) %>%
    # relative biomass of each species by station
    mutate(rel_biomass = biomass_sp_depth / biomass_tot * 100) %>%
    select(-c(biomass_sp_depth, biomass_tot)) %>%
    rename(Species_name = species) %>%
    relocate(Status, .after = rel_biomass) %>%
    relocate(Species_code, .after = Species_name) %>%
    arrange(Species_code) %>%
    distinct()
}

# Format the data to match the parameters of the si_div function ----

#' format isotope data to match si_div function parameters 
#'
#' @param data 
#'
#' @return a data frame that can be used in the si_div function
#'

individuals_si_format <- function(isotope_data_fish, species_status_biomass, Species_code) {
  
    # sourcing the R functions from 'si_div' R script
    source("R/si_div.R")
  
  # Format indiviudal_si to match si_div function ----
  individuals_si <- isotope_data_fish%>%
    dplyr::select(species, station, d13c, d15n) %>%
    inner_join(Species_code) %>%
    rename(d13C = d13c,
           d15N = d15n,
           Species_name = species) %>%
    group_by(Species_name) %>%
    # add a unique code for each individual
    mutate(indiv_ID = paste(Species_name, row_number(), sep = "_")) %>%
    rename(group=Species_name) %>% 
    ungroup() %>% 
    relocate(indiv_ID, .before = d13C) %>% 
    arrange(Species_code)%>%
    # group by depth layer 
    mutate(
      Status = case_when(
        station %in% c("Z0508") ~ "epipelagic",
        station %in% c("Z0492", "Z0512") ~ "upper-mesopelagic",
        station %in% c("Z0503", "Z0518") ~ "lower-mesopelagic",
        station %in% c("Z0497") ~ "bathypelagic",
        station %in% c("Z0524") ~ "bottom-proximity"
      )
    ) %>% 
    dplyr::select(-station)
}

# index calculation ----

#' Isotopic diversity index calculation
#'
#' @param individuals_si dataframe obtain with the individuals_si_format function 
#' @param status_biomass dataframe obtain with the sp_status_biomass_format function 
#'
#' @return  isotopic diversity index and representation of these indices 
#'
#' @examples diversity_index_calculation (individuals_si, species_status_biomass)


diversity_index_calculation <-
  function(individuals_si, species_status_biomass) {
    depth_layers <-
      c(
        "epipelagic",
        "upper-mesopelagic",
        "lower-mesopelagic",
        "bathypelagic",
        "bottom-proximity"
      )
    
    # Initialize an empty list to store results for each depth layer
    result_list <- list()
    
    # loop for each depth layers
    for (depth_layer in depth_layers) {
      individuals_si_filtered <- individuals_si %>%
        filter(Status == depth_layer) %>%
        select(-Status)
      
      species_status_biomass_filtered <-
        species_status_biomass %>%
        filter(Status == depth_layer)
      
      # computing mean Stable Isotope values for each species
      # "group" column identical to species_code to fit with input format of function meanSI_group
      # no "weight" input as number of individuals sampled per species did not mirror actual species biomass
      individuals_si_depth <-individuals_si_filtered %>% 
        as.data.frame() %>% 
        mutate(group = Species_code)
      
      mean_si_species <- meanSI_group(individuals_si_depth)
      
      # computing coefficent of variation within each species to assess intraspecific variability
      cbind(CV_d13C = mean_si_species[, "sd_d13C"] / mean_si_species[, "d13C"],
            CV_d15N = mean_si_species[, "sd_d15N"] / mean_si_species[, "d15N"])
      # -> intraspecific variability is overall low (<20%)
      
      # checking that species codes are the same in the two tables
      row.names(mean_si_species) == species_status_biomass_filtered[, "Species_code"] # OK
      
      # building a single dataframe with all data for computing isotopic diversity indices
      data_fish <- data.frame(
        mean_si_species[, c("d13C", "d15N", "sd_d13C", "sd_d15N")],
        rel_biomass = species_status_biomass_filtered[, "rel_biomass"],
        Status = species_status_biomass_filtered[, "Status"],
        latin_name = species_status_biomass_filtered[, "Species_name"]
      )
      
      # scaling mean stable isotopes values using function "scale_rge01"
      data_fish_scl <- scaleSI_range01(data_fish)
      
      # computing isotopic diversity of the whole fish assemblage using scaled isotopic values and species relative biomass
      ID_scl_ab <- IDiversity(
        cons = data_fish_scl,
        weight = data_fish_scl[, c("rel_biomass")],
        nm_plot = paste(as.character(seq_along(depth_layers)[depth_layers == depth_layer]), depth_layer, sep = "_")
      )
      
      # Extract the index names dynamically from the result of ID_scl_ab
      index_names <- names(ID_scl_ab)
      
      # Create a data frame for the current depth layer and append it to the result list
      current_result <- data.frame(index = index_names, Depth_Layer = depth_layer, ID_scl_ab = ID_scl_ab)
      result_list[[depth_layer]] <- current_result
    }
    
    # Combine all data frames from the result list into a single data frame
    ID_scl_ab_df <- do.call(rbind, result_list) 
    rownames(ID_scl_ab_df) <- NULL
    
    # Print the final data frame
    print(ID_scl_ab_df)
    return(ID_scl_ab_df)
  }
    
# PCA ----

compute_PCA <- function(diversity_index){
  
  isotopic_indices <- diversity_index %>% 
    filter(index%in% c("IDis", "IDiv", "IUni", "IEve")) %>% 
    tidyr::pivot_wider(names_from = "index", values_from = "ID_scl_ab") %>% 
    tibble::column_to_rownames(var = "Depth_Layer")
  
  res.pca <- FactoMineR::PCA(isotopic_indices, graph = FALSE)
  
  factoextra::fviz_pca_biplot(res.pca, repel = TRUE,
                              col.var = "#00778E", 
                              col.ind = "gray50", 
                              arrowsize = 1,
                              title = "",
                              
  )
  
  ggsave("PCA.png", path = "figures", dpi = 700, height = 5, width = 7)
}
