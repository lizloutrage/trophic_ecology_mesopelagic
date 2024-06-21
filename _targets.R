#' Targets plan
#'
#'@author Liz Loutrage, \email{liz.loutrage@gmail.com}
#'        Anik Brind'amour, \email {Anik.Brindamour@ifremer.fr}
#'

## Attach required packages ----
library(targets)
targets::tar_option_set(packages = c("dplyr", "ggplot2"))

# load functions
targets::tar_source(files = "R")

list(
  # 1. Isotopic niches ----
  # Load isotope data (DOI:)
  tar_target(
    isotope_data_fish,
    readxl::read_excel(
      here::here(
        "data",
        "C_N_isotopes_deep_pelagic_fish_BayofBiscay_2021.xlsx"
      ),
      3
    ) %>%
      # Rename columns for simplicity
      rename(
        trawling_depth = `Trawling depth [m]`,
        d15n = `δ15N (untreated)`,
        species = Taxa,
        station = `Station label`
      ) %>%
      # creation of a d13c column (see mat & met), species with a C:N ratio:
      # >5: have undergone delipidation,
      # between 3.5 and 5: mathematically corrected (following Post et al. 2007)
      # < 3.5: no corrected
      mutate(
        d13c = case_when(
          !is.na(`δ13C (cyclohexane-delipidated)`) ~ `δ13C (cyclohexane-delipidated)`,
          `CN (untreated)` < 3.5 ~ `δ13C (untreated)`,
          TRUE ~ `δ13C (untreated)` - 3.32 + 0.99 * `CN (untreated)`
        )
      )
  ),
  
  # plot the isotopic niche of each species (all depth pooled)
  tar_target(species_niche, niche_plot_community(isotope_data_fish)),
  
  # calculation of the matrix niche overlap
  tar_target(overlap_mx, overlap_matrix(isotope_data_fish)),
  
  # plot matrix niche overlap
  tar_target(plot_matrix, overlap_matrix_plot(overlap_mx)),
  
  # 2. Depth segregation----
  # Load trawling data
  tar_target(
    trawling_data,
    readxl::read_excel(
      here::here(
        "data",
        "biomass_abundance_deep_pelagic_fish_Bay_Biscay.xlsx"
      ),
      3
    ) %>%
      # rename columns for simplicity
      rename(
        trawling_depth = `Trawling depth [m]`,
        station = `Station label`,
        biomass_sp = `Trawl sum biomass [kg]`,
        species = Species
      ) %>%
      # select data of interest
      filter(Year == "2021") %>%
      select(trawling_depth, station, biomass_sp, species) %>%
      distinct() %>%
      mutate(biomass_sp = as.numeric(biomass_sp))
  ),
  
  # Calculation of density depth distribution by species sampled
  tar_target(
    density_distribution_data,
    density_distrubtion(trawling_data)
  ),
  
  # median depth of each species
  tar_target(median_depth, median_depth_sp(density_distribution_data)),
  
  # define number of cluster (trophic guilds)
  tar_target(nb_cluster_gs, nb_cluster(overlap_mx)),
  
  # define the cluster (which species belongs to which cluster)
  tar_target(cluster_definition, k_means_cluster(overlap_mx)),
  
  # plot cluster (dendrogram)
  tar_target(dendrogram, plot_densdrogram(overlap_mx)),
  
  # plot depth distribution
  tar_target(
    depth_distribution,
    depth_distribution_plot(density_distribution_data, cluster_definition)
  ),
  
  # 3. Isotopic diversity indices ----
  
  # Data formatting to match si_div function parameters
  # Species code
  tar_target(
    Species_code,
    utils::read.csv(
      here::here("data", "species_code.csv"),
      sep = ";",
      header = T,
      dec = "."
    ) %>%
      mutate(Species_code = tolower(Species_code))
  ),
  
  # species relative biomass in each depth layer
  tar_target(
    species_status_biomass,
    sp_status_biomass_format(trawling_data, Species_code)
  ),
  
  # individuals stable isotopes values
  tar_target(
    individuals_si,
    individuals_si_format(isotope_data_fish, species_status_biomass, Species_code)
  ),
  
  # Isotopic diversity index calculation
  tar_target(
    diversity_index,
    diversity_index_calculation (individuals_si, species_status_biomass)
  ),
  
  # PCA - isotopic diversity indices
  tar_target(PCA, compute_PCA(diversity_index)),
  
  # 4.Null models ----
  
  # Niche species area
  tar_target(niche_area_sp, null_model_niche_area(isotope_data_fish)),
  
  # Sum of species niche overlap
  tar_target(overlap_sp, null_model_overlap(isotope_data_fish)),
  
  # 5. Appendix ----
  tar_target(niche_cluster, plot_niche_cluster(isotope_data_fish))
)
