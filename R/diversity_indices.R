#' diversity_indices
#'
#' @param data1, dataframe with indiv-ID, d13C, d15N and species codes for a site
#' @param data2, species names, species codes, relative biomass and status for a site
#'
#' @return values of trophic diversity indices and plot them
#' @export
#'
#' @examples
#' diversity_indices(individuals_si_Z0492, species_status_biomass_Z0492)

diversity_indices_Z0492 <- function(data1, data2) {

# importing and preparing datasets for analyzes ---- 
# importing stable isotope values (d13C and d15N)
individuals_si <- data1
dim(individuals_si)
summary(individuals_si[,"Species_code"]) # number of individuals per species

# computing mean Stable Isotope values for each species
# "group" column identical to species_code to fit with input format of function meanSI_group
# no "weight" input as number of indivuals sampled per species did not mirror actual species biomass
individuals_si <- data.frame(group=individuals_si[,"Species_code"], individuals_si)
mean_si_species <- meanSI_group(individuals_si)

# computing coefficient of variation within each species to assess intraspecific variability
Coeff_var <- cbind(CV_d13C = mean_si_species[,"sd_d13C"]/mean_si_species[,"d13C"], CV_d15N=mean_si_species[,"sd_d15N"]/mean_si_species[,"d15N"] )
Coeff_var # CV must be <1

#CV in purcentage 
print(Coeff_var*100)

# importing information on species: latin name, code for species name (3 first letters of Genus name),
#relative biomass in %, taxonomic Order and status of species 
species_status_biomass <- data2

# checking that species codes are the same in the two tables
row.names(mean_si_species) == species_status_biomass[,"Species_code"] # OK

# building a single dataframe with all data for computing isotopic diversity indices
data_fish <- data.frame(mean_si_species[,c("d13C","d15N", "sd_d13C","sd_d15N")], rel_Biomass=species_status_biomass[,"rel_Biomass"], Status=species_status_biomass[,"Status"], latin_name=species_status_biomass[,"Species_name"])
data_fish

# scaling mean stable isotopes values using function "scale_rge01"
data_fish_scl <- scaleSI_range01(data_fish)
data_fish_scl

# computing isotopic diversity of the fish assemblage using scaled isotopic values and species relative biomass ---- 
ID_scl_ab <- IDiversity(cons=data_fish_scl, weight=data_fish_scl[,c("rel_Biomass")], nm_plot= "Z0492")
# figure has been saved to your working directory as a jpeg file named "Figure ID"
# printing results
round(Coeff_var,3)
}

diversity_indices_Z0497 <- function(data1, data2) {
  
  # importing and preparing datasets for analyzes ---- 
  # importing stable isotope values (d13C and d15N)
  individuals_si <- data1
  dim(individuals_si)
  summary(individuals_si[,"Species_code"]) # number of individuals per species
  
  # computing mean Stable Isotope values for each species
  # "group" column identical to species_code to fit with input format of function meanSI_group
  # no "weight" input as number of indivuals sampled per species did not mirror actual species biomass
  individuals_si <- data.frame(group=individuals_si[,"Species_code"], individuals_si)
  mean_si_species <- meanSI_group(individuals_si)
  
  # computing coefficient of variation within each species to assess intraspecific variability
  Coeff_var <- cbind(CV_d13C = mean_si_species[,"sd_d13C"]/mean_si_species[,"d13C"], CV_d15N=mean_si_species[,"sd_d15N"]/mean_si_species[,"d15N"] )
  Coeff_var # CV must be <1
  
  #CV in purcentage 
  print(Coeff_var*100)
  
  # importing information on species: latin name, code for species name (3 first letters of Genus name),
  #relative biomass in %, taxonomic Order and status of species 
  species_status_biomass <- data2
  
  # checking that species codes are the same in the two tables
  row.names(mean_si_species) == species_status_biomass[,"Species_code"] # OK
  
  # building a single dataframe with all data for computing isotopic diversity indices
  data_fish <- data.frame(mean_si_species[,c("d13C","d15N", "sd_d13C","sd_d15N")], rel_Biomass=species_status_biomass[,"rel_Biomass"], Status=species_status_biomass[,"Status"], latin_name=species_status_biomass[,"Species_name"])
  data_fish
  
  # scaling mean stable isotopes values using function "scale_rge01"
  data_fish_scl <- scaleSI_range01(data_fish)
  data_fish_scl
  
  # computing isotopic diversity of the fish assemblage using scaled isotopic values and species relative biomass ---- 
  ID_scl_ab <- IDiversity(cons=data_fish_scl, weight=data_fish_scl[,c("rel_Biomass")], nm_plot= "Z0497")
  # figure has been saved to your working directory as a jpeg file named "Figure ID"
  # printing results
  round(Coeff_var,3)
}

diversity_indices_Z0503 <- function(data1, data2) {
  
  # importing and preparing datasets for analyzes ---- 
  # importing stable isotope values (d13C and d15N)
  individuals_si <- data1
  dim(individuals_si)
  summary(individuals_si[,"Species_code"]) # number of individuals per species
  
  # computing mean Stable Isotope values for each species
  # "group" column identical to species_code to fit with input format of function meanSI_group
  # no "weight" input as number of indivuals sampled per species did not mirror actual species biomass
  individuals_si <- data.frame(group=individuals_si[,"Species_code"], individuals_si)
  mean_si_species <- meanSI_group(individuals_si)
  
  # computing coefficient of variation within each species to assess intraspecific variability
  Coeff_var <- cbind(CV_d13C = mean_si_species[,"sd_d13C"]/mean_si_species[,"d13C"], CV_d15N=mean_si_species[,"sd_d15N"]/mean_si_species[,"d15N"] )
  Coeff_var # CV must be <1
  
  #CV in purcentage 
  print(Coeff_var*100)
  
  # importing information on species: latin name, code for species name (3 first letters of Genus name),
  #relative biomass in %, taxonomic Order and status of species 
  species_status_biomass <- data2
  
  # checking that species codes are the same in the two tables
  row.names(mean_si_species) == species_status_biomass[,"Species_code"] # OK
  
  # building a single dataframe with all data for computing isotopic diversity indices
  data_fish <- data.frame(mean_si_species[,c("d13C","d15N", "sd_d13C","sd_d15N")], rel_Biomass=species_status_biomass[,"rel_Biomass"], Status=species_status_biomass[,"Status"], latin_name=species_status_biomass[,"Species_name"])
  data_fish
  
  # scaling mean stable isotopes values using function "scale_rge01"
  data_fish_scl <- scaleSI_range01(data_fish)
  data_fish_scl
  
  # computing isotopic diversity of the fish assemblage using scaled isotopic values and species relative biomass ---- 
  ID_scl_ab <- IDiversity(cons=data_fish_scl, weight=data_fish_scl[,c("rel_Biomass")], nm_plot= "Z0503")
  # figure has been saved to your working directory as a jpeg file named "Figure ID"
  # printing results
  round(Coeff_var,3)
}

diversity_indices_Z0508 <- function(data1, data2) {
  
  # importing and preparing datasets for analyzes ---- 
  # importing stable isotope values (d13C and d15N)
  individuals_si <- data1
  dim(individuals_si)
  summary(individuals_si[,"Species_code"]) # number of individuals per species
  
  # computing mean Stable Isotope values for each species
  # "group" column identical to species_code to fit with input format of function meanSI_group
  # no "weight" input as number of indivuals sampled per species did not mirror actual species biomass
  individuals_si <- data.frame(group=individuals_si[,"Species_code"], individuals_si)
  mean_si_species <- meanSI_group(individuals_si)
  
  # computing coefficient of variation within each species to assess intraspecific variability
  Coeff_var <- cbind(CV_d13C = mean_si_species[,"sd_d13C"]/mean_si_species[,"d13C"], CV_d15N=mean_si_species[,"sd_d15N"]/mean_si_species[,"d15N"] )
  Coeff_var # CV must be <1
  
  #CV in purcentage 
  print(Coeff_var*100)
  
  # importing information on species: latin name, code for species name (3 first letters of Genus name),
  #relative biomass in %, taxonomic Order and status of species 
  species_status_biomass <- data2
  
  # checking that species codes are the same in the two tables
  row.names(mean_si_species) == species_status_biomass[,"Species_code"] # OK
  
  # building a single dataframe with all data for computing isotopic diversity indices
  data_fish <- data.frame(mean_si_species[,c("d13C","d15N", "sd_d13C","sd_d15N")], rel_Biomass=species_status_biomass[,"rel_Biomass"], Status=species_status_biomass[,"Status"], latin_name=species_status_biomass[,"Species_name"])
  data_fish
  
  # scaling mean stable isotopes values using function "scale_rge01"
  data_fish_scl <- scaleSI_range01(data_fish)
  data_fish_scl
  
  # computing isotopic diversity of the fish assemblage using scaled isotopic values and species relative biomass ---- 
  ID_scl_ab <- IDiversity(cons=data_fish_scl, weight=data_fish_scl[,c("rel_Biomass")], nm_plot= "Z0508")
  # figure has been saved to your working directory as a jpeg file named "Figure ID"
  # printing results
  round(Coeff_var,3)
}

diversity_indices_Z0512 <- function(data1, data2) {
  
  # importing and preparing datasets for analyzes ---- 
  # importing stable isotope values (d13C and d15N)
  individuals_si <- data1
  dim(individuals_si)
  summary(individuals_si[,"Species_code"]) # number of individuals per species
  
  # computing mean Stable Isotope values for each species
  # "group" column identical to species_code to fit with input format of function meanSI_group
  # no "weight" input as number of indivuals sampled per species did not mirror actual species biomass
  individuals_si <- data.frame(group=individuals_si[,"Species_code"], individuals_si)
  mean_si_species <- meanSI_group(individuals_si)
  
  # computing coefficient of variation within each species to assess intraspecific variability
  Coeff_var <- cbind(CV_d13C = mean_si_species[,"sd_d13C"]/mean_si_species[,"d13C"], CV_d15N=mean_si_species[,"sd_d15N"]/mean_si_species[,"d15N"] )
  Coeff_var # CV must be <1
  
  #CV in purcentage 
  print(Coeff_var*100)
  
  # importing information on species: latin name, code for species name (3 first letters of Genus name),
  #relative biomass in %, taxonomic Order and status of species 
  species_status_biomass <- data2
  
  # checking that species codes are the same in the two tables
  row.names(mean_si_species) == species_status_biomass[,"Species_code"] # OK
  
  # building a single dataframe with all data for computing isotopic diversity indices
  data_fish <- data.frame(mean_si_species[,c("d13C","d15N", "sd_d13C","sd_d15N")], rel_Biomass=species_status_biomass[,"rel_Biomass"], Status=species_status_biomass[,"Status"], latin_name=species_status_biomass[,"Species_name"])
  data_fish
  
  # scaling mean stable isotopes values using function "scale_rge01"
  data_fish_scl <- scaleSI_range01(data_fish)
  data_fish_scl
  
  # computing isotopic diversity of the fish assemblage using scaled isotopic values and species relative biomass ---- 
  ID_scl_ab <- IDiversity(cons=data_fish_scl, weight=data_fish_scl[,c("rel_Biomass")], nm_plot= "Z0512")
  # figure has been saved to your working directory as a jpeg file named "Figure ID"
  # printing results
  round(Coeff_var,3)
}

diversity_indices_Z0518 <- function(data1, data2) {
  
  # importing and preparing datasets for analyzes ---- 
  # importing stable isotope values (d13C and d15N)
  individuals_si <- data1
  dim(individuals_si)
  summary(individuals_si[,"Species_code"]) # number of individuals per species
  
  # computing mean Stable Isotope values for each species
  # "group" column identical to species_code to fit with input format of function meanSI_group
  # no "weight" input as number of indivuals sampled per species did not mirror actual species biomass
  individuals_si <- data.frame(group=individuals_si[,"Species_code"], individuals_si)
  mean_si_species <- meanSI_group(individuals_si)
  
  # computing coefficient of variation within each species to assess intraspecific variability
  Coeff_var <- cbind(CV_d13C = mean_si_species[,"sd_d13C"]/mean_si_species[,"d13C"], CV_d15N=mean_si_species[,"sd_d15N"]/mean_si_species[,"d15N"] )
  Coeff_var # CV must be <1
  
  #CV in purcentage 
  print(Coeff_var*100)
  
  # importing information on species: latin name, code for species name (3 first letters of Genus name),
  #relative biomass in %, taxonomic Order and status of species 
  species_status_biomass <- data2
  
  # checking that species codes are the same in the two tables
  row.names(mean_si_species) == species_status_biomass[,"Species_code"] # OK
  
  # building a single dataframe with all data for computing isotopic diversity indices
  data_fish <- data.frame(mean_si_species[,c("d13C","d15N", "sd_d13C","sd_d15N")], rel_Biomass=species_status_biomass[,"rel_Biomass"], Status=species_status_biomass[,"Status"], latin_name=species_status_biomass[,"Species_name"])
  data_fish
  
  # scaling mean stable isotopes values using function "scale_rge01"
  data_fish_scl <- scaleSI_range01(data_fish)
  data_fish_scl
  
  # computing isotopic diversity of the fish assemblage using scaled isotopic values and species relative biomass ---- 
  ID_scl_ab <- IDiversity(cons=data_fish_scl, weight=data_fish_scl[,c("rel_Biomass")], nm_plot= "Z0518")
  # figure has been saved to your working directory as a jpeg file named "Figure ID"
  # printing results
  round(Coeff_var,3)
}

diversity_indices_Z0524 <- function(data1, data2) {
  
  # importing and preparing datasets for analyzes ---- 
  # importing stable isotope values (d13C and d15N)
  individuals_si <- data1
  dim(individuals_si)
  summary(individuals_si[,"Species_code"]) # number of individuals per species
  
  # computing mean Stable Isotope values for each species
  # "group" column identical to species_code to fit with input format of function meanSI_group
  # no "weight" input as number of indivuals sampled per species did not mirror actual species biomass
  individuals_si <- data.frame(group=individuals_si[,"Species_code"], individuals_si)
  mean_si_species <- meanSI_group(individuals_si)
  
  # computing coefficient of variation within each species to assess intraspecific variability
  Coeff_var <- cbind(CV_d13C = mean_si_species[,"sd_d13C"]/mean_si_species[,"d13C"], CV_d15N=mean_si_species[,"sd_d15N"]/mean_si_species[,"d15N"] )
  Coeff_var # CV must be <1
  
  #CV in purcentage 
  print(Coeff_var*100)
  
  # importing information on species: latin name, code for species name (3 first letters of Genus name),
  #relative biomass in %, taxonomic Order and status of species 
  species_status_biomass <- data2
  
  # checking that species codes are the same in the two tables
  row.names(mean_si_species) == species_status_biomass[,"Species_code"] # OK
  
  # building a single dataframe with all data for computing isotopic diversity indices
  data_fish <- data.frame(mean_si_species[,c("d13C","d15N", "sd_d13C","sd_d15N")], rel_Biomass=species_status_biomass[,"rel_Biomass"], Status=species_status_biomass[,"Status"], latin_name=species_status_biomass[,"Species_name"])
  data_fish
  
  # scaling mean stable isotopes values using function "scale_rge01"
  data_fish_scl <- scaleSI_range01(data_fish)
  data_fish_scl
  
  # computing isotopic diversity of the fish assemblage using scaled isotopic values and species relative biomass ---- 
  ID_scl_ab <- IDiversity(cons=data_fish_scl, weight=data_fish_scl[,c("rel_Biomass")], nm_plot= "Z0524")
  # figure has been saved to your working directory as a jpeg file named "Figure ID"
  # printing results
  round(Coeff_var,3)
}

