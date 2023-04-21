#######################################################################################################################
#######################################################################################################################
# EXAMPLES OF HOW USING 'si_div.R' script 
# data used here come from a real study case (Lavernose lake, France, unpublished data)
# authors: S. Villéger and J. Cucherousset
#######################################################################################################################
#######################################################################################################################

# removing objects from R memory
rm(list=ls()) 


# setting working directory (a folder where objects to import are and where the outputs will be saved)
setwd("/Volumes/Data/isotopic diversity/example")

# sourcing the R functions from 'si_div' R script
source("si_div.R") # NB: several R packages are autmoatically loaded

#######################################################################################################################
#######################################################################################################################

# importing and preparing datasets for analyzes

##############################################################
# importing stable isotope values (d13C and d15N) for 97 individuals fish (different sampling size between species)
individuals_si<-read.csv("individuals_si.csv", sep=";")
dim(individuals_si)
summary(individuals_si[,"Species_code"]) # number of individuals per species

# computing mean Stable Isotope values for each species
# "group" column identical to species_code to fit with input format of function meanSI_group
# no "weight" input as number of indivuals sampled per species did not mirror actual species biomass
individuals_si<-data.frame(group=individuals_si[,"Species_code"], individuals_si)
mean_si_species<-meanSI_group(individuals_si)

# computing coefficent of variation within each species to assess intraspecific variability
cbind( CV_d13C=mean_si_species[,"sd_d13C"]/mean_si_species[,"d13C"], CV_d15N=mean_si_species[,"sd_d15N"]/mean_si_species[,"d15N"] )
# -> intraspecific variability is overall low (<20%)
##############################################################
# importing information on species: latin name, code for species name (3 firt letters of Genus name), relative biomass in %, taxonomic Order and status of species ( Native=N or  Non-native=E)
species_status_biomass<-read.csv("species_status_biomass.csv", sep=";")
species_status_biomass   # 14 fish species including 6 Native and 8 Exotic species

# checking that species codes are the same in the two tables
row.names(mean_si_species)==species_status_biomass[,"Species_code"] # OK

# building a single dataframe with all data for computing isotopic diversity indices
data_fish<-data.frame(mean_si_species[,c("d13C","d15N", "sd_d13C","sd_d15N")], rel_Biomass=species_status_biomass[,"rel_Biomass"], Status=species_status_biomass[,"Status"], latin_name=species_status_biomass[,"Species_name"])
data_fish

##############################################################
# scaling mean stable isotopes values using function "scale_rge01"
data_fish_scl<-scaleSI_range01(data_fish)
data_fish_scl

#######################################################################################################################
#######################################################################################################################
# computing isotopic diversity of the whole fish assemblage using scaled isotopic values and species relative biomass
ID_scl_ab<-IDiversity(cons=data_fish_scl, weight=data_fish_scl[,c("rel_Biomass")], nm_plot="Figure ID")
# figure has been saved to your working directory as a jpeg file named "Figure ID"
# printing results
round(ID_scl_ab,3)

# computing isotopic diversity of only native species and of only non-native species using scaled isotopic values and species relative biomass
# standard deviation of stable istope mean values are not given in the inputs, and are thus not plotted for graphical convenience
IDnat_scl_ab<-IDiversity(cons=data_fish_scl[which( data_fish_scl[,"Status"]=="N"),c("d13C","d15N")], weight=data_fish_scl[which( data_fish_scl[,"Status"]=="N"),c("rel_Biomass")], nm_plot="Figure ID_native", scaled=T)
IDnonnat_scl_ab<-IDiversity(cons=data_fish_scl[which( data_fish_scl[,"Status"]=="E"),c("d13C","d15N")], weight=data_fish_scl[which( data_fish_scl[,"Status"]=="E"),c("rel_Biomass")], nm_plot="Figure ID_nonnative", scaled=T)

# as data have been scaled at the assemblage level, range on axes of each plot is higher than the actual range filled by native or non-native species
# Such a scaling at the whole assemblage level allows comparing Isotopic Diversity of native and non-native subsets of species

# printing results
round( cbind(Native=IDnat_scl_ab, Non_native=IDnonnat_scl_ab) , 3) # Non native species fill a higher portion of the isotopic space (range, IRic), with a lower regularity (IEve) and a higher redundancy (lower IUni)

#######################################################################################################################
# computing isotopic overlap between native and exotic species using scaled isotopic values
# standard deviation of stable isotope mean values are not given in the inputs, and are thus not plotted for graphical convenience
IO_natvsexo_scl<-IOverlap(cons1=data_fish_scl[which( data_fish_scl[,"Status"]=="N"),c("d13C","d15N")], 
  cons2=data_fish_scl[which( data_fish_scl[,"Status"]=="E"),c("d13C","d15N")], nm_plot="Figure IO")
# figure has been saved to your working directory as a jpeg file named "Figure IO"

# printing Isotopic similarity and nestedness on each axis and in the 2D space
round(IO_natvsexo_scl,3)

#######################################################################################################################
#######################################################################################################################

# Example of how computing indices at the species level based on individuals isotopic values

# we want to compare the isotopic diversity of an exotic species Lepomis gibbosus (LEP, pumpkinseed) and a native species Perca fluviatilis (PER, European perch) and to measure the isotopic similarity between these two species

# we first extract the individuals of these 2 species from the database
data_ind_LEP_PER<-individuals_si[which(individuals_si[,"group"]%in%c("LEP","PER")),]
summary(data_ind_LEP_PER) # 27 individuals of LEP and only 10 individuals of PER

# scaling stable isotopes values using function "scale_rge01"
data_ind_LEP_PER_scl<-scaleSI_range01(data_ind_LEP_PER)
summary(data_ind_LEP_PER_scl)

# computing isotopic diversity for each species based on individuals values (no weighting by fish mass)
IDLEP_scl_ind<-IDiversity(cons=data_ind_LEP_PER_scl[which( data_ind_LEP_PER_scl[,"group"]=="LEP"),c("d13C","d15N")], nm_plot="Figure ID_LEP", scaled=T)
IDPER_scl_ind<-IDiversity(cons=data_ind_LEP_PER_scl[which( data_ind_LEP_PER_scl[,"group"]=="PER"),c("d13C","d15N")], nm_plot="Figure ID_PER", scaled=T)
cbind(LEP=IDLEP_scl_ind , PER=IDPER_scl_ind)
# Isotopic richness is higher for PER than for LEP.
# but there are 10 individuals PER and 27 for LEP so with the same number of individuals sampled we could expect a much lower IRic of LEP

# computing isotopic similarity
IO_LEP_PER_scl<-IOverlap(cons1=data_ind_LEP_PER_scl[which( data_ind_LEP_PER_scl[,"group"]=="LEP"),c("d13C","d15N")], 
  cons2=data_ind_LEP_PER_scl[which( data_ind_LEP_PER_scl[,"group"]=="PER"),c("d13C","d15N")], nm_plot="Figure IO_LEP_vs_PER")
IO_LEP_PER_scl
# the observed low similarity and low nestedness may be due to this difference in sample size not to a true low similarity


# BOOTSTRAP PROCEDURE to test thee two points:  random sampling of 10 individuals among the 27 LEP (999 times) and computing again richness and similarity with the 10 individuals of PER
nbrep<-1000 # number of runs
mat_bootstrap<-matrix(NA, nbrep, 3, dimnames=list(1:nbrep, c("IRic_LEP", "ISim", "INes"))) # matrix to store results

# loop
for (k in 1:nbrep)
{
  # random sampling of 10 indivudals among the 27 of LEP (= rows position)
  LEP_subsample_k<-sample( which( data_ind_LEP_PER_scl[,"group"]=="LEP") , 10 )
  
  # Isotopic diversity of this subsample of 10 LEP
  IDLEPsubsampled_scl_ind_k<-IDiversity(cons=data_ind_LEP_PER_scl[ LEP_subsample_k,c("d13C","d15N")], nm_plot=NA)  
  
  # Isotopic similarity btween this subsample 10 LEP and the 10 PER
  IO_LEPsubsampled_PER_scl_k<-IOverlap(cons1=data_ind_LEP_PER_scl[ LEP_subsample_k ,c("d13C","d15N")], 
    cons2=data_ind_LEP_PER_scl[which( data_ind_LEP_PER_scl[,"group"]=="PER"),c("d13C","d15N")], nm_plot="Figure IO_LEP_vs_PER") 

  # storing values
  mat_bootstrap[k, ]<- c(IDLEPsubsampled_scl_ind_k["IRic"], IO_LEPsubsampled_PER_scl_k["ISim","2D"] , IO_LEPsubsampled_PER_scl_k["INes","2D"] )

print(k)  # flag to see progress of the loop
  
} # end of k 
  

# comparing isotopic richness
cbind(LEP=IDLEP_scl_ind , PER=IDPER_scl_ind)["IRic",] # IRic of LEP=0.356 vs IRic of PER=0.498 so PER
IDPER_scl_ind["IRic"] / IDLEP_scl_ind["IRic"] # ratio IRic PER / IRic LEP = 1.400
mean( mat_bootstrap[,"IRic_LEP"] ) ; sd( mat_bootstrap[,"IRic_LEP"] ) # when only 10 individuals for LEP, IRic=0.208 (+-0.059)
quantile(mat_bootstrap[,"IRic_LEP"], c(0.025,0.975)) # 95% confidence interval of IRic of LEP with only 10 individuals is [0.098;0.317]
IDPER_scl_ind["IRic"] / mean( mat_bootstrap[,"IRic_LEP"] ) # with 10 individuals for each species ratio is of 2.396

# computing mean and standard deviation of indices values when sample size is the same
IO_LEP_PER_scl["ISim","2D"] # observed Isotopic similarity of 0.24
mean( mat_bootstrap[,"ISim"] ) ; sd( mat_bootstrap[,"ISim"] ) # if same sample size Isotopic similarity would be lower with a mean (+-sd) of 0.19 +-0.45

IO_LEP_PER_scl["INes","2D"] # observed Isotopic nestedness of 0.47
mean( mat_bootstrap[,"INes"] ) ; sd( mat_bootstrap[,"INes"] ) # if same sample size Isotopic nestedness would be higher with a mean (+-sd) of 0.56 +-0.11

#######################################################################################################################
#######################################################################################################################
#             END OF EXAMPLES
#######################################################################################################################
#######################################################################################################################

