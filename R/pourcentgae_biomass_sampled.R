catch_data <-  utils::read.csv("data_catch_2021_mesopelagic.csv", sep = ";", header = T,dec = ".")

biomass <- catch_data%>%
  select(Code_Station, Nom_Scientifique, Tot_V_HV)%>%
  mutate(depth_layer = case_when(Code_Station %in% c("Z0508") ~"epipelagic",
                                 Code_Station %in% c("Z0492", "Z0512") ~"upper_mesopelagic",
                                 Code_Station %in% c("Z0503", "Z0518") ~"lower_mesopelagic",
                                 Code_Station %in% c("Z0524","Z0497") ~"bathypelagic"))%>%
  select(-Code_Station)%>%
  distinct()%>%
  group_by(depth_layer, Nom_Scientifique)%>%
  mutate(sum_biomass_sp=sum(Tot_V_HV))%>%
  select(-Tot_V_HV)%>%
  distinct()%>%
  group_by(depth_layer)%>%
  mutate(sum_depht_layer=sum(sum_biomass_sp))%>%
  group_by(depth_layer, Nom_Scientifique)%>%
  mutate(poucentage_sp=(sum_biomass_sp/sum_depht_layer)*100)%>%
  select(-c(sum_depht_layer, sum_biomass_sp))

