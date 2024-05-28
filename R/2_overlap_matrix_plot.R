#' Title
#'
#' @param data : isotope data file
#'
#' @return asymmetric matrix niche overlap
#' @export
#'
#' @examples overlap_matrix (isotope_data)

overlap_matrix <- function(data){
  
# Prepare data 
mat_overlap_data <- data%>%
  mutate(species= gsub("_"," ", species))%>%
  mutate(species=recode(species, "Cyclothone"="Cyclothone spp."))

# Arrange species by their d15n values 
mat_overlap <- mat_overlap_data%>%
  group_by(species)%>%
  mutate(mean_d15n=mean(d15n))%>%
  arrange(mean_d15n)%>%
  as.data.frame()

# ellipse at 40%
test.elp_community <- rKIN::estEllipse(data= mat_overlap,  x="d13c", y="d15n", group="species", levels=40, smallSamp = TRUE)

# Extract the area of each polygon
elp.area_community <- rKIN::getArea(test.elp_community)

# determine polygon overlap for all polygons
elp.olp_community <- rKIN::calcOverlap(test.elp_community)%>%
  #renames row names
  mutate(OverlapID= gsub("_"," ", OverlapID))%>%
  mutate(OverlapID= gsub("40","", OverlapID))%>%
  mutate(across(where(is.numeric), round, 2))%>%
  tibble::column_to_rownames("OverlapID")%>%
  as.data.frame()

#renames colnames
colnames(elp.olp_community)<- gsub(x =colnames(elp.olp_community), pattern = "_", replacement = " ")
colnames(elp.olp_community)<- gsub(x =colnames(elp.olp_community), pattern = "40", replacement = "")

return(elp.olp_community)
}


#' Plot overlap matrix 
#'
#' @param overlap_matrix 

overlap_matrix_plot<- function(overlap_matrix){
  
#plot 
ggcorrplot::ggcorrplot(overlap_matrix, lab = T, outline.color = "white", lab_size = 3, tl.cex = 10)+
  scale_fill_gradient2(limit = c(0,1), low = "white", high = "grey50", mid = "grey80", midpoint = 0.5)+
  labs(fill="Overlap value")+
  theme(axis.text = element_text(face="italic", size = 14),
        legend.text = element_text(size=11),
        legend.title = element_text(size=11),
        plot.background = element_rect(colour = "white"))

#save the plot
ggsave("matrix_overlap.png", path = "figures", dpi = 700, width = 8, height = 6) 
}


