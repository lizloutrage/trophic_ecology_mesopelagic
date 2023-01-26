# 'si_div': a script with 4 R functions: 'meanSI_group', 'scaleSI_range01', 'IDiversity', 'IOverlap'		 
# 			for computing isotopic diversity indices based on stable isotope values.
#
# after the article by Julien Cucherousset & Sébastien Villéger
#
# Author: Sébastien Villéger: sebastien.villeger@univ-montp2.fr
#
# IMPORTANT: The R libraries 'geometry', 'ape' and 'rcdd' need to be installed


#' meanSI_group
#'function to compute mean Stable Isotope values for different elements at the group level using individual stable isotope values
#'
#' @param dataset,  a dataframe or matrix with at least 3 columns: 
#' 	$ 'group': a numeric or character variable with the identity (e.g. life stage, sex, population, species) of each individual
#' 	$ 'd13C', 'd15N', 'dD' and/or 'd34S': at least two numeric variables with stable isotope value f each individual
#' 	$ 'weight': an optional numeric variable with weight (e.g. body mass, dry mass) of each individual
#'
#' @return a matrix with for each group of individuals (rows): number of individuals ('n'), mean stable isotope value (e.g. 'd13C', 'd15N'), 
#' standard deviation of stable isotope values (e.g. 'd13C_sd', 'd15N_sd'), and total weight of each group ('weight') [NA if no weight as input].
#' NB: This matrix can be used directly as an INPUT for the subsequent functions: 'scaleSI_rge01', 'IDiversity' and 'IOverlap'
#' @export
#'
#' @examples

meanSI_group <- function(dataset) {
  
# codes for stable isotope ratios
nm_si <- c("d13C", "d15N", "dD", "d34S")

# names of groups and abbreviation (4 characters)
gr <- levels(as.factor(as.character(dataset[, "group"])))

# number of groups
nbgr <- length(gr)

# computing number of individuals per group
nbind_gr <- summary(as.factor(as.character(dataset[, "group"])))

# number and names of elements used
nmel <- colnames(dataset)[which(colnames(dataset) %in% nm_si)]
nbel <- length(nmel)

# isotopic signature of individuals
si <- as.matrix(dataset[,nmel])

# computing mean isotopic signature and total abundance of each group (if abundances provided)
data_gr <- matrix(NA,nbgr,2+nbel*2, dimnames=list(gr, c("n", nmel, paste("sd",nmel,sep="_"), "weight"  ) ) )
  
for (k in gr)
{ 
  rowk <- which(dataset[, "group"]==k)
  data_gr[k, "n"]<-length(rowk) 
  
  if("weight" %in% colnames(dataset)==T) { data_gr[k, "weight"]<-sum(dataset[rowk, "weight"])} # end of weight
       
         for (e in nmel)
         {data_gr[k,e]<-mean(si[rowk,e])
         data_gr[k,paste("sd",e,sep="_")]<-sd(si[rowk,e]) } # end of e
 
 } # end of k

return(data_gr)	
	
} # end of function meanSI_group


#' scaleSI_range01
#' function to standardize Stable Isotope values of a dataset given a reference dataset of organisms (individuals or group of individuals)
#'
#' @param raw_data, a dataframe or matrix with stable isotope values for at least 2 elements (columns) for several groups (rows).
#' Columns' names should be a subset of ('d13C','d15N','dD','d34S').
#' @param all_data, a dataframe or matrix with stable isotope values for the same elements than 'raw_data' (columns) with an equal or larger set of samples (rows). 
#' By default, 'all_data' is identical to 'raw_data'.
#'
#' @return, a matrix similar to 'raw_data' with scaled stable isotope values (each ranging from 0 to 1).
#' If standard deviation values are provided, they are scaled based on the range of mean values.
#' @export
#'
#' @examples

scaleSI_range01 <- function(raw_data, all_data=raw_data) {

nm_si <- c("d13C","d15N","dD","d34S")

# names of elements used to describe data
nmel_raw_data <- colnames(raw_data)[which(colnames(raw_data) %in% nm_si)]
si_raw_data <- as.matrix(raw_data[,nmel_raw_data])
nmel <- nmel_raw_data ; nbel <- length(nmel_raw_data)
 
# checking that elements used to describe raw data are present in whole dataset
if (sum(nmel_raw_data %in% colnames(raw_data)) !=nbel) stop("error: the elements present in 'raw_data' must be present in 'all_data'")

# matrix to store results
data_scaled <- raw_data

# scaling data according to range of whole dataset
for (e in nmel)
data_scaled[,e] <- (raw_data[,e]-min(all_data[,e],na.rm=T))/(max(all_data[,e],na.rm=T)-min(all_data[,e], na.rm=T))	

# scaling standard deviation (if any)
nmelsd <- paste("sd",nmel,sep="_")
if(sum(nmelsd %in% colnames(raw_data) )==nbel) {
	for (e in nmel)
	data_scaled[,paste("sd",e,sep="_")]<-raw_data[,paste("sd",e,sep="_")]/(max(all_data[,e],na.rm=T)-min(all_data[,e],na.rm=T))	
											} # end of if sd

return(data_scaled)
	
} # end of scale_range01



# generic graphical functions used in 'IDiversity' and 'IOverlap'

# axis titles for the 4 types of istopes used
tit_d13C <- expression(bold(paste(delta^"13",C,sep=""))) ; scl_tit_d13C<-expression(bold(paste("Scaled ",delta^"13",C,sep="")))
tit_d15N <- expression(bold(paste(delta^"15",N,sep=""))) ; scl_tit_d15N<-expression(bold(paste("Scaled ",delta^"15",N,sep="")))
tit_dD <- expression(bold(paste(delta^"",D,sep=""))) ; scl_tit_dD<-expression(bold(paste("Scaled ",delta^"",D,sep="")))
tit_d34S <- expression(bold(paste(delta^"34",S,sep=""))) ; scl_tit_d34S<-expression(bold(paste("Scaled ",delta^"34",S,sep="")))


#' isotopic_space
#' graphic function to plot isotopic space
#'
#' @param nmX 
#' @param nmY 
#' @param limX 
#' @param limY 
#' @param labX 
#' @param labY 
#'
#' @return
#' @export
#'
#' @examples

isotopic_space <- function(nmX="X",nmY="Y", limX=c(-0.05,1.05),limY=c(-0.05,1.05), labX=c(0,0.25,0.5,0.75,1),labY=c(0,0.25,0.5,0.75,1) )   {
 
nm_si<-c("d13C","d15N","dD","d34S")
  
# setting graphical parameters
par(mar=c(4,4.5,4,3.5)) ; tick=-0.4 ; lasX=1 ;lasY=1 ; lineX=-0.2 ; lineY=-0.2 ; cexX=0.9 ; cexY=0.9 ; lineXt=lineX+2.1 ; lineYt=lineY+2.2 ; cexXt=1 ; cexYt=1 

# empty window
plot(limX,limY,type="n",axes=F,xaxt="n",yaxt="n",xlab="",ylab="",xlim=limX,ylim=limY) 
rect(limX[1],limY[1],limX[2],limY[2])   # border

# X axis
axis(side=1, at=labX, labels=F, tcl=tick, pos=limY[1])  # X thicks
mtext(side=1, labX, at=labX, line=lineX, cex=cexX, las=lasX) # X labels
mtext(side=1,nmX,cex=cexXt,line=lineXt,font=2) # X title  

# Y axis
axis(side=2, at=labY, labels=F, tcl=tick, pos=limX[1]) # Y thicks 
mtext(side=2, labY, at=labY, line=lineY, cex=cexY, las=lasY) # Y labels
mtext(side=2,nmY,cex=cexYt,line=lineYt,font=2) # Y title   

} # end of isotopic_space	



#' meansexy
#'function to add vertical and horizontal error bars
#'
#' @param meanxy 
#' @param sexy 
#' @param colb 
#' @param lg 
#'
#' @return
#' @export
#'
#' @examples

meansexy <- function(meanxy,sexy,colb="black",lg=0.1) {
segments(meanxy[,1]-sexy[,1],meanxy[,2],meanxy[,1]+sexy[,1],meanxy[,2],col=colb) # x error bar
segments(meanxy[,1],meanxy[,2]-sexy[,2],meanxy[,1],meanxy[,2]+sexy[,2],col=colb) # y error bar
segments(meanxy[,1]-sexy[,1],meanxy[,2]-lg,meanxy[,1]-sexy[,1],meanxy[,2]+lg,col=colb)
segments(meanxy[,1]+sexy[,1],meanxy[,2]-lg,meanxy[,1]+sexy[,1],meanxy[,2]+lg,col=colb)
segments(meanxy[,1]-lg,meanxy[,2]-sexy[,2],meanxy[,1]+lg,meanxy[,2]-sexy[,2],col=colb)
segments(meanxy[,1]-lg,meanxy[,2]+sexy[,2],meanxy[,1]+lg,meanxy[,2]+sexy[,2],col=colb)
} # end of meansexy


#' IDiversity
#' 
#'function to compute complementary indices describing the isotopic diversity for a group of organisms (individuals in a population or species in a community)
#'
#' @param cons, a dataframe or matrix with stable isotope values for at least 2 elements (columns) for several organisms (rows).
#'  It may include standard deviation of mean values (coded as 'sd_d13C', 'sd_d15N',...). 
#'  These standard deviation values are used only for illustrative purposes in the graphical outputs.
#' @param weight a numeric vector with weight of organisms (e.g. individual mass, or relative abundance of species in the community). By default all weight equals 1.
#' @param nm_plot a single character string specifying the name of the .jpeg file where graphics illustrating isotopic diversity will be stored. Default is NA, i.e. no graphics.
#' @param col a single color, coded as hexadecimal characters, for points and convex hull filling. Default is green.
#' @param transp a single numeric value indicating the percentage of transparency for convex hulls filling. Default is 50%.
#' @param scaled a logical value indicating wether isotopic values have been scaled to have a range between 0 and 1. 
#' If TRUE (default) axes of graphics fill at least the 0-1 range and axes titles specify the scaling procedure
#'
#' @return a vector with minimum, maximum, range and abundance-weighted mean values for each isotopic axis (e.g. 'min_d13C', 'max_d13C', 'range_d13C', 'IPos_d13C' for d13C) 
#' 	and 5 multidimensional indices: isotopic richness ('IRic'), isotopic evenness ('IEve') and isotopic divergence ('IDiv'), isotopic dispersion ('IDis') and isotopic uniqueness ('IUni'). 
#'  - for each pair of elements a 6-panels .jpeg file (e.g. ‘nm_plot’_d13C_d15N.jpeg).
#'         All axes have the same range to illustrate potential bias if no standardization has been done prior indices computation.
#'         For each panel, points representing organisms position in the isotopic niche space (and associated standard deviation if values have been provided in “cons” dataframe).
#'         Weights of organisms are illustrated proportionally to point surface and a legend is displayed in the bottom right corner.
#'         
#'         * top left panel: Isotopic position, i.e. weighted-mean values of the organisms on each axis, is illustrated with a square and horizontal and vertical dashed lines.
#'         * top middle panel: Isotopic richness is shown as the colored area. Filled points are organisms being vertices in the multidimensional space. 
#'            If more than two elements were used to build the isotopic space, the convex polygon is a projection of the multidimensional convex hull in 2D.
#'            Minimum and maximum values on each axis are illustrated by vertical bars.
#'         * top right panel: Isotopic divergence is illustrated through the center of gravity of the vertices (diamond) and al the distances to it (dashed lines).
#'         * bottom left panel: Isotopic dispersion is symbolized by the center of gravity of all points (white square) and all the distances to it (dotted lines).
#'         * bottom middle panel: Isotopic evenness is illustrated with the minimum spanning tree linking all points in the multidimensional space.
#'         * bottom right panel: Isotopic uniqueness is symbolized with all the distances to nearest organism (black arrows).	
#' @export
#'
#' @examples IDiversity(cons=data_fish_scl, weight=data_fish_scl[,c("rel_Biomass")], nm_plot= "data1")

IDiversity<-function(cons, weight=rep(1,nrow(cons)), nm_plot=NA, col="#051D8A", transp=40, scaled=TRUE) {
  nm_si<-c("d13C","d15N","dD","d34S")
  
  # names of elements used to describe consumers
  nmel<-colnames(cons)[which(colnames(cons) %in% nm_si)]
  nbel<-length(nmel)
  
  # stable isotope signature
  si<-as.matrix(cons[,nmel])
  
  # checking weighting for all individuals
  if(length(weight) != nrow(cons)) stop(paste(" error: weight does not have the same length than number of consumers"))
  
  # relative weight
  rel_weight<-weight/sum(weight)
  
  # checking number of consumers is higher than number of elements
  if (nrow(cons)<(nbel+1)) stop(paste(" error: computing indices using",nbel,"elements requires at least",nbel+1," consumers"))
  
  
  # vector to store results
  ID<-rep(NA,nbel*4+5) ; names(ID)<-c(paste("min",nmel,sep="_"), paste("max",nmel,sep="_"), paste("range",nmel,sep="_"), paste("IPos",nmel,sep="_"), c("IRic","IDiv","IDis","IEve","IUni") )
  
  ###########################################################
  # computing indices values on each axis
  
  # range of traits values
  ID[paste("min",nmel,sep="_")]<-apply(si,2,min)
  ID[paste("max",nmel,sep="_")]<-apply(si,2,max)
  ID[paste("range",nmel,sep="_")]<-ID[paste("max",nmel,sep="_")]-ID[paste("min",nmel,sep="_")]
  
  # abundance-weighted mean values
  ID[paste("IPos",nmel,sep="_")]<-rel_weight%*%si
  
  ###############################################################################################################################
  
  # generic functions for computing multidimensional diversity indices
  
  I_RED<-function(coord,relab )  {
    
    # number of species
    S<-nrow(coord) 
    
    ###########################################################
    # Richness
    IRic<-round(convhulln(coord,"FA")$vol,6)
    
    # identity of vertices
    vert0<-convhulln(coord,"Fx TO 'vert.txt'")
    vert1<-scan("vert.txt",quiet=T)
    vertices<-(vert1+1)[-1]
    
    ###########################################################
    # Evenness
    
    # inter-species Euclidean distance
    distT<-dist(coord, method="euclidian")
    
    # topology of Minimum Spanning Tree and conversion of the 'mst' matrix into 'dist' class
    linkmst<-mst(distT)
    mstvect<-as.dist(linkmst)
    
    # pairwise cumulative relative abundances and conversion into 'dist' class
    ab2<-matrix(0,nrow=S,ncol=S)
    for (q in 1:S)
      for (r in 1:S)
        ab2[q,r]<-relab[q]+relab[r] # end of q,r
    ab2vect<-as.dist(ab2)
    
    # EW index for the (S-1) segments
    EW<-rep(0,S-1)
    flag<-1
    for (m in 1:((S-1)*S/2))
    {if (mstvect[m]!=0) {EW[flag]<-distT[m]/(ab2vect[m]) ; flag<-flag+1}}  # end of m
    
    # PEW index and comparison with 1/S-1
    minPEW<-rep(0,S-1)  ;  OdSmO<-1/(S-1)
    for (l in 1:(S-1))
      minPEW[l]<-min( (EW[l]/sum(EW)) , OdSmO )  # end of l
    
    # IEve
    IEve<-round( ( (sum(minPEW))- OdSmO) / (1-OdSmO ) ,6)
    
    ###############################################################
    # Divergence
    
    # coordinates of vertices
    coordvertices<-coord[vertices,]
    
    # coordinates of the center of gravity of the vertices (B)
    B<-apply(coordvertices,2,mean)
    
    # Euclidean dstance to B (dB)
    dB<-apply(coord, 1, function(x) { (sum((x-B)^2) )^0.5} )
    
    # mean of dB values and deviations to mean 
    meandB<-mean(dB)
    devdB<-dB-meandB
    
    # abundance-weighted mean deviation
    abdev<-relab*devdB
    ababsdev<-relab*abs(devdB)
    
    # IDiv
    IDiv<-round( (sum(abdev)+meandB) / (sum(ababsdev)+meandB) ,6)
    
    ####################################################################
    # results
    indices<-c(IRic,IEve,IDiv) ; names(indices)<-c("IRic","IEve","IDiv")
    detailsRED<-list(vertices=vertices, mst=linkmst, B=B, meandB=meandB)
    I_RED<-list(indices=indices, details=detailsRED )
    invisible(I_RED)
  } # end of function I_RED
  ########################################################################################################################################
  
  # multivariate indices from Villeger et al 2008
  ired<-I_RED(si,rel_weight)
  ID[c("IRic","IEve","IDiv")]<-ired$indices
  
  # Isotopic dispersion: scaled abundance-weighted mean distance to abundance-weighted centroid 
  dist_centr<-apply(si, 1, function(x) { (sum((x-ID[paste("IPos",nmel,sep="_")])^2) )^0.5} ) # distance to abundance-weighted centroid 
  ID["IDis"]<-(rel_weight %*% dist_centr)/ max(dist_centr) # scaling between 0(=all biomass on the centroid) and 1(=all biomass on the most extreme point)
  
  # Isotopic originality : scaled abundance weighted mean distance to nearest neighbour
  # for each organism distance to, and identity of, nearest neighbour
  dist_T<-as.matrix(dist(si,method="euclidean")) ; dist_T[which(dist_T==0)]<-NA
  oriT<-apply(dist_T, 1, min, na.rm=T )
  NN<-dist_T ; NN<-NN-apply(NN,1,min,na.rm=T) ; NN[which(NN!=0)]<-NA   ; NN[which(NN==0)]<-1
  ID["IUni"]<-(oriT %*% rel_weight) / max(oriT)   # abundance weighting and scaling by maximal distance between 2 points
  
  ########################################################################################################################################
  ########################################################################################################################################
  # graphical output
  if( is.na(nm_plot)==FALSE) {
    
    # setting axes limits given consumers signature for all elements
    nmelsd<-paste("sd",nmel,sep="_")
    
    min_axes<- apply(cons[,nmel], 2, min, na.rm=T) ; max_axes<- apply(cons[,nmel], 2, max, na.rm=T) # limits of each axis
    
    # limits of each axis given sd
    if(sum(nmelsd %in% colnames(cons) )==nbel) 
    {min_axes<-apply(cons[,nmel]-cons[,nmelsd], 2, min, na.rm=T)
    max_axes<-apply(cons[,nmel]+cons[,nmelsd], 2, max, na.rm=T) }
    
    # same range on each axis for graphics: min=observed minimal value - 5%  maximal range ; max=observed minimal value + maximal range + 5% maximal range
    rge_axes<-max_axes-min_axes # range on each axis
    
    newlim_axes<-matrix(0,length(nmel),2, dimnames=list(nmel,c("min","max") ) )
    newlim_axes[,"min"]<-min_axes-max(rge_axes)*0.05
    newlim_axes[,"max"]<-min_axes+max(rge_axes)*1.05
    
    rge_plot<-max(rge_axes)*1.1
    
    
    # one jpeg file per pair of elements with 6 panels
    
    for (e1 in 1:(nbel-1))
      for (e2 in (e1+1):nbel) 
      {
        # names of elements
        nmel1<-nmel[e1] ; eval(parse(text=paste("tit1<-tit_",nmel1,sep="") ) )
        nmel2<-nmel[e2] ; eval(parse(text=paste("tit2<-tit_",nmel2,sep="") ) )
        nmel12<-c(nmel1,nmel2)
        
        # creating jpeg file
        nmjpeg<-paste(nm_plot,"_",nmel1,"_",nmel2,".jpeg",sep="")
        jpeg(file=nmjpeg, res=150, width=1200, height=1800)
        layout(matrix(c(1:6),3,2,T)) ; layout.show(6)
        
        # limits of axes
        lim_1<-newlim_axes[nmel1,]
        lim_2<-newlim_axes[nmel2,]
        
        # if axes are for scaled isotope values, "Scaled" in axis title and range is at least from 0 to 1
        if (scaled==TRUE) {
          tit1<-eval(parse(text=paste("tit1<-scl_tit_",nmel1,sep="") ) ); tit2<-eval(parse(text=paste("tit2<-scl_tit_",nmel2,sep="") ) )
          lim_1<-c( min(-0.05, lim_1[1]) , max(1.05,lim_1[2])   )
          lim_2<-c( min(-0.05, lim_2[1]) , max(1.05,lim_2[2])   )
        }  # end of if scaled axes
        
        # labels  on axes
        lab_1<-pretty(lim_1,n=5,min.n=4) ; lab_1<-lab_1[which(lab_1>=lim_1[1] & lab_1<=lim_1[2])]
        lab_2<-pretty(lim_2,n=5,min.n=4) ; lab_2<-lab_2[which(lab_2>=lim_2[1] & lab_2<=lim_2[2])]
        
        ###############################################################
        # Isotopic Position
        
        # Isotopic space given axes limits set using consumers signature
        isotopic_space(nmX=tit1,nmY=tit2, limX=lim_1, limY=lim_2, labX=lab_1,labY=lab_2 )
        
        # mean value
        segments(ID[paste("IPos_",nmel1,sep="")],ID[paste("IPos_",nmel2,sep="")], ID[paste("IPos_",nmel1,sep="")], min(lim_2), lwd=1.5, col=col, lty=2)
        segments(ID[paste("IPos_",nmel1,sep="")],ID[paste("IPos_",nmel2,sep="")], min(lim_1) ,ID[paste("IPos_",nmel2,sep="")], lwd=1.5, col=col, lty=2)
        points( ID[paste("IPos_",nmel1,sep="")],ID[paste("IPos_",nmel2,sep="")], pch=22, bg="white", col=col,cex=2.5)
        
        # abundances, scaling: point area proportional to relative abundance, if relab=100%, circle diameter=15% of axis range
        sizeab<-sqrt(rel_weight)*0.075*rge_plot  
        symbols(si[,nmel1],si[,nmel2], circles=sizeab, inches=FALSE, bg=col, fg=col, add=TRUE)
        
        # legend for abundance 
        rect(max(lim_1)-0.25*rge_plot, min(lim_2), max(lim_1), min(lim_2)+0.12*rge_plot)
        symbols(max(lim_1)-0.19*rge_plot, min(lim_2)+0.06*rge_plot, circles=sqrt(0.1)*0.075*rge_plot, inches=FALSE, bg="black", fg="black", add=TRUE, lwd=1.5)
        text(max(lim_1)-0.15*rge_plot, min(lim_2)+0.06*rge_plot,"10%", adj=c(0,0.5) ) 
        
        # error bars if any
        if(sum(nmelsd %in% colnames(cons) )==nbel) { meansexy(meanxy=si[,nmel12], sexy=cons[,paste("sd",nmel12,sep="_")],colb=col,lg=0.01*rge_plot ) }# sd
        
        # index
        mtext(side=3, tit1, at=min(lim_1)+rge_plot*0.1, line=-0.4, cex=0.7,adj=1)
        mtext(side=3, paste(": IPos=",round(ID[paste("IPos_",nmel1,sep="")],3),sep=""), at=min(lim_1)+rge_plot*0.1, line=-0.4, cex=0.7,adj=0)     
        
        mtext(side=3, tit2, at=mean(lim_1)+rge_plot*0.1, line=-0.4, cex=0.7,adj=1)
        mtext(side=3, paste(": IPos=",round(ID[paste("IPos_",nmel2,sep="")],3),sep=""), at=mean(lim_1)+rge_plot*0.1, line=-0.4, cex=0.7,adj=0)     
        
        mtext(side=3, "Isotopic Position", at=mean(lim_1), line=1.1, cex=0.8,adj=0.5, font=2)                
        
        ###############################################################
        # Isotopic Richness
        
        # Isotopic space given axes limits set using consumers signature
        isotopic_space(nmX=tit1,nmY=tit2, limX=lim_1, limY=lim_2, labX=lab_1,labY=lab_2 )
        
        # range on each axis
        dec1<-rge_plot*0.02
        segments( ID[paste("min_",nmel1,sep="")], min(lim_2)-dec1, ID[paste("min_",nmel1,sep="")], min(lim_2)+dec1, col=col , lwd=3) # min x
        segments( ID[paste("max_",nmel1,sep="")], min(lim_2)-dec1, ID[paste("max_",nmel1,sep="")], min(lim_2)+dec1, col=col , lwd=3) # max x
        
        segments( min(lim_1)-dec1, ID[paste("min_",nmel2,sep="")], min(lim_1)+dec1, ID[paste("min_",nmel2,sep="")],  col=col , lwd=3) # min y
        segments( min(lim_1)-dec1, ID[paste("max_",nmel2,sep="")], min(lim_1)+dec1, ID[paste("max_",nmel2,sep="")],  col=col , lwd=3) # max y
        
        # projected convex hull in 2D
        vert0<-convhulln(si[,nmel12],"Fx TO 'vert.txt'")
        vert1<-scan("vert.txt",quiet=T) ; vertices2D<-(vert1+1)[-1]
        polygon(si[vertices2D,nmel12],border=NA,col=paste(col,transp,sep=""))
        
        # all points (empty) then filling points being vertices in nD
        points(si[,nmel12], pch=21,bg=NA, col=col[1],cex=2)
        points(si[ired$details$vertices,nmel12], pch=21,bg=col, col=col,cex=2)
        
        # error bars if any
        if(sum(nmelsd %in% colnames(cons) )==nbel) { meansexy(meanxy=si[,nmel12], sexy=cons[,paste("sd",nmel12,sep="_")],colb=col,lg=0.01*rge_plot ) }# sd
        
        # index    
        mtext(side=3, tit1, at=min(lim_1)+rge_plot*0.1, line=-0.4, cex=0.7,adj=1)
        mtext(side=3, paste(": ",round(ID[paste("range_",nmel1,sep="")],1), " [",round(ID[paste("min_",nmel1,sep="")],1),";",round(ID[paste("max_",nmel1,sep="")],1),"]",sep=""), at=min(lim_1)+rge_plot*0.1, line=-0.4, cex=0.7,adj=0) 
        
        mtext(side=3, tit2, at=mean(lim_1)+rge_plot*0.1, line=-0.4, cex=0.7,adj=1)
        mtext(side=3, paste(": ",round(ID[paste("range_",nmel2,sep="")],1), " [",round(ID[paste("min_",nmel2,sep="")],1),";",round(ID[paste("max_",nmel2,sep="")],1),"]",sep=""), at=mean(lim_1)+rge_plot*0.1, line=-0.4, cex=0.7,adj=0) 
        
        mtext(side=3, paste("Isotopic Richness=",round(ID['IRic'],3),sep=""), at=mean(lim_1), line=1.1, cex=0.8,adj=0.5, font=2)                
        
        ###############################################################
        # Isotopic Divergence
        
        # Isotopic space given axes limits set using consumers signature
        isotopic_space(nmX=tit1,nmY=tit2, limX=lim_1, limY=lim_2, labX=lab_1,labY=lab_2 )
        
        # projected convex hull in 2D
        vert0<-convhulln(si[,nmel12],"Fx TO 'vert.txt'")
        vert1<-scan("vert.txt",quiet=T) ; vertices2D<-(vert1+1)[-1]
        polygon(si[vertices2D,nmel12],border=col,col=NA, lwd=1 )
        
        segments(ired$details$B[nmel1], ired$details$B[nmel2], si[,nmel1],si[,nmel2],col=col, lty=2, lwd=2)
        points(ired$details$B[nmel1], ired$details$B[nmel2], pch=23,col=col,bg="white",cex=2.5)
        
        # abundances, scaling: point area proportional to relative abundance, if relab=100%, circle diameter=15% of axis range
        sizeab<-sqrt(rel_weight)*0.075*rge_plot  
        symbols(si[,nmel1],si[,nmel2], circles=sizeab, inches=FALSE, bg=col, fg=col, add=TRUE)
        
        # legend for abundance 
        rect(max(lim_1)-0.25*rge_plot, min(lim_2), max(lim_1), min(lim_2)+0.12*rge_plot)
        symbols(max(lim_1)-0.19*rge_plot, min(lim_2)+0.06*rge_plot, circles=sqrt(0.1)*0.075*rge_plot, inches=FALSE, bg="black", fg="black", add=TRUE, lwd=1.5)
        text(max(lim_1)-0.15*rge_plot, min(lim_2)+0.06*rge_plot,"10%", adj=c(0,0.5) ) 
        
        # error bars if any
        if(sum(nmelsd %in% colnames(cons) )==nbel) { meansexy(meanxy=si[,nmel12], sexy=cons[,paste("sd",nmel12,sep="_")],colb=col,lg=0.01*rge_plot ) }# sd
        
        # index    
        mtext(side=3, paste("Isotopic Divergence=",round(ID['IDiv'],3),sep=""), at=mean(lim_1), line=0.5, cex=0.8,adj=0.5, font=2)               
        
        ###############################################################
        # Isotopic Dispersion
        
        # Isotopic space given axes limits set using consumers signature
        isotopic_space(nmX=tit1,nmY=tit2, limX=lim_1, limY=lim_2, labX=lab_1,labY=lab_2 )
        
        # distance to abundance weighted centroid
        segments(ID[paste("IPos_",nmel1,sep="")],ID[paste("IPos_",nmel2,sep="")], si[,nmel1],si[,nmel2],col=col, lty=3, lwd=2)
        points( ID[paste("IPos_",nmel1,sep="")],ID[paste("IPos_",nmel2,sep="")], pch=22, bg="white", col=col,cex=2.5)
        
        # abundances, scaling: point area proportional to relative abundance, if relab=100%, circle diameter=15% of axis range
        sizeab<-sqrt(rel_weight)*0.075*rge_plot  
        symbols(si[,nmel1],si[,nmel2], circles=sizeab, inches=FALSE, bg=col, fg=col, add=TRUE)
        
        # legend for abundance 
        rect(max(lim_1)-0.25*rge_plot, min(lim_2), max(lim_1), min(lim_2)+0.12*rge_plot)
        symbols(max(lim_1)-0.19*rge_plot, min(lim_2)+0.06*rge_plot, circles=sqrt(0.1)*0.075*rge_plot, inches=FALSE, bg="black", fg="black", add=TRUE, lwd=1.5)
        text(max(lim_1)-0.15*rge_plot, min(lim_2)+0.06*rge_plot,"10%", adj=c(0,0.5) ) 
        
        # error bars if any
        if(sum(nmelsd %in% colnames(cons) )==nbel) { meansexy(meanxy=si[,nmel12], sexy=cons[,paste("sd",nmel12,sep="_")],colb=col,lg=0.01*rge_plot ) }# sd
        
        # index    
        mtext(side=3, paste("Isotopic Dispersion=",round(ID['IDis'],3),sep=""), at=mean(lim_1), line=0.5, cex=0.8,adj=0.5, font=2)               
        
        ###############################################################
        # Isotopic Evenness
        
        # Isotopic space given axes limits set using consumers signature
        isotopic_space(nmX=tit1,nmY=tit2, limX=lim_1, limY=lim_2, labX=lab_1,labY=lab_2 )
        
        # MST
        for (j in 1:nrow(ired$details$mst))
          for (i in 1:nrow(ired$details$mst))
            if (ired$details$mst[j,i]==1 & j>i) segments(si[,nmel1][j], si[,nmel2][j], si[,nmel1][i], si[,nmel2][i], col=col, lwd=1.5)
        
        
        # abundances, scaling: point area proportional to relative abundance, if relab=100%, circle diameter=15% of axis range
        sizeab<-sqrt(rel_weight)*0.075*rge_plot  
        symbols(si[,nmel1],si[,nmel2], circles=sizeab, inches=FALSE, bg=col, fg=col, add=TRUE)
        
        # legend for abundance 
        rect(max(lim_1)-0.25*rge_plot, min(lim_2), max(lim_1), min(lim_2)+0.12*rge_plot)
        symbols(max(lim_1)-0.19*rge_plot, min(lim_2)+0.06*rge_plot, circles=sqrt(0.1)*0.075*rge_plot, inches=FALSE, bg="black", fg="black", add=TRUE, lwd=1.5)
        text(max(lim_1)-0.15*rge_plot, min(lim_2)+0.06*rge_plot,"10%", adj=c(0,0.5) ) 
        
        # error bars if any
        if(sum(nmelsd %in% colnames(cons) )==nbel) { meansexy(meanxy=si[,nmel12], sexy=cons[,paste("sd",nmel12,sep="_")],colb=col,lg=0.01*rge_plot ) }# sd
        
        # index    
        mtext(side=3, paste("Isotopic Evenness=",round(ID['IEve'],3),sep=""), at=mean(lim_1), line=0.5, cex=0.8,adj=0.5, font=2)     
        
        ###############################################################
        # Isotopic Uniqueness
        
        # isotopic space given axes limits set using consumers signature
        isotopic_space(nmX=tit1,nmY=tit2, limX=lim_1, limY=lim_2, labX=lab_1,labY=lab_2 )
        
        # abundances, scaling: point area proportional to relative abundance, if relab=100%, circle diameter=15% of axis range
        sizeab<-sqrt(rel_weight)*0.075*rge_plot  
        symbols(si[,nmel1],si[,nmel2], circles=sizeab, inches=FALSE, bg=col, fg=col, add=TRUE)
        
        # distance to nearest neighbour
        for (k in 1:nrow(NN))
        {
          arrows( si[k,nmel1],si[k,nmel2], si[which(NN[k,]==1)[1],nmel1], si[which(NN[k,]==1)[1],nmel2], col="black", lwd=1.8, length=0.1, angle=20)
        } # end of k
        
        # legend for abundance 
        rect(max(lim_1)-0.25*rge_plot, min(lim_2), max(lim_1), min(lim_2)+0.12*rge_plot)
        symbols(max(lim_1)-0.19*rge_plot, min(lim_2)+0.06*rge_plot, circles=sqrt(0.1)*0.075*rge_plot, inches=FALSE, bg="black", fg="black", add=TRUE, lwd=1.5)
        text(max(lim_1)-0.15*rge_plot, min(lim_2)+0.06*rge_plot,"10%", adj=c(0,0.5) ) 
        
        # error bars if any
        if(sum(nmelsd %in% colnames(cons) )==nbel) { meansexy(meanxy=si[,nmel12], sexy=cons[,paste("sd",nmel12,sep="_")],colb=col,lg=0.01*rge_plot ) }# sd
        
        # index    
        mtext(side=3, paste("Isotopic Uniqueness=",round(ID['IUni'],3),sep=""), at=mean(lim_1), line=0.5, cex=0.8,adj=0.5, font=2)               
        
        ###############################################
        graphics.off()	
      } # end of e1, e2  
    
    
  } # end of plot
  
  ##############################################################################################
  # returning results	
  return(ID)	
  
} # end of function IDiversity