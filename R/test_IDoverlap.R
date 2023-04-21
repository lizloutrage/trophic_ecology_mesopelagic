##################################################################################################################################################################
##################################################################################################################################################################
# 'IOverlap': function to compute isotopic overlap indices between two sets of organisms (e.g. life stages within a population, species within a community)
#
# INPUTS: - 'cons1' and 'cons2': two dataframes or matrices with stable isotope values for two sets of organisms (same format as for 'IDiversity'). 
#					Elements names should be identical between the two sets and be a subset of  ('d13C','d15N','dD','d34S').
# 		- 'nm_plot': a single character string specifying the name of the jpeg file where graphics illustrating isotopic diversity will be stored. Default is NA, i.e. no graphics.
# 		- 'col': a vector with 2 colors, coded as hexadecimal characters, for points and convex hull filling of the two sets of organisms. Default is 'cons1' in red and 'cons2' in blue.
# 		- 'transp': a single numeric value indicating the percentage of transparency for convex hulls filling. Default is 50%.
#     -  'scaled': a logical value indicating wether isotopic values have been scaled to have a range between 0 and 1. 
#                   If TRUE (default) axes of graphics fill at least the 0-1 range and axes titles specify the scaling procedure
#
# OUTPUTS: - a matrix with 5 indices (rows) for each element and for their combination (columns). 
# 					Indices are Isotopic Richness (TRic1, IRic2) of the two sets respectively, the volume of their intersection (IRic1n2), 
#              Isotopic similarity (ISim), i.e. percentage of Isotopic Overlap expressed relatively to total Isotopic Richness [IRic1n2/(IRic1+IRic2-IRic1n2)], 
#              and Isotopic nestedness (INes), i.e. percentage of Isotopic Overlap expressed relatively to to minimum Isotopic Richness [IRic1n2/(min(IRic1,IRic2))]
#
#          - a jpeg file (‘nm_plot’.jpeg) with one panel for each pair of elements  . 
#               All axes have the same range to illustrate potential bias if no standardization are done.
#               Points represent organism's position in the isotopic niche space (and associated standard deviation if values were provided in the corresponding 'cons' dataframe).
#               Isotopic Overlap on each axis is illustrated by colored segment and as the intersection of colored polygons in the multidimensional space.
#				If more than two elements were used to build the isotopic space, the convex polygons are projection of the multidimensional convex hulls in 2D.
#
##################################################################################################################################################################


IOverlap <- function(cons1, cons2, cons3, cons4, cons5, cons6, cons7, nm_plot=NA, 
                     col=c("#504AE8","#FA1900", "yellow", "black"), transp=50, scaled=TRUE) {
  
  # names of elements used to describe consumers of set1
  nmel_cons1<-colnames(cons1)[which(colnames(cons1) %in% nm_si)]
  si_cons1<-as.matrix(cons1[,nmel_cons1])
  
  # names of elements used to describe consumers of set2
  nmel_cons2<-colnames(cons1)[which(colnames(cons2) %in% nm_si)]
  si_cons2<-as.matrix(cons2[,nmel_cons2])
  
  # names of elements used to describe consumers of set3
  nmel_cons3<-colnames(cons1)[which(colnames(cons3) %in% nm_si)]
  si_cons3<-as.matrix(cons3[,nmel_cons3])
  
  # names of elements used to describe consumers of set4
  nmel_cons4 <-colnames(cons1)[which(colnames(cons4) %in% nm_si)]
  si_cons4 <-as.matrix(cons4[,nmel_cons4])
  
  # checking that elements used to describe the 2 sets of consumers are identical
  if ( sum(nmel_cons1 !=nmel_cons2) !=0 ) stop(" error: the 2 set of consumers should be described using the same elements")
  nmel<-nmel_cons1 ; nbel<-length(nmel_cons1)
  
  # checking number of consumers is higher than number of elements
  if (nrow(cons1)<(nbel+1)) stop(paste(" error: computing indices using",nbel,"elements requires at least",nbel+1," consumers"))
  
  ##########################################################################################
  # function to compute overlap between 2 sets of points in a multidimensional space
  
  intersect<-function(set1,set2, set3, set4)  {
    
    # tranforming points coordinates in the Euclidean space in true rational number written as character string
    # reduce set of points to vertices using redundant function
    # changing polytope representation: vertices to inequality constraints
    set1rep <- d2q(cbind(0, cbind(1, set1)))
    polytope1 <- redundant(set1rep, representation = "V")$output
    H_chset1 <- scdd(polytope1, representation = "V")$output
    
    set2rep <- d2q(cbind(0, cbind(1, set2)))
    polytope2 <- redundant(set2rep, representation = "V")$output
    H_chset2 <- scdd(polytope2, representation = "V")$output
    
    set3rep <- d2q(cbind(0, cbind(1, set3)))
    polytope3 <- redundant(set3rep, representation = "V")$output
    H_chset3 <- scdd(polytope3, representation = "V")$output
    
    set4rep <- d2q(cbind(0, cbind(1, set4)))
    polytope4 <- redundant(set4rep, representation = "V")$output
    H_chset4 <- scdd(polytope4, representation = "V")$output
    
    # intersection between the two polytopes
    H_inter <- rbind(H_chset1, H_chset2, H_chset3, H_chset4)
    V_inter <- scdd(H_inter, representation = "H")$output
    
    # extracting coordinates of vertices
    vert_1n2 <- q2d(V_inter[ , - c(1, 2)])
    
    # computing convex hull volume of the intersection (if it exists)
    vol_inter<-0
    if (is.matrix(vert_1n2)) # vector if one vertex in common
      if( nrow(vert_1n2)>ncol(vert_1n2) ) vol_inter<-convhulln(vert_1n2,"FA")$vol
    
    return(vol_inter)
    
  } # end of function intersect
  ##########################################################################################
  
  # vector to store results
  nD<-paste(nbel,"D",sep="")
  IO<-matrix(NA,5,nbel+1, dimnames=list( c("IRic1","IRic2","IRic3","IRic4", "IRic1n2", "IRic1n3", "IRic1n4", "IRic2n3", "IRic2n4",
                                           "IRic3n4","ISim12","ISim13","ISim14","ISim23","ISim24","ISim34",
                                           "Ines12","Ines13","Ines14","Ines23","Ines4","Ines34"), c(nmel,nD) ) ) 
  
  
  # computing overlap on each axis  
  overlap1d<-function(x,y) { max(0, ( min(max(x),max(y)) - max(min(x),min(y)) )  ) }
  for (e in nmel)
  { 
    IO["IRic1",e]<-max(si_cons1[,e])-min(si_cons1[,e])
    IO["IRic2",e]<-max(si_cons2[,e])-min(si_cons2[,e])	
    IO["IRic3",e]<-max(si_cons3[,e])-min(si_cons3[,e])
    IO["IRic4",e]<-max(si_cons4[,e])-min(si_cons4[,e])	
    IO["IRic1n2",e]<-overlap1d( si_cons1[,e] , si_cons2[,e] )
    IO["IRic1n3",e]<-overlap1d( si_cons1[,e] , si_cons3[,e] )
    IO["IRic1n4",e]<-overlap1d( si_cons1[,e] , si_cons4[,e] )
    IO["IRic2n3",e]<-overlap1d( si_cons2[,e] , si_cons3[,e] )
    IO["IRic2n4",e]<-overlap1d( si_cons2[,e] , si_cons4[,e] )
    IO["IRic3n4",e]<-overlap1d( si_cons3[,e] , si_cons4[,e] )
    IO["ISim12",e]<-IO["IRic1n2",e]/(IO["IRic1",e]+IO["IRic2",e]-IO["IRic1n2",e])	
    IO["ISim13",e]<-IO["IRic1n3",e]/(IO["IRic1",e]+IO["IRic3",e]-IO["IRic1n3",e])
    IO["ISim14",e]<-IO["IRic1n4",e]/(IO["IRic1",e]+IO["IRic4",e]-IO["IRic1n4",e])
    IO["ISim23",e]<-IO["IRic2n3",e]/(IO["IRic2",e]+IO["IRic3",e]-IO["IRic2n3",e])
    IO["ISim24",e]<-IO["IRic2n4",e]/(IO["IRic2",e]+IO["IRic4",e]-IO["IRic2n4",e])
    IO["ISim34",e]<-IO["IRic2n4",e]/(IO["IRic2",e]+IO["IRic4",e]-IO["IRic2n4",e])
    IO["INes",e]<-( IO["IRic1n2",e] ) /min(c(IO["IRic1",e],IO["IRic2",e]))	
  } # end of e
  
  
  # multidimensional IRic and vertices (if plot needed) for consumers of set1
  IO["IRic1",nD]<-round(convhulln(si_cons1,"FA")$vol,6)
  if( is.na(nm_plot)==FALSE) { 
    vert0<-convhulln(si_cons1,"Fx TO 'vert.txt'")
    vert1<-scan("vert.txt",quiet=T)
    vertices_cons1<-(vert1+1)[-1] } # end of vertices for plot
  
  # multidimensional IRic and vertices (if plot needed) for consumers of set2
  IO["IRic2",nD]<-round(convhulln(si_cons2,"FA")$vol,6)
  if( is.na(nm_plot)==FALSE) { 
    vert0<-convhulln(si_cons2,"Fx TO 'vert.txt'")
    vert1<-scan("vert.txt",quiet=T)
    vertices_cons2<-(vert1+1)[-1] } # end of vertices for plot
  
  # multidimensional intersection 
  IO["IRic1n2",nD]<-intersect(si_cons1, si_cons2 )
  
  # multidimensional similarity= intersection/total volume 
  IO["ISim",nD]<-IO["IRic1n2",nD]/(IO["IRic1",nD]+IO["IRic2",nD]-IO["IRic1n2",nD])
  
  # multidimensional nestedness= intersection/min(volume) 
  IO["INes",nD]<-IO["IRic1n2",nD]/min(c(IO["IRic1",nD],IO["IRic2",nD]))
  
  
  ######################################################################################	
  # graphical output
  if( is.na(nm_plot)==FALSE) {
    
    #####################################
    # creating jpeg file
    nmjpeg<-paste(nm_plot,".jpeg",sep="")
    # one panel per pairs of elements 
    if( nbel==2) { jpeg(file=nmjpeg, res=150, width=1800, height=600) ; layout(matrix(c(1,0,0),1,3,F)) ; layout.show(1) }
    if( nbel==3) { jpeg(file=nmjpeg, res=150, width=1800, height=600) ; layout(matrix(c(1:3),1,3,F)) ; layout.show(3) }
    if( nbel==4) { jpeg(file=nmjpeg, res=150, width=1800, height=1200) ; layout(matrix(c(1:6),2,3,F)) ; layout.show(6)  }
    
    
    # setting axes limits given consumers signature for all elements
    nmelsd<-paste("sd",nmel,sep="_")
    cons<-rbind(cons1,cons2)
    min_axes<- apply(cons[,nmel], 2, min, na.rm=T) ; max_axes<- apply(cons[,nmel], 2, max, na.rm=T) # limits of each axis
    
    # limits of each axis given sd
    if(sum(nmelsd %in% colnames(cons) )==nbel) 
    {min_axes<-apply(cons[,nmel]-cons[,nmelsd], 2, min, na.rm=T)
    max_axes<-apply(cons[,nmel]+cons[,nmelsd], 2, max, na.rm=T) }
    
    # same range on each axis for graphics: min=observed minimal value - 5%  maximal range ; max=observed minimal value + maximal range + 5% maximal range
    rge_axes<-max_axes-min_axes # range on each axis
    
    newlim_axes<-matrix(0,length(nmel),2, dimnames=list(nmel,c("min","max") ) )
    newlim_axes[,"min"]<-min_axes-max(rge_axes)*0.1
    newlim_axes[,"max"]<-min_axes+max(rge_axes)*1.1
    
    rge_plot<-max(rge_axes)*1.2
    
    ###############################################################
    # plot for each pair of elements
    
    
    for (e1 in 1:(nbel-1))
      for (e2 in (e1+1):nbel) 
      {
        
        # names of elements
        nmel1<-nmel[e1] ; eval(parse(text=paste("tit1<-tit_",nmel1,sep="") ) )
        nmel2<-nmel[e2] ; eval(parse(text=paste("tit2<-tit_",nmel2,sep="") ) )
        nmel12<-c(nmel1,nmel2)
        
        
        # limits of axes
        lim_1<-newlim_axes[nmel1,]
        lim_2<-newlim_axes[nmel2,]
        
        # if axes are for scaled isotope values, "Scaled" in axis title and range is at least from 0 to 1
        if (scaled==TRUE) {
          tit1<-eval(parse(text=paste("tit1<-scl_tit_",nmel1,sep="") ) ); tit2<-eval(parse(text=paste("tit2<-scl_tit_",nmel2,sep="") ) )
          lim_1<-c( min(-0.05, lim_1[1]) , max(1.05,lim_1[2])   )
          lim_2<-c( min(-0.05, lim_2[1]) , max(1.05,lim_2[2])   )
        }  # end of if scaled axes
        
        # labels on axes
        lab_1<-pretty(lim_1,n=6,min.n=6) ; lab_1<-lab_1[which(lab_1>=lim_1[1] & lab_1<=lim_1[2])]
        lab_2<-pretty(lim_2,n=6,min.n=6) ; lab_2<-lab_2[which(lab_2>=lim_2[1] & lab_2<=lim_2[2])]
        
        # isotopic space given axes limits set using consumers signature
        isotopic_space(nmX=tit1,nmY=tit2, limX=lim_1, limY=lim_2, labX=lab_1,labY=lab_2 )
        
        # consumers of set1
        # range on each axis
        dec1<-rge_plot*0.015
        segments( min(si_cons1[,nmel1]), min(lim_2)+dec1, max(si_cons1[,nmel1]), min(lim_2)+dec1, col=col[1] , lwd=3) # x
        segments( min(lim_1)+dec1, min(si_cons1[,nmel2]), min(lim_1)+dec1, max(si_cons1[,nmel2]), col=col[1] , lwd=3) # y
        
        # projected convex hull in 2D
        vert0<-convhulln(si_cons1[,nmel12],"Fx TO 'vert.txt'")
        vert1<-scan("vert.txt",quiet=T) ; vertices2D<-(vert1+1)[-1]
        polygon(si_cons1[vertices2D,nmel12],border=NA,col=paste(col[1],transp,sep="") )
        
        # all points (empty) then filling points being vertices in nD
        points(si_cons1[,nmel12], pch=21,bg=NA, col=col[1],cex=2)
        points(si_cons1[vertices_cons1,nmel12], pch=21,bg=col[1], col=col[1],cex=2)
        
        # error bars if any
        if(sum(nmelsd %in% colnames(cons1) )==nbel) meansexy(si_cons1[,nmel12], cons1[,paste("sd",nmel12,sep="_")],colb=col[1],lg=0.01*rge_plot ) 
        
        
        # consumers of set2
        # range on each axis
        dec2<-rge_plot*0.03
        segments( min(si_cons2[,nmel1]), min(lim_2)+dec2, max(si_cons2[,nmel1]), min(lim_2)+dec2, col=col[2] , lwd=3) # x
        segments( min(lim_1)+dec2, min(si_cons2[,nmel2]), min(lim_1)+dec2, max(si_cons2[,nmel2]), col=col[2] , lwd=3) # y
        
        # projected convex hull in 2D
        vert0<-convhulln(si_cons2[,nmel12],"Fx TO 'vert.txt'")
        vert1<-scan("vert.txt",quiet=T) ; vertices2D<-(vert1+1)[-1]
        polygon(si_cons2[vertices2D,nmel12],border=NA,col=paste(col[2],transp,sep=""))
        
        # all points (empty) then filling points being vertices in nD
        points(si_cons2[,nmel12], pch=21,bg=NA, col=col[2],cex=2)
        points(si_cons2[vertices_cons2,nmel12], pch=21,bg=col[2], col=col[2],cex=2)
        
        # error bars if any
        if(sum(nmelsd %in% colnames(cons2) )==nbel) meansexy(si_cons2[,nmel12], cons2[,paste("sd",nmel12,sep="_")],colb=col[2],lg=0.01*rge_plot ) 
        
        
        # indices values    
        mtext(side=3, paste("Isotopic Similarity=",round(IO['ISim',nD],3),sep=""), at=mean(lim_1), line=0.9, cex=0.8,adj=0.5, font=2)     
        mtext(side=3, paste("Isotopic Nestedness=",round(IO['INes',nD],3),sep=""), at=mean(lim_1), line=-0.3, cex=0.8,adj=0.5,font=2) 
      } # end of e1, e2             
    
    
    graphics.off()	
  } # end of plot
  
  ###############################################
  # returning results	
  return(IO)	
  
} # end of IOverlap




##############################################################################################################################################
################################                             END OF SRIPT                         ############################################
##############################################################################################################################################
