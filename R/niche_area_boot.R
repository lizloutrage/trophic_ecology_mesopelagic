#' Function to bootstrap the ellipse size (inspired from Brind'Amour & Dubois 2013 and  Suchomel et al. 2022)
#'
#' @param table_iso
#' @param samp
#' @param nbBoot
#'
#'@author Liz Loutrage, \email{liz.loutrage@gmail.com}
#'        Anik Brind'amour, \email {Anik.Brindamour@ifremer.fr}
#'
#'
#' @examples  ellipse_boot(table_iso, samp = 10, nbBoot = 100)

niche_area_boot <- function(table_iso, samp, nbBoot) {
  table_iso <- data.frame(na.omit(table_iso))
  nbBoot <- nbBoot
  SpNames <- table_iso$species
  habNames <- unique(table_iso$habitat)
  d13C <- table_iso$d13C
  d15N <- table_iso$d15N
  d13Cd15N <- cbind(d13C, d15N)
  X <- table_iso$d13C
  Y <- table_iso$d15N
  PID <- 1:nrow(table_iso)
  xydata <- PBSmapping::as.PolyData(cbind(PID, X, Y), projection = NULL, zone = NULL)
  xyCoord <- sp::coordinates(cbind(X, Y))
  
  ## original sampling
  res0 <- niche_area(table_iso, alea = T)
  sres0 <- sp::SpatialPolygons(list(sp::Polygons(list(
    sp::Polygon(as.matrix(res0$ConvHull[, 3:4]))
  ), "x"))) ##transform points to spatial points
  
  ## Boostraping in the result of the convex hull
  boot_iso <- list()
  Ind <- list()
  
  for (i in 1:nbBoot) {
    set.seed(i)
    
    # sample point locations within the polygon, using random sampling method
    sampiS <- sp::spsample(sres0, n = samp, "random")
    boot_iso[[i]] <- cbind(data.frame(sampiS), rep(paste(habNames), length(sampiS)), paste(SpNames))
    names(boot_iso[[i]]) <- c("d13C" , "d15N", "habitat", "species")  
    Ind[[i]] <- niche_area(boot_iso[[i]], alea = F)
  }
  
  res <-
    data.frame(matrix(unlist(Ind), ncol = (unique(
      sapply(Ind, length)
    )), byrow = TRUE))
  colnames(res) <- c("ELP")
  res$habitat <- rep(habNames, nbBoot)
  return(res)
  
} 
