#' bootstrap isotopic values in convex hull
#'
#' @param table_iso: data frame with species, d13C, d15N and habitat columns
#' @param samp: number of samples
#' @param nbBoot: number of bootstrapping isotopic values in the convex hull
#'
#' @examples sum_overlap_boot (table_iso, samp=10, nbBoot=1000)

sum_overlap_boot <- function(table_iso, samp, nbBoot) {
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
  xydata <- as.PolyData(cbind(PID, X, Y), projection = NULL, zone = NULL)
  xyCoord <- coordinates(cbind(X, Y))
  
  ## original sampling
  res0 <- sum_overlap(table_iso, alea = TRUE)
  sres0 <-
    SpatialPolygons(list(Polygons(list(
      Polygon(as.matrix(res0$ConvHull[, 3:4]))
    ), "x")))
  
  ## Boostraping  in the result of the convex hull
  boot_iso <- list()
  Ind <- list()
  
  for (i in 1:nbBoot) {
    set.seed(i)
    sampiS <- sp::spsample(sres0, n = samp, "random")
    boot_iso[[i]] <-
      cbind(data.frame(sampiS),
            rep(paste(habNames), length(sampiS)),
            rep(SpNames, length(sampiS)))
    names(boot_iso[[i]]) <- c("d13C" , "d15N", "habitat", "species")
    Ind[[i]] <- sum_overlap(boot_iso[[i]], alea = FALSE)
  }
  
  res <-
    data.frame(matrix(unlist(Ind), ncol = (unique(
      sapply(Ind, length)
    )), byrow = TRUE))
  colnames(res) <- c("OVER")
  res$habitat <- rep(habNames, nbBoot)
  return(res)
}