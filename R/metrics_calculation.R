#' Calculate group (TA, SEA, SEAc) and 
#' community metrics (dY_range, dX_range, TA, CD, MNND, SDNND) 
#' 
#' @description
#' This functions calculate group and community metrics 
#' 
#' @param x siber data 
#'
#' @return metrics for group and community 
#' 
#' @export
#'
#' @examples
#' group_metrics (siber_data)
#' community_metrics (siber_data)
#' 

# Calculate summary statistics for each group: TA, SEA and SEAc
print_group_metrics <- function (siber_data){
  siber_object <- SIBER::createSiberObject (siber_data)
  group.ML <- SIBER::groupMetricsML (siber_object) 
  print(group.ML)
}

# Calculate the various Layman metrics on each of the communities.
print_community_metrics <- function (siber_data){
 siber_object <- SIBER::createSiberObject (siber_data)
 community.ML <- SIBER::communityMetricsML (siber_object) 
 print(community.ML) 
}
