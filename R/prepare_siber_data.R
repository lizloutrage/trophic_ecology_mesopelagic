#' Prepare the raw data in a SIBER format 
#' 
#' @description
#' This function create a dataframe than can be used for SIBER analysis 
#' 
#' @param x raw data 
#'
#' @return matrix in a good format for SIBER uses 
#' 
#' @export
#'
#' @examples
#' prepare_siber_data()
#' prepare_siber_data(raw_isotope_data)
#' 

prepare_siber_data <- function (isotope_data){
  
  # extract only the relevant data from the full data frame
  d13C <- isotope_data [,26]
  d15N <- isotope_data [,30]
  rawgroups <- isotope_data [,15]
  rawcomms <- isotope_data[,14] 
  
  #Creating a matrix with all those ordered properly
  CNraw <- cbind (d13C, d15N, rawgroups, rawcomms)
  
  #Changing column names to match SIBER requirements and create dataframe 
  colnames (CNraw) <- c ("iso1","iso2","group","community")
  CNraw.df <- as.data.frame (CNraw)
  return (CNraw)
}
