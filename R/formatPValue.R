##############################################################################
# University of Texas Health Science Center at San Antonio
# Department of Epidemiology and Biostatistics                        
##############################################################################
# Filename: formatPValue.R                                 
# Author: Christopher Louden                                                 
# Project Name: Shared Functions          
# Input: p.values: a vector of numeric p-values                             
# Output: p.values: a vector of p-values formated as a character 
#
# Modification History:
# v 1.0 Creation                              
##############################################################################

FormatPValue <- function(p.values){
  if (is.numeric(p.values)){
    if (is.matrix(p.values)){
      apply(p.values, c(1, 2), function(x) ifelse(x < 0.001, "< 0.001", ifelse(x < 0.055, round(x, 3), round(x, 2))))
    } else {
      ifelse(p.values < 0.001, "< 0.001", ifelse(p.values < 0.055, round(p.values, 3), round(p.values, 2)))
    }
  } else {
    stop("The supplied argument is non-numeric.")
  } 
}