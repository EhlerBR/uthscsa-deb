##############################################################################
# University of Texas Health Science Center at San Antonio
# Department of Epidemiology and Biostatistics                        
##############################################################################
# Filename: factorToNumeric.R                                  
# Author: Christopher Louden                                                 
# Project Name: Shared Functions           
# Input: factor: a vector of class factor                             
# Output: a vector of class numeric.
# Note: Non-numeric values will be converted to missing.
#
# Modification History:
# v 1.0 Creation                              
##############################################################################

FactorToNumeric <- function(factor){
  if(is.factor(factor)){
    as.numeric(as.character(factor))
  } else {
    stop("The supplied argument is not a factor.")
  }
}