##############################################################################
# University of Texas Health Science Center at San Antonio
# Department of Epidemiology and Biostatistics                        
##############################################################################
# Filename: calculateBMI.R                                 
# Author: Christopher Louden                                                 
# Project Name: Shared Functions          
# Input: mass: the weight in lbs or kgs (scalar)
#        height: the height in feet or meters (scalar)
#        metric: TRUE if kg and meters are used.  False if pounds and feet are used.
# Output: bmi: The BMI
#
# Modification History:
# v 1.0 Creation                              
##############################################################################

calculateBMI <- function(mass, height, metric = TRUE){
  if (!is.numeric(mass) || mass <= 0) stop("Mass must be a positive number")
  else if (!is.numeric(height) || height <= 0) stop("Height must be a positive number")
  if (metric){
    bmi <- mass/height^2
  } else {
    bmi <- 703*mass/height^2
  }
  return(bmi)
}