##############################################################################
# University of Texas Health Science Center at San Antonio
# Department of Epidemiology and Biostatistics                        
##############################################################################
# Filename:     survival.statistics.R                             
# Author:       Benjamin Ehler                                          
# Project Name: R Programming Tools          
# Input:        data: dataset (one line per subject ID)
#               time.name: time variable name in data
#               event.name: event variable name in data
#               tx.name: treatment variable name in data
# Output:       $fit: fit object
#               $pvalue: log-rank p-value
#               $survival.table: list object of percentage survival by treatment 
#
# Modification History:
# v 0.1 Creation (BE - 12 Sep 2013)                             
##############################################################################

survival.statistics <- function(data, censor.time, subject.name, event.time.name, tx.name) {

# Remove excess variables & missing data #
  data <- data[, c(subject.name, event.time.name, tx.name)]
  data <- data[!is.na(data[[event.time.name]]), ]

# Create event based on event.time.name variable and censor.time #
  for(subject.index in 1:nrow(data)) {
    data$event[subject.index] <- ifelse(data[[event.time.name]][subject.index] > censor.time, FALSE, TRUE)
    data$time[subject.index] <- min(data[[event.time.name]][subject.index], censor.time)
  }

### Figure Survival (All Tx) ###
  Tx.factors <- unique(data[[tx.name]])
  Tx.factors <- Tx.factors[order(Tx.factors)]
  n.factors <- length(Tx.factors)
# Create figure #  
  fit <- survfit(Surv(data$time, data$event) ~ data[[tx.name]])
# Find log-rank p-value #  
  if(n.factors > 1) {
    temp.survdiff <- survdiff(Surv(data$time, data$event) ~ data[[tx.name]], data = data)
    pvalue <- 1 - pchisq(temp.survdiff$chisq, (length(temp.survdiff$obs) - 1))
  }  
# Create list of percentage survival by treatment #
  survival.list <- paste((100 * (round((summary(fit)$surv), 4))), "%", sep = "")
  time.list <- summary(fit)$time
  if(names(summary(survfit(Surv(data$time, data$event) ~ data[[tx.name]]))[8]) == "strata") {
    survival.table <- cbind(time.list, survival.list, gsub("data\\[\\[tx.name\\]\\]=", "", as.character(summary(survfit(Surv(data$time, data$event) ~ data[[tx.name]]))[8]$strata)))
  } else {
    survival.table <- cbind(time.list, survival.list, as.character(Tx.factors))
  }
  survival.table <- as.data.frame(survival.table)
  names(survival.table)[3] <- as.character(tx.name)
  survival.list <- split(survival.table, survival.table[[tx.name]])
  for (list.index in names(survival.list)){
    for (index in 1:ncol(survival.table)){
      survival.list[[list.index]][,index] <- as.character(survival.list[[list.index]][,index])
    }
  }

# Set return for function #
  if(n.factors > 1) {
    return(list(fit = fit, pvalue = pvalue, survival.list = survival.list))
  } else {
    return(list(fit = fit, survival.list = survival.list))
  }

} 

### EOF ###