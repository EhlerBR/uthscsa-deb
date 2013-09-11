##############################################################################
# University of Texas Health Science Center at San Antonio
# Department of Epidemiology and Biostatistics                        
##############################################################################
# Filename:     update.verification.R ###
# Author:       Benjamin Ehler
# Project Name: R Programming Tools
# Input:        df1: original data.frame object
#               df2: new, updated data.frame object
#               unique.key: A unique identifier for each row
#               added.cols: Character vector of column names that have been added and do not need to be verified
#               added.unique.key: Character vector of unique keys that have been added and do not need to be verified
#               pdf.file: Path (including filename and extension) where the pdf is created
# Output:       No Return
#               pdf (image and up to 3 tables that represent changes)
#
# Modification History:
# v 0.1 Creation (04 Sep 2013)                             
##############################################################################

require(gplots)

##############################################################################

update.verification <- function(df1, df2, unique.key, added.cols = NULL, added.unique.key = NULL, pdf.file = paste(documents.dir, "Update Verification (", date, ").pdf", sep = "")) {

  if(!is.data.frame(df1) | !is.data.frame(df2))
    stop("df1 and df2 must be of class, data.frame")

# Remove expected changes
  df2 <- df2[!df2[[unique.key]] %in% added.unique.key, !names(df2) %in% added.cols]

# Convert all to character
  print("Warning:, all variables converted to character for verification")
  for (col.index in 1:ncol(df2))
    df2[, col.index] <- as.character(df2[, col.index])
  for (col.index in 1:ncol(df2))
    df1[, col.index] <- as.character(df1[, col.index])

  if(sum(c(is.na(df1[[unique.key]]), is.na(df2[[unique.key]]))) > 0)
    stop("NAs in the unique.key")
  
### CHECK UNIQUE KEY VARIABLE ###  
  if (!identical(df1[[unique.key]], df2[[unique.key]])) {
    df1.missing.keys <- setdiff(df2[[unique.key]], df1[[unique.key]])
    df2.missing.keys <- setdiff(df1[[unique.key]], df2[[unique.key]])
    if (length(df1.missing.keys) > 0) {    
      key.rows.for.df1 <- as.data.frame(matrix(c(df1.missing.keys, rep("Entire Row was Added", times = (ncol(df1) - 1) * length(df1.missing.keys))), byrow = FALSE, ncol = ncol(df1), nrow = length(df1.missing.keys)))
      names(key.rows.for.df1) <- c(unique.key, names(df1)[names(df1) != unique.key])
      key.rows.for.df1 <- key.rows.for.df1[, names(df1)]        
      for(missing.key.index in 1:length(df1.missing.keys))
        df1 <- as.data.frame(rbind(df1, key.rows.for.df1), stringsAsFactors = FALSE)
    } else {
      df1.missing.keys <- NULL
    }
    if (length(df2.missing.keys)) {
      key.rows.for.df2 <- as.data.frame(matrix(c(df2.missing.keys, rep("Entire Row was Added", times = (ncol(df2) - 1) * length(df2.missing.keys))), byrow = FALSE, ncol = ncol(df2), nrow = length(df2.missing.keys)))
      names(key.rows.for.df2) <- c(unique.key, names(df2)[names(df2) != unique.key])
      key.rows.for.df2 <- key.rows.for.df2[, names(df2)]        
      for(missing.key.index in 1:length(df2.missing.keys))
        df2 <- as.data.frame(rbind(df2, key.rows.for.df2), stringsAsFactors = FALSE)
    } else {
      df2.missing.keys <- NULL
    }   
    if(!identical(df1[[unique.key]], df2[[unique.key]])) {  # now that the unique.keys are the same, sort each dataset by its corresponding unique.key
      print("Warning: The unique key variables have changed or are not ordered identically; They will be sorted for verification")
      
      if(suppressWarnings(sum(is.na(c(df1[[unique.key]], df2[[unique.key]])))) == 0) {
        df1 <- df1[order(as.numeric(as.character(df1[[unique.key]]))), ]
        df2 <- df2[order(as.numeric(as.character(df2[[unique.key]]))), ]
      } else {
        df1 <- df1[order(df1[[unique.key]]), ]
        df2 <- df2[order(df2[[unique.key]]), ]
      }
    }
  } else {
    df1.missing.keys <- NULL
    df2.missing.keys <- NULL
  }
  
### CHECK COLUMNS AND COLUMN NAMES ###  
    if (!identical(names(df1), names(df2))) {
    df1.missing.cols <- setdiff(names(df2), names(df1)) 
    df2.missing.cols <- setdiff(names(df1), names(df2)) 
       
    if (length(df1.missing.cols)) {
      for(missing.col.index in 1:length(df1.missing.cols))
        df1[[df1.missing.cols[missing.col.index]]] <- "Entire Column was Added"    
    } else {
      df1.missing.cols <- NULL
    }
    if (length(df2.missing.cols)) {
      for(missing.col.index in 1:length(df2.missing.cols))
        df2[[df2.missing.cols[missing.col.index]]] <- "Entire Column was Deleted"
    } else {
      df2.missing.cols <- NULL
    }    
    if(!identical(names(df1), names(df2))) {
      print("Warning: The columns have changed or are not ordered identically; They will be sorted for verification")
      df2 <- df2[, order(as.character(names(df2)))]  # Make both columns in the same order
      df1 <- df1[, order(as.character(names(df1)))]  # Make both columns in the same order      
    }
  } else {
    df1.missing.cols <- NULL
    df2.missing.cols <- NULL
  }

  
  if(!identical(dim(df1),  dim(df2)))
    stop("Diminsions of df1 and df2 are not compatible")

### Create data.frame of all cells with changes ###    
  diffs <- !((is.na(df1) & is.na(df2)) | (df1 == df2)) 
  diffs[is.na(diffs)] <- TRUE
  n.diffs <- sum(diffs)
  diff.mat <- matrix(NA, nrow = n.diffs, ncol = 4)
  diff.counter <- 1
  for (col.index in 1:ncol(df1)) {
    for (row.index in 1:nrow(df1)) {
      if(!(is.na(df1[row.index, col.index]) & is.na(df2[row.index, col.index]))) {
        if(is.na(df1[row.index, col.index]) | is.na(df2[row.index, col.index]) | (df1[row.index, col.index] != df2[row.index, col.index])) {
          diff.mat[diff.counter, ] <- c(names(df1)[col.index], df1[[unique.key]][row.index], df1[row.index, col.index], df2[row.index, col.index])
          diff.counter <- diff.counter + 1
        }
      }
    }
  }
  colnames(diff.mat) <- c("Variable", "Unique Key", "df1 Value", "df2 Value")
  diff.df <- as.data.frame(diff.mat, stringsAsFactors = FALSE)
  entire.deletions <- c(grep("Entire Column was Deleted", diff.df[, 3]), grep("Entire Column was Deleted", diff.df[, 4]), grep("Entire Row was Deleted", diff.df[, 3]), grep("Entire Row was Deleted", diff.df[, 4]),
                        grep("Entire Column was Added", diff.df[, 3]), grep("Entire Column was Added", diff.df[, 4]), grep("Entire Row was Added", diff.df[, 3]), grep("Entire Row was Added", diff.df[, 4]))
  if(length(entire.deletions) > 0)
    diff.df <- diff.df[-unique(entire.deletions), ]

### Create data.frame of all columns with changes ###    
  cols.with.changes <- c(unique(diff.df$Variable), df2.missing.cols, df1.missing.cols)
  col.diff.matrix <- matrix(NA, nrow = length(cols.with.changes), ncol = 2)
  col.name.counter <- 0
  for(col.name.index in cols.with.changes) {
    if(col.name.index %in% df1.missing.cols) {
      col.name.counter <- col.name.counter + 1
      col.diff.matrix[col.name.counter, ] <- c(col.name.index, "Column added in df2")
    } else if(col.name.index %in% df2.missing.cols) {
      col.name.counter <- col.name.counter + 1
      col.diff.matrix[col.name.counter, ] <- c(col.name.index, "Column deleted in df2")
    } else if(col.name.index %in% diff.df$Variable) {
      col.name.counter <- col.name.counter + 1
      col.diff.matrix[col.name.counter, ] <- c(col.name.index, "Value Changes")
    }   
  }
  colnames(col.diff.matrix) <- c("Column Name", "Change")
  col.diff.df <- as.data.frame(col.diff.matrix)

### Create data.frame of all rows with changes ###      
  keys.with.changes <- c(unique(diff.df[["Unique Key"]]), df2.missing.keys, df1.missing.keys)
  key.diff.matrix <- matrix(NA, nrow = length(keys.with.changes), ncol = 2)
  key.name.counter <- 0
  for(key.name.index in keys.with.changes) {
    key.name.counter <- key.name.counter + 1
    if(key.name.index %in% df1.missing.keys) {
      key.diff.matrix[key.name.counter, ] <- c(key.name.index, "Unique Key row added in df2")
    } else if(key.name.index %in% df2.missing.keys) {
      key.diff.matrix[key.name.counter, ] <- c(key.name.index, "Unique Key row deleted in df2")
    } else if(key.name.index %in% diff.df[["Unique Key"]]) {
      key.diff.matrix[key.name.counter, ] <- c(key.name.index, "Value Changes")
    }
  }
  colnames(key.diff.matrix) <- c("Unique Key", "Change")
  key.diff.df <- as.data.frame(key.diff.matrix)
  if(suppressWarnings(sum(is.na(c(df1[[unique.key]], df2[[unique.key]])))) == 0) {
    key.diff.df <- key.diff.df[order(as.numeric(as.character(key.diff.df[["Unique Key"]]))), ]    
  } else {
    key.diff.df <- key.diff.df[order(key.diff.df[["Unique Key"]]), ]  
  }
    
### Create PDF file with all information ###    
  pdf(file = pdf.file)
# Plot change image #   
    par(mar = c(8.1, 4.1, 2.1, 2.1))
    label.factor <- 40
    if(!is.null(df1.missing.keys))
      df2[[unique.key]][df1[[unique.key]] %in% df1.missing.keys] <- "Entire Row was Added"
    if(!is.null(df2.missing.keys))
      df2[[unique.key]][df2[[unique.key]] %in% df2.missing.keys] <- "Entire Row was Added"      
    image.matrix <- t(((is.na(df1) & is.na(df2)) | (df1 == df2)))
    image.matrix[is.na(image.matrix)] <- FALSE
    image(z = image.matrix, col = c("red", "white"), xaxt = "n", yaxt = "n", main = "Value Changes (in red)\nDoes Not Include Addition of Entire Rows or Columns", cex.main = 0.6, ylab = "Unique Key", cex.lab = 0.6)
    grid.limit <- 300  # the limit of grid lines before cells are hidden behind them at a full screen view    
    ny <- ifelse(ncol(image.matrix) < grid.limit, ncol(image.matrix), NA)
    nx <- ifelse(nrow(image.matrix) < grid.limit, nrow(image.matrix), NA)    
    grid(nx = nx, ny = ny, lty = 1, lwd = 0.1)
    box(col = "lightgray")

    box.height <- 1/nrow(df1)  
    y.axis.labels <- df1[[unique.key]][nrow(df1):1]
    y.axis.labels.with.changes <- y.axis.labels[y.axis.labels %in% as.character(key.diff.df[["Unique Key"]])]
    y.axis.at.loc <- NULL
    for(y.label.index in y.axis.labels.with.changes)
      y.axis.at.loc <- c(y.axis.at.loc, grep(TRUE, y.axis.labels == y.label.index))
    y.axis.at <- seq(((box.height * (nrow(df1)))), 0, length.out = nrow(df1))[y.axis.at.loc]                         
    axis(2, labels = y.axis.labels.with.changes, at = y.axis.at, las = 1, col = "lightgray", col.ticks = "lightgray", cex.axis = max(min(label.factor / ncol(image.matrix), 0.6), 0.3))
    
    box.width <- 1/ncol(df1)
    text(seq(((box.width * (ncol(df1)))), 0, length.out = ncol(df1)), par("usr")[3] - .005, labels = names(df1)[ncol(df1):1], srt = 270, adj = 0, xpd = TRUE, offset = 0, tck = 0, cex = max(min(label.factor / nrow(image.matrix), 0.6), 0.2))
# Table of column changes #
    page.limit <- 40  # the page limit of lines for this style     
    if(nrow(col.diff.df)) {
      if(nrow(col.diff.df) <= page.limit) {
        textplot(col.diff.df, cex = 0.75, show.rownames = FALSE, valign = "top")
        title("Changes in Variables")
      } else {
        for(col.diff.page in 1:(ceiling(nrow(col.diff.df)/page.limit))) {
          textplot(col.diff.df[(1 + (page.limit * (col.diff.page - 1))):min(nrow(col.diff.df), (page.limit + (page.limit * (col.diff.page - 1)))), ], cex = 0.75, show.rownames = FALSE, valign = "top")
          title(ifelse(col.diff.page == 1, "Changes in Variables", "Changes in Variables (Continued)"))          
        }
      }
    }      
# Table of row changes #    
    if(nrow(key.diff.df)) {
      if(nrow(key.diff.df) <= page.limit) {    
        textplot(key.diff.df, cex = 0.75, show.rownames = FALSE, valign = "top")
        title("Changes in the Unique Key")   
      } else {
        for(key.diff.page in 1:(ceiling(nrow(key.diff.df)/page.limit))) {
          textplot(key.diff.df[(1 + (page.limit * (key.diff.page - 1))):min(nrow(key.diff.df), (page.limit + (page.limit * (key.diff.page - 1)))), ], cex = 0.75, show.rownames = FALSE, valign = "top")
          title(ifelse(key.diff.page == 1, "Changes in the Unique Key", "Changes in the Unique Key (Continued)"))          
        }           
      }
    }
# Table of cell changes #    
    if(nrow(diff.df)) {
      if(nrow(diff.df) <= page.limit) {       
        textplot(diff.df, cex = 0.75, show.rownames = FALSE, valign = "top")
        title("Changes in Cell Values") 
      } else {
        for(diff.page in 1:(ceiling(nrow(diff.df)/page.limit))) { 
          textplot(diff.df[(1 + (page.limit * (diff.page - 1))):min(nrow(diff.df), (page.limit + (page.limit * (diff.page - 1)))), ], cex = 0.75, show.rownames = FALSE, valign = "top")
          title(ifelse(diff.page == 1, "Changes in Cell Values", "Changes in Cell Values (Continued)"))          
        }           
      }                   
    }    
    par(mar = c(5.1, 4.1, 4.1, 2.1))      
  dev.off()

}

### EOF ###
