######## pagination.r ##########
# paginate function            #
# Author: Benjamin Ehler       #
# Date: 29 May 2012            #
################################

paginate <- function(data.table, panel.column, page.length){
  paginated.table <- NULL
  page <- 1
  new.table <- NULL
  for(index in unique(panel.column)){
    if(is.null(new.table)) space <- page.length
    table <- data.table[panel.column == index, ]
      rows <- dim(table)[1]
    extend.pages <- rows/page.length
    if (extend.pages %% 1 == 0) extend.pages <- extend.pages - 1
    extend.pages <- floor(extend.pages)
  
    if(extend.pages > 0){
      print("pageinate WARNING: panel factor length > page length")  
      table$page <- page
      for(page_index in 1:(extend.pages+1)){
        page <- ifelse(page == 1, page - 1, page)
        table$page[(page.length * (page_index - 1) + 1):min((page.length * page_index), rows)] <- page + page_index
      }    
      page <- page + extend.pages + 2  # Can change to + 1 to allow for smaller panels to fill in behind large panels that have stretched pages   
      space <- page.length - (rows - (page.length * extend.pages))
      
    } else {
      space <- space - rows
      
      if(space < 0){
        page <- page + 1
        space <- page.length - rows
      }
      table <- cbind(table, page)
      if(space == 0){
        page <- page + 1
        space <- page.length
      }
    }
  new.table <- rbind(new.table, table)
  }
  paginated.table[["table"]] <- new.table
  paginated.table[["pages"]] <- new.table$page
  return(paginated.table)
}
