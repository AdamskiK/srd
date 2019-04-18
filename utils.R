col_to_factor <- function(data, colname){
  data[colname] <- as.factor(data[colname])
  
  return(data)
  
}