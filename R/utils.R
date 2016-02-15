# Extract data from mousetrap object
extract_data <- function(data,use){
  
  extracted <- data[[use]]
  if(is.null(extracted)){
    stop("No data called '",use,"' found.")
  }
  return(extracted)
}