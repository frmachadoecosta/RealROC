sapply(list('shiny','DT','AROC','shinythemes'), 
       function(x) library(x, character.only=T))

autopooled <- function(data,testcol,resultcol,healthyRes,diseaseRes){
  #missing NA treatment
  
  
  positiveResIndex <- data[[resultcol]]==healthyRes 
  results <- data[[testcol]]
  positiveRes <- results[positiveResIndex]
  negativeRes <- results[!positiveResIndex]
  
  
  
  AROC::pooledROC.emp(positiveRes, negativeRes)#,method = c("coutcome"))
}