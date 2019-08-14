sapply(list('shiny','DT','AROC','shinythemes','sm','dplyr'), 
       function(x) library(x, character.only=T))

autopooled <- function(data,testcol,resultcol,healthyRes,diseaseRes){
  #missing NA treatment
  
  
  positiveResIndex <- data[[resultcol]]==healthyRes 
  results <- data[[testcol]]
  positiveRes <- results[positiveResIndex]
  negativeRes <- results[!positiveResIndex]
  
  
  
  AROC::pooledROC.emp(positiveRes, negativeRes)#,method = c("coutcome"))
}

specnaomit <- function(col1, col2, df) {
  newdf <- df
  
  if (any(is.na(newdf[[col1]]))) {
    newdf <- newdf[!is.na(newdf[[col1]]),]
    
  } 
  if (any(is.na(newdf[[col2]]))) {
    newdf <- newdf[!is.na(newdf[[col2]]),]
    
  }
  
  newdf
  
}


density_builder <- function(data,testcol,resultcol,color1=3,color2=2){
  
  sm::sm.density.compare(data[[testcol]],data[[resultcol]], 
                         col=c(color1,color2), xlab=testcol)
  legend('topright', legend = levels(as.factor(data[[resultcol]])), fill=c(color1,color2))
  
  
}

gene_aroc_analysis <- function(data, testcol, resultcol, covcol, result_tag ){
  x <- specnaomit(covcol, testcol, data)
  x2 <- x %>% select(testcol, covcol, resultcol)
  formula = paste(testcol,"~",covcol)
  aroc_analysis <-  AROC.sp(formula.healthy = formula,
                            group = resultcol,
                            tag.healthy = result_tag,
                            data = x2)
  aroc_analysis
}
aroc_density_builder <- function(data,testcol,resultcol,covcol,color1=3,color2=2){
  
  resultcolfactor <- as.factor(data[[resultcol]])

  
  plot(data[[covcol]],data[[testcol]],
       pch = c(16, 17)[resultcolfactor],  
       col = c(color1,color2)[resultcolfactor])
  legend('topright', legend = levels(resultcolfactor), fill=c(color1,color2))
}

