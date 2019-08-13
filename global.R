sapply(list('shiny','DT','AROC','shinythemes','sm'), 
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

singlecontinualcovpepeanal <- function(Y_marker, covariate, result_col, result_tag, df){
  x <- specnaomit(covariate, Y_marker, df)
  x2 <- x %>% select(Y_marker, covariate, result_col)
  formula = paste(Y_marker,"~",covariate)
  aroc_analysis <-  AROC.sp(formula.healthy = formula,
                            group = result_col,
                            tag.healthy = result_tag,
                            data = x2)
  plot(aroc_analysis)
  print(summary(aroc_analysis$fit.h))
}

density_builder <- function(data,testcol,resultcol,color1=2,color2=3){
  
  sm::sm.density.compare(data[[testcol]],data[[resultcol]], 
                         col=c(color1,color2), xlab=testcol)
  legend('topright', legend = levels(as.factor(data[[resultcol]])), fill=c(2,3))
  
  
}


