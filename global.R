sapply(list('shiny','DT','AROC','shinythemes','sm','dplyr','npROCRegression','pROC'), 
       function(x) library(x, character.only=T))

autopooled <- function(data,testcol,resultcol,healthyRes,diseaseRes,type){
  #missing NA treatment
  
  
  positiveResIndex <- data[[resultcol]]==healthyRes 
  results <- data[[testcol]]
  positiveRes <- results[positiveResIndex]
  negativeRes <- results[!positiveResIndex]
  
  
  if (type == 'Pooled Empirical'){
  res <- AROC::pooledROC.emp(positiveRes, negativeRes)#,method = c("coutcome"))
  } else { res <- AROC::pooledROC.BB(positiveRes, negativeRes, B=100)}
  res
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

gene_aroc_analysis <- function(data, testcol, resultcol, covcol, result_tag, aroc_type ){
  x <- specnaomit(covcol, testcol, data)
  x2 <- x %>% select(testcol, covcol, resultcol)
  formula = paste(testcol,"~",covcol)
  
  if (aroc_type == 'Nonparametric Bayesian'){
    
    aroc_analysis <-  AROC.bnp(formula.healthy = formula,
                              group = resultcol,
                              tag.healthy = result_tag,
                              data = x2)
    
  } else if (aroc_type == 'Semiparametric Bayesian'){
    aroc_analysis <-  AROC.bsp(formula.healthy = formula,
                              group = resultcol,
                              tag.healthy = result_tag,
                              data = x2)
  } else{
  
  aroc_analysis <-  AROC.sp(formula.healthy = formula,
                            group = resultcol,
                            tag.healthy = result_tag,
                            data = x2)
  }
  aroc_analysis
}
aroc_density_builder <- function(data,testcol,resultcol,covcol,color1=3,color2=2){
  
  resultcolfactor <- as.factor(data[[resultcol]])

  
  plot(data[[covcol]],data[[testcol]],
       pch = c(16, 17)[resultcolfactor],  
       col = c(color1,color2)[resultcolfactor],
       xlab = covcol, ylab = testcol)
  legend('topright', legend = levels(resultcolfactor), fill=c(color1,color2))
}

roccondiButtons <- function(id, label = "ROCParam") {
  ns <- NS(id)
  uiOutput(ns("roccondicionals"), label = label)
  
}

roccondi <- function(input, output, session,ndata) {
    output$roccondicionals <- renderUI({
      tagList(
        selectInput('marker','Select Marker or Test ',
                    multiple=FALSE, choices = names(ndata)),
        selectInput('resultcol','Select Result Column',
                    multiple=FALSE, choices = names(ndata)),
        textInput('healthy_pop','Select Healthy Value'),
        textInput('disease_pop','Select Disease Value')
      )
      })
}

compAROC_ggplot <- function(obj1, obj2, plottitle, leged1, leged2){
  library(ggplot2)
  p <- seq(0, 1, l = 101)
  
  df <- data.frame(Approach = rep(c(leged1, leged2), each = length(p)), x = rep(p, 2),
                   y = c(obj1$ROC[,1], obj2$ROC[,1]),
                   
                   ql = c(obj1$ROC[,2], obj2$ROC[,2]),
                   
                   qh = c(obj1$ROC[,3], obj2$ROC[,3])
  )
  
  
  g0 <- ggplot(df, aes(x = x, y = y, ymin = ql, ymax = qh)) +
    geom_line(aes(colour = Approach, linetype = Approach), size = 1) +
    scale_color_manual(values = c("#F8766D", "#7CAE00")) +
    geom_ribbon(aes(fill = Approach), alpha = 0.2) +
    scale_fill_manual(values = c("#F8766D", "#7CAE00")) +
    scale_linetype_manual(values=c("dashed", "dotdash")) +
    guides(colour=guide_legend(keywidth = 3, keyheight = 1)) +
    labs(title = plottitle, x = "FPF", y = "TPF") +
    theme(legend.position = c(0.7, 0.15),
          plot.title = element_text(hjust = 0.5, size = 20),
          axis.text = element_text(size=20),
          axis.title = element_text(size=20),
          legend.title = element_text(size=15),
          legend.text = element_text(size=15))
  
  g0
}

#check if any missing arguments
values_inputed <- function(...){
  x <- list(0,...)


}

# classic empirical curve
empiricalcurve <- function(data,testcol,resultcol,healthyRes,diseaseRes){
  
  #print(data[[resultcol]])
  
  res <- roc(data[[resultcol]], data[[testcol]],
      smoothed = TRUE,
      # arguments for ci
      ci=TRUE, ci.alpha=0.9, stratified=FALSE,
      # arguments for plot
      plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
      print.auc=TRUE, show.thres=TRUE)
  res
}



