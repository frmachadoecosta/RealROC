sapply(list('shiny','DT','AROC','shinythemes','sm','dplyr','npROCRegression','pROC', 'Comp2ROC', 'ggplot2'), 
       function(x) library(x, character.only=T))


do_classicroc <- function(data,testcol,resultcol,healthyRes,diseaseRes,type){
  if (type == 'Empirical' || type == 'Empirical Smooth') {
    res <- empiricalcurve(data,testcol,resultcol,healthyRes,diseaseRes,type)
  }
  if (type == 'Pooled Empirical' || type == 'Pooled Bayesian'){
    res <- autopooled(data,testcol,resultcol,healthyRes,diseaseRes,type)
  }
 res 
}



autopooled <- function(data,testcol,resultcol,healthyRes,diseaseRes,type){
  #missing NA treatment
  data <- specnaomit(testcol, resultcol, data)  
  
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
  data <- specnaomit(testcol, resultcol, data)  
  
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
                              data = x2,nsim = 1000, nburn = 200)
    
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
  data <- specnaomit(covcol, testcol, data)
  
  resultcolfactor <- as.factor(data[[resultcol]])
  
  if (is.factor(data[[covcol]])){
    
    ggplot(data, aes_string(x=covcol,y=testcol, fill=factor(data[[resultcol]]))) +
      geom_boxplot() + scale_fill_manual(values=c('3', '2'))
    
  }else{
    
    plot(data[[covcol]],data[[testcol]],
         pch = c(16, 17)[resultcolfactor],  
         col = c(color1,color2)[resultcolfactor],
         xlab = covcol, ylab = testcol)
    legend('topright', legend = levels(resultcolfactor), fill=c(color1,color2))
  }
}
roccondiButtons <- function(id, label = "ROCParam") {
  ns <- NS(id)
  uiOutput(ns("roccondicionals"), label = label)
  
}

roccondi <- function(input, output, session,ndata,n) {
    output$roccondicionals <- renderUI({
      tagList(
        selectInput(paste0('marker',n),'Select Marker or Test ',
                    multiple=FALSE, choices = names(ndata)),
        selectInput(paste0('resultcol',n),'Select Result Column',
                    multiple=FALSE, choices = names(ndata)),
        textInput(paste0('healthy_pop',n),'Select Healthy Value'),
        textInput(paste0('disease_pop',n),'Select Disease Value')
      )
      })
    outputOptions(output, "roccondicionals", suspendWhenHidden = FALSE)
}

covselButtons <- function(id, label = "AROCParam") {
  ns <- NS(id)
  uiOutput(ns("covsel"), label = label)
  
}

covselect <- function(input, output, session,ndata,n) {
  output$covsel <- renderUI({
    tagList(
      selectInput(paste0('cov',n),'Select Covariate',
                  multiple=FALSE, choices = names(ndata))
    )
  })
  outputOptions(output, "covsel", suspendWhenHidden = FALSE)
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
empiricalcurve <- function(data,testcol,resultcol,healthyRes,diseaseRes,curvetype){
  
  #print(data[[resultcol]])
  
  res <- roc(data[[resultcol]], data[[testcol]],
      smoothed = TRUE,
      # arguments for ci
      ci=TRUE, ci.alpha=0.9, stratified=FALSE,
      # arguments for plot
      plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
      print.auc=TRUE, show.thres=TRUE)
  if (curvetype == 'Empirical Smooth'){res <- pROC::smooth(res)}
  res
}

# ----- Comp Module functions

comp_converter <- function(df1,df2,rescol1,rescol2, related){
  
  if(length(df1)==2 && length(df2==2)){
    
    var1 <- names(df1)[names(df1)!=rescol1]
    var2 <- names(df2)[names(df2)!=rescol2]
    
    list1 <- df1[order(df1[[rescol1]]),]
    list2 <- df2[order(df2[[rescol2]]),]
    
    
    listall <- list(
      list1[[var1]] , 
      list1[[rescol1]] ,
      list2[[var2]] , 
      list2[[rescol2]] 
    )
    
    var1n <- paste(var1,'1',sep='_')
    var2n <- paste(var2,'2',sep='_')
    rescol1n <- paste(rescol1,'1',sep = '_')
    rescol2n <- paste(rescol2,'2',sep = '_')
    
    names(listall) <- c(var1n, rescol1n, var2n, rescol2n)
    
    
    comp2rocdata <- read.manually.introduced(listall,
                                             modality1=var1n, testdirection1=TRUE, 
                                             modality2=var2n,testdirection2=TRUE, 
                                             status1=rescol1n, related = related, status2 = rescol2n)
    
    
  }else {stop('Dataframes must have 2 columns each')}
  
  
}

# loading function

loadfunc <- function(){
  withProgress(message = 'Ploting Curve',
              value = 0, {
               for (i in 1:15) {
                 incProgress(1/15)
                 Sys.sleep(0.05)
               }
             })
}

#update report text (must be inside observe element)
newreport <- function(reportval, newtext) {
    current_value <- reportval()
    new_value <- paste0(current_value,'\n', newtext)
    
    new_value
  }




propersummary <- function(arocobj, marker, resultcol, healthytag, covariate){
  
  auc <- signif(as.numeric(arocobj[['AUC']][1]), 4)
  low <- signif(as.numeric(arocobj[['AUC']][2]), 4)
  high <- signif(as.numeric(arocobj[['AUC']][3]), 4)
  
  method <- attr(arocobj, 'class')[2]
  
  pre <- 'Report for AROC Module'
  sepline <- '-----'
  a <- paste0('Method Used: ' , method)
  b <- paste0('Marker: ', as.character(marker))
  c <- paste0('Result Column: ', as.character(resultcol))
  d <- paste0('Healthy Value: ', as.character(healthytag))
  e <- paste0('Covariate: ', as.character(covariate))
  aucCI <- paste0( ' (',low,', ',high,')')
  f <- paste0('>Area under the AROC (AAUC): ', auc, aucCI)
  
  #res <- c(sepline, pre, sepline, a,b,c,d,e, ' ', f)
  res <- HTML(paste(sepline, pre, sepline, a,b,c,d,e, ' ', f, sep='<br/>'))
  
  if (method == 'AROC.sp'){
    f1 <-  signif(as.numeric(summary(arocobj$fit.h)$fstatistic[1]), 4)
    f2 <- summary(arocobj$fit.h)$fstatistic[2]
    f3 <- summary(arocobj$fit.h)$fstatistic[3]
    
    mrs <-  signif(as.numeric(summary(arocobj$fit.h)$r.squared), 4)
    ars <- signif(as.numeric(summary(arocobj$fit.h)$adj.r.squared), 4)
    pvalue1 <- signif(as.numeric(summary(arocobj$fit.h)$coefficients[2,4]), 4)
    
    g0 <- 'Fited Regression Model Report'
    g <- paste0('>Multiple R Squared: ', mrs)
    h <- paste0('>Adjusted R-squared: ', ars)
    k <- paste0('>F-statistic: ', f1, ' on ' , f2, ' and ', f3, ' DF')
    l <- paste0('>p-value:',pvalue1)
    

    res <- HTML(paste(res,sepline,g0,g,h,k,l,'', sep = '<br/>'))
  } 
  
  res
}


summaryCompAroc <- function(arocobj, pooledobj, cov){
  
  auc <- signif(as.numeric(arocobj[['AUC']][1]), 4)
  low <- signif(as.numeric(arocobj[['AUC']][2]), 4)
  high <- signif(as.numeric(arocobj[['AUC']][3]), 4)
  aaucCI <- paste0( ' (',low,', ',high,')')
  
  aucClassicEst <- signif(as.numeric(pooledobj[['AUC']][1]), 4)
  aucClassicLow <- signif(as.numeric(pooledobj[['AUC']][2]), 4)
  aucClassicHigh <- signif(as.numeric(pooledobj[['AUC']][3]), 4)
  classicCI <- paste0( ' (',aucClassicLow,', ',aucClassicHigh,')')
  
  sepline <- '-----'
  intro <- 'Comparison between ROC and AROC curves using the AROC method'
  covID <- paste0('Covariate being tested: ', as.character(cov))
  auccomp1 <- paste0('AUC for unajusted ROC: ', aucClassicEst, classicCI )
  auccomp2 <- paste0('AAUC for adjusted ROC curve: ', auc, aaucCI)
  
  f1 <-  signif(as.numeric(summary(arocobj$fit.h)$fstatistic[1]), 4)
  f2 <- summary(arocobj$fit.h)$fstatistic[2]
  f3 <- summary(arocobj$fit.h)$fstatistic[3]
  
  mrs <-  signif(as.numeric(summary(arocobj$fit.h)$r.squared), 4)
  ars <- signif(as.numeric(summary(arocobj$fit.h)$adj.r.squared), 4)
  pvalue1 <- signif(as.numeric(summary(arocobj$fit.h)$coefficients[2,4]), 4)
  
  g0 <- 'Fited Regression Model Report'
  g <- paste0('>Multiple R Squared: ', mrs)
  h <- paste0('>Adjusted R-squared: ', ars)
  k <- paste0('>F-statistic: ', f1, ' on ' , f2, ' and ', f3, ' DF')
  l <- paste0('>p-value:',pvalue1)
  
  res <- HTML(paste(sepline,intro,sepline, covID, auccomp1, auccomp2,sepline,g0,g,h,k,l,'',sep='<br/>'))
  
  res
}

summaryCOmp2ROC <- function(comp2rocobj, name1, name2){
  sepline <- '-----'
  intro <- 'Comparison between ROC and AROC curves using the Comp2ROC method'
  auc1 <- paste0(
    'AUC for ',name1,': ', signif(comp2rocobj[['Area1']],4),' (', 
    signif(comp2rocobj[['ICLB1']],4),', ', signif(comp2rocobj[['ICUB1']],4), ')')
  auc2 <- paste0(
    'AUC for ',name2,': ', signif(comp2rocobj[['Area2']],4),' (', 
    signif(comp2rocobj[['ICLB2']],4),', ', signif(comp2rocobj[['ICUB2']],4), ')')
  
  testdiff <- 'Test of Differences'
  zstat <- paste0('Z statistic: ', signif(comp2rocobj[['zstats']],4))
  pval <- paste0('p-value: ', signif(comp2rocobj[['pvalue1']],4))
  
  global <- paste0('Sum of Global Areas Differences (TS): ' ,
                   signif(comp2rocobj[['diff']],4),' (', 
                   signif(comp2rocobj[['ICLBDiff']],4),', ', 
                   signif(comp2rocobj[['ICUBDiff']],4), ')')
  
  
  res <- HTML(paste(sepline,intro,sepline,auc1,auc2, testdiff,zstat,pval, global,'',sep = '<br/>'))
  res
}



classicsummary <- function(curve, method, marker, resultcol) {
  aucv <- NULL
  sepline <- '-----'
  intro <- 'Report for Classic ROC Module'
  methodcar <- paste0('Method used: ', method)
  markv <- paste0('Marker: ', marker)
  restulv <- paste0('Result Column: ', resultcol)
  
  if (is.null(curve[['auc']])) {
    aucv <- paste0('AUC: ', signif(curve[['AUC']]['est'], 4))
  } else {
    aucv <- paste0('AUC: ', signif(curve[['auc']], 4))
  }
  
  
  res <- HTML(paste(sepline, intro, sepline,methodcar, markv, restulv, aucv,'', sep = '<br/>' ))
}

updateMyReactive <- function(session, newvalue, oldvalues) {
  for (val in oldvalues) {
    watid <- substr(val ,7, nchar(val))
    updateSelectInput(session, watid, selected = newvalue)
  }
}

is.not.unique <- function(totest) {
  length(unique(totest)) != 1 
}

seedifferent <- function(session, stringlist, varlist){
  res <- list()
  if (varlist[1]==varlist[2]){
    
    res <- list(varlist[3], c(stringlist[1], stringlist[2]))
    
  } else if (varlist[2]==varlist[3]) {
    
    res <- list(varlist[1], c(stringlist[2], stringlist[3]))
    
  } else{
    
    res <- list(varlist[2],c(stringlist[3], stringlist[1]))
  }
  res
}

roc.curves.plot2 <-
  function(sim1.curve,sim2.curve,mod1,mod2) {
    
    # draw the original curve
    res <- {plot(sim1.curve,col="blue",type="o",pch=16,lwd=2,yaxs="i",xaxs="i")
    plot(sim2.curve,col="red",type="o",pch=20, lwd=2,lty=3, add=TRUE)
    abline(0,1,col="gray",lty=2)
    legend(0.7,0.7,c(mod1,mod2),pch=c(16,20),col=c("blue","red"),lwd=2,lty=c(1,3), bty = "n",box.lty=0)
    mtext(paste("Empirical ROC curves"),line=3)}
    
    res
  }


