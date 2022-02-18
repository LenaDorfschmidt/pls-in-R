#' Plots the variance explained by PLS components
#'
#' @description Plots the variance explained by a PLS model in X and Y and save the plot. 
#' 
#' 
#' @param varX Variance explained in X 
#' @param varY Variance explained in Y
#' @param outpath Outputpath
#' 

plot_variance_explained = function(varX, varY,outpath){
  ncomp = length(varX)
  #data = as.table(rbind(varX, varY))
  #plot.new()
  data = data.frame(variance=c(varX, varY), comp=rep(seq(1:ncomp),2),label=c(rep('X', length(varX)),rep('Y',length(varY))))
  
  #data = data.frame(variance=c(varX, varY), 
                    #noncumvar = c(varX[1],diff(varX),rep(0,length(varX)-1),varY[1],diff(varY),rep(0,length(varY)-1)),
                    #comp=c(seq(1:ncomp),seq(2:length(varX-1)),seq(1:ncomp),seq(2:length(varX-1))),
                    #label=c(rep('X', length(varX)),rep('Y',length(varY))))
  #data= data.frame(ncomp = sort(rep(1:ncomp,ncomp)),
                   #stack = rep(1:ncomp,ncomp),
                   #)
  
  ggplot(data, aes(y=noncumvar, x=comp, fill=label)) + 
    geom_bar(stat="identity",position=position_dodge()) + #
    scale_fill_manual(values=c(rgb(0,0.7,0.7,alpha=0.7),rgb(0,0.4,0.4,alpha=0.8))) +
    facet_grid(~ comp)
  
  ggplot(data, aes(x = cat, y = value, fill = variable)) + geom_col() + facet_grid(~ person)
  
  
  g <- ggplot(data, aes(y=variance, x=comp, fill=label)) + 
    geom_bar(stat="identity",position=position_dodge()) + #
    scale_fill_manual(values=c(rgb(0,0.7,0.7,alpha=0.7),rgb(0,0.4,0.4,alpha=0.8))) +
    xlab('PLS Component') +
    ylab('Variance Explained (%)') +
    ggtitle('Variance Explained in X and Y') +
    labs(fill = "") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

  plot(g)
  
  pdf(paste0(outpath,'/variance.explained.pdf'))
    plot(g)
  dev.off()
  
}

