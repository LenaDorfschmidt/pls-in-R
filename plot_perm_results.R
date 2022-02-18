#' Plots PLS component permutation results.
#'
#' \code{plot_perm_results} plots the permutation distribution of the variance explained 
#' in each PLS component, with the real variance explained.
#'
#' @param real.values The real variance explained. 1*ncomp numeric vector.
#' @param perm.distribution The permutation distribution of the variance explained. nperm*ncomp numeric matrix.
#' @param outpath The path to the location to save the plots to. String.

plot_perm_results = function(real.values,perm.distribution,outpath){
  # Check if input dimensions are correct
  if (!(any(length(real.values) == dim(perm.distribution)))){ 
    stop(paste('Dimensions of input do not match. Dimensions should be:\n ',
               'real.value = 1 x ncomp, \n ',
               'perm.distribution = nperm x ncomp'))
  }
  # If matrices are rotatet, align their dimensions
  if (length(real.values) == dim(perm.distribution)[1]) {
    perm.distribution = t(perm.distribution)  
  }
  nperm = dim(perm.distribution)[1]
  ncomp = length(real.values)
  plotstore = list()
  # Loop through components and create histograms of permutation distributions
  for (c in 1:ncomp) {
    df = data.frame(distr=perm.distribution[,c])
    pval = sum(perm.distribution[,c]>real.values[c])/nperm
    plotstore[[c]] <- ggplot(data=df, aes(distr)) + 
      geom_histogram(fill = "#009999",alpha=.5) +
      geom_vline(xintercept = real.values[c],linetype="dashed", color = "#006666",size=1.5) + 
      ggtitle(paste0('PLS Component ', toString(c))) +
      xlab('Variance Explained') +
      ylab('Count') +
      annotate(geom = 'text', label = paste0('p-value: ', pval), x = -Inf, y = Inf, hjust = -0.2, vjust = 2, size=5) +
      theme_bw() +
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14),
            plot.title = element_text(size=20,hjust=0.5))
    print(plotstore[[c]])
    pdf(paste0(outpath,'/perm.distr.pls.component.',toString(c),'.pdf'))
      print(plotstore[[c]])
    dev.off()
  }
  
}
