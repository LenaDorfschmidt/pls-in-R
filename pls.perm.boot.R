pls.perm.boot = function(X,Y,ncomp=NULL,nperm=NULL,nboot=NULL,col_names=NULL, outpath=NULL){
  # This function was 'translated' from MATLAB after the publication of Whitaker & Vértes (2016)
  # It estimates a PLS regression between X and Y, bootstrapping the PLS weights and calculating te component p-values
  # 
  # Inputs:
  # X -> The predicting variables, formatted as a N_observations by N_predicting_variables  matrix
  # Y -> The predicted variables, formatted as N_observations by N_predicted_variables 
  # ncomp -> optional, allows to determine the number of PLS components a priori
  # nperm -> optional, number of permutations, defaults to 1000
  # nboot -> optional, number of bootstraps, defaults to 10000
  # col_names -> optional, names of predicting_variables
  # outpath -> optional, place where plots are saved, defaults to working directory
  #
  # Returns:
  # var.expl.real ->  Variance Explained by each PLS component
  # component.pvalues ->  PLS Component P-values, estimated by permutation testing
  # pls.comp -> PLS Component weights
  
  
  # =======================================================
  #                  Step 0: Boring Data Checks      
  # =======================================================

  # Convert X and Y to numeric
  if (!is.numeric(X)) {
    X = apply(X, 2,function(x) as.numeric(as.character(x))) 
  }
  if(!is.numeric(Y)){
    if (dim(Y)[2]>1) {
      Y = apply(Y, 2,function(x) as.numeric(as.character(x)))
    }else{
      Y = as.numeric(Y)
    }
  }
  
  if(is.null(dim(Y)[2])){ # Reformat Y to matrix
    tmp = matrix(nrow=length(Y),ncol=1)
    tmp[,1] = Y
    Y = tmp
  }
  
  if (any(is.na(X)) | any(is.na(Y))) {
    print('Missing Values in Data! Cannot run PLS')
  }
  
  # Set missing arguments to defaults
  if(is.null(col_names)){
    col_names = seq(1:dim(X)[2]) # Rownames
  }
  
  if(is.null(outpath)){
    outpath='.' #output path
  }
  
  if(is.null(nboot)){
    nboot=10000 #number of bootstraps 
  }
  
  if(is.null(nperm)){
    nperm=1000 #number of permutations
  }
  
  #nsamples = ifelse(is.null(dim(Y)[2]),length(Y), dim(Y)[1])
  
  # =======================================================
  #                  Step 1: Basic PLS Model            
  # =======================================================
  
  if (dim(Y)[2]==1) {
    if(is.null(ncomp)){
      pls.model = plsreg1(X, Y)
      ncomp = dim(pls.model$x.scores)[2]
      print(paste0('Defaulting to ',ncomp,' components.'))
    }else{
      pls.model = plsreg1(X, Y, comps=ncomp)
    }
    var.expl.real = pls.model$R2
  }else{
    # Number of PLS components are chosen by cross validation
    if(is.null(ncomp)){
      pls.model = plsreg2(X, Y, comps=NULL, crosval = TRUE)
      ncomp = dim(pls.model$x.scores)[2]
      print(paste0('Crossvalidation estimated ',ncomp,' components.'))
    }else{
      pls.model = plsreg2(X, Y, comps=ncomp, crosval = TRUE)
    }
    var.expl.real = pls.model$expvar
  }
  
  # Save number of components from original model

  # Plot (Cumulative) Variance Explained
  if (dim(Y)[2]>1) {
    plot_variance_explained(pls.model$expvar[,2],pls.model$expvar[,4],outpath)
  }
  
  # =======================================================
  #          Step 2: Significance of PLS Components      
  # =======================================================
  # Does the model explain more variance than expected by chance?
  perm.var.expl = matrix(NA, ncol=ncomp, nrow=nperm)
  #pb = txtProgressBar(min = 0, max = length(nperm), style=3)
  for (perm in 1:nperm) {
    #Sys.sleep(0.1)
    #setTxtProgressBar(pb,perm)
    print(paste0('Permutation: ',perm))
    
    # Permute Y randomly
    order = sample(1:dim(Y)[1]) 
    # Recalculate PLS with permuted Y
    if (dim(Y)[2]==1) {
      perm.model = plsreg1(X, Y[order], comps=ncomp)
      # Save explained variance in Y
      perm.var.expl[perm,] = perm.model$R2
    }else{
      perm.model = plsreg2(X, Y[order,], comps = ncomp, crosval = TRUE)
      # Save explained variance in Y
      perm.var.expl[perm,] = perm.model$expvar[,3]
    }
    
  }
  # Calculate PLS component p-values
  component.pvalues = vector(length = ncomp)
  for (c in 1:ncomp) {
    # Is the real variance explained higher than in the permutation distribution?
    component.pvalues[c] = sum(var.expl.real[c]<perm.var.expl[,c])/nperm
  }
  
  # Plot permutation results
  if (dim(Y)[2]>1) {
    plot_perm_results(pls.model$expvar[,1],perm.var.expl,outpath)
  }

  # =======================================================
  #        Step 3: Significance Testing of PLS Weights 
  # =======================================================
  # We want the PLS components to correlate (not anti-correlate) with X
  # Therefor we realign each component if it anticorrelates
  R1 = cor(pls.model$x.scores,X)
  for (c in 1:ncomp) {
    if (abs(max(R1[c,])) < abs(min(R1[c,]))) {
      pls.model$x.scores[,c] = -pls.model$x.scores[,c]
      pls.model$mod.wgs[,c] = -pls.model$mod.wgs[,c]
    }
  }
  # Bootstrapping loop
  npred = dim(X)[2]
  boot.weights = array(NA,dim=c(nboot,ncomp,npred))
  #pb = txtProgressBar(min = 0, max = length(nboot), initial = 0)
  for (b in 1:nboot) {
    print(paste0('Bootstrap: ',b))
    #Sys.sleep(0.1)
    #setTxtProgressBar(pb,b)
    
    bootorder = sample(1:dim(Y)[1], replace = T) # Random order
    # Recalculate PLS with bootstrapped X and Y
    Xrand = X[bootorder,]
    Yrand = Y[bootorder,]
    if (dim(Y)[2]==1) {
      boot.model = plsreg1(Xrand,Yrand, comps = ncomp)
    }else{
      boot.model = plsreg2(Xrand,Yrand, comps = ncomp, crosval = FALSE)
    }
    for (c in 1:ncomp){
      # Realign each bootstrap component so it correlated positively with X
      if (cor(boot.model$mod.wgs[,c],pls.model$mod.wgs[,c])<0){
        boot.model$mod.wgs[,c] = -boot.model$mod.wgs[,c]
      }
      # Save bootstrap weights
      boot.weights[b,c,] = boot.model$mod.wgs[,c]
    }
  }
  
  corr.weights = weight.pvals = weights.pvals.fdr = matrix(NA,nrow=npred,ncol=ncomp)
  pval = 0.5 # Desired significance level of weights
  pls.comp = list()
  for (c in 1:ncomp) {
    boot.sd = apply(boot.weights[,c,],2,sd)
    corr.weights[,c] = pls.model$mod.wgs[,c]/boot.sd
    weight.pvals[,c] = 1-pnorm(corr.weights[,c])
    weights.pvals.fdr[,c] = p.adjust(weight.pvals[,c] ,method = 'fdr')
    
    df = data.frame(name = col_names, boot.weight = corr.weights[,c], 
                    pvalue = weight.pvals[,c], fdr.pvalue = weights.pvals.fdr[,c])
    pls.comp[[c]] = df
  }
  names(pls.comp) <- paste0('PLS',rep(1:ncomp))
  
  return(list(var.expl.real=var.expl.real,  # Variance Explained by each PLS component
              component.pvalues=component.pvalues, # PLS Component P-values, estimated by permutation testing
              pls.comp = pls.comp)) # PLS Components

}

