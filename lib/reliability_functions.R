plotReliability = function(df, variant = NULL, xlab = 'Reliability', columnName = 'Reliability',
                           rightCensor){
  #filter by variant if specified
  if (!is.null(variant)) {
    dfPlot = df[df$Variant==variant,]
    title.suffix = paste("(",variant,")",sep="")
  } else {
    dfPlot = df
    title.suffix = ""
  }
  
  if(rightCensor){
    
      plot1 = DOTE_hist(data = subset(dfPlot, is_cen == 0 & TestPhase == "DT")[[columnName]],
                        xlab = xlab, ylab = 'Frequency', title = paste("DT Exact Failures",title.suffix))
      
      plot2 = DOTE_hist(data = subset(dfPlot, is_cen == 0 & TestPhase == "OT")[[columnName]],
                        xlab = xlab, ylab = 'Frequency', title = paste("OT Exact Failures",title.suffix))
      
      plot3 = DOTE_hist(data = subset(dfPlot, is_cen == 1 & TestPhase == "DT")[[columnName]],
                        xlab = xlab, ylab = 'Frequency', title = paste("DT Right-Censored Points",title.suffix))
      
      plot4 = DOTE_hist(data = subset(dfPlot, is_cen == 1 & TestPhase == "OT")[[columnName]],
                        xlab = xlab, ylab = 'Frequency', title = paste("OT-Right Censored Points",title.suffix))
      
      grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
    
    }else{
  
  
    plot1 = DOTE_hist(data = subset(dfPlot, TestPhase == "DT")[[columnName]],
                      xlab = xlab, ylab = 'Frequency', title = paste("DT Observations",title.suffix))
    
    plot2 = DOTE_hist(data = subset(dfPlot, TestPhase == "OT")[[columnName]],
                      xlab = xlab, ylab = 'Frequency', title = paste("OT Observations",title.suffix))
    
    grid.arrange(plot1, plot2, ncol = 2)
    
  }
}

sliderRanges = function(data){
  fit = fitdist(data, "weibull")
  
  beta_mean_range = c(floor(fit$estimate[1] - 2*fit$sd[1]), ceiling(fit$estimate[1] + 2*fit$sd[1]))
  eta_mean_range = c(floor(fit$estimate[2] - 2*fit$sd[2]), ceiling(fit$estimate[2] + 2*fit$sd[2]))
  
  beta_std_range = c(0.1, ceiling(0.15*fit$estimate[1]))
  eta_std_range = c(0.1, ceiling(0.15*fit$estimate[2]))
  
  return(list(beta_mean_range,
              eta_mean_range,
              beta_std_range,
              eta_std_range))
  
}

computeWeibullPosterior_general = function(data,
                                           nVariants,   #needed because of case where data doesn't include one
                                           priorParams, #list of prior parameters
                                           stanFile,    #stan file to use
                                           chains   = 4,
                                           samples  = 50000,
                                           refresh  = 25000,
                                           columnName)
 {
  #beta ~ gamma
  print("about to build stan.data")
  if ("betashape" %in% names(priorParams)) {
    #cat("detected gamma prior for beta\n")
    stan.data <- list(
      nVariants  = nVariants,
      nData      = nrow(data),           #number of data points
      reliability = as.numeric(data[, columnName]),
      is_cen     = data$is_cen,     #censored (0 or 1)
      variant    = data$VehicleFactor,   #vehicle variant
      #prior parameters
      beta_shape = priorParams$betashape,      
      beta_rate  = priorParams$betarate,
      eta_shape  = priorParams$etashape,      
      eta_rate   = priorParams$etarate
    )
    #beta ~ normal
  } else {
    cat("detected normal prior for beta\n")
    stan.data <- list(
      nVariants  = nVariants,
      nData      = nrow(data),           #number of data points
      reliability = as.numeric(data[, columnName]),
      is_cen     = data$is_cen,     #censored (0 or 1)
      variant    = data$VehicleFactor,   #vehicle variant
      #prior parameters
      beta_mean  = priorParams$betamean,      
      beta_std   = priorParams$betastd,
      eta_shape  = priorParams$etashape,      
      eta_rate   = priorParams$etarate
    )
  }
  print(class(stan.data$eta_shape))
  print(class(stan.data$eta_shape))
  print("just built stan.data and about to start fitting")
  # Run Stan
  fit.stan <- stan(
    stanFile,
    data     = stan.data, 
    chains   = chains,
    iter     = samples,
    refresh  = refresh
  )
  print(stan.data)
  return( rstan::extract(fit.stan) ) #return mcmc results
  #return( fit.stan ) #return mcmc results
}

updateDT_general = function(prior.inputs, dtDf, nVariants, columnName,
                            variantNames){
  
  showNotification("Updating analysis. Please wait...",
  id = "updating",
  duration = NULL, closeButton = FALSE, type = "message")
  
  pr.params = priorParameters(beta.mean.pr=prior.inputs$beta_mean, beta.std.pr=prior.inputs$beta_std,
                              eta.mean.pr=prior.inputs$eta_mean, eta.std.pr=prior.inputs$eta_std)
  
  showNotification("Sampling for DT inference... (1/3)",
                   id = "dt_starting",
                   duration = NULL, closeButton = FALSE, type = "message")

  mcmc.dt = computeWeibullPosterior_general(data = dtDf,nVariants,pr.params,"stan/weibull_gamma_gamma.stan", columnName = columnName)
  beta.dt = as.vector(mcmc.dt$beta)  #beta column
  eta.dt  = as.matrix(mcmc.dt$eta) #eta columns
  
  showNotification("DT inference complete. Fitting Priors to Results from DT Inference... (2/3)",
                   id = "dt_complete",
                   duration = 5, closeButton = FALSE, type = "message")
  removeNotification("dt_starting")

  thinFactor = 10
  nSamples = nrow(eta.dt)
  thinIdx = seq(1,nSamples,thinFactor) #indices used for thinning
  
  eta.shape = array(rep(0,nVariants))
  eta.rate  = array(rep(0,nVariants))
  eta.fit.list = list()
  
  for(i in 1:nVariants) { 
    #deal with special case where DT doesn't include a variant
    #(in this case fitdist() errors out)
    if (nrow(dtDf[dtDf$Variant==variantNames[i],])>0) {
      suppressWarnings(fit.eta <- fitdist(eta.dt[thinIdx,i], distr = "gamma", method = "mle"))
      eta.shape[i] = as.numeric(fit.eta$estimate["shape"])
      eta.rate[i]  = as.numeric(fit.eta$estimate["rate"])
      eta.fit.list[[i]] = fit.eta
    } else {
      eta.shape[i] = pr.params$etashape
      eta.rate[i]  = pr.params$etarate
    }
  }
  
  suppressWarnings(fit.beta <- fitdist(beta.dt[thinIdx], distr = "norm", method = "mle"))
  beta.mean = as.numeric(fit.beta$estimate["mean"])
  beta.std  = as.numeric(fit.beta$estimate["sd"])
  
  dt_parameters = list(beta.mean = beta.mean, beta.std = beta.std,
                       eta.rate = eta.rate, eta.shape = eta.shape)
  
  showNotification("Done fitting priors. Creating plots... (3/3)",
                   id = "fitting_complete",
                   duration = NULL, closeButton = FALSE, type = "message")
  
  p = DOTE_hist(beta.dt, title = "DT inference on shape parameter (prior in blue)", xlab = 'Shape Parameter', ylab = 'Density')
  range = layer_scales(p)$x$get_limits()
  plot.x = seq(range[1], range[2], length.out = 1000)
  prior_line = geom_line(data = data.frame(x = plot.x), aes(x=x,y=dgamma(x,pr.params$betashape,rate = pr.params$betarate)),color="blue", size = 1)
  p = p + prior_line
  dt_beta_plot = grid.arrange(p)
  
  dt_eta_plots = list()
  for(i in 1:nVariants){
    plot.x = seq(0,max(eta.dt[,i]),length.out = 1000)
    p = DOTE_hist(eta.dt[,i], title = paste("DT inference on scale parameter [",variantNames[i],"] (prior in blue)",sep=""),
                  xlab = paste("scale parameter [",variantNames[i],"]",sep=""), y = 'Density') 
    range = layer_scales(p)$x$get_limits()
    plot.x = seq(range[1], range[2], length.out = 1000)
    prior_line = geom_line(data = data.frame(x = plot.x), aes(x=x,y=dgamma(x,pr.params$etashape,rate = pr.params$etarate)),color="blue", size = 1)
    p = p + prior_line

    dt_eta_plots[[i]] = grid.arrange(p)
  }
  
  removeNotification("updating")
  removeNotification("fitting_complete")
  showNotification("Analysis update complete.",
                   id = "complete",
                   duration = 5, closeButton = FALSE, type = "message")
  
  return_plots = list(dt_beta_plot = dt_beta_plot,
                      dt_eta_plots = dt_eta_plots,
                      fit.beta = fit.beta,
                      eta.fit.list = eta.fit.list,
                      dt_parameters = dt_parameters,
                      mcmc.dt = mcmc.dt,
                      pr.params = pr.params,
                      fit.beta = fit.beta,
                      eta.fit.list = eta.fit.list)
  
  return(return_plots)
}

priorParameters = function(beta.mean.pr=1, beta.std.pr=2,
                           eta.mean.pr=1000, eta.std.pr=2000){
  #beta parameters
  beta.mean.pr = beta.mean.pr; #beta = 1 is sort of the default
  beta.std.pr  = beta.std.pr; #large-ish stdev relative to the value
  beta.pr.params = gammaParams(beta.mean.pr,beta.std.pr)
  beta.pr.shape  = beta.pr.params$shape
  beta.pr.rate   = beta.pr.params$rate
  cat(paste("beta prior parameters:\n  shape =",beta.pr.shape,"\n  rate = ",beta.pr.rate,"\n"))
  
  #eta parameters
  eta.mean.pr = eta.mean.pr; #mbsa = 1000 is the goal. try to be "neutral" about this
  eta.std.pr  = eta.std.pr; #large-ish stdev relative to the value
  eta.pr.params = gammaParams(eta.mean.pr,eta.std.pr)
  eta.pr.shape  = eta.pr.params$shape
  eta.pr.rate   = eta.pr.params$rate
  cat(paste("eta prior parameters:\n  shape =",eta.pr.shape,"\n  rate = ",eta.pr.rate,"\n"))
  
  return(list(betashape = beta.pr.shape, betarate = beta.pr.rate,
              etashape = eta.pr.shape, etarate = eta.pr.rate))
}

gammaParams = function(mu,sigma) {
  a = (mu/sigma)^2; #scale
  b = mu/(sigma^2); #rate
  return( list(shape=a,rate=b) )
}

impliedMmbsa = function(n = 1000, prior.params) {
  y_beta=rgamma(n, shape = prior.params$betashape, rate = prior.params$betarate)
  y_eta =rgamma(n, shape = prior.params$etashape , rate = prior.params$etarate )
  y_mmbsa = weibullMean(y_beta,y_eta)
  return(y_mmbsa)
}

weibullMean = function(p_shape,p_scale) {
  return( p_scale*gamma(1+(1/p_shape)) )
}

plot_normal_fit_result = function(fit_object){
  data = fit_object$data
  x = seq(min(data), max(data), length.out = 1000)
  y = dnorm(x, fit_object$estimate[1], fit_object$estimate[2])
  df = data.frame(x=x, density=y)
  
  density_plot = DOTE_hist(data, title = 'Empirical and Theoretical Density', xlab = 'Data', ylab = 'Density') +
    geom_line(color = 'red', data = df, aes(x = x, y = density)) + theme(aspect.ratio = 0.5)
  
  sorted_data = sort(data)
  cdf_df = data.frame(
    cdf_x = sorted_data,
    cdf_y = seq(1, length(sorted_data)) / length(sorted_data)
  )
  
  tcdf_x = seq(min(data), max(data), length.out = 1000)
  tcdf_y = pnorm(tcdf_x, fit_object$estimate[1], fit_object$estimate[2])
  tcdf_df = data.frame(x=tcdf_x, y=tcdf_y)
  
  cdf_plot = ggplot(cdf_df, aes(x=cdf_x, y=cdf_y), aspect.ratio = 2) + geom_point() +
    labs(x = 'Data', y = 'CDF', title = 'Empirical and Theoretical CDFs') +
    geom_line(data = tcdf_df, aes(x=tcdf_x, y=tcdf_y), color = 'red')+ theme(aspect.ratio = 0.5)
  
  qq_df = data.frame(y = data)
  
  qq_plot = ggplot(qq_df, aes(sample = y)) +
    stat_qq(distribution = stats::qnorm, dparams = list(fit_object$estimate[1], fit_object$estimate[2])) +
    stat_qq_line(distribution = stats::qnorm,
                 dparams = list(fit_object$estimate[1], fit_object$estimate[2]),
                 color = 'red') + theme(aspect.ratio = 0.5) +
    labs(x = 'Theoretical Quantiles', y = 'Empirical Quantiles', title = 'Q-Q Plot')
  
  pp_data = data.frame(
    theoretical = pnorm(sorted_data, fit_object$estimate[1], fit_object$estimate[2]),
    empirical = (1:length(sorted_data)) / (length(sorted_data) + 1)
  )
  
  tpp_data = data.frame(x = seq(0, 1, length.out = 1000),
                        y = seq(0, 1, length.out = 1000))
  
  pp_plot = ggplot(data = pp_data, aes(x = theoretical, y = empirical), aspect.ratio = 2) + geom_point() +
    labs(x = 'Theoretical Probabilities', y = 'Empirical Probabilities', title = 'P-P Plot') +
    geom_line(data = tpp_data, aes(x=x, y=y), color = 'red') + theme(aspect.ratio = 0.5)
  
  p = grid.arrange(density_plot, qq_plot, cdf_plot, pp_plot, ncol = 2)
  return(p)
}

plot_gamma_fit_result = function(fit_object){
  
  data = fit_object$data
  x = seq(min(data), max(data), length.out = 1000)
  y = dgamma(x, fit_object$estimate[1], fit_object$estimate[2])
  df = data.frame(x=x, density=y)
  
  density_plot = DOTE_hist(data, title = 'Empirical and Theoretical Density', xlab = 'Data', ylab = 'Density') +
    geom_line(color = 'red', data = df, aes(x = x, y = density)) + theme(aspect.ratio = 0.5)
  
  sorted_data = sort(data)
  cdf_df = data.frame(
    cdf_x = sorted_data,
    cdf_y = seq(1, length(sorted_data)) / length(sorted_data)
  )
  
  tcdf_x = seq(min(data), max(data), length.out = 1000)
  tcdf_y = pgamma(tcdf_x, shape = fit_object$estimate[1], rate = fit_object$estimate[2])
  tcdf_df = data.frame(x=tcdf_x, y=tcdf_y)
  
  cdf_plot = ggplot(cdf_df, aes(x=cdf_x, y=cdf_y), aspect.ratio = 2) + geom_point() +
    labs(x = 'Data', y = 'CDF', title = 'Empirical and Theoretical CDFs') +
    geom_line(data = tcdf_df, aes(x=tcdf_x, y=tcdf_y), color = 'red') +
    theme(aspect.ratio = 0.5)
  
  qq_df = data.frame(y = data)
  
  qq_plot = ggplot(data = qq_df, aes(sample = y)) +
    stat_qq(distribution = stats::qgamma, dparams = list(fit_object$estimate[1], fit_object$estimate[2])) +
    stat_qq_line(distribution = stats::qgamma,
                 dparams = list(fit_object$estimate[1], fit_object$estimate[2]),
                 color = 'red') + theme(aspect.ratio = 0.5) +
    labs(x = 'Theoretical Quantiles', y = 'Empirical Quantiles', title = 'Q-Q Plot')
  
  pp_data = data.frame(
    theoretical = pgamma(sorted_data, shape = fit_object$estimate[1], rate = fit_object$estimate[2]),
    empirical = (1:length(sorted_data)) / (length(sorted_data) + 1)
  )
  
  tpp_data = data.frame(x = seq(0, 1, length.out = 1000),
                        y = seq(0, 1, length.out = 1000))
  
  pp_plot = ggplot(data = pp_data, aes(x = theoretical, y = empirical), aspect.ratio = 2) + geom_point() +
    labs(x = 'Theoretical Probabilities', y = 'Empirical Probabilities', title = 'P-P Plot') +
    geom_line(data = tpp_data, aes(x=x, y=y), color = 'red') + theme(aspect.ratio = 0.5)
  
  p = grid.arrange(density_plot, qq_plot, cdf_plot, pp_plot, ncol = 2)
  return(p)
}

updateOT = function(downweight_parameter, current_dt, otDf,
                    nVariants, columnName, variantNames, threshold){
  
  showNotification("Sampling for OT inference... (1/2)",
                   id = "ot_sampling",
                   duration = NULL, closeButton = FALSE, type = "message")
  mcmc.dt = current_dt$mcmc.dt
  dt_parameters = current_dt$dt_parameters
  eta.dt = as.matrix(mcmc.dt$eta)
  pr.params = current_dt$pr.params
  
  # Update the analysis results with the new parameter value
  var_scale  = downweight_parameter
  beta.mean = dt_parameters$beta.mean
  #dividing both shape and rate by c increases gamma distribution variance
  #by c while keeping mean unchanged
  eta.shape = dt_parameters$eta.shape / var_scale
  eta.rate  = dt_parameters$eta.rate  / var_scale
  #to increase normal distribution variance by c, multiply standard deviation by sqrt(c)
  beta.std  = dt_parameters$beta.std  * sqrt(var_scale)
  dt.params = list(betamean = beta.mean, betastd = beta.std,
                   etashape = array(eta.shape), etarate = array(eta.rate))
  
  reliability = columnName
  print("about to start computing")
  post_results = computeWeibullPosterior_general(otDf,nVariants,dt.params,
                                                 "stan/weibull_normal_gammad.stan",
                                                 columnName = reliability)
  beta.ot = as.vector(post_results$beta)
  eta.ot = as.matrix(post_results$eta)
  
  showNotification("OT inference Complete. Creating plots... (2/2)",
                   id = "ot_complete",
                   duration = NULL, closeButton = FALSE, type = "message")
  removeNotification("ot_sampling")
  
  df.list = list()
  for(i in 1:nVariants){
    variantId = i
    
    # Generate MMBSA samples from prior and create dataframe
    mmbsa.prior <- impliedMmbsa(n = 100000, pr.params)
    dfTmpPr <- data.frame(MMBSA = mmbsa.prior, method = "(1) Prior")
    
    # Calculate MMBSA samples from after DT
    mmbsa.dt <- weibullMean(as.vector(mcmc.dt$beta), as.matrix(mcmc.dt$eta))
    dfTmpDt <- data.frame(MMBSA = mmbsa.dt[, variantId], method = "(2) DT")
    
    # Calculate MMBSA samples from posterior (after OT)
    mmbsa.post <- weibullMean(beta.ot, eta.ot)
    dfTmp <- data.frame(MMBSA = mmbsa.post[, variantId], method = "(3) OT")
    
    # Combine dataframes for each phase
    dfTmpPlot <- rbind(dfTmpPr, dfTmpDt, dfTmp)
    df.list[[i]] = dfTmpPlot
  }
  
  ot_plots = list()
  textGrobs = list()
  for (i in 1:nVariants){
    plot.x = seq(1,quantile(df.list[[i]]$MMBSA, probs = 0.95),length.out = 1000)
    p = DOTE_hist(eta.dt[,i], title = paste("DT inference on scale parameter [",variantNames[i],"] (prior in blue)",sep=""),
                  xlab = paste("scale parameter [",variantNames[i],"]",sep=""), y = 'Density') +
      geom_line(data=as.data.frame(plot.x),aes(x=plot.x,y=dgamma(plot.x,pr.params$etashape,rate=pr.params$etarate)), color="blue", size = 1)
    
    p1 = DOTE_hist(eta.ot[,i], xlab = paste("scale parameter [",variantNames[i],"]",sep=""), ylab = "Density", title = paste("OT inference on scale parameter [",variantNames[i],"] (prior in blue)",sep="")) +
      geom_line(data=as.data.frame(plot.x),aes(x=plot.x,y=dgamma(plot.x,eta.shape[i],rate=eta.rate[i])), color="blue", size = 1)
    
    p2 = ggplot(df.list[[i]]) +
      geom_density(aes(x = MMBSA, y = after_stat(density), fill = method), alpha = 0.2) +
      geom_vline(xintercept = threshold, lwd = 1, color = "black", linetype = "dotted") +
      xlim(0, quantile(df.list[[i]]$MMBSA, 0.95)) +
      labs(x = paste("Mean", columnName), y = "Density", title = paste("Mean", columnName,"Distribution of",variantNames[i],"by Method")) #+ scale_fill_manual(values=plotColors)
    
    # Compute summary statistics
    #summary_stats <- c(round(quantile(df.list[[i]]$MMBSA, 0.025)),
    #round(quantile(df.list[[i]]$MMBSA, 0.975)),
    #round(median(df.list[[i]]$MMBSA)))
    
    # Create the summary string
    ot.success.df <- df.list[[i]][df.list[[i]]$MMBSA > threshold & df.list[[i]]$method == '(3) OT',]
    n.successes = nrow(ot.success.df)
    n = nrow(df.list[[i]][df.list[[i]]$method == '(3) OT',])
    summary_string <- paste0(
      #"Lower Quantile: ", summary_stats[1], "\n",
      #"Median: ", summary_stats[3], "\n",
      #"Upper Quantile: ", summary_stats[2], "\n",
      "Probability Mean ",columnName, " > ",threshold,": ", round(n.successes/n, 3), "\n"
      
    )
    
    bottom_text <- textGrob(
      label = paste(#'OT Method MMBSA Summary Statistics: ', "\#n",
        summary_string),
      gp = gpar(fontsize=20)
    )
    textGrobs[[i]] = bottom_text
    
    
    p = grid.arrange(p, p1, p2, ncol = 3, widths = c(1, 1, 1.25))
    ot_plots[[i]] = p
  }
  
  plot.x = seq(beta.mean-4*beta.std,beta.mean+4*beta.std,length.out = 1000)
  p = DOTE_hist(beta.ot, xlab = 'Shape Parameter', ylab = 'Density', title = "OT inference on shape parameter (prior in blue)") +
    geom_line(data=as.data.frame(plot.x),aes(x=plot.x,y=dnorm(plot.x,beta.mean,beta.std)), color="blue", size = 1)
  ot_beta_plot = grid.arrange(p)
  
  return_plots = list(ot_beta_plot = ot_beta_plot,
                      ot_plots = ot_plots,
                      textGrobs = textGrobs)
  
  removeNotification("ot_complete")
  showNotification("Analysis update complete.",
                   id = "complete",
                   duration = 5, closeButton = FALSE, type = "message")
  return(list(return_plots = return_plots,
              ot_sampling_results = post_results))
}
