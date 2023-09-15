model{
  
  #-------------------------------------## 
  
  ## Nest initiation day Model for White-headed Woodpecker
  # Ana Miller-ter Kuile
  # April 1, 2022
  
  # this is a normal model for the nest initiation day
  # for whwo in 3CFLRP in Oregon and Idaho
  # the goal of this project is to understand how forest management and other
  # environmental covariates at multiple scales influence nesting ecology
  
  # Nest initiation is a function of adult fitness, so covariates in this model
  # include those local and landscape variables that influence adult fitness,
  # including antecedent climate variables that influence food availability prior
  # to the nesting period and also availability of foraging habitat on the landscape
  
  # Attributes of the model include:
  # - spatial hierarchy of nests occurring on the same transect
  # - crossed temporal hierarchy of nests occurring in the same year
  # - treatment covariates and other relevant environmental covariates
  ## at multiple scales
  # - SAM model for antecedent temperature & precip variables
  # - data follow a  normal distribution of log-transformed data
  # - relatively uninformative priors
  # - Bayesian p-value estimation for each covariate parameter
  
  #-------------------------------------## 
  
  #-------------------------------------## 
  #Likelihood of nest initiation normal model
  #-------------------------------------## 
  
  for(i in 1:n.nests){
    
    #log-tranformed data distribution follows a normal distribution
    ## with a mean mu and an overall variation for all data
    y[i] ~ dnorm(mu[i], tau)
    
    #likelihood
    mu[i] <- 
      #transect hierarchy
      b0.transect[Transect.num[i]] +
      #crossed with year intercept
      b0.year[Year.num[i]] +
      
      #local scale
      b[1]*Trees50[i] +
      b[2]*Trees2550[i] +
      b[3]*PercPonderosa[i] +
      #climate (antecedent)
      b[4]*TmaxAnt[i] +
      b[5]*PPTAnt[i] +
      #landscape scale
      b[6]*ForestCV[i] +
      b[7]*Contag[i] +
      b[8]*LPI[i] +
      b[9]*NumPatches[i] +
      b[10]*MeanForestPatchSz[i] +
      b[11]*LandHa[i] +
      b[12]*LandBu[i] +
      #interactions
      b[13]*Trees50[i]*PercPonderosa[i] +
      b[14]*Trees2550[i]*PercPonderosa[i] +
      b[15]*TmaxAnt[i]*Trees2550[i] +
      b[16]*TmaxAnt[i]*Trees50[i] +
      b[17]*LandHa[i]*LandBu[i] +
      b[18]*TmaxAnt[i]*PPTAnt[i]
    
    #summing the antecedent values
    TmaxAnt[i] <- sum(TmaxTemp[i,]) #summing across the total number of antecedent months
    PPTAnt[i] <- sum(PPTTemp[i,])
    
    #Generating each month's weight to sum above
    for(t in 1:n.lag){ #number of time steps we're going back in the past
      TmaxTemp[i,t] <- Tmax[i,t]*wA[t] 
      PPTTemp[i,t] <- PPT[i,t]*wB[t]
    }
    
    #Some covariate data are missing, so use the following to model those 
    # missing data
    #Basing these distributions off of the distributions of the 
    # data for each variable
    Trees2550[i] ~ dnorm(mu.t25, tau.t25)
    Trees50[i] ~ dnorm(mu.t50, tau.t50)
    PercPonderosa[i] ~ dnorm(mu.pp, tau.pp)
    
    for(t in 1:n.lag){
      Tmax[i,t] ~ dnorm(mu.tmp[t], tau.tmp[t])
      PPT[i,t] ~ dnorm(mu.ppt[t], tau.ppt[t])
    }
    
    #-------------------------------------## 
    # Goodness of fit parameters ###
    #-------------------------------------##
    
    #replicated data
    yrep[i] ~ dnorm(mu[i], tau)
    
    #residuals
    resid[i] <- y[i] - mu[i]
    
    #-------------------------------------## 
    # Model selection parameters ###
    #-------------------------------------##
    
    #WAIC
    lpd[i] <- logdensity.norm(y[i], mu[i], tau)
    pd[i] <- exp(lpd[i])
    
    #Dinf
    sqdiff[i] <- pow(yrep[i] - y[i], 2)
  }
  
  #-------------------------------------## 
  # Model selection parameters ###
  #-------------------------------------##
  #Dinf
  Dsum <- sum(sqdiff[])
  
  #-------------------------------------## 
  # Priors ###
  #-------------------------------------##
  
  #HIERARCHICAL STRUCTURE PRIORS
  #hierarchical centering of transects on b0
  for(t in 1:n.transects){
    b0.transect[t] ~ dnorm(b0, tau.transect)
  }
  
  #for every year but the last one:
   for(y in 1:(n.years-1)){
     b0.year[y] ~ dt( 0, tau.year, 2)
   }
  #set the last year to be the -sum of all other years so the 
  # overall fo all year levels == 0
  b0.year[n.years] <- -sum(b0.year[1:(n.years-1)])
  
  #PRIORS for intercept/hierarchical variance
  b0 ~ dnorm(0, 1E-2)
  sig.transect ~ dunif(0, 10)
  sig.year ~ dunif(0,10)
  
  tau.transect <- 1/pow(sig.transect,2)
  tau.year <- 1/pow(sig.year,2)
 
  #overall variance
  tau ~ dgamma(0.001, 0.001)
  sig <- 1/tau
  
  #PRIORS FOR COVARIATES
  
  #for all continuous b's:
  for(i in 1:18){
    b[i] ~ dnorm(0, 1E-2) #relatively uninformative priors
  }
  
  # ANTECEDENT CLIMATE PRIORS
  #Sum of the weights for climate lag
  sumA <- sum(deltaA[]) #all the Tmax weights
  sumB <- sum(deltaB[]) #PPT weights
  #Employing "delta trick" to give vector of weights dirichlet priors
  #this is doing the dirichlet in two steps 
  #see Ogle et al. 2015 SAM model paper in Ecology Letters
  for(t in 1:n.lag){ #for the total number of lags
    #the weights for tmax - getting the weights to sum to 1
    wA[t] <- deltaA[t]/sumA
    #and follow a relatively uninformative gamma prior
    deltaA[t] ~ dgamma(1,1)
    
    #for precip:
    wB[t] <- deltaB[t]/sumB
    deltaB[t] ~ dgamma(1,1)
  }
  
  
  #PRIORS FOR IMPUTING MISSING DATA
  #Priors for mean and tau of missing covariates in the model
  mu.t25 ~ dunif(-10, 10)
  sig.t25 ~ dunif(0, 20)
  tau.t25 <- pow(sig.t25, -2)
  mu.t50 ~ dunif(-10, 10)
  sig.t50 ~ dunif(0, 20)
  tau.t50 <- pow(sig.t50, -2)
  mu.pp ~ dunif(-10, 10)
  sig.pp ~ dunif(0, 20)
  tau.pp <- pow(sig.pp, -2)
  
  # Priors for parameters in the Temp missing data model:
  for(t in 1:n.lag){
    mu.tmp[t] ~ dnorm(0, 1E-2)
    sig.tmp[t] ~ dunif(0,500)
    tau.tmp[t] <- pow(sig.tmp[t],-2)
    
    mu.ppt[t] ~ dnorm(0, 1E-2)
    sig.ppt[t] ~ dunif(0, 500)
    tau.ppt[t] <- pow(sig.ppt[t], -2)
  }
  
  
  #-------------------------------------## 
  # Covariate P-values ###
  #-------------------------------------##
  
  #generate a 1-0 vector for each covariate
  #such that 1 = + in that iteration, 0 = - in that iteration
  # the mean of this value will tell us whether something is mostly positive
  # (high mean posterior value), mostly negative (low mean posterior value)
  # or somewhree in the middle (often 0, so 0.5 mean posterior)
  
  #generate p-values for all continuous covariates
  for(i in 1:18){
    z[i] <- step(b[i])
  }
  
}