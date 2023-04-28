model{
  
  #-------------------------------------## 
  
  ## Egg Production Model for White-headed Woodpecker
  # Ana Miller-ter Kuile
  # April 1, 2022
  
  # this is a count model for the # of eggs for whwo in 3CFLRP in Oregon and Idaho
  # the goal of this project is to understand how forest management and other
  # environmental covariates at multiple scales influence egg production
  
  # Egg production is a function of adult fitness, so covariates in this model
  # include those local and landscape variables that influence adult fitness,
  # including antecedent climate variables that influence food availability prior
  # to the nesting period and also availability of foraging habitat on the landscape
  
  # Attributes of the model include:
  # - spatial hierarchy of nests occurring on the same transect
  # - crossed temporal hierarchy of nests occurring in the same year
  # - treatment covariates and other relevant environmental covariates
  ## at multiple scales
  # - data follow a  poisson (count) distribution 
  # - relatively uninformative priors
  # - Bayesian p-value estimation for each covariate parameter
  
  #-------------------------------------## 
  
  #-------------------------------------## 
  #Likelihood of egg productivity poison model
  #-------------------------------------## 
  
  for(i in 1:n.nests){
    
    #data distribution follows a poisson with rate parameter lambda
    y[i] ~ dnorm(mu[i], tau)
    
    #likelihood
    mu[i] <- 
      #transect hierarchy
      b0.transect[Transect.num[i]] +
      #crossed with year intercept
      b0.year[Year.num[i]] +
      
      #Treatment covariates
      b1[TrtID[i]] +
      b2[TrtTime[i]] +
      b[3]*NTrt[i] +
      #local scale
      b[4]*Trees50[i] +
      b[5]*Trees2550[i] +
      b[6]*PercPonderosa[i] +
      #landscape scale
      b[7]*PPTAnt[i] +
      b[8]*ForestCV[i] +
      b[9]*ForestProx[i] +
      b[10]*Contag[i] +
      b[11]*LandHa[i] +
      b[12]*LandBu[i] +
      #interactions
      b[13]*Trees50[i]*PercPonderosa[i] +
      b[14]*Trees2550[i]*PercPonderosa[i] +
      b[15]*PPTAnt[i]*Trees2550[i] +
      b[16]*PPTAnt[i]*Trees50[i] + 
      b[17]*LandHa[i]*LandBu[i] 
    
    #summing the antecedent values
    PPTAnt[i] <- sum(PPTTemp[i,])
    #Generating each month's weight to sum above
    for(t in 1:n.lag){ #number of time steps we're going back in the past
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
    lpd[i] <-  logdensity.norm(y[i], mu[i], tau)
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
    b0.year[y] ~ dnorm( 0, tau.year)
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
  
  tau ~ dgamma(0.001, 0.001)
  #PRIORS FOR COVARIATES
  
  #Categorical variables
  #ensure b[1,1] has most observations
  for(tt in 2:n.trt){
    b1[tt] ~ dnorm(0, 1E-2)
  }
  
  b1[1] <- 0
  
  #ensure b[2,1] has the most observations
  for(t in 2:n.times){
    b2[t] ~ dnorm(0, 1E-2)
  }
  
  b2[1] <- 0
  
  #for all continuous b's:
  for(i in 3:17){
    b[i] ~ dnorm(0, 1E-2) #relatively uninformative priors
  }
  
  # ANTECEDENT CLIMATE PRIORS
  #Sum of the weights for climate lag
  sumB <- sum(deltaB[])
  #Employing "delta trick" to give vector of weights dirichlet priors
  #this is doing the dirichlet in two steps 
  #see Ogle et al. 2015 SAM model paper in Ecology Letters
  for(t in 1:n.lag){ #for the total number of lags

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
  
  #generates per level of categorical variables
  z.b1 <- step(b1)
  z.b2 <- step(b2)
  
  #generate p-values for all continuous covariates
  for(i in 3:17){
    z[i] <- step(b[i])
  }
  
}