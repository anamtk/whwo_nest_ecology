model{
  
  #-------------------------------------## 
  
  ## Nestling survival model for White-headed Woodpecker
  # Ana Miller-ter Kuile
  # April 1, 2022
  
  # this is a binomial model of the number of the number of fledglnigs
  # based on the number of nestlings in each nest. 
  
  # Nestling survival is a function of the same variables as 
  #the survival model,which are related to adult fitness, 
  #predation risk, and nest climatic conditions
  
  # Attributes of the model include:
  # - treatment covariates and other relevant environmental covariatest 
  ##  at multiple scales
  # - crossed hierarchical structure with a spatial hierarchy of transect
  ##  and a temporal hierarchy of the survey year
  # - relatively uninformative priors for all variables
  # - Bayesian p-value for every covariate parameter
  
  #-------------------------------------## 
  
  #-------------------------------------## 
  #Likelihood of nestling binomial model
  #-------------------------------------## 
  for(i in 1:n.nests){
    
    # observed fledglings is a binomial distribution
    # around survival probability and the number
    # of initial nestlings in the nest
    y[i] ~ dbin(p[i], N.nestlings[i])
    
    #Likelihood for survival probability with covariates
    logit(p[i]) <- 
      #hierarchical effects
      b0.transect[Transect.num[i]] +
      b0.year[Year.num[i]] +
      
      #categorical covariates
      #treatment categorical
      b1TrtID[TreatmentID[i]] +
      #nest categorical
      b2SpeciesID[SpeciesID[i]] +
      #continuous covariates
      #nest continuous
      b[3]*NestHt[i] +
      b[4]*CosOrientation[i] +
      b[5]*InitDay[i] +
      #local continuous
      b[6]*Trees50[i] +
      b[7]*Trees2550[i] +
      b[8]*PercPonderosa[i] +
      #landscape covariates
      b[9]*Tmax[i] +
      b[10]*Tmax[i]^2 +
      b[11]*PPT[i] +
      b[12]*PPT[i]^2 +
      b[13]*PForest[i] +
      b[14]*NPatches[i] +
      b[15]*LandBu[i] +
      b[16]*LandHa[i] +
      #interactions
      b[17]*Trees50[i]*PercPonderosa[i] +
      b[18]*Trees2550[i]*PercPonderosa[i] +
      b[19]*Trees50[i]*Tmax[i] +
      b[20]*Trees2550[i]*Tmax[i] +
      b[21]*LandHa[i]*LandBu[i] 
    
    
    ## IMPUTING MISSING COVARIATE DATE
    #Some covariate data are missing for some nests,
    #so use the following to model those missing data
    #Basing these distributions off of the distributions of the 
    # data for each variable
    Trees2550[i] ~ dnorm(mu.t25, tau.t25)
    Trees50[i] ~ dnorm(mu.t50, tau.t50)
    PercPonderosa[i] ~ dnorm(mu.pp, tau.pp)
    InitDay[i] ~ dnorm(mu.init, tau.init)
    CosOrientation[i] ~ dnorm(mu.orient, tau.orient)
    
    #making temperature and dependent on forest location,
    #since they have slightly different climates
    Tmax[i] ~ dnorm(mu.tmax[Forest.num[i]], tau.tmax[Forest.num[i]])
    
    #-------------------------------------## 
    # Derived variables ###
    #-------------------------------------##
    
    #replicated data 
    yrep[i] ~ dbin(p[i], N.nestlings[i])
    
    #residuals for goodness-of-fit
    resid[i] <- y[i] - p[i]
    
  }
  
  # 
  #-------------------------------------## 
  # Priors ###
  #-------------------------------------##
  
  #HIEARCHICAL PRIORS
  ##hierarchical centering of transects on b0
  #(for identifiability)
  for(t in 1:n.transects){
    b0.transect[t] ~ dnorm(b0, tau.transect)
  }
  
  #Sum-to-zero method for yearly effects
  #(for identifiability)
  #for every year but the last one:
  for(y in 1:(n.years-1)){
    b0.year[y] ~ dnorm( 0, tau.year)
  }
  #set the last year to be the -sum of all other years so the 
  # overall fo all year levels == 0
  b0.year[n.years] <- -sum(b0.year[1:(n.years-1)])
  
  #Random and intercept priors
  b0 ~ dnorm(0, 1E-2)
  #variation by transect
  sig.transect ~ dunif(0,10)
  tau.transect <- 1/pow(sig.transect,2)
  #variation by year
  sig.year ~ dunif(0, 10)
  tau.year <- 1/pow(sig.year,2)
  
  #FIXED COVARIATE PRIORS
  #categorical variables
  #Treatment covariates
  for(tt in 2:n.trt){
    b1TrtID[tt] ~ dnorm(0, 1E-2)
  }
  b1TrtID[1] <- 0
  
  for(s in 2:n.species){
    b2SpeciesID[s] ~dnorm(0, 1E-2)
  }
  b2SpeciesID[1] <- 0
  #continuous covariates
  for(i in 3:21){
    b[i] ~ dnorm(0, 1E-2)
  }
  
  #DATA IMPUTING PRIORS
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
  mu.init ~ dunif(-10, 10)
  sig.init ~ dunif(0, 20)
  tau.init  <- pow(sig.init, -2)
  mu.orient ~ dunif(-10, 10)
  sig.orient ~ dunif(0, 20)
  tau.orient <- pow(sig.orient, -2)
  
  #these need to be indexed by forest ID
  for(f in 1:n.forests){
    mu.tmax[f] ~ dunif(-10, 10)
    sig.tmax[f] ~ dunif(0, 20)
    tau.tmax[f] <- pow(sig.tmax[f], -2)
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
  z.b1 <- step(b1TrtID)
  z.b2 <- step(b2SpeciesID)
  
  #generate p-values for all continuous covariates
  for(i in 3:21){
    z[i] <- step(b[i])
  }

}