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
    y[i] ~ dpois(lambda[i])
    
    #likelihood
    log(lambda[i]) <- 
      #transect hierarchy
      b0.transect[Transect.num[i]] +
      #crossed with year intercept
      b0.year[Year.num[i]] +
      
      #Treatment covariates
      b1[TrtID[i]] +
      #nest scale
      b[2]*InitDay[i] +
      #local scale
      b[3]*Trees50[i] +
      b[4]*Trees2550[i] +
      b[5]*PercPonderosa[i] +
      #landscape scale
      b[6]*TmaxAnt[i] +
      b[7]*ForestCV[i] +
      b[8]*ForestProx[i] +
      b[9]*Contag[i] +
      b[10]*LandHa[i] +
      b[11]*LandBu[i] +
      #interactions
      b[12]*Trees50[i]*PercPonderosa[i] +
      b[13]*Trees2550[i]*PercPonderosa[i] +
      b[14]*TmaxAnt[i]*Trees2550[i] +
      b[15]*TmaxAnt[i]*Trees50[i] +
      b[16]*LandHa[i]*LandBu[i] 
    
    #summing the antecedent values
    TmaxAnt[i] <- sum(TmaxTemp[i,]) #summing across the total number of antecedent months
    #Generating each month's weight to sum above
    for(t in 1:n.lag){ #number of time steps we're going back in the past
      TmaxTemp[i,t] <- Tmax[i,t]*wA[t] 
    }
    
    #Some covariate data are missing, so use the following to model those 
    # missing data
    #Basing these distributions off of the distributions of the 
    # data for each variable
    Trees2550[i] ~ dnorm(mu.t25, tau.t25)
    Trees50[i] ~ dnorm(mu.t50, tau.t50)
    PercPonderosa[i] ~ dnorm(mu.pp, tau.pp)
    InitDay[i] ~ dnorm(mu.init, tau.init)
    
    for(t in 1:n.lag){
      Tmax[i,t] ~ dnorm(mu.tmp[t], tau.tmp[t])
    }
    
    #-------------------------------------## 
    # Goodness of fit parameters ###
    #-------------------------------------##
    
    #replicated data
    yrep[i] ~ dpois(lambda[i])
    
    #residuals
    resid[i] <- y[i] - lambda[i]
    
  }
  
  
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
  
  #PRIORS FOR COVARIATES
  
  #Categorical variables
  #ensure b[1,1] has most observations
  for(tt in 2:n.trt){
    b1[tt] ~ dnorm(0, 1E-2)
  }
  
  b1[1] <- 0
  
  #for all continuous b's:
  for(i in 2:16){
    b[i] ~ dnorm(0, 1E-2) #relatively uninformative priors
  }
  
  # ANTECEDENT CLIMATE PRIORS
  #Sum of the weights for climate lag
  sumA <- sum(deltaA[]) #all the Tmax weights
  #Employing "delta trick" to give vector of weights dirichlet priors
  #this is doing the dirichlet in two steps 
  #see Ogle et al. 2015 SAM model paper in Ecology Letters
  for(t in 1:n.lag){ #for the total number of lags
    #the weights for tmax - getting the weights to sum to 1
    wA[t] <- deltaA[t]/sumA
    #and follow a relatively uninformative gamma prior
    deltaA[t] ~ dgamma(1,1)
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
  mu.init ~ dunif(-10, 10)
  sig.init ~ dunif(0, 20)
  tau.init <- pow(sig.init, -2)
  
  # Priors for parameters in the Temp missing data model:
  for(t in 1:n.lag){
    mu.tmp[t] ~ dnorm(0, 1E-2)
    sig.tmp[t] ~ dunif(0,500)
    tau.tmp[t] <- pow(sig.tmp[t],-2)
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
  
  #generate p-values for all continuous covariates
  for(i in 2:16){
    z[i] <- step(b[i])
  }
  
}