getDesignGroupSequential_east <- function (..., kMax = NA_integer_, alpha = NA_real_, beta = NA_real_, sided = 1L, informationRates = NA_real_, futilityBounds = NA_real_, typeOfDesign = c("OF", "P", "WT", "PT", "HP", "WToptimum", "asP", "asOF", "asKD", "asHSD", "asUser", "noEarlyEfficacy"), deltaWT = NA_real_, deltaPT1 = NA_real_, deltaPT0 = NA_real_, optimizationCriterion = c("ASNH1", "ASNIFH1", "ASNsum"), gammaA = NA_real_, typeBetaSpending = c("none", "bsP", "bsOF", "bsKD", "bsHSD", "bsUser"), userAlphaSpending = NA_real_, userBetaSpending = NA_real_, gammaB = NA_real_, bindingFutility = NA, betaAdjustment = NA, constantBoundsHP = 3, twoSidedPower = NA, delayedInformation = NA_real_, tolerance = 1e-08, efficacy_IA=NA, futility_IA=NA){
  
  para <- as.list(match.call())
  para <- para[-1]
  para <- sapply(para, function(x)eval(x, envir = .GlobalEnv), simplify = F)
  
  if ((all(is.na(efficacy_IA)) & all(is.na(futility_IA))) | length(informationRates)==1){
    tmp <- para
    tmp$efficacy_IA <- NULL
    tmp$futility_IA <- NULL
    design_final <- do.call(getDesignGroupSequential, tmp)

  }else{
    if (!any(efficacy_IA==length(informationRates))){
      stop('FA must be in the efficacy_IA')
    }
    if (!any(futility_IA==length(informationRates))){
      stop('FA must be in the futility_IA')
    }
    
    tmp <- para
    tmp$informationRates <- tmp$informationRates[efficacy_IA]
    tmp$typeBetaSpending <- 'none'
    tmp$efficacy_IA <- NULL
    tmp$futility_IA <- NULL
    design_efficacy <- suppressWarnings(do.call(getDesignGroupSequential, tmp))
    
    tmp <- para
    tmp$informationRates <- tmp$informationRates[futility_IA]
    tmp$typeOfDesign <- 'noEarlyEfficacy'
    tmp$efficacy_IA <- NULL
    tmp$futility_IA <- NULL
    design_futility <- suppressWarnings(do.call(getDesignGroupSequential, tmp))
    
    as <- rep(0, length(informationRates))
    as[efficacy_IA] <- design_efficacy$alphaSpent
    for (i in 2:(length(informationRates))){
      if (as[i] < as[i-1]){
        as[i] <- as[i-1]
      }
      if (as[i] > alpha){
        as[i] <- alpha
      }
    }
    
    bs <- rep(0, length(informationRates))
    if (!all(is.na(design_futility$betaSpent))){
      bs[futility_IA] <- design_futility$betaSpent
      for (i in 2:length(informationRates)){
        if (bs[i] < bs[i-1]){
          bs[i] <- bs[i-1]
        }
        if (bs[i] > beta){
          bs[i] <- beta
        }
      }
      tmp <- para
      tmp$typeBetaSpending <- 'bsUser'
      tmp$typeOfDesign <- 'asUser'
      tmp$userAlphaSpending <- as
      tmp$userBetaSpending <- bs
      tmp$efficacy_IA <- NULL
      tmp$futility_IA <- NULL
      design_final <- suppressWarnings(do.call(getDesignGroupSequential, tmp))
    }else{
      tmp <- para
      # tmp$typeBetaSpending <- 'bsUser'
      tmp$typeOfDesign <- 'asUser'
      tmp$userAlphaSpending <- as
      # tmp$userBetaSpending <- bs
      tmp$efficacy_IA <- NULL
      tmp$futility_IA <- NULL
      design_final <- suppressWarnings(do.call(getDesignGroupSequential, tmp))
    }

  }
  

  
  attr(design_final, 'original_aug') <- para
  return(design_final)
}