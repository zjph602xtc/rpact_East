getSampleSizeSurvival_east <- function(design = NULL, ..., typeOfComputation = c("Schoenfeld", "Freedman", "HsiehFreedman"), thetaH0 = 1, pi1 = NA_real_, pi2 = NA_real_, lambda1 = NA_real_, lambda2 = NA_real_, median1 = NA_real_, median2 = NA_real_, kappa = 1, hazardRatio = NA_real_, piecewiseSurvivalTime = NA_real_, allocationRatioPlanned = NA_real_, eventTime = 12, accrualTime = c(0, 12), accrualIntensity = 0.1, accrualIntensityType = c("auto", "absolute", "relative"), followUpTime = NA_real_, maxNumberOfSubjects = NA_real_, dropoutRate1 = 0, dropoutRate2 = 0, dropoutTime = 12){
  
  if (is.null(design)) stop('design cannot be NULL')
  para <- as.list(match.call())
  para <- para[-1]
  
  y <- do.call(getSampleSizeSurvival, para)
  
  tmp <- para
  tmp$maxNumberOfEvents <- ceiling(y$maxNumberOfEvents)
  tmp$directionUpper <- FALSE
  y1 <- do.call(getPowerSurvival, tmp)
  
  IF <- floor(ceiling(y$maxNumberOfEvents)*design$informationRates+0.5)/ceiling(y$maxNumberOfEvents)
  
  
  design_para <- attr(design, 'original_aug')
  design_para$beta <- 1-y1$overallReject
  design_para$informationRates <- IF
  design_new <- do.call(getDesignGroupSequential_east, design_para)
  
  tmp <- para
  tmp$design <- design_new
  res <- do.call(getSampleSizeSurvival, tmp)

  attr(res, 'original_aug') <- para
  return(res)
  
}