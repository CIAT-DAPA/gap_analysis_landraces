##################################################################################
##################     TUNING MAXNET PARAMETERS FUNCTION    #####################
## SOURCE: https://github.com/adamlilith/enmSdm/blob/master/R/trainMaxNet.r ####
### July of 2019, Andres camilo Mendez alzate #################################
##############################################################################

trainMaxNet <- function(
data,
resp=names(data)[1],
preds=names(data)[2:ncol(data)],
regMult=c(seq(0.5, 5, by=0.5), 6:10, 12.5, 15, 17.5, 20),
classes='default',
testClasses=TRUE,
out='model',
anyway=TRUE,
verbose=FALSE,
...
) {
  
  ###########
  ## setup ##
  ###########
  
  # response and predictors
  if (class(resp) %in% c('integer', 'numeric')) resp <- names(data)[resp]
  if (class(preds) %in% c('integer', 'numeric')) preds <- names(data)[preds]
  
  # get response and predictors
  presentBg <- data[ , resp]
  data <- data[ , preds, drop=FALSE]
  
  ### get combinations of features to test for each regularization multiplier
  
  if (classes == 'default') {
    classesToTest <- if (ncol(data) > 1) {
      c('l', 'p', 'q', 'h')
    } else {
      c('l', 'q', 'h')
    }
  } else {
    classesToTest <- rep(NA, nchar(classes))
    for (i in 1:nchar(classes)) classesToTest[i] <- substr(classes, i, i)
  }
  
  # create df of 1/0 to indicate each combination of classes to test
  if (testClasses) {
    classGrid <- expand.grid(rep(list(c(1, 0)), length(classesToTest)))
    classGrid <- classGrid[-which(rowSums(classGrid) == 0), ]
    classGrid <- as.data.frame(classGrid)
  } else {
    classGrid <- data.frame(matrix(rep(1, length(classesToTest)), nrow=1))
  }
  
  names(classGrid) <- classesToTest
  if (any(classGrid$l == 0)) classGrid <- classGrid[-which(classGrid$l == 0), ]
  
  ### collate presences and BG sites
  presences <- data[which(presentBg == 1), ]
  if (class(presences) != 'data.frame') presences <- as.data.frame(presences)
  names(presences) <- names(data) # names of data to which to predict
  
  bg <- data[which(presentBg == 0), ]
  if (class(bg) != 'data.frame') bg <- as.data.frame(bg)
  names(bg) <- names(data)
  
  ##########
  ## MAIN ##
  ##########
  
  tuning <- data.frame()
  
  # for each regularization multiplier
  for (thisRegMult in regMult) {
    
    if (verbose) cat('Calculating AICc for multipler ', thisRegMult, '\n')
    
    # for each combination of class features
    for (countCombo in 1:nrow(classGrid)) {
      
      if (verbose) cat("Whit Features:", classesToTest[c(classGrid[countCombo, ]) == 1], "\n")
      
      theseClasses <- paste(classesToTest[as.logical(unlist(classGrid[countCombo, ]))], collapse='')
      
      # add dummy column if doing univariate model to avoid error in maxnet.default.regularizationMOD
      if (ncol(data) == 1 & theseClasses == 'l') {
        
        thisData <- data
        thisPresences <- presences
        thisBg <- bg
        
        thisData$DUMMY <- rep(1, nrow(thisData))
        thisPresences$DUMMY <- rep(1, nrow(presences))
        thisBg$DUMMY <- rep(1, nrow(bg))
        
      } else {
        
        thisData <- data
        thisPresences <- presences
        thisBg <- bg
        
      }
      
      # # train model
      # model <- maxnet::maxnet(
      # p=as.vector(presentBg),
      # data=thisData,
      # f=maxnet::maxnet.formula(p=as.vector(presentBg), data=thisData, classes=theseClasses),
      # regfun=maxnet.default.regularizationMOD,
      # regmult=thisRegMult,
      # ...
      # )
      
      # train model
      model <- maxnet::maxnet(
        p=as.vector(presentBg),
        data=thisData,
        f=maxnet::maxnet.formula(p=as.vector(presentBg), data=thisData, classes=theseClasses),
        regfun=maxnet::maxnet.default.regularization,
        regmult=thisRegMult,
        ...
      )
      
      # predict presences
      predPres <- stats::predict(
        object=model,
        newdata=presences,
        type='exponential',
        ...
      )
      
      # predict to background
      predBg <- stats::predict(
        object=model,
        newdata=bg,
        type='exponential',
        ...
      )
      
      rawSum <- sum(c(predPres, predBg), na.rm=TRUE)
      
      ## calculate log likelihood
      ll <- sum(log(predPres / rawSum), na.rm=TRUE)
      
      ## number of parameters
      K <- length(model$betas)
      
      # AICc
      AICc <- -2 * ll + 2 * K + (2 * K * (K + 1)) / (sum(presentBg) - K - 1)
      
      # remember
      thisAicFrame <- data.frame(
        regMult=thisRegMult,
        n=sum(presentBg),
        classes=theseClasses,
        logLik=ll,
        K=K,
        AICc=AICc
      )
      
      tuning <- rbind(tuning, thisAicFrame)
      
    } # next combination of class features
    
    if (verbose) cat('\n')
    
  } # next reg mult
  
  # remove models with more parameters than data points that have more than 0 parameters
  tuning <- tuning[which(tuning$n >= tuning$K & tuning$K > 0), ]
  
  # re-order frame so one with lowest AICc, number of parameters, and reg mult are used (in that order, used to break ties)
  if (nrow(tuning) > 0) {
    
    tuning <- tuning[order(tuning$regMult, decreasing=TRUE), ]
    tuning <- tuning[order(tuning$AICc, tuning$K, tuning$regMult), ]
    
    tuning$deltaAICc <- tuning$AICc - min(tuning$AICc)
    tuning$relLike <- exp(-0.5 * tuning$deltaAICc)
    tuning$aicWeight <- tuning$relLike / sum(tuning$relLike)
    
  }
  
  if (verbose) {
    
    cat(tuning , ' \n')
    
  }
  
  # if user wants best model returned
  if ('model' %in% out) {
    
    # train model
    if (nrow(tuning) > 0) {
      
      if (anyway) {
        
        # add dummy column if doing univariate model to avoid error in maxnet.default.regularizationMOD
        if (ncol(data) == 1 & theseClasses == 'l') {
          
          thisData <- data
          thisPresences <- presences
          thisBg <- bg
          
          thisData$DUMMY <- rep(1, nrow(thisData))
          thisPresences$DUMMY <- rep(1, nrow(presences))
          thisBg$DUMMY <- rep(1, nrow(bg))
          
        } else {
          
          thisData <- data
          thisPresences <- presences
          thisBg <- bg
          
        }
        
        model <- maxnet::maxnet(
          p=as.vector(presentBg),
          data=thisData,
          f=maxnet::maxnet.formula(p=as.vector(presentBg), data=thisData, classes=if (nrow(tuning) == 0) { 1 } else { tuning$classes[1] }),
          regfun=maxnet::maxnet.default.regularization,
          regmult=if (nrow(tuning) == 0) { 1 } else { tuning$regMult[1] },
          ...
        )
        
        if (nrow(tuning) == 0) warning('No models had fewer coefficients than predictors. No model returned.', immediate.=TRUE)
        
      } else {
        
        warning('No models had fewer coefficients than predictors. No model returned.', immediate.=TRUE)
        model <- 'No MAXENT model had number of parameters < number of training presences.'
        
      }
      
    }
    
  }
  
  # return stuff
  if ('model' %in% out & !('tuning' %in% out)) {
    model
  } else if (!('model' %in% out) & 'tuning' %in% out) {
    tuning
  } else {
    list(tuning=tuning, model=model)
  }
  
}