# this code was copied and modified from https://rdrr.io/cran/AICcmodavg/src/R/predictSE.R

predictSE <- function(mod, newdata, se.fit = TRUE, level = 0, multicore = NULL){
   
   require(furrr)
   
   # mod = ssf_fit; newdata = test; se.fit = TRUE; print.matrix = FALSE; level = 0; type = "response"
   # rm(test, train)
   # newdata$subad <- 0
   
   ##logical test for level
   if(!identical(level, 0)) stop("\nThis function does not support computation of predicted values\n",
                                 "or standard errors for higher levels of nesting\n")
   

      ##this part of code converts data.frame (including factors) into design matrix of model
   tt <- terms(mod)
   TT <- reformulate(all.vars(tt[[3]]))
   # Save response for later
   resp <- newdata$case
   newdata <- subset(newdata, select = -c(case))
   
   
   #################################################################################################################
   ########################### This following clever piece of code is modified from predict.lme( ) from nlme package
   #################################################################################################################  
   mfArgs <- list(formula = TT, data = newdata)
   dataMix <- do.call("model.frame", mfArgs)
   
   
   ######START OF PREDICT FUNCTION
   ######
   fix.coef <- fixef(mod)$cond
   ncoefs <- length(fix.coef)
   names.coef <- labels(fix.coef)
   ##check for intercept fixed effect term in model
   int.yes <- any(names.coef == "(Intercept)")
   
   ##m <- model.frame(TT, data = dataMix) 
   ##m <- model.frame(TT, data = newdata) gives error when offset is converted to log( ) scale within call
   des.matrix <- model.matrix(TT, dataMix)
   newdata <- if(int.yes) des.matrix else des.matrix[,-1]  #we now have a design matrix
   nvals <- dim(newdata)[1]
   
   ##if no intercept term, return error
   # if(!int.yes) stop("\nThis function does not work with models excluding the intercept\n")
   
   formula <- character(length=ncoefs)
   nbetas <- ncoefs
   
   if(int.yes & nbetas >= 1) {
      ##create loop to construct formula for derivative
      formula <- paste("Beta", 0:nbetas, sep="")
   } else if(int.yes & nbetas == 0) {
      formula <- "Beta0"
   } else {
      formula <- paste("Beta", 1:nbetas, sep="")
   }
   ##for models without intercept - formula <- paste("Beta", 1:ncoefs, sep="")
   
   
   ##a loop to assemble formula
   ##first, identify interaction terms
   inters <- rep(NA, ncoefs)
   for (m in 1:ncoefs) {
      inters[m] <- attr(regexpr(pattern = ":", text = names.coef[m]), "match.length")
   }
   
   
   ##change the name of the labels for flexibility
   names.cov <- paste("cov", 1:ncoefs-1, sep="")
   
   if(!int.yes) {names.cov <- paste("cov", 1:ncoefs, sep="")}
   
   id <- which(inters == 1)
   for (k in 1:length(id)) {
      names.cov[id[k]] <- paste("inter", k, sep="")
   }
   
   ##iterate and combine betas and covariates
   formula2 <- character(length = ncoefs)
   for(b in 1:ncoefs) {
      formula2[b] <- paste(formula[b], names.cov[b], sep="*")
   }
   ##replace with Beta0 if fixed intercept term present
   if(int.yes) {formula2[1] <- "Beta0"}
   
   ##collapse into a single equation and convert to expression
   ##parse returns the unevaluated expression
   eq.space <- parse(text  = as.expression(paste(formula2, collapse="+")),
                     srcfile = NULL)
   ##add step to remove white space to avoid reaching 500 character limit
   
   ##remove space within expression
   no.space <- gsub("[[:space:]]+", "", as.character(eq.space))
   equation <- parse(text = as.expression(no.space))
   
   
   ################################################################
   ###### FIX INTERACTIONS (THIS CODE IS MINE)
   ################################################################
   
   
   ##determine number of covariates excluding interaction terms
   ncovs <- ncoefs - length(id)
   
   ##assign values of covariates
   cov.values <- list( )
   
   for(i in seq_along(names.coef)){
      varname <- names.coef[i]
      if(i %in% id){
         varnames <- unlist(strsplit(varname, ":"))
         newdata[,varnames[1]] * newdata[,varnames[2]]
         cov.values[[i]] <- apply(newdata[,varnames], 1, "prod")
      } else {
         cov.values[[i]] <- newdata[,varname]
      }
   }
   
   names(cov.values) <- names.cov
   cov.values.mat <- matrix(data = unlist(cov.values), nrow = nvals, ncol = ncoefs)
   
   
   ################################################################
   ####use the following code to compute predicted values and SE's
   ####on response scale if identity link is used OR link scale
   
   if(identical(se.fit, TRUE)) {
      ##substitute a given row for each covariate
      predicted.SE <- matrix(NA, nrow = nvals, ncol = 2)
      colnames(predicted.SE) <- c("Pred.value", "SE")
      rownames(predicted.SE) <- 1:nvals
      
      ##extract vc matrix
      vcmat <- as.matrix(vcov(mod)[[1]])
      
      pred <- function(w, vcmat.=vcmat){
         
         mat_partialdevs <- as.matrix(w) #create matrix from vector of 2 rows by 1 column
         mat_tpartialdevs <- t(w)        #transpose of partial derivatives to have 2 columns by 1 row
         
         var_hat <- mat_tpartialdevs %*% vcmat. %*% mat_partialdevs
         SE <- sqrt(var_hat)
         predicted.vals <- fix.coef%*%w
         
         return(data.frame(fit = predicted.vals, se.fit = SE))
      }
      
      w <- lapply(1:nvals, function(x) cov.values.mat[x,])
      
      if(!is.null(multicore)) future::plan("multisession", workers = multicore)
      
      out.fit.SE <- furrr::future_map_dfr(w, ~pred(.x))   
      
      future::plan("sequential")
      
   } else {
      
      predicted.SE <- matrix(NA, nrow = nvals, ncol = 1)
      colnames(predicted.SE) <- c("Pred.value")
      rownames(predicted.SE) <- 1:nvals
      
      for (w in 1:nvals) {
         predicted.vals <- fix.coef%*%cov.values.mat[w,]
         predicted.SE[w, 1] <- predicted.vals
      }
      
      out.fit.SE <- predicted.SE
      colnames(out.fit.SE) <- "fit"
      
   }
   
   return(out.fit.SE)
}
