#' matrixMetric
#'
#' Takes a binary metric function metric_func, turns it into a function that takes 2 vectors, and returns a matrix of the value.
#' 
#'
#' @usage matrixMetric(metric_func)
#'
#' @param metric_func A function that takes any 2 values and returns a single numeric value.
#' @return \code{matrixMetric} Returns a function that takes 2 vectors and returns a matrix of values, such that the (i,j)th value of the returned matrix is the metric func
#' of the ith element of the first vector and the jth value of the second. 
#'
#' @author Sam Murray <slmurray@andrew.cmu.edu>
#'
#' @export
#' @import tidyverse
matrixMetric <- function(metric_func){
  
  output <- function(vecA, vecB){
    return(outer(vecA, vecB, metric_func))
  }
}
#' applyCutoffs
#'
#' Takes a function that returns a matrix of values, returns function that returns a sparse matrix of levels corrisponding to which set of cutoffs the original return value was between.
#' For example, if the (i,j)th element in the matrix returned by matrix_func was between cutoffs n and n+1, the (i,j)th value of applyCutoffs(matrix_func, cutoffs) result would be n.
#' 
#'
#' @usage applyCutoffs(matrix_func)
#'
#' @param matrix_func A function that returns a matrix of values
#' @param cutoffs A numeric vector of cutoff values. Should always be ordered from least to greatest.
#' @param descending A boolean that signals whether the least bucket has highest score, or the greatest range has highest score. See details.
#' 
#' @details applyCutoffs assigns a score from 0 to \code{length(cutoffs)} to every numeric value(or other ordered type) depending on what cutoffs the value falls between, and whether descending is true or false.
#' If descending is false, any value in the range \code{[cutoffs[a], cutoffs[a+1])} is assigned score \code(a). If descending is true, any value in the range \code{(cutoffs[a], cutoffs[a+1]] is assigned score \code(length(cutoffs) -  a)
#' 
#' @return \code{matrixMetric} Returns a function that has the same arguments as matrix_func, but returns a sparse matrix of cutoff levels corrisponding to the greatest cutoff less than the original value.
#'  
#'
#' @author Sam Murray <slmurray@andrew.cmu.edu>
#'
#' @export
#' @import tidyverse
applyCutoffs <- function(metric_func, cutoffs, descending = TRUE){
  output <- function(...){
    
    if(is.primitive(metric_func)){
      res <- metric_func(...)
    }else{
      res <- do.call(metric_func, as.list(match.call())[-1])
    }
    
    print(res)
    #Number of total ranges produced by the cutoffs
    cutoff_num = length(cutoffs)
    
    if(descending){
      cutoff_function <- function(x){
        match(T, x <= cutoffs, nomatch = 0)
      } 
    }else{
      cutoff_function <- function(x){
        match(T, x >= cutoffs, nomatch = 0 )
      }
    }

    res[] <- vapply(res, cutoff_function, numeric(1))
    print(res)
    res <- Matrix(res, sparse = T)
    gc()
    #For each cutoff i, determines which values in the matrix fall between that cutoff.
    
    if(descending){
      out_inds <- map( 1:cutoff_num, ~which(res == (.x), arr.ind = T))
    }else{
      out_inds <- map( rev(1:cutoff_num), ~which(res == (.x), arr.ind = T))
    }
    
    
    gc() 
    return(out_inds)
  }
  if(!is.primitive(metric_func)){formals(output) <- formals(metric_func)}
  
  return(output)
}

makeGamma <- function(cutoff_func, parallelize_limit = 4500){
  output <- function(matAp, matBp, n.cores = NULL){
    if(any(class(matAp) %in% c("tbl_df", "data.table"))){
      matAp <- as.data.frame(matAp)[,1]
    }
    if(any(class(matBp) %in% c("tbl_df", "data.table"))){
      matBp <- as.data.frame(matBp)[,1]
    }
    
    matAp[matAp == ""] <- NA
    matBp[matBp == ""] <- NA
    
    if(sum(is.na(matAp)) == length(matAp) | length(unique(matAp)) == 1){
      cat("WARNING: You have no variation in this variable, or all observations are missing in dataset A.\n")
    }
    if(sum(is.na(matBp)) == length(matBp) | length(unique(matBp)) == 1){
      cat("WARNING: You have no variation in this variable, or all observations are missing in dataset B.\n")
    }
    
    if(is.null(n.cores)) {
      n.cores <- detectCores() - 1
    }
    
    matrix.1 <- as.matrix(as.numeric(matAp))
    matrix.2 <- as.matrix(as.numeric(matBp))
    
    u.values.1 <- unique(matrix.1)
    u.values.2 <- unique(matrix.2)
    
    u.values.1 <- na.omit(u.values.1)
    u.values.2 <- na.omit(u.values.2)
    
    n.slices1 <- max(round(length(u.values.1)/(4500), 0), 1) 
    n.slices2 <- max(round(length(u.values.2)/(4500), 0), 1) 
    
    limit.1 <- round(quantile((0:nrow(u.values.2)), p = seq(0, 1, 1/n.slices2)), 0)
    limit.2 <- round(quantile((0:nrow(u.values.1)), p = seq(0, 1, 1/n.slices1)), 0)
    
    temp.1 <- temp.2 <- list()
    
    n.cores2 <- min(n.cores, n.slices1 * n.slices2)
    
    for(i in 1:n.slices2) {
      temp.1[[i]] <- list(u.values.2[(limit.1[i]+1):limit.1[i+1]], limit.1[i])
    }
    
    for(i in 1:n.slices1) {
      temp.2[[i]] <- list(u.values.1[(limit.2[i]+1):limit.2[i+1]], limit.2[i])
    }
    
    
    do <- expand.grid(1:n.slices2, 1:n.slices1)
    
    if (n.cores2 == 1) '%oper%' <- foreach::'%do%'
    else { 
      '%oper%' <- foreach::'%dopar%'
      cl <- makeCluster(n.cores2)
      registerDoParallel(cl)
      on.exit(stopCluster(cl))
    }
    
    temp.f <- foreach(i = 1:nrow(do), .packages = c("Rcpp", "Matrix")) %oper% {
      r1 <- do[i, 1]
      r2 <- do[i, 2]
      cutoff_func(temp.1[[r1]], temp.2[[r2]])
    }
    
    gc()
    
    cutoff_num <- length(temp.f[[1]])
    
    
    reshape2 <- function(s) { s[[1]] }
    reshape1 <- function(s) { s[[2]] }
    
    temps <- map(1:cutoff_num, ~map(temp.f, function(y){y[[.x]]}))
    indexes<- map(temps, ~do.call('rbind', .x))
    n.values <- map(indices, ~as.matrix(cbind(u.values.1[.x[, 2]], u.values.2[.x[, 1]])))
    matches <- map(n.values, function(y){ map(seq_len(nrow(y)), ~y[.x, ]) })
    final.list <- vector(mode = "list", length = cutoff_num + 1)

    if(Sys.info()[['sysname']] == 'Windows') {
      if (n.cores == 1) '%oper%' <- foreach::'%do%'
      else { 
        '%oper%' <- foreach::'%dopar%'
        cl <- makeCluster(n.cores)
        registerDoParallel(cl)
      }
      
      
      for(bucket in 1:(cutoff_num)){
        final.list[[bucket]] <- foreach(i = 1:length(matches[[bucket]])) %oper% {
          ht1 <- which(matrix.1 == matches[[bucket]][[i]][[1]]); ht2 <- which(matrix.2 == matches[[bucket]][[i]][[2]])
          list(ht1, ht2)
        }
      }

      if(n.cores > 1){
        stopCluster(cl)
      }
    } else {
      no_cores <- n.cores
      for(bucket in 1:(cutoff_num)){
        final.list[[bucket]] <- mclapply(matches[[bucket]], function(s){
          ht1 <- which(matrix.1 == s[1]); ht2 <- which(matrix.2 == s[2]);
          list(ht1, ht2) }, mc.cores = getOption("mc.cores", no_cores))
        }
    }
      
    
    na.list <- list()
    na.list[[1]] <- which(is.na(matrix.1))
    na.list[[2]] <- which(is.na(matrix.2))
    
    final.list[[cutoff_num+1]] <- na.list
    
    final.names <- c(paste0("matches",rev(1:cutoff_num)),"nas")
    
    names(final.list) <- final.names
    class(final.list) <- c("fastLink", "gammaResult")
    
    return(final.list)
  }
  
  return(output)
}

#' bundleMetric
#'
#' Generalised way to create comparison vectors for any metric.
#' 
#'
#' @usage bundleMetric(metric_func, names, cutoffs, ... , type, typeCheck)
#'
#' @param metric_func A function that takes any 2 values that satisfy typeCheck and maps them to a real number.
#' If type = NULL and typeCheck = NULL, then metric_func will accept any pair of inputs. This may raise an error or cause undefined behavior, so if
#' both type and typeCheck are NULL it throws a warning.
#' @param names Names of the feilds to be compared using this metric
#' @param cutoffs A vector of numeric values indicating the cutoffs for each agreement level (0:length(cutoffs) agreement levels in total)
#' @param ... Any other arguments to be passed to metric_func
#' @param type A string vector of classes that metric_func can take. If typeCheck = NULL, bundleMetric will generate a typeCheck using this vector. 
#' Otherwise, type is ignored.
#' @param typeCheck A unary function that returns true if the value its passed can be used by metric_func, false otherwise.(ie if metric_func can only
#' take numeric values, a good choice for typeCheck is is.numeric). 
#' @return \code{bundleMetric} returns a list with class bundledMetric, with fields:
#'  \item{metricFunc}{A parallelized version of metric_func that takes 2 vectors of values that satisfy typeCheck and the number of cores to use in the process
#'  (if NULL, then it will use up to the maximum number of cores available - 1), and returns a list with length length(cutoffs) + 1 of the indices corresponding to each
#'  matching pattern, which can be fed directly into \code{tableCounts} and \code{matchesLink}.
#'  }
#' \item{typeCheck}{A unary function that takes a value and determines whether it can be passed to metric-func. Either same as one provided, or generated from
#' type vector, or NULL}
#' \item{varnames}{Names of the feilds to be compared using this metric.}

#'
#' @author Sam Murray <slmurray@andrew.cmu.edu>
#'
#' @export
#' @import tidyverse

bundleMetric <-  function(metric_func, names, cutoffs, ... , type = NULL, typeCheck = NULL){
  
  
  return("hio")
  
  
}



















#' bundleMatMetric
#'
#' Generalised way to create comparison vectors for any metric.
#' 
#'
#' @usage bundleMatMetric(metric_func, names, cutoffs, ... , type, typeCheck)
#'
#' @param metric_func A function that takes any 2 vectors of values(size a, b) that satisfy typeCheck and maps them to a real axb matrix.
#' If type = NULL and typeCheck = NULL, then metric_func will accept any pair of inputs. This may raise an error or cause undefined behavior, so if
#' both type and typeCheck are NULL it throws a warning.
#' @param names Names of the feilds to be compared using this metric
#' @param cutoffs A vector of numeric values indicating the cutoffs for each agreement level (0:length(cutoffs) agreement levels in total)
#' @param ... Any other arguments to be passed to metric_func
#' @param type A string vector of classes that metric_func can take. If typeCheck = NULL, bundleMetric will generate a typeCheck using this vector. 
#' Otherwise, type is ignored.
#' @param typeCheck A unary function that returns true if the value its passed can be used by metric_func, false otherwise.(ie if metric_func can only
#' take numeric values, a good choice for typeCheck is is.numeric). 
#' @return \code{bundleMetric} returns a list with class bundledMetric, with fields:
#'  \item{metricFunc}{A parallelized version of metric_func that takes 2 vectors of values that satisfy typeCheck and the number of cores to use in the process
#'  (if NULL, then it will use up to the maximum number of cores available - 1), and returns a list with length length(cutoffs) + 1 of the indices corresponding to each
#'  matching pattern, which can be fed directly into \code{tableCounts} and \code{matchesLink}.
#'  }
#' \item{typeCheck}{A unary function that takes a value and determines whether it can be passed to metric-func. Either same as one provided, or generated from
#' type vector, or NULL}
#' \item{varnames}{Names of the feilds to be compared using this metric.}

#'
#' @author Sam Murray <slmurray@andrew.cmu.edu>
#'
#' @export
#' @import tidyverse

bundleMatMetric <-  function(metric_func, varnames, cutoffs, ... , type = NULL, typeCheck = NULL){
  
  output = list()
  output$varnames <-  varnames
  if(is.null(type) && (is.null(typeCheck))){
    warning("bundleMatMetric recommends passing a type or type checking function in order to ensure that the output function is never given a type it cannot handle")
  }else if(is.null(typeCheck)){
    typeCheck <-  function(x){
      result <-  (unlist(x) == type)
      return(all(result, na.rm = TRUE))
    }
  }
  output$typeCheck = typeCheck
  
  return(3)
  
  
  
}
