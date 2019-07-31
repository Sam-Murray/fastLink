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
    
    #Number of total ranges produced by the cutoffs
    cutoff_num = length(cutoffs)
    
    #If descending is true, then we want 0 to go to the first bucket with cutoff greater than or equal to it
    if(descending){
      zeroCutoff <-  match(TRUE, 0 <= cutoffs, nomatch = 0)
    }else{
      zeroCutoff <-  match(TRUE, 0 >= cutoffs, nomatch = 0)
    }
    
    
    if(zeroCutoff != 0){
      if(descending){
        
      }
    }
    print(res)
     
    
    out <- Matrix(res, sparse = T)
    gc()
    #For each cutoff i, determines which values in the matrix fall between that cutoff.
    for(i in 1:(cutoff_num)){
      if(i == 1 && descending){
        out@x[out@x <= cutoffs[1]] <- cutoffs[1]
      }else{
        if(descending){
          out@x[ cutoffs[i-1] < out@x & out@x <= cutoffs[i] ] <- cutoffs[i]
          print(out)
        }else{
          out@x[ cutoffs[i-1] <= out@x & out@x < cutoffs[i] ] <- cutoffs[i-1]
        }
      }
      gc() 
    }
    
    #Catches upper bound edge case
    if(!descending){
      out@x[out@x >= cutoffs[cutoff_num]] <- cutoffs[cutoff_num]
    }
    
    
    #Gets the index of each score
    out_inds <- map(cutoffs, ~which(out == .x, arr.ind = T))
    
    gc() 
    return(out)
  }
  if(!is.primitive(metric_func)){formals(output) <- formals(metric_func)}
  
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
