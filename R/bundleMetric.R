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
    warning("bundleMatMetric recommends passing a type or type checking function in order to ensure that the output function is nevery given a type it cannot handle")
  }else if(is.null(typeCheck)){
    typeCheck <-  function(x){
      result <-  (unlist(x) == type)
      return(all(result, na.rm = TRUE))
    }
  }
  output$typeCheck = typeCheck
  
  metric 
  
  
  
}
