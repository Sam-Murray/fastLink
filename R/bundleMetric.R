
typeCheck <- function(val, check){
  if(is.character(check)){
    return(class(val) == check)
  }else if(is.function(check) | class(check) == "function"){
    return(check(val))
  }
}


#' checkArgs
#'
#' Takes a function with arguments, and returns a modified function that checks the types of each argument.
#' 
#'
#' @usage checkArgs(func, ...)
#'
#' @param func Any function with arguments.
#' @param ... Name value pairs of arguments to func, and their associated type(ie "numeric") or type check function(ie is.numeric). Check args will use either
#'
#' @details If multiple name value pairs in ... contain the same name, checkArgs will do two checks on that argument. However, this is not reccomended.
#'  
#' @example
#' #Say we have a function f that should only take a string:
#'str_only_print <- function(x){
#'  if(!is.character(x)){
#'    stop("AAAAAAAAAAAA")
#'  }else{
#'    print(x)
#'  }
#'}
#'#We can add on the argument checks using checkArgs:
#'str_only_print <-  checkArgs(str_only_print, x = is.character)
#'#Now we instead of throwing error "AAAAAAAAAAAA" when being passed a string, it will throw "Incorrect argument type for argument x in str_only_print"
#' 
#' 
#' @author Sam Murray <slmurray@andrew.cmu.edu>
#'
#'
#' @export
#' @import tidyverse
checkArgs <-  function(func, ...){
  func_name = deparse(substitute(func))
  if(!(is.function(func)|(class(func) == "function"))){
    stop(paste("checkArgs only accepts functions, but was passed something with class: ", class(func)))
  } 
  #Gets the list of arguments to check
  checkList = list(...)
  
  #Checks that the tests are valid tests:
  for(i in checkList){
    if(!(is.character(i)|is.function(i)|class(i)=="function")){
      stop("All checks passed to checkArgs must be either functions or strings denoting classes")
    }
  }
  
  #Gets the list of args for 
  argList = names(formals(func))
  #Checks that the variables being checked are in the list of args for func
  
  if(!all(names(checkList) %in% argList)){
    stop("All name value pairs passed to checkArgs must corrispond to a argument of func")
  }
  
  output <-  function(){
    my_args <- as.list(match.call())
    
    my_args <- my_args[2:length(my_args)]
    for(var_name in unique(names(checkList))){
      # print(paste0("checking var of name ", var_name))
      var_checks <- checkList[names(checkList) == var_name]
      val <- my_args[[var_name]]
      # print(paste0("Val of that var is ", val))
      # print(paste0("And the checks to perform are: ", var_checks))
      for(check in var_checks){
        if(!typeCheck(val,check)){
          
          stop(paste0("Incorrect argument class: ", class(val), " for argument: ", var_name ," in function: ", func_name))
        }
      }
    }
    
    return(do.call(func,my_args))
  }
  
  formals(output) <-  formals(func)
  
  return(output)
}


str_only_print <- function(x, y){
 if(!is.character(x)){
   stop("AAAAAAAAAAAA")
 }else{
   print(x)
 }
}


#We can add on the argument checks using checkArgs:
str_only_print <-  checkArgs(str_only_print, x = is.character, x = function(x){x == "Hello"}, y = "logical")
str_only_print("Hello", TRUE)



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
