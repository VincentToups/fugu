#' FUGU - Function(al/ level) programming in R
#' 

#' Partial Application on the Left
#'
#' Returns a function partially applied on the left.
#' @param f a function
#' @param ... arguments list to apply on the left
#' @return g st. partialLeft(f,a,b,c)(q,r,s) == f(a,b,c,q,r,s) 
#' @keywords functional
#' @export
partialLeft <- function(f,...){
  fixed <- list(...)
  function(...){
      unfixed <- list(...)
      do.call(f,append(fixed, unfixed))
  }
}

#' Partial Application on the Right
#'
#' Returns a function partially applied on the right.
#' @param f a function
#' @param ... arguments list to apply on the left
#' @return g st. partialRight(f,a,b,c)(q,r,s) == f(q,r,s,a,b,c) 
#' @keywords functional
#' @export
partialRight <- function(f,...){
  unfixed <- list(...)
  function(...){
      fixed <- list(...)
      do.call(f,append(fixed, unfixed))
  }
}

#' Currying on the right.
#'
#' Returns a function curried applied on the right.
#' @param f a function
#' @param ... arguments list to apply on the left
#' @return g st. curryRight(f)(a,b,c)(q,r,s) == f(q,r,s,a,b,c) 
#' @keywords functional
#' @export
curryRight <- function(f){
   partialRight(partialRight,f)
}

#' Currying on the left.
#'
#' Returns a function curried applied on the left.
#' @param f a function
#' @param ... arguments list to apply on the left
#' @return g st. curryLeft(f)(a,b,c)(q,r,s) == f(a,b,c,q,r,s) 
#' @keywords functional
#' @export
curryLeft <- function(f){
    partialRight(partialLeft,f)
}

.l <- partialLeft
.r <- partialRight

#' Function application
#'
#' Returns the result of substituting args for the arguments of f.
#' @param f a function to apply
#' @param args a list of arguments
#' @return v st. ap(f,list(a,b,c)) == f(a,b,c)
#' @keywords functional
#' @export
ap <- function(f,args){    
    do.call(f,args)
}

#' Curried (left) function application
#'
#' Returns a function waiting for an argument list.
#' @param f a function to convert to a function needing a list
#' @return g st. cap(f)(list(a,b,c)) == f(a,b,c)
#' @keywords functional
#' @export
cap <- curryLeft(ap)

#' (reverse) Function Composition
#'
#' Returns a new function F which threads its values through all
#' passed in functions
#' @param ... a list of functions - all but the first must be single argument
#' @return a function g st. g(a,b,c) == f3(f2(f1(a,b,c))) etc
#' @keywords functional
#' @export
rCompose <- function(...){
    fs <- list(...)
    n <- length(fs)
    function(...){        
        intermediate <- ap(fs[[1]],list(...))
        for(i in seq(2,n)){
            intermediate <- fs[[i]](intermediate)
        }
        intermediate
    }
}

#' Function Composition
#'
#' Returns a new function F which threads its values through all
#' passed in functions
#' @param ... a list of functions - all but the first must be single argument
#' @return a function g st. g(a,b,c) == f1(f2(f3(a,b,c))) etc
#' @keywords functional
#' @export
compose <- function(...){
    ap(rCompose,rev(list(...)))
}

#' Functional cat
#'
#' Calls `cat` on its argument and then returns that value
#' @param x an argument
#' @return x
#' @keywords functional
#' @export
fcat <- function(x){
    cat(x)
    x
}
    
.r <- rCompose
.c <- compose

#' Access an index
#'
#' equivalent to o[[k]]
#' @param o an object
#' @param a key
#' @return o[[k]]
#' @keywords functional
#' @export
access <- function(o,k){
     o[[k]]
}

#' Transform an index
#'
#' equivalent to o[[k]] <- f(o[[k]])
#' @param o an object
#' @param k a key
#' @param f a function to transform o[[k]]
#' @return o
#' @keywords functional
#' @export
tr <- function(o,k,f){
    o[[k]] <- f(o[[k]])
    o
}

#' The custom constant function
#'
#' Return a function which always returns a single value
#' @param v a value
#' @return a function which returns v
#' @keywords functional
#' @export
always <- function(v){
    function(...){
        v
    }
}

#' Return x
#'
#' The identity function
#' @param x a value
#' @return x a value
#' @keywords functional
#' @export
id <- function(x){
    x
}

#' Nickname for Map
#'
#' Map f over lists
#' @param f a function of N arguments
#' @param ... N lists
#' @return a list st. map(f,l1,l2,l3) -> list(f(l1[[1]],l2[[1],l3[[1]]... etc
#' @export
map <- Map

#' Nickname for Reduce
#'
#' Reduce elements of the list with a function
#' @param f (ac,it) -> ac, a function from an accumulating value and a current item to the next value
#' @param lst a list of items
#' @param init an initial value - otherwise lst[[1]] is used
#' @keywords functional
#' @export
reduce <- Reduce

#' The cleave operator
#'
#' Apply a list of functions to a value, returning a list
#' @param fs a list of functions
#' @param v a value
#' @keywords functional
#' @return a list of values [f1(v),f2(v)...]
#' @export
cleave <- function(fs,v){
  map(function(f){ f(v) }, fs)
}

#' The deal operator
#'
#' Apply a list of functions to an equally long list of values
#' @param fs a list of functions
#' @param vs a list of values
#' @return a list of values [f1(v1),f2(v2)...]
#' @export
deal <- function(fs,vs){
    Map(function(f,v){ f(v) }, fs, vs)
}

#' Argument-wise composition
#'
#' Compose functions on the argument list of a function f
#' @param f a function
#' @param fs a set of functions to be applied to the arguments of f
#' @return a function g st. g(a,b,c) -> f(fs[[1]](a),fs[[2]](b), fs[[3]](c))
#' @keywords functional
#' @export
argCompose <- function(f,fs){
    function(...){
        ap(f,deal(fs,list(...)))
    }
}

#' Apply from a list of transforms
#'
#' Apply f to the results of transforming v with fs
#' @param f a function
#' @param fs a list of functions to cleave over v
#' @param v a value
#' @return q like f(fs1(v),fs2(v),...)
#' @keywords functions
#' @export
apFrom <- function(f,fs,v){
    ap(f,cleave(fs,v))
}



