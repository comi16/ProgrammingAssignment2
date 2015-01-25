## The function cache the inverse of a matrix

## This function creates a special matrix object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  cm<- NULL
  set<-function(y){
    x<<-y
    cm<<-NULL
  }
  get<-function() x
  setInverse <-function(solve) cm<<-solve
  getInverse<- function() cm
  list(set=set,get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of the special matrix
##returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cm<- x$getInverse()
  if(!is.null(cm)){
    message("getting cached data")
    return(cm)
  }
  data<-x$get()
  cm<-solve(data,...)
  x$setInverse(cm)
  cm
  
        
}
