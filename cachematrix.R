## Put comments here that give an overall description of what your
## functions do
## This pair of functions helps to store  
## a matrix and cache its inverse, so that it can
## be retrieved, given that the matrix has not changed

## Write a short comment describing this function
## This first function creates a special "matrix" object
## that can cache its inverse. To that end, it creates a 
## list of functions 

makeCacheMatrix <- function(x = matrix()) {
  xinv<-NULL
  set<-function(y){
    x<<-y
    xinv<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) xinv<<-solve
  getinverse<-function() xinv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## This second function computes the inverse, hence 
## the use of the solve. The if conditional helps to check
## if the inverse has already been computed to avoid
## redundant computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xinv<-x$getinverse()
  if(!is.null(xinv)){
    message("getting cached data")
    return(xinv)
  }
  data<-x$get()
  xinv<-solve(data,...)
  x$setinverse(xinv)
  xinv
}
