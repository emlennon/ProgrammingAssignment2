## Create a matrix that has a cache-able inverse

## makeCacheMatrix creates a matrix that can
#  have a cached inverse.  This means a matrix
#  created by this function will need to have
#  its inverse only calculated once
makeCacheMatrix <- function(x = matrix()) {
   inverse<-NULL
   set<-function(y) {
       x<<-y
       inverse<<-NULL
     }
   get<-function() { x }
   setinverse<-function(inv) { inverse <<- inv }
   getinverse<-function() { inverse }
   list(set=set,
        get=get,
        setinverse=setinverse,
        getinverse=getinverse)
}

## cacheSolve returns the inverse of a matrix
#  if it's already been calculated, return the
#  precalculated value.  Otherwise compute the 
#  inverse and return it
cacheSolve <- function(x, ...) {
    inverse<-x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data<-x$get()
    inverse<-solve(data,...)
    x$setinverse(inverse)
    inverse
}
