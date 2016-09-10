## makeCacheMatrix creates a special matrix object. When cacheSolve is 
## called using this special matrix, it calculates the inverse.
## if the inverse is already calculated, it returns the cached inverse, if 
## not, it calculates and caches the inverse before returning it.


## makeCacheMatrix creates a special matrix object for use in cacheSolve
makeCacheMatrix <- function(x = matrix()) {
  
  mi<- NULL #matrix inverse
  set <- function(y){
    x <<- y
    mi <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inv) {
    mi <<- inv
  }
  
  getinverse <- function() mi
  
  list(set=set, get=get, getinverse = getinverse, setinverse = setinverse)
}


## cacheSolve returns the inverse of a matrix X created with 
## makeCacheMatrix function. If a cached inverse of the matrix is 
## available it will return that, if not, it computes the the inverse, caches
## and returns it.

cacheSolve <- function(x, ...) {
  mi <- x$getinverse()
  
  if(!is.null(mi)){
    message("getting matrix inverse from cache")
    return(mi)
  }
  
  m <- x$get() # get the matrix
  inv <- solve(m, ...) # calc matrix inverse
  x$setinverse(inv) # add to cache
  
  x$getinverse() # get from cache (same as returning inv)
}
