## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  mi<- NULL
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mi <- x$getinverse()
  
  if(!is.null(mi)){
    message("getting matrix inverse from cache")
    return(mi)
  }
  
  m <- x$get() # get data
  r <- solve(m,...) # calc matrix inverse
  x$setinverse(r) # add to cache
  x$getinverse() # get from cache
}
