## Create matrix and compute the inverse of it ,put inverse in cache so it will won't be recalculated

## Create special Matrix with setter and getter function of value and inverse of matrix respectively

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse , getinverse = getinverse)
}


## Fetch the inverse from cache if available otherwise calculate it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if( !is.null(inv)){
    message("Getting Cached Data")
    return( inv)
  }
  mdata <- x$get()
  inv <- solve(mdata, ...)
  x$setinverse(inv)
  inv
}
