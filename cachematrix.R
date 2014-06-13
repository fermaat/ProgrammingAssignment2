##
## This function allows the user to create a matrix object
## which will be able to cache its inverse
##
makeCacheMatrix <- function(x = matrix()){ 
  inverse <- NULL
  set <- function(y) {
          x <<- y
    inverse <<- NULL 
    }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  ## inverse matrix will be stored in here
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##
## This function allows the user to obtain the inverse matrix of a given one
## by first looking at the cache data in order to get a faster result
##
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) { 
    ## message will be displayed when inverse is already in cache
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
