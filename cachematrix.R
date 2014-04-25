## These functions create a special kind of matrix which can be 
## used to cache the inverse of a matrix rather than calculating it repeatedly.

## The makeCacheMatrix function creates a list of functions which cache the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  ## Creates the inverse, initialized to NULL  
  inv  <- NULL
  ##Creating the functions to get and set both the matrix and its inverse  
  set  <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function computes the inverse of a matrix, unless the inverse of 
## the matrix is already in the cache, in which case it returns the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Check to see if the inverse is already cached
  inv  <- x$getinverse()
  ## Return the cached inverse if it is there
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  ## Since the inverse is not cached, calculate the inverse and cache it
  data  <- x$get()
  inv  <- solve(data, ...)
  x$setinverse(inv)
  ## Return the newly calculated inverse
  inv
}
