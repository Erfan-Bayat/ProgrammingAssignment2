# makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
# This function takes in a matrix as an argument, creates an empty cache for storing the inverse of the matrix, and
# returns a list with four functions:
# 1. set - to set the value of the matrix
# 2. get - to get the value of the matrix
# 3. setInverse - to set the value of the inverse of the matrix in the cache
# 4. getInverse - to get the value of the inverse of the matrix from the cache
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
# This function takes in the special "matrix" object created by makeCacheMatrix function as an argument and
# checks if the inverse of the matrix has already been computed and is available in the cache.
# If the inverse is found in the cache, it is returned from the cache. Otherwise, the inverse is computed using the solve function,
# set in the cache using the setInverse function of the special "matrix" object, and returned.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
