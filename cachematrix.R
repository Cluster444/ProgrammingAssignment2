# A set of functions that, given a matrix, will allow the inverse of
# the matrix to be cached such that subsequent calls to solve the inverse
# will not require any computation, but rather will simply return the precomputed
# inverse from the cache.
#
# Example:
#
# Create a matrix and construct the matrix cache object
# > m1 <- matrix(1:4, 2, 2)
# > m2 <- makeCacheMatrix(m1)
# > identical(m1, m2$get())
# [1] TRUE
#
# Call cacheSolve to compute the inverse, caching the result
# > cacheSolve(m2)
# > i1 <- solve(m1)
# > identical(i1, cacheSolve(m2)) # No computation performed, uses cached value
# [1] TRUE
#
# Update the matrix with a new matrix
# > m2$set(matrix(rnorm(100), 10, 10))
# > m2$getinverse()
# [1] NULL
# > cacheSolve(m2)
# > identical(solve(m2$get()), cacheSolve(m2))
# [1] TRUE



# Constructor function that creates an object that contains a given
# matrix and a cache of its inverse. The matrix may be updated, at which point
# the cache is cleared so the inverse can be recomputed and cached again.
#
# $get - returns the matrix
# $set - updates the matrix and sets the cache to NULL
# $getinverse - returns the inverse of the matrix (may be NULL)
# $setinverse - updates the cached inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL

  get <- function() { x }

  getinverse <- function() { i }

  set <- function(y) {
    x <<- y
    i <<- NULL
  }

  setinverse <- function(inverse) {
    i <<- inverse
  }

  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# Solves the inverse of a given matrix, caching the result. Will use the cached
# result if it has already been solved. Additional arguments will be passed through
# to the R solve() function.
#
# This function requires an object returned by makeCacheMatrix().
cacheSolve <- function(x, ...) {
  i <- x$getinverse()

  if(is.null(i)) {
    i <- solve(x$get(), ...)
    x$setinverse(i)
  }

  i
}
