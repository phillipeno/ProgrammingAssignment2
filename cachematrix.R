## Factory for creating a matrix that can store its
## inverse matrix. Also, a function that returns the
## inverse matrix, using the cached version if it is
## available.
##
## Includes one small optimization which is that it keeps the
## cache if the matrix being set is the same.
##
## Could add option to return inverse matrix as a
## CacheMatrix. If implemented, the inverse matrix should
## already be cached as the value of the original matrix.
##
## Includes one test just for fun.

## Factory for creating an instance of the CacheMatrix
## class, which is a matrix that can store its
## inverse matrix. The class supports methods for
## getting and setting the matrix (get, set) and getting
## and setting the inverse matrix (getInverse, setInverse).
## If the inverse matrix has not been set, getInverse
## returns null.
## Args: 
##   m - initial value of matrix
makeCacheMatrix <- function(m = matrix()) {
  # Holds calculated inverse matrix.
  inv <- NULL
  mat <- m
  # Setter.
  set <- function(m) {
    if (!is.matrix(m)) {
      message("Argument is not a matrix.")
      return()
    }
    # If new matrix equals the old one, don't disturb the cache.
    if (equalMatrices(m, mat)) {
      message("Matrix unchanged.")
      return()
    }
    mat <<- m
    # Clear inverse for new matrix.
    inv <<- NULL
  }
  # Getter.
  get <- function() {
    mat
  }
  # Inverse matrix setter.
  setInverse <- function(inverse) {
    if (!is.matrix(inverse)) {
      message("Argument is not a matrix.")
      return()
    }
    inv <<- inverse
  }
  # Inverse matrix getter.
  getInverse <- function() {
    inv
  }
  # Return instance of CacheMatrix class.
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Returns the inverse matrix of the CacheMatrix.
## If a cached version of the inverse is available, it
## uses it. Otherwise, it calculates the inverse and
## saves it in the cache.
## TODO: Consider returning inverse matrix as a
##       CacheMatrix.
## Args: 
##   m - CacheMatrix to get inverse matrix of.
cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = m$getInverse()
  if (!is.null(inv)) {
    message("Using cached inverse matrix.")
    return(inv)
  }
  inv = solve(m$get(), ...)
  m$setInverse(inv)
  inv
}

## Matrix equality test with tolerance.
## The tolerance allows for round off errors.
## Args: 
##   m1, m2 - Matrices to compare.
##   tolerance - Maximum difference where values are still
##               considered equal.
equalMatrices <- function(m1, m2, tolerance = 0.0) {
  if (!is.matrix(m1) || !is.matrix(m2)) {
    message("Non-matrices in matrix equality test.")
    return(FALSE)
  }
  if (!all(dim(m1) == dim(m2))) {
    return(FALSE)
  }
  # Check tolerance on difference for all elements.
  all(abs(m1 - m2) <= tolerance)
}

## Basic tests of the cacheSolve function.
cacheSolveTest <- function() {
  # Test basic usage.
  m = matrix(1:4, 2, 2)
  cm = makeCacheMatrix(m)
  message("Expect no message about using cache:")
  print(cacheSolve(cm))
  message("Expect message about using cache:")
  cacheSolve(cm)
  message("Expect message about matrix being the same.")
  cm$set(m)
  message("Expect message about using cache:")
  cacheSolve(cm)
  # Make sure we can get back the original.
  cm2 = makeCacheMatrix(cm$getInverse())
  if (!equalMatrices(cacheSolve(cm2), m, 1e-14)) {
    message("** FAIL ** Expected inverse of inverse to equal original")
    return()
  } else {
    message("Inverse of inverse was original.")
  }
  
  m2 = matrix(c(5, 2, 8, 3, 5, 7, 3, 1, 4), 3, 3)
  cm$set(m2)
  message("Expect no messages about using cache:")
  print(cacheSolve(cm))
  message("Expect one messages about using cache:")
  cmInv = makeCacheMatrix(cacheSolve(cm))
  message("Expect no messages about using cache:")
  if (!equalMatrices(cacheSolve(cmInv), m2, 1e-14)) {
    message("** FAIL ** Expected inverse of inverse to equal original")
    return()
  } else {
    message("Inverse of inverse was original.")
  }
}

cacheSolveTest()
