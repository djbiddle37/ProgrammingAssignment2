## makeCacheMatrix stores inverted matrices
## cacheSolve returns the cached value if it's been stored, otherwise calculates and caches it

## makeCacheMatrix
## This function creates a 'cache' to store an inverted matrix
##"i" is an initially 'empty' object that allows storage of inverted matrix (or other object)
## set 'flushes out' the old inverted matrix when we feed the function a new matrix "y"
## get returns the matrix, so that cacheSolve can calculate inverse
## setinv puts the inverted matrix in the cache 'i'
## getinv returns the inverted matrix from the cache 'i' so it doesn't need to be calculated again
## list of functions which acts as directory allowing cacheSolve to call them

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function takes an instantiation of makeCacheMatrix 'x' as an argument
## if the cache is not empty, it returns the inverted matrix that is stored there
## ...otherwise, it first gets the matrix from makeCacheMatrix
##...then calculates the inverse, stores it in the cache, and returns the result (the inverted matrix)

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
