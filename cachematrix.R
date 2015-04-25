## Below two functions are used to create a matrix object that can cache its inverse
## and retrieve the cached matrix if it is available

## Function#1 = makeCacheMatrix 
## input = a square invertible matrix
## output = a list of functions
## This function creates and returns a list of functions
##    1. set the matrix = setMatrix
##    2. get the matrix = getMatrix
##    3. set the inverse of the matrix = setInvertedMatrix
##    4. get the inverse of the matrix = getInvertedMatrix
## These functions are used as the input to cacheSolve() function
## in order to get or set the inverted matrix in cache
makeCacheMatrix <- function(mat = matrix()) {
  
  # initialize the cache value to NULL
  cache <- NULL
  
  # define the matrix setter function 
  setMatrix <- function(y) {
    # use "<<-" to assign values to objects to make 
    # them available outside of the defined environment
    mat <<- y
    cache <<- NULL
  }
  
  # define the matrix getter function
  getMatrix <- function() mat
  
  # define the inverted matrix setter function, which assigns it to cache
  setInvertedMatrix <- function(inverse) cache <<- inverse
  
  # define the inverted matrix getter function, which gets the inverted matrix from cache
  getInvertedMatrix <- function() cache
  
  # return the created functions to be used as inputs
  list(setMatrix = setMatrix, getMatrix = getMatrix, getInvertedMatrix = getInvertedMatrix, setInvertedMatrix = setInvertedMatrix)
}

## Function#2 = cacheSolve
## input = output of makeCacheMatrix()
## output = inverse of the original matrix, which was the input to makeCacheMatrix()
## This function returns the inverse of a matrix 
## created with the makeCacheMatrix function.
## If the inverse is already available in the cache, 
## then this function retrieves and returns the cached object
## Otherwise, it calculates the inverse, caches it, and then returns it.

cacheSolve <- function(x, ...) {
  # first, try to get the cached matrix 
  cache <- x$getInvertedMatrix()
  
  # check if this returned any result
  # if so, then return this cached result
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  
  # since cache does not yet exist,
  # then, start by getting the matrix
  matrix = x$getMatrix()
  
  # we can only calculate the inverse with Solve 
  # if the matrix is square invertible matrix
  # so, wrap it with error handler code
tryCatch({
    cache = solve(matrix, ...)
  }
  , error = function(e) {
    print(paste("ERROR: non-invertible matrix provided : ",e))
    return(NA)
  })
  
  # set the value of the inverted matrix in the cache 
  # with the setInvertedMatrix function.
  x$setInvertedMatrix(cache)
  
  # finally retunred the inverted and chached value
  return(cache)
}

## SAMPLE RUNS
## > source("ProgrammingAssignment2/cachematrix.R")
##
## > b <- makeCacheMatrix() 
## > b$setMatrix(matrix(1:16, 4, 4)) 
## > cacheSolve(b)
## [1] "ERROR: non-invertible matrix provided :  Error in solve.default(matrix, ...): Lapack routine dgesv: system is exactly singular: U[3,3] = 0\n"
## NULL
##
## > b$setMatrix(rbind(c(1, 1/2), c(1/2, 1))) 
## > b$getMatrix() 
##       [,1] [,2]
## [1,]  1.0  0.5
## [2,]  0.5  1.0
## > cacheSolve(b)
##      [,1]       [,2]
## [1,]  1.3333333 -0.6666667
## [2,] -0.6666667  1.3333333
## > cacheSolve(b)
## getting cached data
##      [,1]       [,2]
## [1,]  1.3333333 -0.6666667
## [2,] -0.6666667  1.3333333
## > cacheSolve(b)
## getting cached data
##      [,1]       [,2]
## [1,]  1.3333333 -0.6666667
## [2,] -0.6666667  1.3333333
