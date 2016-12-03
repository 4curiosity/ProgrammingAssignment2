## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object
## that can cache its inverse.
## It creates internal, cache variables, initializes them
## x and InvertedMatrix <<- NULL
## It also createes the methods to accept, store and produce the cashe data
makeCacheMatrix <- function(x = matrix()) {
    InvertedMatrix <- NULL
    set <- function(y) {
        x <<- y
        InvertedMatrix <<- NULL
    }
    get <- function() x
    setInvertedMatrix  <- function(invert) InvertedMatrix <<- invert
    getInvertedMatrix  <- function() InvertedMatrix
    list(set = set, get = get,
         setInvertedMatrix  = setInvertedMatrix ,
         getInvertedMatrix  = getInvertedMatrix )
}


## Write a short comment describing this function
## `cacheSolve`: This function compares the inverse of the special
## "matrix" returned by `makeCacheMatrix` above to NULL. If the inverse has
## already been calculated, (is not NULL), and the input matrix has not changed, then
## `cacheSolve` returns the previously cached inverted matrix
## else it solves and returns the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    InvertedMatrix <- x$getInvertedMatrix()
    if (!is.null(InvertedMatrix)) {
        message("getting cached data")
        return(InvertedMatrix)
    }
    data <- x$get()
    InvertedMatrix <- solve(data, ...)
    x$setInvertedMatrix(InvertedMatrix)
    InvertedMatrix
}

##  Tested with the folowing
##  debug(makeCacheMatrix)
##  debug(cacheSolve)
#   source(".../cachematrix.R")
#   my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
#   startTime <- Sys.time()
#   cacheSolve(my_matrix)
#   # endcacheSolve1 <- Sys.time()
#   # endcacheSolve1 - startTime
#   cacheSolve(my_matrix)
#   # Sys.time() - endcacheSolve1
#   my_matrix$getInvertedMatrix()
#   Sys.time() - startTime

