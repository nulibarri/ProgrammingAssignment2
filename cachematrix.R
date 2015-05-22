## makeCacheMatrix and cacheSolve are used to calculate the inverse of 
## a matrix and store it for later use.

## makeCacheMatrix function stores the matrix, the function (solve), 
## and the inverse matrix (getSolve) as a list in a temporary cached 
## environment.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## create variable m
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## store matrix x and variable m in a temporary cache
    get <- function() x
    ## set get equal to the value of matrix x
    setSolve <- function(solve) m <<- solve
    ## store a function to solve an inverse
    getSolve <- function() m
    ## retrieve the solved inverse
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
    ## print a placeholder list with the cached values, the matrix, 
    ## the solve function, and the inverse
}


## This function first looks to see if there is an inverse matrix
## already calculated and stored. If it does not exist, it calculates
## the inverse using setSolve. If it does exist, it just pulls the 
## cached inverse.

cacheSolve <- function(x, ...) {
    m <- x$getSolve()
    ## Pull the fourth element (the inverse matrix) of makeCacheMtrix
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## Check to see if an inverse matrix is already stored. If so, 
    ## print stored matrix.
    data <- x$get()
    ## Identify the matrix to calculate the inverse by pulling second
    ## element of makeCacheMatrix
    m <- solve(data, ...)
    ## calculate the inverse of the matrix
    x$setSolve(m)
    ## cache the inverse.
    m
    ## print the inverse
}
