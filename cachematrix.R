##############################################################################
## Name: Richard Kwon
## Course: R-Programming:Lexical Scoping Assignment
##
## Matrix Inversion
## A pair of functions that will cache the inverse of a matrix
##############################################################################

## function::makeCacheMatrix
## creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    ## initialize inverse matrix variable
    ## this variable will store the inverse matrix of x
    inv_matrix <- NULL
    
    set <- function(y) {
        x <<- y                   ## assign y (passed in) to x
        inv_matrix <<- NULL       ## set inverse matrix to NULL
    }
    get <- function () x          ## return the current value of x
    
    setinvmatrix <- function(m) inv_matrix <<- m  ## set inverse matrix value
    getinvmatrix <- function() inv_matrix         ## get inverse matrix value
    
    list(set=set, get=get, setinvmatrix=setinvmatrix, getinvmatrix=getinvmatrix)
}


## function: cacheSolve
## returns the inverse of a matrix either from cache or thru function solve() call
cacheSolve <- function(x, ...) {
    
    m <- x$getinvmatrix()         ## get the inverse matrix value
    
    if (!is.null(m)) {            ## if inverse matrix value is NOT NULL then
        print ("***** PRINTING FROM CACHE *****")
        return (m)
    }
    
    data <- x$get()               ## get the x matrix
    m <- solve(data)              ## get the inverse matrix of x
    x$setinvmatrix(m)             ## set the calculated inverse matrix value
    m
}
