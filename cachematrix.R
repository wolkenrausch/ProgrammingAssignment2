## makeCacheMatrix is used to create a special "matrix" object that 
## can cache its inverse. It builds a set of functions and returns 
## the functions within a list to the parent environment.

## cacheSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve will retrieve 
## the inverse from the cache.

## --------------------------------------------------------------
## makeCacheMatrix() creates a special "matrix" object that can 
## cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## get the value of the matrix
        get <- function() x
        
        ## set the value of the inverse matrix
        setInverse <- function(inverse) inv <<- inverse
        
        ## get the value of the inverse matrix
        getInverse <- function() inv
        
        ## a list of functions is returned: 
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)      
}

## -----------------------------------------------------------
## cacheSolve() computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## gets value from the cache
         inv <- x$getInverse()
        
        ## if inv (cached value) is empty, return inv
        if(!is.null(inv)) {
                message("this returns cached data")
                return(inv)
        }
        
        ## else, get the value of the matrix and store it in 'data'
        data <- x$get()
        
        ## calculate the inverse matrix of 'data' using the solve() function
        ## this assumes matrix supplied is always invertable
        ## 'inv' will be the inverse matrix of 'data'
        inv <- solve(data, ...)
        
        ## store 'inv' (the inverse matrix) in cache  
        x$setInverse(inv)
        
        ## return the inverse matrix
        inv
}

## ----------------------------------------------------------
## solve(a, b, ...)
## a: a square numeric or complex matrix containing the coefficients of the linear system.
## b: a numeric or complex vector or matrix giving the right-hand side(s) of the linear system. 
## If missing, b is taken to be an identity matrix and solve will return the inverse of a.

