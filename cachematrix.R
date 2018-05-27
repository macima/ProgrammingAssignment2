## The two functions in this files are used to create a special object that 
## stores a matrix and cache's its inverse. The first function, makeCacheMatrix,
## creates this special matrix object and the cacheSolve function calculates its
## inverse or retrieves the  cached inverse matrix.

## makeCacheMatrix creates a special "matrix" object which can cache its inverse.
## This function also garantees that if new values are set for this "matrix" the
## cached inverse value is cleaned.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function()x
        setinv <- function(mInverse) inv <<- mInverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)        
}


## This function computes the inverse of the special "matrix" object returned by
## makeCacheMatrix. It checkes if this "matrix" inverse has already been computed 
## (and the matrix values have not changed*), in case it has the cacheSolve will 
## retrieve the cached inverse instead of recalculating it.
## *If the matrix values have changed its inverse will have been set to null by 
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv          
}

