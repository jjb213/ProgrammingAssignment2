## R Programming (Coursera Data Scientist Signature Track)
## Jeff Bachovchin
## 25-01-2015
##
## Assignement #2

## This function creates a special "matrix" object that can cache its inverse. The matrix passed to it is assumed
## to be invertible and no checks are made to ensure that. Errors will result if an the matrix is non-invertible.

makeCacheMatrix <- function(m = matrix()) {
        ## m is the matrix (assumed to be invertible) passed to makeCacheMatrix
        
	## i = inverse, initialize to NULL
        i <- NULL  
        
        ## sets the values of the matrix (m) to the matrix passed to it, and the inverse (i) initialize to NULL
        set <- function(y) {
                m <<- y
                i <<- NULL
        }
        
        ## simply returns the initial matrix
        get <- function() m
        
        ## sets the value of i in the parent environment to the inverse
        setinv <- function(inverse) i <<- inverse
        
        ## gets the inverse of the matrix
        getinv <- function() i
        
        ## return the list of functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
        
        ## set temp variable i to what is returned by the getinv() function of makeCacheMatrix
        i <- m$getinv()
        
        ## see if getinv() returned a value
        if(!is.null(i)) {
                
                ## if it did, say so and return it
                message("getting cached data")
                return(i)
        }
        
        ## if it didn't, calculate the inverse, save it to the cache, and return it
        data <- m$get()
        i <- solve(data, ...)
        m$setinv(i)
        i
}