## makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

## This function assumes the input matrix is square and invertible


makeCacheMatrix <- function(x = matrix()) {
        ## initialize an empty matrix
        m <- matrix(numeric(0), nrow = 0, ncol = 0)
        
        ## create a function that sets the matrix
        set <- function(y) {
                x <<- y
                m <<- matrix(numeric(0), nrow = 0, ncol = 0)
        }
        
        ## create a function that returns the value of the matrix
        get <- function() x
        
        ##  create a function that will find the inverse of the matrix
        setsolve <- function(solve) m <<- solve
        
        ## create a function that will return the inverse matrix
        getsolve <- function() m
        list(set = set, get = get,
            setsolve = setsolve,
            getsolve = getsolve)
}


## The cachesolve function calculates the inverse of the special "matrix"
## created with the function makeCacheMatrix. However, it first checks to 
## see if the inverse has already been calculated. If so, it `get`s the 
## inverse from the cache and skips the computation. Otherwise, it calculates 
## the mean of the data and sets the value of the mean in the cache via 
## the `setSolve` function.

cacheSolve <- function(x, ...) {
        ## get the value of the inverse matrix, it may not yet be cached
        m <- x$getsolve()
        
        ## if the matrix is not empty, assume it is the inverse and return it
        if(!nrow(m)==0 && !ncol(m)==0) {
                message("getting cached data")
                return(m)
        }
        
        ## else, the matrix was empty, not yet cached, so find the inverse
        
        ## get the original matrix
        data <- x$get()
        
        ## find its inverse
        m <- solve(data, ...)
        
        ## set the value of the inverse
        x$setsolve(m)
        
        ## Return the matrix that is the inverse of 'x'
        m
}
