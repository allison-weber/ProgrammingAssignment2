## Functions to calculate and cache the inverse of a matrix
## Allison Weber
## 3/22/2022

## Creates a special "matrix" object that can cache its inverse
## parameter x- square matrix (that can be inverted)
## returns list of getter/setter functions can access data
makeCacheMatrix <- function(x = matrix()) {
    # set inverse of matrix to null initially 
    m <- NULL  
    # set value of matrix, reset inverse to null        
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # returns the matrix
    get <- function() x
    # set the inverse of the matrix to the input value
    setinverse <- function(inverse) m <<- inverse
    # returns the inverse of the original matrix
    getinverse <- function() m
    
    # provide access to 4 key functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of the special "matrix" or if possible,
## retrieves the inverse from the cache
## parameter x- special matrix created by makeCacheMatrix
## returns- inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    # get the stored inverse value, will be null if not yet calculated
    m <- x$getinverse()
    # if inverse already calculated, print message and return inverse
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # get initial matrix
    data <- x$get()
    # calculate inverse
    m <- solve(data, ...)
    # set inverse in original special matrix
    x$setinverse(m)
    # return inverse matrix
    m
}
