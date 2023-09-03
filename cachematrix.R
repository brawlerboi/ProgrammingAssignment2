## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is used to either set the elements or get the elements of either the user defined matrix, or its inverse.
## It uses set, get and setinverse, getinverse to do this.

makeCacheMatrix <- function(x = matrix()) {
        m_inverse <- NULL       ##Initialize to null first
        set <- function (y) {
                x <<- y
                m_inverse <<- NULL
        }
        get <- function() x     ##Acquires matrix x
        setinverse <- function(inverse)m_inverse <<- inverse    ##Sets the inverse of x
        getinverse <- function() m_inverse      ##Gets the inverse of x
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function works by acquiring the cache data, if any

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inverse <- x$getinverse()
        if(!is.null(m_inverse)) {       ##To check if inverse has already been calculated, if it has, then it simply returns the inverse of matrix x
                message("Getting Cached Data")
                return(m_inverse)
        }
        req <- x$get()
        m_inverse <- solve(req, ...)
        x$setinverse(m_inverse)         ##If inverse has not been calculated initially, then it is calculated using this function
        m_inverse
}
