## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m_inverse <- NULL
        
        # set a new value for matrix x
        set <- function(y) {
                x <<- y
                m_inverse<<- NULL
        }
        
        # Get the Matrix x from the parent enviroment 
        get <- function() x
        
        # Defines the setter for the inverse m_inverse
        #
        setinverse <- function(inver) m_inverse <<- inver
        
        
        getinverse <- function() m_inverse
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## CacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inverse <- x$getinverse()
        
        # In the case the inverse was already calculated
        
        if(!is.null(m_inverse)) {
                message("getting cached data")
                return(m_inverse)
        }
        
        # If the inverse was not calculated, it gets the matrix X first
        data <- x$get()
        
        
        # It calculated the inverse of x
        
        m_inverse<- solve(data, ...)
        
        #Uses setinverse() function to set inverse
        x$setinverse(m_inverse)
        m_inverse
}
