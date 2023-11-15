# In the following I will create two functions that are used to cache the
# inverse of a matrix. For this, we assume that the given matrix is always
# invertable.

# makeCacheMatrix creates a special 'matrix' object 
# that can cache its inverse for efficient computation.
# It creates a list containing functions to 
#       1. set the matrix
#       2. get the matrix
#       3. set the inverse
#       4. get the inverse

makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) inv <<- inverse
    
    getInverse <- function() inv
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# cacheSolve computes the inverse of the special 'matrix' created by
# makeCacheMatrix.
# If the matrix has not changed and the inverse is already calculated, 
# Cachesolve will return the inverse from the cache.
# Otherwise it sets the inverse of the special 'matrix' via the setInverse
# function.

cacheSolve <- function(x, ...){
    inv <- x$getInverse()
    
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)  
    x$setInverse(inv)  
    inv
}
