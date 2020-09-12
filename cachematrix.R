## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# function which returns a list of methods/functions
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # set value of variable to matrix
    set <- function(y) {
        # <<- operator allows caching of value which allows retrieval 
        # from outside of the environment that it was defined
        x <<- y
        m <<- NULL
    }
    # retrieve value of matrix 
    # similar to get <- function () {x}
    get <- function() x
    # set inverse of matrix
    # similar to setinverse <- function(solve) {m <<- solve}
    setinverse <- function(solve) m <<- solve
    # retrieve value of inverse of matrix
    getinverse <- function() m
    # store all methods/functions in list and return
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# function to retrieve value of inverse from cache if available, 
# if not, calculate and return inverse of matrix
cacheSolve <- function(x, ...) {
    # try to retrieve inverse
    m <- x$getinverse()
    # retrieve from cache (lexical scoping) if available
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # if inverse not available in cache, retrieve matrix values
    data <- x$get()
    # calculate inverse of matrix
    m <- solve(data, ...)
    # set inverse values of matrix 
    x$setinverse(m)
    # return inverse value
    m
}
