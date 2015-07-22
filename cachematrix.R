## The following pair of functions generate and cache the inverse of a matrix
##  so that once the inverse of the matrix has been solved for once, the next
##  identical operations on this matrix would directly use the cached value

## makeCacheMatrix function generates a list of functions as the following:
##  1. setmatrix: cache a matrix
##  2. getmatrix: retrieve cached matrix
##  3. setinverse: cache the inverse of the cached matrix
##  4. getinverse: retrieve the cached inverse of the cached matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setmatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(inver) inv <<- inver
    getinverse <- function() inv
    list(setmatrix = setmatrix,
         getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve function is a wrapper function to solve for inverse of a matrix.
##  If the inverse has been solved once, it will simply retrieve the cache
##  inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # First, check cached inverse
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    # if cached inverse is NULL, calculate the inverse
    m <- x$getmatrix()
    inv <- solve(m, ...)
    # cache newly calculated inverse
    x$setinverse(inv)
    inv
}
