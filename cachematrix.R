## The functions below provide the ability to cache the inverse of a matrix, in
## order to avoid the cost of recalculation when the inverse is needed 
## repeatedly in some procedure

## makeCacheMatrix() creates a special object that stores both a matrix and its 
## inverse, and returns a list of functions to set/get both the matrix and its
## inverse.
## 
makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the inverse matrix variable to NULL object
    m <- NULL
 
    ## set() function uses <<- operator to update the variables x and m in the
    ## scope of the parent function makeCacheMatrix()
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## get() function simply returns the matrix x
    get <- function() x
    
    ## setinverse() function uses <<- operator to update the variable m in the
    ## scope of the parent function makeCacheMatrix()
    setinverse <- function(inverse) m <<- inverse
    
    ## getinverse() function simply returns the inverse matrix m
    getinverse <- function() m
    
    ## makeCacheMatrix() returns to caller a list of functions defined above
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
