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


## cacheSolve() returns the inverse of a matrix stored in an object created 
## using makeCacheMatrix(). The first time cacheSolve() is called for a 
## particular matrix, it calculates the inverse and stores the inverse in the 
## special object. Subsequent calls of cacheSolve() for the same matrix just
## returns the stored inverse matrix.
## 
cacheSolve <- function(x, ...) {
    ## Get the inverse matrix currently stored in the object
    m <- x$getinverse()
    
    ## If we got something besides the NULL object, just return that - it is 
    ## the inverse matrix previously cached
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## No inverse was cached, so get the matrix...
    data <- x$get()
    ## ... and calculate the inverse...
    m <- solve(data, ...)
    ## ... and store the inverse back in the object...
    x$setinverse(m)
    ## and return the inverse.
    m
}
