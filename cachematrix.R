## The functions below cache the inverse of a matrix.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
##It contain 4 functions setmatrix,getmatrix,setinverse,getinverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setmatrix <- function(y){
                x <<- y
                inv <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the special "matrix" created with the above function. 
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation.
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        data <- x$getmatrix()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}