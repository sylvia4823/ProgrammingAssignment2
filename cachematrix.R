## There are two functions within the R script
## makeCacheMatrix() stored the inverse matrix of an input matrix 'x'; output is an object
## cacheSolve() evaluated if makeCacheMatrix() already have inverse matrix or not; 
## if yes, extract the stored 'inv'; if not, calculate inverse matrix

## Function makeCacheMatrix; input is matrix x; out put is an object
## "get" store matrix x; "setinverse" calculate inverse by solve(x) and stored in 'inv'

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL   
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve(x)
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Access stored matrix from makeCacheMatrix()
## If 'inv' stored, return 'inv'
## Else, use matrix 'x', calculate inverse, then output 'inv'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
