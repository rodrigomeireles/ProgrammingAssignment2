
## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

# makeCacheMatrix() creates a list object containing functions to retrieve and set both the input matrix and its inverse.
# Every matrix has inverse set to NULL as default.


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function
# cacheSolve retrieves the inverse for the matrix object created using makeCacheMatrix and if it is NULL then it calculates using solve() function

cacheSolve <- function(x, ...){
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(x$get(), ...)
    x$setinv(inv)
    inv
}
