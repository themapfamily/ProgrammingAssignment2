## Matrix inversion is usually a costly computation and there may
## be some benefit to caching the inverse of a matrix rather than
## compute it repeatedly (there are also alternatives to matrix 
## inversion that we will not discuss here).

## Below are two functions that are used to create a special object
## that stores a Matrix and cache's its Inverse.
## Caching the Inverse of a Matrix.

## Assume that the matrix supplied is always invertible

## The first function creates a special "matrix" object that can cache
## its inverse.

## makeCacheMatrix creates a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse
## 4. get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve: This second function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then cacheSolve should
## retrieve the inverse from the cache.

## Assume that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        ## Computing the inverse of a square matrix can be done with the
        ## solve function in R. For example, if X is a square invertible
        ## matrix, then solve(X) returns its inverse.
        i <- solve(data, ...)
        x$setinverse(i)
        i
}