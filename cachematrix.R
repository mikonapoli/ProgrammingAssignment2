## The following two functions allow to cache the inverse of a matrix, so that
## it is computed only once.
## 
## In Objected Oriented terms, we create an object with getters and setters for
## the values of the original matrix and its inverse (makeCacheMatrix being the
## constructor) and a method to compute the inverse only if it has not been yet
## computed (namely the cacheSolve function).

## The first function "creates" a special matrix. In fact it returns a list of 
## four functions that allow to store and retrieve the values of the matrix and 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The second function takes as input a "matrix" created with the makeCacheMatrix
## functions and returns its inverse. If the inverse has already been computed and
## stored, cacheSolve returns it without computing it again, otherwise the values
## of the inverse are computed and stored with the setinverse function of the
## "special" cached matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse        
}
