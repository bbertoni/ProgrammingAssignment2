## These two functions are designed to calculate the inverse of a matrix and
## cache its value so that it only needs to be computed once.

## makeCacheMatrix creates a special matrix object that can cache its inverse, i.e.
## it returns a list of 4 objects: a function that sets the value of the matrix,
## a function that returns the value of the matrix, a function that sets the inverse
## of the matrix, and a function that returns the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # initialize inv as an undefined variable
    set <- function(y) {  
        x <<- y  # re-assign the matrix x the value y 
        inv <<- NULL  # re-assign the variable inv to be undefined i
    }
    get <- function() x  # returns the value of x
    setinverse <- function(inverse) inv <<- inverse  # re-assigns the variable inv the value
                                                     # inverse 
    getinverse <- function() inv  # returns the value of inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes in a list defined by the output of makeCacheMatrix.  It checks
## to see if the inverse of the matrix x has a value other than NULL.  If that is
## the case, it returns the cached inverse of x.  If the inverse of the matrix x
## is NULL, then it calculates the inverse, stores it in the cache for x, and 
## reutrns the inverse.

cacheSolve <- function(xfunc, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- xfunc$getinverse()  
    if(!is.null(inv)) {  # if the value of the inverse exists in the cache, return it
        message("getting cached data")
        return(inv)
    }
    data <- xfunc$get()  # this gets the value of the matrix 
    inv <- solve(data, ...)  # this solves for the inverse
    xfunc$setinverse(inv)  # this caches the value of the inverse of x in xfunc
    inv  # return the inverse of the matrix
}
