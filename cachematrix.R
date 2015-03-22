## Computing the inverse of a matrix can be time-consuming. In order to avoid re-computations 
## of inverses, two functions are implemented: The first function (makeCacheMatrix) 
## creates a list with setters and getters. The second function (cacheSolve) computes the inverse.  

## makeCacheMatrix function
## Several steps are implemented:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse_ <- NULL
        # with "set" a new matrix can be assigned: syntax ...$set(...)
        set <- function(y) {
                x <<- y 
                inverse_ <<- NULL
        }
        get <- function() x # no curly brackets needed
        # <<- operator is needed since inverse_ is defined in a parent environment 
        setinverse <- function(inverse) inverse_ <<- inverse 
        getinverse <- function() inverse_
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve function
## cacheSolve computes the inverse of a matrix if it isn't already computed. 
## The function returns the inverse of a matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_ <- x$getinverse()
        # if the inverse of a matrix is already computed return the inverse stored
        if(!is.null(inverse_)) {
                message("getting cached data.")
                return(inverse_)
        }
        # if the inverse isn't computed yet compute it
        matrixToInvert <- x$get()
        inverse_ <- solve(matrixToInvert)
        # set the inverse to the list
        x$setinverse(inverse_)
        # return the inverse
        inverse_
}