## The below two functions would enable 
## 1) Creation of a special 'matrix' object that can cache its inverse
## (Matrix supplied is asummed to be always invertible)
## 2) Compute inverse of the matrix object from 1) and save it to cache or retrieve 
## the inverse solely from cache if it's available
## 

## this function set the value of the matrix, get the value of the matrix,
## set the value for the inverse of the matrix and get the value of the inverse 
## matrix. 
makeCacheMatrix <- function(x = matrix()) {

        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        } 
        
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s        
        list(set = set, get = get, 
             setsolve = setsolve,
             getsolve = getsolve)

}

## this function get the inverse matrix of the 'special' matrix created by 
## function makeCacheMatrix. It will checks if the inverse matrix have been 
## generated.If yes, it gets the inverse from cache and skips the computation
## Otherwise, it computes the inverse using solve function and set the value
## in the cache via setsolve function.
cacheSolve <- function(x, ...) {

        s <- x$getsolve()
        if (!is.null(s)) {
                message("getting cached data ")
                return(s)
        }
        data <- x$get()
        s <- solve(data)
        x$setsolve(s)
        s

}
