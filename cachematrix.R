## A pair of functions that cache the inverse of a matrix. The matrix
## is assumed to be always invertible.
##
## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.
## 
## cacheSolve: This function gets precalculated inverse of the matrix, if 
## value is not availale, it calculates the inverse and sets the value 
## in cache.


## makeCacheMatrix creates a special "matrix", containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) m <<- inverse
    getinv <- function() m
    list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)  ## Ref: http://www.statmethods.net/advstats/matrix.html
    x$setinv(m)
    m        ## Return a matrix that is the inverse of 'x'
}

## Sample Run
## Ref: https://class.coursera.org/rprog-012/forum/thread?thread_id=189
## > source("ProgrammingAssignment2/cachematrix.R")
## > mat <- matrix(c(-1,-2,1,1),2,2)
## > x <- makeCacheMatrix(mat)
## > x$get()
## [,1] [,2]
## [1,]   -1    1
## [2,]   -2    1
## > inv <- cacheSolve(x)
## > inv
## [,1] [,2]
## [1,]    1   -1
## [2,]    2   -1
## > inv <- cacheSolve(x)
## getting cached data
## > inv
## [,1] [,2]
## [1,]    1   -1
## [2,]    2   -1
