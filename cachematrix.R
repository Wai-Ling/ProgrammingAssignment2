## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly.
## The pair of functions below make use of lexical scoping of R, help to cache the inverse of a matrix.

## makeCacheMatrix - This function creates a special "matrix" object that can cache its inverse matrix.
## It takes an invertible matrix as an input parameter and returns a list containing 4 functions:
##    (set, get, setinv, getinv).  The default for the input parameter is an empty matrix.
## Example of calling this function:
##   v_mtx <- matrix(1:4, nrow=2, ncol=2)
##   my_special <- makeCacheMatrix(v_mtx)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve - This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## This function takes 1 input parameter which is the output from the makeCacheMatrix.
##
## Example of calling this function:
##   my_inv <- cacheSolve(my_special)   #first time calling will place the inverse into cache
##   my_inv2 <- cacheSolve(my_special)  #second time calling should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
	if(!is.null(m)) {
                message("getting inverse matrix from the cache.")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
