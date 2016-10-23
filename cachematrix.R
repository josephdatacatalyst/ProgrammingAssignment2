## This is for an assignment for Week 3 of R Programming
## assignment by Joseph Lalonde (me)
## These are two functions, 

## This function 'makeCacheMatrix' creates a special matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL #create empty matrix. Represents inversion. ("m" is for "matrix" and "inv" is for inversion, natch.)
    set <- function(y){
         x <<- y
         m_inv <<- NULL
    }
    get <- function() x
    set_m_inv <- function(solve) m_inv <<- solve
    get_m_inv <- function() m_inv
    list(set = set, get = get, set_m_inv = set_m_inv, get_m_inv = get_m_inv)
    
}


## This function cacheSolve computes the inverse of the matrix returned by makeCacheMatrix (above). It then caches the calculation and only recalculates if it has been changed.
## example:
## x1 <- matrix(rexp(200, rate=.1),nrow = 10, ncol=10) # a matrix that is invertible always has equal number of rows, columns
## t <- makeCacheMatrix(x1)
## print(t)
## cacheSolve(t)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$get_m_inv()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <-solve(data, ...)
    x$set_m_inv(m)
    m
}
