# Aim of the assignment is to write a pair of functions that cache the inverse of a matrix.
##Matrix inversion is usually a costly computation and there may be some benefit
##to caching the inverse of a matrix rather than compute it repeatedly 


# Function makeCacheMatrix
##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# Function cacheSolve
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


# testing the function
## call the matrix M
M <- matrix(c(1,2,3,4),2,2)
## create M1 to cache inverse of matrix M
M1 <- makeCacheMatrix(M)
## compute the inverse, if already calculated, retrieve the inverse from the cache
cacheSolve(M1)
