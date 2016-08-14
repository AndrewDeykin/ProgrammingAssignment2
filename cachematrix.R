# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. This
# assignment writes a pair of functions that cache the inverse of a matrix.


# makeCacheMatrix: This function creates a special "matrix" object that can cache
# its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invmat <<- inverse
  getinverse <- function() invmat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has
# not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  invmat <- x$getinverse()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinverse(invmat)
  invmat
}

# Testing it out with sample matrix

#> a <- makeCacheMatrix(matrix(1:4, 2, 2))
#> a$get()
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4

#Not in the cache yet 
#> cacheSolve(a)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

#Now that it is in the cache
#> cacheSolve(a)
#getting cached data
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5


