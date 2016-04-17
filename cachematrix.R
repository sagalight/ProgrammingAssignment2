## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 inver_x <- NULL
  set <- function(y) {
    x <<- y
    inver_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver_x <<- inverse
  getinverse <- function() inver_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
inver_x <- x$getinverse()
  if(!is.null(inver_x)) {
    message("getting cached data")
    return(inver_x)
  }
  data <- x$get()
  inver_x <- solve(data, ...)
  x$setinverse(inver_x)
  inver_x
        ## Return a matrix that is the inverse of 'x'
}
