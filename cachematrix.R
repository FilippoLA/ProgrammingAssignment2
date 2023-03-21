## The first function creates a special "matrix" that caches its inverse
##    so that it won't need to be recalculated if the same.
## The second checks if the the inverse has already been cached and retrieves
##    it if so.  If not it computes and returns the inverse

## This function creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This is a Function to return the cached calculation if already done
## or compute the inverse of a matrix if needed and return it.
## Returns a matrix that is the inverse of 'x'
    
 # Check if the inverse has already been computed and cached
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
      message("getting cached inverse")
      return(inv)
    }
        # If not, it computes the inverse and delivers the result
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
m <- matrix(c(1, 2, 3, 4), 2, 2)
cacheSolve(makeCacheMatrix(m))
m1<-makeCacheMatrix(m)
cacheSolve(m1)
