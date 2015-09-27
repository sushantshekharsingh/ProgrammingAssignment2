# makeCacheMatrix creates is a function to
# set the value of the matrix
# get the value of the matrix
# set the value of inverse of the matrix
# get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix.
# We assume that the matrix is always invertible.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
	  ## Return a matrix that is the inverse of 'x'
}
