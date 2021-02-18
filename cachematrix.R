# Extends matrix by allowing in memory storage of inverse
makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_x <<- inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Inverts matrix created by makeCacheMatrix function
cacheSolve <- function(x, ...) {
  inv_x <- x$getinverse()
  if(!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inv_x)
  }
  inv_x <- solve(x$get(), ...)
  x$setinverse(inv_x)
  return(inv_x)
}