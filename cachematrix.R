
## This function caches the inverse of the given matrix

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Retrieves the inverse of the matrix

cacheSolve <- function(x, ...) {
  ## Check if the inverse is already cached
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  ## compute inverse and cache
  data <- x$get()
  m <- inverse(data, ...)
  x$setinverse(m)
  m
}
