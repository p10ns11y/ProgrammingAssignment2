## Caching the inverse of a Matrix
## Author : Peramanathan Sathyamoorthy
## template provided, filling objective

## creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # matrix update
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  # inverse values update
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  # Special means it stores its inverse / cached
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' - solve(x)
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
