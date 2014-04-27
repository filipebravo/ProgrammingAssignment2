## Creates a cached matrix object
## Receives a matrix
## Returns the resulting cached matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Solves the inverse of a cached matrix
## Receives a cached matrix
## Returns the inversed matrix
## If using the cached matrix, prints 'getting cached data'
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Use the following stub for testing the functions
## a <- makeCacheMatrix(matrix(1:4,2))
## a$get()
## a$getinverse()
## a$set(matrix(5:8,2))
## a$get()
## cacheSolve(a)
## cacheSolve(a)
## a$getinverse()
## b = a$getinverse()
## a$get() %*% b 
