## The following two functions are used to create a special object
## that stores a matrix and cache's its inverse

## makeCacheMatrix function creates a special "matrix". This function
## is really a list to: (set the value of the matrix, get the value
## of the matrix, set the value of the inverse, get the value of the
## inverse)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function calculate the inverse of the matrix created
## with makeCacheMatrix function. If the inverse has already been
##calculated the calculation is skipped and it is retrieved
## from the cache.Otherwise, it calculates the inverse of the matrix
## and set it in the cache via setinverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
