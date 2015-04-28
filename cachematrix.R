
## makeCacheMatrix is used to create a matrix (passed as parameter x) and its inverse in cache
## It returns an oject that can be used when calling the cachesolve function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function that computes the inverse of the special "matrix" passed on parameter (x) and created via the 
## makeCacheMatrix fubction above.
## If the inverse has already been calculated (i.e is in cache), then cacheSolve retrieves the inverse from the cache.
## The function returns the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
