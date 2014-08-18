## The problem of calculate a matrix's inversion it is a hard problem to solve.
## This operation is expensive and if it has possible to cache the result for 
## avoid calculate again the same matrix it will be awesome.

## The functions 'makeCacheMatrix' and 'cacheSolve' keep a matrix into an object and 
## it has allowed to access this with 'get' function or set another matrix by a 'set'
## function. The second function will calculate a inverse of a matrix given. However
## if it has already been calculated it is just return a inverse of matrix and print a 
## message of 'getting cached data'.


## This function makeCacheMatrix contains this following functions:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix 
makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns a inverse of a matrix. If the matrix is already been calculated 
## so it returns a inverse with a message 'getting cached data'. If it has not been
## calculated so the function calculate a inverse of a matrix given and set it on
## cache.
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
