## The goal of the folowing functions is determine the inverse of a matrix in an efficent way.
## The goal of the functions belows is to determine and cache the inverse of a matrix

## The first function, makeVector creates a special "matrix" that can cashe its inverse.
## makeCacheMatrix is really a list containing a function to
## set the matrix 
## get the matrix
## set the inverse of the matrix (cashing the inverse of the matrix)
## get the inverse of the matrix 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
## to compute the inverse we are using the Resolve function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
