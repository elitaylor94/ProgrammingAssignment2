## a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  
  # create the matrix in the working environment
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  # invert the matrix and store in cache
  setMatrix <- function(inverse) 
    cache <<- inverse
  # get the inverted matrix from cache
  getInverse <- function() cache
  
  # return the created functions to the working environment as a list
  list(set = set, #gives the name 'set' to the set() function above
       get = get, #gives the name 'get' 'to the get() function above
       setMatrix = setMatrix, #gives the name 'setMatrix' to the setMatrix() function defined above
       getInverse = getInverse) #gives the name 'getInverse' to the getInverse() function defined above
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## attempt to get the inverse of the matrix stored in cache
  cache <- x$getInverse()
  
  # return inverted matrix from cache if it exists
  # else create the matrix in working environment
  if (!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }

  matrix <- x$get()
  # set and return inverse of matrix
    cache <- solve(matrix, ...)
    x$setMatrix(cache)
  
  # display matrix in console
  return (cache)
}
