## Special matrix function that can cache the inverse of matrix

makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse property
  i <- NULL
  
  ## Method to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() m
  
  ## Method to set the inverse of the matrix
  set_Inverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  get_Inverse <- function() i
  
  ## Return a list of the methods
  list(set = set, get = get, set_Inverse = set_Inverse, get_Inverse = get_Inverse)
}



##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$get_Inverse()
  
  ## Just return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$set_Inverse(m)
  
  ## Return the matrix
  m
}