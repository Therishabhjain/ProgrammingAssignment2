## Special matrix function that can cache the inverse of matrix

makeCacheMatrix <- function(m = matrix()) {
  
  i <- NULL
  
  set <- function(matrix){
    m <<- matrix
    i <<- NULL
  }
  
  get <- function() m
  
  set_inverse <- function(inverse){
    i <<- inverse
  }
  
  get_inverse <- function() i
  
  list(set=set, get=get, set_inverse = set_inverse, get_inverse = get_inverse)

}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  m <- x$get_inverse()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data) %*% data
  
  x$set_inverse(m)
  
  m
}
