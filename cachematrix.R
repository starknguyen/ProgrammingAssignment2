## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


# Create a matrix that cached its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverse_matrix) inv_matrix <<- inverse_matrix
  getInverseMatrix <- function() inv_matrix
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

# Computes the inverse of the matrix
# If the value was already calculated, it can be return from the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getInverseMatrix()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data,...)
  x$setInverseMatrix(inv_matrix)
  inv_matrix
}
