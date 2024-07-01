## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize a variable to store the inverse matrix
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y  # Assign the matrix 'y' to 'x'; use '<<-' to assign in parent environment
    inv <<- NULL  # Invalidate the cached inverse
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to get the cached inverse
  cache_inverse <- function() inv
  
  # Function to compute and cache the inverse
  set_inverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Return a list of functions
  list(set = set, get = get, cache_inverse = cache_inverse, set_inverse = set_inverse)
}

cacheSolve <- function(mat) {
  inv <- mat$cache_inverse()  # Retrieve the cached inverse
  
  # If the inverse is already computed, return it
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  data <- mat$get()
  inv <- solve(data)
  
  # Cache the computed inverse
  mat$set_inverse(inv)
  
  # Return the inverse matrix
  inv
}
## Let's call the functions
# Create a cacheable matrix
mat <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2))

# Compute the inverse using cacheSolve
cacheSolve(mat)

# Example of updating the matrix
mat$set(matrix(c(2, 4, 1, 3), nrow = 2))

# Compute the inverse again using cacheSolve (should use cached result if matrix is the same)
cacheSolve(mat)