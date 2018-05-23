## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function build all the operator needed to retrieve or set the values of the
# matrix or its inverse
# usage:
# 1) Build a matrix, i.e. X<-x<-matrix(c(2,5,3,7,6,4,9,2,3), nrow=3, ncol=3)
# 2) Verify that - for example - the getter works, i.e. X$get() will print the matrix content
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the matrix inverse
  i <- NULL
  
  # Setter 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # Getter
  get <- function() x
  
  # Fill the inverse of a matrix
  setinverse <- function(inverse) i <<- inverse
  
  # Get the inverse of a matrix
  getinverse <- function() i
  
  # Build a list including with the above functions
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
# This function returns the inverse of the matrix (if it exists).
# The first time it is invoked it stores the resulting inverse in a cache
# to speed up calculations
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  # Retrieve the matrix
  data <- x$get()
  
  # If the determinant of the matrix is equal to zero
  # then the inverse does not exist
  if (det(data)==0)
  {
   stop("Cannot calculate the inverse of the matrix (det = 0)")
  }
  
  # Try to get the inverse of the matrix from the cache
  i <- x$getinverse()
  
  # If the cache has been filled earlier, then return its content
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # The cache is empty. Calculate the inverse of the matrics
  i <- solve(data, ...)
  
  # Store the result into the cache
  x$setinverse(i)
  i

}
