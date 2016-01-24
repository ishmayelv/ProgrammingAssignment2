# Usage :

  # Create a matrix x
  sampleMat <- matrix(rnorm(16), nrow = 4) 
  # Creating the our special matrix
  cMat <- makeCacheMatrix(sampleMat)   
  # Return the matrix
  cMat$get()   
  # Return the inverse matrix
  cacheSolve(cMat)   
  # Calling the 2nd time, to return the cached inverse matrix
  cacheSolve(cMat)    
   
####################################################  
####################################################

# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(mat = matrix()) {
 
   # invMat will store the cached inverse matrix
  invMat <- NULL
  # Setter for the matrix
  set <- function(setMat) {
    mat <<- setMat
    invMat <<- NULL
  }
  # Getter for the matrix
  get <- function() mat
  
  # Setter for the inverse
  setinv <- function(inverse) invMat <<- inverse
  # Getter for the inverse
  getinv <- function() invMat
  # Return the matrix with our newly defined functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# cacheSolve: Compute the inverse of the matrix. 
# If the inverse is already calculated then it will returns the cached inverse.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMat <- x$getinv()
  
  # If the inverse is already calculated, return it
  if (!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  # The inverse is not yet calculated, so we calculate it
  data <- x$get()
  invMat <- solve(data, ...)
  
  # Cache the inverse
  x$setinv(invMat)
  
  # Return it
  invMat
}
