makeCacheMatrix <- function(x = numeric()) {
  # Check whether the input x supplied by the user
  # is of class of matrix or not
  if(class(x) != "matrix") {
    print("ERROR: matrix must be supplied as an input");
    return(NULL);
  }
  
  # Check whether the input x supplied by the user
  # is a square matrix or not
  # (Square matrix is a necessary condition that it is invertible)
  if(dim(x)[1] != dim(x)[2]) {
    print("ERROR: square matrix must supplied as an input");
    return(NULL);
  }
  
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) minv <<- inverse
  getinv <- function() minv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}