cacheSolve <- function(x, ...) {
  minv <- x$getinv()
  if(!is.null(minv)) {
    message("getting cached inverse matrix")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setinv(minv)
  minv
  print(minv)
}