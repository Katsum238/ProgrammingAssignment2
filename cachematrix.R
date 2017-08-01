## makeCacheMatrix function gets,sets the matrix passed to it,it assumes it to be a invertible matrix always
makeCacheMatrix <- function(x = matrix()) {
  i = NULL
  set = function(y) {
    x <<- y
    i <<- NULL
  }
  get = function() x
  setinv = function(i2) i <<- i2
  getinv = function() i
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## check if inverse of the matrix passed already exists in cache,if not uses solve to inverse the matrix.

cacheSolve <- function(x, ...) {
  i = x$getinv()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  mat.data = x$get()
  i = solve(mat.data, ...)
  x$setinv(i)
  return(i)
}
