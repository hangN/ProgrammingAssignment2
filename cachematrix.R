
makeCacheMatrix <- function(mat = matrix()) {
  inv_mat <- NULL
  set <- function(m) {
    mat <<- m
    inv_mat <<- NULL
  }
  get <- function() { mat }
  setinv <- function(inv) { inv_mat <<- inv }
  getinv <- function() { inv_mat }
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Compute the inverse of the special "matrix" returned by `makeCacheMatrix`

cacheSolve <- function(mat, ...) {
  inv_mat <- mat$getinv()
  if (!is.null(inv_mat)) {
    message("from cache")
    return(inv_mat)
  }
  m <- mat$get()
  inv <- solve(m)
  mat$setinv(inv)
  inv
}