## Optimization of a process that requires repretedly calculating a matrix
## The matrix values are calcuated and cached. 

## sets up a matrix for inversion

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x<<- y
    inv<<- NULL
    
  }
  get <- function(){x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function(){inv}
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


## calcuates inversion or retrives cached values

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
