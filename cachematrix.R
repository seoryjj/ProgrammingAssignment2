## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    if(x != y) {
      x <<- y
      inv <<- NULL
      message("cache clear !!!")
    }
  }
  get <- function() x
  setinv <- function(solve_value) {
    inv <<- solve_value
    
  }
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, s = makeCacheMatrix(x)) {
  ## Return a matrix that is the inverse of 'x'
  #s <- makeCacheMatrix(x)
  inv <- s$getinv()
  
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  data <- s$get()
  inv <- solve(data)
  s$setinv(inv)
  
  inv
}
