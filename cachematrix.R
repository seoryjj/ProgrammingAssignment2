# makeCacheMatrix function : 
# creates a special "square matrix", which is really a list containing a function to
# set : set the matrix 
# get : get the matrix
# setinv : set the inverse matrix 
# getinv : get the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
  # inv : inverse matrix of x
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


# cacheSolve function :
# calculates the inverse matrix of the special "square matrix" created with the above function. 
# process : 1. checks to see if the inverse matrix(inv) has already been calculated. 
#           2. If so, it gets the inverse matrix(inv) from the cache and skips the computation. 
#           3. Otherwise, it calculates the inverse matrix of the data 
#              and sets the inverse matrix in the cache via the setinv function.

cacheSolve <- function(x, s) {
  # s is the list of 4(set, get, setinv, getinv) 

  # process 1  
  inv <- s$getinv()

  # process 2
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  
  # process 3
  data <- s$get()
  inv <- solve(data)
  s$setinv(inv)
  
  return(inv)
}

######### R exceute sample code #########
#R > x <- matrix(1:4, nrow=2)
# x
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
#R > s <- makeCacheMatrix(x)
#R > cacheSolve(x, s)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#R > cacheSolve(x, s)
# getting cached matrix
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#R > x %*% cacheSolve(x, s)
# getting cached matrix
#     [,1] [,2]
# [1,]    1    0
# [2,]    0    1
