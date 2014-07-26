## The 2 functions below create and use a cacheMatrix. A cacheMatrix is a list of functions for
## getting and setting the value of a matrix and its inverse. The list has access to the values 
## of the matrix and its inverse through closure. 


## This function takes a matrix and returns a list of functions that manage a cached matrix 
## and a cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL             #initializes cached inverse to null
  set <- function(y) {
    x <<- y               #sets cached matrix to new value
    inv <<- NULL          #sets inv to null since matrix is being set ot new value 
  }
  get <- function() x
  setinv <- function(newInv) inv <<- newInv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function take a cacheMatrix and attempts to get its inverse. If the inverse is cached it retuns that.
## if not it calculates the inverse and sets it on the cacheMatrix using setinv() function and then returns it

cacheSolve <- function(x, ...) {
  m <- x$getinv()                        #get inverse
  if(!is.null(m)) {                      # check to see if there is a valid value cached
    message("getting cached data")       # if so return cached value  
    return(m)
  }
  data <- x$get()                         #otherwise get matrix
  m <- solve(data, ...)                   #calculate inverse
  x$setinv(m)                             #set the cacheMatrix inverse to calculated invers
  m                                       # reutnr inverss
}
