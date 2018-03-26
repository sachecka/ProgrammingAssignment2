## makeCacheMatrix function creates a special matrix object that
##cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverx <- NULL
  ## the inverse is set to a null value
  set <- function(y) {
    x <<- y
    inverx <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inverx <<-inverse
  getinverse <- function() inverx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function checks for computed inverse of matrix, if already
## computed it returns a cached value otherwise computes the inverse

cacheSolve <- function(x=matrix(), ...) {
  inverx <- x['getinverse()']
  ##gets the cached value of inverse if already computed
  if (!is.na(inverx)) {
    message("getting cached inverse matrix")
    return(inverx)
    ## cached value of the inverse returned
  } else {
    ##computes the inverse if cached value not present
    inverx <- solve(x['get()'])
    x['setinverse(inverx)']
    return(inverx)
  }
}
