
makeCacheMatrix <- function(z = matrix()) {
  inv_z <- NULL
  set <- function(y) {
    z <<- y
    inv_z <<- NULL
  }
  get <- function() z
  setinverse<- function(inverse) inv_z <<-inverse
  getinverse <- function() inv_z
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(z, ...) {
  ## Return a square  matrix that is the inverse of 'z'
  inv_z <- z$getinverse()
  if (!is.null(inv_z)) {
    message("This is the  cached inverse matrix")
    return(inv_z)
  } else {
    inv_x <- solve(z$get())
    z$setinverse(inv_z)
    return(inv_z)
  }
}