## The function makeCacheMatrix consists of set, get, setinv, and getinv
## library(MASS) will be used to calculate the inverse of matrices

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ##initialize inverse as null
  set <- function(y) {
    x <<- y
    inv <<- NULL 
    }
  get <- function()x
  setinv <- function(inverse)inv<<- inverse
  getinv <-function(){
    inver <- ginv(x)
    inver%*%x  ## obtain inverse of our matrix
    }
  list(set=set, get = get,
       setinv = setinv, getinv = getinv)
}

## This will be used to get cache data

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){  ##check if inverse us null
    message("getting cached data")
    return(inv)
    }
  data <- x$get()
  inv <- solve(data,...)  ##calculate inverse value
  x$setinv(inv)
  inv ##return an inverse matrix
}
