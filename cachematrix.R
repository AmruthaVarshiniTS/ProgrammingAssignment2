## functions that cache the inverse of a matrix

## creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
		}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
	}


## computes the inverse of "matrix" returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {
      invmatrix <- x$getinv()
	if(!is.null(invmatrix)) {
		message("getting cached matrix data")
		return(inv)
	} 
	data <- x$get()
	invmatrix <- solve(data, ...)
	x$setinv(invmatrix)
	invmatrix
}
