## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly.

## To test the functions:
## a<-makeCacheMatrix() - Create a special "Matrix" object
## a$set(matrix(6:9,2,2)) - Populate the Matrix
## cacheSolve(a) - Retrieve inverse value from cache if available

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL ## if matrix is changed, initialized the cached value
	}
	get <- function() x
	setMatrix <- function(solve) m <<- solve
	getMatrix <- function() m
	list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x = matrix(), ...) {

      ## Return inverse from cache if available
	m <- x$getMatrix()
	if(!is.null(m)) {
		message("Returned Cached Data")
		return(m)
	}

	## No cache available, compute inverse and store in matrix
	data <- x$get()
	message("No Cached Data, Compute inverse")
	m <- solve(data, ...)
	x$setMatrix(m)
	m
}