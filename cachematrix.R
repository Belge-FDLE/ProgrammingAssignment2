## The two functions below provide a way to avoid recomputing the
## inverse of a matrix.
## They do so by creating an object able to store both the value of
## a matrix 'x' and its inverse (purpose of makeCacheMatrix) and by
## checking, prior to computing the inverse of 'x', that the value is
## not already cached within the object (purpose of cacheSolve).

## makeCacheMatrix : takes a matrix as input parameter (which defaults
## to the 1x1 NA matrix in case none is provided) and returns an object
## able to store the numeric matrix as well as its inverse.
## This cacheMatrix object is really just a list with getter and setter
## functions for the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
	# Initializing inverse matrix
	i <- NULL
	# Defining get-er and set-er functions for x
	getMat <- function() x
	setMat <- function(y) {
		x <<- y
		i <<- NULL
	}
	# Defining get-er and set-er functions for i
	getInv <- function() i
	setInv <- function(y) {
		i <<- y
	}
	# Returning the object
	list(setMat = setMat, getMat=getMat,
		setInv = setInv, getInv=getInv)
}

## cacheSolve : takes as input parameter an object (returned by the
## makeCacheMatrix function) representing a matrix and returns its
## inverse.
## It first checks if this inverse matrix is already cached in the
## cacheMatrix object. If it is not the case, it computes and stores it
## within the object. In both case, the inverse matrix is returned.

cacheSolve <- function(x, ...) {
	# Get the inverse cached in the object
	i <- x$getInv()
	# In case i is NULL, we need to compute and cache the inverse of x
	# before returning it. Otherwise, we can directly return i
	if(is.null(i)) {
		i <- solve(x$getMat())
		x$setInv(i)
	}
	# Returning the inverse
	i
}
