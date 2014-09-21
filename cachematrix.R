################################################################################
##
## Description : 
## 
## Computes the inverse of a square matrix and cached it. If the square matrix
## is to be computed again, instead of computing again the cached inverse 
## matrix is returned. 	 
##
################################################################################

################################################################################
##
## Function : makeCacheMatrix()
##
## Description : 
## This function creates a square matrix and cache its inverse. It contains a list
## of functions
##    1) setMatrix() : set a square matrix
##	2) getMatrix() : get a square matrix
## 	3) setInverseMatrix() : set the cached inverse square matrix
##	4) getInverseMatrix() : get the cached inverse square matrix
##
## Arguments:
## m		a square matrix.
##
## Return:
## A list of functions
##
################################################################################

makeCacheMatrix <- function(m = matrix()) {
	## initialise the static cache inverse matrix to NULL
	cacheInversedMatrix <- NULL
	
	## set static matrix
	SetMatrix <- function(m) {
		iMatrix <<- m

		## since static iMatrix is assigned with a new input matrix, 
		## initialise static cache inverse matrix to NULL
		cacheInversedMatrix <<- NULL
	}

	## get static matrix
	GetMatrix <- function(){
		iMatrix
	}
 
	## set static cache inverse matrix
	SetInverseMatrix <- function(inverseMatrix){
		cacheInversedMatrix <<- inverseMatrix
	}

	## get static cache inverse matrix
	GetInverseMatrix <- function(){
		cacheInversedMatrix
	}

	## return a list. Set each function as an element in a list
	list(setMatrix = SetMatrix, 
		getMatrix = GetMatrix,
		setInverseMatrix = SetInverseMatrix, 
		getInverseMatrix = GetInverseMatrix)
}


################################################################################
##
## Function : cacheSolve()
##
## Description : 
## This function computes the inverse of a square matrix. If the square matrix 
## did not change, the previous computed inverse matrix is returned.
##
## Arguments:
## m		list of functions returned from makeCacheMatrix().
## ...	further arguments passed to solve().
##
## Return:
## The inverse of a square matrix
##
################################################################################

cacheSolve <- function(m, ...) {
	## get static cache inverse matrix
	inversedMatrix <- m$getInverseMatrix()

	## if static cache inverse matrix exists
	if (!is.null (inversedMatrix)) {
		message("getting the cached inversed matrix")
		return(inversedMatrix)
	}

	## otherwise get matrix
	iMatrix <- m$getMatrix()
	
	## compute a new inverse matrix
	inversedMatrix <- solve(iMatrix, ...)

	## cache new inverse matrix
	m$setInverseMatrix(inversedMatrix)

	## return inverse matrix
	inversedMatrix
}
