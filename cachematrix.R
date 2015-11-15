## Matrix inversion is usually a costly computation. These functions will
## cache the inverse of a matrix rather than compute it repeatedly


## makeCacheMatrix:creates a special MATRIX object that able to cache its inverse
## MatInv = Matrix Inverse, NewMat = New Matrix that has not been cache

makeCacheMatrix <- function(x = matrix()) {
		MatInv <- NULL
		set <- function(y) {
				x <<- y
				MatInv <<- NULL
		}
		get <- function() { 
				x 
		}
		setInverse <- function(inverse) { 
				MatInv <<- inverse 
		}
		getInverse <- function() {
				MatInv
		}
		list(set = set,
			 get = get,
			 setInverse = setInverse,
			 getInverse = getInverse)
}


## cacheSolve:computes the inverse of the special MATRIX returned by makeCacheMatrix 
##			  If the inverse has already been calculated, the cachesolve will retrieve 
##			  the inverse from the cache   
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		MatInv <- x$getInverse()
		if (!is.null(MatInv)) {
				message("Found in cached data!")
				return(MatInv)
				##it will return the cache data and will not continue below coding
		}
		NewMat <- x$get()
		MatInv <- solve(NewMat, ...)
		x$setInverse(MatInv)
		return(MatInv)
}
