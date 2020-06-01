## This function calculate the inverse of one matrix and 
## cache the result. Hence, when the same matrix appears,
## the result can be looked up and no extra calculation 
## is needed.

## The first function creates a object that set the value
## of the matrix, get the value of the matrix, set the
## value of the inverse matrix and get the value of the
## inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
		n <- NULL
		set <- function(y) {
				x <<- y
				n <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) n <<- inverse
		getinverse <- function() n
		list(set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)
}


## The second function calculates the inverse matrix of the
## result returned by the first function. In addtion, it 
## tests that if the matrix has appearred, the function will
## look up the result from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        n <- x$getinverse()
        if(!is.null(n)){
        		message("getting cached data")
        		return(n)
        }
        mat <- x$get()
        n <- solve(mat, ...)
        x$setinverse(n)
        n
}
