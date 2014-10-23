## These functions will create a inverse of a matrix. It will calculate the inverese only if
## the inverse is not allready cached, otherwise the inversed will be retrieved from the cache 

## Nulls minv (if the cacheSolve has not yet been called
## Set the value to the matrix
## checks the inputted matrix with cacheSolve, inverse will be returned
makeCacheMatrix <- function(x = matrix()) {
				minv<-NULL
        		setmatrix <- function(y){
                x <<- y
                minv <<- NULL
        		}
        		getmatrix <- function() x
        		setinverse <- function(inverse) minv <<- inverse
        		getinverse <- function() minv
        		list(setmatrix = setmatrix, getmatrix = getmatrix,
            	setinverse = setinverse,
            	getinverse = getinverse)
}


## if cached return that, otherwise calculate the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getinverse()
        if(!is.null(minv)) {
                message("found cached matrix")
                return(minv)
        }
        mtrx <- x$get()
        minv <- Solve(mtrx)
        mtrx$setinverse(minv)
        minv
}
