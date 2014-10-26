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
	##setinverse will take inverse and superassign it to minv
        	setinverse <- function(solve) minv <<- solve  ##computes, caches matrix inverse
        	getinverse <- function() minv			  ##returns matrix inverse
        	##a list of the functions
        	list(setmatrix = setmatrix, getmatrix = getmatrix,
            	setinverse = setinverse, getinverse = getinverse)
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
        minv <- solve(mtrx)
        x$setinverse(minv)  ##sets the computed value in the cache and save the value to calling env.
        minv
}
