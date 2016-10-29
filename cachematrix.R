## These functions calculate the inverse of a matrix and
## and cache the matrix inverse making it retrievable for 
## future use for future use

## Creates the get and set properties for the matrix inverse
## allowing for retreval or the initial calculation

makeCacheMatrix <- function(x = matrix()) {
		minv <- NULL
		set <- function(y){
			x <<- y
			minv <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) minv <<- inverse
		getinverse <- function() minv
		list(set = set, get = get
			, setinverse = setinverse
			, getinverse = getinverse
	)
}


## Calculates the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		minv <- x$getinverse()
        if(!is.null(minv)) {
                message("Inverse already calculated. 
					Getting matrix inverse.")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setinverse(minv)
        minv
}
