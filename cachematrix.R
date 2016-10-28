## These functions calculate the inverse of a matrix and
## and cache the matrix inverse making it retrievable for 
## future use for future use

## Creates the get and set properties for the matrix inverse
## allowing for retreval or the initial calculation

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y){
			x <<- y
			inv <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) inv <<- inverse
		getinverse <- function() inv
		list(set = set, get = get
			, setinverse = setinverse
			, getinverse = getinverse
	)
}


## Calculates the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Inverse already calculated. 
					Getting matrix inverse.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
