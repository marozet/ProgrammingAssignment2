## Cashed inversion of a matrix v1.0
## by: marozet
## 2015-02-19

## Example
# Initialize cachable matrix X from matrix M
# X will be set to a list of getters and setters
# X <- makeCacheMatrix(M)
# you can get the matrix by calling X$get()
# to compute the inverse of a matrix for the first time use
# cacheSolve(X)
# next time you run cacheSolve(X) it will returned cached result
 
#### makeCacheMatrix(M)
# X<-makeCacheMatrix(M) creates a cachable matrix X from matrix M
# setter and getter are added for the matrix x
# setter and getter for inversed matrix are also added
# list of setters and getters is returned

makeCacheMatrix <- function(x = matrix()) {
	inversed <- NULL
	set <- function(y){
		x <<- y
		inversed <<- NULL
	}
	get <- function(){ x }
	setInversed <- function(solve) inversed <<- solve
	getInversed <- function() inversed
	list(set = set, get = get, setInversed = setInversed, getInversed = getInversed)
}


#### cacheSolve(X)
# cacheSolve(X) calculates the inverse of matrix X 
# first it checks whether an inversed matrix already exists
# if it does it returns the cached result
# if not it calculates the inversed matrix using solve(data,...)
# and caches the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inversed <- x$getInversed()
		if(!is.null(inversed)) {
			message("getting cached data")
			return(inversed)
		}
		data<-x$get()
		inversed <- solve(data, ...)
		x$setInversed(inversed)
		inversed
}
