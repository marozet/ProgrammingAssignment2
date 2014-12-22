## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix will calculate the inverse of a matrix x
#it addss setter and getter for the matrix as well as for inversed matrix data

makeCacheMatrix <- function(x = matrix()) {
	inversed <- NULL
	set <- function(y){
		x <<- y
		inversed <<- NULL
	}
	get <- function(){ x }
	setInversed <- function(solve) <<- solve
	getInversed <- function() inversed
	list(set = set, get = get, setInversed = setInversed, getInversed = getInversed)
}


## Write a short comment describing this function
#cacheSolve first checks whether an inversed matrix already exists
#if it does it returns the cached result
#if not it calculates the inversed matrix using solve(x)
#and caches the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inversed <- x$getInversed()
		if(!is.null(inversed)) {
			message("getting cached data")
			return(inversed)
		}
		data<-x$get
		inversed <- solve(data, ...)
		x$setInversed(inversed)
		inversed
}
