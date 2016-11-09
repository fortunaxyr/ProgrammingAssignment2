## 1.set the matrix
## 2.get the matrix
## 3.set the inverse
## 4.get the inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function (y) {
		x <- y
		inverse <- NULL
	}
	get <- function () return (x)
	setinv <- function (inv) inverse <- inv
	getinv <- function () return (inverse)
	return (list (set = set, get = get, setinv = setinv, getinv = getinv))
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
       ## if the inverse has already been calculated
        if (!is.null (inv)) {
        	##skip the computation
        	return(inv)
        }
        
        ##otherwise, calculates the inverse
        mat.data <- x$get()
        inv <- solve (mat.data, ...)
        
        x$setinv (inv)
        return (inv)
}
