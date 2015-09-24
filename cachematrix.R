## creates set of functions to 
##	1. create a random Square Matrix 
##	2. make a function-list to cache Matrix Inverse operation
##	3. compute cached Matrix Inverse
## For Testing:	(X <- sampleMatrix(3,rnorm)); v <- makeCacheMatrix(X); (Y <- cacheSolve(v)); (XmmultY = X%*%Y)

## sampleMatrix: create sz*sz sized square matrix object, with random values generated using rfunc (rnorm/runif/etc), with additional params ...
sampleMatrix <- function (sz, rfunc, ...) {
	data <- rfunc(sz*sz, ...)
	m <- matrix(data, nrow=sz, ncol=sz)
	m
}

## makeCacheMatrix: creates a special "matrix" object to cache its inverse.
makeCacheMatrix <- function(X) {
        Y <- NULL
        set <- function(x) {
                X <<- x
                Y <<- NULL
        }
        get <- function() X
        setinv <- function(inv) Y <<- inv
        getinv <- function() Y
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

