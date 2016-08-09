##cache the inverse of a matrix


## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv_val <- NULL
        set <- function(y) {
                x <<- y
                inv_val <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv_val <<- inverse
        getinverse <- function() inv_val
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## computes the inverse of the special "matrix" 
##or retrieve the inverse from the cache if it has already been cal.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_val<-x$getinverse()
        if(!is.null(inv_val)) {
                message("getting cached data")
                return(inv_val)
                }
        data <- x$get()
        inv_val <- solve(data, ...)
        x$setinverse(inv_val)
        inv_val         
}
