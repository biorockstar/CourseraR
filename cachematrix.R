## makeCacheMatrix will first clear all the variables from
## the cache.  Then it will evaluate the 'x' matrix
## and solve() for its inverse, storing it in cache.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(z) {
        x <<- z
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve will look to see if an inverted matrix
## has been cached. If it has not, then it will solve it
## and store it to cache to be recalled later.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if (!is.null(i)) {
        print("Retrieving inverted matrix from cache...")
        return(i)
    }
    i <- x$setinv(solve(x$get()))
    print("Inverting matrix and storing to cache...")
    i
}
