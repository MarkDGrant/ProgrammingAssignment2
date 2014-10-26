## Functions to 1) cache inverse of matix, and 2) then solve inverse if not cached.

## Function creating object that will cache inverse of a matrix by: 
## 1) setting the values in the matrix, 2) getting its value, 3) set the inverse, 4) get the inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function to return if inverse cached, else solve it using setinverse function
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
