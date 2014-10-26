## the makeCacheMatrix function creates a list with 4 functions: 
## set, get, setinverse, and getinverse

makeCacheMatrix <- function(mat = matrix()) {
    invmatrix <- NULL
    inv <- NULL
    set <- function(y) {
      mat <<- y
      inv <<- NULL
    }
    get <- function() mat 
    setinverse <- function(invmatrix) inv <<- invmatrix
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve inverts a matrix. First it looks for cached data and if "NULL" it'll calculate

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
