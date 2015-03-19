## Cache the inverse of a matrix.

##Creates a special 'matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y) {
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setinverse<-function(inverse) m<<-inverse
    getinverse<-function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Computes the inverse of the special "matrix" created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
    m<-x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    matrix<- x$get()
    m<-solve(matrix, ...)
    x$setinverse(m)
    m
    ## Return a matrix that is the inverse of 'x'
}
