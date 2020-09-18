## converse an invertible matrix to special format of "matrix" and then
## find inverse of the matrix

## convert a matrix to a special "Matrix" with the format of list

makeCacheMatrix <- function(x = matrix()){
    Inv <- NULL
    set <- function(y) {
        x <<- y
        Inv <<- NULL
    }
    get <- function() x
    setinverse <- function(Inverse) Inv <<- Inverse
    getinverse <- function() Inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## accept special "Matrix" and return the inverse of original matrix

cacheSolve <- function(x, ...) {
    Inv <- x$getinverse()
    if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    MyMatrix <- x$get()
    Inv <- solve(MyMatrix,...)
    x$setinverse(Inv)
    Inv
}
