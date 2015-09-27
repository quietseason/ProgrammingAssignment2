makeCacheMatrix <- function(x = matrix()){
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x){
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}


##Generate invertible matrix for test
##hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
