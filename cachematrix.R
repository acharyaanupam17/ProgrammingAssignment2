## There are two functions. The first function takes a matrix as input and
##stores the inverse in cache. The second function checks if the inverse has
##been calculated. If it has been calculated it uses the value, else it 
##calculates the new inverse.

##makeCacheMatrix() takes a matrix as input and saves the inverse of the matrix
##in cache.
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
        set <- function(y){
            x <<- y
            m <<- NULL
        }
    
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## catcheSolve() gets the inverse from cache and checks if the inverse has been
## calculated. If it has been calculated, it takes the inverse else it calculates
## the inverse.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}