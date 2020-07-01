# makeCacheMatrix creates a matrix object that can cache its inverse.
# It returns a list of functions
# set - set the value of the matrix
# get - get the value of the matrix
# setinv - set the inverse
# getinv - get the inverse

makeCacheMatrix <- function(x=matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(s) inv<<-s
    getinv <- function() inv
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}

# cacheSolve returns the inverse: either retrieves the inverse from cache (if available), using getinv
# returned by makeCacheMatrix, or calculates the inverse using solve, and stores it in cache
# using setinv returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()                      # retrieve inverse from cache
    if(!is.null(inv)) {                    # if available, write the message "getting cached data", and return this inverse
        message("getting cached data")
        return(inv)
    }
    data <- x$get()                        # if inverse is not yet available, get the matrix and store it in data
    inv <- solve(data, ...)                # use the solve function to calculate the inverse, and store it in inv
    x$setinv(inv)                          # put inverse in cache
    inv                                    # return the inverse
}
