# Here are two functions - 
#     makeCacheMatrix stores the cached inverse of a given matrix
#     It has 4 components: set, get, setinv and getinv 
#     
#     cacheSolve is where the inverse calculation takes place
    

## This function stores the cached inverse of a matrix and stores in the parent environment

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) m <<- inverse
    getinv <- function() m
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function checkes if the inverse is already cached.
# If not, it checkes for the sinularity of the matrix and based on that, calculates the inverse

cacheSolve <- function(x, ...) {
    install.packages('matlib')
    require(matlib)
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    if(det(data) != 0) {m <- inv(data, ...)} else {message("It is a singular matrix")}
    x$setinv(m)
    # print(m)
    m
}
