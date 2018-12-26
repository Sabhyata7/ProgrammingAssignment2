# setwd("C:/One-Drive/OneDrive - Tredence/TALL/JHU/cache dir")

# v <- matrix(1:25,nrow = 5)
# v <- matrix( c(5, 1, 0, 3,-1, 2, 4, 0,-1), nrow=3, byrow=TRUE)
# v1 <- makeMat(v)
# v2 <- cacheinv(v1)

install.packages('matlib')
require(matlib)

makeMat <- function(x = numeric()) {
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

cacheinv <- function(x, ...) {
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
