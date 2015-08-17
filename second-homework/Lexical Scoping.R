makeCacheMatrix<- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    inversematrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         inversematrix = inversematrix,
         getmatrix  = getmatrix )
}
cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    data <- x$get()
    if(qr(data)$rank==ncol(data)) {
        message("getting cached data")
        solvematrix<-solve(data)
        x$inversematrix(solvematrix)
        solvematrix
    }
    else print("There is no solvematrix")
}