

## This function creates a matrix which is able to cache an inverted itself

makeCacheMatrix <- function(x = matrix()) {
inver <- NULL
create <- function(z) {
x <<- z
inver <<- NULL
}
obtain <- function() {
x
}
create_i <- function(i) {
inver <<- i
}
obtain_i <- function() {
inver
}
list(create = create,
obtain = obtain,
create_i = create_i,
obtain_i = obtain_i)
}

## This function calculates the inverse or just shows it, taking it from the cache
## if the inverse was calculated before

cacheSolve <- function(x, ...) {
inver <- x$obtain_i()
if(!is.null(inver)) {
return(inver)
}
mx <- x$obtain()
inver <- solve(mx, ...)
x$create_i(inver)
return(inver)
}

