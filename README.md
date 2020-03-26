    makeCacheMAatrix <- function(x=matrix) {
            inv <- NULL
            get<- function() x
            set <- function(y) {
                    x <<- y
                    inv <<- NULL
            }
            
            setinv <- function(inverse) inv <<- inverse
            getinv <- function() inv
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
    }

    CacheSolve <- function(x, ...) {
            inv <- x$getinv()
            if(!is.null(inv)) {
                    message("inverse is cached")
                    return(inv)
            }
            data <- x$get()
            inv <- solve(data, ...)
            
            x$setinv(inv)
            inv
    }

### lala 222 

    