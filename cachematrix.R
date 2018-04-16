## The functions below provide a way to cache the calculation of 
## an inverted matrix. 
## makeCacheMatrix creates a list that stores data needed to provide 
## fast access to the inverted matrix
## after cached Matrix was  created, from now on, call cacheSolve 

## This function will create a vector to acces the cache data
## x is the matrix that will be inverted
## 4 functions are created and results are stored in the vector: 
## 1) get: retrieve the original matrix 2) set: store the value of the original 
## matrix 3) getinverse: retrieve the inverse matrix 4) setinverse: store the 
## inverese matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve will retrieve the inversematrix if calculated. If not, will 
## calculate it, cache it and return it
## x is the cached list

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached matrix")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
