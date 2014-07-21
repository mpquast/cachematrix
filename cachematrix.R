#The first function creates an object that is a list containing a matrix (given) and
#its inverse (initially set as NULL).
#The second function takes a list created as above and returns its inverse as follows:
#first it checks the given object for the element that should contain the inverse; 
#if it's not NULL, it returns the said element (that contains the inverse), ie., the cached inverse;
#if it is NULL, the function calculates the inverse of the matrix, assign it to the proprer element of the list and returns it. 


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}