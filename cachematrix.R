## Course 2 Week 3 - Programming Assignment
## Two functions to optimise the processing & inversion of a matrix
## These are makeCacheMatrix() & cacheSolve().
##
## There is also a third function - cacheSome() - to generate sample
## matrices & test the output from the other two functions.
##
## function 1 - makeCacheMatrix - create the matrix (set it) & provide 
## the means for other functions to access it (get it)
##

makeCacheMatrix <- function(x, y, nr = 3, nc = 3) {
    m <- NULL
    set <- function(y) {
        x <<- x + matrix(c(y), nrow = nr, ncol = nc) 
        m <<- NULL
        x      
    }

##
## Have to set initial values here, otherwise nothing to work with?
##

    set(y)
    
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    xl <<- list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##
## cacheSolve - The following function calculates the inverse of the matrix 
## created by the function makeCacheMatrix above. However, it does first check
## to see if that inverse has already been calculated. If so, it gets the mean 
## from the cache and skips the computation. Otherwise, it calculates the inverse 
## of the matrix and sets its value in the cache via the setinverse function.
##

cacheSolve <- function(x, y, ...) {
    tm <- xl$set(y)
    print(tm)

##
## Check that the matrix is invertible
##
    
    if(det(xl$set(y)) != 0) {
        m <<- xl$getinv()        
    } else {
        message("matrix can not be inverted")
        m <<- NULL
        return(m)
    }
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- xl$get()
    m <<- solve(data)
    xl$setinv(m)
    m
}

##
## Run some data through the two functions above
##

cacheSome <- function() {
    r1 <- makeCacheMatrix(0, 0)
    print ("test matrix 1")
    r2 <- cacheSolve(0, rnorm(9))
    print("test matrix 1 inverted")
    print(r2)
    print ("test matrix 2")
    r4 <- cacheSolve(0, rnorm(9))
    print("test matrix 2 inverted")
    print(r4)
    print ("test matrix 3")
    r6 <- cacheSolve(0, rnorm(9))
    print("test matrix 3 inverted")
    print(r6)
}
