##makeCacheMatrix function will make the matrix passed ,a cache matrix.
##which caches the inverse of the matrix. 

##we can pass matrix as argument. that will assigned to x. we can also paas annonymous matrix
##it will return 4 methods. : set, get, setInverse and getInverse.

##set will only set matrix.
##get will return matrix
##setInverse will set the inverse of the matrix. and caches it. <<- operator used.
##getInverse will return matrix inverse..without computing it...beause it is already solved. by cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
       inverseMatrix <- NULL
       
       set <- function(y) {
                             x <<- y
                             inverseMatrix <<- NULL
       }
  
       get <- function() x
       setInverse <- function(InverseM) inverseMatrix <<- InverseM
       getInverse <- function() inverseMatrix
       
       list(set = set, get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}

##cacheSolve function solves the inverse of the matrix if it is not already calculated.
##it takes CachedMatrix return by makeCachedMatrix function.
##first checks that the inverse is already calculated or not.
##if so , return that calculated inverse only
##otherwise it will calculate it for first time and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        inverseMatrix <- x$getInverse()
        
        if(!is.null(inverseMatrix)) {
          message("getting cached data")
          return(inverseMatrix)
        }
        
       data <- x$get()
       inverseMatrix <- solve(x$get())
       x$setInverse(inverseMatrix)
       inverseMatrix
}

##sample usage : (for running and testing both functions) , remove "##" to run below code
## myCachedMatrix <- makeCacheMatrix(matrix(c(4,7,2,6),nrow = 2,ncol = 2)
## myCachedMatrix$get()    ## to display matrix 
## cacheSolve(myCachedMatrix) ## to solve, for first time 
## myCachedMatrix$getInverse()  ## for getting cached inverse.
