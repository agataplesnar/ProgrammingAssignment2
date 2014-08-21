## cache the inverse of a matrix

## Takes a  matrix and creates a matrix object caching its inverse. 

makeCacheMatrix <- function(x = matrix()) {
      reverse <- NULL
      set <- function(y) {
            x <<- y
            rev <<- NULL
      }
      get <- function() x
      setReverse <- function(solve) reverse <<- solve
      getReverse <- function() reverse
      list(
            set = set,
            get = get,
            setReverse = setReverse,
            getReverse = getReverse
      )

}


## Checks if the inverse has benn previously calculated. If yes, returnes cached inverse. Atherwise, calculates the reverse of a matrix returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {
      reverse <- x$getReverse()
      if(!is.null(reverse)) {
            message("getting cached data")
            return(reverse)
      }
      matr <- x$get()
      reverse <- solve(matr)
      x$setReverse(reverse)
      reverse
        
}
