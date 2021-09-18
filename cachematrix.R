## makeCacheMatrix: returns a list with 4 functions: set, get, setinverse, getinverse
## mtrx is the input variable that is also returned when calling get
## without it, we could have an error thrown if we called 
## y <- MakeCacheMatrix()
## y$get()


makeCacheMatrix <- function(mtrx = matrix()) {
  inv_cache <- NULL
  set <- function(y){
    mtrx <<- y
    inv_cache <<-NULL
  }
  get <- function() {mtrx}
  setinverse <- function(inverse) {inv_cache <<- inverse}
  getinverse <- function() {inv_cache}
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## inv_cache is the variable within calling function environment that store value of inversed matrix
## if we change the base matrix, then also set the inv_cache to NULL
## so we can say that if inv_cache is null then the matrix has changed or the invese was not calculated
## therefore needs to be re-calculated
## no need to introduce another variable that stores the information wether or not the matrix has changed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return((inv))
  }
  inv <- solve(x$get(),...)
  x$setinverse(inv)
  inv
}