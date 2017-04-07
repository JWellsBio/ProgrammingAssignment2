# This is for the assignment for R Programming on Coursera, Week 3

# First: Need to create a function that creates a 'special' matrix object that can cache its inverse.
# This should be fairly easy given the example code in the question. We can modify it to work with a matrix as opposed to a vector

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x # up to here is exactly the same as the example as we have not needed anything different
  setinverse <- function(solve) m <<- solve # here we are setting the inverse of the given matrix (in the example it calculated the mean of the vector)
  getinverse <- function() m # here we are obtaining the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) # create the list for use later
}

# Second: we need to create a function that computes the inverse of the 'special' matrix created above. If already done, we retrieve the cached matrix
# This is almost exactly like the given example in the assignment

cacheSolve <- function(x, ...) {
  m <- x$getinverse() # get inverse from list from previous function
  if(!is.null(m)) {
    message("getting cached data")
    return(m) # recalls cached matrix if exists
  }
  data <- x$get() # gets matrix if not cached yet
  m <- solve(data, ...) # performs inverse of matrix
  x$setinverse(m) # stores cached inverse matrix
  m # displays inverse matrix
}

# This works for all invertible matrices that were checked. As per the assignment, we do not have to account for any exceptions to this rule as we are assuming all matrices entered are invertible.

