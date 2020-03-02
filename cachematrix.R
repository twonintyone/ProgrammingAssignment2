## The first function creates a R object that stores a matrix and its inverse.
## makeCacheMatrix builds a set of functions - set, get, set_inverse and get_inverse
## It also has two data objects x and m
## Below the function, there is an example to show how it works
## If we pass a matrix x (2,3,4,5) in this function and call the object "aMatrix"
## We can get the same matrix by passing aMatrix$get()

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL    # We set this null for cacheSolve() below
  set <- function(y) {
    x <<- y    # Assign the input argument to x in the parent environment
    m <<- NULL # For clearing cache - memory from prior execution of cacheSolve
  }
  get <- function() x # Obtaining the matrix x
  set_inverse <- function(inverse) m <<- inverse # Setter for the inverse m
  get_inverse <- function() m ## Obtaining inverse m from set_inverse()
  list(set = set, get = get, ## List of functions under the created object 
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## Demostration on makeCacheMatrix
# Passing a matrix x c(2,3,4,5) and name the created object as "aMatrix" 
aMatrix <- makeCacheMatrix(matrix(c(2,3,4,5), 2, 2))

# Obtain matrix x with the get function
aMatrix$get()

# Since we did not pass an inverse matrix with set_inverse()
# Passing get_inverse should give us NULL
aMatrix$get_inverse() 



# The second function completes the first function above
# cacheSolve() computes the inverse of the special matrix above
# The function starts with argument x and an ellipsis for additional arguments
# When we pass an matrix object to cacheSolve, it will do two things
# First, it will search if an inverse is cached in the memory by calling get_inverse()
# If it is, a message will return along with the inverse
# Second, given that there is no cached memory of an inverse, 
# cacheSolve() will calculate the inverse and return the inverse matrix

cacheSolve <- function(x, ...){
  m <- x$get_inverse() # Calls get_inverse from the input object and assign to m
  if(!is.null(m)){    # Check if m is NULL
    message("getting cached data") # If m is not NULL
    return(m) # a cached inverse is returned to the parent environment
  }
  data <- x$get() # If m is NULL, obtain the matrix with get() 
  m <- solve(data,...) # Calculates the inverse with the matrix obtained
  x$set_inverse(m)  # Set the inverse in the input object
  m ## Return a matrix that is the inverse of 'x'
}

## Demostration on cacheSolve
# Pass the matrix object above, where no inverse was set in the first function
cacheSolve(aMatrix) # No message should appear

## If we pass the matrix object "aMatrix" in cacheSolve() again
## Since the inverse is cached after executing the above line
## we will get the message "getting cached data" along with the inverse
cacheSolve(aMatrix)
