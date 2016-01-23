## makeCacheMatrix creates the list of functions set, get, setinverse and getinverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {x <<- y;m <<- NULL}
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
## takes function list and new matrix as input and calculates inverse if the 
## previously stored matrix is not already stored
cacheSolve <- function(x,new_m) {
    cached_m<-x$get()
    cached_inverse_m <- x$getinverse()
    print("cached_m is");print(cached_m)
    print("cached_inverse_m is"); print(cached_inverse_m)
    if(is.null(cached_inverse_m)){
        print("in cacheSolve: no cached inverse matrix available, get new inverse")
        new_inverse_m <- solve(new_m)
        x$set(new_m)
        x$setinverse(new_inverse_m) }
    else if(is.na(cached_m[1,1])){ # there is no matrix stored to compare
        print("in cacheSolve: no cached matrix available, get new inverse")
        new_inverse_m <- solve(new_m)
        x$set(new_m)
        x$setinverse(new_inverse_m)}
    else if(identical(new_m,cached_m)) { #now we can compare
        print("in cacheSolve: new_m and cached_m are the identical, use cached inverse")
        new_inverse_m<-cached_inverse_m }
    else {
        print("in cacheSolve: new_m and cached_m are different, calculate new inverse")
        new_inverse_m <- solve(new_m)
        x$set(new_m)
        x$setinverse(new_inverse_m)}
    
    new_inverse_m
}
