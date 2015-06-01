###############################################################################
# R programming : coursera.com - May 2015
# Johns Hopkins Bloomberg School of Public Health
# by Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD
# 
# Week 3 Programming Homework Assignment Number 2
# Author: tim
# Function: makeCacheMatrix and cacheSolve
###############################################################################

## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## Variable: x is a matrix with default value as an empty matrix. 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
	
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	get <- function() {
		x
	}
	
	setinv <- function(inverse) {
		m <<- inverse
	}
	
	getinv <- function() {
		m
	}
	
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function cacheSolve returns a matrix that is the inverse of 'x'
## Variable: x is an invertable matrix.
## This fuction checks to see if the inverse of the matrix has already been solved.
## If it has it returns te inverse.  If it has not it solves for the inverse and 
## stores the result in the cache variable.
cacheSolve <- function(x, ...) {
    m <- x$getinv()
	
	if(!is.null(m)) {
		message("getting cached matrix")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m
}
	
## Some test code
##m <- matrix(rnorm(16), 4, 4)
##print(m)
##x <- makeCacheMatrix(m)
##z<- cacheSolve(x)
##print(z)
## Call cacheSolve again to verify we are returning our cached value.
##z <- cacheSolve(x)
##print(z)
