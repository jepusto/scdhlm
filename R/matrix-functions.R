
##------------------------------------------------------------------------
## block-diagonal matrix addition, multiplication, and trace functions 
##------------------------------------------------------------------------

# turn block-diagonal into regular matrix

unblock <- function(A, block=NULL) {
  if (is.null(block)) block <- rep(names(A), times = sapply(A, function(x) dim(x)[1]))
  n <- length(block)
  mat <- matrix(0, n, n)
  for (i in unique(block)) {
    index <- i == block
    mat[index,index] <- A[[i]]
  }
  return(mat)
}


# sum of two conformable block-diagonal matrices

sum_blockblock <- function(A, B)
  mapply(function(a,b) a + b, a = A, b = B, SIMPLIFY = FALSE)


# generic matrix minus block-diagonal

matrix_minus_block <- function(A, B, block=NULL) {
  if (is.null(block)) block <- rep(names(B), times = sapply(B, function(x) dim(x)[1]))
  
  mat <- A
  for (i in unique(block)) {
    index <- i == block
    mat[index,index] <- mat[index, index] - B[[i]]
  }
  return(mat)
}


# block-diagonal minus generic matrix

block_minus_matrix <- function(A, B, block=NULL) {
  if (is.null(block))
    block <- rep(names(A), times = sapply(A, function(x) dim(x)[1]))
  
  mat <- -B
  for (i in unique(block)) {
    index <- i == block
    mat[index,index] <- mat[index, index] + A[[i]]
  }
  return(mat)
}


# product of two conformable block-diagonal matrices

prod_blockblock <- function(A, B)
  mapply(function(a, b) a %*% b, a = A, b = B, SIMPLIFY = FALSE)



# product of a block-diagonal matrix and a generic matrix

prod_blockmatrix <- function(A, B, block = NULL) {
  
  if (is.null(names(A))) names(A) <- 1:length(A)
  A_names <- names(A)
  
  if (is.null(block)) block <- rep(A_names, times = sapply(A, function(x) dim(x)[1]))
  
  C <- matrix(0, length(block), dim(B)[2])
  
  for (b in A_names) {
    ind <- block == b
    C[ind, ] <- A[[b]] %*% B[ind,]
  }
  return(C)
}  


# product of a generic matrix and a block-diagonal matrix

prod_matrixblock <- function(A, B, block = NULL) {
  
  if (is.null(names(B))) names(B) <- 1:length(B)
  B_names <- names(B)
  
  if (is.null(block)) block <- rep(B_names, times = sapply(B, function(x) dim(x)[2]))
  
  C <- matrix(0, dim(A)[1], length(block))
  
  for (b in B_names) {
    ind <- block == b
    C[,ind] <- A[,ind] %*% B[[b]]
  }
  return(C)
}  



# trace of the product of two generic matrices

product_trace <- function(A,B) sum(as.vector(t(A)) * as.vector(B))


# trace of the product of two conformable block-diagonal matrices

product_trace_blockblock <- function(A, B) 
  mapply(function(a, b) product_trace(a,b), a = A, b = B)



# Hedges G correction

J <- function(x) 1 - 3 / (4 * x - 1)
