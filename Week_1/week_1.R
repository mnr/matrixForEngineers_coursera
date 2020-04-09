returnMatrix <- function(tnrow, tncol){
  A <- matrix(as.integer(runif(tnrow * tncol) * 10) ,
              nrow = tnrow, ncol = tncol)
  return(A)
}


# Matrix Definitions ---------------------------------------------------------------


A <- matrix(c("a","b","c","d"), nrow = 2, byrow = "FALSE")

# The main diagonal of a matrix A are the entries 
# $a_{ij}$ where $i = j$
  
  (a) Write down the three-by-three matrix with ones on the diagonal and zeros elsewhere.

```{r}
matrix(c(1,0,0,
         0,1,0,
         0,0,1), nrow = 3)
```

(b) Write down the three-by-four matrix with ones on the diagonal and zeros elsewhere.
```{r}
matrix(c(1,0,0,0,
         0,1,0,0,
         0,0,1,0), nrow = 3, byrow = TRUE)
```

(c) Write down the four-by-three matrix with ones on the diagonal and zeros elsewhere.
```{r}
matrix(c(1,0,0,
         0,1,0,
         0,0,1,
         0,0,0), nrow = 4, byrow = TRUE)
```

Define the matrices

```{r}
A <- matrix(c(2,1,1,-1,-1,1), nrow = 2)
B <- matrix(c(4,2,-2,-4,1,-2), nrow = 2)
C <- matrix(c(1,2,2,1), nrow = 2)
D <- matrix(c(3,4,4,3), nrow = 2)
E <- matrix(c(1,2), nrow = 2)
```
compute if defined

```{r}
B-2*A
```

```{r}
3*C-E
```

```{r}
A%*%C
```


```{r}
C%*%D
```


```{r}
C%*%B
```

```{r}
A <- matrix(c(1,2,2,4), nrow = 2)
B <- matrix(c(2,1,1,3), nrow = 2)
C <- matrix(c(4,0,3,2), nrow = 2)

```

```{r}
A%*%B
```


```{r}
A%*%C
```


```{r}
B == C
```


```{r}
identical(B,C)
```


```{r}
all.equal(B,C)
```




```{r}
A <- matrix(c(1,1,1,1,2,3,1,3,4), nrow = 3)
D <- matrix(c(2,0,0,0,3,0,0,0,4), nrow = 3)
```

```{r}
A%*%D
```


```{r}
D%*%A
```

Prove the associative law for matrix multiplication. 
That is, let 
A be an m-by-n matrix, 
B an n-by-p matrix, and 
C a p-by-q matrix.

Prove that `A(BC) = (AB)C`

```{r}
m <- 3
n <- 4
p <- 5
q <- 6
A <- matrix(rnorm(m*n), nrow = m)
B <- matrix(rnorm(n*p), nrow = n)
C <- matrix(rnorm(p*q), nrow = p)

```

```{r}
A %*% (B %*% C) 
```

```{r}
(A %*% B) %*% C
```

create a zero matrix

This works because of element recycling. Need to declare number of row and column
```{r}
matrix(0, nrow = 5, ncol = 4)
```
...or...
```{r}
matrix(1, nrow = 3, ncol = 2)
```

Create an identity matrix
```{r}
diag(nrow = 3)
```

There are other uses of diag()

Creating a banded matrix is not so straight-forward

Tri-diagonals are built-in

```{r}
A <- matrix(runif(25), nrow = 5)
A[!upper.tri(A)] <- 0
A
```

```{r}
A <- matrix(runif(25), nrow = 5)
A[!lower.tri(A)] <- 0
A
```

AB = 0 when A and B are not zero
```{r}
A <- matrix(c(-1,2,
              4,-8), nrow = 2, byrow = TRUE)
A
B <- matrix(c(2,0,
              1,0), nrow = 2, ncol = 2, byrow = TRUE)
B
C <- matrix(0, nrow = 2, ncol = 2)
A

A[1,1]*B[1,1] + A[1,2]*B[2,1] == 0
-1 * B[1,1] + 2 * B[2,1] == 0
-B[1,1] + 2 * B[2,1] == 0
2 * B[2,1] == B[1,1]

A%*%B

A%*%B # should equal zero
```

Verify that `A` * `B` = $\begin{pmatrix}
a1b1 & 0\\
0 & a2b2
\end{pmatrix}$
  

a1 <- as.integer(runif(1) * 10)
a2 <- as.integer(runif(1) * 10)
b1 <- as.integer(runif(1) * 10)
b2 <- as.integer(runif(1) * 10)
A <- matrix(c(a1, 0, 0, a2), nrow = 2)
B <- matrix(c(b1, 0, 0, b2), nrow = 2)

A
B
A%*%B


# Transpose and Inverses --------------------------------------------------

# Prove that (AB)^T = B^T*A^T

A <- returnMatrix(4,4)
B <- returnMatrix(4,4)

t(A%*%B) == t(B)%*%t(A)
identical(t(A%*%B), t(B)%*%t(A))

# Show using the transpose operator that any square matrix 
# can be written as the sum of a symmetric and a skew-symmetric matrix.

# create a symmetrical square matrix
symmSqA <- returnMatrix(3,3)
diag(symmSqA) <- 0
symmSqA[lower.tri(symmSqA)] <- t(symmSqA)[lower.tri(symmSqA)]

# set up the equation
(symmSqA + t(symmSqA))/2 + (symmSqA - t(symmSqA))/2
symmSqA

# checking the formulas for a symmetric and skew-symmetric matrix
newB <- returnMatrix(4,4)
diag(newB) <- 0
newB
newB + t(newB)
newB - t(newB)

# Prove that A^T A is symmetric.
tposeAMat <- returnMatrix(4,5)
t(tposeAMat)%*%tposeAMat
tposeAMat%*%t(tposeAMat)

# compute (A^T)A and show that it is a symmetric matrix and that the
# sum of its diagonal elements is the sum of the squares of all the
# elements of A
A <- returnMatrix(3,2)
B <- t(A)%*%A
identical(sum(diag(B)), sum(A^2))

# The trace of a square matrix B, denoted as TrB, is the sum of the diagonal
# elements of B. 
# Prove that Tr(ATA) is the sum of the squares of all the elements of A.

B <- returnMatrix(3,3)
sum(diag(B)) # trace of B - TrB
identical(sum(diag(t(B)%*%B)), sum(B^2))
