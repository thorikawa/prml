# Training data
x=list(c(1,0),c(2,0), c(3,0), c(0,1), c(0,2), c(0,3))
t=c(1,1,1,0,0,0)
training_data_num <- length(x)

# Sigmoid Function
sigmoid <- function (x) { 
  if (class(x) == "matrix") {
    apply(x, c(1,2), function (y) { return(1/(1+exp(-y))) })
  } else {
    sapply(x, function (y) { return(1/(1+exp(-y))) })
  }
}

# Gaussian process main function
gp <- function (kernel, title_str, sub_str="") {
  # Gram matrix
  K <- sapply(x, function(a) {
    sapply(x, function (b) kernel(a,b))
  })
  beta <- 25
  
  C_N <- K + diag(training_data_num)/beta
  
  newa <- function (a) {
    W <- diag(c(sigmoid(a)*(1-sigmoid(a))));
    C_N %*% solve(diag(training_data_num) + W %*% C_N) %*% (t - sigmoid(a) + W %*% a);
  }
  a<-numeric(training_data_num)
  for (i in 1:1000) {
    a <- newa(a)
  }
  print(a);
  
  # Plot predictive distribution
  nx <- ny <- seq(-5,5,len=100);
  nz <- sapply(ny, function (ny_) {
    sapply(nx, function (nx_) {
      k<-sapply(x, function (c) kernel(c(nx_,ny_),c));
      return( t(k) %*% (t-sigmoid(a)) );
    })
  })
  nz <- sigmoid(nz)
  input <- list(x=nx, y=ny, z=nz)
  image(input);
  contour(input,add=T);
  x0 <- sapply(x[t==0], function(a) a[1])
  y0 <- sapply(x[t==0], function(a) a[2])
  points(x0, y0, col="green", pch=20);
  x1 <- sapply(x[t==1], function(a) a[1])
  y1 <- sapply(x[t==1], function(a) a[2])
  points(x1, y1, col="blue", pch=20);
  title(title_str, sub_str);
}

jpeg(quality=90, pointsize=20, width=600, height=600);

# Gaussian kernel
sigma <- 0.2
kernel <- function (x1, x2) {
  exp(-sum((x1-x2)*(x1-x2))/(2*sigma^2));
}
gp(kernel, "gaussian kernel", "sigma=0.2");

# Gaussian kernel
sigma <- 0.4
kernel <- function (x1, x2) {
  exp(-sum((x1-x2)*(x1-x2))/(2*sigma^2));
}
gp(kernel, "gaussian kernel", "sigma=0.4");

# Linear kernel
kernel <- function (x1, x2) {
  sum(x1*x2);
}
gp(kernel, "linear kernel");

# Exponential kernel
theta <- 1.0
kernel <- function (x1, x2) {
  exp(-theta*sqrt(sum((x1-x2)*(x1-x2))));
}
gp(kernel, "exponential kernel", "theta=1.0");

# Exponential kernel
theta <- 0.3
kernel <- function (x1, x2) {
  exp(-theta*sqrt(sum((x1-x2)*(x1-x2))));
}
gp(kernel, "exponential kernel", "theta=0.3");

# Polynomial kernel
kernel <- function (x1, x2) {
  (sum(x1*x2)+1)^2;
}
gp(kernel, "polynomial kernel", "degree=2");

# Polynomial kernel
kernel <- function (x1, x2) {
  (sum(x1*x2)+1)^3;
}
gp(kernel, "polynomial kernel", "degree=3");

dev.off();