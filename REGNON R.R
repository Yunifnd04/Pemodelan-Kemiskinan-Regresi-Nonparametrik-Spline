library(readxl)
data <- read_excel("D:/Kuliah/Semester 5/Regresi Nonparametrik/Dataregnon.xlsx")
View(data)
summary(data)
str(data)
head(data)

library(ggplot2)
ggplot(data, aes(x = X1, y = Y)) +
  geom_point() +
  geom_smooth(method = "lm", color = "orange") +
  labs(x = "X", y = "Y", title = "Scatter Plot dengan Garis Tren")

#Mencari model regresi linear sederhana
reg= lm(formula=Y~X1,data=data)
reg

#Memeriksa Signifikansi Statistik
summary(reg)

# Membuat model regresi linear sederhana dengan metode MLE
library(stats4)
# Mendefinisikan fungsi log-likelihood untuk regresi linear sederhana
logLikelihood <- function(beta, x, y) {
  mu <- beta[1] + beta[2] * x
  -sum(dnorm(y, mean = mu, sd = 1, log = TRUE))
}
# Memulai dengan tebakan awal untuk parameter
initial_guess <- c(intercept = 0, slope = 1)
# Meminimalkan fungsi log-likelihood
result <- optim(par = initial_guess, fn = logLikelihood, x = data$X1, y = data$Y)
# Menampilkan hasil
result$par  #Parameter estimasi terbaik

# Matrix x
X <- matrix(0, nrow = nrow(data), ncol = 2)
X[, 1] <- 1
X[, 2] <- data$X1
X

# Hitung matriks transpose dari matrix X
X_transpose <- t(X)
X_transpose

# Hitung hasil perkalian matrix X' * matrix X
X_mult <- X_transpose %*% X
X_mult

# Hitung matriks invers dari matrix X' * matrix X
X_inverse <- solve(X_mult)
X_inverse

# Banyak n
n <- 37
n_sqrt <- sqrt(n)
n_sqrt


# Matrix y
Y <- matrix(data$IHSG, nrow = nrow(data), ncol = 1)
Y

# Matrix Y Transpose
Y_t <- t(Y)
Y_t

# Matrix identitas 33x33
I <- diag(1, nrow = 33, ncol = 33)
I

# Range nilai K
K_values <- seq(14448, 14468, by = 0.5)

# Inisialisasi vektor untuk menyimpan MSE
MSE_values <- numeric(length(K_values))
MSE_values

# Loop untuk menghitung MSE dengan berbagai nilai K
for (i in 1:length(K_values)) {
  K <- K_values[i]
  A <- X %*% X_inverse %*% X_transpose
  MSE <- n_sqrt * Y_t %*% (I - A * K) %*% Y
  MSE_values[i] <- MSE
}

# Menampilkan hasil MSE untuk berbagai nilai K
result <- data.frame(K = K_values, MSE = MSE_values)
print(result)


# Inisialisasi vektor untuk menyimpan GCV
GCV_values <- numeric(length(K_values))
GCV_values

# Loop untuk menghitung GCV dengan berbagai nilai K
for (i in 1:length(K_values)) {
  K <- K_values[i]
  A <- X %*% X_inverse %*% X_transpose
  MSE <- n_sqrt * Y_t %*% (I - A * K) %*% Y
  GCV_values[i] <- MSE/(n_sqrt * sum (diag (I - A * K)))^2
}
GCV_values[i]

# Menampilkan hasil GCV untuk berbagai nilai K
Tabel <- data.frame(K = K_values, GCV = GCV_values)
print(Tabel)

x1<- matrix(1, nrow = 33, ncol = 1)
x1
x2 <- matrix(c(data$X1),ncol=1)
x2
X <- cbind(x1, x2)
head(X)
Y <- matrix(c(data$Y),ncol=1)
head(Y)

trun <- function(data,a,power)
{
  data[data<a] <- a
  (data-a)^power
}
MPL<-function(x,eps=1e-20)
{
  x<-as.matrix(x)
  xsvd<-svd(x)
  diago<-xsvd$d[xsvd$d>eps]
  if(length(diago)==1)
  {
    xplus<-as.matrix(xsvd$v[,1])%*%t(as.matrix(xsvd$u[,1])/diago)
  }
  else
  {
    xplus<-
      xsvd$v[,1:length(diago)]%*%diag(1/diago)%*%t(xsvd$u[,1:length(diago)])
  }
  return(xplus)
}


gcv1<-function(y,x,m,l)
{
  a<-min(x)+1
  b<-max(x)-1
  k<-seq(a,b,l)
  v<-length(k)
  n<-length(y)
  Gcv<-matrix(nrow=v,ncol=1)
  Mse<-matrix(nrow=v,ncol=1)
  for (j in 1:v)
  {
    w<-matrix(0,ncol=m+1,nrow=n)
    for (i in 1:m)
      w[,i]<-x^(i-1)
    for (i in m+1)
      w[,i]<-trun(x,k[j],m-1)
    wtw<- t(w) %*% w
    z<- MPL(wtw) 
    beta<- z %*% (t(w) %*% y)
    h<- w %*% z %*% t(w)
    mu<-w%*%beta
    MSE<- t(y-mu) %*% (y-mu)/n
    I<-matrix(0,ncol=n,nrow=n)
    for(i in 1: n)
      I[i,i]<-1
    GCV<-(1/n*MSE)/(1/n*sum(diag(I-h)))^2
    Gcv[j]<-GCV
    Mse[j]<-MSE
  } 
  R<-matrix(c(k,Gcv,Mse),ncol=3)
  sort.R<-R[order(R[,2]),]
  S<-sort.R[1:10,]
  cat("Untuk spline order",m,"dengan 1 titik knot, diperoleh knot optimal=",S[1,1]," dengan GCV minimum=",S[1,2],"dan MSE =",S[1,3])
  cat("\nBerikut 10 nilai GCV terkecil, nilai MSE dan letak titik knotnya:\n")
  cat("====================================\n")
  cat("  No  Ttk knot   GCV     MSE   \n")
  cat("====================================\n")
  S
}

model.spline=function(prediktor,respon,m,knots=c(...))
{
  y<-respon
  n<-length(y)
  k<-length(knots)
  w<-matrix(0, ncol=m+k, nrow=n)
  for (i in 1:m)
    w[,i]<-prediktor^(i-1)
  for(i in (m+1):(m+k))
    w[,i]<-trun(prediktor,knots[i-m],m-1)
  wtw<-t(w)%*%w
  Z<-MPL(wtw)
  beta<-Z%*%t(w)%*%y
  yfits<-w%*%beta
  res<-y-yfits
  MSE<-t(y-yfits)%*%(y-yfits)/n
  I<-matrix(0,ncol=n,nrow=n)
  for(i in 1:n)
    I[i,i]<-1
  h<-w%*%MPL(wtw)%*%t(w)
  GCV<-(1/n*MSE)/(1/n*sum(diag(I-h)))^2
  q<-seq(min(prediktor),max(prediktor),length=1000)
  u<-matrix(0,ncol=m+k,nrow=1000)
  cat("\n Spline orde",m)
  cat("\n Titik Knots  = c( ",format(knots),")")
  cat("\n Nilai GCV    = ",format(GCV),
      "\n Nilai MSE    = ",format(MSE),"\n")
  cat("\n ******************************************************************")
  cat("\n      Koefisen         Estimasi")
  cat("\n ******************************************************************")
  for(i in 1:(m+k))
    cat("\n     beta[",i-1,"]          ",format(beta[i]))
  cat("\n ******************************************************************")
  par(mfrow=c(1,1))
  z0=cbind(prediktor,respon)
  z1=z0[order(z0[,1]),]
  x1=z1[,1]
  y1=z1[,2]
  w1<-matrix(0, ncol=m+k, nrow=n)
  for (i in 1:m)
    w1[,i]<-x1^(i-1)
  for(i in (m+1):(m+k))
    w1[,i]<-trun(x1,knots[i-m],m-1)
  yfits1<-w1%*%beta
  plot(x1,y, type="p",xlim=c(min(prediktor),max(prediktor)),ylim=c(min(respon),max(respon)),
       xlab="Indeks Pembangunan Manusia",ylab="Persentase Kemsikinan")
  par(new=T)
  plot(x1,yfits1, type="l",col="red",
       xlim=c(min(prediktor),max(prediktor)),
       ylim=c(min(respon),max(respon)), xlab="Indeks Pembangunan Manusia",ylab="Persentase Kemiskinan")
}


gcv1(data$Y,data$X1,2,1)
model.spline(data$X1,data$Y,2,c(66.51)

model <- lm(Y~X1,data=data)
tabel <- anova(model)
tabel

Ftabel <- qf(1-0.05,1,31)
Ftabel

             
