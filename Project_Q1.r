
library(MASS)

data = read.csv('Project_data.csv')
data = na.omit(data)
data[1:15,]
dim(data)
part1 = data

# In the question, the return is 0.016, as this is the annual return
# To get the daily return, divide it by 360.

rf <- 0.016/360

JenIndex = c()
pvalue   = c()
part1t <- part1 - rf

StockName= c()
JenIndex = c()
pvalue   = c()
part1t <- part1 - rf

Market = part1t[,1]

for (i in 2:101) {
    lm.par1 <- lm(part1t[,i] ~ Market)
    s <- summary(lm.par1)
    tp <- s$coefficients[1,3]
    StockName=c(StockName, colnames(part1t)[i])
    JenIndex =c(JenIndex, round(s$coefficients[1,1][[1]],8))
    pvalue <- c( pvalue,round(pt(tp, df = 249,lower.tail = FALSE),8))
}

library(plyr)
result = data.frame(cbind(StockName, JenIndex, pvalue))
# class(result)
result = arrange(result, pvalue)
result[, 2] <- as.numeric(as.character( result[, 2] ))
result[, 3] <- as.numeric(as.character( result[, 3] ))
result[1:15,]

m = length(pvalue)
alpha = 0.05
P <- c()
for (k in 1:m) {
  P[k]<- round(alpha/(m+1-k),8)
}

result['P'] = P
result['LessEqP'] = pvalue <= P
result[1:15,]

spvalue = result$pvalue

# Holm's step-down procedure

StepDown = rep("Accept", m)

for (k in 1:m) {
    if (spvalue[k] > alpha/(m+1-k)) {
        break
    }
    StepDown[k] = 'Reject'
}

StepDown

# Hochberg's step-up procedure
StepUp <- rep("Reject", m)
for (k in m:1) {
    if (spvalue[k]<= alpha/(m+1-k)) {
        break
    }
    StepUp[k] = 'Accept'
}

StepUp

result$StepDown = StepDown
result$StepUp   = StepUp
result[1:15,]

write.csv(result, 'result_Normal.csv')

cormat <- cor(part1t[,-1])

# number of |correlation| > 0.7
(sum(abs(cormat)>0.7)-100)/2

# number of |correlation| > 0.6
(sum(abs(cormat)>0.6)-100)/2

# number of |correlation| > 0.5
(sum(abs(cormat)>0.5)-100)/2

# number of |correlation| > 0.4
(sum(abs(cormat)>0.4)-100)/2

plot(cormat)

#***********************
#     SOURCE CODE      #
#***********************

Ffun = function(a, b, mu, si, x){
    xs = (x - mu)/si
    as = (a - mu)/si
    bs = (b - mu)/si
    
    f = function(y) (1/y-1/y^3+3/y^5-15/y^7);
    
    if (as > 4 & bs > 4){
        F = 1 - (exp((as^2-bs^2)/2)*f(bs) - exp((as^2-xs^2)/2)*f(xs))/(exp((as^2-bs^2)/2)*f(bs) - f(as))
    }
    else if (as < -4 & bs < -4){
        F = ( exp((bs^2-xs^2)/2)*f(-xs) - exp((bs^2-as^2)/2)*f(-as))/( f(-bs)-exp((bs^2-as^2)/2)*f(-as))
    }
    else{
        denom = pnorm(bs)-pnorm(as)
        if (denom < 0.00001){
            F <- (xs-as)/(bs-as)
        }
        else{
            F <- (pnorm(xs)-pnorm(as))/denom
        }
    }
    return(F)
}



GetTaylerPValue <- function(Zs, Sigma, u, v, Gamma){
    
    # calculate rho
    tmp = Sigma%*%v
    A = v%*%tmp
    rho = (Gamma%*%tmp)/A[1,1]
    
    # calculate Vs in order to find Vup, Vlo
    Vs = (u- (Gamma%*%Zs) + rho*sum(v*Zs))/rho
    if (sum(rho<0)==0){
        Vup <- Inf
    } 
    else{
        Vup <- min(Vs[rho<0])
    }
    
    if (sum(rho>0)==0){
        Vlo <- -Inf
    } 
    else {
        Vlo <- max(Vs[rho>0])
    }
    # here we have Vlo, Vup, v, 
    a  = Vlo
    b  = Vup
    mu = 0
    si = A[1,1]
    x  = sum(v*Zs)
    p_tmp = Ffun(a, b, mu, si, x)
    
    p_tayler = 1- p_tmp
    #p_tayler = p_tmp
    return(p_tayler)
}


# Zs is normal distributed, transformed from p-value, well sorted
# must be sorted before taken as the input
# under Zs ~ N(theta, Sigma)
# here we need to test theta, so, don't take it as the input

Taylor <- function(Zs, Sigma){
    # length of Zs
    # Zs = sort(rnorm(100), decreasing = T)
    len = length(Zs)
    
    alpha = 0.05
    Z_alpha = qnorm(alpha, lower.tail = F)
    
    # the number that bigger than the desired value
    rlen = sum(Zs >= Z_alpha )
    
    # Get Gamma Matrix
    Gamma = rep(1,len)
    Gamma = diag(Gamma)
    Gamma = Gamma[1:rlen,]
    
    # Get u vector 
    u = rep(Z_alpha, rlen) 
    
    Taylor_PValues <- rep(0,rlen)
    for (idx in 1:rlen){
        v = rep(0,len)
        v[idx] = 1
        Taylor_PValues[idx] = GetTaylerPValue(Zs, Sigma, u, v, Gamma)
    }
    return(sort(Taylor_PValues))
}




result = result[,c(1,2,3)]
result[1:15,]

Zs = qnorm(result$pvalue, lower.tail = F)
result$Zs = Zs
result_Taylor = result[result$Zs >= qnorm(alpha, lower.tail = F), ]
result_Taylor

# resampling to find covariance matrix of z
Z <- c()
x <- part1t[,1]
n <- length(x)
numBoot = 200
for (i in 2:101) {
    y<-part1t[,i]
    l <- lm(y~x)
    epshat <- residuals(l)
    z<-c()
    for (b in 1:numBoot) {
        y.b <- predict(l) + sample(epshat,n,replace = T)
        lb <- lm(y.b~x)
        s <- summary(lb)
        tp <- s$coefficients[1,3]
        pvalue <- pt(tp,df=249,lower.tail = FALSE)
        z[b] <- qnorm(pvalue,lower.tail = F)
    }
    Z<-cbind(Z,z)
}

dim(Z)

Zs = Zs[Zs!=-Inf]

#Sigma = diag(length(Zs))
Sigma <- cov(Z)
dim(Sigma)

taylor_pvalue = Taylor(Zs, Sigma)
taylor_pvalue

m = length(taylor_pvalue)
alpha = 0.05
taylor_P <- c()
for (k in 1:m) {
  taylor_P[k]<- alpha/(m+1-k)
}

taylor_P

result_Taylor$taylor_pvalue = taylor_pvalue
result_Taylor = arrange(result_Taylor, taylor_pvalue)
result_Taylor[, 5]  <- as.numeric(as.character( result_Taylor[, 5] ))
result_Taylor$taylor_P = taylor_P
result_Taylor$LessEqP = taylor_pvalue <= taylor_P
result_Taylor

spvalue = result_Taylor$taylor_pvalue
m = length(spvalue)
# Holm's step-down procedure

StepDown = rep("Accept", m)

for (k in 1:m) {
    if (spvalue[k] > alpha/(m+1-k)) {
        break
    }
    StepDown[k] = 'Reject'
}

StepUp = rep("Reject", m)

for (k in 1:m) {
    if (spvalue[k] <= alpha/(m+1-k)) {
        break
    }
    StepUp[k] = 'Accept'
}

result_Taylor$StepDown = StepDown
result_Taylor$StepUp   = StepUp
result_Taylor

write.csv(result_Taylor, 'result_normal_Taylor.csv')

index <- sample(2:101, 10, replace = F)

x <- part1t[,1]

# test for normal by qqplot

stdres <- c()

l1 <- lm(part1t[,index[1]] ~ x)
qqnorm(studres(l1))
qqline(studres(l1))
stdres <- cbind(stdres,studres(l1))


par(mfrow = c(3,3))

l2 <- lm(part1t[,index[2]]~x)
qqnorm(studres(l2))
qqline(studres(l2))
stdres<-cbind(stdres,studres(l2))

l3 <- lm(part1t[,index[3]]~x)
qqnorm(studres(l3))
qqline(studres(l3))
stdres<-cbind(stdres,studres(l3))

l4 <- lm(part1t[,index[4]]~x)
qqnorm(studres(l4))
qqline(studres(l4))
stdres<-cbind(stdres,studres(l4))

l5 <- lm(part1t[,index[5]]~x)
qqnorm(studres(l5))
qqline(studres(l5))
stdres<-cbind(stdres,studres(l5))

l6 <- lm(part1t[,index[6]]~x)
qqnorm(studres(l6))
qqline(studres(l6))
stdres<-cbind(stdres,studres(l6))

l7 <- lm(part1t[,index[7]]~x)
qqnorm(studres(l7))
qqline(studres(l7))
stdres<-cbind(stdres,studres(l7))

l8 <- lm(part1t[,index[8]]~x)
qqnorm(studres(l8))
qqline(studres(l8))
stdres<-cbind(stdres,studres(l8))

l9 <- lm(part1t[,index[9]]~x)
qqnorm(studres(l9))
qqline(studres(l9))
stdres<-cbind(stdres,studres(l9))

l10 <- lm(part1t[,index[10]]~x)
qqnorm(studres(l10))
qqline(studres(l10))
stdres<-cbind(stdres,studres(l10))


# shapiro test
rej <-c()
for (i in 1:10) {
    sh <- shapiro.test(stdres[,i])
    rej[i] <-(sh$p.value<=0.05) 
}

rej

GetBootStat <- function(b, Xbeta_theta, epshat, n){
    y.b <- Xbeta_theta + sample(epshat, n, replace=T)
    thetahat.b <- lm(y.b ~ part1t[,1])$coefficients[1]
    return(thetahat.b)
}


Hybrid <- function(idx, numBoot, part1t){
    y <- part1t[,idx]
    # beta_hat_0
    thetahat <- lm( y ~ part1t[,1])$coefficients[1]
    That <- thetahat - theta
    
    l <- lm(y~part1t[,1]-1)
    epshat <- residuals(l)
    Xbeta_theta <- predict(l)
    
    TS.dist <- rep(0, numBoot)
    
    for (b in 1:numBoot){
        TS.dist[b] = GetBootStat(b, Xbeta_theta, epshat, n)
    }
    
    qua <- seq(0,1,0.01)
    for (j in 1:length(qua)) {
        if (That <= quantile(TS.dist,qua[j])){
            pvalue <- 1-qua[j]
            break
        }
    }
    
    result = list(pvalue, thetahat, TS.dist)
    return(result)
}


numBoot = 200
theta   = 0
hybrid_pvalue  <- c()
n <- length(part1t[,1])

for (idx in 2:101){
    pvalue  = Hybrid(idx, numBoot, part1t)[[1]]
    hybrid_pvalue[idx-1] = pvalue
}

hybrid_pvalue

result_Hybrid = result[, c(1,2,3)]
result_Hybrid$hybrid_pvalue = hybrid_pvalue
result_Hybrid = arrange(result_Hybrid, hybrid_pvalue)
result_Hybrid[,4] = as.numeric(as.character( result_Hybrid[, 4] ))
result_Hybrid$P = P
result_Hybrid$LessEqP = result_Hybrid$hybrid_pvalue <= result_Hybrid$P
result_Hybrid$LessEqP

result_Hybrid$StepDown = rep('Accept', length(result_Hybrid[,1]))
result_Hybrid$StepUp = rep('Accept', length(result_Hybrid[,1]))
result_Hybrid[1:20,]
write.csv(result_Hybrid, 'result_Hyrid.csv')
