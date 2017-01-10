
if (getwd() != "~/Documents/PhD/Courses/Computational-Statistics/Cwk") {
    setwd("~/Documents/PhD/Courses/Computational-Statistics/Cwk")
}

require(mvtnorm)
set.seed(42)

# multivariate Gaussian Hidden Markov Model

# model as if weather forecasting: 
# state vector X_t represents temperature on consecutive days at d different weather stations
# observed values Y_t are forecasts 

# forecast errors may occur due to spatial/temporal aggregation, as well as model errors

# try fitting with & without trend in state space (


####################################################################################################

# 1.1 SYNTHESISE DATA                                                                           ####

sim.mvn <- function(TT = 100, params, seed = 42) {
    
    set.seed(seed)
    
    mu.0 <- params$mu.0; sig.0 <- params$sig.0; phi <- params$phi
    sig.v <- params$sig.v; sig.w <- params$sig.w; rho <- params$rho
    
    d <- length(mu.0)
    x <- y <- array(dim = c(TT, d))
    
    x[1,] <- rmvnorm(1, mu.0, sig.0)      # stationary distribution
    
    # sample state X_t sequentially
    for(i in 2:TT) {
        x[i,] <- x[i-1,] * phi + rmvnorm(1, rep(0, d), sig.v)
    }
    
    # calculate observations Y_t as batch (dependent only on X_t)
    y <- x * rho + rmvnorm(TT,  rep(0, d), sig.w)
    
    list(x = x, y = y)
}

# bivariate data
par.2v <- list(mu.0 = c(3,5), sig.0 = diag(c(2,4)),                           # initial state
           phi = 1, sig.v = diag(c(0.5,1)),                               # state path
           rho = 1, sig.w = matrix(c(.6, .2, .2, .3), ncol = 2))          # forecast error

dat.2v <- sim.mvn(TT = 1000, par.2v)       


####################################################################################################

# 1.2 PARTICLE FILTER                                                                           ####

# use alpha in [0,1] to control resampling (never/adaptive/always)

SIR.filter <- function(y, params, N = 100, alpha = 1, seed = 101) {
    
    set.seed(seed)
    
    mu.0 <- params$mu.0; sig.0 <- params$sig.0
    sig.v <- params$sig.v; sig.w <- params$sig.w; rho <- params$rho
    
    if(class(y) == "numeric") {
        
        # UNIVARIATE
        
        TT <- length(y)
        
        # initial model setup, time t = 1
        particles <- rnorm(N, prior$m, prior$s)                 # simulate particles
        w <- dnorm(y[1], particles, sig)                   # calculate weights
        
        # store log-likelihood BEFORE normalizing
        ll <- log(mean(w))
        
        w <- w / sum(w)                                         # normalise weights
        
        # store mean & variance of filter, effective sample size
        filt.M <- sum(w * particles)                        
        filt.V <- sum(w * particles^2) - filt.M^2
        ESS <- 1/sum(w^2)
        
        # resample & propagate particles
        for (t in 2:TT) {
            
            # additional step: resample according to weights, if specified
            if (ESS[t-1] < (N * alpha)) {
                resamp <- sample(1:N, prob = w, size = N, rep = T)
                particles <- particles[resamp]
            }
            
            # propagate resampled particles
            particles <- particles * rho + rnorm(N, 0, sig)
            
            w <- dnorm(data$y[t], particles, sig)                   # calculate weights
            
            ll[t] <- ll[t-1] + log(mean(w))
            
            w <- w / sum(w)                                         # normalise weights
            
            filt.M[t] <- sum(w * particles)                         # filter mean
            filt.V[t] <- sum(w * particles^2) - filt.M[t]^2         # filter variance
            ESS[t] <- 1/sum(w^2)                                    # effective sample size
        }
        
    } else {
        
        # MULTIVARIATE
        
        TT <- nrow(y)
        d <- ncol(y)
        
        filt.M <- filt.V <- array(dim = c(TT, d))
        
        # initial model setup, time t = 1
        particles <- rmvnorm(N, mu.0, sig.0)                    # simulate particles
        w <- apply(particles, 1, function(pp) dmvnorm(y[1,], pp, sig.w))    # calculate weights
        
        # store log-likelihood BEFORE normalizing
        ll <- log(mean(w))
        
        w <- w / sum(w)                                         # normalise weights
        
        # store mean & variance of filter, effective sample size
        filt.M[1,] <- apply(sweep(particles, 1, w, "*"), 2, sum)
        filt.V[1,] <- apply(sweep(particles^2, 1, w, "*"), 2, sum) - filt.M[1,]^2
        ESS <- 1/sum(w^2)
        
        # resample & propagate particles
        for (t in 2:TT) {
            
            # additional step: resample according to weights if ESS is below threshold
            if (ESS[t-1] < (N * alpha)) {
                resamp <- sample(1:N, prob = w, size = N, rep = T)
                particles <- particles[resamp,]
            } 
            
            # propagate particles
            particles <- particles * rho + rmvnorm(N, rep(0, d), sig.w)
            
            w <- apply(particles, 1, function(pp) dmvnorm(y[t,], pp, sig.w))    # calculate weights
            
            ll[t] <- ll[t-1] + log(mean(w))
            
            w <- w / sum(w)
            
            filt.M[t,] <- apply(sweep(particles, 1, w, "*"), 2, sum)
            filt.V[t,] <- apply(sweep(particles^2, 1, w, "*"), 2, sum) - filt.M[t,]^2 
            ESS[t] <- 1/sum(w^2)                                    # effective sample size
        }
    }
    
    list("mean" = filt.M, "var" = filt.V, "ESS" = ESS, "ll" = ll)
}



####################################################################################################

# 1.3 KALMAN FILTER                                                                             ####

Kalman.filter <- function(y, s.eps, s.eta, a0, P0) {
    
    d <- ncol(y)
    TT <- nrow(y)
    
    a <- array(dim = c(TT + 1, d))
    P <- array(dim = c(TT + 1, d, d))
    
    v <- array(dim = c(TT, d))
    FF <- K <- array(dim = c(TT, d, d))
    
    a[1,] <- a0; P[1,,] <- P0
    
    for (t in 1:(TT)) {
        v[t,] <- y[t,] - a[t,]
        FF[t,,] <- P[t,,] + s.eps
        K[t,,] <- P[t,,] / FF[t,,]
        
        a[t+1,] <- a[t,] + (K[t,,] %*% v[t,])
        P[t+1,,] <- P[t,,] * (1 - K[t,,]) + s.eta
    }
    list(a = a[-1,], P = P[-1,,])
}

# require(dlm)
# need to find out how to properly estimate s.eps and s.eta before this will work!
kf <- Kalman.filter(y = dat$y,
                    s.eps = diag(c(0.5,1)), 
                    s.eta = matrix(c(.6, .2, .2, .3), ncol = 2),
                    a0 = c(4, 4), P0 = matrix(rep(1,4), ncol = 2))

plot(dat$x[,1], type = "l", lwd = 2, xlab = "", ylab = "", main = paste0("Var ", varb))
lines(dat$y[,1], col = "darkred")
lines(kf$mean[,1], col = "blue")

plot(kf$a[,1], type = "l")

####################################################################################################

# 2.1 EFFECT OF VARYING N: BIVARIATE                                                            ####

sis.2v.100 <- SIR.filter(dat.2v$y, params = par.2v, N = 100, alpha = 0)
sia.2v.100 <- SIR.filter(dat.2v$y, params = par.2v, N = 100, alpha = 0.5)
sir.2v.100 <- SIR.filter(dat.2v$y, params = par.2v, N = 100, alpha = 1)

plot(dat.2v$x[,1], type = "l", lwd = 2, xlab = "", ylab = "", main = "State mean (1)")
lines(sis.2v.100$mean[,1], col = "blue")
lines(sir.2v.100$mean[,1], col = "green3")
lines(sia.2v.100$mean[,1], col = "red")

plot(dat.2v$x[,2], type = "l", lwd = 2, xlab = "", ylab = "", main = "State mean (2)")
lines(sis.2v.100$mean[,2], col = "blue")
lines(sir.2v.100$mean[,2], col = "green3")
lines(sia.2v.100$mean[,2], col = "red")


plot(sis.2v.100$var[,1], type = "l", col = "blue", xlab = "", ylab = "", main = "State variance (1)")
lines(sir.2v.100$var[,1], col = "green3")
lines(sia.2v.100$var[,1], col = "red")

plot(sis.2v.100$var[,2], type = "l", col = "blue", xlab = "", ylab = "", main = "State variance (2)")
lines(sir.2v.100$var[,2], col = "green3")
lines(sia.2v.100$var[,2], col = "red")

plot(sis.2v.100$ESS, type = "l", col = "blue", xlab = "", ylab = "", ylim = c(0,100),
     main = "Effective sample size")
lines(sir.2v.100$ESS, col = "green3")
lines(sia.2v.100$ESS, col = "red")

# can we plot SIR support particles (as in slides3.123)? Or do we need more output?
plot(sis.2v.100$ll, type = "l", col = "blue", xlab = "", ylab = "", 
     main = "Log-likelihood")
lines(sir.2v.100$ll, col = "green3")
lines(sia.2v.100$ll, col = "red")


####################################################################################################

# 3.1 PARAMETER ESTIMATION - EM ALGORITHM                                                       ####

####################################################################################################

# JUNKYARD                                                                                      ####

# plot of actual & observed (forecast) data
par(mfrow = c(2, ceiling(ncol(dat$x)/2)), mar = c(2,2,3,1))
invisible(sapply(1:ncol(dat$x), function(varb) {
    plot(dat$x[,varb], type = "l", lwd = 2, xlab = "", ylab = "", main = paste0("Var ", varb))
    lines(dat$y[,varb], col = "magenta3")
}))

# run model using different values of alpha
{
    sis <- SIR.filter(dat$y, prior = list(mu.0, sig.0), rho, sig.v, sig.w, alpha = 0)
    
    sia.25 <- SIR.filter(dat$y, prior = list(mu.0, sig.0), rho, sig.v, sig.w, alpha = 0.25)
    
    sia.50 <- SIR.filter(dat$y, prior = list(mu.0, sig.0), rho, sig.v, sig.w, alpha = 0.5)
    
    sia.75 <- SIR.filter(dat$y, prior = list(mu.0, sig.0), rho, sig.v, sig.w, alpha = 0.75)
    
    sir <- SIR.filter(dat$y, prior = list(mu.0, sig.0), rho, sig.v, sig.w, alpha = 1)
}

# plot results

par(mfrow = c(1,1))

plot(dat$x[,1], type = "l", lwd = 2, xlab = "", ylab = "")
lines(dat$y[,1], col = "magenta3")

lines(sis$mean[,1], col = "blue")
lines(sia.50$mean[,1], col = "green3")
lines(sir$mean[,1], col = "orange")


plot(sis$ESS, type = "l", ylim = c(0,100), col = "blue")
lines(sia.25$ESS, type = "l", col = "red")
lines(sia.50$ESS, type = "l", col = "green3")
lines(sia.75$ESS, type = "l", col = "purple")
lines(sir$ESS, type = "l", col = "orange")

abline(h = mean(sia.25$ESS), col = "red")
abline(h = mean(sia.50$ESS), col = "green3")
abline(h = mean(sia.75$ESS), col = "purple")
abline(h = mean(sir$ESS), col = "orange")

plot(sis$ll, type = "l")
lines(sia.25$ll, col = "red")
lines(sia.50$ll, col = "green3")
lines(sia.75$ll, col = "purple")
lines(sir$ll, col = "orange")