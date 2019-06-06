################################################################
###### Functions written to explore                       ######
###### how the Secretary Problem and Distributions relate ######
################################################################

#################################################
### Ways to generate some distributions in R: ###
## Uniform
#?runif(n,min,max)
## Normal / Gaussian
#?rnorm(n,mean=0,sd=1)
## Beta
#?rbeta(n,a,b)
## Binomial
#?rbinom(n,size,prob)
## Poisson
#?rpois(n,lambda)
## Logistic
#?rlogis(n,location=0,scale=1)
## Exponential
#?rexp(x, rate)
## Lognormal
#?rlnorm(n,meanlog=0,sdlog=1)
## Gamma
#?rgamma(n,shape,rate=1,scale=1/rate)
## Weibull
## Hypergeometric
## Triangular
## Geometric
## Pareto
#################################################



###########################################
### Building block supporting functions ###
###########################################

###############################################################
### Variables to enter in (most of) the suppporting functions:
### 1. alpha & beta (a & b)
### 2. normalized or not
### 3. population size
### 4. sample size
### 5. repetition number
###############################################################

###
### Reverse ECDF function
###
ecdf_fun <- function(x,percentile) ecdf(x)(percentile)

### This function returns beta distribution normalized
### x centered at 0
### y-curve covered area = 1
### and a standard deviation of 1
dBetaNormalized <- function(x,a,b) {
    ## the theoretical mean and stddev for a given beta distribution
    theoretical_mean <- a/(a+b)
    theoretical_stddev <- (a*b/((a+b)^2 * (a+b+1)))^.5
    xmax <- (1-theoretical_mean)/theoretical_stddev
    xmin <- (0-theoretical_mean)/theoretical_stddev
    xdist <- xmax - xmin
    ## x input here should assume x of normalized curve
    ## i.e. centering at 0
    ## now convert x back to x on a beta distribution
    ## i.e. supposedly bounded by 0 & 1
    xadjusted <- (x - xmin)/xdist

    ## x distance is divided by theoretical_stddev (above),
    ## to maintain that the area under curve equal to 1
    ## y is multipled back by theoretical_stddev.
    normalized_y = dbeta(xadjusted,a,b)*theoretical_stddev

    return(normalized_y)
}
## usage, e.g.
#plot(seq(-2,2,0.0001),dBetaNormalized(seq(-2,2,0.0001),1,1),type='l')

xNormalized <- function(x,a,b) {
    ## the theoretical mean and stddev for a given beta distribution
    theoretical_mean <- a/(a+b)
    theoretical_stddev <- (a*b/((a+b)^2 * (a+b+1)))^.5

    return((x-theoretical_mean)/theoretical_stddev)
}
## usage, e.g. check the normalized X limits of beta(1,1)
#xNormalized(seq(0,1,0.1),1,1)

### This function returns a random beta distribution normalized
### a center point at 0
### a standard deviation = 1
rBetaNormalized <- function(n,a,b) {
    y = rbeta(n,a,b);
    ## the theoretical mean and stddev for a given beta distribution
    theoretical_mean = a/(a+b);
    theoretical_stddev = (a*b/((a+b)^2 * (a+b+1)))^.5;
    normalized_y = (y - theoretical_mean)/theoretical_stddev;
    return(normalized_y);
}


rBetaMeanZero <- function(n,a,b) {
    y = rbeta(n,a,b);
    ## the theoretical mean and stddev for a given beta distribution
    theoretical_mean = a/(a+b);
    # theoretical_stddev = (a*b/((a+b)^2 * (a+b+1)))^.5;
    meanzero_y = (y - theoretical_mean);
    return(meanzero_y);
}


SampleGen <- function(n,a,b,normal,popfactor,samsize,repfactor) {
    ## scale up the population by # popfactor and create y (the population)
    if(normal==0) {
        y = rbeta(n,a,b);
        for(i in 1:popfactor) {
            y = c(y,rbeta(n,a,b));
        }
    }
    else if(normal==1) {
        y = rBetaNormalized(n,a,b);
        for(i in 1:popfactor) {
            y = c(y,rBetaNormalized(n,a,b));
        }
    }
    else {
        print("Please specify if the distribution should be normalized.");
        return(-1);
    }

    ## Create samples of repfactor for every 1:(sample size -1)
    #TotalSampleNumber = (samsize - 1)*repfactor;
    TotalSampleNumber = repfactor;
    SampleMatrix <- numeric(0);
    for(N in 1:TotalSampleNumber)
        ## each row can be a sample of samsize
        SampleMatrix <- cbind(SampleMatrix,sample(y,samsize));

    return(SampleMatrix);
}


SampleGen2 <- function(n,a,b,normal,popfactor,samsize,repfactor) {
    ## scale up the population by # popfactor and create y (the population)
    if(normal==0) {
        y = rbeta(n,a,b);
        for(i in 1:popfactor) {
            y = c(y,rbeta(n,a,b));
        }
    }
    else if(normal==1) {
        y = rBetaNormalized(n,a,b);
        for(i in 1:popfactor) {
            y = c(y,rBetaNormalized(n,a,b));
        }
    }
    else if(normal==2) {
        y = rBetaMeanZero(n,a,b);
        for(i in 1:popfactor) {
            y = c(y,rBetaMeanZero(n,a,b));
        }
    }
    else {
        print("Please specify if the distribution should be normalized.");
        return(-1);
    }

    ## Create samples of repfactor for every 1:(sample size -1)
    #TotalSampleNumber = (samsize - 1)*repfactor;
    TotalSampleNumber <- repfactor;
    SampleMatrix <- numeric(0);
    for(N in 1:TotalSampleNumber)
        ## each row can be a sample of samsize
        SampleMatrix <- cbind(SampleMatrix,sample(y,samsize));

    return(SampleMatrix);
}


SampleGen3 <- function(n,a,b,normal,popfactor) {
    ## scale up the population by # popfactor and create y (the population)
    if(normal==0) {
        y = rbeta(n,a,b);
        for(i in 1:popfactor) {
            y = c(y,rbeta(n,a,b));
        }
    }
    else if(normal==1) {
        y = rBetaNormalized(n,a,b);
        for(i in 1:popfactor) {
            y = c(y,rBetaNormalized(n,a,b));
        }
    }
    else if(normal==2) {
        y = rBetaMeanZero(n,a,b);
        for(i in 1:popfactor) {
            y = c(y,rBetaMeanZero(n,a,b));
        }
    }
    else {
        print("Please specify if the distribution should be normalized.");
        return(-1);
    }

    ## return only a vector but not a sample matrix
    return(y);
}


SampleGen4 <- function(n,a,b,normal) {
    ## assigned signal (a=0, b=1) to return a normal / Gaussian distribution
    ## despite it is possible to have a beta(0,1) distribution
    if (a == 0 & b == 1) {
        return(rnorm(n, 0, 1))
    } else {
        ## otherwise return a beta distribution
        if (normal == 0) {
            return(rbeta(n, a, b))
        } else if (normal == 1) {
            return(rBetaNormalized(n, a, b))
        } else if (normal == 2) {
            return(rBetaMeanZero(n, a, b))
        } else {
            print("Please specify if the distribution should be normalized.")
            return(-1);
        }
    }
}



#################################################################
### Explore optimization thresholds for various distributions ###
#################################################################

### Thresholds for uniform distributions, e.g.
#dunif(seq(0,1,0.1),0,1)
## Described in Gilbert & Mosteller (1966) section 5a (p. 65)
GMValueUniformOptimalPolicy <- function(n) {
    if (n == 1)
        return (1/2)
    else
        return (1/2 *(1 + GMValueUniformOptimalPolicy(n-1)^2))
}

## Described in Gilbert & Mosteller (1966) 5b-2 (p. 67)
GMIntegralUniformOptimalPolicy <- function(n) {
    if (n == 1)
        return (1/2)
    else {
        ## recursive call the function 1 time, instead of calling it 3 times below
        RecursiveVal <- GMIntegralUniformOptimalPolicy(n-1)
        ## I used x/x below to replace 1 because there is no way to integrate constant
        return (integrate((function (x) x),lower=RecursiveVal,upper=1,rel.tol=1e-5)$value + RecursiveVal*integrate((function (x) x/x),lower=0,upper=RecursiveVal,rel.tol=1e-5)$value)
    }
}
### with options(expressions=5000) can only go up to GMValueUniformOptimalPolicy(625)

OptimalPolicyUniform <- function(n,N) {
    n <- N - (n-1) ### n-th item in the sequence with N total items
    return (GMValueUniformOptimalPolicy(n))
}

### To get the thresholds from 1st to 40th items when N = 40, do
#sapply(1:40, OptimalPolicyUniform, N=40)
#sapply(1:10, GMValueUnformOptimalPolicy)
#sapply(1:10, GMIntegralUniformOptimalPolicy)

### Jonathan's thresholds for uniform beta (1,1) are basically
### normalized threshold according to the mean & sd of the beta distribution
### (from the formulae to obtain the mean & sd of beta distribution)
#beta11mean <- sqrt((1*1)/(((1+1)^2)*(1+1 + 1))) #sqrt((a*b)/(((a+b)^2)*(a+b + 1)))
#beta11sd <- 1/(1+1) #a/(a+b)
#(sapply(1:25, OptimalPolicyUniform, N=25) - beta11mean)/beta11sd
## > rev(fkb11[1:25,1])
##  [1]  1.494147e+00  1.485364e+00  1.475893e+00  1.465649e+00  1.454533e+00
##  [6]  1.442425e+00  1.429186e+00  1.414644e+00  1.398595e+00  1.380785e+00
## [11]  1.360903e+00  1.338553e+00  1.313236e+00  1.284299e+00  1.250881e+00
## [16]  1.211817e+00  1.165485e+00  1.109554e+00  1.040532e+00  9.529103e-01
## [21]  8.373764e-01  6.765823e-01  4.330127e-01 -3.885781e-16          -Inf
## > (sapply(1:25, OptimalPolicyUniform, N=25) - beta11mean)/beta11sd
##  [1] 1.5023165 1.4941473 1.4853637 1.4758927 1.4656491 1.4545328 1.4424253
##  [8] 1.4291856 1.4146440 1.3985947 1.3807853 1.3609026 1.3385534 1.3132357
## [15] 1.2842986 1.2508809 1.2118169 1.1654850 1.1095538 1.0405318 0.9529103
## [22] 0.8373764 0.6765823 0.4330127 0.0000000
### Note: the number -3.885781e-16 by Jonathan is a manipulated tiny positive
## de-normalization of Jonathan's values
#rev(fkb11[1:N,1])*sqrt( 1*1 / ((1+1)^2 * (1+1 +1))) + (1/(1+1))

## Described in Gilbert & Mosteller (1966) section 5b (p. 68)
### The function 't' part of the cumulative distribution function (CDF) of the standard normal distribution
### https://en.wikipedia.org/wiki/Normal_distribution#Cumulative_distribution_function
### (other that integrate() also check out ?cumsum and ?auc or ?ecdf (for raw data)
### http://www.r-bloggers.com/from-area-under-the-curve-to-the-fundamental-theorem-of-calculus/
lambda <- function(t) { exp(-(t^2)/2) }
normfunCDF <- function(x) {1/sqrt(2*pi)*integrate(lambda,lower=-Inf,upper=x,rel.tol=1e-5)$value}
### defined on Gilbert & Mosteller (1966) section 5b (p. 68)
normfunY <- function(x) { exp(-(x^2)/2)/sqrt(2*pi) }
GMNormalOptimalPolicy <- function(n) {
    if (n == 1)
        return (0)
    else {
        RecursiveVal <- GMNormalOptimalPolicy(n-1)
        return (normfunY(RecursiveVal) + RecursiveVal*normfunCDF(RecursiveVal))
    }
}

OptimalPolicyNormal <- function(n,N) {
    n <- N - (n-1) ### n-th item in the sequence with N total items
    return (GMNormalOptimalPolicy(n))
}

### To get the thresholds from 1st to 10th items when N = 10, do
#sapply(1:10, OptimalPolicyNormal, N=10)
#sapply(1:10, GMNormalOptimalPolicy)
#rev(c(-Inf,sapply(1:(N-1), GMNormalOptimalPolicy)))
### Values verified on Gilbert & Mosteller (1966) Table 13 (p. 69)

GMNormalCondExpVal <- function(n) {
    return (normfunY(GMNormalOptimalPolicy(n)) / (1 - normfunCDF(GMNormalOptimalPolicy(n))))
}
## To get conditional expected values, i.e. value given reaching a position from start
#sapply(1:10, GMNormalCondExpVal)
#rev(c(0,sapply(1:(N-1), GMNormalCondExpVal)))

### beta functions (see Beta function wiki)
alphabeta <- function(u,a,b) { u^(a-1) * (1-u)^(b-1) }
betacompletefun <- function(a,b) {
    integrate(alphabeta,a=a,b=b,lower=0,upper=1,rel.tol=1e-5)$value
}
betaincompletefun <- function(x,a,b) {
    integrate(alphabeta,a=a,b=b,lower=0,upper=x,rel.tol=1e-5)$value
}
### y function of beta
abfunY <- function(x,a,b) {
    1/integrate(alphabeta,a=a,b=b,lower=0,upper=1,rel.tol=1e-5)$value * alphabeta(x,a,b)
}
### CDF function of beta
abfunCDF <- function(x,a,b) {
    betaincompletefun(x,a,b)/betacompletefun(a,b) ## from beta distribution wiki page
}
abXfx <- function(x,a,b) { ##  Gilbert & Mosteller, 1966, p67 (5b-1) first term
    x * 1/integrate(alphabeta,a=a,b=b,lower=0,upper=1,rel.tol=1e-5)$value * alphabeta(x,a,b)
}
abOptimalPolicy <- function(n,a,b) {
    if (n == 1) ## n counting from the last (must-be-chosen) item
        return (a/(a+b)) ## mean according to the beta distribution formula
    else { ## according to Gilbert & Mosteller (1966) 5b-1 (p. 67)
        RecursiveVal <- abOptimalPolicy((n-1),a,b)
        return (integrate(abXfx,a=a,b=b,lower=RecursiveVal,upper=1,rel.tol=1e-5)$value + RecursiveVal*abfunCDF(RecursiveVal,a,b))
    }
}

OptimalPolicyBeta <- function(a,b,n,N) {
    n <- N - n ### n-th item in the sequence with N total items
    if (n == 0)
        return (-Inf)
    else
        return (abOptimalPolicy(n,a,b))
}

## Gilbert & Mosteller, 1966, p67, 5b-2 (modified for beta distributions, i.e. from 0 to 1)
# abOptimalCondExpVal <- function(n,a,b) {
#     return (integrate(abXfx,a=a,b=b,lower=abOptimalPolicy(n,a,b),upper=1,rel.tol=1e-5)$value / (1 - abfunCDF(abOptimalPolicy(n,a,b),a,b)))
# }
abOptimalCondExpVal <- function(n,a,b) {
    return (qbeta((pbeta(abOptimalPolicy(n,a,b),a,b) + (1- pbeta(abOptimalPolicy(n,a,b),a,b))/2),a,b))
}
## To get conditional expected values, i.e. value given reaching a position from start:
#rev(c(0,BetaNormalize(sapply(1:(N-1),abOptimalCondExpVal, a=1,b=1),1,1)))
#rev(c(0,BetaNormalize(sapply(1:(N-1),abOptimalCondExpVal, a=1,b=3.7),1,3.7)))

abStopProbBeta <- function(a,b,n,N) {
    if (n == N)
        return (1)
    else {
        ProbChosenAtn <- 1-pbeta(OptimalPolicyBeta(a,b,n,N),a,b) ## P(selected) at position n
        ProbNotYetChosen <- 1
        if ( n > 1)
            for (i in 1:(n-1))
                ProbNotYetChosen <- ProbNotYetChosen * pbeta(OptimalPolicyBeta(a,b,i,N),a,b)
        return (ProbNotYetChosen * ProbChosenAtn)
    }
}

cumStopProbBeta <- function(a,b,n,N) {
    if (n == N)
        return (1)
    else {
        ProbCumulative <- 0
        for (i in 1:n) ## Add up all the Prob. select at and before position n
            ProbCumulative <- ProbCumulative + abStopProbBeta(a,b,i,N)
        return (ProbCumulative)
    }
}

BetaNormalize <- function(n,a,b) {
    ## minus mean and then divided by the standard deviation
    ## http://stats.stackexchange.com/questions/12232/calculating-the-parameters-of-a-beta-distribution-using-the-mean-and-variance
    return ( ( n - (a/(a+b)) ) / sqrt( a*b / ((a+b)^2 * (a+b+1)) ) )
}


### Gilbert & Mosteller, 1966, p53-54: 3b-2, 3b-3, Table 7
indifferentValuesUniform <- function(n) { ## n = position counting from last item
    i <- n - 1 ## i = position before the last item
    if (i == 0) ## the last item itself indeed
        return (0) ## must be selected
    else
        return ( 1 / (1+(0.80435226286/i)) ) ## value from their approx. in the paper
}

abStopProbSecBeta <- function(a,b,n,N) {
    if (n == N) ## n counts from the beginning of sequence
        return (1) ## or return (1 - qbeta(indifferentValuesUniform(1), 1, 1))
    else { ## greater than the indifferent value
        ## n has to be transformed for indifferentValuesUniform() as it counts from the end
        ## P(selected) at position n
        #ProbChosenAtn <- 1-pbeta(qbeta(indifferentValuesUniform((N-n+1)),a,b),a,b)
        ProbChosenAtn <- 1-indifferentValuesUniform((N-n+1)) # same as above
        ProbNotYetChosen <- 1
        if ( n > 1)
            for (i in ((N-n+1)+1):N)
                ProbNotYetChosen <- ProbNotYetChosen * indifferentValuesUniform(i)
        return (ProbNotYetChosen * ProbChosenAtn)
    }
}

cumStopProbSecBeta <- function(a,b,n,N) {
    if (n == N) ## n counts from the beginning of sequence
        return (1)
    else { ## greater than the indifferent value
        ## n has to be transformed for indifferentValuesUniform() as it counts from the end
        ProbCumulative <- 0
        for (i in 1:n) ## Add up all the Prob. select at and before position n
            ProbCumulative <- ProbCumulative + abStopProbSecBeta(a,b,i,N)
        return (ProbCumulative)
    }
}




### To get the thresholds from 1st to 10th items when N = 10, do
#sapply(1:10, OptimalPolicyBeta, a=1, b=1, N=10)
#sapply(1:10, abOptimalPolicy, a=1, b=3.7)
### To get normalized thresholds from 1st to 10th items when N = 10, do
#BetaNormalize(sapply(1:10, OptimalPolicyBeta, a=1, b=1, N=10),1,1)
#BetaNormalize(sapply(1:10,abOptimalPolicy, a=1,b=1),1,1)
### To get EXACT output required for simulations
#rev(c(-Inf,BetaNormalize(sapply(1:(N-1),abOptimalPolicy, a=1,b=3.7),1,3.7)))
#rev(c(-Inf,BetaNormalize(sapply(1:(N-1),abOptimalPolicy, a=1,b=1),1,1)))
### Compare with Jonathan's numbers from imported CSV files
#rev(fkb14[1:N,1])
#rev(fkb11[1:N,1])
### May need to increase (to the maximum) the recursion limit in R by doing:
#options(expressions= 100000)



#########################################################
### Simulation functions, use some of the above functions
#########################################################

### This function plots the average quality (expected quality Y) of item ###
### against the actual stopping (and choosing) position (effort X)       ###
ExpectedQualityVsEffort <- function(x,a,b,normal,popfactor,samsize,repfactor,writetofile) {

    if (writetofile==1) {
        cat(x,file="ExpectedQualityVsEffortOut.txt",sep=",");
        cat(",",file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
        cat(a,file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
        cat(",",file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
        cat(b,file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
        cat(",",file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
        cat(normal,file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
        cat(",",file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
        cat(popfactor,file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
        cat(",",file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
        cat(samsize,file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
        cat(",",file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
        cat(repfactor,file="ExpectedQualityVsEffortOut.txt",sep="\n",append=TRUE);
    }

    ## get the whole matrix of samples from SampleGen
    samPool <- SampleGen4((x*popfactor),a,b,normal);

    ## create arrays to store values for Expected/Average quality of item
    Quality <- rep(NA, (samsize - 1));

    ## create arrays to count trials that ended search with a certain effort
    EffortCounter <- rep(0, (samsize - 1));
    QualityGivenEffort <- rep(0, (samsize - 1));

    samPoolCount <- 0;
    ## set stopping N from N = 1 to N = (sample_size - 1)
    for(N in 1:(samsize - 1)) {
    ### We are using only 1 form of Heuristic, i.e. set criterion after N
    ### and the pick the first item exceeding the criterion

        QualitySum <- 0;

        ## Repeat the same procedure by # repfactor
        for(j in 1:repfactor) {

            samPoolCount <- samPoolCount + 1;
            ## Retrieve a sample from the population
            sam <- sample(samPool,samsize);
            ## ~~ PRINT
            if (writetofile==1) {
                cat(N,file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
                cat(",",file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
                cat(sam,file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
                cat(",",file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
            }
            rank <- numeric(0);
            for (i in 1:length(sam))
                rank = c(rank,match(sam[i],sort(sam)));
            if (writetofile==1) {
                cat(rank,file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
                cat(",",file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
            }

            ## set Criterion out of the first N
            C <- max(sam[1:N]);#C = max(head(sam,N));
            ## ~~ PRINT
            if (writetofile==1) {
                cat(C,file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
                cat(",",file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
            }
            ## isolate remains of the sample
            sam2 <- tail(sam,(length(sam)-N));

            for(i in 1:length(sam2)) {
                ##Candidate is going to store the item at the point of stopping
                Candidate <- as.numeric(sam2[i]);
                diff <- C - Candidate;
                #if(diff < 0) {break;}
                if(diff < 0) {#print(paste('here: ',N,',',i))
                    EffortCounter[(N+i)] <- 1 + EffortCounter[(N+i)]
                    QualityGivenEffort[(N+i)] <- Candidate + QualityGivenEffort[(N+i)]
                    break;
                }
            }

            if (writetofile==1) {
                cat(Candidate,file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
                cat(",",file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
                cat(match(Candidate,sort(sam)),file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
                cat(",",file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
                cat(match(Candidate,sam),file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
                cat("\n",file="ExpectedQualityVsEffortOut.txt",sep=",",append=TRUE);
            }
            ## Adding to the total quality values in repfactor runs
            QualitySum <- QualitySum + Candidate;
        }

        ## Putting the total quality values after repfactor runs in the array
        Quality[N] <- QualitySum;
    }

    ## Averaging the total quality values
    Quality <- Quality/repfactor;
    QualityGivenEffort <- QualityGivenEffort/EffortCounter

    #QualityGivenEffort[is.na(QualityGivenEffort)] <- 0
    #EffortCounter[is.na(EffortCounter)] <- 0

    pdf(file=paste("./ExpectedQualityVsEffortA",a,"B",b,"S",samsize,"R",repfactor,".pdf",sep=''));
    plot(0,type="n",xlab=paste("Effort (point of choice) when N=",samsize,sep=''),ylab=paste("Mean quality of item"),xlim=c(0,samsize),ylim=c(min(QualityGivenEffort,na.rm = TRUE),max(QualityGivenEffort,na.rm = TRUE)));
    lines(1:samsize,QualityGivenEffort,type="l",col="blue");
    max_x <- which((QualityGivenEffort)==max(QualityGivenEffort,na.rm=TRUE));
    max_y <- max(QualityGivenEffort,na.rm=TRUE);
    points(x=max_x,y=max_y,cex=3,pch='*',col='red');
    text(x=max_x,y=max_y,labels=paste("x=",max_x,", y=",round(max_y,3)),cex=0.7,pos=4);
    title(main=paste("Beta distribution","\n", "alpha=", a, "beta=", b));
    dev.off();

    return(list(Quality,QualityGivenEffort,EffortCounter,min(samPool),max(samPool),a,b));
}

#############################################################
### Simulation function that takes in external thresholds ###
### for full-knowledge models threshold pre-calculated    ###
#############################################################

### This one added Top50% and more
DistribPSuccessFull2 <- function(thr,x,a,b,normal,popfactor,repfactor,rvalue,writetofile) {
    samsize <- length(thr);
    N <- samsize;

    if (writetofile==1) {
        cat(x,file="DistribPSuccessOut.txt",sep=",");
        cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(a,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(b,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(normal,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(popfactor,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(samsize,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(repfactor,file="DistribPSuccessOut.txt",sep="\n",append=TRUE);
    }

    ## get the whole matrix of samples from SampleGen
    #samMatrix <- SampleGen(x,a,b,normal,popfactor,samsize,repfactor);
    #print(dim(samMatrix));#print(samMatrix);
    ## create arrays to store values for Probability of success
    PSuccessMax = rep(NA, (samsize - 1));
    PSuccessTop5p = rep(NA, (samsize - 1));
    PSuccessTop10p = rep(NA, (samsize - 1));
    PSuccessTop25p = rep(NA, (samsize - 1));
    PSuccessTop50p = rep(NA, (samsize - 1));

    #samMatrixCount <- 0;
### We are using only 1 form of Heuristic, i.e. set criterion after N
### and the pick the first item exceeding the criterion

    ## Total number of success (out of # repfactor)
    #TotalFlagMax = 0;
    #TotalFlagTop5p = 0;
    #TotalFlagTop10p = 0;
    #TotalFlagTop25p = 0;

    ## Repeat the same procedure by # repfactor
    for(j in 1:repfactor) {
        #print(paste('Rep: ',j));
        #samMatrixCount <- samMatrixCount + 1;
        ## Retrieve a sample from the population
        #sam <- samMatrix[,samMatrixCount];
        #sam <- sample(SampleGen3(x,a,b,normal,popfactor),samsize);
        sam <- SampleGen4(samsize,a,b,normal);
        #print(paste('N total length:',length(sam)));
        ## ~~ PRINT
        if (writetofile==1) {
            cat(N,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
            cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
            cat(sam,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
            cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        }
        rank = numeric(0);
        for (i in 1:length(sam))
            rank = c(rank,match(sam[i],sort(sam)));
        if (writetofile==1) {
            cat(rank,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
            cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        }

        ## ~~ PRINT
        if (writetofile==1) {
            cat(C,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
            cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        }

        for(i in 1:length(sam)) {
            #print(paste('N: ',i));
            ##Candidate is going to store the item at the point of stopping
            if((thr[i] - as.numeric(sam[i])) < 0) {
                N <- i;
                Candidate <- as.numeric(sam[i]);
                #print(paste('item CHOSEN at ',i));
                break;
            }
            #else
                #print(paste('threshold failed at ',i));
        }
        ## ~~ PRINT
        if (writetofile==1) {
            cat(Candidate,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
            cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
            cat(match(Candidate,sort(sam)),file="DistribPSuccessOut.txt",sep=",",append=TRUE);
            cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
            cat(match(Candidate,sam),file="DistribPSuccessOut.txt",sep=",",append=TRUE);
            cat("\n",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        }
        ## find the number of items should be in category according to sample size
        NumTop5p <- round(length(sam)*.05);
        NumTop10p <- round(length(sam)*.1);
        NumTop25p <- round(length(sam)*.25);
        NumTop50p <- round(length(sam)*.50);

        ## find the values above which an item will be in a certain category
        TheMax <- max(sam);
        MarkTop5p <- tail(sort(sam, decreasing=TRUE)[1:NumTop5p],1);
        MarkTop10p <- tail(sort(sam, decreasing=TRUE)[1:NumTop10p],1);
        MarkTop25p <- tail(sort(sam, decreasing=TRUE)[1:NumTop25p],1);
        MarkTop50p <- tail(sort(sam, decreasing=TRUE)[1:NumTop50p],1);

        if(Candidate == TheMax) {
            FlagMax = 1;
        } else {
            FlagMax = 0;
        }

        if(Candidate >= MarkTop5p) {
            FlagTop5p = 1;
        } else {
            FlagTop5p = 0;
        }

        if(Candidate >= MarkTop10p) {
            FlagTop10p = 1;
        } else {
            FlagTop10p = 0;
        }

        if(Candidate >= MarkTop25p) {
            FlagTop25p = 1;
        } else {
            FlagTop25p = 0;
        }

        if(Candidate >= MarkTop50p) {
            FlagTop50p = 1;
        } else {
            FlagTop50p = 0;
        }

        if(is.na(PSuccessMax[N])) PSuccessMax[N] <- 0;
        if(is.na(PSuccessTop5p[N])) PSuccessTop5p[N] <- 0;
        if(is.na(PSuccessTop10p[N])) PSuccessTop10p[N] <- 0;
        if(is.na(PSuccessTop25p[N])) PSuccessTop25p[N] <- 0;
        if(is.na(PSuccessTop50p[N])) PSuccessTop50p[N] <- 0;

        ## Adding to the total count of success in repfactor runs
        PSuccessMax[N] <- PSuccessMax[N] + FlagMax;
        PSuccessTop5p[N] <- PSuccessTop5p[N] + FlagTop5p;
        PSuccessTop10p[N] <- PSuccessTop10p[N] + FlagTop10p;
        PSuccessTop25p[N] <- PSuccessTop25p[N] + FlagTop25p;
        PSuccessTop50p[N] <- PSuccessTop50p[N] + FlagTop50p;

        ## Putting the total count of success after repfactor runs in the array
        #PSuccessMax[N] = TotalFlagMax;
        #PSuccessTop5p[N] = TotalFlagTop5p;
        #PSuccessTop10p[N] = TotalFlagTop10p;
        #PSuccessTop25p[N] = TotalFlagTop25p;
    }

    ## Converting to probability of success in percentage
    PSuccessMax = (PSuccessMax/repfactor)*100;
    PSuccessTop5p = (PSuccessTop5p/repfactor)*100;
    PSuccessTop10p = (PSuccessTop10p/repfactor)*100;
    PSuccessTop25p = (PSuccessTop25p/repfactor)*100;
    PSuccessTop50p = (PSuccessTop50p/repfactor)*100;

    pdf(file=paste("./DistribPSuccessFullA",a,"B",b,"S",samsize,"R",repfactor,".pdf"));

    plot(0,type="n",xlab="Point of stopping in a full-knowledge model",ylab=paste("Probability of success (%) out of",repfactor," repetitions"),col="gold4",xlim=c(1,samsize),ylim=c(0,100));
    lines(PSuccessTop50p,col='darkgreen',lty=2,lwd=1);
    lines(PSuccessTop25p,col='gold4',lty=2,lwd=1);
    #lo <- loess(PSuccessTop25p~(seq(1,(samsize-1),1)));
    #lines(predict(lo),col='gold4',lty=2,lwd=1);
    #points(which.max(predict(lo)),max(predict(lo)),cex=2,pch='*',col='pink');
    #sspline <- smooth.spline((seq(1,(samsize-1),1)),PSuccessTop25p,spar=.36);
    #lines(sspline,col='gold4',lwd=1);
    #points(which.max(sspline),max(sspline),cex=2,pch='+',col='red');
    #points(PSuccessTop10p,col="forestgreen");
    lines(PSuccessTop10p,col='red',lty=2,lwd=1);
    #lo <- loess(PSuccessTop10p~(seq(1,(samsize-1),1)));
    #lines(predict(lo),col='forestgreen',lty=2,lwd=1);
    #points(which.max(predict(lo)),max(predict(lo)),cex=2,pch='*',col='pink');
    #sspline <- smooth.spline((seq(1,(samsize-1),1)),PSuccessTop10p,spar=.36);
    #lines(sspline,col='forestgreen',lwd=1);
    #points(PSuccessTop5p,col="darkslategray");
    lines(PSuccessTop5p,col='blue',lty=2,lwd=1);
    #lo <- loess(PSuccessTop5p~(seq(1,(samsize-1),1)));
    #lines(predict(lo),col='darkslategray',lty=2,lwd=1);
    #points(which.max(predict(lo)),max(predict(lo)),cex=2,pch='*',col='pink');
    #sspline <- smooth.spline((seq(1,(samsize-1),1)),PSuccessTop5p,spar=.36);
    #lines(sspline,col='darkslategray',lwd=1);
    #points(PSuccessMax,col="black",lwd=2);
    lines(PSuccessMax,col='black',lty=2,lwd=2);
    #lo <- loess(PSuccessMax~(seq(1,(samsize-1),1)));
    #lines(predict(lo),col='black',lty=2,lwd=2);
    #points(which.max(predict(lo)),max(predict(lo)),cex=2,pch='*',col='pink');
    #sspline <- smooth.spline((seq(1,(samsize-1),1)),PSuccessMax,spar=.36);
    #lines(sspline,col='black',lwd=2);
    title(main=paste("Beta distribution","\n", "alpha=", a, "beta=", b), sub="[Color key: Top25%=gold,Top10%=green,Top5%=gray,Max=black]");

    dev.off();

    ## Select what to return by 'rvalue'
    z = switch(rvalue,
    max = PSuccessMax,
    top5 = PSuccessTop5p,
    top10 = PSuccessTop10p,
    top25 = PSuccessTop25p,
    top50 = PSuccessTop50p,
    ## By default z = all of them combined
    data.frame(PSuccessTop50p,PSuccessTop25p,PSuccessTop10p,PSuccessTop5p,PSuccessMax)
    );

    #return(list(z,min(samMatrix),max(samMatrix),a,b));
    return(z);
}

DistribPSuccessFull <- function(thr,x,a,b,normal,popfactor,repfactor,rvalue,writetofile) {
    samsize <- length(thr);
    N <- samsize;

    if (writetofile==1) {
        cat(x,file="DistribPSuccessOut.txt",sep=",");
        cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(a,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(b,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(normal,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(popfactor,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(samsize,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(repfactor,file="DistribPSuccessOut.txt",sep="\n",append=TRUE);
    }

    ## get the whole matrix of samples from SampleGen
    #samMatrix <- SampleGen(x,a,b,normal,popfactor,samsize,repfactor);
    #print(dim(samMatrix));#print(samMatrix);
    ## create arrays to store values for Probability of success
    PSuccessMax = rep(NA, (samsize - 1));
    PSuccessTop5p = rep(NA, (samsize - 1));
    PSuccessTop10p = rep(NA, (samsize - 1));
    PSuccessTop25p = rep(NA, (samsize - 1));

    #samMatrixCount <- 0;
### We are using only 1 form of Heuristic, i.e. set criterion after N
### and the pick the first item exceeding the criterion

    ## Total number of success (out of # repfactor)
    #TotalFlagMax = 0;
    #TotalFlagTop5p = 0;
    #TotalFlagTop10p = 0;
    #TotalFlagTop25p = 0;

    ## Repeat the same procedure by # repfactor
    for(j in 1:repfactor) {
        #print(paste('Rep: ',j));
        #samMatrixCount <- samMatrixCount + 1;
        ## Retrieve a sample from the population
        #sam <- samMatrix[,samMatrixCount];
        #sam <- sample(SampleGen3(x,a,b,normal,popfactor),samsize);
        sam <- SampleGen4(samsize,a,b,normal);
        #print(paste('N total length:',length(sam)));
        ## ~~ PRINT
        if (writetofile==1) {
            cat(N,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
            cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
            cat(sam,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
            cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        }
        rank = numeric(0);
        for (i in 1:length(sam))
            rank = c(rank,match(sam[i],sort(sam)));
        if (writetofile==1) {
            cat(rank,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
            cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        }

        ## ~~ PRINT
        if (writetofile==1) {
            cat(C,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
            cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        }

        for(i in 1:length(sam)) {
            #print(paste('N: ',i));
            ##Candidate is going to store the item at the point of stopping
            if((thr[i] - as.numeric(sam[i])) < 0) {
                N <- i;
                Candidate <- as.numeric(sam[i]);
                #print(paste('item CHOSEN at ',i));
                break;
            }
            #else
                #print(paste('threshold failed at ',i));
        }
        ## ~~ PRINT
        if (writetofile==1) {
            cat(Candidate,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
            cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
            cat(match(Candidate,sort(sam)),file="DistribPSuccessOut.txt",sep=",",append=TRUE);
            cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
            cat(match(Candidate,sam),file="DistribPSuccessOut.txt",sep=",",append=TRUE);
            cat("\n",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        }
        ## find the number of items should be in category according to sample size
        NumTop5p <- round(length(sam)*.05);
        NumTop10p <- round(length(sam)*.1);
        NumTop25p <- round(length(sam)*.25);

        ## find the values above which an item will be in a certain category
        TheMax <- max(sam);
        MarkTop5p <- tail(sort(sam, decreasing=TRUE)[1:NumTop5p],1);
        MarkTop10p <- tail(sort(sam, decreasing=TRUE)[1:NumTop10p],1);
        MarkTop25p <- tail(sort(sam, decreasing=TRUE)[1:NumTop25p],1);

        if(Candidate == TheMax) {
            FlagMax = 1;
        } else {
            FlagMax = 0;
        }

        if(Candidate >= MarkTop5p) {
            FlagTop5p = 1;
        } else {
            FlagTop5p = 0;
        }

        if(Candidate >= MarkTop10p) {
            FlagTop10p = 1;
        } else {
            FlagTop10p = 0;
        }

        if(Candidate >= MarkTop25p) {
            FlagTop25p = 1;
        } else {
            FlagTop25p = 0;
        }

        if(is.na(PSuccessMax[N])) PSuccessMax[N] <- 0;
        if(is.na(PSuccessTop5p[N])) PSuccessTop5p[N] <- 0;
        if(is.na(PSuccessTop10p[N])) PSuccessTop10p[N] <- 0;
        if(is.na(PSuccessTop25p[N])) PSuccessTop25p[N] <- 0;

        ## Adding to the total count of success in repfactor runs
        PSuccessMax[N] <- PSuccessMax[N] + FlagMax;
        PSuccessTop5p[N] <- PSuccessTop5p[N] + FlagTop5p;
        PSuccessTop10p[N] <- PSuccessTop10p[N] + FlagTop10p;
        PSuccessTop25p[N] <- PSuccessTop25p[N] + FlagTop25p;

        ## Putting the total count of success after repfactor runs in the array
        #PSuccessMax[N] = TotalFlagMax;
        #PSuccessTop5p[N] = TotalFlagTop5p;
        #PSuccessTop10p[N] = TotalFlagTop10p;
        #PSuccessTop25p[N] = TotalFlagTop25p;
    }

    ## Converting to probability of success in percentage
    PSuccessMax = (PSuccessMax/repfactor)*100;
    PSuccessTop5p = (PSuccessTop5p/repfactor)*100;
    PSuccessTop10p = (PSuccessTop10p/repfactor)*100;
    PSuccessTop25p = (PSuccessTop25p/repfactor)*100;

    pdf(file=paste("./DistribPSuccessFullA",a,"B",b,"S",samsize,"R",repfactor,".pdf"));

    plot(0,type="n",xlab="Point of stopping in a full-knowledge model",ylab=paste("Probability of success (%) out of",repfactor," repetitions"),col="gold4",xlim=c(1,samsize),ylim=c(0,100));
    lines(PSuccessTop25p,col='gold4',lty=2,lwd=1);
    #lo <- loess(PSuccessTop25p~(seq(1,(samsize-1),1)));
    #lines(predict(lo),col='gold4',lty=2,lwd=1);
    #points(which.max(predict(lo)),max(predict(lo)),cex=2,pch='*',col='pink');
    #sspline <- smooth.spline((seq(1,(samsize-1),1)),PSuccessTop25p,spar=.36);
    #lines(sspline,col='gold4',lwd=1);
    #points(which.max(sspline),max(sspline),cex=2,pch='+',col='red');
    #points(PSuccessTop10p,col="forestgreen");
    lines(PSuccessTop10p,col='red',lty=2,lwd=1);
    #lo <- loess(PSuccessTop10p~(seq(1,(samsize-1),1)));
    #lines(predict(lo),col='forestgreen',lty=2,lwd=1);
    #points(which.max(predict(lo)),max(predict(lo)),cex=2,pch='*',col='pink');
    #sspline <- smooth.spline((seq(1,(samsize-1),1)),PSuccessTop10p,spar=.36);
    #lines(sspline,col='forestgreen',lwd=1);
    #points(PSuccessTop5p,col="darkslategray");
    lines(PSuccessTop5p,col='blue',lty=2,lwd=1);
    #lo <- loess(PSuccessTop5p~(seq(1,(samsize-1),1)));
    #lines(predict(lo),col='darkslategray',lty=2,lwd=1);
    #points(which.max(predict(lo)),max(predict(lo)),cex=2,pch='*',col='pink');
    #sspline <- smooth.spline((seq(1,(samsize-1),1)),PSuccessTop5p,spar=.36);
    #lines(sspline,col='darkslategray',lwd=1);
    #points(PSuccessMax,col="black",lwd=2);
    lines(PSuccessMax,col='black',lty=2,lwd=2);
    #lo <- loess(PSuccessMax~(seq(1,(samsize-1),1)));
    #lines(predict(lo),col='black',lty=2,lwd=2);
    #points(which.max(predict(lo)),max(predict(lo)),cex=2,pch='*',col='pink');
    #sspline <- smooth.spline((seq(1,(samsize-1),1)),PSuccessMax,spar=.36);
    #lines(sspline,col='black',lwd=2);
    title(main=paste("Beta distribution","\n", "alpha=", a, "beta=", b), sub="[Color key: Top25%=gold,Top10%=green,Top5%=gray,Max=black]");

    dev.off();

    ## Select what to return by 'rvalue'
    z = switch(rvalue,
    max = PSuccessMax,
    top5 = PSuccessTop5p,
    top10 = PSuccessTop10p,
    top25 = PSuccessTop25p,
    ## By default z = all of them combined
    data.frame(PSuccessTop25p,PSuccessTop10p,PSuccessTop5p,PSuccessMax)
    );

    #return(list(z,min(samMatrix),max(samMatrix),a,b));
    return(z);
}

DistribExpectedQualityFull <- function(x,a,b,normal,popfactor,repfactor,writetofile) {
    samsize <- length(thr);

    if (writetofile==1) {
        cat(x,file="DistribExpectedQualityOut.txt",sep=",");
        cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(a,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(b,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(normal,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(popfactor,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(samsize,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(repfactor,file="DistribExpectedQualityOut.txt",sep="\n",append=TRUE);
    }

    ## get the whole matrix of samples from SampleGen
    #samMatrix <- SampleGen(x,a,b,normal,popfactor,samsize,repfactor);
    samPool <- SampleGen4((x*samsize*popfactor),a,b,normal);

    ## create arrays to store values for Expected/Average quality of item
    QualitySum = rep(0, (samsize - 1));
    Quality = rep(NA, (samsize - 1));
    Success = rep(0, (samsize - 1));

    #samMatrixCount = 0;
    #samPoolCount = 0;
    ## set stopping N from N=1 to N=(sample_size - 1)

    ## Repeat the same procedure by # repfactor
    C <- sapply(1:samsize, OptimalPolicyUniform, N=samsize);

    for(j in 1:repfactor) {

        #samMatrixCount <- samMatrixCount + 1;
        #samPoolCount <- samPoolCount + 1;
        ## Retrieve a sample from the population
        sam <- sample(samPool,samsize);

        ## ~~ PRINT
        if (writetofile==1) {
            cat(N,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            cat(sam,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        }

        if (writetofile==1) {
            rank = numeric(0);
            for (i in 1:length(sam))
                rank = c(rank,match(sam[i],sort(sam)));
            cat(rank,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        }

        for(N in 1:(length(sam)-1)) {
            ## ~~ PRINT

            #sam2 <- tail(sam,(length(sam)-N));
            #print("sam[N+1]");print(sam[N+1]);

            #Candidate <- as.numeric(sam2[1]);
            #print("sam2[1]");print(sam2[1]);

            #diff <- C[N] - Candidate;
            #diff <- C[N] - as.numeric(sam[N+1]);
            if( (C[N] - as.numeric(sam[N+1])) <  0) {
                Success[N] <- Success[N] + 1;
                #QualitySum[N] <- QualitySum[N] + Candidate;
                QualitySum[N] <- QualitySum[N] + as.numeric(sam[N+1]);
                if (writetofile==1) {
                    cat(C[N],file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                    cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                }
                break;
            }
        }

        if (writetofile==1) {
            cat(Candidate,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            cat(match(Candidate,sort(sam)),file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            cat(match(Candidate,sam),file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            cat("\n",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        }
    }

    #print(QualitySum);
    #print(Success);
    ## Converting to probability of success in percentage

    #Quality <- QualitySum/Success;
    #Success = Success/repfactor;

    #pdf(file=paste("./DistribExpectedQualityA",alpha,"B",beta,"S",samsize,"-",format(Sys.time(),"%Y%m%d%H%M"),"Fig.pdf"));
    pdf(file=paste("./DistribExpectedQualityA",a,"B",b,"S",samsize,"R",repfactor,"Fig.pdf",sep=''));

#    plot(Quality,type="l",xlab="Point to stop and set criterion (1 - sample size)",ylab=paste("Expected quality of item (range: ",round(min(samMatrix),2)," - ",round(max(samMatrix),2),")"),col="blue",ylim=c(min(samMatrix),max(samMatrix)));
    plot(0,type="n",xlab="Point to stop and set criterion (1 - sample size)",ylab=paste("Expected quality of item (range: ",round(min(samPool),2)," - ",round(max(samPool),2),")"),xlim=c(0,samsize),ylim=c(min(samPool),max(samPool)));
    #lines(Quality,type="l",col="blue");
    max_x = which(Success==max(Success));
    max_y = max(Success);
    #points(x=max_x,y=max_y,cex=3,pch='*',col='red');
    text(x=max_x,y=max_y,labels=paste("x=",max_x,", y=",round(max_y,3)),cex=0.7,pos=4);
    title(main=paste("Beta distribution","\n", "alpha=", a, "beta=", b));

    dev.off();

    return(list(QualitySum,Success,min(samPool),max(samPool),a,b));
}

### This function plots the probability of success (for finding an item at or above certain value range) against the number of stopping
### The criteria changes from i=1 to i=N, with total item number = N, as calculated by OptimalPolicyUniform
## *This function relies on the 'NormalizeBeta' function above
DistribExpectedQuality3 <- function(x,a,b,normal,popfactor,samsize,repfactor,writetofile) {

    if (writetofile==1) {
        cat(x,file="DistribExpectedQualityOut.txt",sep=",");
        cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(a,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(b,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(normal,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(popfactor,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(samsize,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(repfactor,file="DistribExpectedQualityOut.txt",sep="\n",append=TRUE);
    }

    ## get the whole matrix of samples from SampleGen
    #samMatrix <- SampleGen(x,a,b,normal,popfactor,samsize,repfactor);
    samPool <- SampleGen4((x*samsize*popfactor),a,b,normal);

    ## create arrays to store values for Expected/Average quality of item
    QualitySum = rep(0, (samsize - 1));
    Quality = rep(NA, (samsize - 1));
    Success = rep(0, (samsize - 1));

    #samMatrixCount = 0;
    #samPoolCount = 0;
    ## set stopping N from N=1 to N=(sample_size - 1)

    ## Repeat the same procedure by # repfactor
    C <- sapply(1:samsize, OptimalPolicyUniform, N=samsize);

    for(j in 1:repfactor) {

        #samMatrixCount <- samMatrixCount + 1;
        #samPoolCount <- samPoolCount + 1;
        ## Retrieve a sample from the population
        sam <- sample(samPool,samsize);

        ## ~~ PRINT
        if (writetofile==1) {
            cat(N,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            cat(sam,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        }

        if (writetofile==1) {
            rank = numeric(0);
            for (i in 1:length(sam))
                rank = c(rank,match(sam[i],sort(sam)));
            cat(rank,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        }

        for(N in 1:(length(sam)-1)) {
            ## ~~ PRINT

            #sam2 <- tail(sam,(length(sam)-N));
            #print("sam[N+1]");print(sam[N+1]);

            #Candidate <- as.numeric(sam2[1]);
            #print("sam2[1]");print(sam2[1]);

            #diff <- C[N] - Candidate;
            #diff <- C[N] - as.numeric(sam[N+1]);
            if( (C[N] - as.numeric(sam[N+1])) <  0) {
                Success[N] <- Success[N] + 1;
                #QualitySum[N] <- QualitySum[N] + Candidate;
                QualitySum[N] <- QualitySum[N] + as.numeric(sam[N+1]);
                if (writetofile==1) {
                    cat(C[N],file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                    cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                }
                break;
            }
        }

        if (writetofile==1) {
            cat(Candidate,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            cat(match(Candidate,sort(sam)),file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            cat(match(Candidate,sam),file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            cat("\n",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        }
    }

    #print(QualitySum);
    #print(Success);
    ## Converting to probability of success in percentage

    #Quality <- QualitySum/Success;
    #Success = Success/repfactor;

    #pdf(file=paste("./DistribExpectedQualityA",alpha,"B",beta,"S",samsize,"-",format(Sys.time(),"%Y%m%d%H%M"),"Fig.pdf"));
    pdf(file=paste("./DistribExpectedQualityA",a,"B",b,"S",samsize,"R",repfactor,"Fig.pdf",sep=''));

#    plot(Quality,type="l",xlab="Point to stop and set criterion (1 - sample size)",ylab=paste("Expected quality of item (range: ",round(min(samMatrix),2)," - ",round(max(samMatrix),2),")"),col="blue",ylim=c(min(samMatrix),max(samMatrix)));
    plot(0,type="n",xlab="Point to stop and set criterion (1 - sample size)",ylab=paste("Expected quality of item (range: ",round(min(samPool),2)," - ",round(max(samPool),2),")"),xlim=c(0,samsize),ylim=c(min(samPool),max(samPool)));
    #lines(Quality,type="l",col="blue");
    max_x = which(Success==max(Success));
    max_y = max(Success);
    #points(x=max_x,y=max_y,cex=3,pch='*',col='red');
    text(x=max_x,y=max_y,labels=paste("x=",max_x,", y=",round(max_y,3)),cex=0.7,pos=4);
    title(main=paste("Beta distribution","\n", "alpha=", a, "beta=", b));

    dev.off();

    return(list(QualitySum,Success,min(samPool),max(samPool),a,b));
}

#########################################################
### Simulation functions, use some of the above functions
#########################################################

### This function plots the probability of success (for finding an item at or above certain value range) against the number to stop sampling and set a criterion in the sample
## *This function relies on the 'NormalizeBeta' function above
DistribExpectedQuality2e <- function(x,alpha,beta,normal,popfactor,samsize,repfactor,writetofile) {

    if (writetofile==1) {
        cat(x,file="DistribExpectedQualityOut.txt",sep=",");
        cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(a,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(b,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(normal,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(popfactor,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(samsize,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(repfactor,file="DistribExpectedQualityOut.txt",sep="\n",append=TRUE);
    }

    ## get the whole matrix of samples from SampleGen
    samPool <- SampleGen4((x*samsize*popfactor),a,b,normal);

    ## create arrays to store values for Expected/Average quality of item
    Quality = rep(NA, (samsize - 1));

    #samPoolCount = 0;
    ## set stopping N from N = 1 to N = (sample_size - 1)
    for(N in 1:(samsize - 1)) {
    ### We are using only 1 form of Heuristic, i.e. set criterion after N
    ### and the pick the first item exceeding the criterion

        QualitySum = 0;

        ## Repeat the same procedure by # repfactor
        for(j in 1:repfactor) {

            #samPoolCount <- samPoolCount + 1;
            ## Retrieve a sample from the population
            sam <- sample(samPool,samsize);
            ## ~~ PRINT
            if (writetofile==1) {
                cat(N,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                cat(sam,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            }
            if (writetofile==1) {
                rank = numeric(0);
                for (i in 1:length(sam))
                    rank = c(rank,match(sam[i],sort(sam)));
                cat(rank,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            }

            ## set Criterion out of the first N
            #C <- max(sam[1:N]);#C = max(head(sam,N));

            ## ~~ PRINT
            if (writetofile==1) {
                #cat(C,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                cat(max(sam[1:N]),file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            }
            ## isolate remains of the sample
            #sam2 = tail(sam,(length(sam)-N));

            for(i in 1:length(tail(sam,(length(sam)-N)))) {
                ##Candidate is going to store the item at the point of stopping
                #Candidate = as.numeric(tail(sam,(length(sam)-N))[i]);
                #diff = C - Candidate;
                if((max(sam[1:N]) - as.numeric(tail(sam,(length(sam)-N))[i])) < 0) {break;}
            }

            if (writetofile==1) {
                Candidate = as.numeric(tail(sam,(length(sam)-N))[i]);
                cat(Candidate,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                cat(match(Candidate,sort(sam)),file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                cat(match(Candidate,sam),file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                cat("\n",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            }
            ## Adding to the total count of success in repfactor runs
            #QualitySum = QualitySum + Candidate;
            QualitySum = QualitySum + as.numeric(tail(sam,(length(sam)-N))[i]);
        }

        ## Putting the total count of success after repfactor runs in the array
        Quality[N] = QualitySum;
    }

    ## Converting to probability of success in percentage
    Quality = Quality/repfactor;

    #pdf(file=paste("./DistribExpectedQualityA",alpha,"B",beta,"S",samsize,"-",format(Sys.time(),"%Y%m%d%H%M"),"Fig.pdf"));
    pdf(file=paste("./DistribExpectedQualityA",a,"B",b,"S",samsize,"R",repfactor,"Fig.pdf",sep=''));


#    plot(Quality,type="l",xlab="Point to stop and set criterion (1 - sample size)",ylab=paste("Expected quality of item (range: ",round(min(samMatrix),2)," - ",round(max(samMatrix),2),")"),col="blue",ylim=c(min(samMatrix),max(samMatrix)));
    plot(0,type="n",xlab="Point to stop and set criterion (1 - sample size)",ylab=paste("Expected quality of item (range: ",round(min(samPool),2)," - ",round(max(samPool),2),")"),xlim=c(0,samsize),ylim=c(min(samPool),max(samPool)));
    lines(Quality,type="l",col="blue");
    max_x = which(Quality==max(Quality));
    max_y = max(Quality);
    points(x=max_x,y=max_y,cex=3,pch='*',col='red');
    text(x=max_x,y=max_y,labels=paste("x=",max_x,", y=",round(max_y,3)),cex=0.7,pos=4);
    title(main=paste("Beta distribution","\n", "alpha=", a, "beta=", b));

    dev.off();

    return(list(Quality,min(samPool),max(samPool),a,b));
}



### This function plots the probability of success (for finding an item at or above certain value range) against the number to stop sampling and set a criterion in the sample
## *This function relies on the 'NormalizeBeta' function above
DistribExpectedQuality2 <- function(x,a,b,normal,popfactor,samsize,repfactor,writetofile) {

    if (writetofile==1) {
        cat(x,file="DistribExpectedQualityOut.txt",sep=",");
        cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(a,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(b,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(normal,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(popfactor,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(samsize,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
        cat(repfactor,file="DistribExpectedQualityOut.txt",sep="\n",append=TRUE);
    }

    ## get the whole matrix of samples from SampleGen
    #samMatrix <- SampleGen(x,a,b,normal,popfactor,samsize,repfactor);
    samPool <- SampleGen4((x*samsize*popfactor),a,b,normal);

    ## create arrays to store values for Expected/Average quality of item
    Quality = rep(NA, (samsize - 1));

    #samMatrixCount = 0;
    ## set stopping N from N = 1 to N = (sample_size - 1)
    for(N in 1:(samsize - 1)) {
    ### We are using only 1 form of Heuristic, i.e. set criterion after N
    ### and the pick the first item exceeding the criterion

        QualitySum = 0;

        ## Repeat the same procedure by # repfactor
        for(j in 1:repfactor) {

            #samMatrixCount = samMatrixCount + 1;
            ## Retrieve a sample from the population
            #sam = samMatrix[,samMatrixCount];
            sam <- sample(samPool, samsize)
            ## ~~ PRINT
            if (writetofile==1) {
                cat(N,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                cat(sam,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            }
            rank = numeric(0);
            for (i in 1:length(sam))
                rank = c(rank,match(sam[i],sort(sam)));
            if (writetofile==1) {
                cat(rank,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            }

            ## set Criterion out of the first N
            C = max(sam[1:N]);#C = max(head(sam,N));
            ## ~~ PRINT
            if (writetofile==1) {
                cat(C,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            }
            ## isolate remains of the sample
            sam2 = tail(sam,(length(sam)-N));

            for(i in 1:length(sam2)) {
                ##Candidate is going to store the item at the point of stopping
                Candidate = as.numeric(sam2[i]);
                diff = C - Candidate;
                if(diff < 0) {break;}
            }

            if (writetofile==1) {
                cat(Candidate,file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                cat(match(Candidate,sort(sam)),file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                cat(",",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                cat(match(Candidate,sam),file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
                cat("\n",file="DistribExpectedQualityOut.txt",sep=",",append=TRUE);
            }
            ## Adding to the total count of success in repfactor runs
            QualitySum = QualitySum + Candidate;
        }

        ## Putting the total count of success after repfactor runs in the array
        Quality[N] = QualitySum;
    }

    ## Converting to probability of success in percentage
    Quality = Quality/repfactor;

    #pdf(file=paste("./DistribExpectedQualityA",alpha,"B",beta,"S",samsize,"-",format(Sys.time(),"%Y%m%d%H%M"),"Fig.pdf"));
    pdf(file=paste("./DistribExpectedQualityA",a,"B",b,"S",samsize,"R",repfactor,"Fig.pdf",sep=''));


#    plot(Quality,type="l",xlab="Point to stop and set criterion (1 - sample size)",ylab=paste("Expected quality of item (range: ",round(min(samMatrix),2)," - ",round(max(samMatrix),2),")"),col="blue",ylim=c(min(samMatrix),max(samMatrix)));
    plot(0,type="n",xlab="Point to stop and set criterion (1 - sample size)",ylab=paste("Expected quality of item (range: ",round(min(samPool),2)," - ",round(max(samPool),2),")"),xlim=c(0,samsize),ylim=c(min(samPool),max(samPool)));
    lines(Quality,type="l",col="blue");
    max_x = which(Quality==max(Quality));
    max_y = max(Quality);
    points(x=max_x,y=max_y,cex=3,pch='*',col='red');
    text(x=max_x,y=max_y,labels=paste("x=",max_x,", y=",round(max_y,3)),cex=0.7,pos=4);
    title(main=paste("Beta distribution","\n", "alpha=", a, "beta=", b));

    dev.off();

    return(list(Quality,min(samPool),max(samPool),a,b));
}



### This function plots the probability of success (for finding an item at or above certain value range) against the number to stop sampling and set a criterion in the sample
## *This function relies on the 'NormalizeBeta' function above
DistribExpectedQuality <- function(x,a,b,normal,popfactor,samsize,repfactor) {

    ## scale up the population by # popfactor
    if(normal==0) {
        y = rbeta(x,a,b);
        for(i in 1:popfactor) {
            y = c(y,rbeta(x,a,b));
        }
    }
    else if(normal==1) {
        y = NormalizeBeta(x,a,b);
        for(i in 1:popfactor) {
            y = c(y,NormalizeBeta(x,a,b));
        }
    }
    else {
        print("Please specify if the distribution should be normalized.");
        return(-1);
    }

    ## create arrays to store values for Expected/Average quality of item
    Quality = rep(NA, (samsize - 1));

    ## set stopping N from N = 1 to N = (sample_size - 1)
    for(N in 1:(samsize - 1)) {
    ### We are using only 1 form of Heuristic, i.e. set criterion after N
    ### and the pick the first item exceeding the criterion

        QualitySum = 0;

        ## Repeat the same procedure by # repfactor
        for(j in 1:repfactor) {

            ## Create the sample from the population
            sam = sample(y,samsize);
            ## set Criterion out of the first N
            C = max(sam[1:N]);#C = max(head(sam,N));
            ## isolate remains of the sample
            sam2 = tail(sam,(length(sam)-N));

            for(i in 1:length(sam2)) {
                ##Candidate is going to store the item at the point of stopping
                Candidate = as.numeric(sam2[i]);
                diff = C - Candidate;
                if(diff < 0) {break;}
            }

            ## Adding to the total count of success in repfactor runs
            QualitySum = QualitySum + Candidate;
        }

        ## Putting the total count of success after repfactor runs in the array
        Quality[N] = QualitySum;
    }

    ## Converting to probability of success in percentage
    Quality = Quality/repfactor;

    #pdf(file=paste("./DistribExpectedQualityA",alpha,"B",beta,"S",samsize,"R",repfactor,"-",format(Sys.time(),"%Y%m%d%H%M"),"Fig.pdf"));
    pdf(file=paste("./DistribExpectedQualityA",a,"B",b,"S",samsize,"R",repfactor,"Fig.pdf"));

    plot(Quality,type="l",xlab="Point to stop and set criterion (1 - sample size)",ylab=paste("Expected quality of item (range: ",round(min(y),2)," - ",round(max(y),2),")"),col="blue",ylim=c(min(y),max(y)));
    max_x = which(Quality==max(Quality));
    max_y = max(Quality);
    points(x=max_x,y=max_y,cex=3,pch='*',col='red');
    text(x=max_x,y=max_y,labels=paste("x=",max_x,", y=",round(max_y,3)),cex=0.7,pos=4);
    title(main=paste("Beta distribution","\n", "alpha=", a, "beta=", b));

    dev.off();

    return(Quality);
}



RealdataExpectedQuality <- function(y,titlestring,samsize,repfactor) {
    a = alpha;
    b = beta;

    ## create arrays to store values for Expected/Average quality of item
    Quality = rep(NA, (samsize - 1));

    ## set stopping N from N = 1 to N = (sample_size - 1)
    for(N in 1:(samsize - 1)) {
    ### We are using only 1 form of Heuristic, i.e. set criterion after N
    ### and the pick the first item exceeding the criterion

        QualitySum = 0;

        ## Repeat the same procedure by # repfactor
        for(j in 1:repfactor) {

            ## Create the sample from the population
            sam = sample(y,samsize);
            ## set Criterion out of the first N
            C = max(sam[1:N]);#C = max(head(sam,N));
            ## isolate remains of the sample
            sam2 = tail(sam,(length(sam)-N));

            for(i in 1:length(sam2)) {
                ##Candidate is going to store the item at the point of stopping
                Candidate = as.numeric(sam2[i]);
                diff = C - Candidate;
                if(diff < 0) {break;}
            }

            ## Adding to the total count of success in repfactor runs
            QualitySum = QualitySum + Candidate;
        }

        ## Putting the total count of success after repfactor runs in the array
        Quality[N] = QualitySum;
    }

    ## Converting to probability of success in percentage
    Quality = Quality/repfactor;
print(as.list(match.call())$y);
    pdf(file=paste("./RealdataExpectedQuality",as.list(match.call())$y,"Fig.pdf"));

    plot(Quality,type="l",xlab="Point to stop and set criterion (1 - sample size)",ylab=paste("Expected quality of item (range: ",round(min(y),2)," - ",round(max(y),2),")"),col="blue",ylim=c(min(y),max(y)));
    print(max(Quality));print(which(Quality==max(Quality)));
    max_x = which(Quality==max(Quality));
    max_y = max(Quality);
    points(x=max_x,y=max_y,cex=3,pch='*',col='red');
    text(x=max_x,y=max_y,labels=paste("x=",max_x,", y=",round(max_y,3)),cex=0.7,pos=4);
    title(main=paste(titlestring));

    dev.off();

    return(Quality);
}
## add anasterisk to the peak!



### This function plots the probability of success (for finding an item at or above certain value range) against the number to stop sampling and set a criterion in the sample
## *This function relies on the 'NormalizeBeta' function above
DistribPSuccess2 <- function(x,a,b,normal,popfactor,samsize,repfactor,rvalue,writetofile) {

    if (writetofile==1) {
        cat(x,file="DistribPSuccessOut.txt",sep=",");
        cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(a,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(b,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(normal,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(popfactor,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(samsize,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(repfactor,file="DistribPSuccessOut.txt",sep="\n",append=TRUE);
    }

    ## get the whole matrix of samples from SampleGen
    #samMatrix <- SampleGen(x,a,b,normal,popfactor,samsize,repfactor);
    samPool <- SampleGen4((x*samsize*popfactor),a,b,normal)

    ## create arrays to store values for Probability of success
    PSuccessMax = rep(NA, (samsize - 1));
    PSuccessTop5p = rep(NA, (samsize - 1));
    PSuccessTop10p = rep(NA, (samsize - 1));
    PSuccessTop25p = rep(NA, (samsize - 1));

    #samMatrixCount = 0;
    ## set stopping N from N = 1 to N = (sample_size - 1)
    for(N in 1:(samsize - 1)) {
    ### We are using only 1 form of Heuristic, i.e. set criterion after N
    ### and the pick the first item exceeding the criterion
print(paste('N: ',N));
        ## Total number of success (out of # repfactor)
        TotalFlagMax = 0;
        TotalFlagTop5p = 0;
        TotalFlagTop10p = 0;
        TotalFlagTop25p = 0;

        ## Repeat the same procedure by # repfactor
        for(j in 1:repfactor) {

            #samMatrixCount = samMatrixCount + 1;
            ## Retrieve a sample from the population
            #sam = samMatrix[,samMatrixCount];
            sam <- sample(samPool,samsize)
            ## ~~ PRINT
            if (writetofile==1) {
                cat(N,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
                cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
                cat(sam,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
                cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
            }
            rank = numeric(0);
            for (i in 1:length(sam))
                rank = c(rank,match(sam[i],sort(sam)));
            if (writetofile==1) {
                cat(rank,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
                cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
            }
            ## set Criterion out of the first N
            C = max(sam[1:N]);#C = max(head(sam,N));
            ## ~~ PRINT
            if (writetofile==1) {
                cat(C,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
                cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
            }
            ## isolate remains of the sample
            sam2 = tail(sam,(length(sam)-N));

            for(i in 1:length(sam2)) {
                ##Candidate is going to store the item at the point of stopping
                Candidate = as.numeric(sam2[i]);
                diff = C - Candidate;
                if(diff < 0) {break;}
            }
            ## ~~ PRINT
            if (writetofile==1) {
                cat(Candidate,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
                cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
                cat(match(Candidate,sort(sam)),file="DistribPSuccessOut.txt",sep=",",append=TRUE);
                cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
                cat(match(Candidate,sam),file="DistribPSuccessOut.txt",sep=",",append=TRUE);
                cat("\n",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
            }
            ## find the number of items should be in category according to sample size
            NumTop5p = round(length(sam)*.05);
            NumTop10p = round(length(sam)*.1);
            NumTop25p = round(length(sam)*.25);

            ## find the values above which an item will be in a certain category
            TheMax= max(sam);
            MarkTop5p = tail(sort(sam, decreasing=TRUE)[1:NumTop5p],1);
            MarkTop10p = tail(sort(sam, decreasing=TRUE)[1:NumTop10p],1);
            MarkTop25p = tail(sort(sam, decreasing=TRUE)[1:NumTop25p],1);

            if(Candidate == TheMax) {
                FlagMax = 1;
            } else {
                FlagMax = 0;
            }

            if(Candidate >= MarkTop5p) {
                FlagTop5p = 1;
            } else {
                FlagTop5p = 0;
            }

            if(Candidate >= MarkTop10p) {
                FlagTop10p = 1;
            } else {
                FlagTop10p = 0;
            }

            if(Candidate >= MarkTop25p) {
                FlagTop25p = 1;
            } else {
                FlagTop25p = 0;
            }

            ## Adding to the total count of success in repfactor runs
            TotalFlagMax = TotalFlagMax + FlagMax;
            TotalFlagTop5p = TotalFlagTop5p + FlagTop5p;
            TotalFlagTop10p = TotalFlagTop10p + FlagTop10p;
            TotalFlagTop25p = TotalFlagTop25p + FlagTop25p;
        }

        ## Putting the total count of success after repfactor runs in the array
        PSuccessMax[N] = TotalFlagMax;
        PSuccessTop5p[N] = TotalFlagTop5p;
        PSuccessTop10p[N] = TotalFlagTop10p;
        PSuccessTop25p[N] = TotalFlagTop25p;

        ## Converting to probability of success in percentage
        PSuccessMax[N] = (PSuccessMax[N]/repfactor)*100;
        PSuccessTop5p[N] = (PSuccessTop5p[N]/repfactor)*100;
        PSuccessTop10p[N] = (PSuccessTop10p[N]/repfactor)*100;
        PSuccessTop25p[N] = (PSuccessTop25p[N]/repfactor)*100;
    }

    pdf(file=paste("./DistribPSuccessA",a,"B",b,"S",samsize,"R",repfactor,"Fig.pdf",sep=''));

    plot(PSuccessTop25p,type="p",xlab="Point to set criterion during exploration of sample sequence",ylab=paste("Probability of success (%) out of",repfactor," repetitions"),col="gold4",ylim=c(0,100));
    lo <- loess(PSuccessTop25p~(seq(1,(samsize-1),1)));
    lines(predict(lo),col='gold4',lty=2,lwd=1);
    points(which.max(predict(lo)),max(predict(lo)),cex=2,pch='*',col='pink');
    #sspline <- smooth.spline((seq(1,(samsize-1),1)),PSuccessTop25p,spar=.36);
    #lines(sspline,col='gold4',lwd=1);
    #points(which.max(sspline),max(sspline),cex=2,pch='+',col='red');
    points(PSuccessTop10p,col="forestgreen");
    lo <- loess(PSuccessTop10p~(seq(1,(samsize-1),1)));
    lines(predict(lo),col='forestgreen',lty=2,lwd=1);
    points(which.max(predict(lo)),max(predict(lo)),cex=2,pch='*',col='pink');
    #sspline <- smooth.spline((seq(1,(samsize-1),1)),PSuccessTop10p,spar=.36);
    #lines(sspline,col='forestgreen',lwd=1);
    points(PSuccessTop5p,col="darkslategray");
    lo <- loess(PSuccessTop5p~(seq(1,(samsize-1),1)));
    lines(predict(lo),col='darkslategray',lty=2,lwd=1);
    points(which.max(predict(lo)),max(predict(lo)),cex=2,pch='*',col='pink');
    #sspline <- smooth.spline((seq(1,(samsize-1),1)),PSuccessTop5p,spar=.36);
    #lines(sspline,col='darkslategray',lwd=1);
    points(PSuccessMax,col="black",lwd=2);
    lo <- loess(PSuccessMax~(seq(1,(samsize-1),1)));
    lines(predict(lo),col='black',lty=2,lwd=2);
    points(which.max(predict(lo)),max(predict(lo)),cex=2,pch='*',col='pink');
    #sspline <- smooth.spline((seq(1,(samsize-1),1)),PSuccessMax,spar=.36);
    #lines(sspline,col='black',lwd=2);
    title(main=paste("Beta distribution","\n", "alpha=", a, "beta=", b), sub="[Color key: Top25%=gold,Top10%=green,Top5%=gray,Max=black]");

    dev.off();

    ## Select what to return by 'rvalue'
    z = switch(rvalue,
    max = PSuccessMax,
    top5 = PSuccessTop5p,
    top10 = PSuccessTop10p,
    top25 = PSuccessTop25p,
    ## By default z = all of them combined
    data.frame(PSuccessTop25p,PSuccessTop10p,PSuccessTop5p,PSuccessMax)
    );

    return(list(z,min(samPool),max(samPool),a,b));
}



### This function plots the probability of success (for finding an item at or above certain value range) against the number to stop sampling and set a criterion in the sample
## *This function relies on the 'NormalizeBeta' function above
DistribPSuccess <- function(x,a,b,normal,popfactor,samsize,repfactor,writetofile) {

    if (writetofile==1) {
        cat(x,file="DistribPSuccessOut.txt",sep=",");
        cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(a,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(b,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(normal,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(popfactor,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(samsize,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
        cat(repfactor,file="DistribPSuccessOut.txt",sep="\n",append=TRUE);
    }

    ## scale up the population by # popfactor
    if(normal==0) {
        y = rbeta(x,a,b);
        for(i in 1:popfactor) {
            y = c(y,rbeta(x,a,b));
        }
    }
    else if(normal==1) {
        y = rBetaNormalized(x,a,b);
        for(i in 1:popfactor) {
            y = c(y,rBetaNormalized(x,a,b));
        }
    }
    else {
        print("Please specify if the distribution should be normalized.");
        return(-1);
    }

    ## create arrays to store values for Probability of success
    PSuccessMax = rep(NA, (samsize - 1));
    PSuccessTop5p = rep(NA, (samsize - 1));
    PSuccessTop10p = rep(NA, (samsize - 1));
    PSuccessTop25p = rep(NA, (samsize - 1));

    ## set stopping N from N = 1 to N = (sample_size - 1)
    for(N in 1:(samsize - 1)) {
    ### We are using only 1 form of Heuristic, i.e. set criterion after N
    ### and the pick the first item exceeding the criterion

        ## Total number of success (out of # repfactor)
        TotalFlagMax = 0;
        TotalFlagTop5p = 0;
        TotalFlagTop10p = 0;
        TotalFlagTop25p = 0;

        ## Repeat the same procedure by # repfactor
        for(j in 1:repfactor) {

            ## Create the sample from the population
            sam = sample(y,samsize);
            ## ~~ PRINT
            if (writetofile==1) {
             cat(N,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
             cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
             cat(sam,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
             cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
         }
            rank = numeric(0);
            for (i in 1:length(sam))
                rank = c(rank,match(sam[i],sort(sam)));
            if (writetofile==1) {
             cat(rank,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
             cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
         }
            ## set Criterion out of the first N
            C = max(sam[1:N]);#C = max(head(sam,N));
            ## ~~ PRINT
            if (writetofile==1) {
             cat(C,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
             cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
         }
            ## isolate remains of the sample
            sam2 = tail(sam,(length(sam)-N));

            for(i in 1:length(sam2)) {
                ##Candidate is going to store the item at the point of stopping
                Candidate = as.numeric(sam2[i]);
                diff = C - Candidate;
                if(diff < 0) {break;}
            }
            ## ~~ PRINT
            if (writetofile==1) {
             cat(Candidate,file="DistribPSuccessOut.txt",sep=",",append=TRUE);
             cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
             cat(match(Candidate,sort(sam)),file="DistribPSuccessOut.txt",sep=",",append=TRUE);
             cat(",",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
             cat(match(Candidate,sam),file="DistribPSuccessOut.txt",sep=",",append=TRUE);
             cat("\n",file="DistribPSuccessOut.txt",sep=",",append=TRUE);
         }
            ## find the number of items should be in category according to sample size
            NumTop5p = round(length(sam)*.05);
            NumTop10p = round(length(sam)*.1);
            NumTop25p = round(length(sam)*.25);

            ## find the values above which an item will be in a certain category
            TheMax= max(sam);
            MarkTop5p = tail(sort(sam, decreasing=TRUE)[1:NumTop5p],1);
            MarkTop10p = tail(sort(sam, decreasing=TRUE)[1:NumTop10p],1);
            MarkTop25p = tail(sort(sam, decreasing=TRUE)[1:NumTop25p],1);

            if(Candidate == TheMax) {
                FlagMax = 1;
            } else {
                FlagMax = 0;
            }

            if(Candidate >= MarkTop5p) {
                FlagTop5p = 1;
            } else {
                FlagTop5p = 0;
            }

            if(Candidate >= MarkTop10p) {
                FlagTop10p = 1;
            } else {
                FlagTop10p = 0;
            }

            if(Candidate >= MarkTop25p) {
                FlagTop25p = 1;
            } else {
                FlagTop25p = 0;
            }

            ## Adding to the total count of success in repfactor runs
            TotalFlagMax = TotalFlagMax + FlagMax;
            TotalFlagTop5p = TotalFlagTop5p + FlagTop5p;
            TotalFlagTop10p = TotalFlagTop10p + FlagTop10p;
            TotalFlagTop25p = TotalFlagTop25p + FlagTop25p;
        }

        ## Putting the total count of success after repfactor runs in the array
        PSuccessMax[N] = TotalFlagMax;
        PSuccessTop5p[N] = TotalFlagTop5p;
        PSuccessTop10p[N] = TotalFlagTop10p;
        PSuccessTop25p[N] = TotalFlagTop25p;
    }

    ## Converting to probability of success in percentage ***check
    PSuccessMax = (PSuccessMax[N]/repfactor)*100;
    PSuccessTop5p = (PSuccessTop5p[N]/repfactor)*100;
    PSuccessTop10p = (PSuccessTop10p[N]/repfactor)*100;
    PSuccessTop25p = (PSuccessTop25p[N]/repfactor)*100;

    pdf(file=paste("./DistribPSuccessA",a,"B",b,"S",samsize,"R",repfactor,"Fig.pdf"));

    plot(PSuccessTop25p,type="l",xlab="Point to stop and set criterion (1 - sample size)",ylab=paste("Probability of success (%) out of",repfactor),col="blue",ylim=c(0,100));
    lines(PSuccessTop10p,col="green");
    lines(PSuccessTop5p,col="red");
    lines(PSuccessMax,type="l",col="black",lwd=6);
    title(main=paste("Beta distribution","\n", "alpha=", a, "beta=", b), sub="[Max=black, Top5%=red, Top10%=green, Top25%=blue]");

    dev.off();

    z = data.frame(PSuccessTop25p,PSuccessTop10p,PSuccessTop5p,PSuccessMax);

    return(z);
}



### This function plots the effort needed, i.e. number of items went through (for finding an item at or above certain value range) in cases of success against the number to stop sampling and set a criterion in the sample
## *This function relies on the 'NormalizeBeta' function above
DistribEffort2 <- function(x,a,b,normal,popfactor,samsize,repfactor,rvalue,writetofile) {

    if (writetofile==1) {
        cat(x,file="DistribEffortOut.txt",sep=",");
        cat(",",file="DistribEffortOut.txt",sep=",",append=TRUE);
        cat(a,file="DistribEffortOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribEffortOut.txt",sep=",",append=TRUE);
        cat(b,file="DistribEffortOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribEffortOut.txt",sep=",",append=TRUE);
        cat(normal,file="DistribEffortOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribEffortOut.txt",sep=",",append=TRUE);
        cat(popfactor,file="DistribEffortOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribEffortOut.txt",sep=",",append=TRUE);
        cat(samsize,file="DistribEffortOut.txt",sep=",",append=TRUE);
        cat(",",file="DistribEffortOut.txt",sep=",",append=TRUE);
        cat(repfactor,file="DistribEffortOut.txt",sep="\n",append=TRUE);
    }

    ## get the whole matrix of samples from SampleGen
    #samMatrix <- SampleGen(x,a,b,normal,popfactor,samsize,repfactor);
    samPool <- SampleGen4((x*samsize*popfactor),a,b,normal)

    ## creating arrays to store the values of effort for each category
    EffortMax = rep(NA, (samsize - 1));
    EffortTop5p = rep(NA, (samsize - 1));
    EffortTop10p = rep(NA, (samsize - 1));
    EffortTop25p = rep(NA, (samsize - 1));

    #samMatrixCount = 0;
    ## set stopping N from N = 1 to N = (sample_size - 1)
    for(N in 1:(samsize - 1)) {
    ### We are using only 1 form of Heuristic, i.e. set criterion after N
    ### and the pick the first item exceeding the criterion

        ## Total number of success (out of # repfactor)
        TotalEffortMax = 0;
        TotalEffortTop5p = 0;
        TotalEffortTop10p = 0;
        TotalEffortTop25p = 0;
        CountSuccessMax = 0;
        CountSuccessTop5p = 0;
        CountSuccessTop10p = 0;
        CountSuccessTop25p = 0;

        ## Repeat the same procedure by # repfactor
        for(j in 1:repfactor) {

            #samMatrixCount = samMatrixCount + 1;
            ## Retrieve a sample from the population
            #sam = samMatrix[,samMatrixCount];
            sam <- sample(samPool,samsize)
            ## ~~ PRINT
            if (writetofile==1) {
                cat(N,file="DistribEffortOut.txt",sep=",",append=TRUE);
                cat(",",file="DistribEffortOut.txt",sep=",",append=TRUE);
                cat(sam,file="DistribEffortOut.txt",sep=",",append=TRUE);
                cat(",",file="DistribEffortOut.txt",sep=",",append=TRUE);
            }
            rank = numeric(0);
            for (i in 1:length(sam))
                rank = c(rank,match(sam[i],sort(sam)));
            if (writetofile==1) {
                cat(rank,file="DistribEffortOut.txt",sep=",",append=TRUE);
                cat(",",file="DistribEffortOut.txt",sep=",",append=TRUE);
            }
            ## set Criterion out of the first N
            C = max(sam[1:N]);#C = max(head(sam,N));
            ## ~~ PRINT
            if (writetofile==1) {
                cat(C,file="DistribEffortOut.txt",sep=",",append=TRUE);
                cat(",",file="DistribEffortOut.txt",sep=",",append=TRUE);
            }
            ## isolate remains of the sample
            sam2 = tail(sam,(length(sam)-N));
            counter = -1;

            for(i in 1:length(sam2)) {
                ##Candidate is going to store the item at the point of stopping
                Candidate = as.numeric(sam2[i]);
                diff = C - Candidate;
                if(diff < 0) {counter = i; break;}
            }

            ## find the number of items should be in category according to sample size
            NumTop5p = round(length(sam)*.05);
            NumTop10p = round(length(sam)*.1);
            NumTop25p = round(length(sam)*.25);

            ## find the values above which an item will be in a certain category
            TheMax= max(sam);
            MarkTop5p = tail(sort(sam, decreasing=TRUE)[1:NumTop5p],1);
            MarkTop10p = tail(sort(sam, decreasing=TRUE)[1:NumTop10p],1);
            MarkTop25p = tail(sort(sam, decreasing=TRUE)[1:NumTop25p],1);

            if(Candidate == TheMax) {
                TotalEffortMax = TotalEffortMax + N + counter;
                CountSuccessMax = CountSuccessMax + 1;
            }

            if(Candidate >= MarkTop5p) {
                TotalEffortTop5p = TotalEffortTop5p + N + counter;
                CountSuccessTop5p = CountSuccessTop5p + 1;
            }

            if(Candidate >= MarkTop10p) {
                TotalEffortTop10p = TotalEffortTop10p + N + counter;
                CountSuccessTop10p = CountSuccessTop10p + 1;
            }

            if(Candidate >= MarkTop25p) {
                TotalEffortTop25p = TotalEffortTop25p + N + counter;
                CountSuccessTop25p = CountSuccessTop25p + 1;
            }

        }
        EffortMax[N] = TotalEffortMax / CountSuccessMax;
        EffortTop5p[N] = TotalEffortTop5p / CountSuccessTop5p;
        EffortTop10p[N] = TotalEffortTop10p / CountSuccessTop10p;
        EffortTop25p[N] = TotalEffortTop25p / CountSuccessTop25p;

        ## Converting to percentage of item gone through
        EffortMax[N] = EffortMax[N]/samsize*100;
        EffortTop5p[N] = EffortTop5p[N]/samsize*100;
        EffortTop10p[N] = EffortTop10p[N]/samsize*100;
        EffortTop25p[N] = EffortTop25p[N]/samsize*100;
    }

    pdf(file=paste("./DistribEffortA",a,"B",b,"S",samsize,"R",repfactor,"Fig.pdf"));

    plot(EffortTop25p,type="l",xlab="Point to stop and set criterion (1 - sample size)",ylab=paste("Percentage of items have to be gone through out of ",samsize),col="blue",ylim=c(0,100));
    lines(EffortTop10p,col="green");
    lines(EffortTop5p,col="red");
    lines(EffortMax,type="l",col="black",lwd=6);
    title(main=paste("Beta distribution","\n", "alpha=", a, "beta=", b), sub="[Max=black, Top5%=red, Top10%=green, Top25%=blue]");

    dev.off();

    ## Select what to return by 'rvalue'
    z = switch(rvalue,
    max = EffortMax,
    top5 = EffortTop5p,
    top10 = EffortTop10p,
    top25 = EffortTop25p,
    ## By default z = all of them combined
    data.frame(EffortTop25p,EffortTop10p,EffortTop5p,EffortMax)
    );

    return(list(z,min(samPool),max(samPool),a,b));
}






### This function plots the effort needed, i.e. number of items went through (for finding an item at or above certain value range) in cases of success against the number to stop sampling and set a criterion in the sample
## *This function relies on the 'NormalizeBeta' function above
DistribEffort <- function(x,a,b,normal,popfactor,samsize,repfactor) {
    ## scale up the population by # popfactor
    if(normal==0) {
        y = rbeta(x,a,b);
        for(i in 1:popfactor) {
            y = c(y,rbeta(x,a,b));
        }
    }
    else if(normal==1) {
        y = rBetaNormalized(x,a,b);
        for(i in 1:popfactor) {
            y = c(y,rBetaNormalized(x,a,b));
        }
    }
    else {
        print("Please specify if the distribution should be normalized.");
        return(-1);
    }

    ## creating arrays to store the values of effort for each category
    EffortMax = rep(NA, (samsize - 1));
    EffortTop5p = rep(NA, (samsize - 1));
    EffortTop10p = rep(NA, (samsize - 1));
    EffortTop25p = rep(NA, (samsize - 1));

    ##set stopping N from N = 1 to N = sample_size - 1
    for(N in 1:(samsize - 1)) {
    ### We are using only 1 form of Heuristic, i.e. set criterion after N
    ### and the pick the first item exceeding the criterion

        ## Total number of success (out of # repfactor)
        TotalEffortMax = 0;
        TotalEffortTop5p = 0;
        TotalEffortTop10p = 0;
        TotalEffortTop25p = 0;
        CountSuccessMax = 0;
        CountSuccessTop5p = 0;
        CountSuccessTop10p = 0;
        CountSuccessTop25p = 0;

        ## Repeat the same procedure by # repfactor
        for(j in 1:repfactor) {

            ## Create the sample from the population
            sam = sample(y,samsize);
            ## set Criterion out of the first N
            C = max(sam[1:N]);#C = max(head(sam,N));
            ## isolate remains of the sample
            sam2 = tail(sam,(length(sam)-N));
            counter = -1;

            for(i in 1:length(sam2)) {
                ##Candidate is going to store the item at the point of stopping
                Candidate = as.numeric(sam2[i]);
                diff = C - Candidate;
                if(diff < 0) {counter = i; break;}
            }

            ## find the number of items should be in category according to sample size
            NumTop5p = round(length(sam)*.05);
            NumTop10p = round(length(sam)*.1);
            NumTop25p = round(length(sam)*.25);

            ## find the values above which an item will be in a certain category
            TheMax= max(sam);
            MarkTop5p = tail(sort(sam, decreasing=TRUE)[1:NumTop5p],1);
            MarkTop10p = tail(sort(sam, decreasing=TRUE)[1:NumTop10p],1);
            MarkTop25p = tail(sort(sam, decreasing=TRUE)[1:NumTop25p],1);

            if(Candidate == TheMax) {
                TotalEffortMax = TotalEffortMax + N + counter;
                CountSuccessMax = CountSuccessMax + 1;
            }

            if(Candidate >= MarkTop5p) {
                TotalEffortTop5p = TotalEffortTop5p + N + counter;
                CountSuccessTop5p = CountSuccessTop5p + 1;
            }

            if(Candidate >= MarkTop10p) {
                TotalEffortTop10p = TotalEffortTop10p + N + counter;
                CountSuccessTop10p = CountSuccessTop10p + 1;
            }

            if(Candidate >= MarkTop25p) {
                TotalEffortTop25p = TotalEffortTop25p + N + counter;
                CountSuccessTop25p = CountSuccessTop25p + 1;
            }

        }
        EffortMax[N] = TotalEffortMax / CountSuccessMax;
        EffortTop5p[N] = TotalEffortTop5p / CountSuccessTop5p;
        EffortTop10p[N] = TotalEffortTop10p / CountSuccessTop10p;
        EffortTop25p[N] = TotalEffortTop25p / CountSuccessTop25p;

        ## Converting to percentage of item gone through
        EffortMax[N] = (EffortMax[N] / samsize)*100;
        EffortTop5p[N] = (EffortTop5p[N] / samsize)*100;
        EffortTop10p[N] = (EffortTop10p[N] / samsize)*100;
        EffortTop25p[N] = (EffortTop25p[N] / samsize)*100;
    }

    pdf(file=paste("./DistribEffortA",a,"B",b,"S",samsize,"R",repfactor,"Fig.pdf"));

    plot(EffortTop25p,type="l",xlab="Point to stop and set criterion (1 - sample size)",ylab=paste("Percentage of items have to be gone through out of ",samsize),col="blue",ylim=c(0,samsize));
    lines(EffortTop10p,col="green");
    lines(EffortTop5p,col="red");
    lines(EffortMax,type="l",col="black",lwd=6);
    title(main=paste("Beta distribution","\n", "alpha=", a, "beta=", b), sub="[Max=black, Top5%=red, Top10%=green, Top25%=blue]");

    dev.off();

    z = data.frame(EffortTop25p,EffortTop10p,EffortTop5p,EffortMax);

    return(z);
}



### This function plots the probability of success (for finding an item at or above certain value range) against the number to stop sampling and set a criterion in the sample
DistribCriterion <- function(x,a,b,normal,popfactor,samsize,repfactor) {
    ## scale up the population by # popfactor
    if(normal==0) {
        y = rbeta(x,a,b);
        for(i in 1:popfactor) {
            y = c(y,rbeta(x,a,b));
        }
    }
    else if(normal==1) {
        y = rBetaNormalized(x,a,b);
        for(i in 1:popfactor) {
            y = c(y,rBetaNormalized(x,a,b));
        }
    }
    else {
        print("Please specify if the distribution should be normalized.");
        return(-1);
    }

    ## create arrays to store values for Probability of success
    PSuccessMax = rep(NA, (samsize - 1));
    PSuccessTop5p = rep(NA, (samsize - 1));
    PSuccessTop10p = rep(NA, (samsize - 1));
    PSuccessTop25p = rep(NA, (samsize - 1));

    ## set stopping N from N = 1 to N = (sample_size - 1)
    for(N in 1:(samsize - 1)) {
    ### We are using only 1 form of Heuristic, i.e. set criterion after N
    ### and the pick the first item exceeding the criterion

        ## Total number of success (out of # repfactor)
        TotalFlagMax = 0;
        TotalFlagTop5p = 0;
        TotalFlagTop10p = 0;
        TotalFlagTop25p = 0;

        ## Repeat the same procedure by # repfactor
        for(j in 1:repfactor) {

            ## Create the sample from the population
            sam = sample(y,samsize);
            ## set Criterion out of the first N
            C = max(sam[1:N]);#C = max(head(sam,N));
            ## isolate remains of the sample
            sam2 = tail(sam,(length(sam)-N));

            for(i in 1:length(sam2)) {
                ##Candidate is going to store the item at the point of stopping
                Candidate = as.numeric(sam2[i]);
                diff = C - Candidate;
                if(diff < 0) {break;}
            }

            ## find the number of items should be in category according to sample size
            NumTop5p = round(length(sam)*.05);
            NumTop10p = round(length(sam)*.1);
            NumTop25p = round(length(sam)*.25);

            ## find the values above which an item will be in a certain category
            TheMax= max(sam);
            MarkTop5p = tail(sort(sam, decreasing=TRUE)[1:NumTop5p],1);
            MarkTop10p = tail(sort(sam, decreasing=TRUE)[1:NumTop10p],1);
            MarkTop25p = tail(sort(sam, decreasing=TRUE)[1:NumTop25p],1);

            if(Candidate == TheMax) {
                FlagMax = 1;
            } else {
                FlagMax = 0;
            }

            if(Candidate >= MarkTop5p) {
                FlagTop5p = 1;
            } else {
                FlagTop5p = 0;
            }

            if(Candidate >= MarkTop10p) {
                FlagTop10p = 1;
            } else {
                FlagTop10p = 0;
            }

            if(Candidate >= MarkTop25p) {
                FlagTop25p = 1;
            } else {
                FlagTop25p = 0;
            }

            ## Adding to the total count of success in repfactor runs
            TotalFlagMax = TotalFlagMax + FlagMax;
            TotalFlagTop5p = TotalFlagTop5p + FlagTop5p;
            TotalFlagTop10p = TotalFlagTop10p + FlagTop10p;
            TotalFlagTop25p = TotalFlagTop25p + FlagTop25p;
        }

        ## Putting the total count of success after repfactor runs in the array
        PSuccessMax[N] = TotalFlagMax;
        PSuccessTop5p[N] = TotalFlagTop5p;
        PSuccessTop10p[N] = TotalFlagTop10p;
        PSuccessTop25p[N] = TotalFlagTop25p;
    }

    pdf(file="./DistribPSuccessFig.pdf");

    plot(PSuccessTop25p,type="l",xlab="Point to stop and set criterion (1 - sample size)",ylab=paste("Probability of success out of ",repfactor),col="blue",ylim=c(0,repfactor));
    lines(PSuccessTop10p,col="green");
    lines(PSuccessTop5p,col="red");
    lines(PSuccessMax,type="l",col="black",lwd=6);
    title(main=paste("Beta distribution","\n", "alpha=", a, "beta=", b), sub="[Max=black, Top5%=red, Top10%=green, Top25%=blue]");

    dev.off();

    z = data.frame(PSuccessTop25p,PSuccessTop10p,PSuccessTop5p,PSuccessMax);

    return(z);
}
