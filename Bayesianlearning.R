###############################################################################
#                 Team Ranking using Bayesian Optimazation                    #
###############################################################################

# read data
data <- read.csv(file="NFL2018data.csv", header=TRUE)
Teams <- levels(data$Winner.Team)

# parameters

M <- length(Teams) # total 32 NFL teams
##########################################
# updated functions
vx <- function(x){
  return((dnorm(x,0,1)/pnorm(x,0,1)))
}

wx <- function(x){
  v <- vx(x)
  return((v*(v+x)))
}

# Bayesian learning model

BL <- function(theta0, data){
  #parameter
  N <- nrow(data) # 256 budgets (# of games)
  # initial values
  lambda <- 10  #lambda^2=100
  theta <- theta0 #prior performance based on 2017 ranking
  sigma2 <- runif(M,100,150)
  
  # save the results
  Steps <- seq(0,N,1) #steps
  Theta <- matrix(0,nrow=(N+1),ncol=M)
  Sigma2 <- matrix(0,nrow=(N+1),ncol=M)
  Theta[1,] <- theta
  Sigma2[1,] <- sigma2

  # updated theta, sigma
  for (n in 1:N){
    i <- data[n,4] #winner
    j <- data[n,6] #loser
    temp1 <- ((theta[i] - theta[j])/sqrt(sigma2[i]+sigma2[j]+2*lambda^2))
    theta[i] <- theta[i] + (sigma2[i]*vx(temp1)/sqrt(sigma2[i]+sigma2[j]+2*lambda^2))
    theta[j] <- theta[j] - (sigma2[j]*vx(temp1)/sqrt(sigma2[i]+sigma2[j]+2*lambda^2))
    sigma2[i] <- sigma2[i]*(1-(sigma2[i]*wx(temp1)/(sigma2[i]+sigma2[j]+2*lambda^2)))
    sigma2[j] <- sigma2[j]*(1-(sigma2[j]*wx(temp1)/(sigma2[j]+sigma2[i]+2*lambda^2)))
    Theta[(n+1),] <- theta
    Sigma2[(n+1),] <- sigma2
  }
  Rank <- order(Theta[(n+1),],decreasing = T)
  res <- rbind(Rank,Theta,Sigma2)
  return(res)
}

#Results
theta0 <- runif(M,0.3,0.5)
theta0 <- theta0[order(theta0,decreasing = T)]
rank_2017 <- read.csv(file="rank_2017.csv", header=TRUE)[,1]
theta0 <- theta0[rank_2017]
Teams[order(theta0,decreasing = T)]
##########
ranks <- BL(theta0,data)[1,]
Teams[ranks]

#######################
# comparison 
rank2018 <- read.csv(file="rank_2018.csv", header=T)
# 22 18 16  6  3 17 21 13 29 14  9 26 32 20 27  8 32 12  2 19 10  5  7  4 23 30 11 25 28 15 24  1
rank2018_true <- rank2018$rank
Teams[order(rank2018_true)]
rank2018_fit <- order(ranks)
diff1 <- (rank2018_true - rank2018_fit)
diff2 <- (rank2018_true - rank_2017)


diff_res <- data.frame(cbind(rank2018$index,diff1,diff2))
diff <- melt(diff_res,id="V1")


library(ggplot2)
library(reshape2)


p1 <- ggplot(data = diff, aes(x = V1, y = value, color = variable)) + 
  geom_point(size=2) +
  labs(title = "Figure 1: Ranking Comparison, Bandwidth=[-5,5]\n", x = "Teams (index)", y = "Ranking Difference", color = "Comparison\n") +
  scale_color_manual(labels = c("2018 vs Estimated", "2017 vs 2018"), values = c("red", "blue"))+
  theme_bw() +
  theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold", color = "darkgreen"))+
  geom_hline(yintercept=-5, linetype="dashed",size=0.5) +
  geom_hline(yintercept=5, linetype="dashed", size=0.5) +
  geom_hline(yintercept=0, linetype="dashed", size=0.25) 


p2 <- ggplot() +
  # blue plot
  geom_point(data=diff_res, aes(x=V1, y=diff2),colour="blue",size=2) + 
  geom_smooth(data=diff_res, aes(x=V1, y=diff2), fill="blue",
              colour="darkblue", size=1) +
  # red plot
  geom_point(data=diff_res, aes(x=V1, y=diff1),colour="red",size=2) + 
  geom_smooth(data=diff_res, aes(x=V1, y=diff1), fill="red",
               colour="red", size=1) +
  labs(title = "Figure 2: Ranking Comparison\n", x = "Teams (index)", y = "Ranking Difference", color = "Legend Title\n") +
  theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold", color = "darkgreen"))



# Results
# # way 1
# rank_2017 <- read.csv(file="2017 NFL Standings.csv", header=TRUE)
# theta0 <- (M - rank_2017[,2])/10
# Teams[order(theta0,decreasing = T)]
# ########
# ranks1 <- BL(theta0,data)[1,]
# Teams[ranks1]
# 
# # way 2
# # rank 2017 
# theta0 <- runif(M,0.3,0.5)
# #rank2017 <- c(20,21,26,27,5,18,22,2,15,16,3,4,9,11,17,29,31,1,7,12,32,19,25,28,6,10,24,30,13,14,23,8)
# theta0 <- theta0[rank2017]
# Teams[order(theta0,decreasing = T)]
# #########
# ranks2 <- BL(theta0,data)[1,]
# Teams[ranks2]



# #######################
# # comparison 
# rank2018 <- read.csv(file="rank_2018.csv", header=T)
# # 22 18 16  6  3 17 21 13 29 14  9 26 32 20 27  8 32 12  2 19 10  5  7  4 23 30 11 25 28 15 24  1
# rank2018_true <- rank2018$rank
# Teams[order(rank2018_true)]
# # way 1
# rank2018_fit1 <- order(ranks1)
# diff1 <- (rank2018_true - rank2018_fit1)
# Team_index <- 1:32
# plot(Team_index, diff1)
# abline(a=-5,b=0)
# abline(a=5,b=0)
# 
# # way 2
# rank2018_fit2 <- order(ranks2)
# diff2 <- (rank2018_true - rank2018_fit2)
# plot(Team_index, diff2)
# abline(a=-5,b=0)
# abline(a=5,b=0)
# abline(a=0,b=0,col="red")

##################################################
# Matching for the next game
##################################################
# f function
fx <- function(x){
  return(x*pnorm(x,0,1) + dnorm(x,0,1))
}

# LTM function: two groups assignment

LTM <- function(theta, sigma2, group1, group2){
  # if number of teams are even, then K1=K2
  K1 <- length(group1)
  K2 <- length(group2)
  x <- rep(0,K1)
  for (i in 1:K1){
    ltm <- rep(0,K2)
    for(j in 1:K2){
      temp <- -abs(theta[group2[j]]-theta[group1[i]])/sqrt(sigma2[group2[j]]+sigma2[group1[i]])
      ltm[j] <- 2*sqrt(sigma2[group2[j]]+sigma2[group1[i]])*fx(temp)
      
    }
    index <- which.max(ltm)
    x[i] <- group2[index]
    K2 <- K2-1
    group2 <- group2[-index]
  }
  return(rbind(group1,x))
}

DC <- function(theta,sigma2, group1, group2){
  # if number of teams are even, then K1=K2
  lambda <- 10
  K1 <- length(group1)
  K2 <- length(group2)
  x <- rep(0,K1)
  for (i in 1:K1){
    dc <- rep(0,K2)
    for(j in 1:K2){
      temp <- -(theta[group2[j]]-theta[group1[i]])^2/(2*(sigma2[group2[j]]+sigma2[group1[i]]+lambda^2))
      dc[j] <- exp(temp)/sqrt(2*pi*(sigma2[group2[j]]+sigma2[group1[i]]+2*lambda^2))
    }
    index <- which.max(dc)
    x[i] <- group2[index]
    K2 <- K2-1
    group2 <- group2[-index]
  }
  return(rbind(group1,x))
}

# Based on the result of first 240 games, try to match the opponents for the 17 week games.
# totally 240 games
result_16 <- BL(theta0, data[1:240,])
theta_16 <- result_16[242,]  # N+1+1 = 242
sigma2_16 <- result_16[483,] # (N+1)*2+1 = 483

# random divided two groups
all <- 1:32
group1 <- sample(all,16)
group2 <- all[-group1]
# matched teams
match_17 <- LTM(theta_16,sigma2_16,group1,group2)
match_17_2 <- DC(theta_16,sigma2_16,group1,group2)

