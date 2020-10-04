library(ggplot2)
library(WVPlots)
library(reshape2)
library(gridExtra)

groupGames <- c("Game1", "Game2","Game3")
grGoalsUSA <- c(13,3,2)
grGoalsNED <- c(2,3,1)

groupStages = cbind.data.frame(groupGames,grGoalsUSA,grGoalsNED)
colnames(groupStages)= c("GroupStageGames","USA_GroupStageGoals","NED_GroupStageGoals")
print(groupStages)

knockoutGames <- c("Game1","Game2","Game3")
koGoalsUSA <- c(2,2,2)
koGoalsNED <- c(2,2,1)

knockoutStages = cbind.data.frame(knockoutGames,koGoalsUSA,koGoalsNED)
colnames(knockoutStages)= c("KnockoutStageGames","USA_KnockoutGoals","NED_KnockoutGoals")
print(knockoutStages)

groupStages2=melt(groupStages)
print(groupStages2)

knockoutStages2 = melt(knockoutStages)
print(knockoutStages2)

#par(mfrow=c(2,1))
gsHist = ggplot(groupStages2) +
  aes(x = GroupStageGames, fill = variable, weight = value) +
  geom_bar() +
  labs(x = "Group Stage Games", y = "Goal Count")+
  scale_fill_manual(values=c("red","blue")) +
  theme_minimal() +
  facet_wrap(vars(variable))+
  theme(legend.position = "none")+
  ylim(0,13)

ksHist = ggplot(knockoutStages2) +
 aes(x = KnockoutStageGames, fill=variable, weight = value) +
 geom_bar() +
 labs(x = "Knockout Stage Games", y = "Goal Count") +
  scale_fill_manual(values=c("red","blue")) +
  theme_minimal() +
  facet_wrap(vars(variable))+
  theme(legend.position = "none")+
  ylim(0,13)
grid.arrange(gsHist,ksHist, ncol=2)

#Non-informative(Jeffery's prior):
#alpha.U,beta.U = 1/2, 0
#alpha.N,beta.N = 1/2,0

#alpha.U.post = 18.5
#beta.U.post = 3

#alpha.N.post = 6.5
#beta.N.post = 3

alpha.beta.Calc <- function(data){
  alpha = (mean(data)^2)/(var(data)-mean(data))
  beta = mean(data)/(var(data)-mean(data))
  return(list(alpha=alpha,beta=beta))
}

alpha.N = alpha.beta.Calc(groupStages$NED_GroupStageGoals)$alpha
beta.N = alpha.beta.Calc(groupStages$NED_GroupStageGoals)$beta

alpha.U = alpha.beta.Calc(groupStages$USA_GroupStageGoals)$alpha
beta.U = alpha.beta.Calc(groupStages$USA_GroupStageGoals)$beta

dposterior.U <- function(lambda,data,alpha,beta){
  n = length(data)
  shape = alpha+n*mean(data)
  rt = beta + n
  return(dgamma(lambda,shape,rt))
}

dposterior.N <- function(lambda,data,alpha,beta){
  n = length(data)
  shape = alpha+n*mean(data)
  rt = beta + n
  return(dgamma(lambda,shape,rt))
}

rPosteriorU <- function(B, data, alpha, beta){
  n = length(data)
  sh = n*mean(data) + alpha
  rt = n + beta
  return(rgamma(B, sh, rt))
}

rPosteriorN <- function(B, data, alpha, beta){
  n = length(data)
  sh = n*mean(data) + alpha
  rt = n + beta
  return(rgamma(B, sh, rt))
}

par(mfrow=c(2,1))

lambda <- seq(0.00001,8,length=100)

# plot(lambda, dposterior.U(lambda,knockoutStages$USA_KnockoutGoals,alpha.U,beta.U),
#      type = "l",
#      ylab = "density",
#      main="Posterior of lambda for United States")
# 
# plot(lambda, dposterior.N(lambda,knockoutStages$NED_KnockoutGoals,alpha.N,beta.N),
#      type = "l",
#      ylab = "density",
#      main="Posterior of lambda for Netherlands")

#me trying to plot prior distributions 
#when I know full well that NANs will be produced for dgamma(x,alpha.N,beta.N)

x = lambda
y.U = dgamma(x,alpha.U,beta.U)
y.N = dgamma(x,alpha.N,beta.N)
priorData.U = cbind.data.frame(x,y.U)
priorData.N = cbind.data.frame(x,y.N)
ggplot() +
  geom_line(priorData.U, mapping = aes(x = x, y = y.U,colour="USA Prior Distribution"))+
  geom_line(priorData.N, mapping = aes(x = x, y = y.N,colour="Netherlands Prior Distribution"))+
  scale_fill_manual(name="Distributions",values = alpha(c("blue","red")), .18)+
  scale_color_manual(name="",values = c("blue","red"))+
  theme_minimal()+
  theme(legend.position = 'top')+
  xlim(0,8)+
  labs(x="ƛ",y="density")

#Overlay of posterior density distributions
x = lambda
y = dposterior.U(lambda,knockoutStages$USA_KnockoutGoals,alpha.U,beta.U)
y.2 = dposterior.N(lambda,knockoutStages$NED_KnockoutGoals,alpha.N,beta.N)
dayTa = cbind.data.frame(x,y)
dayTa.2 = cbind.data.frame(x,y.2)
ggplot() +
  geom_line(dayTa, mapping = aes(x = x, y = y,colour="USA Posterior Distribution"))+
  geom_line(dayTa.2, mapping = aes(x = x, y = y.2,colour="Netherlands Posterior Distribution"))+
  scale_fill_manual(name="Distributions",values = alpha(c("blue","red")), .18)+
  scale_color_manual(name="",values = c("blue","red"))+
  theme_minimal()+
  theme(legend.position = 'top')+
  xlim(0,8)+
  labs(x="ƛ",y="Density")

# Sample lambda from the posterior
B = 20000

lambda.U.post <- rPosteriorU(B, knockoutStages$USA_KnockoutGoals, alpha.U, beta.U )
plot(lambda.U.post,
     main = "Posterior of lambda for US",
     type = "l")

lambda.N.post <- rPosteriorN(B, knockoutStages$NED_KnockoutGoals, alpha.N, beta.N )
plot(lambda.N.post,
     main = "Posterior of lambda for NED",
     type = "l")

# Sample number of goals score for Final Game
y.U.post <- rpois(B, lambda.U.post) 
y.N.post <- rpois(B,lambda.N.post)

# Posterior Predictive distribution of goals scored by US in Final Game
hist(y.U.post)
hist(y.N.post)
hist(post.diff,freq = FALSE)
# Probability that the US scores at least one goal: P(Y >= 1)
length(which(y.U.post >= 1))/B
length(which(y.N.post >=1))/B

#Overlay of normal approximation on Histogram of lambda differences using generic R plot
#par(mfrow=c(1,1))
#hist(lambda.U.post-lambda.N.post,freq = FALSE)
#curve(dnorm(x,mean=1.24,sd=1.31),type="l",add = TRUE)

#Overlay of Normal approximation on Monte Carlo approximation using ggplot
lambda.post.diff = lambda.U.post-lambda.N.post
ggplot(data=NULL, mapping = aes(lambda.post.diff)) + 
  #geom_histogram(aes(x = lambda.post.diff, y =..density..), colour="black", fill="gray", alpha=0.3)+
  geom_density(colour="red", size = 1, fill="red", alpha = 0.5)+
  geom_area(stat = "function",
            fun = dnorm,
            args = list(mean = 1.24, sd = 1.31),
            colour="steelblue4",
            size=1,
            fill="steelblue3",
            alpha=0.5)+
  theme_minimal()+
  labs(x ="ƛ.USA - ƛ.NED", y = "Density") + 
  ggtitle("         Overlay of Normal approximation(blue) on MC approximation(red)")

#Monte Carlo Approximation
length(which(lambda.U.post>lambda.N.post))/B

#Analytic 
#install.packages("hypergeo")
#library(hypergeo)

n.U.post = length(knockoutStages$USA_KnockoutGoals)
alpha.U.post = alpha.U+n.U.post*mean(knockoutStages$USA_KnockoutGoals)
beta.U.post = beta.U + n.U.post

n.N.post = length(knockoutStages$NED_KnockoutGoals)
alpha.N.post = alpha.N+n.N.post*mean(knockoutStages$NED_KnockoutGoals)
beta.N.post = beta.N + n.N.post

1-(1/(alpha.U.post*beta(alpha.N.post,alpha.U.post))*
     ((beta.U.post/(beta.N.post+beta.U.post))^alpha.U.post)*
   hypergeo(alpha.U.post,1-alpha.N.post,1+alpha.U.post,beta.U.post/(beta.N.post+beta.U.post))
   )

#Normal approximation
pnorm(0,1.24,1.31,lower.tail = FALSE)

## Obtain posterior predictive probability of US winning
length(which(y.U.post > y.N.post))/B

## Obtain posterior predictive probability of Netherlands winning
length(which(y.N.post > y.U.post))/B

## Obtain posterior predictive probability of a tie between both teams
length(which(y.U.post == y.N.post))/B

post.diff = y.U.post-y.N.post
lambda.diff = lambda.U.post - lambda.N.post


Z <- data.frame(V1=post.diff,V2=lambda.diff)
threshold <- quantile(Z[, 2], prob =1- 0.649)[[1]]
p = ShadedDensity(frame = Z, 
                  xvar = "V2",
                  threshold = threshold,
                  title = "                                         Probability that USA will win",
                  tail = "right")
    p+
    theme_minimal()+
    geom_histogram(aes(x = post.diff, y =..density..),bins=40,alpha=0.2)+
    labs(x ="USA.post - NED.post", y = "Density")

