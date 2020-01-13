#STA141AHW4
#Sam Tsoi
library(ggplot2)
##1
simulate_monopoly <- function(n,d)
{
  currRoll <- numeric(2)
  currRollSum <- 0
  spot = 0
  position <- numeric(n+1)
  roll <-numeric(n+1)
  i = 1
  for(i in 1:n+1)
  {
    currRoll <- sample(1:d, 2, replace=T)
    currRollSum <- sum(currRoll)
    currPos <- ((position[i-1] + currRollSum) %%39)
    
    chProb <- c(rep((1/16),10), 6/16) #probability of CH 10/16
    if (currPos == 30) #G2J
    {
      currPos <- 10
    }
    else if (currPos == 2 || currPos == 17 || currPos == 33)#CC
    {
      currPos <- sample(c(0,10, currPos), 1, prob = c(1/16,1/16,14/16)) #advance to GO:1/16 or go to JAIL:1/16 or stay:14/16
    }
    else if(currPos == 7) #CH1
    {
      currPos <- sample(c(0,10,11,24,39,5,15,15,12,currPos-3,currPos),1, prob = chProb)
    }
    else if(currPos == 22) #CH2
    {
      currPos <- sample(c(0,10,11,24,39,5,25,25,28,currPos-3,currPos),1, prob = chProb)
    }
    else if(currPos == 36) ##CH3
    {
      currPos <- sample(c(0,10,11,24,39,5,5,5,12,currPos-3,currPos),1, prob = chProb)
    }
    
    position[i] <- currPos
  }
  return(position)
}
##2
estimate_monopoly <- function(n,d)
{
  rollResult <- simulate_monopoly(n,d)
  # prop <- prop.table(table(rollResult))
  prop <- prop.table(table(factor(rollResult, levels = c(0:39)))) #we need factor() so it doesn't drop the 0 for tile 30 which is G2J
  
  return(prop)
}
longtermprob3 <- estimate_monopoly(10000,3)
longtermprob4 <- estimate_monopoly(10000,4)
longtermprob5 <- estimate_monopoly(10000,5)
longtermprob6 <- estimate_monopoly(10000,6)
mostlikely6 <- sort(longtermprob6, decreasing=TRUE)
mostlikely6[1:3]
mostlikely4 <- sort(longtermprob4, decreasing=TRUE)
mostlikely4[1:3]
plot(longtermprob3,type ="l",col="blue",xlab = "Position on board", ylab= "Probability", ylim = c(0,0.065), main = "Long-term probability (n=10,000) of where a player would \n land in Monopoly with different-sided die")
lines(longtermprob4, type="l", col="red")
lines(longtermprob5, type="l", col ="green")
lines(longtermprob6, type = "l", col = "cyan")
legend(x=29,y=.06, legend =c("3-sided", "4-sided","5-sided","6-sided"), col=c("blue","red","green","cyan"),lty=1:4)


##3
simulations <- replicate(1000, estimate_monopoly(10000,6))
hist(simulations[11,], main="distribution of ending a turn in jail", xlab = "Proportion of ending a turn in jail")
se <- sd(simulations[11,])
mean <- mean(simulations[11,])
median <- median(simulations[11,])
##4
mono6 <- simulate_monopoly(10000,6)
boot<-function(){
  boot.jail = sample(mono6,1000,replace = TRUE)
  jail.prop<-length(which(boot.jail==10))/1000
  return(jail.prop)
}
temp<-boot()
B = replicate(1000,boot())

hist(B, main = "Bootstrap distribution (B=1000) of ending \n a turn in jail on the Monopoly board", xlab = "Proportion of ending a turn in jail")
bootse<-sd(B)
bootmean<-mean(B)
bootmedian <- median(B)
##5
mono3 <- simulate_monopoly(10000,3)
mono4 <- simulate_monopoly(10000,4)
mono5 <- simulate_monopoly(10000,5)
mono6 <- simulate_monopoly(10000,6)
boots<-function(d){
  allcellprop <- c()
  boot.allcell = sample(d,1000,replace = TRUE)
  i = 0
  for(i in 0:39)
  {
    allcellprop <- c(allcellprop, length(which(boot.allcell==i))/1000)
  }
  allcellpropdf <- data.frame(allcellprop)
  return(allcellpropdf)
}
#boots6 <- boots(mono6)
B3 = replicate(1000,boots(mono3))
B4 = replicate(1000,boots(mono4))
B5 = replicate(1000,boots(mono5))
B6 = replicate(1000,boots(mono6))
B3df <- t(data.frame(B3))
B4df <- t(data.frame(B4))
B5df <- t(data.frame(B5))
B6df <- t(data.frame(B6))


boxplot(B3df, main = "Bootstrap distribution (B=1000) of  each cell \n on the Monopoly board with 3-sided dice", xlab = "Position on board+1", ylab = "Proportion of ending a turn in respective cell")
boxplot(B4df, main = "Bootstrap distribution (B=1000) of  each cell \n on the Monopoly board with 4-sided dice", xlab = "Position on board+1", ylab = "Proportion of ending a turn in respective cell")
boxplot(B5df, main = "Bootstrap distribution (B=1000) of  each cell \n on the Monopoly board with 5-sided dice", xlab = "Position on board+1", ylab = "Proportion of ending a turn in respective cell")
boxplot(B6df, main = "Bootstrap distribution (B=1000) of  each cell \n on the Monopoly board with 6-sided dice", xlab = "Position on board+1", ylab = "Proportion of ending a turn in respective cell")