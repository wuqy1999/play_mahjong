ron <- function(numset){
  judge <- 0
  #input the numbers set and return ron(1) or not(0)
  ronlist <- list(c(0,1,2),c(1,2,3),c(2,3,4),c(3,4,5),c(4,5,6),c(5,6,7),c(6,7,8),c(7,8,9),c(8,9,0),c(9,0,1))
  for(i in 1:length(ronlist)){
    if(all(ronlist[[i]]%in%numset)){
      judge <- 1
      break
    }
  }
  return(judge)
}

neighbor <- function(x){
  #return the neighbor of x
  if(x==0){
    return(c(1,9))
  }
  if(x==9){
    return(c(8,0))
  }
  if((x!=0)&&(x!=9)){
    return(c(x-1,x+1))
  }
}

next_neighbor <- function(x){
  #return the neighbor of x
  if(x==0){
    return(c(2,8))
  }
  if(x==1){
    return(c(3,9))
  }
  if(x==9){
    return(c(7,1))
  }
  if(x==8){
    return(c(6,0))
  }
  if((x!=0)&&(x!=9)&&(x!=1)&&(x!=8)){
    return(c(x-2,x+2))
  }
}


score <- function(numset){
  #judge the value of numbers
  value <-c()
  for(i in 1:length(numset)){
    if(numset[i] %in% numset[-i]){
      value <-c(value,0)
      next
    }
    if((neighbor(numset[i])[1] %in% numset) || (neighbor(numset[i])[2] %in% numset)){
      value <-c(value,3)
      next
    }
    if((next_neighbor(numset[i])[1] %in% numset) && (next_neighbor(numset[i])[2] %in% numset)){
      value <-c(value,3)
      next
    }
    if((next_neighbor(numset[i])[1] %in% numset) || (next_neighbor(numset[i])[2] %in% numset)){
      value <-c(value,2)
      next
    }
    value <-c(value,1)
  }  
  return(value)
}


cards <- 0:9


#case1
case1 <- function(iter=1000){
  ronturns <- c()
  for(i in 1 : iter){
    hand <- sample(cards,4,replace = T)
    ronturn <- 0
    while(ron(hand)!=1){
      hand <- c(sample(cards,1,replace = F),sample(hand,3,replace = F))
      ronturn <- ronturn+1
    }
    ronturns <- c(ronturns,ronturn)
  }
  return(ronturns)
}

#case2
case2 <- function(iter=1000){
  ronturns <- c()
  for(i in 1 : iter){
    hand <- sample(cards,4,replace = T)
    ronturn <- 0
    while(ron(hand)!=1){
      hand <- c(sample(cards,1,replace = F),hand)
      ronturn <- ronturn+1
    }
    ronturns <- c(ronturns,ronturn)
  }
  return(ronturns) 
}

#case3
case3 <- function(iter=1000){
  ronturns <- c()
  for(i in 1 : iter){
    hand <- sample(cards,4,replace = T)
    ronturn <- 0
    while(ron(hand)!=1){
      handscore <- score(hand)
      index <- which(handscore==min(handscore))[1]
      hand <- c(sample(cards,1,replace = F),hand[-index])
      ronturn <- ronturn+1
    }
    ronturns <- c(ronturns,ronturn)
  }
  return(ronturns)
}

set.seed(233)
case1_data <- case1(2000)
case2_data <- case2(2000)
case3_data <- case3(2000)

plot(density(case1_data,bw=1),xlab = "和牌巡目",main = "",col="red",lwd=2)
plot(density(case2_data,bw=1),xlab = "和牌巡目",main = "",col="blue",lwd=2)
plot(density(case3_data,bw=1),xlab = "和牌巡目",main = "",col="green",lwd=2)

plot(density(case1_data,bw=1),xlab = "和牌巡目",main = "",col="red",lwd=2,xlim=c(0,60),ylim=c(0,0.18))
lines(density(case2_data,bw=1)$x,density(case2_data,bw=1)$y,col="blue",lwd=2)
lines(density(case3_data,bw=1)$x,density(case2_data,bw=1)$y,col="green",lwd=2)
legend("topright",legend = c("case1","case2","case3"),lwd=2,col = c("red","blue","green"))

boxplot(cbind(case1_data,case2_data,case3_data),names=c("case1","case2","case3"),col = c("red","blue","green"),outline=F)
(c(mean(case1_data),mean(case2_data),mean(case3_data)))
(c(sd(case1_data),sd(case2_data),sd(case3_data)))


#-----------------------#
ron2 <- function(numset){
  judge <- 0
  #input the numbers set and return ron(1) or not(0)
  ronlist <- list(c(3,4,5),c(4,5,6),c(5,6,7))
  for(i in 1:length(ronlist)){
    if(all(ronlist[[i]]%in%numset)){
      judge <- 1
      break
    }
  }
  return(judge)
}

score2 <- function(numset){
  value <- c()
  for(i in 1:length(numset)){
    if(numset[i] %in% numset[-i] || numset[i] %in% c(0,1,2,8,9)){
      value <-c(value,0)
    }
    else{
      value <-c(value,1)
    }
  }
  return(value)
}

#case4
case4 <- function(iter=1000){
  ronturns <- c()
  for(i in 1 : iter){
    hand <- sample(cards,4,replace = T)
    ronturn <- 0
    while(ron2(hand)!=1){
      hand <- c(sample(cards,1,replace = F),sample(hand,3,replace = F))
      ronturn <- ronturn+1
    }
    ronturns <- c(ronturns,ronturn)
  }
  return(ronturns)
}

#case5
case5 <- function(iter=1000){
  ronturns <- c()
  for(i in 1 : iter){
    hand <- sample(cards,4,replace = T)
    ronturn <- 0
    while(ron2(hand)!=1){
      hand <- c(sample(cards,1,replace = F),hand)
      ronturn <- ronturn+1
    }
    ronturns <- c(ronturns,ronturn)
  }
  return(ronturns) 
}

#case6
case6 <- function(iter=1000){
  ronturns <- c()
  for(i in 1 : iter){
    hand <- sample(cards,4,replace = T)
    ronturn <- 0
    while(ron2(hand)!=1){
      handscore <- score2(hand)
      index <- which(handscore==min(handscore))[1]
      hand <- c(sample(cards,1,replace = F),hand[-index])
      ronturn <- ronturn+1
    }
    ronturns <- c(ronturns,ronturn)
  }
  return(ronturns)
}

case4_data <- case4(2000)
case5_data <- case5(2000)
case6_data <- case6(2000)

plot(density(case4_data),xlab = "和牌巡目",main = "",col="red",lwd=2)
plot(density(case5_data),xlab = "和牌巡目",main = "",col="blue",lwd=2)
plot(density(case6_data),xlab = "和牌巡目",main = "",col="green",lwd=2)

plot(density(case4_data),xlab = "和牌巡目",main = "",col="red",lwd=2,xlim=c(0,100),ylim=c(0,0.08))
lines(density(case5_data)$x,density(case5_data,bw=1)$y,col="blue",lwd=2)
lines(density(case6_data)$x,density(case6_data,bw=1)$y,col="green",lwd=2)
legend("topright",legend = c("case4","case5","case6"),lwd=2,col = c("red","blue","green"))

boxplot(cbind(case4_data,case5_data,case6_data),names=c("case4","case5","case6"),col = c("red","blue","green"),outline=F)
(c(mean(case4_data),mean(case5_data),mean(case6_data)))
(c(sd(case4_data),sd(case5_data),sd(case6_data)))

#compare
plot(density(case1_data,bw=1),xlab = "和牌巡目",main = "",col="black",lwd=2)
lines(density(case4_data)$x,density(case4_data)$y,col="red",lwd=2)
legend("topright",legend = c("附加条件前","附加条件后"),lwd=2,col = c("black","red"))

plot(density(case2_data,bw=1),xlab = "和牌巡目",main = "",col="black",lwd=2)
lines(density(case5_data)$x,density(case5_data)$y,col="blue",lwd=2)
legend("topright",legend = c("附加条件前","附加条件后"),lwd=2,col = c("black","blue"))

plot(density(case3_data,bw=1),xlab = "和牌巡目",main = "",col="black",lwd=2)
lines(density(case6_data)$x,density(case6_data)$y,col="green",lwd=2)
legend("topright",legend = c("附加条件前","附加条件后"),lwd=2,col = c("black","green"))
