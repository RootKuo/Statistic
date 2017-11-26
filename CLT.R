#This is a simulation of sampling from a bernoulli distribution(which success prob. = P).
#And the purpose is to show the CENTRAL LIMIT THEOREM by setting the sample size and sample times.
#Please setup the parameters belowing ##Initiation and RUN ALL OF THE CODE(or click the 'Source').

##Initiation
P = 0.5                    #Number within [0,1]
N = c(3,15,30)             #At most five positive numbers (at least a number)
ST = Sampling_Times = 100  #Positive Number
Line_Up = F                #Logical value
Draw_With_Histogram = T    #Logical value
Step = 0.10                #Number within (0,1] ; Requested when Draw_With_Histogram = T
Show_Theorem_Curve = T     #Logical value

######Code Start from Here#####
##Drawing Space
YLIM = ST * 1.2
plot(0,0,type='n',xlim=c(0,1),ylim=c(0,YLIM)
     ,xlab='Ratio',ylab='Counts')
text(0.1,YLIM*0.95,labels=paste0('Sampling Times = ',ST),cex=1.5,adj=0)
text(0.1,YLIM*0.85,labels='Sampling Size',cex=1.5,adj=0)
text(0.2,YLIM*0.75,labels='n =',cex=1.5,adj=0)

##Ploting Color
library(RColorBrewer)
PickCol = c()
if(length(N)==1){
  PickCol = 6
}else if(length(N)==2){
  PickCol = c(5,7)
}else if(length(N)==3){
  PickCol = c(4,6,8)
}else{
  PickCol = 4:8
}
SamCol = brewer.pal(9,'YlOrRd')[PickCol]
TheCol = brewer.pal(9,'YlGnBu')[PickCol]

#display.brewer.all()

for(k in 1:length(N)){
  
  ##Sampling
  x = c()
  for(i in 1:ST){
    x = c(x,rbinom(1,N[k],P))
  }
  Count = table(x)
  Prob = as.numeric(names(Count))/N[k]
  
  ##Count step by step
  NoProb = seq(0,1,Step)
  NoCount = rep(0,1/Step+1)
  i = 1 ; j = 1
  while(j*Step <= 1){
    while(Prob[i] <= NoProb[j]+Step/2){
      NoCount[j] = NoCount[j] + Count[i]
      i = i + 1
      if(i>length(Count)) break()
    }
    j = j + 1
    if(i>length(Count)) break()
  }
  
  ##Ploting color
  SCOL = SamCol[k]
  TCOL = TheCol[k]
  
  ##Ploting without steping
  if(Draw_With_Histogram == F){
    Ytemp = c()
    for(i in 1:length(Prob)) Ytemp = c(Ytemp,0,Count[i])
    for(j in 1:length(Prob)){
      lines(rep(Prob,each=2)[(2*j-1):(2*j)],
            Ytemp[(2*j-1):(2*j)],lwd=4,col=SCOL)
    }
    if(Line_Up == T) lines(c(0,Prob,1),c(0,Count,0),lwd=2,col=SCOL)
  }

  ##Ploting with Histogram
  if(Draw_With_Histogram == T){
    Xtemp = Ytemp = c()
    for(i in 1:length(NoProb)) Xtemp = c(Xtemp,rep(c(NoProb[i]-Step/2,NoProb[i]+Step/2),each=2))
    for(i in 1:length(NoCount)) Ytemp = c(Ytemp,0,NoCount[i],NoCount[i],0)
      lines(Xtemp,Ytemp,lwd=3,col=SCOL)
    if(Line_Up == T) lines(c(0,NoProb,1),c(0,NoCount,0),lwd=2,col=SCOL)
  }
  
  ##Legend
  points((k+2.3)/10,YLIM*0.75,pch=20,col=SCOL,cex=6)
  text((k+2.3)/10,YLIM*0.755,labels=N[k],col=adjustcolor('white',alpha.f=0.8))
  
  ##Theorem Curve
  if(Show_Theorem_Curve == T){
    MU = P
    SD = sqrt(P*(1-P)/N[k])
    curve(dnorm(x,MU,SD)*Step*ST,col=TCOL,lwd=2,add=T)
    points((k+2.3)/10,YLIM*0.65,pch=20,col=TCOL,cex=6)
    text((k+2.3)/10,YLIM*0.655,labels=N[k],col=adjustcolor('white',alpha.f=0.8))
  }
  
}
#####End Here#####
