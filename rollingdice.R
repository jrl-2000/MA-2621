#this part creates vectors to store wether a dice roll is in the set with a 1, or isn't with a 0
fb<-rep(0,1000)
fc<-rep(0,1000)
fbandc<-rep(0,1000)
fborc<-rep(0,1000)

#repeat this 1000 times, indexing by i
for(i in 1:1000){
  dice<-sample(1:6,1) #roll a dice (sample a number between 1 and 6)
  if(dice%%2==0){fb[i]<-1} #if even, put in B
  if(dice>3){fc[i]<-1} #if bigger than 3, put in C
  fbandc[i]<-fb[i]*fc[i] #if in both (both values are 1), put in intersection
  if(fb[i]+fc[i]>0){fborc[i]<-1} #if in either (sum>0 implies at least one is 1), put in union
}

#cumsum=adds all values up until threshold. Dividing by trial yields relative frequencies
plot(1:1000,cumsum(fb)/1:1000,type = "l", main = "relative frequencies of results", xlab = "Trial", ylab = "Frequency", ylim = c(0,1))
lines(1:1000,cumsum(fc)/1:1000,col="red")
lines(1:1000,cumsum(fbandc)/1:1000,col="blue")
lines(1:1000,cumsum(fborc)/1:1000,col="green")