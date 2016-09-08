# Cleaning up the workspace
rm(list=ls())

# Setting the random seed so this code runs smoothly as is. If you change the seed 
# remember to check how many negative values were drawn from the random distribution.

set.seed(32)

# Now we can generate the random population 

distr<-rnorm(200,mean=50,sd=20)
hist(distr)

# Sorting the individuals within the distribution - from the smallest to the largest.

s.distr<-sort(distr)

# Drawing the 30 smallest and the 30 largest individuals from the population generated.

lower<-head(s.distr,n=31) #Here I drew 31 individuals because there is a negative value
upper<-tail(s.distr,n=30)

# Removing the negative values and checking if we have 30 individuals in each subset

lower<-lower[lower>0] 
length(lower)
length(upper)

# Adding the focal individuals to the same vector

bodyS<-c(upper,lower)

# Generating the size-matched opponents

body.list<-c(bodyS,(bodyS*0.75))

# Generating the groups

groups<-as.factor(c(rep("LargeF",30),rep("SmallF",30),rep("LargeO",30),rep("SmallO",30)))

# Adding both vectors to the same data.frame

matchedPairs<-data.frame(v1=groups,v2=body.list)
colnames(matchedPairs)<-c("groups","bodysize")
head(matchedPairs)

# Now that we have our focal individuals and their size-matched opponents, we can proceed
# and calculate the RHP components.

##########                                                                     ##########                        
# The function below will calculate any RHP component you might want with any slope.    #
# We tailored the function to calculate one RHP component per time, but it could easily #
# be tinkered with to calculate all RHP components at the same time. For instance,      #
# you could add more "b"s to calculate all at once. Since our analysis only requires    #
# the calculation of one RHP component (i.e. the offensive capacity), we only calculate #
# that specific component.                                                              #
##########                                                                     ##########

########################################################################################
# Now, the arguments:                                                                  #
# 1."dataframe" is the name of the dataframe containing the body size values. In this  #
#    code it is matchedPairs. But you could name it however you liked it.              #
#                                                                                      #
# 2. "col.bodysize" is the number of the column within the data.frame that contains    #
#    the body size values. In our case it is the second column.                        #
#                                                                                      #
# 3. "b" is the slope or scaling exponent. This can assume any value, but we keep our  #
#    simulations to realistic values (i.e. between 0.5 and 2, Voje (2016) - American   #
#    Naturalist).                                                                      #
########################################################################################

slopes<-function(dataframe,col.bodysize,b){
  linear<-(1+(dataframe[,col.bodysize]*b))
  nonlinear<-(1*(dataframe[,col.bodysize]^b))
  RHPmatched<-dataframe
  RHPmatched$linearOC<-linear
  RHPmatched$nonlinearOC<-nonlinear
  return(RHPmatched)
    }

# Calculating the offensive capacity for three slopes

slope1.1<-slopes(matchedPairs,2,1.1)

slope1.12<-slopes(matchedPairs,2,1.12)

slope1.2<-slopes(matchedPairs,2,1.2)

# Now we have to subtract the opponent's offensive capacity from the focal's 
# offensive capacity. That way, we can have an idea of how RHP is scaling 
# between groups. This could be done easily with a for(), but we will do this
# manually so our trail of thought becomes more apparent. 

SmLin1.1<-slope1.1$linearOC[slope1.1$groups=="SmallF"]-slope1.1$linearOC[slope1.1$groups=="SmallO"]
SmLin1.12<-slope1.12$linearOC[slope1.12$groups=="SmallF"]-slope1.12$linearOC[slope1.12$groups=="SmallO"]
SmLin1.2<-slope1.2$linearOC[slope1.2$groups=="SmallF"]-slope1.2$linearOC[slope1.2$groups=="SmallO"]

SmNonL1.1<-slope1.1$nonlinearOC[slope1.1$groups=="SmallF"]-slope1.1$nonlinearOC[slope1.1$groups=="SmallO"]
SmNonL1.12<-slope1.12$nonlinearOC[slope1.12$groups=="SmallF"]-slope1.12$nonlinearOC[slope1.12$groups=="SmallO"]
SmNonL1.2<-slope1.2$nonlinearOC[slope1.2$groups=="SmallF"]-slope1.2$nonlinearOC[slope1.2$groups=="SmallO"]

LgLin1.1<-slope1.1$linearOC[slope1.1$groups=="LargeF"]-slope1.1$linearOC[slope1.1$groups=="LargeO"]
LgLin1.12<-slope1.12$linearOC[slope1.12$groups=="LargeF"]-slope1.12$linearOC[slope1.12$groups=="LargeO"]
LgLin1.2<-slope1.2$linearOC[slope1.2$groups=="LargeF"]-slope1.2$linearOC[slope1.2$groups=="LargeO"]

LgNonL1.1<-slope1.1$nonlinearOC[slope1.1$groups=="LargeF"]-slope1.1$nonlinearOC[slope1.1$groups=="LargeO"]
LgNonL1.12<-slope1.12$nonlinearOC[slope1.12$groups=="LargeF"]-slope1.12$nonlinearOC[slope1.12$groups=="LargeO"]
LgNonL1.2<-slope1.2$nonlinearOC[slope1.2$groups=="LargeF"]-slope1.2$nonlinearOC[slope1.2$groups=="LargeO"]

# Clumping all differences of the same slope in one single vector

difOC1.1<-c(SmLin1.1,SmNonL1.1,LgLin1.1,LgNonL1.1)
difOC1.12<-c(SmLin1.12,SmNonL1.12,LgLin1.12,LgNonL1.12)
difOC1.2<-c(SmLin1.2,SmNonL1.2,LgLin1.2,LgNonL1.2)

# Since all differences are calculated, we need to generate the factors once again. 

scaling<-as.factor(c(rep("Linear",30),rep("Nonlinear",30),rep("Linear",30),rep("Nonlinear",30)))
size<-as.factor(c(rep("Small",60),rep("Large",60)))

# Making the data frames 

oc1.1<-data.frame(v1=scaling,v2=size,v3=difOC1.1,v4=interaction(scaling,size))
colnames(oc1.1)<-c("Scaling","PairSize","difoc","interact")

oc1.12<-data.frame(v1=scaling,v2=size,v3=difOC1.12,v4=interaction(scaling,size))
colnames(oc1.12)<-c("Scaling","PairSize","difoc","interact")

oc1.2<-data.frame(v1=scaling,v2=size,v3=difOC1.2,v4=interaction(scaling,size))
colnames(oc1.2)<-c("Scaling","PairSize","difoc","interact")

# To plot the values in a more intuitive order, we need to reorder our
# factors in the interaction plot.

oc1.1<-oc1.1[order(oc1.1$Scaling,oc1.1$PairSize),]
oc1.1$interact<-factor(oc1.1$interact,unique(oc1.1$interact))

oc1.12<-oc1.12[order(oc1.12$Scaling,oc1.12$PairSize),]
oc1.12$interact<-factor(oc1.12$interact,unique(oc1.12$interact))

oc1.2<-oc1.2[order(oc1.2$Scaling,oc1.2$PairSize),]
oc1.2$interact<-factor(oc1.2$interact,unique(oc1.2$interact))


# Let's do some statistics to be sure that the groups really differ.

aovOC1.1<-aov(difoc~Scaling*PairSize,data=oc1.1)
summary(aovOC1.1)
TukeyHSD(aovOC1.1)

aovOC1.12<-aov(difoc~Scaling*PairSize,data=oc1.12)
summary(aovOC1.12)
TukeyHSD(aovOC1.12)

aovOC1.2<-aov(difoc~Scaling*PairSize,data=oc1.2)
summary(aovOC1.2)
TukeyHSD(aovOC1.2)

# Plotting the interaction between the variables, so we can visually express
# what is going on with our data.

interaction.plot(oc1.1$Scaling,oc1.1$PairSize,oc1.1$difoc,ylim=c(0,60),
                 ylab="Mean of difference in offensive capacity",xlab="Scaling",trace.label = "")
interaction.plot(oc1.12$Scaling,oc1.12$PairSize,oc1.12$difoc,ylim=c(0,60),
                 ylab="Mean of difference in offensive capacity",xlab="Scaling",trace.label = "")
interaction.plot(oc1.2$Scaling,oc1.2$PairSize,oc1.2$difoc,ylim=c(0,60),
                 ylab="Mean of difference in offensive capacity",xlab="Scaling",trace.label = "")

# This is a function to calculate variances for all groups at once. 
# This escalates better than calculating all by hand

calc.var<-function(data,factor1,factor2){
  groups.interact<-as.factor(interaction(factor1,factor2))
  framing<-data.frame(v1=data,v2=groups.interact)
  colnames(framing)<-c("data","groups.interact")
  var.calc<-{}
  for(i in 1:length(levels(groups.interact))){
    var.calc[i]<-sd(framing$data[as.numeric(framing$groups.interact)==i])
  }
  frame.var<-data.frame(v1=var.calc,v2=levels(groups.interact))
  return(frame.var)
}

var.oc1.1<-calc.var(oc1.1$difoc,oc1.1$Scaling,oc1.1$PairSize)
var.oc1.12<-calc.var(oc1.12$difoc,oc1.12$Scaling,oc1.12$PairSize)
var.oc1.2<-calc.var(oc1.2$difoc,oc1.2$Scaling,oc1.2$PairSize)


# Finally, let's plot the groups
# In case you want to save the files on your computer just remove the "#"
# on the tiff() and dev.off() function.

#tiff(file="dif-oc.tiff",units="mm",width=200,height=120,res=600,
#     compression="lzw")
par(mfrow=c(1,3))
plot(difoc~interact,data=oc1.1,las=1,axes=F,ylab="Difference in offensive capacity (Focal - Opponent)",ylim=c(0,60),
     col=c("darkgray","white","darkgray","white"),at=c(1,2,4,5),xlab="Scaling")
axis(1,at=c(1.5,4.5),labels=c("Linear","Non-linear"))
axis(2,las=1)
box()
text(5.2,59,'(a)')
legend("topleft",legend=c("Small","Large"),fill=c("white","darkgray"),
       bty='n',cex=1.1)
text(1,24.78,round(var.oc1.1[1,1],3))
text(2.01,10,round(var.oc1.1[2,1],3))
text(4,37.5,round(var.oc1.1[3,1],3))
text(5.02,13.2,round(var.oc1.1[4,1],3))

plot(difoc~interact,data=oc1.12,las=1,axes=F,ylab="",ylim=c(0,60),yaxt='n',ann=F,
     col=c("darkgray","white","darkgray","white"),at=c(1,2,4,5),xlab="Scaling")
axis(1,at=c(1.5,4.5),labels=c("Linear","Non-linear"))
axis(2,las=1,labels=F)
box()
text(5.2,59,'(b)')
text(1.02,25.2,round(var.oc1.12[1,1],3))
text(2.02,10,round(var.oc1.12[2,1],3))
text(4,41.5,round(var.oc1.12[3,1],3))
text(5,14.2,round(var.oc1.12[4,1],3))

plot(difoc~interact,data=oc1.2,las=1,axes=F,yaxt="n",ann=F,ylim=c(0,60),ylab='',
     col=c("darkgray","white","darkgray","white"),at=c(1,2,4,5),xlab="Scaling")
axis(1,at=c(1.5,4.5),labels=c("Linear","Non-linear"))
axis(2,las=1,labels=F)
box()
text(5.2,59,"(c)")
text(0.99,27,round(var.oc1.2[1,1],3))
text(2.02,10.7,round(var.oc1.2[2,1],3))
text(4.02,44.3,round(var.oc1.2[3,1],3))
text(5.02,19.5,round(var.oc1.2[4,1],3))
  
#dev.off()
