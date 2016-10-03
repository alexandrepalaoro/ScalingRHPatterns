####				                         ####
# 				                            	#
# Cumulative Assessment with Power Laws #
# By Alexandre V. Palaoro & Mark Briffa #
#				                              	#
#   Special thanks to Danilo G. Muniz   #
#   for helping with the coding and	    #
#   discussions                      		#
####				                         ####


## Power Law Scaling
# x = body size
# off = offensive RHP
# def = damage endurance
# stam = stamina

nonL.duration = function(x, off, def, stam) {
  d = (x^stam)+(x^def)-(x^off)
  return(ifelse(d>0, yes=d, no=0)) 
}

## Linear Scaling
# x = body size
# off = offensive RHP
# def = damage endurance
# stam = stamina

L.duration = function(x, off, def, stam) {
  d = (x*stam)+(x*def)-(x*off)
  return(ifelse(d>0, yes=d, no=0)) 
}

##                                                        ##
# The modeling can be done by using the curve() function.  #
# curve() calculates several values for the variable "x"   #
# at the same time. Thus, it is perfect for our modeling.  #
# Here, I will leave the code used for Figure 2 in the     #
# paper, but please tinker with the values as you see fit. #
##                                                        ##

# Opens a side window to visualize the graphs better
x11()

# Ploting all of them at once
par(mfrow=c(2,3),las=1)

## curve() needs an x to calculate a range of values. Thus, 'x' will be our body size
## and you can just add the other scaling exponents and slopes as you see fit.
## "from" and "to" will be the range of your body size values. I will start with the 
## linear predictions

curve(L.duration(x = x, def = 1, off = 1, stam = 1), from=1, to=50, xlab="Body size",ylab="Duration",ylim=c(0,50))
curve(L.duration(x = x, def = 1.5, off = 1, stam = 1), from=1, to=50, xlab="Body size",ylab="Duration",add=T)
curve(L.duration(x = x, def = 2, off = 1, stam = 1), from=1, to=50, xlab="Body size",ylab="Duration",add=T)

curve(L.duration(x = x, def = 1, off = 1, stam = 1), from=1, to=50, xlab="Body size",ylab="Duration", main = "Linear scaling",ylim=c(0,50))
curve(L.duration(x = x, def = 1, off = 1.2, stam = 1), from=1, to=50,add=T)
curve(L.duration(x = x, def = 1, off = 1.5, stam = 1), from=1, to=50,add=T)
curve(L.duration(x = x, def = 1, off = 1.8, stam = 1), from=1, to=50,add=T)

curve(L.duration(x = x, def = 0.7, off = 1, stam = 1), from=1, to=50, xlab="Body size",ylab="Duration",ylim=c(0,50))
curve(L.duration(x = x, def = 0.7, off = 1.3, stam = 1), from=1, to=50,add=T)
curve(L.duration(x = x, def = 0.7, off = 1.5, stam = 1), from=1, to=50,add=T)
curve(L.duration(x = x, def = 0.7, off = 1.65, stam = 1), from=1, to=50,add=T)

#The non-linear scalings are next.

curve(nonL.duration(x = x, def = 1.5, off = 1, stam = 1), from=1, to=50, xlab="Body size",ylab="Duration",ylim=c(0,50))
curve(nonL.duration(x = x, def = 1.3, off = 1, stam = 1), from=1, to=50,add=T)
curve(nonL.duration(x = x, def = 1.8, off = 1, stam = 1), from=1, to=50,add=T)

curve(nonL.duration(x = x, def = 1, off = 1, stam = 1), from=1, to=50, xlab="Body size",ylab="Duration", main = "Power law scaling",ylim=c(0,50))
curve(nonL.duration(x = x, def = 1, off = 1.1, stam = 1), from=1, to=50, add=T)
curve(nonL.duration(x = x, def = 1, off = 1.15, stam = 1), from=1, to=50,add=T)
curve(nonL.duration(x = x, def = 1, off = 1.2, stam = 1), from=1, to=50, add=T)

curve(nonL.duration(x = x, def = 0.7, off = 1, stam = 1), from=1, to=50, xlab="Body size",ylab="Duration",ylim=c(0,50))
curve(nonL.duration(x = x, def = 0.7, off = 1.05, stam = 1), from=1, to=50, add=T)
curve(nonL.duration(x = x, def = 0.7, off = 1.1, stam = 1), from=1, to=50, add=T)


# Now, let's get predictive:

par(mfrow=c(1,3),las=1)

curve(duration(x=x, def=1.5, off=1), from=1, to=30, main="Offense < Damage endurance",
      xlab="Body size",ylab="Duration")

curve(duration(x=x, def=1, off=1), from=1, to=30, main="Offense = Damage endurance",
      xlab="Body size",ylab="Duration")

curve(duration(x=x, def=1, off=1.2), from=1, to=30, main ="Offense > Damage endurance",
      xlab="Body size",ylab="Duration")

# Here we are predicting that once size-matched contests escalate to physical contact, contests will ensue as follows:
# 1 - Species that have a higher damage endurance than offensive capacity will show an exponential increase in 
#     contest duration (e.g. arthropods);
# 2 - Species that have similar damage endurance and offensive capcity will show a linear increase (e.g. arthropods);
# 3 - Species that have a higher offensive capacity than damage endurance will have short fights with a decrease in contest 
#     duration as the individual grows (e.g. ungulates, sea anemones).
#
# Done :)