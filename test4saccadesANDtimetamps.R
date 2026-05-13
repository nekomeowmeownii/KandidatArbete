#inte relevant till rapporten på något sätt men en extra fil för att jämföra tiden som grupperna tittar på tavlan, tiden på saccades osv.

load("") #copy paste your path for data
library("spatstat")
library("ggplot2")
library("jpeg")
library("viridis")
library("plot3D") 
library("sets")
rm(list = ls()) #removes stuff from global environment if you want to clear shit
dev.off(dev.list()["RStudioGD"]) #removes all existing plots 
monet_window <- owin(c(5, 1017), c(0, 767))

curr_id <- NULL

dataA <- subset(Monet, inpic == 1 & duration >= 100) #everyone
dataNN <- subset(Monet, class == 2 & inpic == 1 & duration >= 100 & timestamp <= 45000) #non-novices
dataN <- subset(Monet, class == 1 & inpic == 1 & duration >= 100 & timestamp <= 45000) #novices

dataPresacNN <- na.omit(dataNN)
dataPresacN <- na.omit(dataN)

totalPresacNN <- sum(dataPresacNN$presacinpic) #sums the times of saccades
totalPresacN <- sum(dataPresacN$presacinpic)

meanPresacNN <- mean(dataPresacNN$presacinpic) #take the mean wrt amount of fixations
meanPresacN <- mean(dataPresacN$presacinpic)

#### ---------------- essentially the code above proved that the non-novices had less fixations, but a higher sum of saccade duration, but also a higher mean because of that. 

t.test(dataPresacNN$presacinpic, dataPresacN$presacinpic) #with a p less than 4.6e-05, that essentially just means that we can reject the null hypothesis which is that they are of no difference, the mean that is. We instead embrace the fact that they are INDEED different.


# kolla från 45 sek till 90 sek

dataA2 <- subset(Monet, inpic == 1 & duration >= 100) #everyone
dataNN2 <- subset(Monet, class == 2 & inpic == 1 & duration >= 100 & 90000 >= timestamp & timestamp >= 45000) #non-novices
dataN2 <- subset(Monet, class == 1 & inpic == 1 & duration >= 100 & 90000 >= timestamp & timestamp >= 45000) #novices

dataPresacNN2 <- na.omit(dataNN2)
dataPresacN2 <- na.omit(dataN2)

totalPresacNN2 <- sum(dataPresacNN2$presacinpic) #sums the times of saccades
totalPresacN2 <- sum(dataPresacN2$presacinpic)

meanPresacNN2 <- mean(dataPresacNN2$presacinpic) #take the mean wrt amount of fixations
meanPresacN2 <- mean(dataPresacN2$presacinpic)

t.test(dataPresacNN2$presacinpic, dataPresacN2$presacinpic)

# kolla från 90 sek till 135 sek

dataA3 <- subset(Monet, inpic == 1 & duration >= 100) #everyone
dataNN3 <- subset(Monet, class == 2 & inpic == 1 & duration >= 100 & 135000 >= timestamp & timestamp >= 90000) #non-novices
dataN3 <- subset(Monet, class == 1 & inpic == 1 & duration >= 100 & 135000 >= timestamp & timestamp >= 90000) #novices

dataPresacNN3 <- na.omit(dataNN3)
dataPresacN3 <- na.omit(dataN3)

totalPresacNN3 <- sum(dataPresacNN3$presacinpic) #sums the times of saccades
totalPresacN3 <- sum(dataPresacN3$presacinpic)

meanPresacNN3 <- mean(dataPresacNN3$presacinpic) #take the mean wrt amount of fixations
meanPresacN3 <- mean(dataPresacN3$presacinpic)

t.test(dataPresacNN3$presacinpic, dataPresacN3$presacinpic)

# kolla från 135 sek till 180 sek

dataA4 <- subset(Monet, inpic == 1 & duration >= 100) #everyone
dataNN4 <- subset(Monet, class == 2 & inpic == 1 & duration >= 100 & 180000 >= timestamp & timestamp >= 135000) #non-novices
dataN4 <- subset(Monet, class == 1 & inpic == 1 & duration >= 100 & 180000 >= timestamp & timestamp >= 135000) #novices

dataPresacNN4 <- na.omit(dataNN4)
dataPresacN4 <- na.omit(dataN4)

totalPresacNN4 <- sum(dataPresacNN4$presacinpic) #sums the times of saccades
totalPresacN4 <- sum(dataPresacN4$presacinpic)

meanPresacNN4 <- mean(dataPresacNN4$presacinpic) #take the mean wrt amount of fixations
meanPresacN4 <- mean(dataPresacN4$presacinpic)

t.test(dataPresacNN4$presacinpic, dataPresacN4$presacinpic)


#### samla ihop allt, och plotta meanet för båda grupper.

totDatNN <- na.omit(subset(Monet, class == 2 & inpic == 1 & duration >= 100 & 180000 >= timestamp))

totDatN <- na.omit(subset(Monet, class == 1 & inpic == 1 & duration >= 100 & 180000 >= timestamp))

meanDatNN <- c(meanPresacNN, meanPresacNN2, meanPresacNN3, meanPresacNN4)
meanDatN <- c(meanPresacN, meanPresacN2, meanPresacN3, meanPresacN4)
timeS <- c(45/2, 45+45/2, 90+45/2, 135+45/2)

plot(x=timeS, y=meanDatNN, col="red", type="b", xlim = c(10, 170), ylim = c(55,75), xlab="tid", ylab="mean values")
lines(x=timeS, y=meanDatN, col="blue", type="b")
legend("topright", legend=c("NN", "N"), col=c("red", "blue"), lwd=1)

######################################################################################


#går in på duration only nu. basically samma som ovan men jag fokuserar mer på att kolla på duration. börjar med 0-45

#0-45

datNN <- subset(Monet, class == 2 & inpic == 1 & timestamp <= 45000)
datN <- subset(Monet, class == 1 & inpic == 1 & timestamp <= 45000)

totalDurNN <- sum(datNN$duration)/1000
totalDurN <- sum(datN$duration)/1000

meanDurNN <- mean(datNN$duration)
meanDurN <- mean(datN$duration)

t.test(datNN$duration, datN$duration)

#45-90

datNN2 <- subset(Monet, class == 2 & inpic == 1 & timestamp >= 45000 & timestamp <= 90000)
datN2 <- subset(Monet, class == 1 & inpic == 1 & timestamp >= 45000 & timestamp <= 90000)

totalDurNN2 <- sum(datNN2$duration)/1000
totalDurN2 <- sum(datN2$duration)/1000

meanDurNN2 <- mean(datNN2$duration)
meanDurN2 <- mean(datN2$duration)

t.test(datNN2$duration, datN2$duration) #intressant här p-värde ligger på 0.06468

#90-135

datNN3 <- subset(Monet, class == 2 & inpic == 1 & timestamp >= 90000 & timestamp <= 135000)
datN3 <- subset(Monet, class == 1 & inpic == 1 & timestamp >= 90000 & timestamp <= 135000)

totalDurNN3 <- sum(datNN3$duration)/1000
totalDurN3 <- sum(datN3$duration)/1000

meanDurNN3 <- mean(datNN3$duration)
meanDurN3 <- mean(datN3$duration)

t.test(datNN3$duration, datN3$duration) #p-värde ligger på 0.001682

#135-180
datNN4 <- subset(Monet, class == 2 & inpic == 1 & timestamp >= 135000 & timestamp <= 180000)
datN4 <- subset(Monet, class == 1 & inpic == 1 & timestamp >= 135000 & timestamp <= 180000)

totalDurNN4 <- sum(datNN4$duration)/1000
totalDurN4 <- sum(datN4$duration)/1000

meanDurNN4 <- mean(datNN4$duration)
meanDurN4 <- mean(datN4$duration)

t.test(datNN4$duration, datN4$duration)

