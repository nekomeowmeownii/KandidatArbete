load("Z:\\Chalmers\\KANDIDAT\\programmering\\Monet.RData") #copy paste your path for data
library("spatstat")
library("ggplot2")
library("jpeg")
library("viridis")
library("plot3D") 
library("sets")
rm(list = ls()) #removes stuff from global environment if you want to clear shit
dev.off(dev.list()["RStudioGD"]) #removes all existing plots 
monet_window <- owin(c(5, 1017), c(0, 767))
image <- readJPEG("Z:\\Chalmers\\KANDIDAT\\programmering\\Monet.JPG") #copy paste your path for image





curr_id <- 19

#data from first 45 seconds
data451 <- subset(Monet, id == curr_id & inpic == 1 & dur40 == 1 & timestamp <= 45000)
ppp451 <- ppp(data451$locX, y = data451$locY, window=monet_window)
L451 <- Linhom(ppp451, correction="border")


#data from the last 45 seconds
data454 <- subset(Monet, id == curr_id & inpic == 1 & dur40 == 1 & timestamp <= 180000 & timestamp >= 135000)
ppp454 <- ppp(data454$locX, y = data454$locY, window=monet_window)
L454 <- Linhom(ppp454, correction="border")



plot(L451$border, type="l", col="red", xlab="r", ylab="L(r)", ylim=c(0, 250))
lines(L454$border, type="l", col="blue")
legend("topleft", legend = c("första 45s", "sista 45s"), col=c("red","blue"), lwd=2)

#integration for the first 45 and the last 45. här användes abs. consider using the one tailed test.

integral451454 <- integral((L451 - L454)^2)
pooled451454 <- subset(Monet, id == curr_id & inpic == 1 & dur40 == 1 & (timestamp <= 45000 | (timestamp >= 135000 & timestamp <= 180000)))

n451 <- nrow(data451)
n454 <- nrow(data454)
n451454 <- nrow(pooled451454)

  #data splitting
simuleringar <- 999
res_array <- array(dim = simuleringar)
for (i in 1:simuleringar) {
  random_sample <- sample(n451454, n451)
  g1 <- pooled451454[random_sample, ]
  g2 <- pooled451454[-random_sample, ]
  ppp1 <- ppp(g1$locX, g1$locY, window=monet_window)
  ppp2 <- ppp(g2$locX, g2$locY, window=monet_window)
  L1 <- Linhom(ppp1, correction = "border")
  L2 <- Linhom(ppp2, correction = "border")
  res_array[i] <- integral((L1 - L2)^2)["border"]
}

#gör histogrammet nu. och sedan skaffa ett test statistika för att jämföra med histogrammet och gör beslut om resultatet.
sorted_ra <- sort(res_array)
higher <- as.integer(0.95*nrow(res_array))


if (integral451454["border"] >= sorted_ra[higher]) {
  hist(res_array,main=paste("histogram för testsubjekt", curr_id), xlab="resultat från integration", xlim=c(0,integral451454["border"] + 0.2*integral451454["border"]), nclass=15)
} else {
  hist(res_array,main=paste("histogram för testsubjekt", curr_id), xlab="resultat från integration", xlim=c(0,sorted_ra[nrow(sorted_ra)]+100+abs(sorted_ra[nrow(sorted_ra)]*0.1)), nclass=15)
}


abline(v=integral451454["border"], col="green")
abline(v=sorted_ra[higher], col="red")



################# for loop for all students, 45 seconds, abs. 
pdf("histograms.pdf", width=8, height=40)
par(mfrow = c(10, 2))
for (j in 1:20) {
  curr_id <- j
  
  #data from first 45 seconds
  data451 <- subset(Monet, id == curr_id & inpic == 1 & dur40 == 1 & timestamp <= 45000)
  ppp451 <- ppp(data451$locX, y = data451$locY, window=monet_window)
  L451 <- Linhom(ppp451, correction="border")
  
  
  #data from the last 45 seconds
  data454 <- subset(Monet, id == curr_id & inpic == 1 & dur40 == 1 & timestamp <= 180000 & timestamp >= 135000)
  ppp454 <- ppp(data454$locX, y = data454$locY, window=monet_window)
  L454 <- Linhom(ppp454, correction="border")
  
  
  #integration for the first 45 and the last 45. här användes abs. consider using the one tailed test.
  
  integral451454 <- integral(abs(L451 - L454))
  pooled451454 <- subset(Monet, id == curr_id & inpic == 1 & dur40 == 1 & (timestamp <= 45000 | (timestamp >= 135000 & timestamp <= 180000)))
  
  n451 <- nrow(data451)
  n454 <- nrow(data454)
  n451454 <- nrow(pooled451454)
  
  #data splitting
  simuleringar <- 999
  res_array <- array(dim = simuleringar)
  for (i in 1:simuleringar) {
    random_sample <- sample(n451454, n451)
    g1 <- pooled451454[random_sample, ]
    g2 <- pooled451454[-random_sample, ]
    ppp1 <- ppp(g1$locX, g1$locY, window=monet_window)
    ppp2 <- ppp(g2$locX, g2$locY, window=monet_window)
    L1 <- Linhom(ppp1, correction = "border")
    L2 <- Linhom(ppp2, correction = "border")
    res_array[i] <- integral(abs(L1 - L2))["border"]
  }
  
  #gör histogrammet nu. och sedan skaffa ett test statistika för att jämföra med histogrammet och gör beslut om resultatet.
  sorted_ra <- sort(res_array)
  higher <- as.integer(0.95*nrow(res_array))
  
  
  if (integral451454["border"] >= sorted_ra[higher]) {
    hist(res_array,main=paste("histogram för testperson", curr_id), xlab="resultat från integration", xlim=c(0,integral451454["border"] + 0.2*integral451454["border"]), nclass=15)
  } else {
    hist(res_array,main=paste("histogram för testperson", curr_id), xlab="resultat från integration", xlim=c(0,sorted_ra[nrow(sorted_ra)]+100+abs(sorted_ra[nrow(sorted_ra)]*0.1)), nclass=15)
  }
  abline(v=integral451454["border"], col="green")
  abline(v=sorted_ra[higher], col="red")
}
dev.off()







































#### samma som data ovan, men det är two tailed och icke använder abs
####
####
####
####

curr_id <- 3

#data from first 45 seconds
data451 <- subset(Monet, id == curr_id & inpic == 1 & dur40 == 1 & timestamp <= 45000)
ppp451 <- ppp(data451$locX, y = data451$locY, window=monet_window)
L451 <- Linhom(ppp451, correction="border")


#data from the last 45 seconds
data454 <- subset(Monet, id == curr_id & inpic == 1 & dur40 == 1 & timestamp <= 180000 & timestamp >= 135000)
ppp454 <- ppp(data454$locX, y = data454$locY, window=monet_window)
L454 <- Linhom(ppp454, correction="border")


plot(L451$border, type="l", col="red")
lines(L454$border, type="l", col="blue")
legend("topright", legend = c("första 45", "sista 45"), col=c("red","blue"), lwd=2)

#integration for the first 45 and the last 45. här användes abs. consider using the one tailed test.

integral451454 <- integral(L451 - L454)
pooled451454 <- subset(Monet, id == curr_id & inpic == 1 & dur40 == 1 & (timestamp <= 45000 | (timestamp >= 135000 & timestamp <= 180000)))

n451 <- nrow(data451)
n454 <- nrow(data454)
n451454 <- nrow(pooled451454)

#data splitting
simuleringar <- 999
res_array <- array(dim = simuleringar)
for (i in 1:simuleringar) {
  random_sample <- sample(n451454, n451)
  g1 <- pooled451454[random_sample, ]
  g2 <- pooled451454[-random_sample, ]
  ppp1 <- ppp(g1$locX, g1$locY, window=monet_window)
  ppp2 <- ppp(g2$locX, g2$locY, window=monet_window)
  L1 <- Linhom(ppp1, correction = "border")
  L2 <- Linhom(ppp2, correction = "border")
  res_array[i] <- integral(L1 - L2)["border"]
}

#gör histogrammet nu. och sedan skaffa ett test statistika för att jämföra med histogrammet och gör beslut om resultatet. 
sorted_ra <- sort(res_array)
lower <- as.integer(0.025*nrow(res_array))
higher <- as.integer(0.975*nrow(res_array))


if (integral451454["border"] >= sorted_ra[higher]) {
  hist(res_array,main=paste("histogram för testsubjekt", curr_id), xlab="resultat från integration", xlim=c(sorted_ra[1]+sorted_ra[1]*0.2,integral451454["border"] + 0.2*integral451454["border"]), nclass=15)
} else if ( integral451454["border"] <= sorted_ra[lower]) {
  hist(res_array,main=paste("histogram för testsubjekt", curr_id), xlab="resultat från integration", xlim=c(integral451454["border"] + 0.2*integral451454["border"],sorted_ra[nrow(sorted_ra)] + 0.1*sorted_ra[nrow(sorted_ra)]), nclass=15)
} else {
  hist(res_array,main=paste("histogram för testsubjekt", curr_id), xlab="resultat från integration", xlim=c(sorted_ra[1]-100-abs(sorted_ra[1]*0.1),sorted_ra[nrow(sorted_ra)]+100+abs(sorted_ra[nrow(sorted_ra)]*0.1)), nclass=15)
}


abline(v=integral451454["border"], col="green")
abline(v=sorted_ra[lower], col="red")
abline(v=sorted_ra[higher], col="red")




p4 <- ggplot(data454, mapping=aes(x=locX, y=locY))
p4 +
      annotation_raster(image,xmin=5,xmax=1017,ymin=0,ymax=767) +
       geom_point(mapping=aes(x=locX, y=locY), data=data454, color="white")+
       coord_fixed(xlim = c(5, 1017), ylim = c(0, 767)) +
       xlab("x") +
       ylab("y") +
       ggtitle("testsubjekt3")

p1 <- ggplot(data451, mapping=aes(x=locX, y=locY))
p1 +
  annotation_raster(image,xmin=5,xmax=1017,ymin=0,ymax=767) +
  geom_point(mapping=aes(x=locX, y=locY), data=data451, color="white")+
  coord_fixed(xlim = c(5, 1017), ylim = c(0, 767)) +
  xlab("x") +
  ylab("y") +
  ggtitle("testsubjekt3")






#spara för all evighet
# curr_id <- 3
# 
# #data from first 45 seconds
# data451 <- subset(Monet, id == curr_id & inpic == 1 & dur40 == 1 & timestamp <= 45000)
# ppp451 <- ppp(data451$locX, y = data451$locY, window=monet_window)
# L451 <- Linhom(ppp451, correction="border")
# 
# 
# #data from the last 45 seconds
# data454 <- subset(Monet, id == curr_id & inpic == 1 & dur40 == 1 & timestamp <= 180000 & timestamp >= 135000)
# ppp454 <- ppp(data454$locX, y = data454$locY, window=monet_window)
# L454 <- Linhom(ppp454, correction="border")
# 
# 
# 
# 
# #integration for the first 45 and the last 45. här användes abs. consider using the one tailed test.
# 
# integral451454 <- integral(abs(L451 - L454))
# pooled451454 <- subset(Monet, id == curr_id & inpic == 1 & dur40 == 1 & (timestamp <= 45000 | (timestamp >= 135000 & timestamp <= 180000)))
# 
# n451 <- nrow(data451)
# n454 <- nrow(data454)
# n451454 <- nrow(pooled451454)
# 
# #data splitting
# simuleringar <- 999
# res_array <- array(dim = simuleringar)
# for (i in 1:simuleringar) {
#   random_sample <- sample(n451454, n451)
#   g1 <- pooled451454[random_sample, ]
#   g2 <- pooled451454[-random_sample, ]
#   ppp1 <- ppp(g1$locX, g1$locY, window=monet_window)
#   ppp2 <- ppp(g2$locX, g2$locY, window=monet_window)
#   L1 <- Linhom(ppp1, correction = "border")
#   L2 <- Linhom(ppp2, correction = "border")
#   res_array[i] <- integral(abs(L1 - L2))["border"]
# }
# 
# #gör histogrammet nu. och sedan skaffa ett test statistika för att jämföra med histogrammet och gör beslut om resultatet.
# sorted_ra <- sort(res_array)
# lower <- as.integer(0.025*nrow(res_array))
# higher <- as.integer(0.975*nrow(res_array))
# 
# 
# if (integral451454["border"] >= sorted_ra[higher]) {
#   hist(res_array,main=paste("histogram för testsubjekt", curr_id), xlab="resultat från integration", xlim=c(0,integral451454["border"] + 0.2*integral451454["border"]), nclass=15)
# } else {
#   hist(res_array,main=paste("histogram för testsubjekt", curr_id), xlab="resultat från integration", xlim=c(0,sorted_ra[nrow(sorted_ra)]+100+abs(sorted_ra[nrow(sorted_ra)]*0.1)), nclass=15)
# }
# 
# 
# abline(v=integral451454["border"], col="green")
# abline(v=sorted_ra[lower], col="red")
# abline(v=sorted_ra[higher], col="red")






#################### runs every subject 5 times, to check for how many rejects.

students <- c(
  "1" = 0,
  "2" = 0,
  "3" = 0,
  "4" = 0,
  "5" = 0,
  "6" = 0,
  "7" = 0,
  "8" = 0,
  "9" = 0,
  "10" = 0,
  "11" = 0,
  "12" = 0,
  "13" = 0,
  "14" = 0,
  "15" = 0,
  "16" = 0,
  "17" = 0,
  "18" = 0,
  "19" = 0,
  "20" = 0
)
studs <- names(students)
for (s in studs) {
  for (j in 1:5) {
    curr_id <- as.integer(s)
    
    #data from first 45 seconds
    data451 <- subset(Monet, id == curr_id & inpic == 1 & dur40 == 1 & timestamp <= 45000)
    ppp451 <- ppp(data451$locX, y = data451$locY, window=monet_window)
    L451 <- Linhom(ppp451, correction="border")
    
    #data from the last 45 seconds
    data454 <- subset(Monet, id == curr_id & inpic == 1 & dur40 == 1 & timestamp <= 180000 & timestamp >= 135000)
    ppp454 <- ppp(data454$locX, y = data454$locY, window=monet_window)
    L454 <- Linhom(ppp454, correction="border")
    
    integral451454 <- integral(abs(L451 - L454))
    pooled451454 <- subset(Monet, id == curr_id & inpic == 1 & dur40 == 1 & (timestamp <= 45000 | (timestamp >= 135000 & timestamp <= 180000)))
    
    n451 <- nrow(data451)
    n454 <- nrow(data454)
    n451454 <- nrow(pooled451454)
    
    #data splitting
    simuleringar <- 999
    res_array <- array(dim = simuleringar)
    for (i in 1:simuleringar) {
      random_sample <- sample(n451454, n451)
      g1 <- pooled451454[random_sample, ]
      g2 <- pooled451454[-random_sample, ]
      ppp1 <- ppp(g1$locX, g1$locY, window=monet_window)
      ppp2 <- ppp(g2$locX, g2$locY, window=monet_window)
      L1 <- Linhom(ppp1, correction = "border")
      L2 <- Linhom(ppp2, correction = "border")
      res_array[i] <- integral(abs(L1 - L2))["border"]
    }
    
    sorted_ra <- sort(res_array)
    lower <- as.integer(0.025*nrow(res_array))
    higher <- as.integer(0.975*nrow(res_array))
    
    
    if (integral451454["border"] >= sorted_ra[higher] | integral451454["border"] <= sorted_ra[lower]) {
      students[s] = students[s] + 1
    } 
  }
}







#######################checks data for EXPERIENCE NONABS

#data from 1 group non-novices
dataNN <- subset(Monet, class == 2 & inpic == 1 & dur40 == 1)
#data from 1 group of novices
dataN <- subset(Monet, class == 1 & inpic == 1 & dur40 == 1)
#data from 1 group of all peeps
dataNNN <- subset(Monet, inpic == 1 & dur40 == 1)






pppNN <- ppp(x=dataNN$locX, y=dataNN$locY, window=monet_window)
pppN <- ppp(x=dataN$locX, y=dataN$locY, window=monet_window)

LNN <- Linhom(pppNN, correction="border")
LN <- Linhom(pppN, correction="border")

integralNNN <- integral(abs(LNN - LN))["border"]


plot(LNN$border, type="l", col="red", ylab="L(r)", xlab="r")
lines(LN$border, type="l", col="blue")
legend("topleft", legend = c("non-novice", "novice"), col=c("red","blue"), lwd=2)



antalNN <- nrow(dataNN)
antalN <- nrow(dataN)
antalNNN <- nrow(dataNNN)
simuleringar <- 999
res=array(dim=simuleringar)
for(i in 1:simuleringar) {
  samplad <- sample(antalNNN, antalNN)
  gNN <- dataNNN[samplad, ]
  gN <- dataNNN[-samplad,]
  pppSNN <- ppp(x=gNN$locX, y=gNN$locY, window=monet_window)
  pppSN <- ppp(x=gN$locX, y=gN$locY, window=monet_window)
  SLNN <- Linhom(pppSNN, correction="border")
  SLN <- Linhom(pppSN, correction="border")
  integralSNNN <- integral(SLNN - SLN)["border"]
  res[i] = integralSNNN
}

sorted_NNNra <- sort(res)
lowerNNN <- as.integer(0.025*nrow(res))
higherNNN <- as.integer(0.975*nrow(res))

if (integralNNN["border"] >= sorted_NNNra[higherNNN]) {
  hist(res,main="histogram, där NN är mer klustrad", xlab="resultat från integration", xlim=c(sorted_NNNra[1]+sorted_NNNra[1]*0.2,integralNNN["border"] + 0.2*integralNNN["border"]), nclass=15)
} else if (integralNNN["border"] <= sorted_NNNra[lowerNNN]) {
  hist(res,main="histogram, där NN är mer klustrad", xlab="resultat från integration", xlim=c(integralNNN["border"] + 0.2*integralNNN["border"],sorted_NNNra[nrow(sorted_NNNra)] + 0.1*sorted_NNNra[nrow(sorted_NNNra)]), nclass=15)
} else {
  hist(res,main="ingen förkastning mellan NN och N", xlab="resultat från integration", xlim=c(sorted_NNNra[1]-100-abs(sorted_NNNra[1]*0.1),sorted_NNNra[nrow(sorted_NNNra)]+100+abs(sorted_NNNra[nrow(sorted_NNNra)]*0.1)), nclass=15)
}


abline(v=integralNNN["border"], col="green")
abline(v=sorted_NNNra[lowerNNN], col="red")
abline(v=sorted_NNNra[higherNNN], col="red")





#################################ABS
#######################checks data for EXPERIENCE

#data from 1 group non-novices
dataNN <- subset(Monet, class == 2 & inpic == 1 & dur40 == 1)
#data from 1 group of novices
dataN <- subset(Monet, class == 1 & inpic == 1 & dur40 == 1)
#data from 1 group of all peeps
dataNNN <- subset(Monet, inpic == 1 & dur40 == 1)






pppNN <- ppp(x=dataNN$locX, y=dataNN$locY, window=monet_window)
pppN <- ppp(x=dataN$locX, y=dataN$locY, window=monet_window)

LNN <- Linhom(pppNN, correction="border")
LN <- Linhom(pppN, correction="border")

integralNNN <- integral(abs(LNN - LN))["border"]


plot(LNN$border, type="l", col="red", ylab="L(r)", xlab="r")
lines(LN$border, type="l", col="blue")
legend("topleft", legend = c("non-novice", "novice"), col=c("red","blue"), lwd=2)


ggplot(dataNN, aes(x=locX, y=locY)) +
  stat_density_2d_filled(h=c(bw.diggle(pppNN)*3, bw.diggle(pppNN)*3)) +
  scale_fill_viridis(discrete=TRUE, option="magma")



ggplot(dataN, aes(x=locX, y=locY)) +
  stat_density_2d_filled(h=c(bw.diggle(pppN)*3, bw.diggle(pppN)*3)) +
  scale_fill_viridis(discrete=TRUE, option="magma")





antalNN <- nrow(dataNN)
antalN <- nrow(dataN)
antalNNN <- nrow(dataNNN)
simuleringar <- 999
res=array(dim=simuleringar)
for(i in 1:simuleringar) {
  samplad <- sample(antalNNN, antalNN)
  gNN <- dataNNN[samplad, ]
  gN <- dataNNN[-samplad,]
  pppSNN <- ppp(x=gNN$locX, y=gNN$locY, window=monet_window)
  pppSN <- ppp(x=gN$locX, y=gN$locY, window=monet_window)
  SLNN <- Linhom(pppSNN, correction="border")
  SLN <- Linhom(pppSN, correction="border")
  integralSNNN <- integral(abs(SLNN - SLN))["border"]
  res[i] = integralSNNN
}

sorted_NNNra <- sort(res)
higherNNN <- as.integer(0.95*nrow(res))

if (integralNNN["border"] >= sorted_NNNra[higherNNN]) {
  hist(res,main="histogram, där NN är mer klustrad", xlab="resultat från integration", xlim=c(sorted_NNNra[1]+sorted_NNNra[1]*0.2,integralNNN["border"] + 0.2*integralNNN["border"]), nclass=15)
} else {
  hist(res,main="ingen förkastning mellan NN och N", xlab="resultat från integration", xlim=c(sorted_NNNra[1]-100-abs(sorted_NNNra[1]*0.1),sorted_NNNra[nrow(sorted_NNNra)]+100+abs(sorted_NNNra[nrow(sorted_NNNra)]*0.1)), nclass=15)
}


abline(v=integralNNN["border"], col="green")
abline(v=sorted_NNNra[higherNNN], col="red")













#data from males
dataM <- subset(Monet, sex == 2 & inpic == 1 & dur40 == 1)


#data from females
dataF <- subset(Monet, sex == 1 & inpic == 1 & dur40 == 1)






