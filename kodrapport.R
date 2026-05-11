load("") #copy paste your path for data
library("spatstat")
library("ggplot2")
library("jpeg")
library("viridis")
library("plot3D") 
library("sets")
library("pracma")
monet_window <- owin(c(5, 1017), c(0, 767))
image <- readJPEG("") #copy paste your path for image
curr_id <- 1 #testperson 1


#test Inhomogen Poisson
subject_data_oneperson <- subset(Monet, id == curr_id & inpic == 1 & dur40 == 1)
subject_data <- subset(Monet, inpic == 1 & dur40 == 1)
monet_ppp <- ppp(x = subject_data_oneperson$locX, 
                 y = subject_data_oneperson$locY, 
                 window = monet_window)

alldata_ppp <- ppp(x = subject_data$locX,
                   y = subject_data$locY, 
                   window = monet_window)

b <- bw.diggle(monet_ppp)#bandbredd
ba <- bw.diggle(alldata_ppp)
z <- density(monet_ppp, sigma=b)#intensitet
za <- density(alldata_ppp, sigma=ba)

n <- 999
Li <- Linhom(monet_ppp, correction=c("border"), lambda = za)
Li_v <- Li$border
Li_r <- Li$r
l <- length(Li$r)
#Tom matris, där vi sparar K-värden som vi simulerar från CSR
L_MC <- matrix(,nrow=l, ncol=n)
#Tom vektor för att samla in testvärden
teststat <- c(numeric(n))

for(i in 1:n){
  simulering <- rpoispp(lambda = za) 
  L_sim <- Linhom(simulering, correction = c("border"), lambda = za, r=Li_r)
  L_MC[,i] <- L_sim$border
}

teststat_obs <- trapz(Li_r, abs(Li_v-Li_r))

for(i in 1:n){
  teststat[i] <- trapz(Li_r, abs(Li_v-L_MC[,i])) 
}


#test start mot slut

#data första 45 sekunder
data451 <- subset(Monet, id == curr_id & inpic == 1 & dur40 == 1 & timestamp <= 45000)
ppp451 <- ppp(data451$locX, y = data451$locY, window=monet_window)
L451 <- Linhom(ppp451, correction="border")


#data sista 45 sekunder
data454 <- subset(Monet, id == curr_id & inpic == 1 & dur40 == 1 & timestamp <= 180000 & timestamp >= 135000)
ppp454 <- ppp(data454$locX, y = data454$locY, window=monet_window)
L454 <- Linhom(ppp454, correction="border")


integral451454 <- integral(abs(L451 - L454)) 
pooled451454 <- subset(Monet, id == curr_id & inpic == 1 & dur40 == 1 & (timestamp <= 45000 | (timestamp >= 135000 & timestamp <= 180000)))

n451 <- nrow(data451)
n454 <- nrow(data454)
n451454 <- nrow(pooled451454)

#simulations
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


#test erfarenhet
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