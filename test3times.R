load("Z:\\Chalmers\\KANDIDAT\\programmering\\Monet.RData") #copy paste your path for data
library("spatstat")
library("ggplot2")
library("jpeg")
library("viridis")
library("plot3D") 
rm(list = ls()) #removes stuff from global environment if you want to clear shit

dev.off(dev.list()["RStudioGD"]) #removes all existing plots 
par(bg = "#212120", col.axis ="white", col.lab="white", col.main="white")

monet_window <- owin(c(5, 1017), c(0, 767))

curr_id <- NULL

dataA <- subset(Monet, inpic == 1 & duration >= 100) #everyone
dataNN <- subset(Monet, class == 2 & inpic == 1 & duration >= 100) #non-novices
dataN <- subset(Monet, class == 1 & inpic == 1 & duration >= 100) #novices

image <- readJPEG("Z:\\Chalmers\\KANDIDAT\\programmering\\Monet.JPG")


#### ------- list of subjects who are non-novices and novices respectively -------- ####

losNN <- unique(dataNN$id) # [1]  1  2  3  4  7  8 14 15 19 20
losN <- unique(dataN$id) # [1]  5  6  9 10 11 12 13 16 17 18

#### ------- ppp -------- ####

monet_pppNN <- ppp(x = dataNN$locX, y = dataNN$locY, window = monet_window)
monet_pppN <- ppp(x = dataN$locX, y = dataN$locY, window = monet_window)
monet_pppA <-  ppp(x = dataA$locX, y = dataA$locY, window = monet_window)

#### ------- density -------- ####

bNN <- bw.ppl(monet_pppNN)
bN <- bw.diggle(monet_pppN)
bA <- bw.diggle(monet_pppA)

densNN <- density(monet_pppNN, sigma = bNN)
densN <- density(monet_pppN, sigma = bN)
densA <- density(monet_pppA, sigma = bA)

plot(densNN)

#### ------- non-novices against the group of non-novices -------- ####

# shows the Kinhom against each other. every x_i ignores themselves
pdf("Kinhom&MC, NN vs NN, duration_over_100.pdf", width=20, height=40)
par(mfrow = c(10, 2))

for (i in losNN) {
  data_i <- subset(Monet, id == i & inpic == 1 & duration >= 100)
  data_r <- subset(Monet, id != i & inpic == 1 & class == 2 & duration >= 100)
  
  monet_i <- ppp(x = data_i$locX, y = data_i$locY, window = monet_window)
  monet_r <- ppp(x = data_r$locX, y = data_r$locY, window = monet_window)

  densNN_r <- density(monet_r, sigma=bw.ppl(monet_r), positive = TRUE, edge= TRUE, diggle=TRUE)
  
  KiNN_NN <- Kinhom(monet_i, correction=c("iso", "trans"), lambda=densNN_r)
  #KiNN_NN <- Kinhom(monet_i, sigma=bw.ppl(monet_r), correction=c("border", "bord.modif"))
  plot(KiNN_NN, main=paste("testsubjekt", i, "är klassen non-novice men mot grupp av non-novices"), lwd=3)
  #plot(envelope(Y = monet_i, fun = Kinhom, lambda = densNN_r, nsim = 39,
  #              simulate = expression(rpoispp(densNN_r))),
  #     lwd = 3, main = paste("subjekt", i, "non-novice mot non-novice-grupp"))
}
dev.off()


for (i in losNN) {
  data_i <- subset(Monet, id == i & inpic == 1 & duration >= 100)
  print(paste("subject", i, "has", nrow(data_i), "fixations"))
}


#### ------- non-novices against the group of novices -------- ####

pdf("Kinhom&MC, NN vs N, duration_over_100.pdf", width=20, height=40)
par(mfrow = c(10, 2))

for (i in losNN) {
  data_i <- subset(Monet, id == i & inpic == 1 & duration >= 100)
  data_r <- subset(Monet, inpic == 1 & class == 1 & duration >= 100)
  
  monet_i <- ppp(x = data_i$locX, y = data_i$locY, window = monet_window)
  monet_r <- ppp(x = data_r$locX, y = data_r$locY, window = monet_window)
  
  densN_r <- density(monet_r, sigma=bw.diggle(monet_r), positive = TRUE, edge=TRUE, diggle=TRUE)
  
  KiNN_N <- Kinhom(monet_i, lambda=densN_r, correction=c("border"))
  plot(KiNN_N, main=paste("testsubjekt", i, "är klassen non-novice men mot grupp av novices"), lwd=3)
  plot(envelope(Y = monet_i, fun = Kinhom, lambda = densN_r, nsim = 39,
                simulate = expression(rpoispp(densN_r))),
       lwd = 3, main = paste("subjekt", i, "non-novice mot novice-grupp"))
}
dev.off()

#### ------- novices against the group of novices -------- ####

#kinhom and mc
pdf("Kinhom&MC, N vs N, duration_over_100.pdf", width=20, height=40)
par(mfrow = c(10, 2))

for (i in losN) {
  data_i <- subset(Monet, id == i & inpic == 1 & duration >= 100)
  data_r <- subset(Monet, inpic == 1 & class == 1 & duration >= 100)
  
  monet_i <- ppp(x = data_i$locX, y = data_i$locY, window = monet_window)
  monet_r <- ppp(x = data_r$locX, y = data_r$locY, window = monet_window)
  
  densN_r <- density(monet_r, sigma=bw.diggle(monet_r), positive = TRUE, edge=TRUE, diggle=TRUE)
  KiN_N <- Kinhom(monet_i, lambda=densN_r, correction=c("border"))
  plot(KiN_N, main=paste("testsubjekt", i, "är klassen novice men mot grupp av novices"), lwd=3)
  plot(envelope(Y = monet_i, fun = Kinhom, lambda = densN_r, nsim = 39,
                simulate = expression(rpoispp(densN_r))),
       lwd = 3, main = paste("subjekt", i, "novice mot novice-grupp"))
}
dev.off()

#density 
pdf("density, N vs N, duration_over_100.pdf", width=10, height=40)
par(mfrow = c(10, 2))

  for (i in losN) {
    data_i <- subset(Monet, id == i & inpic == 1 & duration >= 100)
    data_r <- subset(Monet, inpic == 1 & class == 1 & duration >= 100)
    
    monet_i <- ppp(x = data_i$locX, y = data_i$locY, window = monet_window)
    monet_r <- ppp(x = data_r$locX, y = data_r$locY, window = monet_window)
    
    densN_i <- density(monet_i, sigma=bw.diggle(monet_i), positive = TRUE, edge=TRUE, diggle=TRUE)
    
    densN_r <- density(monet_r, sigma=bw.diggle(monet_r), positive = TRUE, edge=TRUE, diggle=TRUE)
    
    plot(densN_i, main=paste("testsubjekt", i, "är av klass novice mot gruppen novice"))
    plot(densN_r, main=paste("gruppen novice mot testsubjekt", i))
  }
dev.off()

#### ------- novices against the group of non-novices -------- ####

pdf("Kinhom&MC, N vs NN, duration_over_100.pdf", width=20, height=40)
par(mfrow = c(10, 2))

for (i in losN) {
  data_i <- subset(Monet, id == i & inpic == 1 & duration >= 100)
  data_r <- subset(Monet, id != i & inpic == 1 & class == 2 & duration >= 100)
  
  monet_i <- ppp(x = data_i$locX, y = data_i$locY, window = monet_window)
  monet_r <- ppp(x = data_r$locX, y = data_r$locY, window = monet_window)
  
  densNN_r <- density(monet_r, sigma=bw.diggle(monet_r))
  densNN_r$v[densNN_r$v < 0] <- 0
  
  KiN_NN <- Kinhom(monet_i, lambda=densNN_r, correction=c("border", "bord.modif"))
  plot(KiN_NN, main=paste("testsubjekt", i, "är klassen novice men mot grupp av non-novices"), lwd=3)
  plot(envelope(Y = monet_i, fun = Kinhom, lambda = densNN_r, nsim = 39,
                simulate = expression(rpoispp(densNN_r))),
       lwd = 3, main = paste("subjekt", i, "novice mot non-novice-grupp"))
}
dev.off()
