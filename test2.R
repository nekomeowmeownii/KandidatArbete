load("Z:\\Chalmers\\KANDIDAT\\programmering\\Monet.RData") #copy paste your path for data
library("spatstat")
library("ggplot2")
library("jpeg")
library("viridis")
library("plot3D") 
library("sets")
rm(list = ls()) #removes stuff from global environment if you want to clear shit

dev.off(dev.list()["RStudioGD"])
par(bg = "#212120", col.axis ="white", col.lab="white", col.main="white")

monet_window <- owin(c(5, 1017), c(0, 767))

curr_id <- NULL

dataA <- subset(Monet, inpic == 1) #everyone
dataNN <- subset(Monet, class == 2 & inpic == 1) #non-novices
dataN <- subset(Monet, class == 1 & inpic == 1) #novices


image <- readJPEG("Z:\\Chalmers\\KANDIDAT\\programmering\\Monet.JPG") #copy paste your path for image


#### ------- list of subjects who are non-novices and novices respectively -------- ####

losNN <- unique(dataNN$id) # [1]  1  2  3  4  7  8 14 15 19 20
losN <- unique(dataN$id) # [1]  5  6  9 10 11 12 13 16 17 18


#### ------- ppp -------- ####

monet_pppNN <- ppp(x = dataNN$locX, y = dataNN$locY, window = monet_window)
monet_pppN <- ppp(x = dataN$locX, y = dataN$locY, window = monet_window)
monet_pppA <-  ppp(x = dataA$locX, y = dataA$locY, window = monet_window)

#### ------- ppp plots -------- ####

plot.ppp(monet_pppNN, ann=TRUE, border ="white", cols="white", xlab ="plot för non-novices", col.lab="white",pch=3)

plot.ppp(monet_pppN, ann=TRUE, border ="white", cols="white", xlab ="plot för novices", col.lab="white",pch=3)


#### ------- density (converted to data frames, idk why) and their plots -------- ####

bNN <- bw.diggle(monet_pppNN)
bN <- bw.diggle(monet_pppN)
bA <- bw.diggle(monet_pppA)

densNN <- density(monet_pppNN, sigma = bNN, positive = TRUE, edge = TRUE, diggle = TRUE)
densN <- density(monet_pppN, sigma = bN, positive = TRUE, edge = TRUE, diggle = TRUE, dimyx = c(768, 1024))
densA <- density(monet_pppA, sigma = bA, positive = TRUE, edge = TRUE, diggle = TRUE)

densA_df <- as.data.frame(densA)
densNN_df <- as.data.frame(densNN)
densN_df <- as.data.frame(densN)

plot(densNN, main="density plot of non-novices")
plot(densN, main="density plot of novices")
plot(densA, main="density plot of all")


#### ------- density with image as background -------- ####

ggplot(dataNN, mapping=aes(x=locX, y=locY)) +
  annotation_raster(image,xmin=5,xmax=1017,ymin=0,ymax=767)+
  stat_density_2d(aes(fill = after_stat(level)), geom="polygon", h=c(bNN,bNN), alpha=0.3)+
  coord_fixed(xlim = c(5, 1017), ylim = c(0, 767))+
  scale_fill_gradient(name="venne alltså", low="darkblue", high="lightpink")



ggplot(dataN, mapping=aes(x=locX, y=locY)) +
  annotation_raster(image,xmin=5,xmax=1017,ymin=0,ymax=767)+
  stat_density_2d(aes(fill = after_stat(level)), geom="polygon", h=c(bN,bN), alpha=0.3)+
  coord_fixed(xlim = c(5, 1017), ylim = c(0, 767))+
  scale_fill_gradient(name="venne alltså", low="turquoise", high="#360f5a")


#### ------- everyone at once -------- ####


list_Adens <- list()
list_Appp <- list()
for (i in 1:20) {
  data_i <- subset(Monet, id == i & inpic == 1)
  monet_i <- ppp(x = data_i$locX, y = data_i$locY, window = monet_window)
  optB <- bw.diggle(monet_i)
  dens_i <- density(monet_i, sigma = optB)
  list_Adens[[length(list_Adens)+1]] = dens_i
  list_Appp[[length(list_Appp)+1]] = monet_i
  KiA <- Kinhom(monet_i, lambda=densA, correction=c("border", "bord.modif"))
  plot(KiA, main=paste("testsubjekt", i), lwd=3)
}

#### ------- non-novices against the group of non-novices -------- ####

# shows the Kinhom against each other.
pdf("Kinhom, NN vs NN.pdf", width=20, height=20)
par(mfrow = c(5, 2))
for (i in losNN) {
  data_i <- subset(Monet, id == i & inpic == 1)
  data_r <- subset(Monet, inpic == 1 & class == 2)
  
  monet_i <- ppp(x = data_i$locX, y = data_i$locY, window = monet_window)
  monet_r <- ppp(x = data_r$locX, y = data_r$locY, window = monet_window)
  
  densNN_r <- density(monet_r, sigma=bw.diggle(monet_r))
  
  KiNN_NN <- Kinhom(monet_i, lambda=densNN_r, correction=c("border"))
  plot(KiNN_NN, main=paste("testsubjekt", i, "är klassen non-novice men mot grupp av non-novices"), lwd=3) 
}
dev.off()
# shows the image
for (i in losNN) {
  data_i <- subset(Monet, id == i & inpic == 1)
  print(ggplot() +
    annotation_raster(image,xmin=5,xmax=1017,ymin=0,ymax=767)+
    geom_raster(data = densNN_df, aes(x=x, y=y, fill=value), alpha=0.7) +
    geom_point(mapping=aes(x=locX, y=locY), data=data_i, color="white")+
    coord_fixed(xlim = c(5, 1017), ylim = c(0, 767))+
    scale_fill_gradient(name="venne alltså", low="darkblue", high="darkorange") +
    annotate(geom="text", label = paste("subjekt", i), x= 506, y = -15))
}

# trying out monte carlo test for non-novices . ska man ha intensitet här eller?


pdf("montecarlo, NN vs NN.pdf", width=20, height=20)
par(mfrow = c(5, 2))
for (i in losNN) {
  data_i <- subset(Monet, id == i & inpic == 1)
  data_r <- subset(Monet, inpic == 1 & class == 2)
  
  monet_i <- ppp(x = data_i$locX, y = data_i$locY, window = monet_window)
  monet_r <- ppp(x = data_r$locX, y = data_r$locY, window = monet_window)
  
  densNN_r <- density(monet_r, sigma=bw.diggle(monet_r))
  densNN_r$v[densNN_r$v < 0] <- 0
  
  plot(envelope(Y = monet_i, fun = Kinhom, lambda=densNN_r, nsim=39, simulate = expression(rpoispp(densNN_r))), lwd=3,main=paste("Monte Carlo test för subjekt", i, "som är", "klass", unique(data_i$class), "mot gruppen", unique(data_i$class)))
}
dev.off()

#### ------- non-novices against the group of novices -------- ####
pdf("Kinhom, NN vs N, normal.pdf", width=20, height=20)
par(mfrow = c(5, 2))

for (i in losNN) {
  data_i <- subset(Monet, id == i & inpic == 1)
  monet_i <- ppp(x = data_i$locX, y = data_i$locY, window = monet_window)
  KiNN_N <- Kinhom(monet_i, lambda=densN, correction=c("border", "bord.modif"))
  plot(KiNN_N, main=paste("testsubjekt", i, "är klassen non-novice men mot grupp av novices"), lwd=3) 
}

dev.off()


#### ------- novices against the group of novices -------- ####
pdf("Kinhom, N vs N.pdf", width=20, height=20)
par(mfrow = c(5, 2))

for (i in losN) {
  data_i <- subset(Monet, id == i & inpic == 1)
  data_r <- subset(Monet, inpic == 1 & class == 1)
  
  monet_i <- ppp(x = data_i$locX, y = data_i$locY, window = monet_window)
  monet_r <- ppp(x = data_r$locX, y = data_r$locY, window = monet_window)
  
  densN_r <- density(monet_r, sigma=bw.diggle(monet_r))
  
  KiN_N <- Kinhom(monet_i, lambda=densN_r, correction=c("border"))
  plot(KiN_N, main=paste("testsubjekt", i, "är klassen novice men mot grupp av novices"), lwd=3) 
}

dev.off()
# trying out Monte Carlo test for novices. this one is homogenous, change it to kinhom later.

for (i in losN) {
  data_i <- subset(Monet, id == i & inpic == 1)
  monet_i <- ppp(x = data_i$locX, y = data_i$locY, window = monet_window)
  monet_df <- as.ppp(monet_i)
  plot(envelope(Y = monet_i, fun = Kest), nsim=20, lwd=3, main=paste("Monte Carlo test för subjekt", i, "som är", "klass", unique(data_i$class)))
}

#### ------- novices against the group of non-novices -------- ####

for (i in losN) {
  data_i <- subset(Monet, id == i & inpic == 1)
  monet_i <- ppp(x = data_i$locX, y = data_i$locY, window = monet_window)
  KiN_NN <- Kinhom(monet_i, lambda=densNN, correction=c("border", "bord.modif"))
  plot(KiN_NN, main=paste("testsubjekt", i, "är klassen novice men mot grupp av non-novices"), lwd=3) 
}

