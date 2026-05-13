pdf("20hist_test1_diggletrue.pdf", width=8, height=40)
par(mfrow = c(5, 2),
    cex.axis = 1.2,
    cex.lab = 1.3,
    cex.main = 1.5)

for (q in 1:20){
  curr_id <- q
  subject_data_oneperson <- subset(Monet, id == curr_id & inpic == 1 & dur40 == 1)
  #subject_data_oneperson <- subset(Monet, id == curr_id & inpic == 1) #av någon anledning funkar typ inte denna
  monet_ppp <- ppp(x = subject_data_oneperson$locX, 
                   y = subject_data_oneperson$locY, 
                   window = monet_window)
  
  subject_data <- subset(Monet, inpic == 1 & dur40 == 1)
  alldata_ppp <- ppp(x = subject_data$locX,
                     y = subject_data$locY,
                     window = monet_window)
  ba <- bw.diggle(alldata_ppp)
  za <- density(alldata_ppp, sigma=ba)
  za$v <- pmax(za$v, 0)
  
  b <- bw.diggle(monet_ppp)
  z <- density(monet_ppp, sigma=b, diggle=TRUE)
  z$v <- pmax(z$v, 0)#se till att alla värden är positiva
  
  n <- 999
  #r <-  seq(0, 192, length.out = 500)
  #r_l <- length(r) 
  Li <- Linhom(monet_ppp, correction=c("border"), lambda = z)
  Li_v <- Li$border
  Li_r <- Li$r
  #Li_t <- Ki$theo
  l <- length(Li$r)
  #l = length(subject_data_oneperson$id)
  #test <- 192
  #Ki_simvalues <- Ki$border
  #NH <- Ki$theo
  #Tom matris, där vi sparar K-värden som vi simulerar från CSR
  L_MC <- matrix(,nrow=l, ncol=n)
  #Skapar tom vektor så att vi kan räkna ut r och n(i formeln för p-värde)
  d  <- c(numeric(n))
  #Skapar en tom vektor för p värden
  p <- c(numeric(n))
  teststat <- c(numeric(n))
  
  for(i in 1:n){
    simulering <- rpoispp(lambda = z) 
    #L_MC[,i] <- Linhom(simulering, correction=c("border"), lambda = za, monet_window)$border
    L_sim <- Linhom(simulering, correction = c("border"), lambda = z, r=Li_r)
    L_MC[,i] <- L_sim$border
    #d[i] <- sum((K_MC[i,]-Ki_v)^2)#för att få avstånd mellan värdena för funktionerna
    #d[i] <- (L_MC[i,] >= Li_v) 
    
    #teststat[i] <- trapz(Li_r, abs(Li_v - L_MC[,i]))
    #teststat[i] <- trapz(Li_r, (Li_v-L_sim$border)^2)
  }
  
  
  #teststat_obs <- trapz(Li_r, (Li_v-Li_r)^2)
  teststat_obs <- trapz(Li_r, abs(Li_v-Li_r))
  
  for(i in 1:n){
    #teststat[i] <- trapz(Li_r, (Li_v-L_MC[,i])^2) 
    teststat[i] <- trapz(Li_r, abs(Li_v-L_MC[,i]))
    #teststat[i] <- trapz(Li_r, (Li_v-L_sim$border)^2)
    
  }
  
  #sorted_ra <- sort(teststat)
  #lower <- as.integer(0.025*nrow(teststat))
  #higher <- as.integer(0.95*nrow(teststat))
  
  #lower <- quantile(teststat, 0.025)
  higher <- quantile(teststat, 0.95)
  
  hist(teststat, main=paste("histogram för testsubjekt", curr_id), xlab = "resultat från integrationen")
  #hist(teststat,main=paste("histogram för testsubjekt", curr_id), xlab="resultat från integration", xlim=c(sorted_ra[1]+sorted_ra[1]*0.2,teststat_obs+ 0.2*teststat_obs), nclass=15)
  abline(v=teststat_obs, col="green", lwd=2)
  
  #abline(v=lower, col="red", lwd=2)
  abline(v=higher, col="red", lwd=2)
  

  
  p <- (sum(teststat >= teststat_obs)+1)/(length(teststat)+1)
  print(p)
  
  
}

dev.off()






