load("Z:\\Chalmers\\KANDIDAT\\programmering\\Monet.RData")
monet_window <- owin(c(5, 1017), c(0, 767))
subject1_data <- subset(Monet, id == 1 & inpic == 1)

monet_ppp <- ppp(x = subject1_data$locX, 
                 y = subject1_data$locY, 
                 window = monet_window)

plot(monet_ppp)