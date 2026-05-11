load("Z:\\Chalmers\\KANDIDAT\\programmering\\Monet.RData") #copy paste your path for data
library("spatstat")
library("ggplot2")
library("jpeg")
library("viridis")
library("plot3D") 
library("sets")
library("tidyverse")
library("gridExtra")
rm(list = ls()) #removes stuff from global environment if you want to clear shit
dev.off(dev.list()["RStudioGD"]) #removes all existing plots 
monet_window <- owin(c(5, 1017), c(0, 767))
image <- readJPEG("Z:\\Chalmers\\KANDIDAT\\programmering\\Monet.JPG") #copy paste your path for image


plot_lst <- vector("list", length = 20)
for (i in 1:20) {
  curr_id <- i
  dat_i <- subset(Monet, id == curr_id & inpic == 1)
  p <- ggplot(dat_i, aes(x=locX, y=locY)) +
    annotation_raster(image,  xmin = 5, xmax = 1017, ymin = 0, ymax = 767) +
    geom_point(color = "white", size = 0.5) +
    coord_fixed(xlim = c(5, 1017), ylim = c(0, 767)) +
    labs(
      subtitle=paste("testperson", curr_id),
      x="x",
      y="y"
    ) +
    theme(plot.subtitle=element_text(hjust=0.5))
  plot_lst[[i]] <- p
}
pdf("alla_testpersoner.pdf", width = 8, height = 30)
print(cowplot::plot_grid(plotlist = plot_lst, nrow = 10, ncol = 2))
dev.off()

pdf("alla_testpersoner.pdf", width = 2, height = 2)
par(mfrow=c(10,2))
for (i in 1:20) {
  curr_id <- i
  dat_i <- subset(Monet, id == curr_id & inpic == 1)
  p <- ggplot(dat_i, aes(x=locX, y=locY)) +
    annotation_raster(image,  xmin = 5, xmax = 1017, ymin = 0, ymax = 767) +
    geom_point(color = "white", size = 0.5) +
    coord_fixed(xlim = c(5, 1017), ylim = c(0, 767))
  print(p)
}
dev.off()


