#i denna fil testade vi runt med intensitet och ggplot. mest för att känna igen oss med R

load("")
library("spatstat")
library("ggplot2")
library("jpeg")
library("viridis")
library("rayshader")
library("plot3D") 

dev.off(dev.list()["RStudioGD"]) #KÖR INTE DENNA TIDIGT. BARA NÄR DU HAR PLOTTAT OCH VILL FÅ BORT ALLA PLOTS.


par(bg = "#212120", col.axis ="white", col.lab="white", col.main="white") #fixar bakgrund och färger till vad du vill. jag har valt att bakgrunden ska vara en specifik grå, axis, labels o mains vita osv. feel free to change.

monet_window <- owin(c(5, 1017), c(0, 767))
curr_id <- 5
subject_data <- subset(Monet, id == curr_id & inpic == 1)
image <- readJPEG("")

monet_ppp <- ppp(x=subject_data$locX, y=subject_data$locY, window=monet_window)

for (i in 1:20) {
  subject_data_i <- subset(Monet, id == i & inpic == 1)
  gg <- ggplot(subject_data_i, aes(x = locX, y = locY)) +
  annotation_raster(image, xmin = 5, xmax = 1017, ymin = 0, ymax = 767) +
  geom_point(color ="white", size=2, shape = 3) +
  coord_fixed(xlim = c(5, 1017), ylim = c(0, 767)) +
  labs(title = paste("subjekt", i), x = "x", y = "y")
  plot(gg)
}

#### ----- kernel density estimation, fixar intensitet. testa runt med sigma, bandbredden. kolla på rapporten för olika resultat. de påverkar även surface plotsen nedan.

z <- density(monet_ppp, sigma=100) 
plot(z, main = "hello")

####------- detta hära är figur 1 ifrån rapporten

colormat = viridis(n=1000, option="magma")
persp3D(main="surface", x = z$xcol, y = z$yrow, z=t(z$v), theta = 0, phi = 90, col=colormat, xlab = "x", ylab = "y", zlab = "intensity", ticktype = "simple", expand = 0.3, bty = "u", col.axis ="white", col.panel =NA, lwd.panel = 1)


####------ den här visar samma surface plot, fast med theta 0 och phi 90, basically ovanifrån bara.

colormat = viridis(n=1000, option="magma")
persp3D(main=paste("surface med bandbredd", 100), x = z$xcol, y = z$yrow, z=t(z$v), theta = 0, phi = 90, col=colormat, xlab = "x", ylab = "y", zlab = "intensity", ticktype = "simple", expand = 0.35, contour = TRUE, bty = "u", col.axis ="white", col.panel =NA, lwd.panel = 1)




####----- testade bara utan viridis färgen. och om det blev en skillnad i z paramtern. 

persp3D(x = z$xcol, y = z$yrow, z=t(z$v), theta = 0, phi = 90, xlab = "x", ylab = "y", zlab = "intensity", ticktype = "simple", expand = 0.35, bty = "f")
persp3D(x = z$xcol, y = z$yrow, z=z$v, theta = 0, phi = 90, xlab = "x", ylab = "y", zlab = "intensity", ticktype = "simple", expand = 0.35, bty = "f")


####------ plottar monet 2d alla punkter. 

plot.ppp(monet_ppp, ann=TRUE, border ="white", cols="white", xlab ="hej", col.lab="white",pch=3)


####----- denna är en inbyggd funktion som hittar "optimala" bandbredd enligt programmet. den tycktte 21.54 var ok.

c <- bw.ppl(monet_ppp)
print(c)
dens <- density(monet_ppp, sigma=c)
plot(dens, main = paste("bandbredd är", round(c,2)))
  
#bild på punkter o tavla
ggplot(subject_data, mapping = aes(x = locX, y = locY)) +
  annotation_raster(image, xmin = 5, xmax = 1017, ymin = 0, ymax = 767) +
  geom_point(color = "red", size = 2) +
  coord_fixed(xlim = c(5, 1017), ylim = c(0, 767))

bb = c(100,100)
ggplot(subject_data, aes(x = locX, y = locY)) +
  annotation_raster(image, xmin = 5, xmax = 1017, ymin = 0, ymax = 767) +
  stat_density2d(mapping = aes(fill = after_stat(level)),
  alpha = 0.2,
  geom = "polygon",
  h=bb) +
  scale_fill_gradient(low = "darkblue", high ="darkred") +
  #scale_fill_gradient(low = "yellow", high = "red") +
  coord_fixed(xlim = c(5, 1017), ylim = c(0, 767)) +
  geom_text(x=500, y=-15, label = paste("kör med", bb[1], "och", bb[2]))


#letar efter optimal bandbredd tror jag. vet inte skillnaden. 
b <- bw.diggle(monet_ppp)
print(b)
plot(density(monet_ppp, bw.diggle))
