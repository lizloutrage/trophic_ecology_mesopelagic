#' Plot ellipses for station Z0492 (370m) 
#' 
#' @description
#' This function plot the ellipse using the package SIBER and ggplot2 
#' 
#' @param x isotope data in format siber (siber_data)
#'
#' @return a plot of the ellipses in ggplot
#' 
#' @export
#'
#' @examples
#' plot_Z0492()
#' plot_Z0492(siber_data)
#' 

plot_Z0492 <- function(data){
  
mydata_Z0492 <- subset(data, community=="2")
 
siber.CN <- SIBER::createSiberObject(mydata_Z0492)  

nichecol <- c("#E4A33A","#F67451","#94B3AE","#d193f7", "#678FCB","#6B54A0")

#When you want to use SEA corrected for small samples, you have to change the argument "m"
#like as is done for the ellipse 2 with the coordinates of the ellipse
ellipse1 <- "2.1"
tmp <- strsplit(ellipse1, "[.]")
c.1 <- tmp[[1]][1]
e.1 <- tmp[[1]][2]
SECN1 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 1],
                    siber.CN$ML.cov[[1]][ , , 1],
                    m =  siber.CN$sample.sizes[c.1,e.1],
                    n = 100,
                    p.interval = NULL,
                    ci.mean = FALSE,
                    col = "black",
                    lty = 1,
                    lwd = 2)

#Same for group 2
ellipse2 <- "2.2"
tmp <- strsplit(ellipse2, "[.]")
c.2 <- tmp[[1]][1]
e.2 <- tmp[[1]][2]
SECN2 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 2],
                    siber.CN$ML.cov[[1]][ , , 2],
                    m = siber.CN$sample.sizes[c.2,e.2],
                    n = 100,
                    p.interval = NULL,
                    small.sample = TRUE,
                    ci.mean = FALSE,
                    col = "black",
                    lty = 1,
                    lwd = 2)

#And for group 3
ellipse3 <- "2.3"
tmp <- strsplit(ellipse3, "[.]")
c.3 <- tmp[[1]][1]
e.3 <- tmp[[1]][2]
SECN3 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 3],
                    siber.CN$ML.cov[[1]][ , , 3],
                    m = siber.CN$sample.sizes[c.3,e.3],
                    small.sample = TRUE,
                    n = 100,
                    p.interval = NULL,
                    ci.mean = FALSE,
                    col = "black",
                    lty = 1,
                    lwd = 2)
#And for group 4
ellipse4 <- "2.4"
tmp <- strsplit(ellipse4, "[.]")
c.4 <- tmp[[1]][1]
e.4 <- tmp[[1]][2]
SECN4 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 4],
                    siber.CN$ML.cov[[1]][ , , 4],
                    m =  siber.CN$sample.sizes[c.4,e.4],
                    n = 100,
                    p.interval = NULL,
                    ci.mean = FALSE,
                    col = "black",
                    lty = 1,
                    lwd = 2)
#group5
ellipse5 <- "2.5"
tmp <- strsplit(ellipse5, "[.]")
c.5 <- tmp[[1]][1]
e.5 <- tmp[[1]][2]
SECN5 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 5],
                    siber.CN$ML.cov[[1]][ , , 5],
                    m =  siber.CN$sample.sizes[c.5,e.5],
                    n = 100,
                    p.interval = NULL,
                    ci.mean = FALSE,
                    col = "black",
                    lty = 1,
                    lwd = 2)

#group6
ellipse6 <- "2.6"
tmp <- strsplit(ellipse6, "[.]")
c.6 <- tmp[[1]][1]
e.6 <- tmp[[1]][2]
SECN6 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 6],
                    siber.CN$ML.cov[[1]][ , , 6],
                    m =  siber.CN$sample.sizes[c.6,e.6],
                    n = 100,
                    p.interval = NULL,
                    ci.mean = FALSE,
                    col = "black",
                    lty = 1,
                    lwd = 2)

#get the ellipse coordinates into a data frame that. First, we need to
#repeat the group codes for as long as each ellipse coordinate vector is (100 points by default)
CN1_ <- rep(1, length(SECN1[,1]))
CN2_ <- rep(2, length(SECN2[,1]))
CN3_ <- rep(3, length(SECN3[,1]))
CN4_ <- rep(4, length(SECN4[,1]))
CN5_ <- rep(5, length(SECN5[,1]))
CN6_ <- rep(6, length(SECN6[,1]))
CN_ <- c(CN1_, CN2_, CN3_, CN4_,CN5_, CN6_)

#Then extract x and y coordinates from the ellipses created before
xCN <- c(SECN1[,1],SECN2[,1],SECN3[,1],SECN4[,1],SECN5[,1],SECN6[,1])
yCN <- c(SECN1[,2],SECN2[,2],SECN3[,2],SECN4[,2],SECN5[,2],SECN6[,2])

#And put all that in a data frame
df_SECNZ0492 <- data.frame(xCN,yCN,CN_)

#graph with filling :
plotz4092 <- ggplot(mydata_Z0492, aes(x=iso1, y=iso2, group=factor(group))) +
  theme_light()+
  geom_point(aes(color=factor(group))) +
  scale_x_continuous(expression({delta}^13*C~'\u2030'))+ #, limits = c(-22.5,-16.5)) +
  scale_y_continuous(expression({delta}^15*N~'\u2030'))+ #,limits = c(7.5,13.5)) +
  geom_polygon(data=df_SECNZ0492,size=0.6, aes(x=xCN, y=yCN,group=factor(CN_),
                                               fill=factor(CN_),col=factor(CN_))) +
  scale_fill_manual(aesthetics = "fill",values=alpha(nichecol,0.34),
                    labels = c("Arctozenus risso", "Argyropelecus olfersii",
                               "Lampanyctus crocodilus",
                               "Myctophum punctatum","Notoscopelus kroyeri",
                               "Xenodermichthys copei"))+
  scale_color_manual(values=nichecol )+
  theme(panel.grid.minor = element_blank(),
        legend.text = element_text(face="italic", size=11),
        legend.title = element_text(size = 11),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11),
        axis.title.x = element_text(size=11),
        axis.title.y = element_text(size=11)) +
  guides(size="none", linetype="none",col="none",
         fill=guide_legend(title="Species",
                           keywidth=1.3,
                           title.hjust=0.5))
plotz4092
 }

plot_Z0497 <- function(data){
  
  mydata_Z0497 <- subset(data, community=="7")
  
  siber.CN <- SIBER::createSiberObject(mydata_Z0497)  
  
  nichecol <- c("#F67451","#94B3AE","#18206F","#d193f7","#00547A","#6B54A0")
  
  #When you want to use SEA corrected for small samples, you have to change the argument "m"
  #like as is done for the ellipse 2 with the coordinates of the ellipse
  ellipse1 <- "7.1"
  tmp <- strsplit(ellipse1, "[.]")
  c.1 <- tmp[[1]][1]
  e.1 <- tmp[[1]][2]
  SECN1 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 1],
                             siber.CN$ML.cov[[1]][ , , 1],
                             m =  siber.CN$sample.sizes[c.1,e.1],
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  
  #Same for group 2
  ellipse2 <- "7.2"
  tmp <- strsplit(ellipse2, "[.]")
  c.2 <- tmp[[1]][1]
  e.2 <- tmp[[1]][2]
  SECN2 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 2],
                             siber.CN$ML.cov[[1]][ , , 2],
                             m = siber.CN$sample.sizes[c.2,e.2],
                             n = 100,
                             p.interval = NULL,
                             small.sample = TRUE,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  
  #And for group 3
  ellipse3 <- "7.3"
  tmp <- strsplit(ellipse3, "[.]")
  c.3 <- tmp[[1]][1]
  e.3 <- tmp[[1]][2]
  SECN3 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 3],
                             siber.CN$ML.cov[[1]][ , , 3],
                             m = siber.CN$sample.sizes[c.3,e.3],
                             small.sample = TRUE,
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  #And for group 4
  ellipse4 <- "7.4"
  tmp <- strsplit(ellipse4, "[.]")
  c.4 <- tmp[[1]][1]
  e.4 <- tmp[[1]][2]
  SECN4 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 4],
                             siber.CN$ML.cov[[1]][ , , 4],
                             m =  siber.CN$sample.sizes[c.4,e.4],
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  #group5
  ellipse5 <- "7.5"
  tmp <- strsplit(ellipse5, "[.]")
  c.5 <- tmp[[1]][1]
  e.5 <- tmp[[1]][2]
  SECN5 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 5],
                             siber.CN$ML.cov[[1]][ , , 5],
                             m =  siber.CN$sample.sizes[c.5,e.5],
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  
  #group6
  ellipse6 <- "7.6"
  tmp <- strsplit(ellipse6, "[.]")
  c.6 <- tmp[[1]][1]
  e.6 <- tmp[[1]][2]
  SECN6 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 6],
                             siber.CN$ML.cov[[1]][ , , 6],
                             m =  siber.CN$sample.sizes[c.6,e.6],
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  
  #get the ellipse coordinates into a data frame that. First, we need to
  #repeat the group codes for as long as each ellipse coordinate vector is (100 points by default)
  CN1_ <- rep(1, length(SECN1[,1]))
  CN2_ <- rep(2, length(SECN2[,1]))
  CN3_ <- rep(3, length(SECN3[,1]))
  CN4_ <- rep(4, length(SECN4[,1]))
  CN5_ <- rep(5, length(SECN5[,1]))
  CN6_ <- rep(6, length(SECN6[,1]))
  CN_ <- c(CN1_, CN2_, CN3_, CN4_,CN5_, CN6_)
  
  #Then extract x and y coordinates from the ellipses created before
  xCN <- c(SECN1[,1],SECN2[,1],SECN3[,1],SECN4[,1],SECN5[,1],SECN6[,1])
  yCN <- c(SECN1[,2],SECN2[,2],SECN3[,2],SECN4[,2],SECN5[,2],SECN6[,2])
  
  #And put all that in a data frame
  df_SECNZ0497 <- data.frame(xCN,yCN,CN_)
  
  #graph with filling :
  plotz4097 <- ggplot(mydata_Z0497, aes(x=iso1, y=iso2, group=factor(group))) +
    theme_light()+
    geom_point(aes(color=factor(group))) +
    scale_x_continuous(expression({delta}^13*C~'\u2030'))+ #, limits = c(-22.5,-16.5)) +
    scale_y_continuous(expression({delta}^15*N~'\u2030'))+ #,limits = c(7.5,13.5)) +
    geom_polygon(data=df_SECNZ0497,size=0.6, aes(x=xCN, y=yCN,group=factor(CN_), 
                                                 fill=factor(CN_),col=factor(CN_))) +
    scale_fill_manual(aesthetics = "fill",values=alpha(nichecol,0.34),
                      labels = c("Argyropelecus olfersii",
                                 "Lampanyctus crocodilus","Lampanyctus macdonaldi",
                                 "Myctophum punctatum",
                                 "Serrivomer beanii", "Xenodermichthys copei"))+
    scale_color_manual(values=nichecol)+
    theme(panel.grid.minor = element_blank(),
          legend.text = element_text(face="italic", size=11),
          legend.title = element_text(size = 11),
          axis.text.x = element_text(size=11),
          axis.text.y = element_text(size=11),
          axis.title.x = element_text(size=11),
          axis.title.y = element_text(size=11)) +
    guides(size="none", linetype="none",col="none",
           fill=guide_legend(title="Species",
                             keywidth=1.3,
                             title.hjust=0.5))
  plotz4097
}

plot_Z0503 <- function(data){
  
  mydata_Z0503 <- subset(data, community=="4")
  
  siber.CN <- SIBER::createSiberObject(mydata_Z0503)  
  
  nichecol <- c("#E4A33A","#3DA5D9","#94B3AE","#00547A","#6B54A0")
  
  #When you want to use SEA corrected for small samples, you have to change the argument "m"
  #like as is done for the ellipse 2 with the coordinates of the ellipse
  ellipse1 <- "4.1"
  tmp <- strsplit(ellipse1, "[.]")
  c.1 <- tmp[[1]][1]
  e.1 <- tmp[[1]][2]
  SECN1 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 1],
                             siber.CN$ML.cov[[1]][ , , 1],
                             m =  siber.CN$sample.sizes[c.1,e.1],
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  
  #Same for group 2
  ellipse2 <- "4.2"
  tmp <- strsplit(ellipse2, "[.]")
  c.2 <- tmp[[1]][1]
  e.2 <- tmp[[1]][2]
  SECN2 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 2],
                             siber.CN$ML.cov[[1]][ , , 2],
                             m = siber.CN$sample.sizes[c.2,e.2],
                             n = 100,
                             p.interval = NULL,
                             small.sample = TRUE,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  
  #And for group 3
  ellipse3 <- "4.3"
  tmp <- strsplit(ellipse3, "[.]")
  c.3 <- tmp[[1]][1]
  e.3 <- tmp[[1]][2]
  SECN3 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 3],
                             siber.CN$ML.cov[[1]][ , , 3],
                             m = siber.CN$sample.sizes[c.3,e.3],
                             small.sample = TRUE,
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  #And for group 4
  ellipse4 <- "4.4"
  tmp <- strsplit(ellipse4, "[.]")
  c.4 <- tmp[[1]][1]
  e.4 <- tmp[[1]][2]
  SECN4 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 4],
                             siber.CN$ML.cov[[1]][ , , 4],
                             m =  siber.CN$sample.sizes[c.4,e.4],
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  #group5
  ellipse5 <- "4.5"
  tmp <- strsplit(ellipse5, "[.]")
  c.5 <- tmp[[1]][1]
  e.5 <- tmp[[1]][2]
  SECN5 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 5],
                             siber.CN$ML.cov[[1]][ , , 5],
                             m =  siber.CN$sample.sizes[c.5,e.5],
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  

  #get the ellipse coordinates into a data frame that. First, we need to
  #repeat the group codes for as long as each ellipse coordinate vector is (100 points by default)
  CN1_ <- rep(1, length(SECN1[,1]))
  CN2_ <- rep(2, length(SECN2[,1]))
  CN3_ <- rep(3, length(SECN3[,1]))
  CN4_ <- rep(4, length(SECN4[,1]))
  CN5_ <- rep(5, length(SECN5[,1]))
  CN_ <- c(CN1_, CN2_, CN3_, CN4_,CN5_)
  
  #Then extract x and y coordinates from the ellipses created before
  xCN <- c(SECN1[,1],SECN2[,1],SECN3[,1],SECN4[,1],SECN5[,1])
  yCN <- c(SECN1[,2],SECN2[,2],SECN3[,2],SECN4[,2],SECN5[,2])
  
  #And put all that in a data frame
  df_SECNZ0503 <- data.frame(xCN,yCN,CN_)
  
  #graph with filling :
  plotz0503 <- ggplot(mydata_Z0503, aes(x=iso1, y=iso2, group=factor(group))) +
    theme_light()+
    geom_point(aes(color=factor(group))) +
    scale_x_continuous(expression({delta}^13*C~'\u2030'))+ #, limits = c(-22.5,-16.5)) +
    scale_y_continuous(expression({delta}^15*N~'\u2030'))+#,limits = c(7.5,13.5)) +
    geom_polygon(data=df_SECNZ0503,size=0.6, aes(x=xCN, y=yCN,group=factor(CN_), 
                                                 fill=factor(CN_),col=factor(CN_))) +
    scale_fill_manual(aesthetics = "fill",values=alpha(nichecol,0.34),
                      labels = c("Arctozenus risso", "Cyclothone",
                                 "Lampanyctus crocodilus",
                                 "Serrivomer beanii",
                                 "Xenodermichthys copei"))+
    scale_color_manual(values=nichecol )+
    theme(panel.grid.minor = element_blank(),
          legend.text = element_text(face="italic", size=11),
          legend.title = element_text(size = 11),
          axis.text.x = element_text(size=11),
          axis.text.y = element_text(size=11),
          axis.title.x = element_text(size=11),
          axis.title.y = element_text(size=11)) +
    guides(size="none", linetype="none",col="none",
           fill=guide_legend(title="Species",
                             keywidth=1.3,
                             title.hjust=0.5))
  plotz0503
}

plot_Z0508 <- function(data){
  
  mydata_Z0508 <- subset(data, community=="1")
  
  siber.CN <- SIBER::createSiberObject(mydata_Z0508)  
  
  nichecol <- c("#FD151B","#072AC8","#d193f7","#d8c2ab","#678FCB")
  
  #When you want to use SEA corrected for small samples, you have to change the argument "m"
  #like as is done for the ellipse 2 with the coordinates of the ellipse
  ellipse1 <- "1.1"
  tmp <- strsplit(ellipse1, "[.]")
  c.1 <- tmp[[1]][1]
  e.1 <- tmp[[1]][2]
  SECN1 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 1],
                             siber.CN$ML.cov[[1]][ , , 1],
                             m =  siber.CN$sample.sizes[c.1,e.1],
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  
  #Same for group 2
  ellipse2 <- "1.2"
  tmp <- strsplit(ellipse2, "[.]")
  c.2 <- tmp[[1]][1]
  e.2 <- tmp[[1]][2]
  SECN2 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 2],
                             siber.CN$ML.cov[[1]][ , , 2],
                             m = siber.CN$sample.sizes[c.2,e.2],
                             n = 100,
                             p.interval = NULL,
                             small.sample = TRUE,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  
  #And for group 3
  ellipse3 <- "1.3"
  tmp <- strsplit(ellipse3, "[.]")
  c.3 <- tmp[[1]][1]
  e.3 <- tmp[[1]][2]
  SECN3 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 3],
                             siber.CN$ML.cov[[1]][ , , 3],
                             m = siber.CN$sample.sizes[c.3,e.3],
                             small.sample = TRUE,
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  #And for group 4
  ellipse4 <- "1.4"
  tmp <- strsplit(ellipse4, "[.]")
  c.4 <- tmp[[1]][1]
  e.4 <- tmp[[1]][2]
  SECN4 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 4],
                             siber.CN$ML.cov[[1]][ , , 4],
                             m =  siber.CN$sample.sizes[c.4,e.4],
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  #group5
  ellipse5 <- "1.5"
  tmp <- strsplit(ellipse5, "[.]")
  c.5 <- tmp[[1]][1]
  e.5 <- tmp[[1]][2]
  SECN5 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 5],
                             siber.CN$ML.cov[[1]][ , , 5],
                             m =  siber.CN$sample.sizes[c.5,e.5],
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  
  
  #get the ellipse coordinates into a data frame that. First, we need to
  #repeat the group codes for as long as each ellipse coordinate vector is (100 points by default)
  CN1_ <- rep(1, length(SECN1[,1]))
  CN2_ <- rep(2, length(SECN2[,1]))
  CN3_ <- rep(3, length(SECN3[,1]))
  CN4_ <- rep(4, length(SECN4[,1]))
  CN5_ <- rep(5, length(SECN5[,1]))
  CN_ <- c(CN1_, CN2_, CN3_, CN4_,CN5_)
  
  #Then extract x and y coordinates from the ellipses created before
  xCN <- c(SECN1[,1],SECN2[,1],SECN3[,1],SECN4[,1],SECN5[,1])
  yCN <- c(SECN1[,2],SECN2[,2],SECN3[,2],SECN4[,2],SECN5[,2])
  
  #And put all that in a data frame
  df_SECNZ0508 <- data.frame(xCN,yCN,CN_)
  
  #graph with filling :
  plotz0508 <- ggplot(mydata_Z0508, aes(x=iso1, y=iso2, group=factor(group))) +
    theme_light()+  theme_light()+
    geom_point(aes(color=factor(group))) +
    scale_x_continuous(expression({delta}^13*C~'\u2030'))+# , limits = c(-22.5,-16.5)) +
    scale_y_continuous(expression({delta}^15*N~'\u2030'))+#,limits = c(7.5,13.5)) +
    geom_polygon(data=df_SECNZ0508,size=0.6, aes(x=xCN, y=yCN,group=factor(CN_), 
                                                 fill=factor(CN_),col=factor(CN_))) +
    scale_fill_manual(aesthetics = "fill",values=alpha(nichecol,0.34),
                      labels = c("Lestidiops sphyrenoides", "Maurolicus muelleri",
                                 "Myctophum punctatum",
                                 "Notoscopelus bolini", "Notoscopelus kroyeri"))+
    scale_color_manual(values=nichecol )+
    theme(panel.grid.minor = element_blank(),
          legend.text = element_text(face="italic", size=11),
          legend.title = element_text(size = 11),
          axis.text.x = element_text(size=11),
          axis.text.y = element_text(size=11),
          axis.title.x = element_text(size=11),
          axis.title.y = element_text(size=11)) +
    guides(size="none", linetype="none",col="none",
           fill=guide_legend(title="Species",
                             keywidth=1.3,
                             title.hjust=0.5))
  plotz0508
}

plot_Z0512 <- function(data){
  
  mydata_Z0512 <- subset(data, community=="3")
  
  siber.CN <- SIBER::createSiberObject(mydata_Z0512)  
  
  nichecol <- c("#E4A33A","#F67451","#94b3ae","#678FCB","#6B54A0")
  
  #When you want to use SEA corrected for small samples, you have to change the argument "m"
  #like as is done for the ellipse 2 with the coordinates of the ellipse
  ellipse1 <- "3.1"
  tmp <- strsplit(ellipse1, "[.]")
  c.1 <- tmp[[1]][1]
  e.1 <- tmp[[1]][2]
  SECN1 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 1],
                             siber.CN$ML.cov[[1]][ , , 1],
                             m =  siber.CN$sample.sizes[c.1,e.1],
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  
  #Same for group 2
  ellipse2 <- "3.2"
  tmp <- strsplit(ellipse2, "[.]")
  c.2 <- tmp[[1]][1]
  e.2 <- tmp[[1]][2]
  SECN2 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 2],
                             siber.CN$ML.cov[[1]][ , , 2],
                             m = siber.CN$sample.sizes[c.2,e.2],
                             n = 100,
                             p.interval = NULL,
                             small.sample = TRUE,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  
  #And for group 3
  ellipse3 <- "3.3"
  tmp <- strsplit(ellipse3, "[.]")
  c.3 <- tmp[[1]][1]
  e.3 <- tmp[[1]][2]
  SECN3 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 3],
                             siber.CN$ML.cov[[1]][ , , 3],
                             m = siber.CN$sample.sizes[c.3,e.3],
                             small.sample = TRUE,
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  #And for group 4
  ellipse4 <- "3.4"
  tmp <- strsplit(ellipse4, "[.]")
  c.4 <- tmp[[1]][1]
  e.4 <- tmp[[1]][2]
  SECN4 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 4],
                             siber.CN$ML.cov[[1]][ , , 4],
                             m =  siber.CN$sample.sizes[c.4,e.4],
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  #group5
  ellipse5 <- "3.5"
  tmp <- strsplit(ellipse5, "[.]")
  c.5 <- tmp[[1]][1]
  e.5 <- tmp[[1]][2]
  SECN5 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 5],
                             siber.CN$ML.cov[[1]][ , , 5],
                             m =  siber.CN$sample.sizes[c.5,e.5],
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  
  
  #get the ellipse coordinates into a data frame that. First, we need to
  #repeat the group codes for as long as each ellipse coordinate vector is (100 points by default)
  CN1_ <- rep(1, length(SECN1[,1]))
  CN2_ <- rep(2, length(SECN2[,1]))
  CN3_ <- rep(3, length(SECN3[,1]))
  CN4_ <- rep(4, length(SECN4[,1]))
  CN5_ <- rep(5, length(SECN5[,1]))
  CN_ <- c(CN1_, CN2_, CN3_, CN4_,CN5_)
  
  #Then extract x and y coordinates from the ellipses created before
  xCN <- c(SECN1[,1],SECN2[,1],SECN3[,1],SECN4[,1],SECN5[,1])
  yCN <- c(SECN1[,2],SECN2[,2],SECN3[,2],SECN4[,2],SECN5[,2])
  
  #And put all that in a data frame
  df_SECNZ0512 <- data.frame(xCN,yCN,CN_)
  
  #graph with filling :
  plotz0512 <- ggplot(mydata_Z0512, aes(x=iso1, y=iso2, group=factor(group))) +
    theme_light()+  theme_light()+
    geom_point(aes(color=factor(group))) +
    scale_x_continuous(expression({delta}^13*C~'\u2030'))+ #, limits = c(-22.5,-16.5)) +
    scale_y_continuous(expression({delta}^15*N~'\u2030'))+ #,limits = c(7.5,13.5)) +
    geom_polygon(data=df_SECNZ0512,size=0.6, aes(x=xCN, y=yCN,group=factor(CN_), 
                                                 fill=factor(CN_),col=factor(CN_))) +
    scale_fill_manual(aesthetics = "fill",values=alpha(nichecol,0.34),
                      labels = c("Arctozenus risso", "Argyropelecus olfersii",
                                 "Lampanyctus crocodilus",
                                 "Notoscopelus kroyeri","Xenodermichthys copei"))+
    scale_color_manual(values=nichecol )+
    theme(panel.grid.minor = element_blank(),
          legend.text = element_text(face="italic", size=11),
          legend.title = element_text(size = 11),
          axis.text.x = element_text(size=11),
          axis.text.y = element_text(size=11),
          axis.title.x = element_text(size=11),
          axis.title.y = element_text(size=11)) +
    guides(size="none", linetype="none",col="none",
           fill=guide_legend(title="Species",
                             keywidth=1.3,
                             title.hjust=0.5))
  plotz0512
}

plot_Z0518 <- function(data){
  
  mydata_Z0518 <- subset(data, community=="5")
  
  siber.CN <- SIBER::createSiberObject(mydata_Z0518)  
  
  nichecol <- c("#F67451","#D664BE","#94B3AE","#049A8F","#A63A49","#00547A")
  
  #When you want to use SEA corrected for small samples, you have to change the argument "m"
  #like as is done for the ellipse 2 with the coordinates of the ellipse
  ellipse1 <- "5.1"
  tmp <- strsplit(ellipse1, "[.]")
  c.1 <- tmp[[1]][1]
  e.1 <- tmp[[1]][2]
  SECN1 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 1],
                             siber.CN$ML.cov[[1]][ , , 1],
                             m =  siber.CN$sample.sizes[c.1,e.1],
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  
  #Same for group 2
  ellipse2 <- "5.2"
  tmp <- strsplit(ellipse2, "[.]")
  c.2 <- tmp[[1]][1]
  e.2 <- tmp[[1]][2]
  SECN2 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 2],
                             siber.CN$ML.cov[[1]][ , , 2],
                             m = siber.CN$sample.sizes[c.2,e.2],
                             n = 100,
                             p.interval = NULL,
                             small.sample = TRUE,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  
  #And for group 3
  ellipse3 <- "5.3"
  tmp <- strsplit(ellipse3, "[.]")
  c.3 <- tmp[[1]][1]
  e.3 <- tmp[[1]][2]
  SECN3 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 3],
                             siber.CN$ML.cov[[1]][ , , 3],
                             m = siber.CN$sample.sizes[c.3,e.3],
                             small.sample = TRUE,
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  #And for group 4
  ellipse4 <- "5.4"
  tmp <- strsplit(ellipse4, "[.]")
  c.4 <- tmp[[1]][1]
  e.4 <- tmp[[1]][2]
  SECN4 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 4],
                             siber.CN$ML.cov[[1]][ , , 4],
                             m =  siber.CN$sample.sizes[c.4,e.4],
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  #group5
  ellipse5 <- "5.5"
  tmp <- strsplit(ellipse5, "[.]")
  c.5 <- tmp[[1]][1]
  e.5 <- tmp[[1]][2]
  SECN5 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 5],
                             siber.CN$ML.cov[[1]][ , , 5],
                             m =  siber.CN$sample.sizes[c.5,e.5],
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  
  
  
  #group6
  ellipse6 <- "5.6"
  tmp <- strsplit(ellipse6, "[.]")
  c.6 <- tmp[[1]][1]
  e.6 <- tmp[[1]][2]
  SECN6 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 6],
                             siber.CN$ML.cov[[1]][ , , 6],
                             m =  siber.CN$sample.sizes[c.6,e.6],
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  
  #get the ellipse coordinates into a data frame that. First, we need to
  #repeat the group codes for as long as each ellipse coordinate vector is (100 points by default)
  CN1_ <- rep(1, length(SECN1[,1]))
  CN2_ <- rep(2, length(SECN2[,1]))
  CN3_ <- rep(3, length(SECN3[,1]))
  CN4_ <- rep(4, length(SECN4[,1]))
  CN5_ <- rep(5, length(SECN5[,1]))
  CN6_ <- rep(6, length(SECN6[,1]))
  CN_ <- c(CN1_, CN2_, CN3_, CN4_,CN5_, CN6_)
  
  #Then extract x and y coordinates from the ellipses created before
  xCN <- c(SECN1[,1],SECN2[,1],SECN3[,1],SECN4[,1],SECN5[,1],SECN6[,1])
  yCN <- c(SECN1[,2],SECN2[,2],SECN3[,2],SECN4[,2],SECN5[,2],SECN6[,2])
  
  #And put all that in a data frame
  df_SECNZ0518 <- data.frame(xCN,yCN,CN_)
  
  #graph with filling :
  plotz0518 <- ggplot(mydata_Z0518, aes(x=iso1, y=iso2, group=factor(group))) +
    theme_light()+  
    geom_point(aes(color=factor(group))) +
    scale_x_continuous(expression({delta}^13*C~'\u2030'))+#, limits = c(-22.5,-16.5)) +
    scale_y_continuous(expression({delta}^15*N~'\u2030'))+#,limits = c(7.5,13.5)) +
    geom_polygon(data=df_SECNZ0518,size=0.6, aes(x=xCN, y=yCN,group=factor(CN_), 
                                                 fill=factor(CN_),col=factor(CN_))) +
    scale_fill_manual(aesthetics = "fill",values=alpha(nichecol,0.34),
                      labels = c("Argyropelecus olfersii",
                                 "Betnhosema glaciale","Lampanyctus crocodilus",
                                 "Maulisia argipalla","Searsia koefoedi",
                                 "Serrivomer beanii"))+
    scale_color_manual(values=nichecol)+
    theme(panel.grid.minor = element_blank(),
          legend.text = element_text(face="italic", size=11),
          legend.title = element_text(size = 11),
          axis.text.x = element_text(size=11),
          axis.text.y = element_text(size=11),
          axis.title.x = element_text(size=11),
          axis.title.y = element_text(size=11)) +
    guides(size="none", linetype="none",col="none", 
           fill=guide_legend(title="Species",
                             keywidth=1.3,
                             title.hjust=0.5))
  plotz0518
}

plot_Z0524 <- function(data){
  
  mydata_Z0524 <- subset(data, community=="6")
  
  siber.CN <- SIBER::createSiberObject(mydata_Z0524)  
  
  nichecol <- c("#F67451","#94B3AE","purple","#00547A","#6B54A0")
  
  #When you want to use SEA corrected for small samples, you have to change the argument "m"
  #like as is done for the ellipse 2 with the coordinates of the ellipse
  ellipse1 <- "6.1"
  tmp <- strsplit(ellipse1, "[.]")
  c.1 <- tmp[[1]][1]
  e.1 <- tmp[[1]][2]
  SECN1 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 1],
                             siber.CN$ML.cov[[1]][ , , 1],
                             m =  siber.CN$sample.sizes[c.1,e.1],
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  
  #Same for group 2
  ellipse2 <- "6.2"
  tmp <- strsplit(ellipse2, "[.]")
  c.2 <- tmp[[1]][1]
  e.2 <- tmp[[1]][2]
  SECN2 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 2],
                             siber.CN$ML.cov[[1]][ , , 2],
                             m = siber.CN$sample.sizes[c.2,e.2],
                             n = 100,
                             p.interval = NULL,
                             small.sample = TRUE,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  
  #And for group 3
  ellipse3 <- "6.3"
  tmp <- strsplit(ellipse3, "[.]")
  c.3 <- tmp[[1]][1]
  e.3 <- tmp[[1]][2]
  SECN3 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 3],
                             siber.CN$ML.cov[[1]][ , , 3],
                             m = siber.CN$sample.sizes[c.3,e.3],
                             small.sample = TRUE,
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  #And for group 4
  ellipse4 <- "6.4"
  tmp <- strsplit(ellipse4, "[.]")
  c.4 <- tmp[[1]][1]
  e.4 <- tmp[[1]][2]
  SECN4 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 4],
                             siber.CN$ML.cov[[1]][ , , 4],
                             m =  siber.CN$sample.sizes[c.4,e.4],
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  #group5
  ellipse5 <- "6.5"
  tmp <- strsplit(ellipse5, "[.]")
  c.5 <- tmp[[1]][1]
  e.5 <- tmp[[1]][2]
  SECN5 <- SIBER::addEllipse(siber.CN$ML.mu[[1]][ , , 5],
                             siber.CN$ML.cov[[1]][ , , 5],
                             m =  siber.CN$sample.sizes[c.5,e.5],
                             n = 100,
                             p.interval = NULL,
                             ci.mean = FALSE,
                             col = "black",
                             lty = 1,
                             lwd = 2)
  

  
  #get the ellipse coordinates into a data frame that. First, we need to
  #repeat the group codes for as long as each ellipse coordinate vector is (100 points by default)
  CN1_ <- rep(1, length(SECN1[,1]))
  CN2_ <- rep(2, length(SECN2[,1]))
  CN3_ <- rep(3, length(SECN3[,1]))
  CN4_ <- rep(4, length(SECN4[,1]))
  CN5_ <- rep(5, length(SECN5[,1]))
  CN_ <- c(CN1_, CN2_, CN3_, CN4_,CN5_)
  
  #Then extract x and y coordinates from the ellipses created before
  xCN <- c(SECN1[,1],SECN2[,1],SECN3[,1],SECN4[,1],SECN5[,1])
  yCN <- c(SECN1[,2],SECN2[,2],SECN3[,2],SECN4[,2],SECN5[,2])
  
  #And put all that in a data frame
  df_SECNZ0524 <- data.frame(xCN,yCN,CN_)
  
  #graph with filling :
  plotz0524 <- ggplot(mydata_Z0524, aes(x=iso1, y=iso2, group=factor(group))) +
    theme_light()+
    geom_point(aes(color=factor(group))) +
    scale_x_continuous(expression({delta}^13*C~'\u2030'))+ #, limits = c(-22.5,-16.5)) +
    scale_y_continuous(expression({delta}^15*N~'\u2030')) +#,limits = c(7.5,13.5)) +
    geom_polygon(data=df_SECNZ0524,size=0.6, aes(x=xCN, y=yCN,group=factor(CN_), 
                                                 fill=factor(CN_),col=factor(CN_))) +
    scale_fill_manual(aesthetics = "fill",values=alpha(nichecol,0.34),
                      labels = c("Argyropelecus olfersii",
                                 "Lampanyctus crocodilus",
                                 "Melanostigma atlanticum","Serrivomer beanii",
                                 "Xenodermichthys copei"))+
    scale_color_manual(values=nichecol)+
    theme(panel.grid.minor = element_blank(),
          legend.text = element_text(face="italic", size=11),
          legend.title = element_text(size = 11),
          axis.text.x = element_text(size=11),
          axis.text.y = element_text(size=11),
          axis.title.x = element_text(size=11),
          axis.title.y = element_text(size=11)) +
    guides(size="none", linetype="none",col="none", 
           fill=guide_legend(title="Species",
                             keywidth=1.3,
                             title.hjust=0.5))
  plotz0524
}