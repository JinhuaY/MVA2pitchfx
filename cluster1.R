# clear history
rm(list = ls(all = TRUE))
graphics.off()

# Install packages if not installed
libraries = c("ggplot2", "pitchRx", "car")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

# Load packages
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

data(pitches)

RM=pitches[which(pitches$pitcher_name=='Mariano Rivera'), ]
PH=pitches[which(pitches$pitcher_name=='Phil Hughes'), ]

cluva=c("start_speed", "break_y" , "spin_dir", "spin_rate", "break_angle", "break_length")

RMC=RM[cluva]
PHC=PH[cluva]

RMM=matrix(as.numeric(unlist(RMC)),nrow=912, ncol=6)
RMM=scale(RMM)
PHM=matrix(as.numeric(unlist(PHC)),nrow=946, ncol=6)
PHM=scale(PHM)

# PCA
#RM
eigen1  = eigen(cov(RMM))  # spectral decomposition  
eva1  = eigen1$values
eve1  = eigen1$vectors
y1    = RMM %*% eve1    

#plot variance explained
plot(eva1, xlab = "Index", ylab = "Lambda", 
     main = "Mariano Rivera: variance \n explained by eigenvectors", type="b")

#plot all the observations on first and second PC coordinates
plot(y1[,1],y1[,2],main = "Pitches from Mariano Rivera",
     xlab = "first PC", ylab = "second PC", col=as.factor(RM$pitch_type))
legend("bottomleft", c("Pitch type: FC","Pitch type: FF"), 
       pch=1, col = c(1,2), cex = 0.75)

#plot factor loadings
plot(eve1[, 1], type = "o", col = "red3", pch = 1,xlab = "Subindex", 
     xaxt = "n", ylab = "Percentage [%]", ylim = c(-0.6,0.9),
     main = "Factor Loadings for Mariano Rivera", lwd = 2)
lines(eve1[, 2], col = "blue3", lwd = 2,pch = 1,type = "o")
axis(1, at = c(1:6), las = 0)
legend("topright", c("First PC", "Second PC"), lwd = 2, 
       col = c("red3", "blue3"), cex = 0.8)

#PH
eigen2  = eigen(cov(PHM))  # spectral decomposition  
eva2  = eigen2$values
eve2  = eigen2$vectors
y2    = PHM %*% eve2
 
#plot variance explained
plot(eva2, xlab = "Index", ylab = "Lambda", 
     main = "Phil Hughes: variance \n explained by eigenvectors", type="b")

#plot all the observations on first and second PC coordinates
plot(y2[,1],y2[,2],main = "Pitches from Phil Hughes",xlab = "first PC", 
     ylab = "second PC", col=as.factor(PH$pitch_type))
legend("bottomleft", c("Pitch type: FC","Pitch type: FF"), 
       pch=1, col = c(1,2), cex = 0.75)

#plot factor loadings
plot(eve2[, 1], type = "o", col = "red3", pch = 1,xlab = "Subindex", 
     xaxt = "n", ylab = "Percentage [%]", ylim = c(-0.6,1.0),
     main = "Factor Loadings for Phil Hughes", lwd = 2)
lines(eve2[, 2], col = "blue3", lwd = 2,pch = 1,type = "o")
axis(1, at = c(1:6), las = 0)
legend("topright", c("First PC", "Second PC"), lwd = 2, 
       col = c("red3", "blue3"), cex = 0.8)



#ward algorithm
#RM
d1 = dist(RMM, "euclidean", p = 2)   # euclidean distance matrix
w1 = hclust(d1, method = "ward.D")   
plot(w1, hang = -0.1, labels = FALSE, frame.plot = TRUE, ann = FALSE)
title(main = "Dendrogram for pitches from Mariano Rivera", 
      xlab = "Ward algorithm",
      ylab = "Squared euclidean distance")

group1 = cutree(w1, h = 300)

plot(y1[,1],y1[,2],main = "Mariano Rivera_Ward's Method",
     xlab = "First PC", ylab = "Second PC", col=as.factor(RM$pitch_type),
     pch=as.numeric(as.factor(group1)))
legend("bottomleft", c("Pitch type: FC","Pitch type: FF","First cluster","Second cluster"),
       pch=c(20,20,2,1), col = c(1,2,1,1),cex = 0.75)




#PH
d2 = dist(PHM, "euclidean", p = 2)   # euclidean distance matrix
w2 = hclust(d2, method = "ward.D")   
plot(w2, hang = -0.1, labels = FALSE, frame.plot = TRUE, ann = FALSE)
title(main = "Dendrogram for pitches from Phil Hughes", 
      xlab = "Ward algorithm",
      ylab = "Squared euclidean distance")

group2 = cutree(w2, h = 400)

# Plot 3: PCA with clusters: Ward

plot(y2[,1],y2[,2],main = "Phil Hughes_Ward's Method",
     xlab = "First PC", ylab = "Second PC", col=as.factor(PH$pitch_type),
     pch=as.numeric(as.factor(group2)))
legend("bottomleft", c("Pitch type: FC","Pitch type: FF","First cluster","Second cluster"),
       pch=c(20,20,2,1), col = c(1,2,1,1),cex = 0.75)


#cluster analysis results compared with original classification
table(RM$pitch_type, group1)
table(PH$pitch_type, group2)





