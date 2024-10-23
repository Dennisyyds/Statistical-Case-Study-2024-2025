source("~/Desktop/Stat Case study/Semester1/Assignemtn1/CaseStudy Assignment1/stylometryfunctions.R")
M <- loadCorpus("~/Desktop/Stat Case study/Semester1/Assignemtn1/CaseStudy Assignment1/FunctionWords/","frequentwords70")
M$authornames

#Visualization MDS

#Similarity of Author

# Initialize an empty matrix
x <- NULL
# Loop through each feature set in M$features
for (i in 1:length(M$features)) {
  # Sum the features column-wise and bind them to the matrix x
  x <- rbind(x, apply(M$features[[i]], 2, sum))
}

# Normalize each row in x so that the sum of each row equals 1
for (i in 1:nrow(x)) {
  x[i,] <- x[i,] / sum(x[i,])
}

# Standardize each column in x
for (j in 1:ncol(x)) {
  x[,j] <- (x[,j] - mean(x[,j])) / sd(x[,j])
}

# Calculate the distance matrix and perform classical MDS
d <- dist(x)
pts <- cmdscale(d)

plot(pts,type = 'n') 
text(pts[,1],pts[,2],label=M$authornames,cex=0.8)
# Plot
plot(pts, type='n', main="Authors Similarity Visualization",
     xlab="Dimension 1", ylab="Dimension 2", 
     xlim=c(min(pts[,1]) - 1, max(pts[,1]) + 1),
     ylim=c(min(pts[,2]) - 1, max(pts[,2]) + 1),
     pch=19, col=alpha("blue", 0.5),
     cex.lab=0.7,
     cex.main=0.8)
# Add text labels
text(pts[,1], pts[,2], labels=M$authornames, 
     cex=0.8, pos=3, col="darkred", font=2)
# Add grid lines
grid()


##############################################################
#plot each book
colorlist<-c()
x <- NULL
z = 0
for (i in 1:length(M$features)){
  for (j in 1:nrow(M$features[[i]])){
    x <- rbind(x,M$features[[i]][j,])
    z = z+1
    colorlist[z] = i
  }
}
for (i in 1:nrow(x)) { x[i,] <- x[i,] / sum(x[i,]) }

for (j in 1:ncol(x)) { x[,j] <- (x[,j] - mean(x[,j]))/sd(x[,j]) }

d <- dist(x)
pts <- cmdscale(d)
# Plot
plot(pts, type = 'n', main = "Visualizing Similarities Among All the Books",
     xlab = "Dimension 1", ylab = "Dimension 2",
     xlim = range(pts[, 1]) + c(-1, 1), 
     ylim = range(pts[, 2]) + c(-1, 1)
     ,cex.lab=0.7,
     cex.main=0.8)

# Add text labels with color coding based on author
text(pts[, 1], pts[, 2], labels = 1:nrow(x), 
     col = colorlist, 
     cex = 0.8, pos = 3, font = 2)

legend("topright", legend = M$authornames, col = 1:12,pch=20,cex = 0.8)
# Add grid lines for better readability
grid()
#########################################################################
#plot each book
Need <- c(3,4,6,7,9,10,11)
colorlist<-c()
x <- NULL
z = 0
y = 0
for (i in Need){
  y = y+1
  for (j in 1:nrow(M$features[[i]])){
      x <- rbind(x,M$features[[i]][j,])
      z = z+1
      colorlist[z] = y
  }
}
for (i in 1:nrow(x)) { x[i,] <- x[i,] / sum(x[i,]) }

for (j in 1:ncol(x)) { x[,j] <- (x[,j] - mean(x[,j]))/sd(x[,j]) }

d <- dist(x)
pts <- cmdscale(d)
# Plot
plot(pts, type = 'n', main = "Visualizing Similarities Among some Books",
     xlab = "Dimension 1", ylab = "Dimension 2",
     xlim = range(pts[, 1]) + c(-1, 1), 
     ylim = range(pts[, 2]) + c(-1, 1)
     ,cex.lab=0.7,
     cex.main=0.8)

# Add text labels with color coding based on author
text(pts[, 1], pts[, 2], labels = 1:nrow(x), 
     col = colorlist, 
     cex = 0.8, pos = 3, font = 2)

legend("topright", legend = c("MaryAndPercy","Mary", "Percy","PercyPoetry","Unknown","WalterScott","WilliamGodwin"), col = c(1,2,3,4,5,6,7),pch=20,cex = 0.8)
# Add grid lines for better readability
grid()

