#libraries & packages
library("ggplot2")
library("Factoshiny")
library("FactoMineR") 
library("factoextra")
library("ggbiplot")
install.packages("ggbiplot")

#PCA analysis percentages

vis.pca <- PCA(data_perc[,3:10], graph = FALSE)

#PCA analysis log-transformed measurements

vis3.pca <- PCA(log2vissen[,13:23], graph = FALSE) 

#PCA analysis smaller dataset
vis2.pca <- PCA(datagrotevissen[,3:10], graph = FALSE)
 
#PCA log-transformed

p <- fviz_pca_ind(vis3.pca, axes = c(2, 3), label="none", habillage=as.factor(log2vissen$Species),
                  addEllipses=TRUE, ellipse.type="convex", ellipse.level=0.95) +
  geom_point(aes(shape = as.factor(log2vissen$sex), color = as.factor(log2vissen$Species))) +
  scale_shape_manual(values = c("M" = 1, "F" = 2)) +
  scale_color_manual(values = c("alt." = "red", "ang." = "blue"))  

p <- fviz_pca_ind(vis3.pca, axes = c(1, 3), label="none", habillage=as.factor(log2vissen$Species),
                  addEllipses=TRUE, ellipse.type="convex", ellipse.level=0.95) +
  geom_point(aes(shape = as.factor(log2vissen$sex), color = as.factor(log2vissen$Species))) +
  scale_shape_manual(values = c("M" = 1, "F" = 2)) +
  scale_color_manual(values = c("alt." = "red", "ang." = "blue"))

p <- fviz_pca_ind(vis3.pca, axes = c(1, 2), label="none", habillage=as.factor(log2vissen$Species),
                  addEllipses=TRUE, ellipse.type="convex", ellipse.level=0.95) +
  geom_point(aes(shape = as.factor(log2vissen$sex), color = as.factor(log2vissen$Species))) +
  scale_shape_manual(values = c("M" = 1, "F" = 2)) +
  scale_color_manual(values = c("alt." = "red", "ang." = "blue"))

# Print PCA percentages
p <- fviz_pca_ind(vis.pca, axes = c(1, 2), label="none", habillage=as.factor(data_perc$Species),
                  addEllipses=TRUE, ellipse.type="convex", ellipse.level=0.95) +
  geom_point(aes(shape = as.factor(data_perc$sex), color = as.factor(data_perc$Species))) +
  scale_shape_manual(values = c("M" = 1, "F" = 2)) +
  scale_color_manual(values = c("alt." = "red", "ang." = "blue"))



#hypothesis testing 
shapiro.test(datagrotevissen$pertooth) 
model1 <- aov(pertooth ~ Species, data = datagrotevissen)
duncan_test1 <- HSD.test(model1, "Species", console = TRUE) 
shapiro.test(datagrotevissen$perh) 
model2 <- aov(perh ~ Species, data = datagrotevissen)
duncan_test2 <- HSD.test(model2, "Species", console = TRUE) 
print(summary(model2)) 



#add PC scores to data frame

warnings()
log2vissen$pc1 <- vis3.pca$ind$coord[, 1]
log2vissen$pc2 <- vis3.pca$ind$coord[, 2] 
log2vissen$pc3 <- vis3.pca$ind$coord[, 3] 


data_perc$pc1 <- vis.pca$ind$coord[, 1]
data_perc$pc2 <- vis.pca$ind$coord[, 2] 
data_perc$pc3 <- vis.pca$ind$coord[, 3] 
data_perc$pc4 <- vis.pca$ind$coord[, 4] 
fviz_screeplot(vis.pca, addlabels = TRUE)

datagrotevissen$pc1 <- vis2.pca$ind$coord[, 1]
datagrotevissen$pc2 <- vis2.pca$ind$coord[, 2]

#plots with SL for Y-axis

if (!requireNamespace("ggpmisc", quietly = TRUE)) {
  install.packages("ggpmisc")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggpmisc)
library(ggplot2)
library(readxl)
dataset <- read_excel("dataset.xlsx")


# SL vs pc1
ggplot(data = data_perc, aes(x = SL, y = pc1, color = Species, group = Species, shape = sex)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = after_stat(eq.label)), formula = y ~ x) +
  geom_point() +
  scale_shape_manual(values = c("F" = 17, "M" = 16)) +
  labs(x = "Standard length [mm]", y = "Coordinates of samples along PC I (percentages)",
       shape = "Sex", color = "Species") 


#print factor loadings
barplot(vis.pca$var$contrib[,1], main="Factor Loadings for PC1", xlab="Variables", ylab="Loadings", names.arg=colnames(data_perc[,3:10]))
barplot(vis.pca$var$contrib[,1], main="Factor Loadings for PC1", xlab="Variables", ylab="Loadings", names.arg=colnames(log2vissen[,13:23])) 
barplot(vis.pca$var$contrib[,2], main="Factor Loadings for PC2", xlab="Variables", ylab="Loadings", names.arg=colnames(log2vissen[,13:23]))


# SL vs variables

ggplot(data = data_perc, aes(x = SL, y = perh, color = Species, group = Species, shape = sex)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = after_stat(eq.label)), formula = y ~ x) +
  geom_point() +
  scale_shape_manual(values = c("F" = 17, "M" = 16)) +
  labs(x = "Standard length [mm]", y = "relative head width",
       shape = "Sex", color = "Species") 

ggplot(data = data_perc, aes(x = SL, y = pertooth, color = Species, group = Species, shape = sex)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = after_stat(eq.label)), formula = y ~ x) +
  geom_point() +
  scale_shape_manual(values = c("F" = 17, "M" = 16)) +
  labs(x = "Standard length [mm]", y = "Relative lower tooth pad width",
       shape = "Sex", color = "Species") 

ggplot(data = data_perc, aes(x = SL, y = perintorb, color = Species, group = Species, shape = sex)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = after_stat(eq.label)), formula = y ~ x) +
  geom_point() +
  scale_shape_manual(values = c("F" = 17, "M" = 16)) +
  labs(x = "Standard length [mm]", y = "relative interorbital width",
       shape = "Sex", color = "Species")

ggplot(data = data_perc, aes(x = SL, y = perorb, color = Species, group = Species, shape = sex)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = after_stat(eq.label)), formula = y ~ x) +
  geom_point() +
  scale_shape_manual(values = c("F" = 17, "M" = 16)) +
  labs(x = "Standard length [mm]", y = "relative orbital length",
       shape = "Sex", color = "Species") 

ggplot(data = data_pec_ray, aes(x = SL, y = pec_ray, color = Species, group = Species, shape = Sex)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = after_stat(eq.label)), formula = y ~ x) +
  geom_point() +
  scale_shape_manual(values = c("F" = 17, "M" = 16)) +
  labs(x = "Standard length [mm]", y = "longest pectoral ray length [mm]",
       shape = "Sex", color = "Species")


#print citation
RStudio.Version()

