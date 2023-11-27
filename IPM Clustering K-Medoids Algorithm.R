Menginstal Package dan Mengimport data
library(cluster)
library(tidyverse)
library(factoextra)
library(MASS)
library(car)
df1 = df[,3:8]
df
model = lm(Y~., data = df)
df2
df2 = df1 %>% scale()
Summary(model)
#Mengecek outlier
df2
boxplot(df2)
#Scalling atau normalisasi data
df = df %>% select(-`Kab/Kota`) %>% scale()
#Penentuan jumlah klaster optimal dengan 3 metode (silhouette, wss atau elbow, dan gap statistics)
fviz_nbclust(df2,FUNcluster = pam,method = "silhouette")+theme_classic()+labs(subtitle = "Silhouette")
fviz_nbclust(df2,FUNcluster = pam,method = "wss")+theme_classic()+labs(subtitle = "Elbow")
fviz_nbclust(df2,FUNcluster = pam,method = "gap_stat")+theme_classic()
#Melakukan proses klastering dengan algoritma k-medoids dan memvisualisasikannya
clust = pam(df2, 6, metric = "euclidean",stand = F)
clust_medoid = clust$cluster
rownames(df2)=df$Provinsi
fviz_cluster(list(cluster=clust_medoid,data = df1),ellipse.type ="convex")+theme_bw()
#Mengexport data hasil klastering
finalmedoidsklust = data.frame(df,clust_medoid)
finalmedoidsklust = finalmedoidsklust[order(finalmedoidsklust$pam.clust),]
finalmedoidsklust
write.csv(finalmedoidsklust,file = "Hasil Cluster.csv",row.names = T)
#Melakukan pengujian kebaikan hasil klastering
silhouette_widths <- silhouette(clust$clustering, dist(df2))
silhouette_widths
observed_avg_width <- mean(silhouette_widths[, "sil_width"])
silhouette_widths
observed_avg_width
num_permutations <- 1000
permuted_avg_widths <- numeric(num_permutations)
for (i in 1:num_permutations) {
  permuted_clusters <- sample(clust$clustering, replace = FALSE)
  permuted_silhouette_widths <- silhouette(permuted_clusters, dist(df2))
  permuted_avg_widths[i] <- mean(permuted_silhouette_widths[, "sil_width"])
}
p_value <- sum(permuted_avg_widths >= 0.5) / num_permutations
p_value
cat("Rata-rata nilai skor siluet observasi:", observed_avg_width, "\n")
cat("Permutation Test p-value:", p_value, "\n")
hist(num_permutations)
