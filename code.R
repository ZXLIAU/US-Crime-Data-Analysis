setwd("C:/Users/CRYSTAL XUAN/Desktop/大学/112年 大三下/多變量分析/期末報告")
data<-read.table("uscrime.dat")
colnames(data)<-c("land area","population 1985","murder","rape","robbery","assault","burglary","larcery","autothieft","US states region number","US states division number")
data
#X1: land area土地面积;X2: population 1985 1985年人口;X3: murder謀殺
#X4: rape强奸;X5: robbery抢劫;X6: assault袭击;X7: burglary入室盗窃X8: larcery盗窃
#X9: autothieft自动盗窃;X10: US states region number美国各州地区编号
#X11: US states division number美国各州分区编号
data<-data[,c(3,4,5,6,7,8,9)]
colnames(data)<-c("X3謀殺","X4強姦","X5搶劫","X6襲擊","X7入室盜竊","X8偷竊","X9汽車盜竊")
write.csv(data, file = "data.csv")


library(DataExplorer)
plot_missing(data)#沒有missing

pca <- prcomp(data, scale. = TRUE)
summary(pca)
screeplot(pca, type = 'line')

#FA
data_norm <- scale(data)
library('psych')
#PCA
Q_princ <- principal(data.frame(data_norm), nfactors = 2, rotate = "varimax",
                     scores = TRUE, method = "regression")
Q_princ$loadings
score<-c(Q_princ$scores,data$V1)

max(Q_princ$scores[,2])
min(Q_princ$scores[,2])


library(ggpubr)
Q_var <- data.frame(Q_princ$loadings)[1:dim(data_norm)[2],]
colnames(Q_var) <- c("F2", "F1", "F3")
rownames(Q_var) <- colnames(data_norm)
Q_var <- data.frame(Q_var)
ggplot(Q_var, aes(x = F1, y = F2, label = rownames(Q_var))) + geom_text() +
  lims(x = c(-1, 1), y = c(-1, 1)) + labs(title = "First vs. Second Factor") +
  theme(plot.title = element_text(hjust = 0.5))
ggplot(Q_var, aes(x = F2, y = F3, label = rownames(Q_var))) + geom_text() +
  lims(x = c(-1, 1), y = c(-1, 1)) + labs(title = "Second vs. Third Factor") +
  theme(plot.title = element_text(hjust = 0.5))
ggplot(Q_var, aes(x = F1, y = F3, label = rownames(Q_var))) + geom_text() +
  lims(x = c(-1, 1), y = c(-1, 1)) + labs(title = "First vs. Third Factor") +
  theme(plot.title = element_text(hjust = 0.5))
ggarrange(p1, p2, p3, ncol = 2, nrow = 2)

#MLE
#m=2
Q2_ml <- fa(data_norm, covar = TRUE, nfactors = 2, rotate = "Promax", fm = "ml",
           scores = "regression")
Q2_ml
h_2 <- Q2_ml$communality # estimated communalities
psi_2 <- Q2_ml$uniquenesses # estimated specific variances
h_2
psi_2
Q2_ml$PVAL
L_2 <- Q2_ml$loadings # L
LLpsi_2 <- L_2 %*% t(L_2) + diag(psi_2) # Lt(L)+Ψ
LLpsi_2
#m=3
Q3_ml <- fa(data_norm, covar = TRUE, nfactors = 3, rotate = "varimax", fm = "ml",
           scores = "regression")
Q3_ml

h_3 <- Q3_ml$communality # estimated communalities
psi_3 <- Q3_ml$uniquenesses # estimated specific variances
h_3
psi_3

L_3 <- Q3_ml$loadings # L
LLpsi_3 <- L_3 %*% t(L_3) + diag(psi_3) # Lt(L)+Ψ
LLpsi_3

cov(data_norm)
setwd("C:/Users/CRYSTAL XUAN/Desktop/大学/112年 大三下/多變量分析/期末報告")
data<-read.table("uscrime.dat")
data<-data[,c(3,4,5,6,7,8,9)]
colnames(data)<-c("X3謀殺","X4強姦","X5搶劫","X6襲擊","X7入室盜竊","X8偷竊","X9汽車盜竊")

data_norm <- scale(data)
abb<-c(
  "ME" , 
  "NH" , 
  "VT", 
  "MA",  
  "RI",  
  "CT",  
  "NY",  
  "NJ",  
  "PA", 
  "OH",  
  "IN",  
  "IL",  
  "MI",  
  "WI",  
  "MN", 
  "IA",  
  "MO",  
  "ND",  
  "SD",  
  "NE",  
  "KS", 
  "DE",  
  "MD", 
  "VA",
  'WV',  
  'NC', 
  'SC',  
  'GA',  
  'FL',  
  'KY', 
  'TN',  
  'AL',  
  'MS', 
  'AR', 
  'LA',   
  'OK',  
  'TX', 
  'MT',  
  'ID', 
  'WY',  
  'CO', 
  'NM',  
  'AZ',  
  'UT',  
  'NV',  
  'WA',  
  'OR',  
  'CA',  
  "AK", 
  "HI"  
)
library('psych')
#PCA
Q_princ <- principal(data.frame(data_norm), nfactors = 3, rotate = "varimax",
                     scores = TRUE, method = "regression")
Q_princ$loadings

Q_var_sc <- factor.scores(data_norm, f = Q_var, method = "Thurstone")

Q_var_sc <- data.frame(Q_princ$scores)
Q_var_sc["ABB"]<-abb
Q_var_sc
Q_var_sc[order(Q_var_sc$RC1), ]
Q_var_sc[order(Q_var_sc$RC2), ]
Q_var_sc[order(Q_var_sc$RC3), ]

#CA
setwd("C:/Users/CRYSTAL XUAN/Desktop/大学/112年 大三下/多變量分析/期末報告")
data<-read.table("uscrime.dat")
data<-data[,c(3,4,5,6,7,8,9)]
data_norm <- scale(data)
data_dist <- dist(data_norm, method = "canberra", diag = TRUE, upper = TRUE)
# "maximum", "manhattan", "canberra", "binary", "minkowski"
as.matrix(data_dist)[1:5, 1:5]


ht_complete <- hclust(data_dist, method = "ward.D")
#  "single", "complete", "average", "centroid", "median"， "ward.D"
plot(ht_complete, main = "Ward.D Linkage", hang = -1)
abline(h = 4, col = "red", lty = 2)

group <- cutree(ht_complete, h = 6) # k = 4
table(group)

library(dendextend)
complete_col = color_branches(as.dendrogram(ht_complete), k = 3, groupLabels = T)
plot(complete_col, main = "Complete Linkage")

library(tibble)
km_model <- kmeans(data_norm, centers = 3, iter.max = 1000, nstart = 25, algorithm = "Hartigan-Wong")
km_model
km_model$cluster # clustering result
km_model$centers # cluster centers
km_model$totss # total sum of squares
km_model$withinss # within-cluster sum of squares
km_model$tot.withinss # total within-cluster sum of squares
km_model$betweenss # between-cluster sum of squares
km_model$size # cluster size
km_model$iter 

library(tibble)
library(ggplot2)
WCSS <- sapply(2:10, FUN = function(k) {
  pam(data_norm, centers = k)$tot.withinss
})
ggplot(tibble(2:10, WCSS), aes(2:10, WCSS)) + geom_line() +
  geom_vline(xintercept = 4, color = "red", linetype = 2)

fviz_cluster(km_model, data = data_norm, ellipse.type = "norm") +
  labs(title = "K-mean Clustering of Crime Data") +
  theme_minimal()


# K-medoids 
#install.packages("cluster")
library("cluster")
kmedoids_result <- pam(data, k=4, metric = "manhattan", stand = TRUE)
kmedoids_result
# 可视化聚类结果
library(factoextra)

# 绘制聚类结果图
fviz_cluster(kmedoids_result, data = data_norm, ellipse.type = "norm") +
  labs(title = "K-medoids Clustering of Crime Data") +
  theme_minimal()

kmedoids_result$clustering # clustering result
kmedoids_result$medoids # cluster medoids
kmedoids_result$id.med
kmedoids_result$clusinfo
# 提取聚类中心
medoids <- kmedoids_result$medoids

# 计算每个数据点到其对应聚类中心的距离的平方和
ssw <- 0
for (i in 1:4) {
  cluster_points <- data_norm[kmedoids_result$clustering == i, ]
  medoid <- medoids[i, ]
  ssw <- ssw + sum(rowSums((cluster_points - medoid)^2))
}
print(paste("SSW:", ssw))


# 计算总均值
overall_mean <- colMeans(data_norm)
# 计算总平方和 (SST)
sst <- sum(apply(data_norm, 1, function(x) sum((x - overall_mean)^2)))

# 计算组间平方和 (SSB)
ssb <- sst - ssw
print(paste("SSB:", ssb))





kmedoids_result$totss # total sum of squares
kmedoids_result$withinss # within-cluster sum of squares
kmedoids_result$tot.withinss # total within-cluster sum of squares
kmedoids_result$betweenss # between-cluster sum of squares
kmedoids_result$size # cluster size
kmedoids_result$iter 

set.seed(123)  # 設置隨機種子以保證結果的可重複性
k <- 3  # 設定集群的數量
kmeans_result <- kmeans(data_norm, centers = k)

# 查看聚類結果
print(kmeans_result$cluster)
library(ggplot2)

data$cluster <- factor(kmeans_result$cluster)
ggplot(data, aes(x = X3謀殺, y = X4強姦, color = cluster)) +
  geom_point() +
  labs(title = "Cluster Analysis of Crime Data",
       x = "Murder Rate",
       y = "Rape Rate") +
  theme_minimal()

ggplot(data, aes(x = X3謀殺, y = X4強姦, color = cluster)) +
  geom_point() +
  labs(title = "Cluster Analysis of Crime Data",
       x = "謀殺 Rate",
       y = "強姦 Rate") +
  theme_minimal()

ggplot(data, aes(x = X5搶劫, y = X6襲擊, color = cluster)) +
  geom_point() +
  labs(title = "Cluster Analysis of Crime Data",
       x = "Murder Rate",
       y = "Rape Rate") +
  theme_minimal()

ggplot(data, aes(x = X7入室盜竊, y = X8偷竊, color = cluster)) +
  geom_point() +
  labs(title = "Cluster Analysis of Crime Data",
       x = "Murder Rate",
       y = "Rape Rate") +
  theme_minimal()

# 計算距離矩陣
dist_matrix <- dist(data_norm)
# 層次聚類
hclust_result <- hclust(dist_matrix, method = "ward.D2")

# 視覺化層次聚類結果
plot(hclust_result, labels = rownames(data), main = "Hierarchical Clustering Dendrogram")


