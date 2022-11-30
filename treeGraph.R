# ENV
rm(list = ls())

#  install packages
BiocManager::install("TreeAndLeaf")
BiocManager::install("geneplast")

# load packages
library(TreeAndLeaf)
library(tidyverse)
library(RedeR)
library(igraph)
library(RColorBrewer)

# 数据基本信息
head(USArrests)
#             Murder Assault UrbanPop Rape
# Alabama      13.2     236       58 21.2
# Alaska       10.0     263       48 44.5
# Arizona       8.1     294       80 31.0
# Arkansas      8.8     190       50 19.5
# California    9.0     276       91 40.6
# Colorado      7.9     204       78 38.7

# 构建聚类树
# 使用USArrests的数据集合，基于dist()函数计算距离矩阵
hc <- hclust(dist(USArrests), "ave")

# 将hclust对象转化为树和叶（转化为igraph对象）
tal <- treeAndLeaf(hc)

## 设置图像属性
## - 将数据映射到树和叶
## - refcol = 0, 表示用数据行名作为id
tal <- att.mapv(g = tal, dat = USArrests, refcol = 0)
## - 定义颜色
pal <- brewer.pal(9, "Reds")
tal <- att.setv(g = tal, from="Murder", to="nodeColor",
                cols=pal, nquant=5)
tal <- att.setv(g=tal, from="UrbanPop", to="nodeSize",
                xlim=c(10, 50, 5), nquant=5)

tal <- att.addv(tal, "nodeFontSize", value = 15)
tal <- att.adde(tal, "edgeWidth", value = 3)

## - 调用RedeR应用程序，在交互式R界面中显示树和叶
rdp <- RedPort()
calld(rdp)
resetd(rdp)

## - 将树和叶发送到交互式R界面
addGraph(obj = rdp, g = tal, gzoom=70)

## - relax函数来微调叶子节点
relax(rdp, p1=25, p2=200, p3=5, p5=5, ps=TRUE)

## -添加图列
addLegend.color(obj = rdp, tal, title = "Murder Rate",
                position = "topright")
addLegend.size(obj = rdp, tal, title = "Urban Population Size",
               position = "bottomright")

# ------------- 大型树 -------------- #
# 数据基本情况
head(quakes)
#      lat   long depth mag stations
# 1 -20.42 181.62   562 4.8       41
# 2 -20.62 181.03   650 4.2       15
# 3 -26.00 184.10    42 5.4       43
# 4 -17.97 181.66   626 4.1       19
# 5 -20.42 181.96   649 4.0       11
# 6 -19.68 184.31   195 4.0       12

# 计算距离树
hc <- hclust(dist(quakes), "ave")

# 将距离转化为树和叶
tal <- treeAndLeaf(hc)
tal <- att.mapv(tal, quakes, refcol = 0)

# 设置属性
pal <- brewer.pal(9, "Greens")
tal <- att.setv(g = tal, from = "mag", to = "nodeColor",
                cols = pal, nquant = 10)
tal <- att.setv(g = tal, from = "depth", to = "nodeSize",
                xlim = c(40, 120, 20), nquant = 5)

tal <- att.addv(tal, "nodeFontSize", value = 1,index = V(tal)$isLeaf)
tal <- att.adde(tal, "edgeWidth", value = 10)

# 可视化
rdp <- RedPort()
calld(rdp)
resetd(rdp)

addGraph(obj = rdp, g = tal, gzoom=10)
relax(rdp, p1=25, p2=200, p3=10, p4=100, p5=10, ps=TRUE)

addLegend.color(obj = rdp, tal, title = "Richter Magnitude",
                position = "bottomright")
addLegend.size(obj = rdp, tal, title = "Depth (km)")








