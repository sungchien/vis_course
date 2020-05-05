#############################################
# 本次課程繪製雷達圖
# 雷達圖可以視為是長條圖的一種變形，檢視個體在各種類別上對應數值屬性的大小。
# 將色彩對應到另一個類別屬性，可以比較不同類別的個體。
# 
# 由於ggplot2並沒有提供雷達圖，因此利用ggplot提供的原型工具ggproto ，
# 修改極座標(polar coordinate)，使其連接的線為直線，建立雷達圖座標系統。
# 在此雷達圖座標上，再以多邊形的方式畫出每個個體。
# x軸：類別屬性
# y軸：數值屬性
# 色彩：類別屬性
#
# 特別注意事項：分群的種類不可過多
#############################################
library(tidyverse)
library(readxl)
library(ggplot2)
library(fmsb)

# 資料輸入
sightseeing <- read_excel("sightseeing.xlsx")

########################################################################
# 示範案例
# 比較各景點前後兩年觀光客人數
# 個體：所有景點觀光客
# 類別屬性一：各景點
# 類別屬性二：前後兩年
# 數值屬性：觀光客人數

sightseeing <- sightseeing %>%
  mutate(fchi=regexpr("\r\n", 地點)) %>%
  mutate(loc=substr(地點, 1, fchi-1)) %>%
  select(2, 3, 5) 

sightseeing <- sightseeing %>%
  column_to_rownames("loc") %>%
  t()

sightseeing <- sightseeing %>%
  as.data.frame()
# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
sightseeing <- rbind(rep(1000000,7) , rep(0,7) , sightseeing)

# Set graphic colors
library(RColorBrewer)
colors_border <- brewer.pal(2, "Set2")
library(scales)
colors_in <- alpha(colors_border,0.3)

radarchart(sightseeing, axistype=1,
           pcol=colors_border, pfcol=colors_in, plwd=2 , plty=1,
           cglcol="grey", cglty=1,
           axislabcol="black", caxislabels=seq(0,1000000,200000), cglwd=0.8,
           vlcex=0.6)

# Add a legend
legend(x=0.7, y=-0.5,
       legend = rownames(sightseeing[-c(1,2),]), bty = "n",
       pch=20 , col=colors_border , text.col = "black", cex=0.5, pt.cex=2)
