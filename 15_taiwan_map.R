##########################################################
# 利用ggplot2繪製地圖
# 主要以多邊形構成地圖的形狀
# shapefile是一種常見的地圖檔案格式
# 本次課程利用臺灣鄉鎮市區界線圖畫出臺灣或某一指名縣市的鄉鎮市區圖

library(rgdal) # requires sp, will use proj.4 if installed
library(ggplot2)
library(tidyverse)

###
# 從data.gov.tw上下載臺灣鄉鎮市區界線圖，然後解壓縮
# 臺灣鄉鎮市區界線圖是shapefile格式
# 標註各鄉鎮市區的界線
# 以下利用rgdal套件讀取shapefile檔案
tw <- readOGR(dsn="[[shapefile的路徑]]", layer="TOWN_MOI_1071226",
              encoding="UTF-8", use_iconv=TRUE,
              stringsAsFactors=FALSE)

# 取得各鄉鎮市區的代表點位置
tw.coords <- coordinates(tw)
colnames(tw.coords) <- c("long_pos", "lat_pos")
tw.data <- cbind(tw@data, tw.coords)

# 產生各鄉鎮市區的界線特徵點
tw@data$id <- rownames(tw@data)
tw.ff <- fortify(tw)
tw.df <- left_join(tw@data, tw.ff, by = "id")

# 根據鄉鎮市區的界線特徵點，利用geom_polygon畫出全臺灣的鄉鎮市區
# 註：需要較大計算資源，可省略，直接使用各縣市資料
ggplot() + 
  geom_polygon(data=tw.df, aes(x=long, y=lat, group=group),
               fill="orange", color="black") +
  coord_equal() +
  scale_x_continuous(NULL, breaks=NULL) +
  scale_y_continuous(NULL, breaks=NULL) +
  labs(title="台灣鄉鎮市區界線圖") +
  theme(legend.position = "none",
        panel.background = element_rect(fill="white"))

# 取出某一縣市的shapefile資料
# 單獨畫出這個縣市的鄉鎮市區
county = "臺北市"
county.df = tw.df[tw.df$COUNTYNAME==county,]
county.data <- tw.data[tw.data$COUNTYNAME==county,]

# 利用ggplot2的sclae_fill_brewer函數將各鄉鎮市區填上不同顏色
ggplot() + 
  geom_polygon(data=county.df, aes(x=long, y=lat, group=group, fill=group),
               color="blue") +
  geom_text(data=county.data, aes(x=long_pos, y=lat_pos, label=TOWNNAME),
            color="black") +
  coord_equal() +
  scale_x_continuous(NULL, breaks=NULL) +
  scale_y_continuous(NULL, breaks=NULL) +
  scale_fill_brewer(palette="Paired") +
  labs(title=paste0(county, "鄉鎮市區地圖")) +
  theme(title=element_text(size=20), legend.position = "none",
        panel.background = element_rect(fill="white"))
