##########################################################
# 利用ggplot2繪製地圖
# Dot Maps 以點表現個體在地圖上的分布
# 本次課程將繪製新北市和臺北市的youbike站點，表現各站點的熱門程度

library(rgdal) # requires sp, will use proj.4 if installed
library(ggplot2)
library(tidyverse)
library(jsonlite)

###
# 從data.gov.tw上下載臺灣鄉鎮市區界線圖，然後解壓縮
# 臺灣鄉鎮市區界線圖是shapefile格式
# 標註各鄉鎮市區的界線
# 以下利用rgdal套件讀取shapefile檔案
tw <- readOGR(dsn="[[shapefile檔案資料夾]]", layer="TOWN_MOI_1071226",
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

## 新北市與臺北市的鄉鎮市區界線圖
county.df <- tw.df[tw.df$COUNTYNAME=="新北市"|tw.df$COUNTYNAME=="臺北市",]
county.data <- tw.data[tw.data$COUNTYNAME=="新北市"|tw.data$COUNTYNAME=="臺北市",]

# 取得新北市youbike資料 (站點、總車位、經緯度、空車位)
file <- "http://data.ntpc.gov.tw/api/v1/rest/datastore/382000000A-000352-001"
youbike_data <- fromJSON(file)
youbike_site1 <- youbike_data$result$records %>%
  filter(act=="1") %>%     # 取出可使用站點
  select(sno, tot, lat, lng, bemp) %>%
  mutate(tot=as.numeric(tot), bemp=as.numeric(bemp),
         lat=as.numeric(lat), lng=as.numeric(lng)) %>%
  mutate(bz=bemp/tot*100) # 計算熱門程度 (空車位比例愈多, 愈熱門)

# 取得臺北市youbike資料 (站點、總車位、經緯度、空車位)
file <- "https://tcgbusfs.blob.core.windows.net/blobyoubike/YouBikeTP.gz"
youbike_data <- fromJSON(file)
youbike_site2 <- youbike_data$retVal %>%
  map(unlist) %>%
  map_dfr(~as.data.frame(t(as.matrix(.)), stringsAsFactors=FALSE)) %>%
  filter(act=="1") %>%     # 取出可使用站點
  select(sno, tot, lat, lng, bemp) %>%
  mutate(tot=as.numeric(tot), bemp=as.numeric(bemp),
         lat=as.numeric(lat), lng=as.numeric(lng)) %>%
  mutate(bz=bemp/tot*100) # 計算熱門程度 (空車位比例愈多, 愈熱門)

# 合併新北市和臺北市的youbike資料
youbike_site <- rbind(youbike_site1, youbike_site2)

### 畫出Dot Map
ggplot() + 
  geom_polygon(data=county.df,
               aes(x=long, y=lat, group=group, fill=COUNTYNAME), color="steelblue") +
  geom_point(data=youbike_site,
             aes(x=lng, y=lat, color=bz), alpha=0.7) +
  coord_equal() +
  scale_x_continuous(NULL, breaks=NULL) +
  scale_y_continuous(NULL, breaks=NULL) +
  scale_color_distiller("熱門程度", palette="Reds", limits=c(0, 100), direction=1) +
  scale_fill_manual(values=c("lightblue", "skyblue")) +
  labs(title="新北市和臺北市的Youbike站程度圖") +
  guides(fill = "none") +
  theme(panel.background = element_blank())
