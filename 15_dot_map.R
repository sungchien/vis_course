##########################################################
# 利用ggplot2繪製地圖
# Dot Maps 以點表現個體在地圖上的分布
# 本次課程將繪製新北市和臺北市的youbike站點，表現各站點的熱門程度

library(rgdal) # requires sp, will use proj.4 if installed
library(ggplot2)
library(tidyverse)

###
# 從data.gov.tw上下載臺灣鄉鎮市區界線圖，然後解壓縮
# 臺灣鄉鎮市區界線圖是shapefile格式
# 標註各鄉鎮市區的界線
# 以下利用rgdal套件讀取shapefile檔案
tw <- readOGR(dsn=".", layer="TOWN_MOI_1071226",
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
county.df = tw.df[tw.df$COUNTYNAME=="新北市"|tw.df$COUNTYNAME=="臺北市",]
county.data <- tw.data[tw.data$COUNTYNAME=="新北市"|tw.data$COUNTYNAME=="臺北市",]

# 取出新北市youbike資料 (站點、總車位、經緯度、空車位)
youbike_data <- jsonlite::fromJSON("http://data.ntpc.gov.tw/api/v1/rest/datastore/382000000A-000352-001")
youbike_site1 <- youbike_data$result$records %>%
  filter(act=="1") %>%     # 取出可使用站點
  select(sno, tot, lat, lng, bemp) %>%
  mutate(tot=as.numeric(tot), bemp=as.numeric(bemp),
         lat_pt=as.numeric(lat), lng_pt=as.numeric(lng)) %>%
  mutate(bz=bemp/tot*100) # 計算熱門程度 (空車位比例愈多, 愈熱門)

# 取出臺北市youbike資料 (站點、總車位、經緯度、空車位)
youbike_data <- jsonlite::fromJSON("https://tcgbusfs.blob.core.windows.net/blobyoubike/YouBikeTP.gz")
youbike_site2 <- youbike_data$retVal %>%
  map(unlist) %>%
  map_dfr(~as.data.frame(t(as.matrix(.)), stringsAsFactors=FALSE)) %>%
  filter(act=="1") %>%     # 取出可使用站點
  select(sno, tot, lat, lng, bemp) %>%
  mutate(tot=as.numeric(tot), bemp=as.numeric(bemp),
         lat_pt=as.numeric(lat), lng_pt=as.numeric(lng)) %>%
  mutate(bz=bemp/tot*100) # 計算熱門程度 (空車位比例愈多, 愈熱門)

# 合併新北市和臺北市的youbike資料
youbike_site <- rbind(youbike_site1, youbike_site2)

### 畫出Dot Map
# 每一個
ggplot() + 
  geom_polygon(data=county.df, aes(x=long, y=lat, group=group, fill=COUNTYNAME), color="black") +
  geom_point(data=youbike_site, aes(x=lng_pt, y=lat_pt, color=bz), alpha=0.7) +
  #geom_text(data=county.data, aes(x=long_pos, y=lat_pos, label=TOWNNAME), color="black") +
  coord_equal() +
  scale_x_continuous(NULL, breaks=NULL) +
  scale_y_continuous(NULL, breaks=NULL) +
  scale_color_viridis_c("熱門程度") +
  scale_fill_manual(values=c("mistyrose", "lightpink")) +
  labs(title="Youbike站點地圖") +
  guides(fill = "none") +
  theme(title=element_text(size=20))
