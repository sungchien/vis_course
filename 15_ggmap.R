##########################################################
# 利用google map繪製Dot Map
# 把每一個臺北市的Youbike站點的位置繪出，並且表現出它們的熱門程度
#
# 注意：需要先加入google developer帳戶 (google cloud platform)
# 雖然需要信用卡帳戶，但第一年有300美元免費額度
# 啟動至少Geocoding API和Map Static API

library(ggmap)      # 在ggplot2上使用google map的套件
library(tidyverse)

youbike_data <- jsonlite::fromJSON("https://tcgbusfs.blob.core.windows.net/blobyoubike/YouBikeTP.gz")
youbike_site <- youbike_data$retVal %>%
  map(unlist) %>%
  map_dfr(~as.data.frame(t(as.matrix(.)), stringsAsFactors=FALSE)) %>%
  filter(act=="1") %>%     # 取出可使用站點
  select(sno, tot, lat, lng, bemp) %>%
  mutate(tot=as.numeric(tot), bemp=as.numeric(bemp),
         lat_pt=as.numeric(lat), lng_pt=as.numeric(lng)) %>%
  mutate(bz=bemp/tot*100) # 計算熱門程度 (空車位比例愈多, 愈熱門)

## 找出最大和最小的經緯度
max_lng <- max(youbike_site$lng_pt)+0.01
min_lng <- min(youbike_site$lng_pt)-0.01
max_lat <- max(youbike_site$lat_pt)+0.01
min_lat <- min(youbike_site$lat_pt)-0.01

register_google(key = "[[你的api_key]]")

### 取得google map
map <- get_map(location = c(left=min_lng, bottom=min_lat, right=max_lng, top=max_lat),
               language = "zh-TW")

### 畫出熱門站點
ggmap(map) +
  geom_point(aes(x=lng_pt, y=lat_pt, color=bz), alpha=0.7, data=youbike_site)+
  scale_color_viridis_c("熱門程度", option="C")
