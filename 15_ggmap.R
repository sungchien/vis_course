##########################################################
# 利用google map繪製Dot Map
# 把每一個臺北市的Youbike站點的位置繪出，並且表現出它們的熱門程度
#
# 注意：需要先加入google cloud platform帳戶
# https://cloud.google.com/
# 雖然需要信用卡帳戶，但第一年有300美元免費額度
# 新增一個專案
# 啟動至少Geocoding API和Map Static API
# 建立憑證

library(ggmap)      # 在ggplot2上使用google map的套件
library(tidyverse)
library(jsonlite)
library(lubridate)  # 日期時間套件

# 臺北市Youbike 站點資料
dat_addr <- "https://tcgbusfs.blob.core.windows.net/blobyoubike/YouBikeTP.gz"
youbike_data <- fromJSON(dat_addr)
youbike_site <- youbike_data$retVal %>%
  map(unlist) %>%
  map_dfr(~as.data.frame(t(as.matrix(.)), stringsAsFactors=FALSE)) %>%
  filter(act=="1")    %>% # 取出可使用站點
  mutate(tot=as.numeric(tot), bemp=as.numeric(bemp), # 改為數值資料
         lat=as.numeric(lat), lng=as.numeric(lng)) %>%
  mutate(bz=bemp/tot*100) %>% # 計算熱門程度 (空車位比例愈多, 愈熱門)
  mutate(mtime=sprintf("%s-%s-%s %s:%s", substr(mday, 1, 4), substr(mday, 5, 6),
                       substr(mday, 7, 8), substr(mday, 9, 10), substr(mday, 11, 12)))
  
area <- "文山區"
youbike_site %<>%
  filter(sarea==area) %>%       # 取得行政區資料
  select(sno, lat, lng, bz, mtime)    # 站點編號、緯度、經度、空車位數比例、時間

## 找出最大和最小的經緯度
max_lng <- max(youbike_site$lng)+0.01
min_lng <- min(youbike_site$lng)-0.01
max_lat <- max(youbike_site$lat)+0.01
min_lat <- min(youbike_site$lat)-0.01

register_google(key = "[[你的api_key]]")

### 取得google map
map <- get_map(location = c(left=min_lng, bottom=min_lat, right=max_lng, top=max_lat),
               language = "zh-TW")

### 畫出各站點的熱門程度
tit_text <- paste0("臺北市", area, "Youbike站點的熱門程度")
sub_text <- paste0("時間：", youbike_site$mtime[1])
ggmap(map) +
  geom_point(aes(x=lng, y=lat, color=bz), alpha=0.7, size=3, data=youbike_site)+
  scale_color_viridis_c("熱門程度", option="C", limits=c(0, 100)) +
  labs(title=tit_text, subtitle = sub_text) +
  theme(axis.text=element_blank(), axis.ticks=element_blank(),
        axis.title=element_blank())
