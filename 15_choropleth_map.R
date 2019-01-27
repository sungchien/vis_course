##########################################################
# 利用ggplot2繪製Choropleth Maps
# Choropleth Maps 以多邊形的填滿色彩(fill)表現各地區(個體)的某一屬性
# 本課程利用Choropleth Map呈現每一鄉鎮市區每人耗電資料

library(rgdal) # requires sp, will use proj.4 if installed
library(ggplot2)
library(tidyverse)
library(rvest)

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

###
# 各鄉鎮市區人口資料
pop <- jsonlite::fromJSON("http://od.moi.gov.tw/api/v1/rest/datastore/301000000A-000605-040")
# 各鄉鎮市區以及其人口總數
pop.data <- pop$result$records %>%
  filter(statistic_yyy!="") %>%
  slice(2:(nrow(.)-2)) %>%
  mutate(people_total=as.numeric(people_total)) %>%
  select(site_id, people_total)

# 台電用電資料提供以郵遞區號為區域，有鄉鎮市區名稱，但無縣市名稱，
# 為避免類似臺北市大安區與臺北市大安區的混淆，
# 需將利用郵遞區號對照表轉為有縣市名稱的資料

# 先取得郵遞區號鄉鎮市區對照表
# 郵遞區號鄉鎮市區對照表為xml格式
con <- "http://download.post.gov.tw/post/download/1050812_%E8%A1%8C%E6%94%BF%E5%8D%80%E7%B6%93%E7%B7%AF%E5%BA%A6%28toPost%29.xml"

xml_data <- readLines(con) %>%
  paste(collapse="\n")
Encoding(xml_data) <- "UTF-8" # 轉換編碼方式
xml_data <- read_xml(xml_data)

# 從每一個區域資料節點中取出各區域的名稱以及郵遞區號
# 除了新竹市和嘉義市，每一個區域基本上對應一個鄉鎮市區
# 因為區域資料節點的標籤為中文，故取得其子節點中具有英文標籤者
tag_name <- "TGOS_URL"
# 找到區域資料節點
pnodes <- xml_parent(xml_nodes(xml_data, tag_name))
# 各區域的名稱
c1text <- sapply(pnodes, function (x) xml_text(xml_child(x)))
# 各區域的名稱和郵遞區號
area_df <- data.frame(COUNTYNAME=substr(c1text, 1, 3),
                      TOWNNAME=sapply(c1text, function (x) substr(x, 4, nchar(x))),
                      COUNTY_TOWN = c1text,
                      zipcode=sapply(pnodes, function (x) xml_text(xml_child(x, 2))),
                      stringsAsFactors = FALSE)

# 取得鄉鎮市區用電資料
dt.data <- readODS::read_ods("台灣電力公司_鄉鎮市(郵遞區)別用電統計資料.ods") %>%
  filter(grepl("^3表", 用電種類)) %>%
  inner_join(area_df, by=c("郵遞區號"="zipcode")) %>% # 根據郵遞區號鄉鎮市區對照表，找出縣市與鄉鎮市區名稱
  select(-用電種類, -用戶數, -契約容量)

# 將各鄉鎮市區用電資料和人口總數合併
dt.data <- dt.data %>% 
  inner_join(pop.data, by=c("COUNTY_TOWN"="site_id"))

# 計算每人平均用電量
dt.data <- dt.data %>%
  mutate(avg_power = `售電度數(當月)`/people_total) %>%
  select(COUNTYNAME, TOWNNAME, avg_power)

# 找出某一縣市每人平均用電地圖
county <- "桃園市"
county.df <- tw.df %>%
  filter(tw.df$COUNTYNAME==county) %>%
  left_join(dt.data, by=c("COUNTYNAME", "TOWNNAME")) # 將每人平均用電量資料和地圖合併
county.data <- tw.data[tw.data$COUNTYNAME==county,]

# Choropleth Map
# 多邊形表示各鄉鎮市區 geom_polygon
# 多邊形的色彩表示該鄉鎮市區的人均用電量
# 各鄉鎮市區名稱 geom_text
ggplot(county.df) + 
  geom_polygon(aes(x=long, y=lat, group=group, fill=avg_power), color="blue") +
  geom_text(data=county.data, aes(x=long_pos, y=lat_pos, label=TOWNNAME), color="white") +
  coord_equal() +
  scale_x_continuous(NULL, breaks=NULL) +
  scale_y_continuous(NULL, breaks=NULL) +
  scale_fill_viridis_c(option="D", "人均用電") +
  labs(title=paste0(county, "用電地圖")) +
  theme(title=element_text(size=20),
        panel.background = element_rect(fill="white"))
