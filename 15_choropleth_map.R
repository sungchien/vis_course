##########################################################
# 利用ggplot2繪製Choropleth Maps
# Choropleth Maps 以多邊形的填滿色彩(fill)表現各地區(個體)的某一數值屬性
# 本課程利用Choropleth Map呈現每一鄉鎮市區每人耗電資料

library(rgdal) # requires sp, will use proj.4 if installed
library(ggplot2)
library(tidyverse)
library(jsonlite)
library(xml2)
library(readODS)

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

###
# 各鄉鎮市區人口資料
file <- "http://od.moi.gov.tw/api/v1/rest/datastore/301000000A-000605-040"
pop <- fromJSON(file)
# 各鄉鎮市區以及其人口總數
pop.data <- pop$result$records %>%
  filter(statistic_yyy!="") %>%
  slice(2:(n()-2)) %>%
  mutate(people_total=as.numeric(people_total)) %>%
  select(area=site_id, people_total)

# 取得鄉鎮市區用電資料
dt.data <- read_ods("[[檔案所在資料夾]]/台灣電力公司_鄉鎮市(郵遞區)別用電統計資料.ods") %>%
  filter(grepl("^3表", 用電種類)) %>%
  select(zipcode=`郵遞區號`, town=`行政區`, pow_use=`售電度數(當月)`)

# 台電用電資料提供以郵遞區號為區域，有鄉鎮市區名稱，但無縣市名稱，
# 為避免類似臺北市大安區與臺北市大安區的混淆，
# 需將利用郵遞區號對照表轉為有縣市名稱的資料

# 先取得郵遞區號鄉鎮市區對照表
# 郵遞區號鄉鎮市區對照表為xml格式
con <- "http://download.post.gov.tw/post/download/1050812_%E8%A1%8C%E6%94%BF%E5%8D%80%E7%B6%93%E7%B7%AF%E5%BA%A6%28toPost%29.xml"
xml_data <- read_xml(con, encoding="UTF-8")

# 每個資料節點的資料
getData <- function (x) {
  # field_data： 各欄位的xml資料 
  field_data <- xml_children(x)
  
  # 欄位值
  f_value <- xml_text(field_data)
  # 欄位名稱
  names(f_value) <- xml_name(field_data)
  
  # 傳回一個向量，向量上的值是各欄位的資料
  return(f_value)
}

# 取得各鄉鎮市區的郵遞區號
t_data <- sapply(xml_children(xml_data), getData)

# 資料處理：矩陣轉置並儲存成tibble，選取行政區名和郵遞區號
area_df <- t_data %>%
  t() %>%                             # 轉置t_data矩陣
  as_tibble() %>%                     # 轉換成tibble格式
  select(area = "行政區名", zipcode ="_x0033_碼郵遞區號") %>%                  # 選取行政區名和郵遞區號兩個欄位
  mutate(COUNTYNAME=substr(area, 1, 3),
         TOWNNAME=sapply(area, function (x) substr(x, 4, nchar(x))))

# 將各鄉鎮市區用電資料合併郵遞區號鄉鎮市區對照表，找出縣市與鄉鎮市區名稱
dt.data <- dt.data %>%
  inner_join(area_df) # 根據郵遞區號合併

# 將各鄉鎮市區用電資料和人口總數合併
dt.data <- dt.data %>%
  left_join(pop.data)

# 計算每人平均用電量
dt.data <- dt.data %>%
  mutate(avg_power = pow_use/people_total) %>%
  select(COUNTYNAME, TOWNNAME, avg_power)

# 找出某一縣市鄉鎮市區界線圖
county <- "桃園市"
county.df <- tw.df %>%
  filter(tw.df$COUNTYNAME==county) %>%
  left_join(dt.data, by=c("COUNTYNAME", "TOWNNAME")) # 將每人平均用電量資料和地圖合併
county.data <- tw.data[tw.data$COUNTYNAME==county,]

# Choropleth Map
# 多邊形表示各鄉鎮市區 geom_polygon
# 多邊形的色彩表示該鄉鎮市區的每人平均用電量
# 各鄉鎮市區名稱 geom_text
ggplot() + 
  geom_polygon(data=county.df,
               aes(x=long, y=lat, group=group, fill=avg_power), color="blue") +
  geom_text(data=county.data,
            aes(x=long_pos, y=lat_pos, label=TOWNNAME), color="black") +
  coord_equal() +
  scale_x_continuous(NULL, breaks=NULL) +
  scale_y_continuous(NULL, breaks=NULL) +
  scale_fill_distiller(palette="Oranges", "每人平均用電", direction=1) +
  labs(title=paste0(county, "各鄉鎮市區用電地圖")) +
  theme(panel.background = element_blank())
