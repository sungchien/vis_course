library(tidyverse)

# 以CSV檔案格式輸入
file <- "http://opendata.epa.gov.tw/webapi/Data/ATM00679/?$orderby=MonitorDate%20desc&$skip=0&$top=1000&format=csv"
df_csv <- read_csv(file)

# 將SiteId資料型態改為character
df_csv <- df_csv %>%
  mutate(SiteId=as.character(SiteId))

# 以JSON檔案格式輸入
library(jsonlite)
file <- "http://opendata.epa.gov.tw/webapi/Data/ATM00679/?$orderby=MonitorDate%20desc&$skip=0&$top=1000&format=json"
df_json <- fromJSON(file)

# 將MonitorDate資料型態改為Date
df_json <- df_json %>%
  mutate(MonitorDate=as.Date(MonitorDate))

# 將資料從wide data format改為long data format
df_json <- df_json %>%
  gather(key=Variables, value=value, -SiteId, -SiteName, -MonitorDate)

# 改變指標Variables的型態
df_json <- df_json %>%
  mutate(value=as.numeric(value))

# 從long data format改回為wide data format
df_json <- df_json %>%
  spread(key=Variables, value=value)

# 以XML檔案格式輸入
library(xml2)
file <- "http://opendata.epa.gov.tw/webapi/Data/ATM00679/?$orderby=MonitorDate%20desc&$skip=0&$top=1000&format=xml"
xml_data <- read_xml(file)

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

t_data <- sapply(xml_children(xml_data), getData)
# t_data：矩陣，每一個row是欄位，每一個column是各資料節點的資料

# 資料處理：矩陣轉置並儲存成tibble
# 如果不熟悉R語言，建議可以先逐行執行，察看結果
df_xml <- t_data %>%
  t() %>%                             # 轉置t_data矩陣
  as_tibble()                         # 轉換成tibble格式
  

df_xml <- df_xml %>%
  # 將MonitorDate資料型態改為Date
  mutate(MonitorDate=as.Date(MonitorDate)) %>% 
  # 將資料從wide data format改為long data format
  gather(key=Variables, value=value, -SiteId, -SiteName, -MonitorDate) %>%
  # 改變數值資料的型態
  mutate(value=as.numeric(value)) %>%
  # 從long data format改回為wide data format
  spread(key=Variables, value=value)
