library(tidyverse)
library(xml2)

# 從XML檔案中取得資料
xml_text <- read_xml("https://www.dgbas.gov.tw/public/data/open/localstat/016-%E5%AE%B6%E5%BA%AD%E4%B8%BB%E8%A6%81%E8%A8%AD%E5%82%99%E6%99%AE%E5%8F%8A%E7%8E%87.xml")

# 每年的節點資料
getData <- function (x) {
  # field_data： 各欄位的xml資料 
  field_data <- xml_children(x)

  f_value <- xml_text(field_data)
  names(f_value) <- xml_name(field_data)
  
  # 傳回一個向量，向量上的值是各欄位的資料
  return(f_value)
}

t_data <- sapply(xml_children(xml_text), getData)
# t_data：矩陣，每一個row是欄位，每一個column是各年的資料

# 資料處理：格式轉換
# 如果不熟悉R語言，建議可以先一行行執行，察看結果
tdata_tibble <- t_data %>%
  t() %>%                             # 轉置t_data矩陣
  as.tibble() %>%                     # 轉換成tibble格式
  gather(key="application", value="value", -Year) %>% # tidy 資料，使得每個欄位都彼此有關
  filter(grepl("[0-9]+[\\.0-9]*", value)) %>% # 去除非數值的資料
  mutate(value=as.numeric(value)) # 轉換成數值

# 寫出資料
write_csv(tdata_tibble, "applications.csv")
