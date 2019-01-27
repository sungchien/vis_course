library(tidyverse)
library(curl)
library(rvest)

# 以每一支股票的編號查詢取回個股交易的資料
getStockInfo <- function (id) {
  # 取回yahoo股票上的各股票網頁的HTML碼
  pos_stock_text <- paste0("https://tw.stock.yahoo.com/q/q?s=", id) %>%
    readLines() %>%
    paste(collapse="\n")
  
  # 設定HTML碼的編碼為big5
  Encoding(pos_stock_text) <- "big5"
  
  pos_table_nodes <- iconv(pos_stock_text, "big5", "UTF-8") %>% # 將big5轉換成UTF-8
    read_html()%>%                          # 利用rvest的HTML抓取功能
    html_nodes(css="table")                 # 抓取網頁上所有表格
  

  st_data <- pos_table_nodes[html_attr(pos_table_nodes, "border")=="2"] %>% # 個股資料
    html_nodes(css="td") %>%                # 表格上各格上的資料
    html_text()                             # 取得資料文字
  
  st_fields <- pos_table_nodes[html_attr(pos_table_nodes, "border")=="2"] %>%
    html_nodes(css="th") %>%                # 表格上各格上的各欄位名稱
    html_text()                             # 取得欄位名稱
  
  st_data <- st_data[c(3:5,7:11)]
  names(st_data) <- st_fields[c(3:5,7:11)]
  
  return(st_data)
}


# 取回某一類股上所有個股的交易資料
cat <- "通信網路"           # 設定類股
cat_url <- curl_escape(cat) # 將中文文字轉成URL上的編碼方式
raw_html_txt <- paste0("https://tw.stock.yahoo.com/h/kimosel.php?tse=1&cat=",
       cat_url, "&form=menu&form_id=stock_id&form_name=stock_name&domain=0") %>%
  readLines() %>%           # 取回類股網頁資料
  paste(collapse="\n")

# 設定HTML碼的編碼為big5
Encoding(raw_html_txt) <- "big5"

stock_id_txt <- iconv(raw_html_txt, "big5", "UTF-8") %>%  # 將big5轉換成UTF-8
  read_html()%>%                         # 利用rvest的HTML抓取功能
  html_nodes(css="a.none") %>%           # 抓取網頁上所有連結(其class屬性為"none")
  html_text() %>%                        # 取得連結上的文字
  strsplit("[[:space:]]")                # 依據空白和和換行符號切分文字資料

# 取出個股編號與名稱
stock_info <- data.frame(id=sapply(stock_id_txt, function (x) x[2]),
                         name=sapply(stock_id_txt, function (x) x[3]))

stock_df <- sapply(stock_info$id, getStockInfo) %>% # 利用個股編號取出交易資料
  t() %>%                                           # 轉置矩陣
  as.data.frame()                                   # 將矩陣轉成data frame格式

stock_info <- cbind(stock_info, stock_df)           # 合併個股編號、名稱和交易資料

