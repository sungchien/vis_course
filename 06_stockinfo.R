library(tidyverse)
library(curl)
library(rvest)

# Yahoo!奇摩股票網頁網址
addr <- "https://tw.stock.yahoo.com/"
session <- html_session(addr)

# 擷取半導體類股網頁
cat_page <- jump_to(session,
                    "h/kimosel.php?form=menu&form_id=stock_id&form_name=stock_name")
 
if (readline(prompt="選擇半導體(y)或其他類股(n):")=="n") {
  # 擷取類股名稱表格
  cat_link <- html_nodes(cat_page,
                         xpath='//table[@cellpadding="2"]/tr/td/a')

  # 類股名稱
  cat_name <- html_text(cat_link)
  # 類股網址
  cat_addr <- html_attr(cat_link, "href")

  # 等待使用者選擇類股
  select_addr <- cat_addr[menu(cat_name)]

  # 進入類股網頁
  cat_page <- jump_to(session, select_addr)
}

stock_id_txt <- html_nodes(cat_page, 'a.none') %>% # 取得個股節點
  html_text() %>% # 檢出個股文字資料
  strsplit("[[:space:]]")

# 取出個股編號與名稱
stock_info <- data.frame(id=sapply(stock_id_txt, `[`, 2),
                         name=sapply(stock_id_txt, `[`, 3))

# 以每一支股票的編號查詢取回個股交易的資料
getStockInfo <- function (id) {
  # 取回yahoo股票上的各股票網頁的HTML碼
  pos_stock_text <- paste0("https://tw.stock.yahoo.com/q/q?s=", id) %>%
    readLines() %>%
    paste(collapse="\n")
  
  # 設定HTML碼的編碼為big5
  Encoding(pos_stock_text) <- "big5"
  
  table_node <- iconv(pos_stock_text, "big5", "UTF-8") %>% # 將big5轉換成UTF-8
    read_html()%>%                          # 利用rvest的HTML抓取功能
    html_node(xpath='//table[@border="2"]') # 抓取個股資料表格
  
  st_data <- table_node %>%                 # 個股資料表格
    html_nodes(css="td") %>%                # 表格上各格上的資料
    html_text()                             # 取得資料文字
  
  st_fields <- table_node %>%
    html_nodes(css="th") %>%                # 表格上各格上的各欄位名稱
    html_text()                             # 取得欄位名稱
  
  st_data <- st_data[c(3:5,7:11)]
  names(st_data) <- st_fields[c(3:5,7:11)]
  
  return(st_data)
}

stock_df <- sapply(stock_info$id, getStockInfo) %>% # 利用個股編號取出交易資料
  t() %>%                                           # 轉置矩陣
  as.data.frame()                                   # 將矩陣轉成data frame格式

stock_info <- cbind(stock_info, stock_df)           # 合併個股編號、名稱和交易資料

