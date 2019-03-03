##################################################
# 本次課程運用色彩做為連續型屬性(項目有無限多種可能)的視覺提示
# 以下是ggplot2中適用於連續型屬性的色彩尺度
# scale_color_continuous (scale_fill_continuous)
# scale_color_distiller (scale_fill_distiller)
# scale_color_viridis_c (scale_fill_viridis_c)
# scale_color_gradient (scale_fill_gradient)
# scale_color_gradient2 (scale_fill_gradient2)
# scale_color_gradientn (scale_fill_gradientn)
#
# 本次課程以散佈圖為例
# 示範上述色彩尺度
#
# 散佈圖透過圖形上點的分布情形(形狀、疏密)表示個體屬性之間的關係
# 點的色彩可表示個體的類別(離散)或另一個數值(連續)
# x軸：數值屬性 y軸：另一個數值屬性
# 色彩： 類別屬性(離散)或數值屬性(連續)
# 如果有其他數值屬性可以用點的大小表達，即一般所謂之泡泡圖 (bubble chart)
##################################################
library(tidyverse)
library(readr)
library(ggplot2)

# 資料輸入
tdf <- data.frame()
for (i in seq(103, 106)) {
  fn = sprintf("http://stats.moe.gov.tw/files/detail/%3d/%3d_students.csv", i, i)
  df <- read_csv(file=fn, col_types="cccccccccccccccccccccccc")
  df$year <- i
  tdf <- rbind(tdf, df)
}

# 轉成tidy data format
tdf <- tdf %>%
  gather(key, value, c("一年級男生", "一年級女生", "二年級男生", "二年級女生",
                       "三年級男生", "三年級女生", "四年級男生", "四年級女生", 
                       "五年級男生", "五年級女生", "六年級男生", "六年級女生", 
                       "七年級男生", "七年級女生", "延修生男生", "延修生女生"))

# 將數值從character形式轉成integer形式
tdf <- tdf %>%
  mutate(value=ifelse(grepl("^[0-9]+$", value), as.integer(value), 0))

#
tdf <- tdf %>%
  rename(dn = `日間∕進修別`)

########################################################################
# 示範案例
# 各學校105與106學年大一學生人數關連 (顯示106學年的增減幅度)

# 先計算各學校105與106學年大一學生人數
std_no <- tdf %>%
  filter(year>=105) %>%                # 取出105與106學年資料
  filter(grepl("B", 等級別)) %>%       # 取出學士班資料
  filter(grepl("^一年級", key)) %>%    # 取出一年級學生資料
  group_by(year, 學校名稱) %>%         # 按照學校分群
  summarise(freshman=sum(value)) %>%   # 統計各學校的人數
  ungroup() %>%
  mutate(year=sprintf("Y%d", year)) %>%        # 變更year上的資料為加上Y的文字
  spread(key=year, value=freshman, fill=0) # 將105與106學年度的資料合併成一筆紀錄

# 計算大一學生人數增減幅度
std_no <- std_no %>%
  mutate(gr=round((Y106-Y105)/Y105*100, 2))          

# 將散布圖儲存到pscatter
pscatter <- ggplot(std_no, aes(x=Y105, y=Y106, color=gr)) +  # 畫出散布圖
  geom_point(alpha=0.7) +
  scale_x_continuous(breaks=seq(0, 6000, 1000)) +
  scale_y_continuous(breaks=seq(0, 6000, 1000)) +
  geom_abline(slope=1, intercept=0, color="grey50") +          # 加上輔助的對角線
  labs(x="105學年度大一學生人數", y="106學年度大一學生人數",
       title="各學校105與106學年大一學生人數關連") +
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_line(color="grey50"),
        panel.grid.major.y = element_line(color="grey50"),
        panel.grid.minor.y = element_line(color="grey80"))

# 畫出散佈圖
pscatter

# 使用viridis colormap (scale_color_viridis_c)
pscatter +
  scale_color_viridis_c(option="C")

# 使用Rbrewer調色盤 (scale_color_distiller)
pscatter +
  scale_color_distiller(palette = "YlOrBr")

# 使用兩個顏色之間的漸層 (scale_color_gradient)
pscatter +
  scale_color_gradient(low = "white", high = "red", na.value="green")

# 使用三個顏色之間的漸層 (scale_color_gradient2)
pscatter +
  scale_color_gradient2(low="#1e9600", high="#ff0000", mid="#fff200")

# 使用兩個顏色之間的漸層或viridis colormap (scale_color_continuous)
pscatter +
  scale_color_continuous()
