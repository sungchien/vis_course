#############################################
# 本次課程繪製直方圖
# 直方圖表現所有個體在某一個數值屬性上的分布
# 將數值屬性分為若干區間，統計各區間上的個體數量
# 直方圖便是呈現各區間上的個體數量
#
# 個體是區間上的一個單位
# x軸：數值屬性的區間，y軸：各區間上的個體數量
#
# 特別注意事項：適當劃分數值屬性的區間
#############################################
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

# 示範範例
# 106學年各科系大一學生人數分布情形
# 個體：各大學各科系大一學生資料, 數值屬性：學生人數
tdf %>%
  filter(year==106) %>%             # 取出106學年資料
  filter(grepl("B", 等級別)) %>%      # 取出大學部資料
  filter(grepl("^一年級", key)) %>% # 取出一年級學生資料
  group_by(學校名稱, 科系名稱) %>%  # 依據學校與科系分群
  summarise(value.sum = sum(value)) %>% # 統計各科系大一學生人數
  ungroup() %>%
  arrange(desc(value.sum))          # 由大到小排列

# 以ggplot2畫出直方圖
# ggplot
# aes(x, y)
# geom_histogram(breaks)
# scale_x_continuous
# scale_y_continuous
# theme

tdf %>%
  filter(year==106) %>%             # 取出106學年資料
  filter(grepl("B", 等級別)) %>%      # 取出大學部資料
  filter(grepl("^一年級", key)) %>% # 取出一年級學生資料
  group_by(學校名稱, 科系名稱) %>%  # 依據學校與科系分群
  summarise(value.sum = sum(value)) %>% # 統計各科系大一學生人數
  ungroup() %>%
  ggplot(aes(x=value.sum)) +        # 畫出直方圖
  geom_histogram(breaks=seq(0, 700, 50), fill="white", color="black") +  # 設定直方圖的區間
  scale_x_continuous(breaks=seq(0, 700, 50), minor_breaks = NULL) +
  scale_y_continuous(breaks=seq(0, 1400, 200)) +
  labs(title="106學年度各科系大一學生人數分布情形", x="各科系大一學生人數", y="科系數") +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="grey80"),
        panel.grid.minor.y = element_line(color="grey90"))

########################################################################
# 練習
# 106學年度各大學學生人數分布情形
# 個體：？   屬性：？
# 提示：
# 1. 取出106學年資料
# 2. 依據學校分群
# 3. 統計各大學學生人數
# 4. 畫出直方圖
tdf %>%
  filter(year==106) %>%             # 取出106學年資料
  group_by(學校名稱) %>%            # 依據學校分群
  summarise(value.sum=sum(value)) %>% # 統計各大學學生人數
  ungroup() %>%
  arrange(desc(value.sum))          # 將大學依據學生人數由大到小排序
