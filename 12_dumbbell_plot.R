##################################################
# 本次課程繪製啞鈴圖 (Dumbbell Plot)
# 啞鈴圖的特色在於表現具有二種相對數值屬性
# (例如：不同生理性別的人數)的個體。
# 繪製時先繪製代表每個個體的水平線段，
# 將二種相對數值屬性的值分別做為水平線段的前後端點
# 線段的長度代表數值屬性的差，
# 可以利用線段的顏色代表數值屬性的比例關係
#
# 特別注意事項：如果用來區隔的類別屬性具有順序關係，
#               可依照這個順序關係，將個體排列
#               否則則以其中一個數值屬性的大小，將個體排列
#
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

# 變更較不適合處理的Variable名稱
tdf <- tdf %>%
  rename(dn = `日間∕進修別`)

########################################################################
# 示範案例
# 世新大學106學年各系大一男女生人數比較
# 個體：世新大學106學年各系大一學生資料
# 屬性：科系名稱、女生人數、男生人數、女生與男生人數差比例
#
shu_std_no <- tdf %>%
  filter(學校名稱=="世新大學") %>%
  filter(year==106) %>%             # 取出106學年資料
  filter(等級別=="B 學士") %>%      # 取出學士班資料
  filter(grepl("^一年級", key)) %>% # 取出一年級學生資料
  select(科系名稱, key, value)  %>% # 選取科系名稱、年級與人數等變數
  spread(key=key, value=value) %>%  # 將各系男女生資料整合成一筆紀錄
  rowwise() %>%                     # 產生男生或女生人數的最大值
  mutate(max_value=max(一年級男生, 一年級女生)) %>%
  ungroup() %>%
  arrange(max_value) %>% # 依照最大值將科系進行排序
  mutate(科系名稱=factor(科系名稱, levels=科系名稱, ordered=TRUE)) %>%
  mutate(sex_ratio=(一年級女生-一年級男生)/max_value) # 計算女生與男生人數差比例

# 畫成啞鈴圖
ggplot(shu_std_no) +
  geom_segment(aes(x=一年級女生, xend=一年級男生,  # 代表個體的線段
                   y=科系名稱, yend=科系名稱,
                   color=sex_ratio),
               size=2) +
  geom_point(aes(x=一年級女生, y=科系名稱), color="red", size=3) + # 女生人數
  geom_point(aes(x=一年級男生, y=科系名稱), color="blue", size=3) + # 男生人數
  labs(title="世新大學106學年各系大一男女生人數比較",
       x="大一學生人數") +
  scale_x_continuous(breaks=seq(0, 200, 20)) +
  scale_color_gradient2(low="blue", high="red",
                        limits=c(-1.0, 1.0),
                        name="女男人數差比例") +
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_line(color="grey80"),
        panel.grid.major.y = element_line(color="grey80"),
        panel.grid.minor.y = element_line(color="grey80"))
