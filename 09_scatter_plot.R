#############################################
# 本次課程繪製散佈圖
# 透過圖形上點的分布情形(形狀、疏密)表示集合內個體上的兩個數值屬性之間的關係
#
# 每個個體對應到圖上的一個點
# x軸：數值屬性 y軸：另一個數值屬性
#
# 特別注意事項：通常以自變數(預測變數)的數值屬性做為x軸，
#               應變數(結果)做為y軸
#
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

# 變更較不適合處理的Variable名稱
tdf <- tdf %>%
  rename(dn = `日間∕進修別`)

########################################################################
# 示範案例
# 各學校105與106學年大一學生人數關連
# 個體：各學校大一學生資料
# 屬性：105學年大一學生人數, 106學年大一學生人數

std_no <- tdf %>%
  filter(year>=105) %>%                # 取出105與106學年資料
  filter(grepl("B", 等級別)) %>%       # 取出學士班資料
  filter(grepl("^一年級", key)) %>%    # 取出一年級學生資料
  group_by(year, 學校名稱) %>%         # 按照學校分群
  summarise(freshman=sum(value)) %>%   # 統計各學校的人數
  ungroup()

std_no <- std_no %>%  
  mutate(year=sprintf("Y%d", year)) %>%     # 變更year上的資料為加上Y的文字
  spread(key=year, value=freshman, fill=0)  # 將105與106學年度的資料合併在一筆紀錄

# 以ggplot2畫出散佈圖
# ggplot
# aes(x, y)
# geom_point
# scale_x_continuous
# scale_y_continuous
# geom_abline 以斜率(slope)和截距(intercept)畫出圖上的一直線
# theme
ggplot(std_no, aes(x=Y105, y=Y106)) +  # 畫出散布圖
  geom_point(alpha=0.7) +
  scale_x_continuous(breaks=seq(0, 6000, 1000)) +
  scale_y_continuous(breaks=seq(0, 6000, 1000)) +
  labs(x="105學年度大一學生人數", y="106學年度大一學生人數",
       title="各學校105與106學年大一學生人數關連") +
  geom_abline(slope=1, intercept=0, color="grey50") +   # 加上輔助的對角線
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_line(color="grey80"),
        panel.grid.major.y = element_line(color="grey80"),
        panel.grid.minor.y = element_line(color="grey80"))

########################################################################
# 練習
# 106學年度各學校大一男女生人數關連
# 個體：？   屬性：？
# 1. 取出106學年資料
# 2. 取出學士班資料
# 3. 取出一年級學生資料
# 4. 按照學校與男女生分群
# 5. 統計各學校的人數
# 6. 將男女生資料合併成一筆紀錄
# 7. 畫出散佈圖
tdf %>%
  filter(year==106) %>%               # 取出106學年資料
  filter(grepl("B", 等級別)) %>%       # 取出學士班資料
  filter(grepl("^一年級", key)) %>%    # 取出一年級學生資料
  group_by(學校名稱, key) %>%          # 按照學校與男女生分群
  summarise(freshman=sum(value)) %>%   # 統計各學校的人數
  ungroup() %>%
  spread(key=key, value=freshman, fill=0) # 將男女生資料合併成一筆紀錄