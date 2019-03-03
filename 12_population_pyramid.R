##################################################
# 本次課程繪製人口金字體圖(Population pyramid)
# 人口金字體的特色在於表現具有二種相對數值屬性
# (例如：不同生理性別的人數)的個體。
# 繪製時可以將人口金字體視為是一種特別的堆疊長條圖，
# 每個個體對應到圖上的一個長條，
# 以個體的某一個類別屬性進行區隔，
# 相對的兩個數值屬性分別放置在0點的兩邊
#
# 特別注意事項：如果用來區隔的類別屬性具有順序關係，
#               可依照這個順序關係，將個體排列
#               否則則以其中一個數值屬性的大小，將個體排列
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
# 世新大學106學年各系大一男女生人數比較
# 個體：世新大學106學年各系大一學生資料
# 屬性：科系名稱、女生人數, 男生人數
# 
shu_std_no <- tdf %>%
  filter(學校名稱=="世新大學") %>%  # 取出世新大學學生資料
  filter(year==106) %>%             # 取出106學年資料
  filter(等級別=="B 學士") %>%      # 取出學士班資料
  filter(grepl("^一年級", key)) %>% # 取出一年級學生資料
  select(科系名稱, key, value)  %>% # 選取科系名稱、年級與人數等變數
  spread(key=key, value=value) %>%  # 將各系男女生資料整合成一筆紀錄
  arrange(desc(一年級女生)) %>%     # 以女生人數進行排序
  mutate(科系名稱=factor(科系名稱, levels=科系名稱, ordered=TRUE)) # 以女生人數為主排序科系


# 畫出人口金字塔圖
ggplot(shu_std_no) +
  geom_col(aes(x=科系名稱, y=一年級女生), fill="red") +
  geom_col(aes(x=科系名稱, y=-一年級男生), fill="blue") +
  scale_y_continuous(limits=c(-200, 200), breaks=seq(-200, 200, 40),
                     labels=c(seq(200, 40, -40), seq(0, 200, 40))) +
  labs(title="世新大學106學年各系大一男女生人數比較", y = "學生人數") +
  coord_flip() +
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_line(color="grey80"),
        panel.grid.major.y = element_line(color="grey80"),
        panel.grid.minor.y = element_line(color="grey80"))

