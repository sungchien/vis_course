#############################################
# 本次課程的學習目標：                      #
# 1. 視覺化的過程                           #
# 2. ggplot2的基本用法                      #
# ggplot                                    #
# aes                                       #
# geom_                                     #
# scale_                                    #
# theme                                     #
# 3. 基本圖形                               #
# 直方圖                                    #
# 長條圖                                    #
# 折線圖                                    #
# 散佈圖                                    #
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
# 盒鬚圖 (Box Whisker Plot)

# 比較各體系106學年各科系大一學生人數分布情形
# 個體：各大學各科系大一學生資料, 數值屬性：體系別、學生人數
dpt_std_no <- tdf %>%
  filter(year==106) %>%             # 取出106學年資料
  filter(grepl("B", 等級別)) %>%      # 取出大學部資料
  filter(grepl("D", dn)) %>%
  group_by(學校名稱, 科系名稱, 體系別) %>%
  summarise(value=sum(value)) %>%
  ungroup()

boy_std_no <- tdf %>%
  filter(year==106) %>%             # 取出106學年資料
  filter(grepl("B", 等級別)) %>%      # 取出大學部資料
  filter(grepl("D", dn)) %>%
  filter(grepl("男生", key)) %>%
  group_by(學校名稱, 科系名稱, 體系別) %>%
  summarise(value.boy=sum(value)) %>%
  ungroup()

higher_std_no <- tdf %>%
  filter(year==106) %>%             # 取出106學年資料
  filter(grepl("B", 等級別)) %>%      # 取出大學部資料
  filter(grepl("D", dn)) %>%
  filter(grepl("[五六七延]", key)) %>%
  group_by(學校名稱, 科系名稱, 體系別) %>%
  summarise(value.higher=sum(value)) %>%
  ungroup()

dpt_std_no <- dpt_std_no %>%
  inner_join(boy_std_no) %>%
  inner_join(higher_std_no) %>%
  mutate(boy_rate=value.boy/value) %>%
  mutate(higher_rate=value.higher/value)

dpt_std_no %>%
  filter(!grepl("3", 體系別)) %>%
  filter(!grepl("醫學系", 科系名稱)) %>%
  filter(value>=50) %>%
  ggplot() +
  geom_point(aes(x=boy_rate, y=higher_rate, color=體系別, size=value)) +
  facet_wrap(~體系別)
#  filter(higher_rate>=0.2) %>%

+
  labs(title="比較各體系106學年各科系大一學生人數分布情形", 
       y="學生人數") +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="grey80"),
        panel.grid.minor.y = element_line(color="grey90"))
