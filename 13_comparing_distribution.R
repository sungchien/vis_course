#############################################
# 本次課程繪製資料分組的直方圖以及盒鬚圖(Box-Whisker Plot)和小提琴圖(Violin Plot)
# 目的在於以視覺化的方式呈現資料分布的比較。
#
# 盒鬚圖呈現各組資料的中位數、第一四分位數及第三分位數等各項統計資料。
# x軸：類別屬性
# y軸：數值屬性
# 色彩：類別屬性
#
# 小提琴圖呈現各組資料的分布形狀，形狀愈寬的地方代表資料分布愈多。
# x軸：類別屬性
# y軸：數值屬性
# 色彩：類別屬性
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
# 比較各體系106學年各科系大一學生人數分布情形
# 個體：各大學各科系大一學生資料, 數值屬性：體系別、學生人數
dpt_std_no <- tdf %>%
  filter(year==106) %>%             # 取出106學年資料
  filter(grepl("B", 等級別)) %>%      # 取出大學部資料
  filter(grepl("^一年級", key)) %>% # 取出一年級學生資料
  group_by(體系別, 學校名稱, 科系名稱) %>%  # 依據學校與科系分群
  summarise(value.sum = sum(value)) %>% # 統計各科系大一學生人數
  ungroup() %>%
  arrange(desc(value.sum))          # 由大到小排列

# 以資料分組的方式呈現直方圖
# x軸：各科系的大一學生人數
# 資料分組：各體系
ggplot(dpt_std_no, aes(x=value.sum)) +
  geom_histogram(aes(fill=體系別), breaks=seq(0, 700, 50)) +
  scale_x_continuous(breaks=seq(0, 700, 100)) +
  scale_y_continuous(breaks=seq(0, 700, 100)) +
  scale_fill_brewer(guide="none", palette="Dark2") +
  labs(title="比較各體系106學年各科系大一學生人數分布情形", 
       x="學生人數", y="科系數") +
  facet_wrap(~體系別) +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="grey80"),
        panel.grid.minor.y = element_line(color="grey90"))

# 盒鬚圖 (Box Whisker Plot)
# x軸：各體系
# y軸：各科系的大一學生人數
ggplot(dpt_std_no, aes(x=體系別, y=value.sum)) +
  geom_boxplot(aes(fill=體系別)) +
  scale_y_continuous(breaks=seq(0, 700, 50)) +
  scale_fill_brewer(guide="none", palette="Dark2") +
  labs(title="比較各體系106學年各科系大一學生人數分布情形", 
       y="學生人數") +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="grey80"),
        panel.grid.minor.y = element_line(color="grey90"))

# 小提琴圖 (Violin Plot)
# x軸：各體系
# y軸：各科系的大一學生人數
ggplot(dpt_std_no, aes(x=體系別, y=value.sum)) +
  geom_violin(aes(fill=體系別)) +
  scale_y_continuous(breaks=seq(0, 700, 50)) +
  scale_fill_brewer(guide="none", palette="Dark2") +
  labs(title="比較各體系106學年各科系大一學生人數分布情形", 
       y="學生人數") +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="grey80"),
        panel.grid.minor.y = element_line(color="grey90"))

# 小提琴圖 + 盒鬚圖
ggplot(dpt_std_no, aes(x=體系別, y=value.sum)) +
  geom_violin(aes(fill=體系別)) +
  geom_boxplot(width=0.2, color="red", fill=NA) +
  scale_fill_brewer(guide="none", palette="Dark2") +
  labs(title="比較各體系106學年各科系大一學生人數分布情形", 
       y="學生人數") +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="grey80"),
        panel.grid.minor.y = element_line(color="grey90"))
