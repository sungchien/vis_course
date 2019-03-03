#############################################
# 本次課程繪製長條圖
# 將個體依據某一個類別屬性分群
# 比較某一個分群上的個體在某一數值屬性上的統計結果(個數、總和、平均、...)
#
# 每個個體表示為圖上的一個長條
# x軸：類別屬性， y軸：個體在某一數值屬性上的結果
#
# 特別注意事項：產生的圖表應按照數值屬性對類別進行排序
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
# 比較106學年度不同等級學生人數
# 類別屬性：每個等級, 數值屬性：學生人數

aca_sys_std_no <- tdf %>%
  filter(year==106) %>%             # 取出106學年資料
  group_by(等級別) %>%              # 依據等級分群
  summarise(value.sum=sum(value)) %>% # 統計各等級學生人數
  arrange(desc(value.sum))          # 將等級依據學生人數由大到小排序

# 以ggplot2畫出長條圖
# ggplot
# aes(x, y)
# geom_col
ggplot(aca_sys_std_no, aes(x=等級別, y=value.sum)) +  # 畫出長條圖
  geom_col()

# 依照各等級人數，設定大小順序
aca_sys_std_no <- aca_sys_std_no %>%
  mutate(等級別=factor(等級別, levels=等級別))

# 以ggplot2畫出長條圖
# ggplot
# aes(x, y)
# geom_col
# scale_y_continuous
# theme
ggplot(aca_sys_std_no, aes(x=等級別, y=value.sum)) +  # 畫出長條圖
  geom_col(color="black", fill="white") +
  scale_y_continuous(breaks=seq(0, 500000, 100000)) +
  labs(y="學生人數", title="106學年度不同等級學生人數比較") +
  theme(axis.text.x = element_text(angle=60, hjust=1, color="black"),
        axis.text.y = element_text(color="black"),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="grey80"),
        panel.grid.minor.y = element_line(color="grey90"))

########################################################################
# 練習
# 比較106學年度不同體系大一學生人數
# 個體：? 屬性：?
# 提示：
# 1. 取出106學年資料
# 2. 取出大學部資料
# 3. 取出一年級學生資料
# 4. 依據體系分群
# 5. 統計各體系大一學生人數
# 6. 畫出長條圖
tdf %>%
  filter(year==106) %>%             # 取出106學年資料
  filter(grepl("B", 等級別)) %>%    # 取出學士班資料
  filter(grepl("^一年級", key)) %>% # 取出一年級學生資料
  group_by(體系別) %>%              # 依據體系分群
  summarise(value.sum=sum(value))   # 統計各體系大一學生人數
