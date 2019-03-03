#############################################
# 本次課程繪製折線圖
# 將個體依照某一個屬性的次序排列，表現出個體的另一個數值屬性的變化情形，
# 包括上升或下降、週期變化、變化快慢等等
#
# 每個個體對應到圖上的一個點，線是將每一個點按照某一個屬性的次序相連
# x軸: 能按照大小次序排列的屬性。 y軸：數值屬性
#
# 特別注意事項：大多數情況下，x軸上使用時間相關屬性
#               表示y軸上的數值屬性依照時間的變化
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
# 示範案例 (x軸上使用時間相關屬性)
# 103到106學年度大一學生人數變化情形
# 個體：各學年度大一學生資料
# 可排序之屬性：學年度, 數值屬性：學生人數

std_no_per_year <- tdf %>%
  filter(grepl("B", 等級別)) %>%      # 取出學士與四技學生資料
  filter(grepl("一年級", key)) %>%    # 取出一年級學生資料
  group_by(year) %>%                  # 依據學年度分群
  summarise(value.sum=sum(value))     # 統計103到106學年度大一學生人數

# 以ggplot2畫出折線圖
# ggplot
# aes(x, y)
# geom_line：線
# geom_point：線上的點
# scale_x_continuous
# scale_y_continuous
# theme
ggplot(std_no_per_year, aes(x=year, y=value.sum)) + # 畫出折線圖
  geom_line() +
  geom_point() +
  scale_x_continuous(minor_breaks = NULL) +
  scale_y_continuous(breaks=seq(0, 300000, 50000), limits=c(0, 300000)) +
  labs(x="學年度", y="學生人數", title="103到106學年度大一學生人數變化") +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        axis.text = element_text(color="black"),
        axis.line = element_line(color="black"),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="grey80"),
        panel.grid.minor.y = element_line(color="grey90"))

# 練習
# 103到106學年度碩士男生人數變化情形
# 個體：？   可排序之屬性：？ 數值屬性：？
# 1. 取出碩士班資料
# 2. 取出男生資料
# 3. 依據各學年度分群
# 4. 統計各學年度男生人數
# 5. 畫出折線圖
tdf %>%
  filter(grepl("M", 等級別)) %>%   # 取出學士與四技學生資料
  filter(grepl("男", key)) %>%  # 取出一年級學生資料
  group_by(year) %>%                # 依據學年度分群
  summarise(value.sum=sum(value)) %>%  # 統計103到106學年度大一學生人數
  ungroup()

########################################################################
# 示範案例 (x軸上使用非時間相關屬性)
# 106學年碩士生各年級比例
# 個體：各年級碩士生資料
# 屬性：年級, 碩士生人數比例

# 先計算106學年碩士生各年級比例
grade_lvl <- c("一年級", "二年級", "三年級", "四年級", "五年級(以上)")
tdf %>%
  filter(year==106) %>%             # 取出106學年資料
  filter(grepl("M", 等級別)) %>%      # 取出碩士班資料
  mutate(grade=substr(key, 1, 3)) %>% # 整合不同生理性別資料 (去除字串中的男生與女生)
  mutate(grade=ifelse(grepl("[五六七延]", grade), "五年級(以上)", grade)) %>% # 整合高年級資料
  mutate(grade=factor(grade, levels=grade_lvl, ordered=TRUE)) %>% # 定義年級的次序
  group_by(grade) %>%                 # 依據各年級分群
  summarise(value.sum=sum(value)) %>% # 統計學生人數
  ungroup() %>%
  mutate(rate=value.sum/sum(value.sum)) # 計算碩士生各年級比例

# 以ggplot2畫出折線圖
# ggplot
# aes(x, y)
# geom_line(aes(group=""))：所有的資料在同一條線上(aes(group=""))
# geom_point：線上的點
# theme
grade_lvl <- c("一年級", "二年級", "三年級", "四年級", "五年級(以上)")
tdf %>%
  filter(year==106) %>%             # 取出106學年資料
  filter(grepl("M", 等級別)) %>%      # 取出碩士班資料
  mutate(grade=substr(key, 1, 3)) %>% # 整合不同生理性別資料(去除字串中的男生與女生)
  mutate(grade=ifelse(grepl("[五六七延]", grade), "五年級(以上)", grade)) %>% # 整合高年級資料
  mutate(grade=factor(grade, levels=grade_lvl, ordered=TRUE)) %>% # 定義年級的次序
  group_by(grade) %>% # 依據各年級分群
  summarise(value.sum=sum(value)) %>% # 統計學生人數
  ungroup() %>%
  mutate(rate=value.sum/sum(value.sum)) %>% # 計算碩士生各年級比例
  ggplot(aes(x=grade, y=rate)) +      # 畫出折線圖
  geom_line(aes(group="")) +          # 將所有資料設為同一條線
  geom_point() +
  labs(x="年級", y="學生人數比例", title="106學年碩士生各年級比例") +
  theme(axis.line = element_line(color="black"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color="grey50"),
        panel.grid.major.y = element_line(color="grey50"),
        panel.grid.minor.y = element_line(color="grey80"),
        axis.text = element_text(color="black"),
        axis.text.x=element_text(angle=60, hjust=1))
