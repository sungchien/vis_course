#############################################
# 本次課程繪製雷達圖
# 雷達圖可以視為是長條圖的一種變形，檢視個體在各種類別上對應數值屬性的大小。
# 將色彩對應到另一個類別屬性，可以比較不同類別的個體。
# 
# 由於ggplot2並沒有提供雷達圖，因此利用ggplot提供的原型工具ggproto ，
# 修改極座標(polar coordinate)，使其連接的線為直線，建立雷達圖座標系統。
# 在此雷達圖座標上，再以多邊形的方式畫出每個個體。
# x軸：類別屬性
# y軸：數值屬性
# 色彩：類別屬性
#
# 特別注意事項：分群的種類不可過多
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
# 比較106學年各體系碩士各年級人數比例
# 個體：各體系碩士學生
# 類別屬性一：各體系
# 類別屬性二：年級
# 數值屬性：人數比例

# 106學年各體系碩士各年級比例
grade_lvl <- c("一年級", "二年級", "三年級", "四年級", "五年級(以上)")
std <- tdf %>%
  filter(year==106) %>%             # 取出106學年資料
  filter(grepl("M", 等級別)) %>%      # 取出碩士班資料
  mutate(grade=substr(key, 1, 3)) %>% # 產生年級屬性
  mutate(grade=ifelse(grepl("[五六七延]", grade), "五年級(以上)", grade)) %>%
  mutate(grade=factor(grade, levels=grade_lvl, ordered=TRUE)) %>%
  group_by(體系別, grade) %>%        # 統計各體系各年級人數
  summarise(value.sum=sum(value)) %>%
  ungroup() %>%
  group_by(體系別) %>%               # 計算各體系下各年級人數比例
  mutate(rate=value.sum/sum(value.sum)) %>%
  ungroup()

# 群組長條圖
ggplot(std, aes(x=grade, y=rate)) +
  geom_col(aes(fill=體系別), position="dodge") +
  labs(title="106學年各體系碩士各年級比例", x="年級", y="人數比例") +
  theme(axis.text.x = element_text(color="black", angle=60, hjust=1),
        axis.text.y = element_text(color="black"),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="grey50"),
        panel.grid.minor.y = element_line(color="grey80"))

# 雷達圖
# Define a new coordinate system 
coord_radar <- function () 
{
  ggproto("CoordRadar", CoordPolar, theta = "x", r = "y", start = 0, 
          direction = 1,
          is_linear = function(coord) TRUE)
}

# 畫出雷達圖
ggplot(std, aes(x=grade, y=rate, group=體系別)) + 
  geom_polygon(aes(color=體系別), fill=NA) +
  coord_radar() +
  labs(title="106學年各體系碩士各年級比例") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color="grey50"),
        panel.grid.major.y = element_line(color="grey50"),
        axis.title=element_blank())

########################################################################
# 示範案例
# 比較106學年各體系不同性別碩士各年級人數比例
# 個體：各體系碩士學生
# 類別屬性一：各體系
# 類別屬性二：年級
# 類別屬性二：性別
# 數值屬性：人數比例
std <- tdf %>%
  filter(year==106) %>%             # 取出106學年資料
  filter(grepl("M", 等級別)) %>%    # 取出碩士班資料
  mutate(sex=substr(key, 4, 5)) %>% # 產生性別屬性
  mutate(grade=substr(key, 1, 3)) %>% # 產生年級屬性
  mutate(grade=ifelse(grepl("[五六七延]", grade), "五年級(以上)", grade)) %>%
  mutate(grade=factor(grade, levels=grade_lvl, ordered=TRUE)) %>%
  group_by(sex, 體系別, grade) %>%    # 統計各體系不同性別的各年級人數
  summarise(value.sum=sum(value)) %>%
  ungroup() %>%
  group_by(sex, 體系別) %>%           # 計算各體系不同性別下各年級人數比例
  mutate(rate=value.sum/sum(value.sum)) %>%
  ungroup()

# 畫出雷達圖
ggplot(std, aes(x=grade, y=rate, group=體系別)) + 
  geom_polygon(aes(color=體系別), fill=NA) +
  coord_radar() +
  facet_wrap(~sex) +
  labs(title="106學年各體系碩士各年級比例") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color="grey50"),
        panel.grid.major.y = element_line(color="grey50"),
        axis.title=element_blank())

########################################################################
# 示範案例
# 比較106學年各體系不同性別碩士各年級人數比例
# 個體：各體系碩士學生
# 類別屬性一：各體系
# 類別屬性二：年級
# 類別屬性二：性別
# 數值屬性：人數比例
std <- tdf %>%
  filter(year==106) %>%             # 取出106學年資料
  filter(grepl("M", 等級別)) %>%      # 取出碩士班資料
  mutate(sex=substr(key, 4, 5)) %>%   # 產生性別屬性
  mutate(grade=substr(key, 1, 3)) %>% # 產生年級屬性
  mutate(grade=ifelse(grepl("[五六七延]", grade), "五年級(以上)", grade)) %>%
  mutate(grade=factor(grade, levels=grade_lvl, ordered=TRUE)) %>%
  group_by(dn, sex, 體系別, grade) %>%   # 統計各體系不同性別、不同學制的各年級人數
  summarise(value.sum=sum(value)) %>%
  ungroup() %>%
  group_by(dn, sex, 體系別) %>%    # 計算各體系不同性別、不同學制下各年級人數比例
  mutate(rate=value.sum/sum(value.sum)) %>%
  ungroup()

# 畫出雷達圖
ggplot(std, aes(x=grade, y=rate, group=體系別)) + 
  geom_polygon(aes(color=體系別), fill=NA) +
  coord_radar() +
  facet_grid(rows=vars(dn), cols=vars(sex)) +
  labs(title="106學年各體系碩士各年級比例") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color="grey50"),
        panel.grid.major.y = element_line(color="grey50"),
        axis.title=element_blank())
