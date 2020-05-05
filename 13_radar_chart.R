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
library(readxl)
library(ggplot2)

# 資料輸入
sightseeing <- read_excel("sightseeing.xlsx")

########################################################################
# 示範案例
# 比較各景點前後兩年觀光客人數
# 個體：所有景點觀光客
# 類別屬性一：各景點
# 類別屬性二：前後兩年
# 數值屬性：觀光客人數

sightseeing <- sightseeing %>%
  mutate(fchi=regexpr("\r\n", 地點)) %>%
  mutate(loc=substr(地點, 1, fchi-1)) %>%
  select(2, 3, 5) %>%
  gather(key="year", value="visitorCount", -loc)

loc_rank <- sightseeing %>%
  filter(year=="2019二月") %>%
  arrange(desc(visitorCount)) %>%
  pull(loc)

sightseeing <- sightseeing %>%
  mutate(year=factor(year)) %>%
  mutate(loc=factor(loc, levels=loc_rank, ordered = TRUE)) %>%
  arrange(year, loc)

# 群組長條圖
ggplot(sightseeing, aes(x=loc, y=visitorCount)) +
  geom_col(aes(color=year, fill=year), alpha=0.3, position="dodge") +
  labs(title="各景點前後兩年觀光客人數", x="景點", y="觀光客人數") +
  scale_y_continuous(breaks=seq(0, 1000000, 200000))+
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
ggplot(sightseeing, aes(x=loc, y=visitorCount, group=year)) + 
  geom_polygon(aes(color=year, fill=year), alpha=0.3) +
  coord_radar() +
  labs(title="各景點前後兩年觀光客人數") +
  scale_y_continuous(breaks=seq(0, 1000000, 200000))+
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
