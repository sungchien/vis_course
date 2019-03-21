##################################################
# 本次課程繪製圓餅圖
# 圓餅圖將個體依據某一個類別屬性分群
# 檢視每個分群上的個體在某一數值屬性(個數或總和)佔全體集合的比例
# 圓餅圖以同一分群下所有的個體所占面積呈現它們在數值屬性上的比例
# 
# 由於ggplot2並沒有提供圓餅圖，因此是利用一種特殊的堆疊長條圖來繪製圓餅圖
# 此一長條圖只有一條長條，並且利用極坐標(Polar coordinates)取代原先的笛卡爾坐標(Cartesian coordinates)
# y軸：數值屬性
# 色彩：類別屬性
#
# 特別注意事項：分群的種類不可過多，每個分群上的比例也不可以過少
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
# 106學年學士班各等級別一年級男女生比例情形
# 個體：學士班的一年級學生
# 類別屬性：各等級不同與性別分群
# 數值屬性：人數比例

# 106學年學士班各等級別一年級男女生比例
std_rate<- tdf %>%
  filter(year==106) %>%             # 取出106學年資料
  filter(grepl("B", 等級別)) %>%      # 取出學士班資料
  filter(grepl("^一年級", key)) %>% # 取出一年級學生資料
  mutate(std_type=paste0(substr(等級別, 3, 4),
                         substr(key, 4, 5))) %>% # 產生合併等級與男女生的字串
  group_by(std_type) %>%            # 進行各等級與男女生分群
  summarise(value.sum = sum(value)) %>% # 統計人數總和
  ungroup() %>%
  mutate(rate=round(value.sum/sum(value.sum)*100, 2)) # 計算各等級與性別人數比例

# 以ggplot2畫出圓餅圖
# ggplot()
# aes(x="", y, fill)
# geom_col
# coord_polar(theta, start)
# scale_fill_brewer(palette)
# theme
ggplot(std_rate)+ # 畫出圓餅圖
  geom_col(aes(x=1, y=rate, fill=std_type), width = 1) +
  coord_polar(theta = "y", start=0) +
  labs(title="106學年學士班各等級別一年級男女生比例情形") +
  scale_fill_brewer(palette="Paired",
                    guide=guide_legend(title="各等級及性別")) +
  theme(axis.text.x=element_blank(),
        axis.title=element_blank(), axis.text=element_blank(),
        panel.border=element_blank(), panel.background = element_blank())

# 將圖標(guides)上的文字標示在圖形上
std_type_order <- c("四技女生", "四技男生", "學士男生", "學士女生")
std_rate <- std_rate %>%
  mutate(std_type=factor(std_type, levels=std_type_order, ordered=TRUE)) %>%
  arrange(std_type) %>%
  mutate(y_pos=100-(cumsum(rate)-rate/2)) # 計算文字在圖形上的位置
  
ggplot(std_rate)+ # 畫出圓餅圖
  geom_col(aes(x=1, y=rate, fill=std_type), width = 1) +
  geom_text(aes(x=1, y=y_pos,
                label=sprintf("%s\n%0.2f%%", std_type, rate))) +
  coord_polar(theta="y", start=0) +
  labs(title="106學年學士班各等級別一年級男女生比例情形") +
  scale_fill_brewer(palette="Paired", guide="none") +
  theme(axis.text.x=element_blank(),
        axis.ticks = element_blank(),
        axis.title=element_blank(), axis.text=element_blank(),
        panel.border=element_blank(), panel.background = element_blank())

########################################################################
# 練習案例
# 106學年碩士一般與在職生比例
# 個體：？
# 類別屬性：？
# 數值屬性：？

tdf %>%
  filter(year==106) %>%             # 取出106學年資料
  filter(grepl("M", 等級別)) %>%      # 取出碩士班資料
  group_by(dn)%>%         # 依據日間∕進修別分群
  summarise(value.sum=sum(value)) %>% # 統計人數
  ungroup() %>%
  mutate(rate=value.sum/sum(value.sum)) # 計算各部分所佔比例