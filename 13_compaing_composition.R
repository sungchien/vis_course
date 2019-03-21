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

# 106學年學士班不同等級不同性別各年級人數比例
base_num <- 4
grade_lvl <- c("一年級", "二年級", "三年級", "四年級", "五年級(以上)")
std_no_by_grade <- tdf %>%
  filter(grepl("B", 等級別)) %>%    # 取出大學部(學士班)資料
  mutate(grade=substr(key, 1, 3)) %>% # 產生年級資料
  mutate(grade=ifelse(grepl("[五六七延]", grade), "五年級(以上)", grade)) %>%  # 合併高年級資料
  mutate(grade=factor(grade, levels=rev(grade_lvl), ordered=TRUE)) %>%
  group_by(year, grade) %>%         # 依據性別與年級別分群
  summarise(value.sum=sum(value)) %>% # 統計各性別各年級學生人數
  ungroup() %>%
  group_by(year) %>%
  mutate(rate=round(value.sum/sum(value.sum)*100, 2)) %>%
  mutate(y_max=cumsum(rate)) %>%
  mutate(y_min=c(0, y_max[1:(n()-1)])) %>%
  mutate(x_max=year-103+base_num) %>%
  mutate(x_min=x_max-1) %>%
  ungroup()

ggplot(std_no_by_grade)+ # 畫出堆疊長條圖
  geom_col(aes(x=year, y=rate, fill=grade, alpha=year), color="blue") +
  geom_text(aes(x=year, y=100-(y_max+y_min)/2,
                label=sprintf("%0.2f%%", rate))) +
  coord_flip() +
  labs(title="各學年學士班各年級學生比例") +
  scale_fill_brewer(palette="Dark2") +
  scale_alpha_continuous(range=c(0.6, 0.9)) +
  theme(axis.ticks.y = element_blank(),
        panel.border=element_blank(), panel.background = element_blank())

# 以ggplot2畫出圓餅圖
# ggplot()
# aes(x="", y, fill)
# geom_col
# coord_polar(theta, start)
# scale_fill_brewer(palette)
# theme
ggplot(std_no_by_grade)+ # 畫出圓餅圖
  geom_col(aes(x=1, y=rate, fill=grade, alpha=year), color="blue", width = 1) +
  geom_text(aes(x=2, y=(y_max+y_min)/2,
                label=sprintf("%0.2f%%", rate))) +
  coord_polar(theta = "y", start=0) +
  labs(title="各學年學士班各年級學生比例") +
  scale_fill_brewer(palette="Dark2") +
  scale_alpha_continuous(range=c(0.6, 0.9)) +
  facet_wrap(~year, nrow=2) +
  theme(axis.text.x=element_blank(),
        axis.ticks = element_blank(),
        axis.title=element_blank(), axis.text=element_blank(),
        panel.border=element_blank(), panel.background = element_blank())

ggplot(std_no_by_grade) +
  geom_rect(aes(xmax=x_max, xmin=x_min,
                ymax=y_max, ymin=y_min,
                fill=grade, alpha=year),
            color="blue") +
  geom_text(aes(x=(x_max+x_min)/2, y=(y_max+y_min)/2,
                label=sprintf("%0.2f%%", rate))) +
  coord_polar(theta="y", direction = -1) +
  xlim(c(0, 8)) +
  labs(title="各學年學士班各年級學生比例") + 
  scale_fill_brewer(palette = "Dark2") +
  scale_alpha_continuous(range=c(0.6, 0.9)) +
  theme(axis.text.x=element_blank(),
        axis.ticks = element_blank(),
        axis.title=element_blank(), axis.text=element_blank(),
        panel.border=element_blank(), panel.background = element_blank())
