##################################################
# 本次課程繪製樹狀地圖 (Tree Map)
# 樹狀地圖表現出個體組成整體集合的情形，
# 個體在圖形上所占面積為其在某一種數值屬性上的值。
# 此外，還可利用顏色表現個體的另一種屬性：
# 類別屬性可以使用離散型色彩尺度，
# 表現同一類別的個體占整體的比例；
# 數值屬性則可以用連續型色彩尺度，
# 表現所有個體的傾向。
# 此外，還可將相同類別的個體聚集成群體，
# 觀察每個具有相同類別的群體占整體的比例。
#
##################################################
library(tidyverse)
library(readr)
library(ggplot2)
library(treemapify)

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
# 屬性：科系名稱、大一學生人數、女生與男生人數差比例
#
shu_std_no <- tdf %>%
  filter(學校名稱=="世新大學") %>%
  filter(year==106) %>%             # 取出106學年資料
  filter(等級別=="B 學士") %>%      # 取出學士班資料
  filter(grepl("^一年級", key)) %>% # 取出一年級學生資料
  select(科系名稱, key, value)  %>% # 選取科系名稱、年級與人數等變數
  spread(key=key, value=value) %>%  # 將各系男女生資料整合成一筆紀錄
  rowwise() %>%                     # 產生男生或女生人數的最大值
  mutate(max_value=max(一年級男生, 一年級女生)) %>%
  ungroup() %>%
  arrange(max_value) %>% # 依照最大值將科系進行排序
  mutate(科系名稱=factor(科系名稱, levels=科系名稱, ordered=TRUE)) %>%
  mutate(sex_ratio=(一年級女生-一年級男生)/max_value) %>% # 計算女生與男生人數差比例
  mutate(total_value=一年級男生+一年級女生) # 計算大一學生人數
  
# 畫成樹狀圖 (Treemap)
ggplot(shu_std_no, aes(area=total_value, fill=sex_ratio, label=科系名稱)) +
  geom_treemap() +
  geom_treemap_text(place = "centre", color="black") +
  labs(title="世新大學106學年各系大一男女生人數比較") +
  scale_fill_gradient2(low="blue", high="red",
                        limits=c(-1.0, 1.0),
                        name="女男人數差比例")
  

########################################################################
# 示範案例
# 世新大學106學年碩士一般與在職生比例
mst_std_no <- tdf %>%
  filter(學校名稱=="世新大學") %>%
  filter(year==106) %>%             # 取出106學年資料
  filter(grepl("M", 等級別)) %>%      # 取出碩士班資料
  group_by(科系名稱, dn)%>%
  summarise(value.sum=sum(value)) %>%
  ungroup()

ggplot(mst_std_no, aes(area=value.sum, fill=dn, subgroup=科系名稱)) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre")
