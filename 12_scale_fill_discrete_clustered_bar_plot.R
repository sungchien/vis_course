##################################################
# 本次課程運用色彩做為離散型屬性(項目為有限個數)的視覺提示
# 以下是ggplot2中適用於離散型屬性的色彩尺度
# scale_color_brewer (scale_fill_brewer)
# scale_color_identity (scale_fill_identity)
# scale_color_manual (scale_fill_manual)
# scale_color_viridis_d (scale_fill_viridis_d)
# scale_color_grey (scale_fill_grey)
# scale_color_hue (scale_fill_hue)
#
# 本次課程以群組長條圖為例
# 示範上述色彩尺度
#
# 群組長條圖將個體依據兩種類別屬性分群
# 第一種分群利用位置做為視覺提示
# 第二種分群利用色彩做為視覺提示
# 針對第一種分群上的個體
# 比較第二種屬性在數值屬性上的統計結果(個數、總和、平均、...)
#
# x軸：第一種類別屬性， y軸：數值屬性
# 色彩：第二種類別屬性
#
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
# 比較106學年度大學部不同性別下各年級學生人數
# 個體：各等級的大學部學生資料
# 第一種類別屬性(位置)：性別
# 第二種類別屬性(色彩)：年級別
# 數值屬性：學生人數

std_no_by_grade <- tdf %>%
  filter(year==106) %>%             # 取出106學年資料
  filter(grepl("B", 等級別)) %>%    # 取出大學部(學士班)資料
  mutate(grade=substr(key, 1, 3)) %>% # 產生年級資料
  mutate(grade=ifelse(grepl("[五六七延]", grade), "五年級(以上)", grade)) %>%  # 合併高年級資料
  mutate(sex=substr(key, 4, 5)) %>%   # 產生性別資料
  group_by(sex, grade) %>%         # 依據性別與年級別分群
  summarise(value.sum=sum(value)) # 統計各性別各年級學生人數

# 將群組長條圖的圖形儲存在pcl_bar中
pcl_bar <- ggplot(std_no_by_grade, aes(x=sex, y=value.sum, fill=grade)) +  # 畫出群組長條圖
  geom_col(position="dodge") +
  scale_y_continuous(breaks=seq(0, 120000, 10000)) +
  labs(x="性別", y="學生人數", title="比較106學年度不同性別各年級學生人數") +
  theme(axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"),
        axis.line.y = element_line(color="black"),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="grey80"),
        panel.grid.minor.y = element_line(color="grey90"))

# 畫出群組長條圖
pcl_bar

# 調整年級的次序
grade_lvl <- c("一年級", "二年級", "三年級", "四年級", "五年級(以上)")
std_no_by_grade <- std_no_by_grade %>%
  mutate(grade=factor(grade, levels=(grade_lvl), ordered=TRUE))
  
# 重新繪製群組長條圖
pcl_bar <- ggplot(std_no_by_grade, aes(x=sex, y=value.sum, fill=grade)) +  # 畫出群組長條圖
  geom_col(position="dodge") +
  scale_y_continuous(breaks=seq(0, 120000, 10000)) +
  labs(x="性別", y="學生人數", title="比較106學年度不同性別各年級學生人數") +
  theme(axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"),
        axis.line.y = element_line(color="black"),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="grey80"),
        panel.grid.minor.y = element_line(color="grey90"))

# 畫出群組長條圖
pcl_bar

# 使用Rbrewer調色盤 (scale_fill_brewer)         #
pcl_bar +
  scale_fill_brewer(palette="Spectral", guide=guide_legend(title="年級"))

# 使用Munsell Color System的Hue(色相環) (scale_fill_hue)               #
pcl_bar +
  scale_fill_hue(h = c(0, 360), l=80,
                 guide=guide_legend(title="年級"), direction=-1)

# 使用viridis colormap (scale_fill_viridis_d)   #
pcl_bar +
  scale_fill_viridis_d(option="C",
                       guide=guide_legend(title="年級"), direction=-1)

# 使用灰階 (scale_fill_grey)             #
pcl_bar +
  scale_fill_grey(start=0.5, end=0.2,
                  guide=guide_legend(title="年級"))

# 自行指定顏色 (scale_fill_manual)         #
grade_col <- c("red", "orange", "yellow", "green", "blue")
pcl_bar +
  scale_fill_manual(values=grade_col,
                    guide=guide_legend(title="年級"))


####################################################
# 練習
# 上面的例子比較不同性別在各年級上的學生人數
# 以下請練習比較各年級在不同性別上的學生人數
# 提示：在aes函數中修改位置與色彩的屬性
# 並請練習各種色彩尺度
