library(tidyverse)

data(iris)

# 選取名稱為Sepal.Length的Variable
select(iris, Sepal.Length)

# 選取名稱開頭為Sepal的Variables
select(iris, starts_with("Sepal"))

# 選取名稱結尾為Length的Variables
select(iris, ends_with("Length"))

# 選取前五筆Observations
slice(iris, 1:5)

# 選取Sepal.Width大於4的Observations
filter(iris, Sepal.Width>4)

# 新增一個Variable (Sepal.Ratio)
mutate(iris, Sepal.Ratio=Sepal.Length/Sepal.Width)

# 按照Sepal.Length由小到大排列
arrange(iris, Sepal.Length)

# 按照Sepal.Length由大到小排列
arrange(iris, desc(Sepal.Length))

# 選出Petal.Width最大的前五筆Observations
top_n(iris, 5, Petal.Width)

# 選出Petal.Width最小的前五筆Observations
top_n(iris, -5, Petal.Width)

# 計算iris中Observationa的個數
summarise(iris, count=n())

# 計算Petal.Length的平均數與中位數
summarise(iris, mean=mean(Petal.Length), med=median(Petal.Length))

# 依據Species將Observations分組
group_by(iris, Species)

# 依據Sepal.Length排序後，選出前5筆Observations
iris %>%
  arrange(desc(Sepal.Length)) %>%
  slice(1:5)

# 依據Species分組後，統計各Species的Sepal.Length之平均數與中位數
iris %>%
  group_by(Species) %>%
  summarise(mean=mean(Sepal.Length), med=median(Sepal.Length))
