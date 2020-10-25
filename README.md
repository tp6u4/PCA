# PCA
Data:NBA

---
title: "PCA_NBA"
author: "Max Chen"
date: "2020/8/16"
output:
  html_document: 
    toc: true
    toc_depth: 6
    toc_float:
      collapsed: False
      smooth_scroll: False
---

```{r setup, include=FALSE}     
require(knitr)
require(kableExtra)
require(readxl)
require(reshape2)
require(ggplot2)
require(tibble)
require(plotly)
require(nsprcomp)
require(tidyverse)
require(dplyr)
require(dbplyr)
knitr::opts_chunk$set(echo = FALSE) #只產出output作為global obtions
options(knitr.table.format = "html") 
```


```{r Input Database,include = FALSE}

nba.wage <- read_excel("/Users/sky/Documents/碩士論文/2017-2018薪資資料.xlsx", sheet = "2016-17")
nba.wagev2 <- nba.wage[, -c(3:9)]
nba.wagev3 <- nba.wagev2[complete.cases(nba.wagev2$`FG%`), ] 
nba.wagev3[is.na(nba.wagev3)] <- 0 
nba.wagev3$Pos[413] <- "PF"
#dim(nba.wagev3)
#table(complete.cases(nba.wagev3))
```
# 一、主成份分析*(PCA)*  
1. **將高維度資料降低維度，使用低維度資料表達，如：BMI指數、加權平均分數**  
2. **找出「有效表達資料的新變數」，新變數為原變數的線性組合，並且捕捉最多的資料變異量*Variation***  
3. **原解釋變數高度相關 *correlated*，有幾個解釋變數，則有幾個相對的主成份**  

# 二、演算法(模型精神與參數估計)---   
a. **每個主成份都是原始變數的加權平均**  
b. **主成份間彼此不相關*(垂直)***  
c. **越前面主成份解釋越多變異**  
d. **拋棄較後面*PCA*進行降維**  
  $$Y_{1} = a_{11}X_{1} + a_{12}X_{2} +...+  a_{1p}X_{p}$$
  $$Y_{2} = a_{21}X_{1} + a_{22}X_{2} +...+  a_{2p}X_{p}$$
  $$...$$
  $$Y_{p} = a_{p1}X_{1} + a_{p2}X_{2} +...+  a_{pp}X_{p}$$  
  
# 三、參數估計:估計$a_{ij}$達成兩大目標，保持不相關與極大化變異數
  $$Var(Y_{i}) = Var(a_{i1}X_{1} +...+ a_{ip}X_{p})  
    = a_{i1}^{2}Var(X_{1}) +...+ a_{ip}^{2}Var(X_{p}) + 
    2a_{i1}a_{i2}Cov(X_{1},X_{2}) + 2a_{ip-1}a_{ip}Cov(X_{p-1},X_{p})$$
* **目標:極大化$Var(Y_{i})$**  
* **限制式:保持第i個主成份與前i-1個不相關**  
* **係數非負限制:非負每一個係數$a_{ij}$都需要≥0**  
* **係數稀疏限制:第i個主成份係數非零(第i個主成份係數a_i1...a_ip非零個數小於k)**   

# 四、*PCA*資料前處理   
1. **進行變數標準化**  
2. **利用標準化變數估計主成份分析參數並得到主成份分數**  

# 五、*PCA*分析結果詮釋  
1. **每個主成份內的變數，若為負數則與該主成份互為反向關係，絕對值數字越大越不相關(貢獻越多)**  
2. **利用熱圖 *HeatMap* 視覺化權重矩陣**  
3. **利用點狀圖 *DotChart* 視覺化 *PC* 的權重**  
4. **利用「解釋變異量」決定要保留多少主成份**  
   + 準則1:超過平均值  
   + 準則2:特定轉折處  
   + 準則3:解釋超過80%  
5. **運用二元圖 *BitPlot* 理解個體、變數與主成分的關係**  
   + 每個點為所選取對應的主成份分數(降維、分群)  
   
# 六、實作過程  

## 第一步:觀察各解釋變數x相關性

```{r correlation_table}
kable(head(cor(nba.wagev3[, 3:ncol(nba.wagev3)]))) %>% 
  kable_styling() %>%
  scroll_box(height = "250px")
```


## 第二步:使用熱圖(heatmap)視覺化變數間的相關程度
1. **使用 ggplot2 繪製相關係數的熱圖**  
2. **必須先將資料整理成 tidy 的「Var1 - Var2 - 相關係數」資料架構**
3. **我們可以利用 reshape2套件中的melt函數輕鬆把矩陣格式轉換成 tidy 資料**
4. **用geom_tile(繪製方形圖)繪製相關係數的熱圖囉！可以看到很清楚的變數群聚現象**

```{r 熱圖觀察變項的相關程度}
head(melt(cor(nba.wagev3[, 3:ncol(nba.wagev3)])),5)  #reshape2套件
ggplot(melt(cor(nba.wagev3[, 3:ncol(nba.wagev3)])), 
       aes(x = Var1, y = Var2)) + 
    geom_tile(aes(fill = value), colour = "gray") + #這邊color為隔線顏色
    #scale_fill_gradient2為漸層顏色設定、midpoint為中點數值(default=0)、mid為cor為0顏色
    scale_fill_gradient2(low = "turquoise", high = "brown", 
                         mid = "white", midpoint = 0) +
    #guides為設定圖例位置及顏色, 這邊如果寫colour = guide_legend(),圖例標題不會成功
    guides(fill = guide_legend(title = "Correlation")) +    
    theme_bw() + 
    #調整相關座標軸、圖例位置的設定theme, legend.position = "bottom" 圖例位置會在下面
    theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
          axis.title = element_blank())
```

* **得分PTS與FG,FGA,2PA,PA...等相關**  
* **失誤TOV得分PTS高度相關**  
* **有效命中率(eFG%)與命中率高度相關，因其公式為命中率推導而來**

## 第三步:資料建模與分析
1. **PCA 模型可以利用 R 語言中的 prcomp 函數**  
2. **其中輸入的資料矩陣先標準化在做參數估計，可以設定 scales = T**

```{r PCA model, ,echo=TRUE, results="hide"}
pca.model <- prcomp(nba.wagev3[, 3:ncol(nba.wagev3)], scale = T)
```

* **出來結果，sdev表示每個PCA可以解釋的變異數為多少;rotation為變數的係數矩陣**
* **center 為標準化時每個變數的中心; scale 為標準化時每個變數的尺度**
```{r}
summary(pca.model) 
```


## 第四步:建立負荷量表選取合適主成份
1. **透過解釋變異 / 累積解釋比率圖來選取主成份**  
2. var：該主成份解釋變異數的數值 **`pca.model$sdev^2`**  
3. **prop：該主成份解釋變異數的比率 = PC 變異數 / 總變異**  
4. **cum_prop：該主成份解釋變異數的累積比率**

```{r set-up variation proportion, echo = TRUE }
var.exp <-tibble(
    pc = paste0("PC_", formatC(1:22, width = 2, flag = "0")),
    var = pca.model$sdev^2,
    prop = pca.model$sdev^2 / sum(pca.model$sdev^2), 
    cum_prop = cumsum(pca.model$sdev^2) / sum(pca.model$sdev^2))
```

```{r}
head(var.exp)
```
- **前三個PCA即解釋了超過80%以上的變異量**

- **利用 plotly 套件建立解釋變異的條狀圖**
```{r, warning=FALSE}
plot_ly(
    x = var.exp$pc,
    y = var.exp$cum_prop,
    type = "bar") %>%
    layout(
        title = "Cumulative Proportion by Each Principal Component",
        xaxis = list(type = 'Principal Component', tickangle = -60),
        yaxis = list(title = 'Proportion'),
        margin = list(r = 30, t = 50, b = 50, l = 30))  #旁邊留白的尺寸
```


* **上述分析，建議取前三個主成份即可。接著可以更仔細了解每一個主成份的係數** *rotation(特徵向量)*
* 主成份的係數矩陣，可以透過 **`pca.model$rotation`**  


```{r}
kable(head(pca.model$rotation)) %>% 
  kable_styling() %>%
  scroll_box(height = "200px")
```
  

- **跟共變異數矩陣(correlation matrix)一樣的視覺化方法。這裏取前三個主成份**  

```{r}
ggplot(melt(pca.model$rotation[, 1:3]), aes(x = Var2, y = Var1)) + 
    geom_tile(aes(fill = value), colour = "gray") + #這邊color為隔線顏色
    #scale_fill_gradient2為漸層顏色設定、midpoint為中點數值(default=0)、mid為cor為0顏色
    scale_fill_gradient2(low = "turquoise", high = "brown", 
                         mid = "white", midpoint = 0) +
    #guides為設定圖例位置及顏色, 這邊如果寫colour = guide_legend(),圖例標題不會成功
    guides(fill = guide_legend(title = "Correlation")) +    
    theme_bw() + 
    #調整相關座標軸、圖例位置的設定theme, legend.position = "bottom" 圖例位置會在下面
    theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
          axis.title = element_blank())
```

* **PC1主要為綜合數據指標(得分PTS、失誤TOV、進攻籃板DRB和罰球數量FTA)**
* **PCA2主要是3分球指標**
* **PCA3主要是命中率的指標**
* **但由於該熱圖仍有負數指標，即與主成分為負相關變數，資料分析較為雜亂不好解釋**

## 第五步：建立非負稀疏主成份分析(non-negative sparce PCA)  
1. **我們發現上面的主成份其實很難解釋，所以改採用非負稀疏主成份分析**
2. **利用 nsprcomp 套件中的 *nscumcomp* 完成，其中有兩個重要的參數**
3. ***k* 為非 0 係數個數，通常是「每個主成份期待非 0 係數個數」x 變數個數**
4. ***nneg* 是否希望所有係數都非負，TRUE 代表有非負限制**

```{r NSPCA model, echo = TRUE, results = "hide"}
set.seed(1234)
nspca.model <- nscumcomp(nba.wagev3[, 3:24], nneg = T, scale. = T, k = 150)
```

- **同樣的依照PCA model, NSPCA model則需要 7個主成份才能解釋8成以上的變異量，因為捨去負數關係**  

```{r}
summary(nspca.model) 
#nspca.model$sdev^2
```

```{r, echo = TRUE }
var.exp <- tibble(
    pc = paste0("PC_", formatC(1:22, width = 2, flag = "0")),
    var = nspca.model$sdev^2,
    prop = (nspca.model$sdev)^2 / sum((nspca.model$sdev)^2),
    cum_prop = cumsum((nspca.model$sdev)^2 / sum((nspca.model$sdev)^2)))
```


```{r}
head(var.exp, 8)
```

- **7個主成份才能解釋8成以上的變異量** *(0.81)*

```{r, warning = FALSE}
plot_ly(
    x = var.exp$pc,
    y = var.exp$cum_prop,
    type = "bar"
) %>%
    layout(
        title = "Cumulative Proportion by Each Principal Component",
        xaxis = list(type = 'Principal Component', tickangle = -90),
        yaxis = list(title = 'Proportion'),
        margin = list(r = 30, t = 50, b = 70, l = 50)
    )
```

- **非負稀疏主成份的係數權重。從熱圖中可以看到**  


```{r}
ggplot(melt(nspca.model$rotation[, 1:7]), aes(x = Var2, y = Var1)) + 
    geom_tile(aes(fill = value), colour = "gray") + #這邊color為隔線顏色
    #scale_fill_gradient2為漸層顏色設定、midpoint為中點數值(default=0)、mid為cor為0顏色
    scale_fill_gradient2(low = "turquoise", high = "brown", 
                         mid = "white", midpoint = 0) +
    #guides為設定圖例位置及顏色, 這邊如果寫colour = guide_legend(),圖例標題不會成功
    guides(fill = guide_legend(title = "Correlation")) +    
    theme_bw() + 
    #調整相關座標軸、圖例位置的設定theme, legend.position = "bottom" 圖例位置會在下面
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
          axis.title = element_blank())

```

- **主成份 1 重點為「罰球指標FT,FTA,FG,3PA」**
- **主成份 2 重點為「籃板指標TRB,PF,PTS」**
- **主成份 3 重點為「2分球指標2PA,BLK,AST」**
- **主成份 4 重點為「防守抄截指標STL,TOV,FG%」**
- **主成份 5 重點為「投球數指標FGA,2PA」**
- **主成份 6 重點為「命中率指標3P%,FT%,BLK」**
- **主成份 7 重點為「第二波進攻指標2P,ORB」**


## 第六步:球員各別分析

* **球員個別分析(找出特別球員的方法，繪製「主成份分數」與「該主成份係數最大變數」的散佈圖**
* **比如下圖可觀察，PC2籃板指標與球員得分比較**
* **給定PTS, 比較PC2--- **
  + **James Harden與 Russell Westbrook，可以知道相似的得分，但後者有較多的籃板，顯示其打球積極度高**
* **給定PC2, 比較PTS--- **
  + **Dwight Howard與AnthoneDavis，可知道相同的PC2下，後者有較高的得分，顯示身價較高**
```{r, echo=TRUE}
nspca.score <- data.frame(nspca.model$x)
row.names(nspca.score) <- nba.wagev3$Player
```

```{r}
plot_ly(
    x = nspca.score[, 2],
    y = nba.wagev3$PTS,
    text = nba.wagev3$Player,
    type = "scatter",
    mode = "markers") %>% 
    layout(
    title = "PTS v.s. PC_1 Score: Scatter Plot",
    xaxis = list(title = 'TRB PC_2'),
    yaxis = list(title = 'Points'),
    margin = list(r = 30, t = 50, b = 70, l = 50)
)
```

* **另外透過不同主成份的散佈圖，也可以找到多種面向都很傑出的球員如:PC_4 V.S. PC_6**
* **給定 PC_4, PC_6較低：比較Russell Westbrook與James Harden 有差不多的抄截和3分球效率**
* **給定 PC_6, PC_4較低：比較Hassan Whiteside與Anthone Davis，同樣的抄截下，後者有較高的3分效率**

```{r}
plot_ly(
    x = nspca.score[, 4],
    y = nspca.score[, 6],
    text = nba.wagev3$Player,
    type = "scatter",
    mode = "markers"
) %>% layout(
    title = "PC_4 v.s. PC_6 Score: Scatter Plot",
    xaxis = list(title = 'STL_index'),
    yaxis = list(title = '3P%_index'),
    margin = list(r = 30, t = 50, b = 70, l = 50)
)
```
