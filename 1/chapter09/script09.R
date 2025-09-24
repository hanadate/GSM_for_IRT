## 2013/3/20 
## 第9章スクリプト
## R version 2.15.3 (2013-03-01) -- "Security Blanket"で動作確認

# 連続反応モデルのパッケージの読み込み
# 初めて使用する場合は以下のコマンドを実行し，パッケージをインストールする。
# install.packages("EstCRM", depend=T)

library(EstCRM)


# データセットの読み込み
# data(EPIA)
EPIA <- read.table("EPIA.csv", sep=",", header=T)
head(EPIA, 10)


#　項目母数の推定
max.item <- c(112,112,112,112,112)    # 理論最大値
min.item <- c(0,0,0,0,0)              # 理論最小値
max.EMCycle=200                       # 解を求める際の反復数

EstCRMitem(EPIA, max.item, min.item, max.EMCycle)


#　ブートストラップ推定値を計算する
bootCRM(EPIA, max.item, min.item, max.EMCycle)


#　被験者母数の推定
max.EMCycle=500      # 解を求める際の反復数

CRM <- EstCRMitem(EPIA, max.item, min.item, max.EMCycle)   # 項目母数の推定

par <- CRM$param   # 項目母数のみを取り出す

EstCRMperson(EPIA, par, min.item, max.item)


#　項目カテゴリ反応曲線の描画
CRM <- EstCRMitem(EPIA, max.item, min.item, max.EMCycle = 500)
par <- CRM$param

plotCRM(par, item=1, min.item, max.item)
plotCRM(par, item=2, min.item, max.item)
plotCRM(par, item=3, min.item, max.item)
plotCRM(par, item=4, min.item, max.item)
plotCRM(par, item=5, min.item, max.item)


#　標準化残差の計算
max.item <- c(112,112,112,112,112)
min.item <- c(0,0,0,0,0)

CRM <- EstCRMitem(EPIA, max.item, min.item, max.EMCycle=500)
par <- CRM$param

CRMthetas <- EstCRMperson(EPIA, par, min.item, max.item)
fit <- fitCRM(EPIA, par, CRMthetas, max.item, group=10)
fit$fit.stat























