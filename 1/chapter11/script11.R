## 第11章スクリプト

# パッケージの読み込み
library(mokken)
# データの読み込み(データは作業ディレクトリに置くこと．確認は"getwd()"で．)
fuan<-read.csv("fuan11_20.csv")
# データ内容の確認．Q1からQ10までが，0から2までの3件法になっている．
head(fuan)

# 11.1.2 顕在単調性を用いた潜在単調性の確認．
# check.monotonicityには主に2つの引数．
# minviは，単調性の仮定に反する確率が出たとしても，
# minviの値以下の確率の差であれば，違反とは見なさないという設定．
# minsizeは，残余得点によるグループ化の際に用いるグループの最少人数．
mono.list<-check.monotonicity(fuan)
# 結果の要約(表1.4)．ac，viなどは本文と一緒．
summary(mono.list)
# 項目5(図11.1)と6(図11.2)のISRF．
# curvesは，IRFを描画するかISRFを描画するかの選択．
# plot.ciは，信頼区間を描画するかの選択．
plot(mono.list,items=5,curves="ISRF",plot.ci="false")
plot(mono.list,items=6,curves="ISRF",plot.ci="false")

# 11.1.3 尺度性係数の算出．
# $Hijは項目間尺度性係数，$Hiは項目尺度性係数，$Hは尺度全体の尺度性係数．
# 表1.4
coefH(fuan)

# 11.1.4 非交差の確認．
# pmatrixの場合
# check.monotonicity同様minviの設定が可能．
pmat.list<-check.pmatrix(fuan)
# 結果の要約．ac，viの解釈の仕方は顕在単調性と類似．
summary(pmat.list)
# pmatrixの値の変化を描画．
# 引数pmatrixをpppとすればP(++)，pmmとすればP(--)，bothとすれば両方描画．
# plot.ciの設定も可能．
plot(pmat.list)

# 残余得点の場合
# 引数としてminviとminsizeが設定可能．
rest.list<-check.restscore(fuan)
# 結果の要約(表1.4)．解釈はこれまでのものを参照．
summary(rest.list)
# 残余得点でグループ分けしたISRF．
# item.pairsで，どの項目ペアを描画するか設定する．
# 1と設定すれば，項目1と2，2と設定すれば項目2と3，n-1と設定すれば項目1とn
# nと設定すれば項目2と3(以下略)
plot(rest.list)
# 項目5と6の非交差の確認(図11.3)
plot(rest.list,plot.ci="false",item.pairs=31)
# 項目2と10の非交差の確認(図11.4)
plot(rest.list,plot.ci="false",item.pairs=17)

# 11.2.1 モッケン尺度による尺度構成．
# 引数searchをnormal(デフォルト)にするとモッケンの方法．
# gaと置くと遺伝的アルゴリズムも可能．
# lowerboundで，尺度の下限(本文中のd)を定められる．デフォルトは0.3
# Scaleの値が同じ項目が同じ尺度．0はどの尺度にも属さない．
# 表11.5
aisp(fuan)

# 11.2.2 IIOによる尺度構成．
# 引数methodで，IIOを確認するために用いる手法を設定できる．
# 本文中の方法はMIIO(デフォルト)．minviやminsizeの選択可能．
iio.list <- check.iio(fuan)
# 結果の要約．$backward.selectionで，徐々に項目が削られているのが確認できる．
# 表11.5
summary(iio.list)
# 結果の描画．残余得点と引数は概ね同じ．
plot(iio.list)

# 比較のための因子分析．因子数2，プロマックス回転．
# 表11.5
factanal(fuan,factors=2,rotation="promax")

# 項目削減後の結果．
# IIOによる項目選択
fuan.miio<-fuan[,c(1:3,6,7,9,10)]
# 項目削減後のH係数(表11.5)
coefH(fuan.miio)
# モッケン，因子分析による項目選択
fuan.msp<-fuan[,c(1,6,7,9,10)]
# 項目削減後のH係数(表11.5，11.6)
coefH(fuan.msp)

# 表11.6の作成
# 顕在単調性
msp.mono.list<-check.monotonicity(fuan.msp)
summary(msp.mono.list)
# 非交差
msp.rest.list<-check.restscore(fuan.msp)
summary(msp.rest.list)
