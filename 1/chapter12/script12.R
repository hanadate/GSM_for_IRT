## 第12章スクリプト

# パッケージの読み込み
library(KernSmoothIRT)
# データの読み込み(データは作業ディレクトリに置くこと．確認は"getwd()"で．)
fuan<-read.csv("fuan11_20.csv")
# 使用項目の絞り込み
fuan.ks<-fuan[,c(1,6,7,9,10)]
# データ内容の確認．5項目について，0から2までの3件法になっている．
head(fuan.ks)

# 12.3 カーネル平滑化による推定
# 関数ksIRTで平滑化を実行する．
# 引数key(ベクトル)で，多肢選択問題であれば項目の正答，
# 順序カテゴリであれば，項目の最大カテゴリを指定する．
# 引数scaleで，名義データであればnominal，
# 順序データであればordinalと指定する．
# 双方が混ざっている場合，項目数と同じ長さのベクトルで設定し，
# 1であれば名義データ，0であれば順序データと見なす．
# 引数kernelで平滑化のためのカーネルを指定する．
# gaussian，quadratic，uniformのいずれか．デフォルトではガウスカーネル．
# 引数bandwidthでカーネル平滑化におけるバンド幅の設定．
# CVと設定した場合，個々の項目のバンド幅は交差妥当化により選択される． 

# 一様カーネルによる平滑化
fuan.uni<-ksIRT(responses=fuan.ks, key=c(2,2,2,2,2), scale="ordinal",kernel="uniform")
# 二次カーネルによる平滑化
fuan.qua<-ksIRT(responses=fuan.ks, key=c(2,2,2,2,2), scale="ordinal",kernel="quadratic")
# ガウスカーネルによる平滑化
fuan.gauss<-ksIRT(responses=fuan.ks, key=c(2,2,2,2,2), scale="ordinal")

# 結果の描画．
# itemsでプロットする項目を設定．
# 引数plottypeでは，様々な情報をプロット出来る．以下はその一例．
# 基本は各カテゴリの選択確率(ICRF，オプションとしてはOCC)
# densityで和得点の確率密度関数．
# ICCで，和得点が与えられた下での各項目に対する期待される得点．
# infoで，各項目の情報関数．

# 一様カーネルによる描画
plot(fuan.uni)
# 図12.2
plot(fuan.uni,items=4,main="Q9")

# 二次カーネルによる描画
# ただし，このデータでも，helpにあるデータを用いても
# 綺麗に平滑化はされないので，使用は推奨できない．
plot(fuan.qua)

# ガウスカーネルによる描画．
plot(fuan.gauss)
#図12.1
plot(fuan.gauss,items=4,main="Q9")
#図12.4
plot(fuan.gauss,items=5,main="Q10")

# 段階反応モデルに必要なパッケージの読み込み．
library(ltm)
# 段階反応モデルによる推定．
fuan.grm<-grm(fuan.ks)
# 段階反応モデルのICRFの描画(図12.3)．
plot(fuan.grm,items=4,main="Q9",col=1,zrange=c(-2,2.5))