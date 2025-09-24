############################################
#####第4章：多値データのための多次元IRT#####
############################################
### 動作確認環境
## R 2.15.2
## package mirt 0.5.0
###以下では適用例のスクリプトを示す。

###パッケージの読み込み
library(mirt)
###データの読み込み
BF.CEdat <- read.csv("CE.csv", header=T)
head(BF.CEdat); tail(BF.CEdat) #表4.4
###モデルの指定()
##関数confmirt.model()によってモデルの記述を行う。
#左辺には因子，右辺にはその因子からパスを受ける変数を記述する。
#COV = F1*F2とすることで，因子間の相関も仮定することができる。
model.bf0 <- confmirt.model()
F1 = 1-10
F2 = 1-10


###モデルの実行
##関数confmirtによって分析を実行する。主な引数は以下の通り。
#data:データ行列あるいはデータフレーム。欠測値がある場合にはNAと入力。
#model:確認的モデルならconfmirt.model()で指定したモデルのオブジェクト，探索的モデルなら仮定する因子数を指定する。
#itemtype:モデルの種類を指定する。"Rasch","2PL","3PL","graded"(段階反応モデル),"gpcm"(一般化部分採点モデル)など。
#D:尺度因子。デフォルトではD=1.702の正規計量。D=1.0とすればロジスティック計量に。
#####段階反応モデルの実行
bfCE_grm <- confmirt(data = BF.CEdat, model = model.bf0, itemtype="graded", D=1)
###推定結果の出力(表4.5)
coef(bfCE_grm)


###IRSの描画(図4.7と図4.8の左列)
##関数itemplotを用いて，IRSや項目情報量関数を描画する。主な引数は以下の通り。
#object:分析の際に定義したオブジェクト
#item：描画したい項目番号
#type:"trace"でIRS，"info"で情報量関数を描画。デフォルトは"trace"。
#screen:zは-180⇔180で図の回転，z=0なら回転なし。xは90⇔-90で図の傾き。
itemplot(bfCE_grm, item = 1, type = "trace", screen = list(z = 140, x = -70))
itemplot(bfCE_grm, 4, screen = list(z = 150, x = -70))
itemplot(bfCE_grm, 5, screen = list(z = 150, x = -70))
itemplot(bfCE_grm, 6, screen = list(z = -70, x = -70))
itemplot(bfCE_grm, 8, screen = list(z = -70, x = -70))
itemplot(bfCE_grm, 9, screen = list(z = -70, x = -70))

#####一般化部分採点モデルの実行
bfCE_gpcm <- confmirt(BF.CEdat, model= model.bf0, itemtype="gpcm", D=1)
###推定結果の出力(表4.6)
coef(bfCE_gpcm)

##MDISCの計算
#各項目の項目母数の推定値はbfCE_gpcm@pars[[項目番号]]に収められている。
#@par[1]でa1，@par[2]でa2を表す。
#(4.6)式に従って計算。
#項目C1
round(sqrt(bfCE_gpcm@pars[[1]]@par[1]^2+bfCE_gpcm@pars[[1]]@par[2]^2), 3)
#項目C2
round(sqrt(bfCE_gpcm@pars[[2]]@par[1]^2+bfCE_gpcm@pars[[2]]@par[2]^2), 3)
#項目C3
round(sqrt(bfCE_gpcm@pars[[3]]@par[1]^2+bfCE_gpcm@pars[[3]]@par[2]^2), 3)
#項目C4
round(sqrt(bfCE_gpcm@pars[[4]]@par[1]^2+bfCE_gpcm@pars[[4]]@par[2]^2), 3)
#項目C5
round(sqrt(bfCE_gpcm@pars[[5]]@par[1]^2+bfCE_gpcm@pars[[5]]@par[2]^2), 3)
#項目E1
round(sqrt(bfCE_gpcm@pars[[6]]@par[1]^2+bfCE_gpcm@pars[[6]]@par[2]^2), 3)
#項目E2
round(sqrt(bfCE_gpcm@pars[[7]]@par[1]^2+bfCE_gpcm@pars[[7]]@par[2]^2), 3)
#項目E3
round(sqrt(bfCE_gpcm@pars[[8]]@par[1]^2+bfCE_gpcm@pars[[8]]@par[2]^2), 3)
#項目E4
round(sqrt(bfCE_gpcm@pars[[9]]@par[1]^2+bfCE_gpcm@pars[[9]]@par[2]^2), 3)
#項目E5
round(sqrt(bfCE_gpcm@pars[[10]]@par[1]^2+bfCE_gpcm@pars[[10]]@par[2]^2), 3)

##MDIFFの計算
#@par[3]から@par[8]までは，閾値母数β_0からβ_5までに対応する。
#常にβ_0=0であるため，分母では@par[4]から@par[8]までを足し上げている。
#(4.7)式に従って計算。
#項目C1
round(-(bfCE_gpcm@pars[[1]]@par[4]+bfCE_gpcm@pars[[1]]@par[5]+
bfCE_gpcm@pars[[1]]@par[6]+bfCE_gpcm@pars[[1]]@par[7]+bfCE_gpcm@pars[[1]]@par[8])/
(5*sqrt(bfCE_gpcm@pars[[1]]@par[1]^2+bfCE_gpcm@pars[[1]]@par[2]^2)), 3)
#項目C2
round(-(bfCE_gpcm@pars[[2]]@par[4]+bfCE_gpcm@pars[[2]]@par[5]+
bfCE_gpcm@pars[[2]]@par[6]+bfCE_gpcm@pars[[2]]@par[7]+bfCE_gpcm@pars[[2]]@par[8])/
(5*sqrt(bfCE_gpcm@pars[[2]]@par[1]^2+bfCE_gpcm@pars[[2]]@par[2]^2)), 3)
#項目C3
round(-(bfCE_gpcm@pars[[3]]@par[4]+bfCE_gpcm@pars[[3]]@par[5]+
bfCE_gpcm@pars[[3]]@par[6]+bfCE_gpcm@pars[[3]]@par[7]+bfCE_gpcm@pars[[3]]@par[8])/
(5*sqrt(bfCE_gpcm@pars[[3]]@par[1]^2+bfCE_gpcm@pars[[3]]@par[2]^2)), 3)
#項目C4
round(-(bfCE_gpcm@pars[[4]]@par[4]+bfCE_gpcm@pars[[4]]@par[5]+
bfCE_gpcm@pars[[4]]@par[6]+bfCE_gpcm@pars[[4]]@par[7]+bfCE_gpcm@pars[[4]]@par[8])/
(5*sqrt(bfCE_gpcm@pars[[4]]@par[1]^2+bfCE_gpcm@pars[[4]]@par[2]^2)), 3)
#項目C5
round(-(bfCE_gpcm@pars[[5]]@par[4]+bfCE_gpcm@pars[[5]]@par[5]+
bfCE_gpcm@pars[[5]]@par[6]+bfCE_gpcm@pars[[5]]@par[7]+bfCE_gpcm@pars[[5]]@par[8])/
(5*sqrt(bfCE_gpcm@pars[[5]]@par[1]^2+bfCE_gpcm@pars[[5]]@par[2]^2)), 3)
#項目E1
round(-(bfCE_gpcm@pars[[6]]@par[4]+bfCE_gpcm@pars[[6]]@par[5]+
bfCE_gpcm@pars[[6]]@par[6]+bfCE_gpcm@pars[[6]]@par[7]+bfCE_gpcm@pars[[6]]@par[8])/
(5*sqrt(bfCE_gpcm@pars[[6]]@par[1]^2+bfCE_gpcm@pars[[6]]@par[2]^2)), 3)
#項目E2
round(-(bfCE_gpcm@pars[[7]]@par[4]+bfCE_gpcm@pars[[7]]@par[5]+
bfCE_gpcm@pars[[7]]@par[6]+bfCE_gpcm@pars[[7]]@par[7]+bfCE_gpcm@pars[[7]]@par[8])/
(5*sqrt(bfCE_gpcm@pars[[7]]@par[1]^2+bfCE_gpcm@pars[[7]]@par[2]^2)), 3)
#項目E3
round(-(bfCE_gpcm@pars[[8]]@par[4]+bfCE_gpcm@pars[[8]]@par[5]+
bfCE_gpcm@pars[[8]]@par[6]+bfCE_gpcm@pars[[8]]@par[7]+bfCE_gpcm@pars[[8]]@par[8])/
(5*sqrt(bfCE_gpcm@pars[[8]]@par[1]^2+bfCE_gpcm@pars[[8]]@par[2]^2)), 3)
#項目E4
round(-(bfCE_gpcm@pars[[9]]@par[4]+bfCE_gpcm@pars[[9]]@par[5]+
bfCE_gpcm@pars[[9]]@par[6]+bfCE_gpcm@pars[[9]]@par[7]+bfCE_gpcm@pars[[9]]@par[8])/
(5*sqrt(bfCE_gpcm@pars[[9]]@par[1]^2+bfCE_gpcm@pars[[9]]@par[2]^2)), 3)
#項目E5
round(-(bfCE_gpcm@pars[[10]]@par[4]+bfCE_gpcm@pars[[10]]@par[5]+
bfCE_gpcm@pars[[10]]@par[6]+bfCE_gpcm@pars[[10]]@par[7]+bfCE_gpcm@pars[[10]]@par[8])/
(5*sqrt(bfCE_gpcm@pars[[10]]@par[1]^2+bfCE_gpcm@pars[[10]]@par[2]^2)), 3)

###IRSの描画(図4.7と図4.8の右列)
itemplot(bfCE_gpcm, 1, screen = list(z = 60, x = -70))
itemplot(bfCE_gpcm, 4, screen = list(z = 60, x = -70))
itemplot(bfCE_gpcm, 5, screen = list(z = 60, x = -70))
itemplot(bfCE_gpcm, 6, screen = list(z = 20, x = -70))
itemplot(bfCE_gpcm, 8, screen = list(z = 20, x = -70))
itemplot(bfCE_gpcm, 9, screen = list(z = 20, x = -70))


###適合度指標(表4.7)
(bfCE_grm)
(bfCE_gpcm)
