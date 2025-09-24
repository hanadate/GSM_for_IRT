#
# 第７章「混合ラッシュモデル」
#


#
# 潜在クラス分析
#

# データの読み込み
# 豊田(2012)学力テスト1の8項目
# 項目32,33,34,35,44,45,46,49
# 内容は表7.4参照

dat <- read.csv("gaku1item8.csv", header=FALSE)

# パッケージの読み込み
library(poLCA)

# poLCAの仕様により0-1データを1-2データに変換
datPlus1 <- dat + 1
# ------ 引数に与える形式の準備
varName <- paste("V", 1:8, sep="")
datName <- paste("datPlus1[, ", 1:8, "]", sep="")
formulaName <- paste(varName, datName, sep="=", collapse=",")
f <- paste("cbind(", formulaName, ") ~ 1")
# ------ 形式の準備ここまで

# 1から3クラスを想定した潜在クラス分析
M1 <- poLCA(eval(parse(text=f)), nclass=1, data=dat, maxiter=5000, nrep=10, verbose=FALSE)
M2 <- poLCA(eval(parse(text=f)), nclass=2, data=dat, maxiter=5000, nrep=10, verbose=FALSE)
M3 <- poLCA(eval(parse(text=f)), nclass=3, data=dat, maxiter=5000, nrep=10, verbose=FALSE)

# 情報量規準（表7.5）の取得
matrix(c(M1$aic, M2$aic, M3$aic, M1$bic, M2$bic, M3$bic), ncol=2, dimnames=list(c("G=1", "G=2", "G=3"),c("AIC", "BIC")))

# 潜在クラス数1の場合の8項目の正答確率
round(matrix(unlist(M1$probs),ncol=2,byrow=TRUE)[,2],3)

# 潜在クラス数2の場合の8項目の正答確率
round(matrix(unlist(M2$probs), ncol=4, byrow=TRUE)[,3:4],3)
# 混合比率
round(M2$P,3)

# 潜在クラス数3の場合の8項目の正答確率
round(matrix(unlist(M3$probs), ncol=6, byrow=TRUE)[,4:6],3)
# 混合比率
round(M3$P,3)

#
# 混合ラッシュモデル
#

# パッケージの読み込み
library(psychomix)
# 零点と満点の被験者の除外
totalScores <- rowSums(dat)
datNo08 <- dat[(0 < totalScores & totalScores < ncol(dat)), ]
datNo08 <- as.matrix(datNo08)
# クラス数を1から3まで想定した2値型潜在ラッシュモデルの推定
# 潜在的合計点分布には倹約的なモデルを設定
# 出力として情報量規準を表示する（表7.6）
set.seed(1234)
( gaku8MeanVar <- raschmix(data = datNo08, k = 1:3, scores = "meanvar" ) )

# BICの値から潜在クラス数2を採用
gaku8Class2 <- getModel(gaku8MeanVar, which = 2)
# 項目困難度プロフィール（図7.2）
xyplot(gaku8Class2)
# 合計点分布の母数
parameters(gaku8Class2, which = "score")
# 合計点分布（表7.7）
round(scoreProbs(gaku8Class2),3)
# 混合比率
round(gaku8Class2@size/gaku8Class2@nobs,3)

#
# 共変量付き混合ラッシュモデル
#

# データの読み込みと整形
tmp <- read.csv("jugyoCov.csv",header=FALSE)
jugyoCov <- data.frame(x=tmp[,7])
jugyoCov$resp <- as.matrix(tmp[,1:6])

# 共変量をともなう混合ラッシュモデルによる分析の実行
# 潜在的合計点分布には倹約的なモデルを設定
# 出力として情報量規準を表示する（表7.9）
set.seed(4321)
( jugyoMeanVarCov <- raschmix(resp ~ x, data = jugyoCov, k = 1:3, scores = "meanvar" ) )

# クラス数2を採用
jugyoClass2 <- getModel(jugyoMeanVarCov, which = 2)
# 項目プロフィール（図7.3）
plot(jugyoClass2)
# 共変量の影響
parameters(jugyoClass2, which = "concomitant")

#
# 混合多値ラッシュモデル
#

# パッケージの読み込み
library(mixRasch)

# データの読み込み
jugyo3cat <- read.csv("jugyo3cat.csv", header=FALSE)

# 分析と情報量規準
# 混合部分採点モデル
#（実行時に項目母数の推定に用いられない零点と満点に関する警告発生）

set.seed(2345)
MixPCM1 <- mixRasch(jugyo3cat, steps=2, max.iter=500, conv.crit=.0001, n.c=1, model="PCM")

MixPCM2 <- mixRasch(jugyo3cat, steps=2, max.iter=500, conv.crit=.0001, n.c=2, model="PCM")

MixPCM3 <- mixRasch(jugyo3cat, steps=2, max.iter=500, conv.crit=.0001, n.c=3, model="PCM")

MixPCM4 <- mixRasch(jugyo3cat, steps=2, max.iter=500, conv.crit=.0001, n.c=4, model="PCM")

# 情報量規準の取得
matrix(c(
 MixPCM1$info.fit$AIC,
 MixPCM2$info.fit$AIC,
 MixPCM3$info.fit$AIC,
 MixPCM4$info.fit$AIC,
 MixPCM1$info.fit$BIC,
 MixPCM2$info.fit$BIC,
 MixPCM3$info.fit$BIC,
 MixPCM4$info.fit$BIC
), ncol=2, dimnames=list(c("G=1", "G=2", "G=3", "G=4"),c("AIC", "BIC")))

# 混合評定尺度モデル
#（実行時に項目母数の推定に用いられない零点と満点に関する警告発生）
set.seed(5432)
MixRSM1 <- mixRasch(jugyo3cat, steps=2, max.iter=500, conv.crit=.0001, n.c=1, model="RSM")

MixRSM2 <- mixRasch(jugyo3cat, steps=2, max.iter=500, conv.crit=.0001, n.c=2, model="RSM")

MixRSM3 <- mixRasch(jugyo3cat, steps=2, max.iter=500, conv.crit=.0001, n.c=3, model="RSM")

MixRSM4 <- mixRasch(jugyo3cat, steps=2, max.iter=500, conv.crit=.0001, n.c=4, model="RSM")

# 情報量規準の取得（表7.10）
matrix(c(
 MixRSM1$info.fit$AIC,
 MixRSM2$info.fit$AIC,
 MixRSM3$info.fit$AIC,
 MixRSM4$info.fit$AIC,
 MixRSM1$info.fit$BIC,
 MixRSM2$info.fit$BIC,
 MixRSM3$info.fit$BIC,
 MixRSM4$info.fit$BIC
), ncol=2, dimnames=list(c("G=1", "G=2", "G=3", "G=4"),c("AIC", "BIC")))


# クラス数を2とした混合評定尺度モデルの項目母数推定値
round(MixRSM2$LatentClass[[1]]$item.par$tau, 3)
round(MixRSM2$LatentClass[[2]]$item.par$tau, 3)
# 表7.11
round(rbind(
 MixRSM2$LatentClass[[1]]$item.par$delta.i,
 MixRSM2$LatentClass[[2]]$item.par$delta.i
),3)


