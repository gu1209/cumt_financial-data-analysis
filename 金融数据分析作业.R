library(lubridate)
library(xts)
library(quantmod)
library(Tushare)
library(TSA)
library(fUnitRoots)
library(urca)
library(fBasics)
library(forecast)
options (warn = -1)
###数据获取
api<-pro_api(token="731ee2eafa47ae6b8c98993e8b30001b51bd3085cd69d63870e745c9")
hxgf<-api(api_name ='daily',ts_code ='300427.SZ',start_date='20170101',end_date='20220101')
###时序类型转换
time<-as.Date(ymd(hxgf$trade_date))
hxgf_close_price<-xts(hxgf$close,order.by=time)
hxgf_close_return<-dailyReturn(hxgf_close_price,type ="arithmetic")[-1]#计算红相股份简单收益率
new_time<-index(hxgf_close_price)[965:1207]
hxgf_close_logreturn<-dailyReturn(hxgf_close_price,type ="log")[-1]#计算红相股份简单收益率
######给出价格序列和收益率序列的样本自相关图和偏自相关图#####
######绘制自相关图#######
acf(hxgf_close_price,lag=36)
acf(hxgf_close_return,lag=36)
### 绘制偏自相关图
pacf(hxgf_close_price,lag=36)
pacf(hxgf_close_return,lag=36)

#对价格序列和收益率序列进行混成检验
Box.test(hxgf_close_price,type='Ljung')
###混成检验p值小于0.05，拒绝原假设，即认为价格序列存在序列相关
Box.test(hxgf_close_return,type='Ljung')
###混成检验p值小于0.05，拒绝原假设，即认为收益率序列存在序列相关

####运用本模块讲授的的多种方法验证该只股票的价格波动是否符合有效市场假说####
#方法一：直接以价格序列为研究对象的检验方法：计算并检验相隔k期的股价的自相关系数
f1<-acf(hxgf_close_price)
plot(f1,main="自相关图" ,xlab="滞后阶数 ",col=4,lwd=2)
for (i in 1:20){print( Box.test(hxgf_close_price,type="Ljung-Box",lag=i))}
#####计算所得的p值显著小于临界值，拒绝原假设，红相股份的股票历史价格存在序列相关，即红相股份的交易市场未达到弱势有效

#方法二：建立股价序列的自回归模型，并对其系数进行显著性检验
hxgf_ar<-ar(hxgf_close_price,method = 'yule-walker')
hxgf_ar
adfTest(hxgf_ar$resid,type="ct")
Box.test(hxgf_ar$resid,lag=12,type="Ljung-Box")
#拟合模型为7阶
#残差通过adf检验，没有单位根，是平稳的序列。红相股份达到弱势有效

#方法三：对价格序列或其对数序列进行单位根检验，以判断其是否符合随机游走
adfTest(hxgf_close_price,type="ct")
adfTest(log(hxgf_close_price),type="ct")
#价格序列及其对数序列都无法通过单位根检验，表明其不平稳，该市场未达到弱势有效

#方法四：以收益率序列建模并对模型的干扰项序列进行相关性检验
Box.test(hxgf_close_logreturn,type = "Ljung-Box")
#p值为0.09732，即在5%的置信水平下，接受原假设，对数收益率序列没有相关性，红相股份达到弱势有效



#运用 ARMA 模型刻画价格或收益率序列的相关性
#收益率序列
adfTest(hxgf_close_return,type="ct")
#拒绝原假设，趋势平稳，序列是平稳序列
acf(hxgf_close_return)
#acf图二阶拖尾
pacf(hxgf_close_return)
#pacf图二阶拖尾
#建立ARMA(2,2)模型，来刻画收益率序列的相关性
xiangguanxing_hxgf<-arima(hxgf_close_return,order=c(2,0,2),fixed=c(NA,NA,NA,NA,0))
summary(xiangguanxing_hxgf)
plot(xiangguanxing_hxgf$residuals)
acf(xiangguanxing_hxgf$residuals)
pacf(xiangguanxing_hxgf$residuals)
Box.test(xiangguanxing_hxgf$residuals,type="Ljung-Box")
#残差序列接受BOX检验的原假设，即残差序列是平稳的，ARMA（2，2）模型对收益率序列的刻画效果很好


#####构建有关价格和收益率序列的 AR(p) 模型并进行平稳性检验和模型检验####
acf(hxgf_close_price)
pacf(hxgf_close_price)
#pacf图在3阶截尾,acf图拖尾，可以建立AR(3)模型
AR_jiage<-arima(hxgf_close_price,order = c(3,0,0))
summary(AR_jiage)
Box.test(AR_jiage$residuals, type='Ljung-Box')
#残差序列通过BOX检验，接受原假设即残差序列不存在序列相关性，模型通过平稳性检验
tsdiag(AR_jiage,gof=12)
abs(polyroot(c(1,-AR_jiage$coef[1:3])))
#残差检验通过，且所计算的特征值均在单位圆之外，这表明模型拟合效果好



pacf(hxgf_close_return)
acf(hxgf_close_return)
#pacf图在2阶截尾,acf图拖尾，可以建立AR(2)模型
AR_shouyi<-arima(hxgf_close_return,order = c(2,0,0),fixed = c(NA,NA,0))
summary(AR_shouyi)
Box.test(AR_shouyi$residuals, type='Ljung-Box')
#残差序列通过BOX检验，接受原假设即残差序列不存在序列相关性，模型通过平稳性检验
tsdiag(AR_shouyi,gof=12)
abs(polyroot(c(1,-AR_shouyi$coef[1:3])))
#残差检验通过，且所计算的特征值均在单位圆之外，这表明模型拟合效果好




#识别刻画价格和收益率序列的 ARMA 模型阶数并对 ARMA 模型进行参数估计与检验
m1_jiage<-eacf(hxgf_close_price,7,13)
jiage_ARMA<-arima(hxgf_close_price,order = c(1,0,2),transform.pars = FALSE)
Box.test(jiage_ARMA$residuals, type='Ljung-Box')
#残差序列通过BOX检验，接受原假设即残差序列不存在序列相关性，模型通过平稳性检验
tsdiag(jiage_ARMA,gof=12)
#残差检验通过，残差是平稳的，模型拟合效果好

m1_shouyi<-eacf(hxgf_close_return,7,13)
shouyi_ARMA<-arima(hxgf_close_return,order = c(2,0,2),transform.pars = FALSE)
Box.test(shouyi_ARMA$residuals, type='Ljung-Box')
#残差序列通过BOX检验，接受原假设即残差序列不存在序列相关性，模型通过平稳性检验
tsdiag(shouyi_ARMA,gof=12)
#残差检验通过，残差是平稳的，模型拟合效果好

#运用多种方法对价格和收益率序列进行单位根检验
#价格
#方法一：ADF检验
adfTest(hxgf_close_price,type=c('ct'))
#adf检验接受原假设，存在单位根，序列不平稳
#方法二：ADF检验的另一种表达
unitrootTest(hxgf_close_price,type=c('ct'))
##adf检验接受原假设，存在单位根，序列不平稳
#方法三：ADF检验的另一种表达
adf_jiage<-ur.df(hxgf_close_price,type='trend',selectlags='AIC')
summary(adf_jiage)
#第一个统计量（-2.483）大于5%临界值（-3.41），即表明接受原假设，认为价格序列存在单位根

#收益率
#方法一：ADF检验
adfTest(hxgf_close_return,type=c('ct'))
#adf检验拒绝原假设，不存在单位根，序列平稳
#方法二：ADF检验的另一种表达
unitrootTest(hxgf_close_return,type=c('ct'))
##adf检验拒绝原假设，不存在单位根，序列平稳
#方法三：ADF检验的另一种表达
adf_return<-ur.df(hxgf_close_return,type='trend',selectlags='AIC')
summary(adf_return)
#第一个统计量（-25.558）小于5%临界值（-3.41），即表明拒绝原假设，认为价格序列不存在单位根


#运用价格和收益率序列构建 ARIMA 模型并进行模型检验
#价格
jiage_arima<-auto.arima(hxgf_close_price)
jiage_arima
tsdiag(jiage_arima,gof=12)
#残差检验通过，模型拟合效果充分
#收益率
shouyi_arima<-arima(hxgf_close_return,order = c(2,0,2),fixed = c(NA,NA,NA,NA,0))
shouyi_arima
tsdiag(shouyi_arima,gof=12)
#残差检验通过
#auto.arima与通过ACF和PACF选定的阶数不一样，最后选择了利用ACF与PACF识别出的结果来建模。







#运用价格和收益率序列样本期内数据构建 ARIMA 模型并预测最近一年的价格与收益率序列且给出评价标准
hxgf_close_price_window<-window(hxgf_close_price,start = "2017-01-01" ,end="2021-01-01",freq=1)
air_arima<-auto.arima(hxgf_close_price_window)

air_predict<-predict(air_arima,n.ahead=243)
m1<-xts(c(air_predict$pred)+2*c(air_predict$se),order=new_time)
m2<-xts(c(air_predict$pred),order=new_time)
m3<-xts(c(air_predict$pred)-2*c(air_predict$se),order=new_time)

plot(window(hxgf_close_price,start = "2021-01-01"), ylim =c(0,50),type="b",lwd =2)
lines(m1,col="green", lwd =3,lty=3,type="l")
lines(m2,col="red", lwd =3,lty=2,type="l",pch=2)
lines(m3,col="green", lwd =3,lty=3,type="l")
#平均绝对预测误差
mean(abs(m2-window(hxgf_close_price,start = "2021-01-01")))
#预测根均方误差
sqrt( mean((m2 - window(hxgf_close_price,start = "2021-01-01"))^2) )
summary(abs(window(hxgf_close_price,start = "2021-01-01")))
#价格的均值为14.59，预测的绝对误差为9.73，这表明模型预测效果并不很好

hxgf_close_return_window<-window(hxgf_close_return,start = "2017-01-01" ,end="2021-01-01",freq=1)
air_arima_1<-arima(hxgf_close_return_window,order = c(2,0,2),fixed = c(NA,NA,NA,NA,0))

air_predict_1<-predict(air_arima_1,n.ahead=243)
m11<-xts(c(air_predict_1$pred)+2*c(air_predict_1$se),order=new_time)
m21<-xts(c(air_predict_1$pred),order=new_time)
m31<-xts(c(air_predict_1$pred)-2*c(air_predict_1$se),order=new_time)

plot(window(hxgf_close_return,start = "2021-01-01"), ylim =c(-0.1,0.2),type="b",lwd =2)
lines(m11,col="green", lwd =3,lty=3,type="l")
lines(m21,col="red", lwd =3,lty=3,type="l",pch=2)
lines(m31,col="green", lwd =3,lty=3,type="l")
#平均绝对预测误差
mean(abs(m21-window(hxgf_close_return,start = "2021-01-01")))
#预测根均方误差
sqrt( mean((m21 - window(hxgf_close_return,start = "2021-01-01"))^2) )
summary(abs(window(hxgf_close_return,start = "2021-01-01")))
#ARIMA预测结果差，收益率的均值为0.024，平均绝对预测误差与均值差异很小，表明模型的预测效果不够理想





























