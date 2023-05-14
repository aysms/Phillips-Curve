*EC207 EP2
*Mingsong Chen
*Dec 7, 2022

clear all
set more off
capture log close
/* indicate where the Stata dataset is located */
cd D:\EC207\EP2\Stata

/* open log file  */
log using EC207_EP21.log, replace

*Read in the dataset
{
import excel using phillips_monthly_19931_202112, clear
rename A INFL
rename B UNPL
save EC207_EP2_raw, replace
}

*Variable Generation
{
use EC207_EP2_raw, clear
*Generate month and set time
gen date = m(1993m1) + _n-1
gen t = _n
gen t2 = _n^2
format date %tm
tsset date

*Generate unemployment rate squared
gen UNPL2 = UNPL^2
order date UNPL UNPL2 INFL

*Mark the testing sample
gen test = date >= m(2021m9)
save EC207_EP2, replace
}

*Section 2: Data Description
{
use EC207_EP2, clear
*Summary Stat
tabstat UNPL UNPL2 INFL if test==0, stat(mean median sd min max) columns(statistics) format(%8.2f) long

*Trend
*Linear
reg INFL t if test==0
outreg2 using "D:\EC207\EP2\Stata\Tables and Figures\EP2_T1.doc", bdec(4) sdec(4) bracket replace
reg INFL t t2 if test==0
outreg2 using "D:\EC207\EP2\Stata\Tables and Figures\EP2_T1.doc", bdec(4) sdec(4) bracket append
test t t2
*No significant time trend

*Graph linear time trend
twoway (tsline INFL, ms(o)) (lfit INFL date), ytitle(Percentage)
graph export "D:\EC207\EP2\Stata\Tables and Figures\F1.png", as(png) replace

*Test for Granger causality
*Max lag=12 given monthly data
var INFL UNPL INFL if test==0,lag(1/12)
vargranger
*Since both are not significant, we conclude independence, no Granger Causality

*ARCH and GARCH
*Start with GARCH(4,4) given AR(4) for INFL
arch INFL L(1/4).INFL if test==0, arch(1/4) garch(1/4) iter(20)
arch INFL L(1/4).INFL if test==0, arch(1/3) garch(1/3) iter(20)
arch INFL L(1/4).INFL if test==0, arch(1/3) garch(1/2) iter(20)
arch INFL L(1/4).INFL if test==0, arch(1/2) garch(1/2) iter(20)
*GARCH(2,2) is selected
outreg2 using "D:\EC207\EP2\Stata\Tables and Figures\EP2_T3.doc", bdec(4) sdec(4) bracket replace
predict garch_predict, v
*Conditional Variance figure
twoway (tsline garch_predict if test==0, ms(o))
graph export "D:\EC207\EP2\Stata\Tables and Figures\F2.png", as(png) replace
*High volatility near 2008 - 2009, which makes sense given the recession.
}

*Section 3: Empirical Analysis
{
use EC207_EP2, clear
tsset date

*E. Test for Unit Root
*Select lag length for INFL
dfuller INFL if test==0,l(12)  regress
dfuller INFL if test==0,l(11)  regress
dfuller INFL if test==0,l(10)  regress
dfuller INFL if test==0,l(9)  regress
dfuller INFL if test==0,l(8)  regress
dfuller INFL if test==0,l(7)  regress
*Lag = 7
*Reject Unit Root


*Select lag length for UNPL
forvalues i=1/12{
dfuller UNPL if test==0,l(`i') regress
}
*Lag = 1
*Fail to reject, so first-difference

*Generate first-differenced UNPL and UNPL2
gen dif_UNPL = UNPL-L1.UNPL
gen dif_UNPL2 = dif_UNPL^2

*F. Test for cointegration
*Generate Price level as inflation is I(0)
gen pl=100*(1+INFL/100) if _n==1
replace pl=L1.pl*(1+INFL/100) if _n>=2

*Test for unit root in Price Level
dfuller pl if test==0,l(12)  regress
dfuller pl if test==0,l(11)  regress
dfuller pl if test==0,l(10)  regress
dfuller pl if test==0,l(9)  regress
dfuller pl if test==0,l(8)  regress
*Lag=8
*Fail to reject

*Test for cointegration between price level and unemployment
reg pl UNPL if test==0
predict presid, resid
dfuller presid if test==0
*Fail to reject unit root, so not cointegrated

*D. Test for linear time trend
reg INFL t if test==0
*Insignificant

*C. Test for seasonality
*Generate month dummys
gen date2=dofm(date)
format date2 %d
gen month=month(date2)
drop date2

reg INFL i.month if test==0
outreg2 using "D:\EC207\EP2\Stata\Tables and Figures\EP2_T6.doc", bdec(4) sdec(4) bracket replace
testparm i.month
*Reject. Yes, inflation has a seasonal component

*B. Test for serial correlation
*First Order
reg INFL L(1/12).INFL L(0/12).dif_UNPL L(0/12).dif_UNPL2 i.month if test==0
outreg2 using "D:\EC207\EP2\Stata\Tables and Figures\EP2_T5.doc", bdec(4) sdec(4) bracket replace
predict iresid1, resid
reg iresid1 L(1).iresid1 L(0/12).dif_UNPL L(0/12).dif_UNPL2 L(1/12).INFL i.month if test==0
outreg2 using "D:\EC207\EP2\Stata\Tables and Figures\EP2_T5.doc", bdec(4) sdec(4) bracket append
testparm L(1).iresid1
*Fail to reject, p-value=0.658, No 1st order serial correlation

*Fourth Order
reg iresid1 L(1/4).iresid1 L(0/12).dif_UNPL L(0/12).dif_UNPL2 L(1/12).INFL i.month if test==0
outreg2 using "D:\EC207\EP2\Stata\Tables and Figures\EP2_T5.doc", bdec(4) sdec(4) bracket append
testparm L(1/4).iresid1
*Fail to reject, p-value=0.369, No 4th order serial correlation

*A. Test for lag length
reg INFL L(1/12).INFL L(0/12).dif_UNPL L(0/12).dif_UNPL2 i.month if test==0
*INFL is significant at lag=12, so add one more lag
test L12.dif_UNPL L12.dif_UNPL2

reg INFL L(1/13).INFL L(0/12).dif_UNPL L(0/12).dif_UNPL2 i.month if test==0
*INFL is not significant at lag=13, so fix it at lag = 12
test L12.dif_UNPL L12.dif_UNPL2

forvalues j=1/11{
local i=12-`j'
reg INFL L(1/12).INFL L(0/`i').dif_UNPL L(0/`i').dif_UNPL2 i.month if test==0
test L`i'.dif_UNPL L`i'.dif_UNPL2
}

*Model2: INFL lag=12, UNPL lag=0
reg INFL L(1/12).INFL dif_UNPL dif_UNPL2 i.month if test==0
test dif_UNPL dif_UNPL2

outreg2 using "D:\EC207\EP2\Stata\Tables and Figures\EP2_T4.doc", bdec(4) sdec(4) bracket replace

*Store RMSE for Model 2
scalar rmse=e(rmse)

*Get dif_UNPL sd
tabstat INFL UNPL UNPL2 dif_UNPL dif_UNPL2 if test==0, stat(mean median sd min max) columns(statistics) format(%8.4f) long

*SR IM
di 2*_b[dif_UNPL2]*-.0061225+_b[dif_UNPL]
* 2*0.002148*-.0061225 - 0.08509=-0.0851
*Std Coeff: -0.0851*0.6101/0.3361
di -0.0851*0.6101/0.3361

*LR IM
di (2*_b[dif_UNPL2]*-.0061225+_b[dif_UNPL])/(1-_b[L1.INFL]-_b[L2.INFL]-_b[L3.INFL]-_b[L4.INFL]-_b[L5.INFL]-_b[L6.INFL]-_b[L7.INFL]-_b[L8.INFL]-_b[L9.INFL]-_b[L10.INFL]-_b[L11.INFL]-_b[L12.INFL])
*-0.0851/(1-.52696+0.18584-0.01528-0.03093+0.09103-0.01714+0.032926+0.058538-0.014539-0.012799-0.13648+0.18201) = -0.1069
*Std Coeff: -0.1069*0.6101/0.3361
di -0.1069*0.6101/0.3361
*Graph fitted vs actual INFL
predict yhat
twoway (scatter INFL date if test==0, ms(o)) (line yhat date), ytitle(Percentage)
graph export "D:\EC207\EP2\Stata\Tables and Figures\F3.png", as(png) replace

*Forecast
*Predict UNPL for the last four periods using AR(4)
arima UNPL L(1/4).UNPL if test==0
predict xhat, dynamic(m(2021m9))
gen xhatd = D.xhat
replace dif_UNPL=xhatd if test==1
replace dif_UNPL2=dif_UNPL^2 if test==1
*Month dummies
forvalues i=1/12{
gen m`i'=month==`i'
}
*Using arima function to predict INFL in the last four periods using Model2
arima INFL L(1/12).INFL dif_UNPL dif_UNPL2 m1-m12 if test==0
predict yhatd, dynamic(m(2021m9))
list date INFL yhatd if test==1

*Test for Accuracy
gen pe=INFL-yhatd if test==1
gen pe2=pe^2
gen sum_pe2=sum(pe2)
scalar sumpe2 = sum_pe2[348]
di sumpe2/rmse^2
*Reject, so Model 2 is not well-specified.
}
