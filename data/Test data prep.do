// Create imaginary data for Part I
clear all
set more off
set obs 30000

version 13
set seed 458267

gen regions = int(runiform()*9)

replace region = 1 if inrange(region,0,3)
replace region = 2 if inrange(region,4,5)

gen x =runiform()

sort x
drop x

gen weight = uniform()*15000
replace weight =10000*runiform() if weight<1000

//Income per capita
forval z=1/200{
	gen income_`z' = region*runiform()*4000 + rnormal()*200
}


//Taxes

forval z=1/200{
	if `z'<35 loc der decrease
	else local der
	anchoredpct income_`z', anchor(income_`z') nq(100) gen(u) `der'
	gen tax`z' = u*runiform()*100 + (rnormal()*20)^2
	drop u
}

save "C:\Users\WB378870\OneDrive - WBG\0.GPWG misc\Stata Functions\Tests\Incomes.dta", replace
