/*
We have available to us KM estiamtes of 
PCa death and other cause mortality.
We need to figure out how to convert these numbers
into estimates that acocunt for each other 
(perhaps competing risks?)
*/

clear
set obs 10000

local dodlambda = 10
local deadlambda = 15

*generate risk of PCa death if untreated
	g ttdod = rweibull(1,`dodlambda') /*mean time is the product*/

*generate time to death from other causes
	g ttocdead = rweibull(1,`deadlambda') /*mean time is the product*/

*this is the observed time to death
	g deadstat = (ttocdead<ttdod) + 1 /*1=DOD 2=DOC*/
	g ttdead = min(ttdod, ttocdead)

*competing risks estimates
	stset ttdead, f(deadstat==1)
	stcompet ci = ci, compet1(2)
	gen dodci = ci if deadstat == 1
	gen deadci = ci if deadstat == 2
		label var dodci "CR: DOD"
		label var deadci "CR: Dead OC"

*KM for dod
	sts generate dodkm = s
		replace dodkm = 1 - dodkm
		label var dodkm "KM: DOD"
	
*KM for death from oc
	stset ttdead, f(deadstat==2)
	sts generate deadkm = s
		replace deadkm = 1 - deadkm
		label var deadkm "KM: Dead OC"
		
* trying to transform KM to CR with a conversion factor
g prob = 1/(`dodlambda'/`deadlambda' + 1)
g dodnew = dodkm*prob
g deadnew = deadkm*prob
	label var dodnew "WTF: DOD"
	label var deadnew "WTF: Dead OC"


*  KM estiamtes only (the observed data)
twoway line  dodkm deadkm ttdead if ttdead<=20, sort

*  KM + CR estiamtes (CR is our target)
twoway line  dodkm deadkm ttdead if ttdead<=20, sort || ///
		line dodci deadci  ttdead if ttdead<=20, sort lwidth(thick thick) 
	


twoway line dodci deadci  ttdead if ttdead<=20, sort lwidth(thick thick) lcolor(red blue) || ///
	line  dodkm deadkm ttdead if ttdead<=20, sort lcolor(red blue) || ///
	line  dodnew deadnew ttdead if ttdead<=20, sort lcolor(red blue) lpattern(longdash longdash) 


g deadstat10yr = 0 if ttdead>10
	replace deadstat10yr = 1 if ttdod<ttocdead & ttdead<=10
	replace deadstat10yr = 2 if ttocdead<ttdod & ttdead<=10
	
g deadstat15yr = 0 if ttdead>15
	replace deadstat15yr = 1 if ttdod<ttocdead & ttdead<=15
	replace deadstat15yr = 2 if ttocdead<ttdod & ttdead<=15

*printing 10 and 15 year estimates
foreach year in 10 15 {
	disp "`year' Year Risk"
	foreach est in ci km {
		foreach outcome in dod dead  {
			qui sum `outcome'`est' if ttdead<=`year'
				local risk = string(`r(max)', "%9.3f")
				disp "    `: var label `outcome'`est'': `risk'"  
		}
	}
}
tab1 deadstat10yr deadstat15yr
	
