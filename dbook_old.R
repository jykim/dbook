
check.and.install.packages <- function(list.of.packages)
{
	new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
	if(length(new.packages)) install.packages(new.packages, repos="http://cran.us.r-project.org")
}

eval.ci <- function(pop, level = 0.95, boot.r = 1000, debug = F)
{
	alpha = (1-level)/2
	#t1 = qt(alpha, length(pop)-1)
	#t2 = qt(level + alpha, length(pop)-1)
	
	# Analytical C.I.
	n = length(pop)
	z1 = qnorm(alpha)
	z2 = qnorm(level + alpha)
	stderr.a = sd(pop) / sqrt(length(pop))
	ci95.a = c( mean(pop) + stderr.a * z1,  mean(pop) + stderr.a * z2)
	
	# Bootstrapped C.I.
	bsm = aaply(seq(1, boot.r), 1, function(i){
		sample.i = pop[sample(length(pop), length(pop), replace=T)]
		if(i <= 10 & debug)
			print(cbind(t(sample.i[1:10]), mean(sample.i)))
		mean(sample.i)
	})
	stderr.b = sd(bsm)
	ci95.b = quantile( bsm, c(alpha, level + alpha) )
	# Visualize
	if( debug ){
		plot(density(bsm), lty=2, xlab = "Sample Mean" ,main = sprintf("Comparing the Sampling Distribution & Confidence Interval"))
		x <- seq(-5, 5, length=100)
		lines(x, dnorm(x, mean(pop), stderr.a))
		arrows(ci95.a[1], 0.1, ci95.a[2], 0.1, code = 3)
		arrows(ci95.b[1], 0.2, ci95.b[2], 0.2, code = 3, lty=2)		
	}
	data.frame(stderr.a=stderr.a , stderr.b=stderr.b, 
			   ci95.a1=ci95.a[1], ci95.b1=ci95.b[1], 
			   ci95.a2=ci95.a[2], ci95.b2=ci95.b[2])
}

build.model <- function(feats, tgt){
	model.str = sprintf("%s ~ %s", tgt, paste(feats, collapse=" + "))
	print(model.str)
	eval(model.str)
}

build.model.nmo <- function(dt, idx.tgt){
	build.model(colnames(dt)[-idx.tgt], colnames(dt)[idx.tgt])
}

rmse <- function(c1, c2){
	sqrt(mean((c1 - c2) ^ 2))
}

accuracy <- function(tbl){
	sum_diag = 0
	for(i in 1:sqrt(length(tbl))){
		sum_diag = sum_diag + tbl[i, i]
	}
	sum_diag / sum(tbl)
}

get.ctab <- function(pred, act){
	table(ifelse(pred > 0, "W (pred)", "L (pred)"), ifelse(act > 0, "W (act)", "L (act)"))
}

cval.model <- function(cvt, fun.model, k.fold, ...){
	cvt$fold = sample(1:k.fold, nrow(cvt), replace=T)
	crt = data.frame()
	for(i in 1:k.fold){
		cvtr = cvt[cvt$fold != i,]
		cvte = cvt[cvt$fold == i,]
		cvte$res = predict(fun.model(cvtr, ...), cvte)
		crt = rbind(crt, cvte)
	}
	crt
}


cval.model2 <- function(cvt, fun.model, k.fold, predict = predict(...) , ...){
	cvt$fold = sample(1:k.fold, nrow(cvt), replace=T)
	crt = data.frame()
	for(i in 1:k.fold){
		cvtr = cvt[cvt$fold != i,]
		cvte = cvt[cvt$fold == i,]
		cvte$res = predict(fun.model(cvtr, ...), cvte)
		#cvte$red = cvte[[idx.tgt]] - cvte$res
		crt = rbind(crt, cvte)
	}
	crt
}


eval.model <- function(cvt, idx.tgt, fun.model, ratio.train = 0.5){
	cvt$fold = rbinom(nrow(cvt), 1, ratio.train)
	cvtr = cvt[cvt$fold == 1,]
	cvte = cvt[cvt$fold == 0,]
	cvtr$res = predict(fun.model(cvtr), cvtr)
	cvte$res = predict(fun.model(cvtr), cvte)
	list(r=cvtr, e=cvte)
}

plot.vss <- function(vs)
{
	vss = summary(vs)
	par(mfrow=c(2,2))
	plot(vss$rsq)
	lines(vss$adjr2)
	plot(vss$rss, type="l")
	plot(vss$cp, type="l")
	plot(vss$bic, type="l")
	vss
}