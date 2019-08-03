library(ggplot2)
regfit <- function(){
cat("データのフィティングをします。\n")
file = readline("計算結果を保存するファイル名は?[graph.csv]:")
if(file == "") {
  file = "graph.csv"
}

cat("R fitting data\n",file=file,append=FALSE)

cat("データを入力してください。\n")

fdata = data.frame(x=0,y=0)
fdata = edit(fdata)
x = fdata$x
y = fdata$y
reg2 = lm(y ~ x + I(x ^ 2))
print(summary(reg2))
readline("OK?")

str = capture.output(fdata)
cat(str,file=file,sep="\n",append=TRUE)

str = capture.output(summary(reg2))
cat(str,file=file,sep="\n",append=TRUE)

cat("グラフを書きます.\n")
xtitle = readline("x軸データの見出し:")
ytitle = readline("y軸データの見出し:")


xmin = min(x)
xmax = max(x)
diff = xmax - xmin
xfit = seq(xmin,xmax,diff/50)
fitfunc <- function(x) {
  predict(reg2,data.frame(x=x))
}

#plot
#plot(x,y,xlab=xtitle,ylab=ytitle)
#lines(xfit,predict(reg2,data.frame(x=xfit)))

#plot ggplot2

line_data.frame=data.frame(lx=xfit,ly=predict(reg2,data.frame(x=xfit)))

g <- ggplot(fdata,aes(x=x,y=y))
g <- g + geom_point()
g <- g + geom_line(data=line_data.frame,aes(x=lx,y=ly))
g <- g + xlab(xtitle) + ylab(ytitle)
plot(g)

input=""
cat("uniroot output\n","root\ty_valu\titer\tinit.it\testim.prec\n",file=file,append=TRUE)
while(input !="end") {
input = readline("yの値は？:")
if(input != "end") {
  ninput = as.numeric(input)
  if((ninput > max(y)) | (ninput < min(y))) {
    cat("推定できる範囲を超えています。","\n")
    cat("x:",ninput,"y: out of range","\n")
    } else {
    yexpect = uniroot(function(x) fitfunc(x) - ninput,interval = c(min(x),max(x)))
    cat("xの値は:",unlist(yexpect),"\n") 
    cat("x:",ninput,"y: ",unlist(yexpect),"\n",file=file,append=TRUE)
    }
  }
}
}
#main
regfit()

