#regfit fitting and drawing graph in R
#by Nobuyuki Horie
#test push2
#for interactive R
library(ggplot2)
regfit <- function(){
cat("Fitting data and drow graph.\n")
#cat("File name to save?[graph.txt]:")
#file_name <- readLines(file("stdin"), n=1)
file_name = readline("File name to save?[./output_data/graph.txt]:")
if(file_name == "") {
  file_name = "./output_data/graph.txt"
}

cat("R fitting data\n",file=file_name,append=FALSE)

cat("Input Data\n")

fdata = data.frame(x=0,y=0)
fdata = edit(fdata)
x = fdata$x
y = fdata$y
reg2 = lm(y ~ x + I(x ^ 2))
print(summary(reg2))
answer = readline("OK?(or no)")
if(answer == "no") {
  return(1)
}

str = capture.output(fdata)
cat(str,file=file_name,sep="\n",append=TRUE)

str = capture.output(summary(reg2))
cat(str,file=file_name,sep="\n",append=TRUE)

cat("Drawing graph...\n")
xtitle = readline("Title for x axis:")
ytitle = readline("Title for y axis:")


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
g <- g + ggtitle(paste("Regfit Graph:",date(),sep=""))
g <- g + geom_line(data=line_data.frame,aes(x=lx,y=ly))
g <- g + xlab(xtitle) + ylab(ytitle)
plot(g)

input=""
cat("uniroot output\n","root\ty_valu\titer\tinit.it\testim.prec\n",file=file_name,append=TRUE)
while(input !="end") {
input = readline("Input y value or end:")
if(input != "end") {
  ninput = as.numeric(input)
  if((ninput > max(y)) | (ninput < min(y))) {
    cat("Out of range!","\n")
    cat("x:",ninput,"y: out of range","\n")
    } else {
    yexpect = uniroot(function(x) fitfunc(x) - ninput,interval = c(min(x),max(x)))
    cat("Expected x value:",unlist(yexpect),"\n") 
    cat("x:",ninput,"y: ",unlist(yexpect),"\n",file=file_name,append=TRUE)
    }
  }
}
}
#main
regfit()


