#regfit fitting and drawing graph in R
#by Nobuyuki Horie
#test push2
#for Rscript
library(ggplot2)
regfit <- function(){
cat("Fitting data and drow graph.\n")
#check output_data directory
if(dir.exists("./output_data")) {
  dir.create("./output_data")
}
#file = readline("File name to save?[graph.txt]:")
cat("File name to save?[./output_data/graph.txt]:")
file_name <- readLines(file("stdin"), n=1)
close(file("stdin"))

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
#answer = readline("OK?(or no)")
cat("OK?(or no)")
answer <- readLines(file("stdin"), n=1)
close(file("stdin"))
if(answer == "no") {
  return(1)
}

str = capture.output(fdata)
cat(str,file=file_name,sep="\n",append=TRUE)

str = capture.output(summary(reg2))
cat(str,file=file_name,sep="\n",append=TRUE)

cat("Drawing graph...\n")

#xtitle = readline("Title for x axis:")
cat("Title for x axis:")
xtitle <- readLines(file("stdin"), n=1)
close(file("stdin"))

#ytitle = readline("Title for y axis:")
cat("Title for y axis:")
ytitle <- readLines(file("stdin"), n=1)
close(file("stdin"))

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
g <- g + ggtitle(paste("Regfit Graph:",date(),sep=""))
g <- g + xlab(xtitle) + ylab(ytitle)
#image output
#png("./output_data/graph.png")
#plot(g)
#dev.off
ggsave(file="./output_data/graph.png",plot=g)
wdir <- getwd()
print(paste("Open file...",wdir,"/output_data/graph.png",sep=""))
#cat("¥n")
browseURL(paste(wdir,"/output_data/graph.png",sep=""))
#browseURL("/home/horie/git/regfit/output_data/graph.png")


input_data=""
#cat("uniroot output\n","root\ty_valu\titer\tinit.it\testim.prec\n",file=file_name,append=TRUE)
while(input_data !="end") {
#input = readline("Input y value or end:")
cat("Input y value or end:")
input_data<- readLines(file("stdin"), n=1)
close(file("stdin"))

print(paste("input is:",input_data,sep=""))

if(input_data != "end") {
  ninput = as.numeric(input_data)
  print(ninput)

  if((ninput > max(y)) | (ninput < min(y))) {
    cat("Out of range!","\n")
    cat("x:",ninput,"y: out of range","\n")
    } else {
    yexpect = uniroot(function(x) fitfunc(x) - ninput,interval = c(min(x),max(x)))
    cat("Expected x value:",unlist(yexpect),"\n") 
    cat("x:",ninput,"y: ",unlist(yexpect),"\n",file=file_name,append=TRUE)
    }
  } else {
     cat("Program end.")
  }
}
}
#main
regfit()


