reliability.plot.verify<- function(x, ...){
#if(sum(class(A) == "prob.bin") < 1){
#  warning("This function works only on probability forecast \n binary outcome objects. \n")}else{
assign("y.i", x$y.i)
assign("obar.i", x$obar.i)
assign("prob.y", x$prob.y)

do.call("reliability.plot.default", list(y.i, obar.i, prob.y, ...))


}
