roc.plot.prob.bin<- function(x, ...){
# retreives data from a verify object.

assign("obs", x$obs)
assign("pred", x$pred)
#assign("thresholds", x$thres)
do.call("roc.plot.default", list(obs, pred, ...) )

}
