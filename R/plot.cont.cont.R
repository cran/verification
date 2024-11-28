plot.cont.cont <- function(x, ...){
assign("pred", x$pred )
assign("obs", x$obs )
do.call("conditional.quantile", list(pred, obs, ...) )

}
