botscale_dataframe <- function (means, errs) {
 
  framed = data.frame(Events = factor(c(rep("baseline",4),rep("unfiltered",4),rep("filtered",4)), c("baseline","unfiltered","filtered")),
                      Bots = factor(c(10,50,100,200,10,50,100,200,10,50,100,200), levels=c(10,50,100,200)),
                      Error = errs,
                      Y = means)
  return(framed)
}