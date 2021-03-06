require(ggplot2)

greyscale_multibar <- function(dataframe, title, xlabel, ylabel, aux=NULL) {
  p <- ggplot(data=dataframe, aes(y=Y)) + 
    geom_bar(stat="identity",position=position_dodge(),colour="black") +
    geom_errorbar(aes(ymin=Y-Error, ymax=Y+Error), width=.2, position=position_dodge(.9)) +
    scale_fill_grey(start = 0.6, end = 0.2, na.value = "grey50") +
    ggtitle(title) +
    #ylim(0,100) + 
    xlab(xlabel) +
    ylab(ylabel) +
    aux
  return(p)
}