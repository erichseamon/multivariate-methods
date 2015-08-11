ppd.plot <- function(data,lower,upper,type) {
  if(require(ggplot2)){
    print("ggplot2 package already installed. Good!")
  } else {
    print("trying to install ggplot2 package...")
    install.packages("ggplot2", dependencies=TRUE)
    suppressPackageStartupMessages(require(ggplot2))
  }
  if (type == "a") {
    a <- ggplot(data=mydata) + geom_line(aes(x=dates,y=prob,color=group)) + scale_x_continuous(limits =c(lower,upper)) + xlab("Calibrated date") + ylab("Probability density")
    print(a)
  } else {
    if (type == "b") {
      b <- ggplot(data=mydata) +geom_area(position="identity", aes(x=dates,y=prob,fill=group), alpha=0.5) + scale_x_continuous(limits =c(lower,upper)) + xlab("Calibrated date") + ylab("Probability density")
      print(b)
    } else {
      if (type == "c") {
        a <- ggplot(data=mydata) + geom_line(aes(x=dates,y=prob,color=group)) + scale_x_continuous(limits =c(lower,upper)) + xlab("Calibrated date") + ylab("Probability density")
        c <- a + geom_area(position="identity",aes(x=dates,y=prob,fill=group), alpha=0.5) + scale_x_continuous(limits =c(lower,upper)) + xlab("Calibrated date") + ylab("Probability density")
        print(c)
      }
    }
  }
}