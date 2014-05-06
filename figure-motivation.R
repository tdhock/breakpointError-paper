works_with_R("3.1.0",
             "tdhock/ggplot2@98cefe4d653ce8f214177b66dc030c2f3c725ffb",
             changepoint="1.1.1")

params <-
  list(mean=data.frame(mean=c(-1, 1/2, -1/2, 1),
         sd=c(1, 1, 1, 1)),
       sd=data.frame(mean=c(0, 0, 0, 0),
         sd=c(1, 3, 1/2, 2)))
segment.last <- c(400, 500, 750, 1000)
change.after <- segment.last[-length(segment.last)]
segment.first <- c(1, change.after+1)
segments <- data.frame()
signals <- data.frame()
changes <- data.frame(change.after, what="model")
set.seed(1)
for(change.param in names(params)){
  param.df <- params[[change.param]]
  these.segments <-
    data.frame(param.df, last=segment.last, first=segment.first,
               size=segment.last-segment.first+1,
               change.param, what="model")
  segments <- rbind(segments, these.segments)
  for(segment.i in 1:nrow(these.segments)){
    seg <- these.segments[segment.i,]
    base <- with(seg, first:last)
    signal <- rnorm(seg$size, seg$mean, seg$sd)
    this.signal <- data.frame(base, signal, segment.i, change.param,
                              what="data")
    signals <- rbind(signals, this.signal)
  }
}
two.sigs <- 
ggplot()+
  geom_point(aes(base, signal, color=what), data=signals, pch=1)+
  geom_vline(aes(xintercept=change.after+1/2, color=what),
             data=changes, lty="dashed")+
  scale_color_manual(values=c(data="black", model="green"))+
  scale_fill_manual(values=c(data="black", model="green"))+
  scale_x_continuous("base position",
                     breaks=segment.first)+
  geom_rect(aes(xmin=first-1/2, xmax=last+1/2,
                ymin=mean-sd, ymax=mean+sd,
                fill=what),
            data=segments, alpha=1/10, color=NA)+
  geom_segment(aes(first-1/2, mean, xend=last+1/2, yend=mean, color=what),
               data=segments)+
  facet_grid(change.param ~ .)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "cm"))
pdf("figure-motivation.pdf")
print(two.sigs)
dev.off()
