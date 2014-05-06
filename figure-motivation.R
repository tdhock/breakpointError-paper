works_with_R("3.1.0",
             breakpointError="1.0",
             Segmentor3IsBack="1.8",
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
segments <- NULL
signals <- NULL
changes <- data.frame(change.after, what="model")
model.segments <- NULL
model.changes <- NULL
set.seed(1)
for(change.param in names(params)){
  param.df <- params[[change.param]]
  these.segments <-
    data.frame(param.df, last=segment.last, first=segment.first,
               size=segment.last-segment.first+1,
               change.param, what="model")
  segments <- rbind(segments, these.segments)
  sig.vec <- NULL
  mean.vec <- NULL
  sd.vec <- NULL
  for(segment.i in 1:nrow(these.segments)){
    seg <- these.segments[segment.i,]
    mean.vec <- c(mean.vec, rep(seg$mean, seg$size))
    sd.vec <- c(sd.vec, rep(seg$sd, seg$size))
    base <- with(seg, first:last)
    signal <- rnorm(seg$size, seg$mean, seg$sd)
    sig.vec <- c(sig.vec, signal)
    this.signal <- data.frame(base, signal, segment.i, change.param,
                              what="data")
    signals <- rbind(signals, this.signal)
  }
  max.segments <- 7
  for(model.type in c("mean", "var")){
    fun.name <- paste0("cpt.", model.type)
    fun <- get(fun.name)
    model.ints <- c(mean=2L, var=4L)
    sfit <- Segmentor(sig.vec, model.ints[[model.type]], max.segments)
    fit <- fun(sig.vec, method="SegNeigh", Q=max.segments, class=FALSE)
    for(model.i in 1:max.segments){
      ## Check that I am interpreting the output of the changepoint
      ## package correctly. The change-points occur after the
      ## locations given in fit$cps.
      if(model.i == 2 && model.type == "mean"){
        change.indices <- 1:(length(sig.vec)-1)
        rss <- rep(NA, length(sig.vec))
        for(last.i in change.indices){
          est.mean <- rep(NA, length(sig.vec))
          for(indices in list(1:last.i, (last.i+1):length(sig.vec))){
            est.mean[indices] <- mean(sig.vec[indices])
          }
          residual <- est.mean-sig.vec
          rss[last.i] <- sum(residual * residual)
        }
        stopifnot(which.min(rss) == fit$cps[2,1])
        stopifnot(which.min(rss) == sfit@breaks[2,1])
      }
      point.mean <- point.sd <- rep(NA, length(sig.vec))
      seg.mean <- seg.sd <- rep(NA, model.i)
      if(model.i == 1){
        first.i <- 1
        last.i <- length(sig.vec)
      }else{
        change.i <- sort(fit$cps[model.i, 1:(model.i-1)])
        stopifnot(change.i == sfit@breaks[model.i, 1:(model.i-1)])
        first.i <- c(1, change.i+1)
        last.i <- c(change.i, length(sig.vec))
        model.changes <- rbind(model.changes, {
          data.frame(change.i, segments=model.i, model.type, change.param)
        })
      }
      for(seg.i in 1:model.i){
        seg.indices <- (first.i[seg.i]):(last.i[seg.i])
        if(model.type == "mean"){
          point.mean[seg.indices] <- seg.mean[seg.i] <-
            mean(sig.vec[seg.indices])
        }else{
          point.sd[seg.indices] <- seg.sd[seg.i] <- sd(sig.vec[seg.indices])
        }
      }
      if(model.type == "var"){
        seg.mean <- point.mean <- mean(sig.vec)
      }else{
        residual <- sig.vec-point.mean
        seg.sd <- point.sd <- sd(residual * residual)
      }
      model.segments <- rbind(model.segments, {
        data.frame(first.i, last.i, segments=model.i,
                   seg.sd, seg.mean,
                   model.type, change.param)
      })
    }
  }
}

sig.models <- 
ggplot()+
  geom_point(aes(base, signal), data=signals, pch=1)+
  geom_vline(aes(xintercept=change.i+1/2,
               color=model.type, linetype=model.type),
           data=model.changes, show_guide=TRUE)+
  scale_x_continuous("base position",
                     breaks=segment.first)+
  facet_grid(segments ~ change.param)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "cm"))
print(sig.models)

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
            data=segments, alpha=2/10, color=NA)+
  geom_segment(aes(first-1/2, mean, xend=last+1/2, yend=mean, color=what),
               data=segments)+
  facet_grid(change.param ~ .)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "cm"))

pdf("figure-motivation.pdf")
print(two.sigs)
dev.off()
