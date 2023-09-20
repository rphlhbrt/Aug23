CustomAxis<-function(xtix=log10(c(1,2,4,10,20,40,100,200,400,1000,2000,4000)),
                     ytix=log10(c(0.01,0.1,1,10,100)),
                     ylims=NULL,xlims=NULL,xlab=NULL,ylab=NULL,
                     xlog10=TRUE,new=TRUE,ylog10=TRUE,col.grid="gray33",labcex=1,
                     cex=1,xcol='black',ycol='black',k=FALSE,tickcex=0.75,
                     xadj=0.9,yadj=0.75,xline=2.,yline=2,clips=NULL,freq=FALSE,notix=FALSE,invert=FALSE){
  Swap<-function(a,b,env=.GlobalEnv){
    ba<-a
    bb<-b
    vara<-deparse(substitute(a))
    varb<-deparse(substitute(b))
    assign(x = varb,value = ba,pos = env)
    assign(x = vara,value = bb,pos = env)
  }
  if(invert){
    this.env<-environment()
    Swap(xtix,ytix,env = this.env)
    Swap(xlims,ylims,env = this.env)
    Swap(xlog10,ylog10,env = this.env)
    Swap(xlab,ylab,env = this.env)
    Swap(xadj,yadj,env = this.env)
    Swap(xline,yline,env = this.env)
  }
  if(is.null(ylims)) ylims<-c(min(ytix),max(ytix))
  if(is.null(xlims)) xlims<-c(min(xtix),max(xtix))
  if(freq) xlims<-(-xlims)
  if(new) plot(xlims,ylims,col=adjustcolor('white',0),axes=FALSE,xlab='',ylab='')
  if(is.null(xlab)) xlab<-"Timescale (years)"
  if(is.null(ylab)) ylab<-expression(PSD~(K^2~years))
  if(is.null(ytix)){ytix<-xtix}
  xtext<-if(xlog10){10^xtix}else{xtix}
  ytext<-if(ylog10){10^ytix}else{ytix}
  if(freq) xtix<-(-xtix)
  if(!notix){axis(1,at=xtix,labels=FALSE,col=xcol)}else{axis(1,at=range(xtix),labels=FALSE,col=xcol)}
  if(k) xtext[which(xtext>=1000)]<-paste(xtext[which(xtext>=1000)]/1000,"k",sep = " ")
  if(!notix) mtext(side=1,line=0.75*cex,at=xtix,text = xtext,cex=tickcex*cex,col=xcol)
  mtext(side=1,line=xline*cex,text=xlab,cex = cex*labcex,adj=xadj,col=xcol)

  if(!notix){axis(2,at=ytix,labels=FALSE,col=ycol)}else{axis(2,at=range(ytix),labels=FALSE,col=ycol)}
  if(!notix) mtext(side=2,line=0.75*cex,at=ytix,text = ytext,cex=tickcex*cex,col=ycol)
  mtext(side=2,line=yline*cex,text= ylab,cex = cex*labcex,adj=yadj,col=ycol)
  if(!is.null(clips)) clip(clips[1],clips[2],clips[3],clips[4])
  if(is.logical(col.grid)){if(col.grid) abline(h=ytix, v=xtix, col='gray33', lty=3)}else{
    abline(h=ytix, v=xtix, col=col.grid, lty=3)
  }
  if(!is.null(clips)) clip(xlims[1]-5,xlims[2]+5,ylims[1]-5,ylims[2]+5)
}

AddSecondAxis<-function(tix,a,b,label,side=4,cex=1,col='brown',log10=FALSE,lineoff=0,adjlab=0.75,tickcex=0.75){
  text<-if(log10){10^tix}else{tix}
  axis(side,at=tix*a+b,labels=FALSE,col=col,line = lineoff)
  mtext(side=side,line=0.75*cex+lineoff,at=a*(tix)+b,text = text,cex=tickcex*cex,col=col)
  mtext(side=side,line=2*cex+lineoff,text = label,cex=cex,col=col,adj=adjlab)
}

