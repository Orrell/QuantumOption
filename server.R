require(ggplot2)
require(derivmkts)
#require(pracma)

getst <- function(nstep,rf,sdval) {
  ipos <- seq(0, nstep-1)
  st <- (nstep-1-ipos)*(-sdval/2) + ipos*(sdval/2) + nstep*log(1+rf) # log price vector
  return(exp(st))
}

getop <- function(nstep,rf,rfper,sdval,probvec,K) {
  st <- getst(nstep,rf,sdval) # price vector
  opvec <- st - K 
  opvec <- 1/2*(opvec + abs(opvec))
  op <- sum(probvec*opvec)  # if K=0, sum(probvec*st) not same as stmn
  op <- 1/(1+rfper)*op  # discount back to initial time
  return(op)
}

getstvar <- function(nstep,rf,sdval,probvec) {
  # variance is the square of the standard deviation
  st <- getst(nstep,rf,sdval)
  stmn <- sum(probvec*log(st))
  stvar <- sum(probvec*(log(st)-mean(log(st)))^2)
  return(list(stmn=stmn,stvar=Re(stvar)))
}


getsdq <- function(nstep,rf,sdc,probquant,probclass) {
  stqvar <- getstvar(nstep,rf,sdc,probquant)$stvar  # quantum variance using sdc
  stcvar <- getstvar(nstep,rf,sdc,probclass)$stvar  # classical variance using sdc
  sdq <- sdc*sqrt(stcvar/stqvar)  # scale step to give same variance as classical at final time
  return(sdq)
}

geterr <- function(nstep,rf,rfper,sdc,sdc0,probclass,K,price) {
  opclass <- getop(nstep,rf,rfper,sdc,probclass,K) 
  err <- (opclass - price)^2  # difference between option price and desired price
  err <- err + 0.1*(sdc - sdc0)^2 # penalty term for departure from target volatility
}

getvimp <- function(nstep,rf,rfper,sdc,sdq,probquant,probclass,Kvec) {
  nk <- length(Kvec)
  vimp <- rep(0,nk)
  for (k in 1: nk) {
    opquant <- getop(nstep,rf,rfper,sdq,probquant,Kvec[k])
    #f <- @(delba) getclass2(nstep,Kvec[k],probclass,rf,delba,opquant) # delba is b-a
    sdimp <- optim(par=sdc,fn=geterr,nstep=nstep,rf=rf,rfper=rfper,sdc0=sdc,probclass=probclass,K=Kvec[k],price=opquant,
                   method='Brent',lower=0,upper=10*sdc) # get sdimp that fits binomial to quantum target
    vimp[k] <- sdimp$par/sdc # implied vol factor, scale by volyr
  }
  return(vimp)
}

getvimpdata <- function(nstep,rf,rfper,sdc,kdatavec,probclass,opdatavec) {
  nk <- length(kdatavec)
  vimp <- rep(0,nk)
  for (k in 1: nk) {
    sdimp <- optim(par=sdc,fn=geterr,nstep=nstep,rf=rf,rfper=rfper,sdc0=sdc,probclass=probclass,K=kdatavec[k],price=opdatavec[k],
                   method='Brent',lower=0,upper=10*sdc) # get sdimp that fits binomial to quantum target
    vimp[k] <- sdimp$par/sdc # implied vol factor, scale by volyr
  }
  return(vimp)
}


# GOOG 22 weeks
# from https://finance.yahoo.com/quote/GOOG/options?date=1592524800

kgoog = c(0.9231, 0.9266, 0.9301, 0.9336, 0.9370, 0.9405, 0.9440, 0.9475, 0.9614, 0.9754, 0.9893, 1.0032, 
          1.0172, 1.0311, 1.0450, 1.0590, 1.0729, 1.0868, 1.1008, 1.1147, 1.1286, 1.1426, 1.1565, 1.1704, 
          1.1844, 1.1983, 1.2122, 1.2262, 1.2401, 1.2540, 1.2680, 1.2819, 1.2958, 1.3098, 1.3237)
pgoog = c(0.1051, 0.1007, 0.0947, 0.0995, 0.0945, 0.0954, 0.0930, 0.0928, 0.0803, 0.0759, 0.0637, 0.0591, 
          0.0514, 0.0426, 0.0391, 0.0327, 0.0278, 0.0222, 0.0199, 0.0167, 0.0118, 0.0113, 0.0086, 0.0075, 
          0.0064, 0.0052, 0.0039, 0.0035, 0.0012, 0.0023, 0.0017, 0.0010, 0.0013, 0.0009, 0.0010)
opposx = seq(0.6,1.4,by=0.05)  # open position
opposy = c(32,122,115,398,829,767,1574,1204,1043,306,636,554,48,187,116,0,0)
opposy = opposy/sum(opposy)
dfgoog = data.frame(kgoog=kgoog,pgoog=pgoog)
dfoppos = data.frame(opposx=opposx,opposy=opposy)

shinyServer(function(input, output, session) {
  
  getpars <- reactive({
    volyr <- input$volyr/100  # volatility is sqrt of variance
    nyr <- input$nweeks/52
    nt <- input$nmax-1
    sdc <- 2*volyr*sqrt(nyr/nt)  # gives correct final volatility
    # correct rf by -volyr^2/2 to shift stock
    rfbase <- (input$rfyr/100 + 1)^(nyr/nt) - 1 # risk-free per step
    rf <- (input$rfyr/100 - volyr^2/2 + 1)^(nyr/nt) - 1 # includes JR correction

    return(list(sdc=sdc, rf=rf, rfbase=rfbase))
  })
  
  getprobclass <- reactive({
    nmax <- input$nmax
    pmax <- 2*nmax+1
    vmat <- matrix(rep(0,len=nmax*pmax),nrow=nmax,ncol=pmax) # tracks position probability on grid for up particles
    probmat <- vmat
    icen <- nmax + 1
    vmat[1,icen] <- 1 
    probmat[1,icen] <- 1
    for (n in 2: nmax) {
      for (m in (1+1): (pmax-1)) {
        vmat[n,m] <- vmat[n-1,m-1]/2 + vmat[n-1,m+1]/2
      }
      probmat[n,] <- vmat[n,]
    }
    evencol <- seq(2,pmax,2)
    probfin <- probmat[nmax,evencol]
    probfin <- probfin/sum(probfin)
    #xpos = seq(-(nmax-1), (nmax-1), 2)
    return(probfin)
  })
  
  getprobquant <- reactive({
    nmax <- input$nmax
    pmax <- 2*nmax+1
    xmatup <- matrix(rep(0,len=nmax*pmax),nrow=nmax,ncol=pmax) # tracks position probability on grid for up particles
    xmatdown <- xmatup
    probmat <- xmatup
    xi <- 0
    theta <- pi/4
    si <- 0
    icen <- nmax + 1
    decoh <- input$decoh
    if (decoh > 0) {
      nens <- input$nens
    } else {
      nens <- 1
    }
    startangle <- input$startangle*pi/180
    xmatup[1,icen] <- sin(startangle) # gives balanced if startangle = 45
    xmatdown[1,icen] <- 1i*cos(startangle)
    withProgress(message = 'Running ensemble', value = 0, {
      for (k in 1: nens) {
        for (n in 2: nmax) {
          si <- decoh*rnorm(1) # Hadamard has si<-0, so perturb slightly
          Rp11 <- exp(1i*xi)*cos(theta)
          Rp12 <- exp(1i*si)*sin(theta) 
          Rp21 <- exp(-1i*si)*sin(theta) 
          Rp22 <- -exp(-1i*xi)*cos(theta)
          for (m in 2:(pmax-1)) {
            xmatup[n,m] <- Rp11*xmatup[n-1,m-1] + Rp12*xmatdown[n-1,m-1]
            xmatdown[n,m] <- Rp21*xmatup[n-1,m+1] + Rp22*xmatdown[n-1,m+1]
          } 
          probmat[n,] <- probmat[n,] + xmatup[n,]*Conj(xmatup[n,]) + xmatdown[n,]*Conj(xmatdown[n,])
        }
        incProgress(1/nmax)
      
    }
    })
    probmat <- Re(probmat)/nens
    evencol <- seq(2,pmax,2)
    probfin <- probmat[nmax,evencol]
    probfin <- probfin/sum(probfin)
    return(probfin)
  })
  
getOptionPrices <- reactive({
  probquant <- getprobquant()
  probclass <- getprobclass()
  nmax <- input$nmax
  strlim <- input$rangeAxisOption
  Kvec <- seq(strlim[1], strlim[2], 0.01)  # strike price
  nstr <- length(Kvec)
  opquant <- rep(0,nstr)
  opclass <- rep(0,nstr)
  opbinom <- rep(0,nstr)
  opblsch <- rep(0,nstr)
  pars <- getpars()
  sdc <- pars$sdc  # step size for classical model
  rf <- pars$rf
  tyr <- input$nweeks/52
  rfper <- (1 + input$rfyr/100)^tyr - 1
  sdq <- getsdq(input$nmax,rf,sdc,probquant,probclass)
  
  volbinom <- input$volyr/100 #*1.05 #exp(input$volyr/100)-1  # for use in binomial function
  rfbinom <- input$rfyr/100  # for use in binomial function
  
  for (k in 1: nstr) {
    opquant[k] <- getop(nmax,rf,rfper,sdq,probquant,Kvec[k])
    opclass[k] <- getop(nmax,rf,rfper,sdc,probclass,Kvec[k])
    if (input$blschplot) {
      opblsch[k] <- bscall(1, Kvec[k], volbinom, rfbinom, input$nweeks/52, d=0)
    }
    # opbinom[k] <- binomopt(1, Kvec[k], volbinom, rfbinom, input$nweeks/52,
    #                        d=0, nmax-1, american = FALSE, putopt=FALSE, jarrowrudd=TRUE)
  }
  delprice <- (opquant - opclass) #/(opclass + 0.05)  # proportional
  #delpos <- pmax(delprice, 0)  # positive only
  #stdelprice <- sqrt(sum(delpos*(Kvec-1)^2)) # assumes mean is 1
  #yopen <- dnorm(Kvec,mean=0.995,sd=0.057)*.0012 # open position normal distn from paper
  
  df <- data.frame(Kvec=Kvec, opquant=opquant, opclass=opclass, opbinom=opbinom, opblsch=opblsch, delprice=delprice) #,yopen=yopen)
  return(df)
})
  
 output$stockPlot <- renderPlot({
    probquant <- getprobquant()
    probclass <- getprobclass()
    strlim <- input$rangeAxisStock
    nmax <- input$nmax
    xpos = seq(-(nmax-1), (nmax-1), 2)
    pars <- getpars()
    sdc <- pars$sdc
    rf <- pars$rf
    rfbase <- pars$rfbase
    st <- getst(nmax,rf,sdc)  # includes JR offset
    sdq <- getsdq(nmax,rf,sdc,probquant,probclass)
    stq <- getst(nmax,rf,sdq)
    # if (input$showJR == FALSE) {
    #   st <- getst(nmax,rfbase,sdc)  # display without correction as default
    #   stq <- getst(nmax,rfbase,sdq)  # display without correction as default
    # }
    pricebinom <- 0*xpos
    probbinom <- 0*xpos
   
    if (input$showJR) {
      volbinom <- input$volyr/100 #*1.05 #exp(input$volyr/100)-1  # for use in binomial function
      rfbinom <- input$rfyr/100  # for use in binomial function
      opbinom0 <- binomopt(1, 0, volbinom, rfbinom, input$nweeks/52,
                           d=0, nmax-1, american = FALSE, putopt=FALSE, jarrowrudd=TRUE,returntrees=TRUE)
      pricebinom <- log(opbinom0$stree[,nmax])
      delpr = pricebinom[1] - pricebinom[2]
      probbinom <- opbinom0$probtree[,nmax]/delpr
    }
    
    g <- ggplot() + theme_bw(base_size = 18)
    if (input$showwalk) {
      df <- data.frame(xpos=xpos,logst=log(st),logstq=log(stq),probclass=probclass,probquant=probquant,pricebinom=pricebinom,probbinom=probbinom)
      g <- g + geom_line(data=df,aes_string(x='xpos',y='probclass'),size=1,alpha=0.8,color='black',linetype=2)
      g <- g + geom_line(data=df,aes_string(x='xpos',y='probquant'),size=1,alpha=0.8,color='black')
      g <- g + labs(x="position index", y="prob density") + coord_cartesian(xlim = c(-nmax,nmax))
    } else {
      df <- data.frame(xpos=xpos,logst=log(st),logstq=log(stq),probclass=probclass/sdc,probquant=probquant/sdq)
      g <- g + geom_line(data=df,aes_string(x='logst',y='probclass'),size=1,alpha=0.8,color='black',linetype=2)
      g <- g + geom_line(data=df,aes_string(x='logstq',y='probquant'),size=1,alpha=0.8,color='black')
      if (input$showJR) {
        #g <- g + geom_line(data=df,aes_string(x='pricebinom',y='probbinom'),size=1,alpha=0.8,color='black',linetype=2)
        g <- g + geom_point(data=df,aes_string(x='pricebinom',y='probbinom'),size=3,shape=1)
      }
      g <- g + labs(x="log stock price", y="prob density") + coord_cartesian(xlim = strlim)
    }
    print(g)
  })
 
 output$stepsize <- renderText({
   probquant <- getprobquant()
   probclass <- getprobclass()
   nmax <- input$nmax
   pars <- getpars()
   sdc <- pars$sdc
   rf <- pars$rf
   sdq <- getsdq(nmax,rf,sdc,probquant,probclass)
   sdcfor = format(sdc,digits=3)
   sdqfor = format(sdq,digits=3)
   paste('class:',sdcfor,'  quant:',sdqfor)
 })
 
 output$stockmn <- renderText({
   probquant <- getprobquant()
   probclass <- getprobclass()
   nmax <- input$nmax
   pars <- getpars()
   sdc <- pars$sdc
   rf <- pars$rf
   st <- getst(nmax,rf,sdc)  # includes JR offset
   sdq <- getsdq(nmax,rf,sdc,probquant,probclass)
   stq <- getst(nmax,rf,sdq)
   stcmn <- sum(probclass*(st))
   stqmn <- sum(probquant*(stq))
   # classical and quantum means are initial stock price S=1 if r=0
   stcmnfor = format(stcmn,digits=3)
   stqmnfor = format(stqmn,digits=3)
   paste('class:',stcmnfor,'  quant:',stqmnfor)
 })
 
 output$posrange <- renderTable({
   df <- getOptionPrices()
   ind1 <- min(which(df$delprice > 0))
   ind2 <- max(which(df$delprice > 0))
   kpos1 <- df$Kvec[ind1]
   kpos2 <- df$Kvec[ind2]
   kposmn <- (kpos2 + kpos1)/2
   kpos <- df$Kvec[ind1:ind2]
   delpos <- df$delprice[ind1:ind2]
   delpos <- delpos/sum(delpos)
   kpossd <- sqrt(sum(delpos*(kpos - kposmn)^2))
   sumvar <- c('min','max','mean','st dev')
   sumval <- as.numeric(c(kpos1,kpos2,kposmn,kpossd))
   tablesummary <- data.frame(quantity=sumvar,value=sumval)
   tablesummary <- format(tablesummary,digits=3,width=8,format='f')
   
   #paste('min:',kpos1,'max:',kpos2,'mean',kposmn,'st dev:',kpossd)
 },include.rownames=FALSE,include.colnames=FALSE)
 
 output$optionPlot <- renderPlot({
   df <- getOptionPrices()
   g <- ggplot() + theme_bw(base_size = 18)
   g <- g + geom_line(data=df,aes_string(x='Kvec',y='opclass'),size=1,alpha=0.8,color='black',linetype=2)
   g <- g + geom_line(data=df,aes_string(x='Kvec',y='opquant'),size=1,alpha=0.8,color='black')
   if (input$blschplot) {
     g <- g + geom_point(data=df,aes_string(x='Kvec',y='opblsch'),size=3,shape=1)
     #g <- g + geom_line(data=df,aes_string(x='Kvec',y='opblsch'),size=1.5,alpha=0.8,color='green',linetype=2)
   }
   # g <- g + geom_line(data=df,aes_string(x='Kvec',y='opbinom'),size=1,alpha=0.8,color='black',linetype=3)
   if (input$optiondata) {
     g <- g + geom_point(data=dfgoog,aes_string(x='kgoog',y='pgoog'),shape=2,size=2,alpha=0.8,color='black')
   }
   g <- g + labs(x="strike price", y="option price")
   print(g)
 })

 output$compPlot <- renderPlot({
   df <- getOptionPrices()
   strlim <- input$rangeAxisOption
   g <- ggplot() + theme_bw(base_size = 18)
   #g <- g + geom_bar(data=dfoppos,aes_string(x='opposx',y='opposy'),stat="identity",alpha=0.5)
   #g <- g + geom_bar(data=df,aes_string(x='Kvec',y='yopen'),stat="identity",alpha=0.5)
   g <- g + geom_line(data=df,aes_string(x='Kvec',y='delprice'),size=1,alpha=0.8,color='black')
   g <- g + labs(x="strike price", y="price difference") # + coord_cartesian(xlim = strlim)
   g <- g + labs(x="strike price", y="delta price") + coord_cartesian(xlim = strlim)
   print(g)
 })
 
 output$vimpPlot <- renderPlot({
   probquant <- getprobquant()
   probclass <- getprobclass()
   strlim <- input$rangeAxisVimp
   Kvec <- seq(strlim[1], strlim[2], 0.01)  # strike price
   nstr <- length(Kvec)
   pars <- getpars()
   sdc <- pars$sdc
   rf <- pars$rf
   tyr <- input$nweeks/52
   rfper <- (1 + input$rfyr/100)^tyr - 1
   sdq <- getsdq(input$nmax,rf,sdc,probquant,probclass)
   vimp <- getvimp(input$nmax,rf,rfper,sdc,sdq,probquant,probclass,Kvec)*input$volyr
  
   df <- data.frame(strike=Kvec, vimp=vimp)
   g <- ggplot() + theme_bw(base_size = 18)
   g <- g + geom_line(data=df,aes_string(x='Kvec',y='vimp'),size=1,alpha=0.8,color='black')
   if (input$vimpdata) {
     vimpdata <- getvimpdata(input$nmax,rf,rfper,sdc,kgoog,probclass,pgoog)*input$volyr
     dfvimp = data.frame(kgoog=kgoog,vimpgoog=vimpdata)
     g <- g + geom_point(data=dfvimp,aes_string(x='kgoog',y='vimpgoog'),shape=1,size=2,alpha=0.8,color='black')
   }
   g <- g + labs(x="strike price", y="implied volatility") # + coord_cartesian(xlim = input$rangeAxis)
   print(g)
 })
 
 output$resultsTable <- DT::renderDataTable(DT::datatable({
   probquant <- getprobquant()
   probclass <- getprobclass()
   nmax <- input$nmax
   strlim <- input$rangeAxisOption
   Kvec <- seq(strlim[1], strlim[2], 0.01)  # strike price
   nstr <- length(Kvec)
   opquant <- rep(0,nstr)
   opclass <- rep(0,nstr)
   opbinom <- rep(0,nstr)
   opblsch <- rep(0,nstr)
   pars <- getpars()
   sdc <- pars$sdc
   rf <- pars$rf
   tyr <- input$nweeks/52
   rfper <- (1 + input$rfyr/100)^tyr - 1
   sdq <- getsdq(input$nmax,rf,sdc,probquant,probclass)
   
   volbinom <- input$volyr/100 #exp(input$volyr/100)-1  # for use in binomial function
   rfbinom <- input$rfyr/100  # for use in binomial function
   for (k in 1: nstr) {
     opquant[k] <- getop(nmax,rf,rfper,sdq,probquant,Kvec[k])
     opclass[k] <- getop(nmax,rf,rfper,sdc,probclass,Kvec[k])
     # opbinom[k] <- binomopt(1, Kvec[k], volbinom, rfbinom, input$nweeks/52, 
     #                        d=0, nmax-1, american = FALSE, putopt=FALSE, crr=TRUE, jarrowrudd=FALSE)
     opblsch[k] <- bscall(1, Kvec[k], volbinom, rfbinom, input$nweeks/52, d=0)
   }
   df <- data.frame(strike=Kvec, quantum=opquant, classical=opclass, BlackScholes=opblsch)
 }))
 
 output$saveimage1 <- downloadHandler(
   filename = function() { 
     'stockplot.pdf'
   },
   content = function(file) {
     ggsave(file, device='pdf', dpi=600, width=20, height=15, units="cm")
   }
 )
 
 output$saveimage2 <- downloadHandler(
   filename = function() { 
     'optionplot.pdf'
   },
   content = function(file) {
     ggsave(file, device='pdf', dpi=600, width=20, height=15, units="cm")
   }
 )
 
 output$saveimage3 <- downloadHandler(
   filename = function() { 
     'vimpplot.pdf'
   },
   content = function(file) {
     ggsave(file, device='pdf', dpi=600, width=20, height=15, units="cm")
   }
 )

 })
