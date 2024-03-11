#Manager/patch properties
#c(p0, L, a


t.fix <- function(db, v=0.95) {
 
 log(1-v)/log(1-db)
 
}

#L.func <- function(L, p0, a) {
#pmin(exp(-a / (L-a/log(p0))), 1)
#}

L.func <- function(L, p0, a) { #l.func updated such that with increasing a, PL increases (i.e. multiplied by a)
 pmin(a * exp(-a / (L - a/log(p0))), 1)
}

Pc.func <- function(dc=0.8, t=t.fix(dc)) {
 
 1-(1-dc)^t
 
}

Pd.func <- function(db=0.8, v=0.95, t=t.fix(db,v), L, p0, a) {
 L.func(L, p0, a) * (1 - (1-db)^t)
}

I.func <- function(L, p0, x, Cl=100, D=Cl*x, Cm=Cl/10, db=0.8, v=0.95, t=t.fix(db, v), a=1) {
 Pd.func(db, v=v, t, L, p0, a) * D - Cl*L - Cm
}

E.func.out <- function(x, L, p0, a, Cl=100, Ch=10, db=0.8, v=0.95, t=t.fix(db,v)) {
 
 x*Pd.func(db=db, v=v, t=t, L=L, p0=p0, a=a)*Cl + (t+1)*Ch
 
}

#by action
I.act <- function(x=11/10, Cl=100, Cm=Cl/10) {x*Cl - Cl - Cm}
E.act <- function(dc=0.95, x=11/10, Cl=100, Ch=10, v=0.95, Pv = 0.6) {
 
 x*Cl+Pv*(t.fix(dc,v)+1)*Ch
 
}

B.act <- function(x=11/10, p0, a) {
 
 ifelse(x>11/10, L.func(1, p0, a), p0) # why x >11/10?
 
}

G.act <- function(x=11/10, p0, a) { #why x = 11/10? What is x?  
 
 B.act(x, p0, a) - p0 
 
}

#by outcome with L=1
I.out.L1 <- function(p0, a, L=1) {
 
 22/(19 * L.func(L=L, p0=p0, a=a))
 
}

E.out.L1 <- function(db, v=0.95, Cl=100, Ch=10) {
 11/10*Cl+(t.fix(db, v)+1)*Ch
}


#by outcome with minimum value of x for positive value of Lopt
x.L0.pos <- function(P0, a) {
 20*a/(19*P0*log(P0)^2)
}


E.L0.pos <- function(P0, a, db) {
 
 E.func.out(x=x.L0.pos(P0,a), L=0, p0=P0, a=a, db=db)
 
}

min.a <- function(P0, Cm=10, Cl=100) {
 Cm*log(P0)^2/Cl
 
}

#by outcome when Lopt > 1, doesn't seem to produce sensible results?
x.L1.pos <- function(P0, a) {
 (20*P0^(a/(log(P0)-a))*(log(P0)-a)^2)/(19*a*log(P0)^2)
}


#optimal values of L
Lopt <- function(p0, a, x.mult, max.L=100, v=0.95) {
 
 opt.func <- function(L) {
  -I.func(L=L, p0=p0, x=x.mult, a=a, v=v)
 }
 optimize(f=opt.func, interval=c(0, max.L))
 
}

batch.opt <- function(a=c(0.1, 0.25, 0.5, 1.0, 1.5), p0=seq(0.01, 0.99, by=0.01), 
                      x.mult=seq(1.0, 50, by=0.1), db=0.95, v=0.95) {
 
 #Lopt.batch <- function(params) {
 #	unlist(Lopt(p0=params[2], a=params[1], x.mult=params[3]))
 #}
 
 out <- expand.grid(a=a, p0=p0, x=x.mult, db=db)
 out$L <- NA
 out$I <- NA
 out$E <- NA
 out$B <- NA
 out$G <- NA
 
 for(i in 1:nrow(out)) {
  out[i, 5:6] <- unlist(Lopt(p0=out[i, 2], a=out[i, 1], x.mult=out[i, 3], v=v))
 }
 out$I <- - out$I
 out$E <- E.func.out(x=out[ ,3], L=out[ ,5], p0=out[, 2], a=out[, 1], db=out[, 4], v=v)
 out$B <- L.func(L=out[ ,5], p0=out[, 2], a=out[, 1])
 out$G <- out$B-out$p0
 out$R <- out$G/out$E
 
 out
 
}



#set-up patch distributions
set.probs <- function(p0, dist, n.man, w.sample=FALSE) {
 
 p0.probs <- dbeta(p0, dist[1], dist[2]) / sum(dbeta(p0, dist[1], dist[2]))
 ex.man <- p0.probs*n.man
 if(w.sample) {
  tmp.samp <- sample(1:length(p0), size=n.man, replace=TRUE, prob=p0.probs)
  ex.man <- table(tmp.samp)
  p0.probs <- ex.man/sum(ex.man)
 }
 list(p0.probs=p0.probs, ex.man=ex.man)
}

probs.mean <- function(p0=seq(0,1,by=0.01), dist) {
 sum(p0*dbeta(p0, dist[1], dist[2]) / sum(dbeta(p0, dist[1], dist[2])))
 
}

fpc <- function(dist.a=c(1,3,5,7,9, seq(10,50, by=10)), 
                dist.b=dist.a, prop=seq(0.01,1,by=0.01), n=1000, simp.dist=TRUE) {
 
 fpc.calc <- function(x) {
  beta.var <- x[1]*x[2]/((x[1]+x[2])^2*(x[1]+x[2]+1))
  beta.var*(n-x[3]*n)/n
 }
 
 out <- expand.grid(a=dist.a, b=dist.b, prop=prop)
 if(simp.dist) out <- out[out$a==1 | out$b==1, ]
 out$var <- apply(out, 1, fpc.calc)
 out$mean <- out$a/(out$a+out$b)
 out
}

#single patch by action
batch.act <- function(a=c(0.1, 0.25, 0.5, 1.0, 1.5), p0=seq(0.01, 0.99, by=0.01), 
                      x.mult=seq(1.0, 50, by=0.1), dc=0.95, v=0.95) {
 
 out <- expand.grid(a=a, p0=p0, x=x.mult, dc=dc)
 out$L <- NA #need to calculate Income first
 out$I <- ifelse(I.act(x=out[, 3])<0, 0, I.act(x=out[, 3]))
 out$L <- ifelse(out$I==0, 0, 1)
 out$E <- ifelse(out$I==0, 0, E.act(dc=out[, 4], x=out[, 3], v=v))
 out$B <- B.act(x=out[,3], a=out[,1], p0=out[,2])
 out$G <- G.act(x=out[,3], a=out[,1], p0=out[,2])
 out$R <- out$G/out$E
 
 out
}

#######
multi.patch <- function(s.data=single.patch[single.patch$db==0.95, ],
                        dist=c(1,1), method=c("random","select","randbudg"), budget=100000, n.man=1000, 
                        p0=seq(0.01, 0.99, by=0.01), a=c(0.1, 0.25, 0.5, 1.0, 1.5),
                        x.mult=seq(1.0, 50, by=0.1), p.in=0.95, p.out=p.in, Ch=10, gain=TRUE, w.sample=NULL) {
 
 
 #select patches function
 select.patch <- function(x) {
  tmp <- x[order(x$R, decreasing=TRUE, na.last=NA), ]
  tmp$total <- tmp$E * tmp$ex.man
  tmp$cum <- cumsum(tmp$total)
  tmp.row <- nrow(tmp)
  if(tmp$cum[tmp.row] > budget) {
   tmp <- rbind(tmp[tmp$cum<budget, ], tmp[tmp$cum>budget, ][1,]) #all the managers in budget plus the boundary
   tmp.row <- nrow(tmp)
   tmp.ex <- (tmp$total[tmp.row]-(tmp$cum[tmp.row]-budget))/tmp$total[tmp.row] #fraction to add up to total budget
   tmp$ex.man[tmp.row] <- tmp$ex.man[tmp.row]*tmp.ex
   tmp$total <- tmp$E * tmp$ex.man #update totals
  }
  cbind(tmp[1, c(1,3)], t(colSums(tmp[,5:10]*tmp$ex.man/n.man)), t(colSums(tmp[,11:12]))) 
 }
 #random patches with budget function
 random.budget <- function(x) {
  tmp <- (x[5]*n.man)/budget #x[5] is E
  tmp.names <- names(x)
  if(tmp>1) {
   tmp.prop <- 1/tmp
  }
  else {
   tmp.prop <- 1
  }
  if(tmp > 1) {
   x[7] <- x[7]/tmp #x[7] is G
   x[5] <- budget/n.man
  }
  x <- c(x, tmp.prop)
  names(x) <- c(tmp.names, "prop")
  x
 }
 
 n.a <- length(a)
 
 #remove managers with negative income from scheme membership
 if(min(s.data$I) <0) s.data[s.data$I<0, 5:10] <- cbind(0,0,0,s.data[s.data$I<0, 2] ,0, 0)
 
 if(!gain) {	#maximise by benefit rather than gain
  s.data$R <- s.data$B/s.data$E
 }
 
 #remove any NaNs from R i.e when G and E are 0
 s.data[is.nan(s.data$R), 10] <- 0
 
 #adjust E for dc, db value
 t.in <- t.fix(p.in)
 t.out <- t.fix(p.out)
 s.data$E <- s.data$E - t.in*Ch + t.out*Ch
 
 if(is.null(w.sample)) {
  #set-up patch distributions
  probs <- set.probs(p0, dist, n.man)
  
  #out <- expand.grid(a=a, x=x.mult)
  if(method=="random") {
   out <- aggregate(s.data[, 5:10], s.data[ , c(1,3)], FUN=function(x) {sum(x * probs$p0.probs)})
   out$prop <- 1
  }
  else if(method=="randbudg") {
   out <- aggregate(s.data[, 5:10], s.data[ , c(1,3)], FUN=function(x) {sum(x * probs$p0.probs)})
   out <- as.data.frame(t(apply(out,1, random.budget)))
  }
  else {
   s.data <- cbind(s.data, ex.man=rep(probs$ex.man, each=n.a))
   s.data <- by(s.data, s.data[, c(1,3)], FUN=select.patch)
   out <- do.call("rbind", s.data)
   out$prop <- out$ex.man/n.man		
  }
 }
 else {
  out <- vector(mode="list", w.sample)
  
  for(i in 1:w.sample) {
   probs <- set.probs(p0, dist, n.man, w.sample=TRUE)
   
   if(method=="random") {
    out[[i]] <- aggregate(s.data[, 5:10], s.data[ , c(1,3)], FUN=function(x) {sum(x * probs$p0.probs)})
    out[[i]]$prop <- 1
   }
   else if(method=="randbudg") {
    out[[i]] <- aggregate(s.data[, 5:10], s.data[ , c(1,3)], FUN=function(x) {sum(x * probs$p0.probs)})
    out[[i]] <- as.data.frame(t(apply(out[[i]],1, random.budget)))
   }
   else {
    s.data <- cbind(s.data, ex.man=rep(probs$ex.man, each=n.a))
    s.data <- by(s.data, s.data[, c(1,3)], FUN=select.patch)
    out[[i]] <- do.call("rbind", s.data)
    out[[i]]$prop <- out[[i]]$ex.man/n.man		
   }
  }
 }
 out
 
}

multi.patch.opt <- function(out, act, budget=c(1000, 500000), test.a=0.1, dc=0.95, db=0.95,  ...) {
 
 max.diff.func <- function(x) {
  (max(multi.patch(s.data=out[out$a==test.a, ], budget=x, n.a=1, p.out=db,...)$G)-
    max(multi.patch(s.data=act[act$a==test.a, ], budget=x, n.a=1, p.out=dc, ...)$G))^2
 }
 
 optimize(f=max.diff.func, interval=budget)
 
 
}

multi.patch.batch <- function(s.data, dist.a=c(1,3,5,7,9, seq(10,50, by=10)), 
                              dist.b=dist.a, simp.dist=FALSE, gain=TRUE,
                              method=c("select","randbudg"), budget=c(seq(1000,9000,by=1000),
                                                                      seq(10000, 100000, by=10000),seq(200000, 1000000, by=100000)), p.out=seq(0.1,1.0,by=0.1),
                              a=c(0.1, 0.25, 0.5, 1.0, 1.5), ...) {
 
 
 max.func <- function(x) {
  if(gain) {
   tmp.max <- which.max(x$G)
  }
  else {
   tmp.max <- which.max(x$B)
  }
  x[tmp.max, c("G", "x", "prop")]
 }
 
 n.a <- length(a)
 out <- vector(mode="list", length=n.a+1)
 names(out) <- c(paste("a", a, sep="_"), "params")
 out.a <- expand.grid(budget=budget, beta.a=dist.a, beta.b=dist.b, d=p.out)
 if(simp.dist) {
  out.a <- out.a[out.a$beta.a==1 | out.a$beta.b==1, ]
 }
 n.runs <- nrow(out.a)
 out.b <- data.frame(max.G=rep(NA,n.runs), max.x=rep(NA,n.runs), prop=rep(NA, n.runs))
 for(i in 1:5) {
  out[[i]] <- out.b
 }
 out[[n.a+1]] <- out.a
 
 for(i in 1:n.runs) {
  
  tmp <- multi.patch(s.data=s.data, dist=c(out.a$beta.a[i], out.a$beta.b[i]),
                     method=method, budget=out.a$budget[i], p.out=out.a$d[i], gain=gain, ...)
  for(j in 1:n.a) {
   out[[j]][i,] <- max.func(tmp[tmp$a==a[j], ])				
  }
 }
 
 out
 
}
#now we make our changes assuming a robot is used
#this mainly concerns the expenditure functions
#we swe assume varying ranges for CL, Pv and Ch

E.func.out_robot <- function(x, L, p0, a, Cl=Cl, Ch=Ch, db=db, v=0.95, t=t.fix(db,v)) {#take pv out/ set it to 1
 
 x*Pd.func(db=db, v=v, t=t, L=L, p0=p0, a=a)*Cl + (t+1)*Ch
 
}


E.act_robot <- function(dc = dc, x=11/10, Cl=Cl, Ch=Ch, v=0.95, Pv = Pv) {#set pv to 1
 
 x*Cl+Pv*(t.fix(dc,v)+1)*Ch
 
}


#include new expenditure functions in batch functions
batch.act_robot <- function(a=a, p0=p0, 
                            x.mult=x.mult, dc = dc, v=0.95, Cl = Cl, Ch = Ch, Pv = Pv) {
 
 out <- expand.grid(a=a, p0=p0, x=x.mult, dc=dc, Cl = Cl, Ch = Ch, Pv = Pv)
 out$L <- NA #need to calculate Income first
 out$I <- ifelse(I.act(x=out[, 3])<0, 0, I.act(x=out[, 3]))
 out$L <- ifelse(out$I==0, 0, 1)
 out$E <- ifelse(out$I==0, 0, E.act_robot(dc=out[, 4], x=out[, 3], v=v, Cl = out[,5], Ch = out[,6], Pv = out[,7]))
 out$B <- B.act(x=out[,3], a=out[,1], p0=out[,2])
 out$G <- G.act(x=out[,3], a=out[,1], p0=out[,2])
 out$R <- out$G/out$E
 out$Cl <- out$Cl
 out$Ch <- out$Ch
 out$Pv <- out$Pv
 
 
 out
}



batch.opt_robot <- function(a=a, p0=p0, 
                            x.mult=x.mult, db=db, v=0.95, Cl = Cl, Ch = Ch) {
 
 #Lopt.batch <- function(params) {
 #	unlist(Lopt(p0=params[2], a=params[1], x.mult=params[3]))
 #}
 
 out <- expand.grid(a=a, p0=p0, x=x.mult, db=db, Cl = Cl, Ch = Ch)
 out$L <- NA
 out$I <- NA
 out$E <- NA
 out$B <- NA
 out$G <- NA
 
 
 for(i in 1:nrow(out)) {
  out[i, 7:8] <- unlist(Lopt(p0=out[i, 2], a=out[i, 1], x.mult=out[i, 3], v=v))
 }
 out$I <- - out$I
 out$E <- E.func.out_robot(x=out[ ,3], L=out[ ,7], p0=out[, 2], a=out[, 1], db=out[, 4], v=v, Cl=out[,5], Ch = out[,6])
 out$B <- L.func(L=out[ ,7], p0=out[, 2], a=out[, 1])
 out$G <- out$B-out$p0
 out$R <- out$G/out$E
 out$Cl <- out$Cl
 out$Ch <- out$Ch
 
 out
 
}


#now create results with robot

test.batch.act_AM_robot_Lfunc <- batch.act_robot(dc = seq(0.1,0.9, by = 0.2),
                                                 a=c(0.667, 1, 10), 
                                                 p0=seq(0.1, 0.9, by=0.1), 
                                                 x.mult=seq(5, 50, by=5), 
                                                 Cl = seq(25, 200, by=25),
                                                 Ch = seq(2, 20, by=2), 
                                                 Pv = seq(0.2, 1, by = 0.2))


test.batch.opt_AM_robot_Lfunc_0.667 <- batch.opt_robot(db = seq(0.1,0.9, by = 0.2),
                                                       a=0.667,
                                                       p0=seq(0.1, 0.9, by=0.1), 
                                                       x.mult=seq(5, 50, by=5), 
                                                       Cl = seq(25, 200, by=25),
                                                       Ch = seq(2, 20, by=2))

test.batch.opt_AM_robot_Lfunc_1 <- batch.opt_robot(db = seq(0.1,0.9, by = 0.2),
                                                   a=1,
                                                   p0=seq(0.1, 0.9, by=0.1), 
                                                   x.mult=seq(5, 50, by=5), 
                                                   Cl = seq(25, 200, by=25),
                                                   Ch = seq(2, 20, by=2))

test.batch.opt_AM_robot_Lfunc_10 <- batch.opt_robot(db = seq(0.1,0.9, by = 0.2),
                                                    a=10,
                                                    p0=seq(0.1, 0.9, by=0.1), 
                                                    x.mult=seq(5, 50, by=5), 
                                                    Cl = seq(25, 200, by=25),
                                                    Ch = seq(2, 20, by=2))

#out of curiosity run test.batch with range of values for a
test.batch.act_AM_robot_Lfunc_rangea2 <- batch.act_robot(dc = seq(0.1,0.9, by = 0.2),
                                                         a=seq((1/3),10, by = (1/3)), 
                                                         p0=seq(0.1, 0.9, by=0.1), 
                                                         x.mult=seq(5, 50, by=5), 
                                                         Cl = seq(25, 200, by=25),
                                                         Ch = seq(2, 20, by=2), 
                                                         Pv = seq(0.2, 1, by = 0.2))

#takes long therefore comment out if not needed
test.batch.opt_AM_robot_Lfunc_rangea <- batch.opt_robot(db = seq(0.1,0.9, by = 0.2),
                                                        a=seq((1/3),10, by = (1/3)),
                                                        p0=seq(0.1, 0.9, by=0.1), 
                                                        x.mult=seq(5, 50, by=5), 
                                                        Cl = seq(25, 200, by=25),
                                                        Ch = seq(2, 20, by=2))


#save old versions with old l.func
#test.batch.act_AM_robot_oldlfunc <- test.batch.act_AM
#test.batch.opt_AM_robot_oldlfunc <- test.batch.opt_AM

#call versions with new lfunc such that we can easily work with them
test.batch.act_AM_robot<- test.batch.act_AM_robot_Lfunc_rangea2
#test.batch.opt_AM_robot<- test.batch.opt_AM_robot_Lfunc_rangea


#bind three version together
test.batch.opt_AM_robot_old <- rbind(test.batch.opt_AM_robot_Lfunc_0.667,test.batch.opt_AM_robot_Lfunc_1,test.batch.opt_AM_robot_Lfunc_10)
test.batch.opt_AM_robot<-test.batch.opt_AM_robot_Lfunc_rangea

#store test.batch_opt_Am_robot once to save it as it need quite some time to run
COPYtest.batch.opt_AM_robot<-test.batch.opt_AM_robot
test.batch.opt_AM_robot<-COPYtest.batch.opt_AM_robot


#lets run L func to undestand of a is correctly specified
L.func <- function(L, p0, a) {
 pmin(exp(-a / (L - a/log(p0))), 1)
}

L.func_AM <- function(L, p0, a) {
 pmin(a * exp(-a / (L - a/log(p0))), 1)
}
# Given values
L_value <- 1
p0_value <- 0.5
a_values <- c(0.1, 1, 10)

# Calculate results for each 'a' value
results_L.func <- sapply(a_values, function(a) L.func(L_value, p0_value, a))

# Display results
results_L.func

###contrarily to what is written in Gibbons et al. with increasing values of a, PL will decrease
###this means that we assume a to be 0.1 which reflects that the biodiveristy is more responsive to the action



