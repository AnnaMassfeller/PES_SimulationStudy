########################################
##Simulation model for the paper#######
########################################

#-----------------------------------------
# original model by Gibbons et al. (2011)
#-----------------------------------------

#time t needed to rqch 95% probability of detetcability
t.fix <- function(db, v=0.95) {
 
 log(1-v)/log(1-db)
 
}


#PL as function of L, p0 and a
L.func <- function(L, p0, a) { #l.func updated such that with increasing a, PL increases (i.e. multiplied by a)
 pmin(a * exp(-a / (L - a/log(p0))), 1)
}

#Probability of non-compliance detecttion
Pc.func <- function(dc=0.8, t=t.fix(dc)) {
 
 1-(1-dc)^t
 
}

#Probability of biodiveristy detection
Pd.func <- function(db=0.8, v=0.95, t=t.fix(db,v), L, p0, a) {
 L.func(L, p0, a) * (1 - (1-db)^t)
}

#Farmers' income inf RBS case
I.func <- function(L, p0, x, Cl=100, D=Cl*x, Cm=Cl/10, db=0.8, v=0.95, t=t.fix(db, v), a=1) {
 Pd.func(db, v=v, t, L, p0, a) * D - Cl*L - Cm
}

#Expenditure of the agency in RBS case
E.func.out <- function(x, L, p0, a, Cl=100, Ch=10, db=0.8, v=0.95, t=t.fix(db,v)) {
 
 x*Pd.func(db=db, v=v, t=t, L=L, p0=p0, a=a)*Cl + (t+1)*Ch
 
}

#Income and expenditure in ABS case
I.act <- function(x=11/10, Cl=100, Cm=Cl/10) {x*Cl - Cl - Cm}
E.act <- function(dc=0.95, x=11/10, Cl=100, Ch=10, v=0.95, Pv = 0.6) {
 
 x*Cl+Pv*(t.fix(dc,v)+1)*Ch
 
}

#Biodiveristy 
B.act <- function(x=11/10, p0, a) {
 
 ifelse(x>11/10, L.func(1, p0, a), p0) # why x >11/10?
 
}

#Gain in Biodiveristy
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

#simulate batch of plots for RBS case
batch.opt <- function(a=c(0.1, 0.25, 0.5, 1.0, 1.5), p0=seq(0.01, 0.99, by=0.01), 
                      x.mult=seq(1.0, 50, by=0.1), db=0.95, v=0.95) {
 
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

#simulate batch of plots for ABS case
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

#-----------------------------------------
# adjustments by Massfeller et al. (2025) 
#-----------------------------------------

#now we make our changes assuming a robot is used
#this mainly concerns the expenditure functions
#we swe assume varying ranges for CL, Pv and Ch

#expenditure function for RBS and ABD case
#RBS
E.func.out_robot <- function(x, L, p0, a, Cl=Cl, Ch=Ch, db=db, v=0.95, t=t.fix(db,v)) {#take pv out/ set it to 1
 
 x*Pd.func(db=db, v=v, t=t, L=L, p0=p0, a=a)*Cl + (t+1)*Ch
 
}

#ABS
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


#now create results with robot by setting parameter values as explained in the paper

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
#test.batch.opt_AM_robot_Lfunc_rangea <- batch.opt_robot(db = seq(0.1,0.9, by = 0.2),
                                                       # a=seq((1/3),10, by = (1/3)),
                                                       # p0=seq(0.1, 0.9, by=0.1), 
                                                       # x.mult=seq(5, 50, by=5), 
                                                       # Cl = seq(25, 200, by=25),
                                                       # Ch = seq(2, 20, by=2))


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

