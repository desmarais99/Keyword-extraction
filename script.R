## fasttext cbow -minCount 0 -dim 10 -bucket 100 -input temp -output temp.res
## fasttext cbow -minCount 0 -dim 10 -bucket 200ll  -input temp -output temp.res
## fasttext cbow -minCount 0 -dim 10 -bucket 1000 -ws 300 -input temp -output temp300.res
## fasttext cbow -minCount 3 -dim 100 -bucket 1000000 -ws 100 -input fil9 -output fil9.res

library(readr)
library(dplyr)

m <- read.table('temp.res.vec')
tbl <- read_table('temp300.res.vec', skip=1, col_names=c('w',1:100))
m <- as.matrix(tbl[,-1])
m.cor <- (cor(t(m)))
diag(m.cor) <- 0
prod(dim(m.cor))
hist(m.cor)
m.hcor <- m.cor>.9
sum(m.hcor)
words <- as.data.frame(tbl[,1])$w
pair.names <- outer(words, words, paste)
pair.names[1:9,1:8]
pair.names[m.hcor]

0

library(fastText)
list_params <- list(command = 'nn',f model = file.path('.', 'fil9.res.bin'), k=3, query_word='entropy')
fasttext_interface(list_params, path_output=file.path('.', 'temp.text'))

library(fastTextR)
model <- read.fasttext('fil9.res.bin')
plot(c(36,45,57,0,115,133,152))

p
#############################################################################
## Data manipulation
#############################################################################
# m <- read.table('fil9.wdist')
## sample:
## gshuf fil9.wdist | head -2000 > h.wdist

library(ggplot2)
m <- read.csv('h.wdist', sep=' ', header=F)
m2 <- apply(m, 1, strsplit, ',')
m2[[1]]
m2.freq <- unlist(lapply(m2, function(i) length(unlist(i))))
table(m2.freq)
ggplot(data.frame(V1=log(m2.freq)), aes(x=V1)) + geom_histogram() + scale_x_log10()
## Only frequency greater than 5
m <- m[intersect(which(m2.freq>5), sample(length(m2), 300, prob=m2.freq)),]
(n5 <- length(m))
m[2]
m.pdist <- lapply(m, function(i) as.numeric(unlist(strsplit(i, ','))[-(1:2)]))
m.w <- lapply(m, function(i) (unlist(strsplit(i, ','))[1]))
## total number of words in corpus
tail(m.pdist)

tnwc <- 124301827/1000
length(m.pdist)
hist(log(tnwc/m.pdist[n5][[1]]))
m[n5]
m.pd2 <- lapply(m.pdist, function(i) log(tnwc/i))

par(mfrow=c(5,5))
foo <- sapply(sample(n5,25), function(i) hist(log(pmax(1,m.pd2[[i]])), main=m.w[i], ylim=c(0,200), xlim=c(0,3)))

#############################################################################
## entropy enthalpy
#############################################################################
m <- read.csv('h-ent.wdist', sep=' ', header=F)
m2 <- apply(m, 1, strsplit, ',')
m2.freq <- unlist(lapply(m2, function(i) length(unlist(i))))
table(m2.freq)
ggplot(data.frame(V1=log(m2.freq)), aes(x=V1)) + geom_histogram() + scale_x_log10()
m.w <- lapply(m[[1]], function(i) (unlist(strsplit(i, ',')))) # i[[1]]
str(m.w)
m.pdist <- lapply(m.w, function(i) as.numeric(unlist(strsplit(i, ','))[-(1:2)]))

## total number of words in corpus
str(m.pdist)

tnwc <- 124301827/1000
length(m.pdist)
m.pd2 <- lapply(m.pdist, function(i) log(tnwc/i))

par(mfrow=c(3,3))
foo <- sapply(1:length(m2), function(i) hist(log(pmax(1,m.pd2[[i]])), main=m2[[i]][[1]][1], ylim=c(0,100), xlim=c(0,3)))

max(m.pdist[[1]])
length(m.pdist[[1]])
max(m.pdist[[2]])
length(m.pdist[[2]])
max(m.pdist[[3]])
length(m.pdist[[3]])
lapply(m.pdist, sum)
0

## histograms
tbl <- read.table('temp.wseq', header=F)
strsplit(tbl[1:2,-1], ',')
m <- (strsplit(tbl[,-1], ','))
m <- lapply(m, as.numeric)
m100.len <- sapply(m, length)
hist(log(m100.len+1))
(max(m100.len))
c(m[[4]][-1], 0) - m[[4]]
c(m[[2000]][-1], 0) - m[[2000]]
m[c(2000,4)]
kmeans(log((m[[length(m)]][-1]) - (m[[length(m)]][-length(m[[length(m)]])])), 2)

m[[which(grepl('wto', tbl[[1]]))]]

library(fastTextR)
ft <- fasttext()
ft$load('fil9.5ws.bin')
ft$nearest_neighbors('wto', 20)
ft$nearest_neighbors('entropy', 20)
ft$nearest_neighbors('enthalpy', 20)
ft$word_vectors(c('entropy', 'enthalpy'))
cor(t(ft$word_vectors(c('entropy', 'enthalpy','heat', 'mercosul','wto','nafta','economy'))))
cor(t(ft$word_vectors(c('svd', 'orthogonality','orthonormal', 'gain','time','dimension','projection','entropy', 'enthalpy','heat', 'mercosul','wto','nafta','economy'))))
## ergodicity, matrix, boltzman, markov chain, aperiodic


freqdec <- function(word) {
    i=which(grepl(paste0('^', word, '$'), tbl[[1]]))
    barplot(log(abs(Re(fft(log((m[[i]][-1]) - ((m[[i]][-1]) - (m[[i]][-length(m[[i]])]))))))), main=tbl[[1]][i])
    barplot(Im(fft(log((m[[i]][-1]) - ((m[[i]][-1]) - (m[[i]][-length(m[[i]])]))))), main=tbl[[1]][i])
}

par(mfrow=c(4,4))

freqdec('himalaya')
freqdec('insurance')
freqdec('kruger')
freqdec('involvement')
freqdec('sakura')
freqdec('although')
freqdec('raincoat')
freqdec('physiques')
freqdec('turfan')
freqdec('humbled')
freqdec('sants')
freqdec('microkernel')
freqdec('sants')
freqdec('grumble')
freqdec('ipoh')
freqdec('bitches')
freqdec('untiring')
freqdec('peculiarities')
freqdec('thamyris')
freqdec('nevada')
freqdec('implosion')
freqdec('dewitt')
freqdec('instrument')
freqdec('lutheranism')
freqdec('')
freqdec('')
freqdec('')
## curiously similar 
freqdec('pilate')
freqdec('undamaged')

0
## 10 mars

m <- read.table('temp.wseq')
l2 <- (lapply(strsplit(apply(m, 1, function(i) sub(',', ' ', i)), ' '), function(j) list(w=j[[1]], seq=as.numeric(strsplit(j[[2]], ',')[[1]]))))
sapply(l2, function(i) c(i[[1]], length(i[[2]])))

a <- l2[[1]][[2]]                       # entropy
b <- l2[[2]][[2]]                       # enthalpy
c <- l2[[3]][[2]]                       # ent
e <- l2[[5]][[2]]                       # ashkenazi

source('pick-next.R')
d <- pick.next(a, b)
f <- pick.next(c, e)
head(c)

par(mfrow=c(1,1))
par(mfrow=c(3,2))

plot(c[1:length(d)],d+rnorm(length(d), 0, .05), type='b')
plot(d+rnorm(length(d), 0, .05), type='b')

plot(c[1:length(f)],f+rnorm(length(f), 0, .05), type='b')
plot(d+rnorm(length(d), 0, .05), type='b')

l2[[7]]
foo <- outer(l2[[7]][[2]], l2[[6]][[2]], '-')
foo[1:8,1:8]
summary(c(log(abs(outer(l2[[1]][[2]], l2[[8]][[2]], '-')))))
image(log(abs(outer(l2[[1]][[2]], l2[[8]][[2]], '-'))), xlab=l2[[1]][[1]], ylab=l2[[8]][[1]], breaks=1:20, col=hcl.colors(19))

f <- function(a, b) image(log(abs(outer(l2[[a]][[2]], l2[[b]][[2]], '-'))), xlab=l2[[a]][[1]], ylab=l2[[b]][[1]], breaks=1:20, col=hcl.colors(19))

f(1,2)
f(13,1)
f(4,7)
f(1,1)
f(2,2)
f(1,11)
f(7,13)
f(17,16)
f(15,12)
f(14,26)
f2(11,11)[1:8,1:8]
0

f2 <- function(a, b) outer(log(l2[[a]][[2]]), log(l2[[b]][[2]]), '-')

foo <- f2(6,7)
svd(foo)$d
svd(f2(6,11))$d[1:20]
svd(f2(6,11))$d[1:20]
qr(f2(6,11))$rank
qr(f2(3,2))$rank
t(sapply(l2, function(i) data.frame(w=i[[1]], len=length(i[[2]]))))
