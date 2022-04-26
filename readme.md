find geometry/intro data_mining/intro physics/intro precalculus/intro -name '*.txt' > tempa
cat $(shuf tempa) > tempb
source shell-fns.sh
raw2nl tempb
nl2nls tempb
nls2wdist tempb
cp tempb.wdist words.wdist 
