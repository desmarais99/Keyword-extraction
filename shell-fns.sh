function raw2nl () { ( cat $1 | tr ' ' '\n' | sed 1d | awk '{printf("%09d %s\n", NR, $1)}' > $1.nl ) }

function nl2nls () { ( sort -k 2,2 -k 1,1 $1.nl | sed 's;^00*;;'  > $1.nls ) }

function nls2wdist () { ( sed 1d $1.nls | paste $1.nls - | sed 1p | sed -e '1s;[0-9][0-9]*;1;g' -e '1s;1 [^	][^	]*;0 null;' | awk '{if($2==$4) print $4, $3-$1; else print $4, 0 ;}' > tempa ) ; ( sed 1p tempa | paste - tempa | sed '1s;[a-z][a-z]* [0-9][0-9]*;null 0;' | awk '{if($1==$3) printf(",%s", $4); else printf("\n%s,%s", $3,$4);}' | sed -e 1d -e '$d' | sed '$d' > $1.wdist ) }

function nls2wseq () { ( sed -f nls2wseq.sed $1.nls | paste $1.nls - | awk '{if($2==$4) printf(",%s", $1); else printf("\n%s,%s", $2,$1);}' | sed -e 1d -e '$d' > $1.wseq ) }
