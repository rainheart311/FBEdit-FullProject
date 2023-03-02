for ii=0 to 63
    oc0(ii)=occupant(ii)
next ii
for ii=0 to 03
    rq0(ii)=rq(ii)
next ii
schach0=schach
c050=c05
ps30=ps3
ps40=ps4
ps50=ps5

redim as integer schach2(1 to len(c05)/4,1 to kk)
redim as string c052(01 to len(c05)/4,1 to kk)
redim as string c052(01 to len(c05)/4,1 to kk)
jj=len(c05)/4
ki=1
un:
for ii=0 to 3
    rq(ii)=rq1(ki,ii)
next ii
jk=len(c06(ki))/4
kj=1
deux:
for ii=0 to 63
    occupant(ii)=oc2(ki,kj,ii)
next ii

#include "esch05.bas"
#include "esch06.bas"

schach2(ki,kj)=schach
c052(ki,kj)=c05
c052(ki,kj)=c05
kj=kj+1
if kj<jk+1 then goto deux
ki=ki+1
if ki<jj+1 then goto un