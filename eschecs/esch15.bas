cz1=mid(c10,1,2) : nc1=numero_case(cz1) : nc5=nc1
cz2=mid(c10,3,2) : nc2=numero_case(cz2) : nc6=nc2
dcl=abs(colonne(nc1)-colonne(nc2))
dlg=abs(ligne(nc1)-ligne(nc2))
for ii=0 to 63
    occupant(ii)=oc0(ii)
next ii
for ii=0 to 03
    rq(ii)=rq0(ii)
next ii
if nature(occupant(nc1))=pio and ligne(nc2)=0 then occupant(nc1)=db
if nature(occupant(nc1))=pio and ligne(nc2)=7 then occupant(nc1)=dn
pion_passant=0 : if nature(occupant(nc1))=pio and dlg=2 then pion_passant=1+colonne(nc1)
pris_en_passant=0
if nature(occupant(nc1))=pio then
    if dcl=1 and occupant(nc2)=0 then
        select case as const trait
        case 0
            pris_en_passant=1 : nc9=nc2+8 : occupant(nc9)=0
        case 1
            pris_en_passant=1 : nc9=nc2-8 : occupant(nc9)=0
        end select
    end if
end if
roque=0
if nature(occupant(nc1))=roi then
    if occupant(nc1)=rb then
        rq(2)=0 : rq(3)=0
    else
        rq(0)=0 : rq(1)=0
    end if
    if dcl=2 then
        if nc2=2 then
            occupant(00)=0 : occupant(03)=8 : roque=1 : nc7=00 : nc8=03
        end if
        if nc2=6 then
            occupant(07)=0 : occupant(05)=8 : roque=1 : nc7=07 : nc8=05
        end if
        if nc2=58 then
            occupant(56)=0 : occupant(59)=7 : roque=1 : nc7=56 : nc8=59
        end if
        if nc2=62 then
            occupant(63)=0 : occupant(61)=7 : roque=1 : nc7=63 : nc8=61
        end if
    end if
end if
if nature(occupant(nc1))=tou then
    if nom_case(nc1)="a8" then rq(0)=0
    if nom_case(nc1)="h8" then rq(1)=0
    if nom_case(nc1)="a1" then rq(2)=0
    if nom_case(nc1)="h1" then rq(3)=0
end if

prise=occupant(nc2)
if pris_en_passant=1 then
    if trait=0 then prise=pn
    if trait=1 then prise=pb
end if

occupant(nc2)=occupant(nc1) : occupant(nc1)=0
nombre_coups=nombre_coups+1 : trait=trait+1
if trait=2 then trait=0
goto nouveau

ouverture :
data "c2c4c7c6g1f3d7d5"
data "c2c4c7c6d2d4d7d5"
data "d2d4g8f6c2c4e7e6"
data "d2d4g8f6c1g5f6e4"
data "d2d4g8f6g1f3d7d5"
data "e2e4e7e5g1f3b8c6"
data "e2e4c7c5g1f3d7d6"
data "e2e4d7d5e4d5d8d5"
data "e2e4e7e6d2d4d7d5"
data "e2e4c7c6d2d4d7d5"
data "e2e4g8f6e4e5f6d5"
data "g1f3g8f6c2c4c7c6"
data "g1f3g8f6d2d4d7d5"
data "fin"