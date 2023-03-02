redim as integer n99(1 to jj)

for ii=1 to jj
    c03=mid(c050,1+4*(ii-1),4)
    n01=0 : n02=0 : n03=0 : n04=0 : n05=0 : n06=0 : n07=0 : n08=0
    n09=0 : n10=0 : n11=0 : n12=0 : n13=0 : n14=0 : n15=0 : n16=0
    n17=0 : n18=0 : n19=0 : n20=0 : n21=0 : n22=0 : n23=0 : n24=0
    n25=0 : n26=0 : n27=0 : n28=0 : n29=0 : n30=0 : n31=0 : n32=0
    cz1=mid(c03,1,2) : nc1=numero_case(cz1)
    cz2=mid(c03,3,2) : nc2=numero_case(cz2)
    dcl=abs(colonne(nc1)-colonne(nc2))
    dlg=abs(ligne(nc1)-ligne(nc2))
    
    n01=nature(oc0(nc2))
    
    if sc1(ii)=1 then n02=1
    
    if sc1(ii)=1 and len(c06(ii))/4=0 then n03=99
    if len(c06(ii))/4=0 and sc1(ii)=0 then n04=98
    
    for ij=1 to len(c06(ii))/4
        if len(c052(ii,ij))/4=0 then
            if sc2(ii,ij)=1 then
                n05=-99
            end if
        end if
    next ij
    
    ll=0 : lm=0
    for ij=1 to len(c06(ii))/4
        nc3=numero_case(mid(c06(ii),1+4*(ij-1),2))
        nc4=numero_case(mid(c06(ii),3+4*(ij-1),2))
        if nc4=nc2 then
            ll=1
            for ik=1 to len(c052(ii,ij))/4
                nc5=numero_case(mid(c052(ii,ij),1+4*(ik-1),2))
                nc6=numero_case(mid(c052(ii,ij),3+4*(ik-1),2))
                if nc6=nc4 then
                    lm=1
                    exit for
                end if
            next ik
            exit for
        end if
    next ij
    
    ln=0
    for ij=1 to len(c06(ii))/4
        nc3=numero_case(mid(c06(ii),1+4*(ij-1),2))
        nc4=numero_case(mid(c06(ii),3+4*(ij-1),2))
        if oc1(ii,nc4)>0 then ln+=nature(oc1(ii,nc4))
    next ij    
    
    if nature(oc1(ii,nc2))=cav then
        if trait=0 and ligne(nc1)=7 then n06=1
        if trait=1 and ligne(nc1)=0 then n06=1        
        if colonne(nc2)*(colonne(nc2)-7)=0 then n06=-1
        if ligne(nc2)>1 and colonne(nc2)>0 then
            ij=oc1(ii,nc2-17)
            if nature(ij)>cav and ll=0 then
                if trait=0 and couleur_piece(ij)=noir then n07+=nature(ij)
                if trait=1 and couleur_piece(ij)=blanc then n07+=nature(ij)
            end if
        end if
        if ligne(nc2)>1 and colonne(nc2)<7 then
            ij=oc1(ii,nc2-15)
            if nature(ij)>cav and ll=0 then
                if trait=0 and couleur_piece(ij)=noir then n07+=nature(ij)
                if trait=1 and couleur_piece(ij)=blanc then n07+=nature(ij)
            end if
        end if
        if ligne(nc2)>0 and colonne(nc2)>1 then
            ij=oc1(ii,nc2-10)
            if nature(oc1(ii,nc2-10))>cav and ll=0 then
                if trait=0 and couleur_piece(ij)=noir then n07+=nature(ij)
                if trait=1 and couleur_piece(ij)=blanc then n07+=nature(ij)
            end if
        end if
        if ligne(nc2)>0 and colonne(nc2)<6 then
            ij=oc1(ii,nc2-06)
            if nature(ij)>cav and ll=0 then
                if trait=0 and couleur_piece(ij)=noir then n07+=nature(ij)
                if trait=1 and couleur_piece(ij)=blanc then n07+=nature(ij)
            end if
        end if
        if ligne(nc2)<7 and colonne(nc2)>1 then
            ij=oc1(ii,nc2+06)
            if nature(ij)>cav and ll=0 then
                if trait=0 and couleur_piece(ij)=noir then n07+=nature(ij)
                if trait=1 and couleur_piece(ij)=blanc then n07+=nature(ij)
            end if
        end if
        if ligne(nc2)<7 and colonne(nc2)<6 then
            ij=oc1(ii,nc2+10)
            if nature(ij)>cav and ll=0 then
                if trait=0 and couleur_piece(ij)=noir then n07+=nature(ij)
                if trait=1 and couleur_piece(ij)=blanc then n07+=nature(ij)
            end if
        end if
        if ligne(nc2)<6 and colonne(nc2)>0 then
            ij=oc1(ii,nc2+10)
            if nature(ij)>cav and ll=0 then            
                if trait=0 and couleur_piece(ij)=noir then n07+=nature(ij)
                if trait=1 and couleur_piece(ij)=blanc then n07+=nature(ij)
            end if
        end if
        if ligne(nc2)<6 and colonne(nc2)<7 then
            ij=oc1(ii,nc2+17)
            if nature(ij)>cav and ll=0 then
                if trait=0 and couleur_piece(ij)=noir then n07+=nature(ij)
                if trait=1 and couleur_piece(ij)=blanc then n07+=nature(ij)
            end if
        end if
    end if
    
    if nature(oc1(ii,nc2))=fou then
        if trait=0 and ligne(nc1)=7 then n10=1
        if trait=1 and ligne(nc1)=0 then n10=1
        if ligne(nc2)>2 then
            if nature(oc1(ii,nc2-24))=cav then n10=n10+1
        end if
        if colonne(nc2)>2 then
            if nature(oc1(ii,nc2-03))=cav then n10=n10+1
        end if
        if colonne(nc2)<5 then
            if nature(oc1(ii,nc2+03))=cav then n10=n10+1
        end if
        if ligne(nc2)<5 then
            if nature(oc1(ii,nc2+24))=cav then n10=n10+1
        end if
    end if
    
    if nature(oc1(ii,nc2))=roi then
        if dcl=2 then
            n11=3
        end if
    end if
    
    if ll=1 and lm=0 then n12=-(10-2*nature(oc0(nc2)))
    
    if nature(oc0(nc2))=nature(oc1(ii,nc2)) then n13=1
    if nature(oc0(nc2))>nature(oc1(ii,nc2)) then n13=10
    
    if nature(oc1(ii,nc2))=pio then
        if dcl=1 and ll=0 then n14=10
    end if
    
    if nature(oc1(ii,nc2))=pio then
        select case as const trait
        case 0
            if colonne(nc2)>0 then
                if oc1(ii,nc2-9)>0 then n15=1
            end if
            if colonne(nc2)<7 then
                if oc1(ii,nc2-7)>0 then n15=1
            end if
        case 1
            if colonne(nc2)>0 then
                if oc1(ii,nc2+7)>0 then n15=1
              
            end if
            if colonne(nc2)<7 then
                if oc1(ii,nc2+9)>0 then n15=1
            end if
        end select
    end if
    
    n16=-ln
    
    if nature(oc1(ii,nc2))=pio then
        select case as const trait
        case 0
            if colonne(nc2)>0 then
                if oc1(ii,nc2+7)=pb then n17=n17+1
                if oc1(ii,nc1+7)=rb then n17=n17-1
            end if
            if colonne(nc2)<7 then
                if oc1(ii,nc2+9)=pb then n17=n17+1
                if oc1(ii,nc1+9)=rb then n17=n17-1
            end if
        case 1
            if colonne(nc2)>0 then
                if oc1(ii,nc2-9)=pn then n17=n17+1
                if oc1(ii,nc1-9)=rn then n17=n17-1
            end if
            if colonne(nc2)<7 then
                if oc1(ii,nc2-7)=pn then n17=n17+1
                if oc1(ii,nc1-7)=rn then n17=n17-1
            end if
        end select
    end if
    
    if nature(oc1(ii,nc2))=pio then
        if colonne(nc2)=3 or colonne(nc2)=4 then n18=01
        if colonne(nc2)=2 or colonne(nc2)=5 then n18=00
        if colonne(nc2)=1 or colonne(nc2)=6 then n18=-1
        if colonne(nc2)=0 or colonne(nc2)=7 then n18=-2
    end if
    
    if nature(oc1(ii,nc2))=pio then n19=00
    if nature(oc1(ii,nc2))=cav then n19=01
    if nature(oc1(ii,nc2))=fou then n19=01
    if nature(oc1(ii,nc2))=tou then n19=-1
    if nature(oc1(ii,nc2))=dam then n19=00
    if nature(oc1(ii,nc2))=roi then n19=-1
    
    if nature(oc1(ii,nc2))=pio then
        n20=(1-2*trait)*diff_coups_naturels(nc2)*(1/10)
    end if
    
    if nombre_coups>1 then
        if left(right(c11,8),4)=cz2+cz1 then n21=-1
    end if
    
    if nature(oc1(ii,nc2))=pio then
        select case as const trait
        case 0
            for ij=0 to 63
                if oc1(ii,ij)<>pb then continue for
                if colonne(ij)>0 and colonne(ij)<7 then
                    if oc1(ii,ij-9)=pb and oc1(ii,ij-7)=pb then n22=-1
                end if
            next ij
        case 1
            for ij=0 to 63
                if oc1(ii,ij)<>pn then continue for
                if colonne(ij)>0 and colonne(ij)<7 then
                    if oc1(ii,ij+7)=pn and oc1(ii,ij+9)=pn then n22=-1
                end if
            next ij
        end select
    end if    
    
    n99(ii)=0
    n99(ii)=n99(ii)+n01+n02+n03+n04+n05+n06+n07+n08+n09+n10
    n99(ii)=n99(ii)+n11+n12+n13+n14+n15+n16+n17+n18+n19+n20
    n99(ii)=n99(ii)+n21+n22+n23+n24+n25+n26+n27+n28+n29+n30
    n99(ii)=n99(ii)+n31+n32
next ii
n98=0 : for ii=1 to jj
    if ii=1 then n98=n99(ii)
    if n99(ii)>n98 then n98=n99(ii)
next ii
c08="" : for ii=1 to jj
    c03=mid(c050,1+4*(ii-1),4)
    if n99(ii)=n98 then c08=c08+c03
next ii
c09="" : if len(c08)/4=1 then c09=c08
if len(c08)/4>1 then c09=left(c08,4)

if nombre_coups=0 then c09="e2e4"
if nombre_coups>0 and nombre_coups<4 then
    restore ouverture
    do
        read c12
        if c12="fin" then exit do
        if left(c12,4*nombre_coups)=c11 then
            c09=mid(c12,1+4*nombre_coups,4)
            exit do
        end if
    loop
end if
