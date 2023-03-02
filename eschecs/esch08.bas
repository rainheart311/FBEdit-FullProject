ij=len(c05)/4
redim as integer oc1(1 to ij,63), sc1(1 to ij), rq1(1 to ij,3), qr1(1 to ij,3)
redim as string ps6(1 to ij), ps7(1 to ij)
for ii=1 to len(c05)/4
    cz1=mid(c05,1+4*(ii-1),2) : cz2=mid(c05,3+4*(ii-1),2)
    nc1=numero_case(cz1) : nc2=numero_case(cz2)
    dcl=abs(colonne(nc1)-colonne(nc2))
    dlg=abs(ligne(nc1)-ligne(nc2))
    for ij=0 to 63
        oc1(ii,ij)=occupant(ij)
    next ij : for ij=0 to 3
        rq1(ii,ij)=rq(ij)
    next ij : if nature(occupant(nc1))=pio then' promotion
        if ligne(nc2)=0 then oc1(ii,nc1)=db
        if ligne(nc2)=7 then oc1(ii,nc1)=dn
        if dcl=1 and occupant(nc2)=0 then' prise en passant
            if trait=0 then oc1(ii,nc2+8)=0
            if trait=1 then oc1(ii,nc2-8)=0
        end if
    end if : if nature(occupant(nc1))=roi and dcl=2 then' roque
        if nc2=02 then
            oc1(ii,00)=0 : oc1(ii,03)=tn
        end if : if nc2=06 then
            oc1(ii,07)=0 : oc1(ii,05)=tn
        end if : if nc2=58 then
            oc1(ii,56)=0 : oc1(ii,59)=tb
        end if : if nc2=62 then
            oc1(ii,63)=0 : oc1(ii,61)=tb
        end if
    end if : if nature(occupant(nc1))=roi then' roque
        if occupant(nc1)=rn then
            rq1(ii,0)=0 : rq1(ii,1)=0
        end if : if occupant(nc1)=rb then
            rq1(ii,2)=0 : rq1(ii,3)=0
        end if
    end if : if nature(occupant(nc1))=tou then' roque
        if nom_case(nc1)="a8" then rq1(ii,0)=0
        if nom_case(nc1)="h8" then rq1(ii,1)=0
        if nom_case(nc1)="a1" then rq1(ii,2)=0
        if nom_case(nc1)="h1" then rq1(ii,3)=0
    end if : oc1(ii,nc2)=oc1(ii,nc1) : oc1(ii,nc1)=0 : ps1="" : for ij=0 to 63
        if trait=0 and couleur_piece(oc1(ii,ij))=blanc then ps1=ps1+nom_case(ij)
        if trait=1 and couleur_piece(oc1(ii,ij))=noir then ps1=ps1+nom_case(ij)
    next ij : c02="" : for ij=1 to len(ps1)/2
        cz1=mid(ps1,1+2*(ij-1),2) : nc1=numero_case(cz1)
        select case as const nature(oc1(ii,nc1))
        case 1
            select case as const trait
            case 0
                for nc2=0 to 63
                    if couleur_piece(oc1(ii,nc2))=blanc then continue for
                    if ligne(nc2)>ligne(nc1)-1 or ligne(nc2)<ligne(nc1)-2 then continue for
                    dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))                
                    if oc1(ii,nc2)=0 then
                        if dcl>0 or oc1(ii,nc1-8)>0 then continue for
                        if ligne(nc1)<6 and dlg>1 then continue for
                    end if : if oc1(ii,nc2)>0 then
                        if dlg>1 or dcl<>1 then continue for
                    end if : cz2=nom_case(nc2) : c02=c02+cz1+cz2
                next nc2
            case 1
                for nc2=0 to 63
                    if couleur_piece(oc1(ii,nc2))=noir then continue for
                    if ligne(nc2)<ligne(nc1)+1 or ligne(nc2)>ligne(nc1)+2 then continue for
                    dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))                
                    if oc1(ii,nc2)=0 then
                        if dcl>0 or oc1(ii,nc1+8)>0 then continue for
                        if ligne(nc1)>1 and dlg>1 then continue for
                    end if : if oc1(ii,nc2)>0 then
                        if dlg>1 or dcl<>1 then continue for
                    end if : cz2=nom_case(nc2) : c02=c02+cz1+cz2
                next nc2
            end select
        case 2
            for nc2=0 to 63
                if trait=0 and couleur_piece(oc1(ii,nc2))=blanc then continue for
                if trait=1 and couleur_piece(oc1(ii,nc2))=noir then continue for
                dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))
                if dcl*dlg=0 or dcl+dlg<>3 then continue for
                cz2=nom_case(nc2) : c02=c02+cz1+cz2             
            next nc2
        case 3
            for nc2=0 to 63
                if trait=0 and couleur_piece(oc1(ii,nc2))=blanc then continue for
                if trait=1 and couleur_piece(oc1(ii,nc2))=noir then continue for              
                dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))            
                if dcl<>dlg then continue for
                if dcl>1 then
                    dis=nc2-nc1 : pas=7
                    if abs(dis/7)<>fix(abs(dis/7)) then pas=9
                    if 2*dis<dis then pas=0-pas
                    nps=dis/pas : ik=1 : do
                        if oc1(ii,nc1+ik*pas)>0 then continue for
                        ik=ik+1
                    loop until ik=nps
                end if : cz2=nom_case(nc2) : c02=c02+cz1+cz2                 
            next nc2
        case 4
            for nc2=0 to 63
                if trait=0 and couleur_piece(oc1(ii,nc2))=blanc then continue for
                if trait=1 and couleur_piece(oc1(ii,nc2))=noir then continue for               
                dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))            
                if dcl*dlg>0 then continue for
                if dcl+dlg>1 then
                    dis=nc2-nc1 : pas=8
                    if abs(dis/8)<>fix(abs(dis/8)) then pas=1
                    if dis*2<dis then pas=0-pas
                    nps=dis/pas : ik=1 : do
                        if oc1(ii,nc1+ik*pas)>0 then continue for
                        ik=ik+1
                    loop until ik=nps
                end if : cz2=nom_case(nc2) : c02=c02+cz1+cz2               
            next nc2
        case 5
            for nc2=0 to 63
                if trait=0 and couleur_piece(oc1(ii,nc2))=blanc then continue for
                if trait=1 and couleur_piece(oc1(ii,nc2))=noir then continue for               
                dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))            
                if dcl<>dlg then
                    if dcl*dlg>0 then continue for
                end if : if dcl>1 or dlg>1 then
                    dis=nc2-nc1 : pas=9
                    if abs(dis/pas)<>fix(abs(dis/pas)) then pas=8
                    if abs(dis/pas)<>fix(abs(dis/pas)) then pas=7
                    if abs(dis/pas)<>fix(abs(dis/pas)) then pas=1
                    if dis*2<dis then pas=0-pas
                    nps=dis/pas : ik=1 : do
                        if oc1(ii,nc1+ik*pas)>0 then continue for
                        ik=ik+1
                    loop until ik=nps
                end if : cz2=nom_case(nc2) : c02=c02+cz1+cz2                
            next nc2
        case 6
        end select
    next ij
    sc1(ii)=0 : for ij=0 to 3
        qr1(ii,ij)=1
    next ij : ps6(ii)="" : ps7(ii)="" : for ij=1 to len(c02)/4
        cz2=mid(c02,3+4*(ij-1),2) : nc2=asc(cz2,1)-8*asc(cz2,2)+351
        select case as const trait
        case 0
            if oc1(ii,nc2)=rn then
                sc1(ii)=1 : qr1(ii,0)=0 : qr1(ii,1)=0
                ps6(ii)=ps6(ii)+mid(c02,1+4*(ij-1),2)
            end if
            if nc2>4 and nc2<8 then
                qr1(ii,1)=0 : ps7(ii)=ps7(ii)+mid(c02,1+4*(ij-1),2)
            end if
            if nc2<4 then
                qr1(ii,0)=0 : ps7(ii)=ps7(ii)+mid(c02,1+4*(ij-1),2)
            end if
        case 1
            if oc1(ii,nc2)=rb then
                sc1(ii)=1 : qr1(ii,2)=0 : qr1(ii,3)=0
                ps6(ii)=ps6(ii)+mid(c02,1+4*(ij-1),2)
            end if
            if nc2>55 and nc2<60 then
                qr1(ii,2)=0 : ps7(ii)=ps7(ii)+mid(c02,1+4*(ij-1),2)
            end if
            if nc2>60 then
                qr1(ii,3)=0 : ps7(ii)=ps7(ii)+mid(c02,1+4*(ij-1),2)
            end if
        end select
    next ij
next ii