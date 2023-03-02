ps2="" : for ii=0 to 63
    if trait=0 and couleur_piece(occupant(ii))=blanc then ps2=ps2+nom_case(ii)
    if trait=1 and couleur_piece(occupant(ii))=noir then ps2=ps2+nom_case(ii)
next ii : c02="" : for ii=1 to len(ps2)/2
    cz1=mid(ps2,1+2*(ii-1),2) : nc1=numero_case(cz1)
    select case as const nature(occupant(nc1))
    case 1
        select case as const trait
        case 0
            for nc2=0 to 63
                if couleur_piece(occupant(nc2))=blanc then continue for
                if ligne(nc2)>ligne(nc1)-1 or ligne(nc2)<ligne(nc1)-2 then continue for
                dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))
                if occupant(nc2)=0 then
                    if dcl>0 or occupant(nc1-8)>0 then continue for
                    if ligne(nc1)<6 and dlg>1 then continue for
                end if : if occupant(nc2)>0 then
                    if dlg>1 or dcl<>1 then continue for
                end if : cz2=nom_case(nc2) : c02=c02+cz1+cz2
            next nc2
            if pion_passant>0 then
                if ligne(nc1)=3 then
                    if colonne(nc1)>0 then
                        if occupant(nc1-1)=pn then
                            if nom_case(nc1-1-16)+nom_case(nc1-1)=c10 then
                                cz2=nom_case(nc1-1-8) : c02=c02+cz1+cz2
                            end if
                        end if
                    end if
                    if colonne(nc1)<7 then
                        if occupant(nc1+1)=pn then
                            if nom_case(nc1+1-16)+nom_case(nc1+1)=c10 then
                                cz2=nom_case(nc1+1-8) : c02=c02+cz1+cz2
                            end if
                        end if
                    end if
                end if
            end if             
        case 1
            for nc2=0 to 63
                if couleur_piece(occupant(nc2))=noir then continue for
                if ligne(nc2)<ligne(nc1)+1 or ligne(nc2)>ligne(nc1)+2 then continue for
                dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))
                if occupant(nc2)=0 then
                    if dcl>0 or occupant(nc1+8)>0 then continue for
                    if ligne(nc1)>1 and dlg>1 then continue for
                end if : if occupant(nc2)>0 then
                    if dlg>1 or dcl<>1 then continue for
                end if : cz2=nom_case(nc2) : c02=c02+cz1+cz2
            next nc2
            if pion_passant>0 then
                if ligne(nc1)=4 then
                    if colonne(nc1)>0 then
                        if occupant(nc1-1)=pb then
                            if nom_case(nc1-1+16)+nom_case(nc1-1)=c10 then
                                cz2=nom_case(nc1-1+8) : c02=c02+cz1+cz2
                            end if
                        end if
                    end if
                    if colonne(nc1)<7 then
                        if occupant(nc1+1)=pb then
                            if nom_case(nc1+1+16)+nom_case(nc1+1)=c10 then
                                cz2=nom_case(nc1+1+8) : c02=c02+cz1+cz2
                            end if
                        end if
                    end if
                end if
            end if
        end select
    case 2
        for nc2=0 to 63
            if couleur_piece(occupant(nc2))=couleur_piece(occupant(nc1)) then continue for
            dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))
            if dcl*dlg=0 or dcl+dlg<>3 then continue for
            cz2=nom_case(nc2) : c02=c02+cz1+cz2
        next nc2
    case 3
        for nc2=0 to 63
            if couleur_piece(occupant(nc2))=couleur_piece(occupant(nc1)) then continue for          
            dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))            
            if dcl<>dlg then continue for
            if dcl>1 then
                dis=nc2-nc1 : pas=7
                if abs(dis/7)<>fix(abs(dis/7)) then pas=9
                if 2*dis<dis then pas=0-pas
                nps=dis/pas : ij=1 : do
                    if occupant(nc1+ij*pas)>0 then continue for
                    ij=ij+1
                loop until ij=nps
            end if : cz2=nom_case(nc2) : c02=c02+cz1+cz2
        next nc2
    case 4
        for nc2=0 to 63
            if couleur_piece(occupant(nc2))=couleur_piece(occupant(nc1)) then continue for            
            dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))
            if dcl*dlg>0 then continue for
            if dcl+dlg>1 then
                dis=nc2-nc1 : pas=8
                if abs(dis/8)<>fix(abs(dis/8)) then pas=1
                if dis*2<dis then pas=0-pas
                nps=dis/pas : ij=1 : do
                    if occupant(nc1+ij*pas)>0 then continue for
                    ij=ij+1
                loop until ij=nps
            end if : cz2=nom_case(nc2) : c02=c02+cz1+cz2
        next nc2
    case 5
        for nc2=0 to 63        
            if couleur_piece(occupant(nc2))=couleur_piece(occupant(nc1)) then continue for            
            dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))
            if dcl<>dlg then
                if dcl*dlg>0 then continue for
            end if : if dcl>1 or dlg>1 then
                dis=nc2-nc1 : pas=9
                if abs(dis/pas)<>fix(abs(dis/pas)) then pas=8
                if abs(dis/pas)<>fix(abs(dis/pas)) then pas=7
                if abs(dis/pas)<>fix(abs(dis/pas)) then pas=1
                if dis*2<dis then pas=0-pas
                nps=dis/pas : ij=1 : do
                    if occupant(nc1+ij*pas)>0 then continue for
                    ij=ij+1
                loop until ij=nps
            end if : cz2=nom_case(nc2) : c02=c02+cz1+cz2
        next nc2
    case 6
        for nc2=0 to 63
            if couleur_piece(occupant(nc2))=couleur_piece(occupant(nc1)) then continue for                      
            dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))            
            if dcl>2 or dlg>1 then continue for
            select case as const trait
            case 0
                if nc1=60 and nc2=58 then
                    if occupant(59)=0 and occupant(57)=0 and occupant(56)=7 then
                        if rq(2)=1 and qr(2)=1 then
                            cz2=nom_case(nc2) : c02=c02+cz1+cz2 : continue for
                        end if : continue for
                    end if
                end if
                if nc1=60 and nc2=62 then
                    if occupant(61)=0 and occupant(63)=7 then
                        if rq(3)=1 and qr(3)=1 then
                            cz2=nom_case(nc2) : c02=c02+cz1+cz2 : continue for
                        end if : continue for
                    end if
                end if
            case 1
                if nc1=4 and nc2=2 then
                    if occupant(3)=0 and occupant(1)=0 and occupant(0)=8 then
                        if rq(0)=1 and qr(0)=1 then
                            cz2=nom_case(nc2) : c02=c02+cz1+cz2 : continue for
                        end if : continue for
                    end if
                end if
                if nc1=4 and nc2=6 then
                    if occupant(5)=0 and occupant(7)=8 then
                        if rq(1)=1 and qr(1)=1 then
                            cz2=nom_case(nc2) : c02=c02+cz1+cz2 : continue for
                        end if : continue for
                    end if
                end if
            end select : if dcl>1 then continue for
            cz2=nom_case(nc2) : c02=c02+cz1+cz2
        next nc2
    end select
next ii : c05="" : ps5="" : for ii=1 to len(c02)/4
    cz1=mid(c02,1+4*(ii-1),2) : cz2=mid(c02,3+4*(ii-1),2)
    c03=cz1+cz2 : nc1=numero_case(cz1) : nc2=numero_case(cz2) : for ij=0 to 63
        ocb(ij)=occupant(ij)
    next ij : if nature(occupant(nc1))=pio then
        if abs(colonne(nc1)-colonne(nc2))=1 and occupant(nc2)=0 then
            if trait=0 then ocb(nc2+8)=0
            if trait=1 then ocb(nc2-8)=0
        end if
    end if : ocb(nc2)=ocb(nc1) : ocb(nc1)=0
    ps2="" : for ij=0 to 63
        if trait=0 and couleur_piece(ocb(ij))=noir then ps2=ps2+nom_case(ij)
        if trait=1 and couleur_piece(ocb(ij))=blanc then ps2=ps2+nom_case(ij)
    next ij : c04="" : for ij=1 to len(ps2)/2
        cz1=mid(ps2,1+2*(ij-1),2) : nc1=numero_case(cz1)
        select case as const nature(ocb(nc1))
        case 1
            select case as const trait
            case 0
                for nc2=0 to 63
                    if nature(ocb(nc2))<roi then continue for
                    if ligne(nc2)<ligne(nc1)+1 or ligne(nc2)>ligne(nc1)+2 then continue for
                    dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))
                    if dlg<>1 or dcl<>1 then continue for
                    cz2=nom_case(nc2) : c04=c04+cz1+cz2
                next nc2              
            case 1
                for nc2=0 to 63
                    if nature(ocb(nc2))<roi then continue for
                    if ligne(nc2)>ligne(nc1)-1 then continue for
                    if ligne(nc2)<ligne(nc1)-2 then continue for
                    dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))
                    if dlg<>1 or dcl<>1 then continue for
                    cz2=nom_case(nc2) : c04=c04+cz1+cz2
                next nc2              
            end select
        case 2
            for nc2=0 to 63
                if nature(ocb(nc2))<roi then continue for
                dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))
                if dcl*dlg=0 or dcl+dlg<>3 then continue for
                cz2=nom_case(nc2) : c04=c04+cz1+cz2
            next nc2
        case 3
            for nc2=0 to 63
                if nature(ocb(nc2))<roi then continue for
                dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))
                if dcl<>dlg then continue for
                if dcl>1 then
                    dis=nc2-nc1 : pas=7
                    if abs(dis/7)<>fix(abs(dis/7)) then pas=9
                    if 2*dis<dis then pas=0-pas
                    nps=dis/pas : ik=1 : do
                        if ocb(nc1+ik*pas)>0 then continue for
                        ik=ik+1
                    loop until ik=nps
                end if : cz2=nom_case(nc2) : c04=c04+cz1+cz2
            next nc2
        case 4
            for nc2=0 to 63
                if nature(ocb(nc2))<roi then continue for
                dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))
                if dcl*dlg>0 then continue for
                if dcl+dlg>1 then
                    dis=nc2-nc1 : pas=8
                    if abs(dis/8)<>fix(abs(dis/8)) then pas=1
                    if dis*2<dis then pas=0-pas
                    nps=dis/pas : ik=1 : do
                        if ocb(nc1+ik*pas)>0 then continue for
                        ik=ik+1
                    loop until ik=nps
                end if : cz2=nom_case(nc2) : c04=c04+cz1+cz2
            next nc2
        case 5
            for nc2=0 to 63
                if nature(ocb(nc2))<roi then continue for
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
                        if ocb(nc1+ik*pas)>0 then continue for
                        ik=ik+1
                    loop until ik=nps
                end if : cz2=nom_case(nc2) : c04=c04+cz1+cz2
            next nc2
        case 6
            for nc2=0 to 63
                if nature(ocb(nc2))<roi or nc2=nc1 then continue for
                dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))
                if dlg>1 or dcl>1 then continue for            
                cz2=nom_case(nc2) : c04=c04+cz1+cz2
            next nc2                    
        end select
    next ij : hcs=0 : for ij=1 to len(c04)/4
        nc2=numero_case(mid(c04,3+4*(ij-1),2))
        if trait=0 and ocb(nc2)=rb then
            hcs=1 : ps5=ps5+mid(c04,1+4*(ij-1),2)
        end if
        if trait=1 and ocb(nc2)=rn then
            hcs=1 : ps5=ps5+mid(c04,1+4*(ij-1),2)
        end if
    next ij : if hcs=0 then c05=c05+c03
next ii