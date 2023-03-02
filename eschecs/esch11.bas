for ii=1 to len(c05)/4
    if ii=1 then ij=len(c06(ii))/4
    if len(c06(ii))/4>ij then ij=len(c06(ii))/4
next ii : kk=ij : ii=len(c05)/4 : redim as integer oc2(1 to ii,1 to ij,63)
redim as integer sc2(1 to ii,1 to ij), qr2(1 to ii,1 to ij,3)
for ii=1 to len(c05)/4
    for ij=1 to len(c06(ii))/4
        cz1=mid(c06(ii),1+4*(ij-1),2) : cz2=mid(c06(ii),3+4*(ij-1),2)
        nc1=numero_case(cz1) : nc2=numero_case(cz2) : dcl=abs(colonne(nc1)-colonne(nc2))
        dlg=abs(ligne(nc1)-ligne(nc2)) : for ik=0 to 63
            ocb(ik)=oc1(ii,ik)
        next ik : if nature(oc1(ii,nc1))=pio then
            if ligne(nc2)=0 then ocb(nc1)=db
            if ligne(nc2)=7 then ocb(nc1)=dn
            if dcl=1 and oc1(ii,nc2)=0 then
                if trait=0 then ocb(nc2-8)=0
                if trait=1 then ocb(nc2+8)=0
            end if
        end if : if nature(oc1(ii,nc1))=roi then
            if dcl=2 then
                if nc2=2 then
                    ocb(0)=0 : ocb(3)=8
                end if
                if nc2=6 then
                    ocb(7)=0 : ocb(5)=8
                end if
                if nc2=58 then
                    ocb(56)=0 : ocb(59)=7
                end if
                if nc2=62 then
                    ocb(63)=0 : ocb(61)=7
                end if
            end if
        end if : ocb(nc2)=ocb(nc1) : ocb(nc1)=0 : for ik=0 to 63
            oc2(ii,ij,ik)=ocb(ik)
        next ik : ps2="" : for ik=0 to 63
            if trait=0 and couleur_piece(ocb(ik))=noir then ps2=ps2+nom_case(ik)
            if trait=1 and couleur_piece(ocb(ik))=blanc then ps2=ps2+nom_case(ik)
        next ik : c01="" : for ik=1 to len(ps2)/2
            cz1=mid(ps2,1+2*(ik-1),2) : nc1=numero_case(cz1)
            select case as const nature(ocb(nc1))
            case 1
                select case as const trait
                case 0
                    for nc2=0 to 63
                        if couleur_piece(ocb(nc2))=noir then continue for
                        if ligne(nc2)<ligne(nc1)+1 then continue for
                        if ligne(nc2)>ligne(nc1)+2 then continue for
                        dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))                
                        if ocb(nc2)=0 then
                            if dcl>0 or ocb(nc1+8)>0 then continue for
                            if ligne(nc1)>1 and dlg>1 then continue for
                        end if : if ocb(nc2)>0 then
                            if dlg>1 or dcl<>1 then continue for
                        end if : cz2=nom_case(nc2) : c01=c01+cz1+cz2
                    next nc2
                case 1
                    for nc2=0 to 63
                        if couleur_piece(ocb(nc2))=blanc then continue for
                        if ligne(nc2)>ligne(nc1)-1 then continue for
                        if ligne(nc2)<ligne(nc1)-2 then continue for
                        dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))                
                        if ocb(nc2)=0 then
                            if dcl>0 or ocb(nc1-8)>0 then continue for
                            if ligne(nc1)<6 and dlg>1 then continue for
                        end if : if ocb(nc2)>0 then
                            if dlg>1 or dcl<>1 then continue for
                        end if : cz2=nom_case(nc2) : c01=c01+cz1+cz2
                    next nc2
                end select
            case 2
                for nc2=0 to 63
                    if couleur_piece(ocb(nc2))=couleur_piece(ocb(nc1)) then continue for
                    dcl=abs(colonne(nc1)-colonne(nc2))
                    dlg=abs(ligne(nc1)-ligne(nc2))
                    if dcl*dlg=0 then continue for
                    if dcl+dlg<>3 then continue for
                    cz2=nom_case(nc2)
                    c01=c01+cz1+cz2
                next nc2
            case 3
                for nc2=0 to 63
                    if couleur_piece(ocb(nc2))=couleur_piece(ocb(nc1)) then continue for
                    dcl=abs(colonne(nc1)-colonne(nc2))
                    dlg=abs(ligne(nc1)-ligne(nc2))
                    if dcl<>dlg then continue for
                    if dcl>1 then
                        dis=nc2-nc1
                        pas=7
                        if abs(dis/7)<>fix(abs(dis/7)) then pas=9
                        if 2*dis<dis then pas=0-pas
                        nps=dis/pas
                        ji=1
                        do
                            if ocb(nc1+ji*pas)>0 then continue for
                            ji=ji+1
                        loop until ji=nps
                    end if
                    cz2=nom_case(nc2)
                    c01=c01+cz1+cz2
                next nc2
            case 4
                for nc2=0 to 63                
                    if couleur_piece(ocb(nc2))=couleur_piece(ocb(nc1)) then continue for
                    dcl=abs(colonne(nc1)-colonne(nc2))
                    dlg=abs(ligne(nc1)-ligne(nc2))
                    if dcl*dlg>0 then continue for
                    if dcl+dlg>1 then
                        dis=nc2-nc1
                        pas=8
                        if abs(dis/8)<>fix(abs(dis/8)) then pas=1
                        if dis*2<dis then pas=0-pas
                        nps=dis/pas
                        ji=1
                        do
                            if ocb(nc1+ji*pas)>0 then continue for
                            ji=ji+1
                        loop until ji=nps
                    end if
                    cz2=nom_case(nc2)
                    c01=c01+cz1+cz2
                next nc2
            case 5
                for nc2=0 to 63
                    if couleur_piece(ocb(nc2))=couleur_piece(ocb(nc1)) then continue for
                    dcl=abs(colonne(nc1)-colonne(nc2))
                    dlg=abs(ligne(nc1)-ligne(nc2))            
                    if dcl<>dlg then
                        if dcl*dlg>0 then continue for
                    end if
                    if dcl>1 or dlg>1 then
                        dis=nc2-nc1 : pas=9
                        if abs(dis/pas)<>fix(abs(dis/pas)) then pas=8
                        if abs(dis/pas)<>fix(abs(dis/pas)) then pas=7
                        if abs(dis/pas)<>fix(abs(dis/pas)) then pas=1
                        if dis*2<dis then pas=0-pas
                        nps=dis/pas
                        ji=1
                        do
                            if ocb(nc1+ji*pas)>0 then continue for
                            ji=ji+1
                        loop until ji=nps
                    end if
                    cz2=nom_case(nc2)
                    c01=c01+cz1+cz2
                next nc2
            case 6
                for nc2=0 to 63
                    if couleur_piece(ocb(nc2))=couleur_piece(ocb(nc1)) then continue for
                    dcl=abs(colonne(nc1)-colonne(nc2))
                    dlg=abs(ligne(nc1)-ligne(nc2))
                    if dlg>1 then continue for
                    if dcl>1 then continue for
                    cz2=nom_case(nc2)
                    c01=c01+cz1+cz2
                next nc2
            end select
        next ik : sc2(ii,ij)=0 : for ik=0 to 3
            qr2(ii,ij,ik)=1
        next ik : for ik=1 to len(c01)/4
            cz2=mid(c01,3+4*(ik-1),2) : nc2=numero_case(cz2)
            select case as const trait
            case 0
                if ocb(nc2)=rb then sc2(ii,ij)=1
                if sc2(ii,ij)=1 then
                    qr2(ii,ij,2)=0
                    qr2(ii,ij,3)=0
                end if
                if nc2>55 and nc2<60 then qr2(ii,ij,2)=0
                if nc2>60 then qr2(ii,ij,3)=0
            case 1
                if ocb(nc2)=rn then sc2(ii,ij)=1
                if sc2(ii,ij)=1 then
                    qr2(ii,ij,0)=0
                    qr2(ii,ij,1)=0
                end if
                if nc2>04 and nc2<08 then qr2(ii,ij,1)=0
                if nc2<04 then qr2(ii,ij,0)=0
            end select
        next ik
    next ij
next ii