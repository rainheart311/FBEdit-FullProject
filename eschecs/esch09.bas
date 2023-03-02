redim as string c06(1 to len(c05)/4)
for ii=1 to len(c05)/4
	ps2="" : for ij=0 to 63
        if trait=0 and couleur_piece(oc1(ii,ij))=noir then ps2=ps2+nom_case(ij)
        if trait=1 and couleur_piece(oc1(ii,ij))=blanc then ps2=ps2+nom_case(ij)
    next ij : c06(ii)="" : for ij=1 to len(ps2)/2
        cz1=mid(ps2,1+2*(ij-1),2) : nc1=numero_case(cz1)
        select case as const nature(oc1(ii,nc1))
        case 1
            select case as const trait
            case 0
                for nc2=0 to 63
                    if couleur_piece(oc1(ii,nc2))=noir then continue for
                    if ligne(nc2)<ligne(nc1)+1 or ligne(nc2)>ligne(nc1)+2 then continue for
                    dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))                
                    if oc1(ii,nc2)=0 then
                        if dcl>0 then continue for
                        if oc1(ii,nc1+8)>0 then continue for
                        if ligne(nc1)>1 and dlg>1 then continue for
                    end if
                    if oc1(ii,nc2)>0 then
                        if dlg>1 then continue for
                        if dcl<>1 then continue for
                    end if : cz2=nom_case(nc2) : c06(ii)=c06(ii)+cz1+cz2
                next nc2
                if pion_passant>0 then' à modifier
                    if ligne(nc1)=4 then
                        if colonne(nc1)>0 then
                            if occupant(nc1-1)=pb then
                                'if nom_case(nc1-1+16)+nom_case(nc1-1)=c13 then
                                    'cz2=nom_case(nc1-1+8)
                                    'c06(ii)=c06(ii)+cz1+cz2
                                'end if
                            end if
                        end if
                        if colonne(nc1)<7 then
                            if occupant(nc1+1)=pb then
                                'if nom_case(nc1+1+16)+nom_case(nc1+1)=c13 then
                                    'cz2=nom_case(nc1+1+8)
                                    'c06(ii)=c06(ii)+cz1+cz2
                                'end if
                            end if
                        end if
                    end if
                end if
            case 1
                for nc2=0 to 63
                    if couleur_piece(oc1(ii,nc2))=blanc then continue for
                    if ligne(nc2)>ligne(nc1)-1 or ligne(nc2)<ligne(nc1)-2 then continue for
                    dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))                
                    if oc1(ii,nc2)=0 then
                        if dcl>0 or oc1(ii,nc1-8)>0 then continue for
                        if ligne(nc1)<6 and dlg>1 then continue for
                    end if
                    if oc1(ii,nc2)>0 then
                        if dlg>1 or dcl<>1 then continue for
                    end if : cz2=nom_case(nc2) : c06(ii)=c06(ii)+cz1+cz2
                next nc2
                if pion_passant>0 then
                    if ligne(nc1)=3 then
                        if colonne(nc1)>0 then
                            if occupant(nc1-1)=pn then
                                'if nom_case(nc1-1-16)+nom_case(nc1-1)=c13 then
                                    'cz2=nom_case(nc1-1-8)
                                    'c06(ii)=c06(ii)+cz1+cz2
                                'end if
                            end if
                        end if
                        if colonne(nc1)<7 then
                            if occupant(nc1+1)=pn then
                                'if nom_case(nc1+1-16)+nom_case(nc1+1)=c13 then
                                    'cz2=nom_case(nc1+1-8)
                                    'c06(ii)=c06(ii)+cz1+cz2
                                'end if
                            end if
                        end if
                    end if
                end if
            end select
        case 2
            for nc2=0 to 63
                if couleur_piece(oc1(ii,nc2))=couleur_piece(oc1(ii,nc1)) then continue for              
                dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))
                if dcl*dlg=0 or dcl+dlg<>3 then continue for
                cz2=nom_case(nc2) : c06(ii)=c06(ii)+cz1+cz2
            next nc2
        case 3
            for nc2=0 to 63
                if couleur_piece(oc1(ii,nc2))=couleur_piece(oc1(ii,nc1)) then continue for                  
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
                end if : cz2=nom_case(nc2) : c06(ii)=c06(ii)+cz1+cz2
            next nc2
        case 4
            for nc2=0 to 63
                if couleur_piece(oc1(ii,nc2))=couleur_piece(oc1(ii,nc1)) then continue for                
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
                end if : cz2=nom_case(nc2) : c06(ii)=c06(ii)+cz1+cz2
            next nc2
        case 5
            for nc2=0 to 63
                if couleur_piece(oc1(ii,nc2))=couleur_piece(oc1(ii,nc1)) then continue for                 
                dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))
                if dcl<>dlg and dcl*dlg>0 then continue for
                if dcl>1 or dlg>1 then
                    dis=nc2-nc1 : pas=9
                    if abs(dis/pas)<>fix(abs(dis/pas)) then pas=8
                    if abs(dis/pas)<>fix(abs(dis/pas)) then pas=7
                    if abs(dis/pas)<>fix(abs(dis/pas)) then pas=1
                    if dis*2<dis then pas=0-pas
                    nps=dis/pas : ik=1 : do
                        if oc1(ii,nc1+ik*pas)>0 then continue for
                        ik=ik+1
                    loop until ik=nps
                end if : cz2=nom_case(nc2) : c06(ii)=c06(ii)+cz1+cz2
            next nc2
        case 6
            for nc2=0 to 63
                if couleur_piece(oc1(ii,nc2))=couleur_piece(oc1(ii,nc1)) then continue for              
                dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))
                if dcl>2 or dlg>1 then continue for
                select case as const trait
                case 0
                    if nc1=60 and nc2=58 then
                        if oc1(ii,59)=0 and oc1(ii,57)=0 and oc1(ii,56)=7 then
                            if rq(2)=1 and qr1(ii,2)=1 then
                                cz2=nom_case(nc2) : c06(ii)=c06(ii)+cz1+cz2
                                continue for
                            end if : continue for
                        end if
                    end if
                    if nc1=60 and nc2=62 then
                        if oc1(ii,61)=0 and oc1(ii,63)=7 then
                            if rq(3)=1 and qr1(ii,3)=1 then
                                cz2=nom_case(nc2) : c06(ii)=c06(ii)+cz1+cz2
                                continue for
                            end if : continue for
                        end if
                    end if
                case 1
                    if nc1=4 and nc2=2 then
                        if oc1(ii,3)=0 and oc1(ii,1)=0 and oc1(ii,0)=8 then
                            if rq(0)=1 and qr1(ii,0)=1 then
                                cz2=nom_case(nc2) : c06(ii)=c06(ii)+cz1+cz2
                                continue for
                            end if : continue for
                        end if
                    end if
                    if nc1=4 and nc2=6 then
                        if oc1(ii,5)=0 and oc1(ii,7)=8 then
                            if rq(1)=1 and qr1(ii,1)=1 then
                                cz2=nom_case(nc2) : c06(ii)=c06(ii)+cz1+cz2
                                continue for
                            end if : continue for
                        end if
                    end if
                end select
                if dcl>1 then continue for
                cz2=nom_case(nc2) : c06(ii)=c06(ii)+cz1+cz2
            next nc2
        end select
    next ij   
next ii