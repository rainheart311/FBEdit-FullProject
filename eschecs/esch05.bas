ps1="" : for ii=0 to 63
    if trait=0 and couleur_piece(occupant(ii))=noir then ps1=ps1+nom_case(ii)
    if trait=1 and couleur_piece(occupant(ii))=blanc then ps1=ps1+nom_case(ii)
next ii : c01="" : for ii=1 to len(ps1)/2
    cz1=mid(ps1,1+2*(ii-1),2) : nc1=numero_case(cz1)
    select case as const nature(occupant(nc1))
    case pio
        select case as const trait
        case 0
            for nc2=0 to 63
                if couleur_piece(occupant(nc2))=noir then continue for
                if ligne(nc2)<ligne(nc1)+1 or ligne(nc2)>ligne(nc1)+2 then continue for
                dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))
                if occupant(nc2)=0 then
                    if dcl>0 or occupant(nc1+8)>0 then continue for
                    if ligne(nc1)>1 and dlg>1 then continue for
                end if : if occupant(nc2)>0 then
                    if dlg>1 or dcl<>1 then continue for
                end if : cz2=nom_case(nc2) : c01=c01+cz1+cz2
            next nc2
        case 1
            for nc2=0 to 63
                if couleur_piece(occupant(nc2))=blanc then continue for
                if ligne(nc2)>ligne(nc1)-1 or ligne(nc2)<ligne(nc1)-2 then continue for
                dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))                
                if occupant(nc2)=0 then
                    if dcl>0 or occupant(nc1-8)>0 then continue for
                    if ligne(nc1)<6 and dlg>1 then continue for
                end if : if occupant(nc2)>0 then
                    if dlg>1 or dcl<>1 then continue for
                end if : cz2=nom_case(nc2) : c01=c01+cz1+cz2
            next nc2
        end select
    case 2
        for nc2=0 to 63
            if couleur_piece(occupant(nc2))=couleur_piece(occupant(nc1)) then continue for
            dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))
            if dcl*dlg=0 or dcl+dlg<>3 then continue for
            cz2=nom_case(nc2) : c01=c01+cz1+cz2
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
            end if : cz2=nom_case(nc2) : c01=c01+cz1+cz2
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
            end if : cz2=nom_case(nc2) : c01=c01+cz1+cz2
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
            end if : cz2=nom_case(nc2) : c01=c01+cz1+cz2
        next nc2
    case 6
    end select
next ii
schach=0 : for ii=0 to 3
    qr(ii)=1
next ii : ps3="" : ps4="" : for ii=1 to len(c01)/4
    nc2=numero_case(mid(c01,3+4*(ii-1),2)) : select case as const trait
    case 0
        if occupant(nc2)=rb then
            schach=1 : qr(2)=0 : qr(3)=0 : ps3=ps3+mid(c01,1+4*(ii-1),2)
        end if : if nc2>55 and nc2<60 then
            qr(2)=0 : ps4=ps4+mid(c01,1+4*(ii-1),2)
        end if : if nc2>60 then
            qr(3)=0 : ps4=ps4+mid(c01,1+4*(ii-1),2)
        end if
    case 1
        if occupant(nc2)=rn then
            schach=1 : qr(0)=0 : qr(1)=0 : ps3=ps3+mid(c01,1+4*(ii-1),2)
        end if : if nc2>04 and nc2<08 then
            qr(1)=0 : ps4=ps4+mid(c01,1+4*(ii-1),2)
        end if : if nc2<04 then
            qr(0)=0 : ps4=ps4+mid(c01,1+4*(ii-1),2)
        end if
    end select
next ii