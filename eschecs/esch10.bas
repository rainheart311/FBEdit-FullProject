
redim as string ps8(1 to len(c05)/4)

for ii=1 to len(c05)/4 : c07="" : for ij=1 to len(c06(ii))/4
        cz1=mid(c06(ii),1+4*(ij-1),2) : cz2=mid(c06(ii),3+4*(ij-1),2) : c03=cz1+cz2
        nc1=numero_case(cz1) : nc2=numero_case(cz2) : dcl=abs(colonne(nc1)-colonne(nc2))
        dlg=abs(ligne(nc1)-ligne(nc2)) : for ik=0 to 63
            ocb(ik)=oc1(ii,ik)
        next ik : if nature(oc1(ii,nc1))=pio then
            if dcl=1 and oc1(ii,nc2)=0 then
                if trait=0 then ocb(nc2-8)=0
                if trait=1 then ocb(nc2+8)=0
            end if
        end if : ocb(nc2)=ocb(nc1) : ocb(nc1)=0
        ps1="" : for ik=0 to 63
                if trait=0 and couleur_piece(ocb(ik))=blanc then ps1=ps1+nom_case(ik)
                if trait=1 and couleur_piece(ocb(ik))=noir then ps1=ps1+nom_case(ik)
            next ik : c02="" : for ik=1 to len(ps1)/2
            cz1=mid(ps1,1+2*(ik-1),2) : nc1=numero_case(cz1)
            select case as const nature(ocb(nc1))
            case 1
                select case as const trait
                case 0
                    for nc2=0 to 63
                        if nature(ocb(nc2))<roi then continue for
                        if ligne(nc2)>ligne(nc1)-1 then continue for
                        if ligne(nc2)<ligne(nc1)-2 then continue for
                        dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))                
                        if dlg>1 or dcl<>1 then continue for
                        cz2=nom_case(nc2) : c02=c02+cz1+cz2
                    next nc2
                case 1
                    for nc2=0 to 63
                        if nature(ocb(nc2))<roi then continue for
                        if ligne(nc2)<ligne(nc1)+1 then continue for
                        if ligne(nc2)>ligne(nc1)+2 then continue for
                        dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))
                        if dlg>1 or dcl<>1 then continue for
                        cz2=nom_case(nc2) : c02=c02+cz1+cz2
                    next nc2
                end select
            case 2
                for nc2=0 to 63
                    if nature(ocb(nc2))<roi then continue for
                    dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))
                    if dcl*dlg=0 or dcl+dlg<>3 then continue for
                    cz2=nom_case(nc2) : c02=c02+cz1+cz2
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
                        nps=dis/pas : ji=1 : do
                            if ocb(nc1+ji*pas)>0 then continue for
                            ji=ji+1
                        loop until ji=nps
                    end if : cz2=nom_case(nc2) : c02=c02+cz1+cz2
                next nc2
            case 4
                for nc2=0 to 63
                    if nature(ocb(nc2))<roi then continue for
                    dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))            
                    if dcl*dlg>0 then continue for
                    if dcl+dlg>1 then
                        dis=nc2-nc1
                        pas=8
                        if abs(dis/8)<>fix(abs(dis/8)) then pas=1
                        if dis*2<dis then pas=0-pas
                        nps=dis/pas : ji=1 : do
                            if ocb(nc1+ji*pas)>0 then continue for
                            ji=ji+1
                        loop until ji=nps
                    end if : cz2=nom_case(nc2) : c02=c02+cz1+cz2
                next nc2
            case 5
                for nc2=0 to 63
                    if nature(ocb(nc2))<roi then continue for
                    dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))            
                    if dcl<>dlg and dcl*dlg>0 then continue for
                    if dcl>1 or dlg>1 then
                        dis=nc2-nc1 : pas=9
                        if abs(dis/pas)<>fix(abs(dis/pas)) then pas=8
                        if abs(dis/pas)<>fix(abs(dis/pas)) then pas=7
                        if abs(dis/pas)<>fix(abs(dis/pas)) then pas=1
                        if dis*2<dis then pas=0-pas
                        nps=dis/pas : ji=1 : do
                            if ocb(nc1+ji*pas)>0 then continue for
                            ji=ji+1
                        loop until ji=nps
                    end if : cz2=nom_case(nc2) : c02=c02+cz1+cz2
                next nc2
            case 6
                for nc2=0 to 63
                    if nature(ocb(nc2))<roi then continue for                   
                    if nc2=nc1 then continue for            
                    dcl=abs(colonne(nc1)-colonne(nc2)) : dlg=abs(ligne(nc1)-ligne(nc2))            
                    if dcl>1 or dlg>1 then continue for            
                    cz2=nom_case(nc2) : c02=c02+cz1+cz2
                next nc2        
            end select
        next ik : hcs=0 : ps8(ii)="" : for ik=1 to len(c02)/4
            cz2=mid(c02,3+4*(ik-1),2) : nc2=numero_case(cz2)
            if trait=0 and ocb(nc2)=rn then
                hcs=1 : ps8(ii)=ps8(ii)+mid(c02,1+4*(ik-1),2)
            end if
            if trait=1 and ocb(nc2)=rb then
                hcs=1 : ps8(ii)=ps8(ii)+mid(c02,1+4*(ik-1),2)
            end if
        next ik : if hcs=0 then c07=c07+c03
    next ij : c06(ii)=c07
next ii