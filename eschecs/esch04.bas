coups_naturels_blanc="" : coups_naturels_noir="" : for ii=0 to 63
    select case as const nature(occupant(ii))
    case pio
        select case as const occupant(ii)
        case pb
            if colonne(ii)>0 then
                coups_naturels_blanc=coups_naturels_blanc+nom_case(ii-9)
            end if
            if colonne(ii)<7 then
                coups_naturels_blanc=coups_naturels_blanc+nom_case(ii-7)
            end if
        case pn
            if colonne(ii)<7 then
                coups_naturels_noir=coups_naturels_noir+nom_case(ii+9)
            end if
            if colonne(ii)>0 then
                coups_naturels_noir=coups_naturels_noir+nom_case(ii+7)
            end if            
        end select
    case cav
        if colonne(ii)<7 and ligne(ii)>1 then
            if occupant(ii)=cb then coups_naturels_blanc=coups_naturels_blanc+nom_case(ii-15)
            if occupant(ii)=cn then coups_naturels_noir=coups_naturels_noir+nom_case(ii-15)
        end if
        if colonne(ii)<6 and ligne(ii)>0 then
            if occupant(ii)=cb then coups_naturels_blanc=coups_naturels_blanc+nom_case(ii-06)
            if occupant(ii)=cn then coups_naturels_noir=coups_naturels_noir+nom_case(ii-06)
        end if
        if colonne(ii)<6 and ligne(ii)<7 then
            if occupant(ii)=cb then coups_naturels_blanc=coups_naturels_blanc+nom_case(ii+10)
            if occupant(ii)=cn then coups_naturels_noir=coups_naturels_noir+nom_case(ii+10)
        end if
        if colonne(ii)<7 and ligne(ii)<6 then
            if occupant(ii)=cb then coups_naturels_blanc=coups_naturels_blanc+nom_case(ii+17)
            if occupant(ii)=cn then coups_naturels_noir=coups_naturels_noir+nom_case(ii+17)
        end if
        if colonne(ii)>0 and ligne(ii)<6 then
            if occupant(ii)=cb then coups_naturels_blanc=coups_naturels_blanc+nom_case(ii+15)
            if occupant(ii)=cn then coups_naturels_noir=coups_naturels_noir+nom_case(ii+15)
        end if
        if colonne(ii)>1 and ligne(ii)<7 then
            if occupant(ii)=cb then coups_naturels_blanc=coups_naturels_blanc+nom_case(ii+06)
            if occupant(ii)=cn then coups_naturels_noir=coups_naturels_noir+nom_case(ii+06)
        end if
        if colonne(ii)>1 and ligne(ii)>0 then
            if occupant(ii)=cb then coups_naturels_blanc=coups_naturels_blanc+nom_case(ii-10)
            if occupant(ii)=cn then coups_naturels_noir=coups_naturels_noir+nom_case(ii-10)
        end if
        if colonne(ii)>0 and ligne(ii)>1 then
            if occupant(ii)=cb then coups_naturels_blanc=coups_naturels_blanc+nom_case(ii-17)
            if occupant(ii)=cn then coups_naturels_noir=coups_naturels_noir+nom_case(ii-17)
        end if
    case fou
        for ij=0 to 63
            dcl=abs(colonne(ii)-colonne(ij)) : dlg=abs(ligne(ii)-ligne(ij))
            if dcl=dlg and ij<>ii then
                if dcl=1 and occupant(ii)=fb then coups_naturels_blanc=coups_naturels_blanc+nom_case(ij)
                if dcl=1 and occupant(ii)=fn then coups_naturels_noir=coups_naturels_noir+nom_case(ij)
                if dcl>1 then
                    dis=ij-ii : pas=7
                    if abs(dis/7)<>fix(abs(dis/7)) then pas=9
                    if 2*dis<dis then pas=0-pas
                    nps=dis/pas : ik=1 : do
                        if occupant(ii+ik*pas)>0 then exit do
                        ik=ik+1
                    loop until ik=nps
                    if ik=nps and occupant(ii)=fb then coups_naturels_blanc=coups_naturels_blanc+nom_case(ij)
                    if ik=nps and occupant(ii)=fn then coups_naturels_noir=coups_naturels_noir+nom_case(ij)
                end if
            end if
        next ij
    case tou
        for ij=0 to 63
            dcl=abs(colonne(ii)-colonne(ij)) : dlg=abs(ligne(ii)-ligne(ij))
            if dcl*dlg=0 and ij<>ii then
                if dcl+dlg=1 and occupant(ii)=tb then coups_naturels_blanc=coups_naturels_blanc+nom_case(ij)
                if dcl+dlg=1 and occupant(ii)=tn then coups_naturels_noir=coups_naturels_noir+nom_case(ij)
                if dcl>1 or dlg>1 then
                    dis=ij-ii : pas=8
                    if abs(dis/8)<>fix(abs(dis/8)) then pas=1
                    if dis*2<dis then pas=0-pas
                    nps=dis/pas : ik=1 : do
                        if occupant(ii+ik*pas)>0 then exit do
                        ik=ik+1
                    loop until ik=nps
                    if ik=nps and occupant(ii)=tb then coups_naturels_blanc=coups_naturels_blanc+nom_case(ij)
                    if ik=nps and occupant(ii)=tn then coups_naturels_noir=coups_naturels_noir+nom_case(ij)
                end if
            end if
        next ij        
    case dam
        for ij=0 to 63
            if ij=ii then continue for
            dcl=abs(colonne(ii)-colonne(ij)) : dlg=abs(ligne(ii)-ligne(ij))
            if dcl<>dlg and dcl*dlg>0 then continue for
            if dcl<2 and dlg<2 then
                if occupant(ii)=db then coups_naturels_blanc=coups_naturels_blanc+nom_case(ij)
                if occupant(ii)=dn then coups_naturels_noir=coups_naturels_noir+nom_case(ij)                
            end if
            if dcl>1 or dlg>1 then
                dis=ij-ii : pas=9
                if abs(dis/pas)<>fix(abs(dis/pas)) then pas=8
                if abs(dis/pas)<>fix(abs(dis/pas)) then pas=7
                if abs(dis/pas)<>fix(abs(dis/pas)) then pas=1
                if dis*2<dis then pas=0-pas
                nps=dis/pas : ik=1 : do
                    if occupant(ii+ik*pas)>0 then exit do
                    ik=ik+1
                loop until ik=nps
                if ik=nps and occupant(ii)=db then coups_naturels_blanc=coups_naturels_blanc+nom_case(ij)
                if ik=nps and occupant(ii)=dn then coups_naturels_noir=coups_naturels_noir+nom_case(ij)
            end if
        next ij        
    case roi
        for ij=0 to 63
            if ij=ii then continue for
            dcl=abs(colonne(ii)-colonne(ij)) : dlg=abs(ligne(ii)-ligne(ij))
            if dcl<2 and dlg<2 then
                if occupant(ii)=rb then coups_naturels_blanc=coups_naturels_blanc+nom_case(ij)
                if occupant(ii)=rn then coups_naturels_noir=coups_naturels_noir+nom_case(ij)                
            end if
        next ij        
    end select
next ii
for ii=0 to 63
    diff_coups_naturels(ii)=0
next ii
for ii=1 to len(coups_naturels_blanc)/2
    nc2=numero_case(mid(coups_naturels_blanc,1+2*(ii-1),2))
    diff_coups_naturels(nc2)=diff_coups_naturels(nc2)+1
next ii
for ii=1 to len(coups_naturels_noir)/2
    nc2=numero_case(mid(coups_naturels_noir,1+2*(ii-1),2))
    diff_coups_naturels(nc2)=diff_coups_naturels(nc2)-1
next ii