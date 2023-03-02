
for ii=0 to 3' tapis pièces prises
    for ij=0 to 7
        x1=384+40*ii : y1=24+40*ij : trouve_image(1) : for ik=0 to 1599
            read pix(ik) : y2=fix(ik/40) : x2=ik-40*y2
            if pix(ik)=0 then preset(x1+x2,y1+y2) else pset(x1+x2,y1+y2)
        next ik
    next ij
next ii

for ii=0 to 63' dessin 64 cases
    x1=aff*328+(1-2*aff)*(40*colonne(ii)+x0)
    y1=aff*328+(1-2*aff)*(40*ligne(ii)+y0)
    trouve_image(2*occupant(ii)+(couleur_case(ii)-1))
    for ij=0 to 1599
        read pix(ij) : y2=fix(ij/40) : x2=ij-y2*40
        if pix(ij)=0 then preset(x1+x2,y1+y2) else pset(x1+x2,y1+y2)
    next ij
next ii

nouveau :

if nombre_coups>0 then
    for ii=nc5 to nc6 step nc6-nc5' dessin deux cases
        x1=aff*328+(1-2*aff)*(40*colonne(ii)+x0)
        y1=aff*328+(1-2*aff)*(40*ligne(ii)+y0)
        trouve_image(2*occupant(ii)+(couleur_case(ii)-1))
        for ij=0 to 1599
            read pix(ij) : y2=fix(ij/40) : x2=ij-y2*40
            if pix(ij)=0 then preset(x1+x2,y1+y2) else pset(x1+x2,y1+y2)
        next ij
        if edc=1 then
            line (x1,y1)-(x1+39,y1+39),,B
        end if
    next ii
end if

if nombre_coups>0 then
    for ii=nc7 to nc8 step nc8-nc7' roque
        if roque=0 then exit for
        x1=aff*328+(1-2*aff)*(40*colonne(ii)+x0)
        y1=aff*328+(1-2*aff)*(40*ligne(ii)+y0)
        trouve_image(2*occupant(ii)+(couleur_case(ii)-1))
        for ij=0 to 1599
            read pix(ij) : y2=fix(ij/40) : x2=ij-y2*40
            if pix(ij)=0 then preset(x1+x2,y1+y2) else pset(x1+x2,y1+y2)
        next ij
    next ii
end if

if nombre_coups>0 then
    for ii=nc9 to nc9' prise en passant
        if pris_en_passant=0 then exit for
        x1=aff*328+(1-2*aff)*(40*colonne(ii)+x0)
        y1=aff*328+(1-2*aff)*(40*ligne(ii)+y0)
        trouve_image(2*occupant(ii)+(couleur_case(ii)-1))
        for ij=0 to 1599
            read pix(ij) : y2=fix(ij/40) : x2=ij-y2*40
            if pix(ij)=0 then preset(x1+x2,y1+y2) else pset(x1+x2,y1+y2)
        next ij
    next ii
end if

for ii=0 to 63' copie de la position
    pst=pst+chr(occupant(ii))
next ii
pst=pst+chr(trait)+chr(pion_passant)
for ii=0 to 3
    pst=pst+chr(rq(ii))
next ii

repetition=0' comparaison avec les positions précédentes
ij=0 : for ii=0 to nombre_coups-1
    if mid(pst,1+70*ii,70)=right(pst,70) then ij+=1
next ii : if ij=3 then repetition=1' (retour répété à la position)

if prise>0 then
    x1=384+trait*80
    if nature(prise)>1 then
        y1=344-80*(nature(prise)-1)
        if nature(prise)<5 then
            ii=0
            for ij=0 to 63
                if occupant(ij)=prise then ii=1
            next ij
            if ii=0 then y1=y1+40
            y1=y1-40
        end if
    end if
    if nature(prise)=1 then
        x1=x1+40
        ii=0
        for ij=0 to 63
            if occupant(ij)=prise then ii=ii+1
        next ij
        y1=304-ii*40
    end if
    ij=2*prise+1
    trouve_image(ij)
    for ij=0 to 1599
        y2=fix(ij/40) : x2=ij-40*y2 : read pix(ij)
        if pix(ij)=0 then preset(x1+x2,y1+y2)
        if pix(ij)=1 then pset(x1+x2,y1+y2)
    next ij
end if
