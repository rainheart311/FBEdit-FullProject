if inkey=chr(27) then end

demande :

c10=""
for ii=0 to 1
    do
        if trait=0 and jeu=1 then
            c10=c09 : exit do
        end if
        if trait=1 and jeu=0 then
            c10=c09 : exit do
        end if
        if jeu=2 then
            c10=c09 : exit do
        end if
        if inkey=chr(27) then end
        res=getmouse(x,y,,buttons)
        if res=0 then
            if buttons=1 then
                if x>23 and x<344 then
                    if y>23 and y<344 then
                        ij=fix((x-24)/40)+8*fix((y-24)/40)
                        if aff=1 then ij=63-ij
                        c10=c10+nom_case(ij)
                        exit do
                    end if
                end if
                if y>375 and y<400 then
                    if x>383 and x<408 then
                        jeu=0 : aff=0 : goto nouvelle
                    end if
                    if x>419 and x<444 then
                        jeu=1 : aff=1 : goto nouvelle
                    end if                
                    if x>455 and x<480 then
                        color c1
                        draw string (488,374),chr(254)
                        draw string (488,384),chr(254)
                        color c2
                        draw string (488,394),chr(219)
                        jeu=2
                        goto demande
                    end if                 
                    if x>519 and x<544 then
                        end
                    end if
                end if
            end if
        end if
    loop
    do
        if c10=c09 then exit do
        res=getmouse(x,y,,buttons)
    loop until buttons=0
    draw string (304,385),c10
next ii

ii=0 : for ij=1 to len(c050)/4
    if c10=mid(c050,1+4*(ij-1),4) then ii=1
next ij
if ii=0 then
    efface_ligne(304,385,4)
    goto demande
end if

c11=c11+c10
