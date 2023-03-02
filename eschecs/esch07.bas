efface_ligne(32,385,13)
efface_ligne(304,385,4)

color c1 : draw string (352,32+2),chr(254)' éteindre voyants rouges
draw string (352,328-2),chr(254) : color c2

if schach=1 and len(c05)>0 then' échec
    color c3 : draw string (32,385),av3
    if aff=1 and trait=1 then draw string (352,328-2),chr(219)
    if aff=1 and trait=0 then draw string (352,32+2),chr(219)
    if aff=0 and trait=0 then draw string (352,328-2),chr(219)
    if aff=0 and trait=1 then draw string (352,32+2),chr(219) : color c2
end if

if len(c05)=0 and schach=0 then' pat
    color c3
    draw string (32,385),av4
    draw string (352,328-2),chr(219)
    draw string (352,32+2),chr(219)
    color c1 : draw string (352,040),chr(254)
    draw string (352,352),chr(254)
    color c2 : jeu=0
    do
        res=getmouse(x,y,,buttons)
        if res=0 and buttons=1 then
            if y>375 and y<400 then
                if x>383 and x<408 then
                    jeu=0 : aff=0 : goto nouvelle
                end if
                if x>419 and x<444 then
                    jeu=1 : aff=1 : goto nouvelle
                end if                
                if x>455 and x<480 then
                    jeu=2 : aff=0 : goto nouvelle
                end if                 
                if x>519 and x<544 then
                    end
                end if
            end if
        end if
    loop until buttons=1 or len(inkey)>0
    goto nouvelle
end if

if len(c05)=0 and schach=1 then' mat
    color c3 : draw string (32,385),av5
    if aff=1 and trait=1 then draw string (352,328-2),chr(219)
    if aff=1 and trait=0 then draw string (352,32+2),chr(219)        
    if aff=0 and trait=0 then draw string (352,328-2),chr(219)
    if aff=0 and trait=1 then draw string (352,32+2),chr(219)
    color c1 : draw string (352,040),chr(254)
    draw string (352,352),chr(254)
    color c2 : jeu=0
    do
        res=getmouse(x,y,,buttons)
        if res=0 and buttons=1 then
            if y>375 and y<400 then
                if x>383 and x<408 then
                    jeu=0 : aff=0 : goto nouvelle
                end if
                if x>419 and x<444 then
                    jeu=1 : aff=1 : goto nouvelle
                end if                
                if x>455 and x<480 then
                    jeu=2 : aff=0 : goto nouvelle
                end if                 
                if x>519 and x<544 then
                    end
                end if
            end if
        end if
    loop until buttons=1 or len(inkey)>0
    goto nouvelle
end if

if repetition=1 then' partie remise (retour répété à une même position)
    if schach=1 and len(c05)>0 then
        efface_ligne(32,385,6)
    end if
    color c3 : draw string (32,385),av6
    draw string (352,326),chr(219)
    draw string (352,34),chr(219)
    color c1 : draw string (352,24),chr(254)
    draw string (352,336),chr(254)
    color c2 : jeu=0
    do
        res=getmouse(x,y,,buttons)
        if res=0 and buttons=1 then
            if y>375 and y<400 then
                if x>383 and x<408 then
                    jeu=0 : aff=0 : goto nouvelle
                end if
                if x>419 and x<444 then
                    jeu=1 : aff=1 : goto nouvelle
                end if                
                if x>455 and x<480 then
                    jeu=2 : aff=0 : goto nouvelle
                end if                 
                if x>519 and x<544 then
                    end
                end if
            end if
        end if
    loop until buttons=1 or len(inkey)>0
    goto nouvelle
end if

if aff=1 then
    select case as const trait
    case 1
        color c1 : draw string (352,024),chr(254)
        color c2 : draw string (352,336),chr(219)
    case 0
        color c1 : draw string (352,336),chr(254)
        color c2 : draw string (352,024),chr(219)
    end select    
else
    select case as const trait
    case 0
        color c1 : draw string (352,024),chr(254)
        color c2 : draw string (352,336),chr(219)
    case 1
        color c1 : draw string (352,336),chr(254)
        color c2 : draw string (352,024),chr(219)
    end select
end if