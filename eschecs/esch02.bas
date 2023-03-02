dim as integer ii, ij, ik
dim as integer colonne(63), ligne(63)
dim as integer couleur_case(63)
dim nom_case(63) as string
const blanc=1, noir=2

for ii=0 to 63
	ligne(ii)=fix(ii/8) : colonne(ii)=ii-8*ligne(ii)
    nom_case(ii)=chr(colonne(ii)+97)+chr(56-ligne(ii))
    ij=colonne(ii)+ligne(ii)
    if ij/2=fix(ij/2) then couleur_case(ii)=blanc else couleur_case(ii)=noir
next ii

function numero_case(caz as string) as integer
    numero_case=asc(caz,1)-8*asc(caz,2)+351
end function

dim as integer occupant(63), nature(12), couleur_piece(12)

const pb=01, pn=02, cb=03, cn=04, fb=05, fn=06, tb=07, tn=08, db=09
const dn=10, rb=11, rn=12, pio=1, cav=2, fou=3, tou=4, dam=5, roi=6

nature(pb)=pio : nature(pn)=pio : nature(cb)=cav : nature(cn)=cav
nature(fb)=fou : nature(fn)=fou : nature(tb)=tou : nature(tn)=tou
nature(db)=dam : nature(dn)=dam : nature(rb)=roi : nature(rn)=roi

couleur_piece(pb)=blanc : couleur_piece(pn)=noir
couleur_piece(cb)=blanc : couleur_piece(cn)=noir
couleur_piece(fb)=blanc : couleur_piece(fn)=noir
couleur_piece(tb)=blanc : couleur_piece(tn)=noir
couleur_piece(db)=blanc : couleur_piece(dn)=noir
couleur_piece(rb)=blanc : couleur_piece(rn)=noir

dim as integer trait, schach, nombre_coups
dim as integer rq(3), qr(3), pion_passant

dim as string pst
dim as integer repetition
dim as string ps1, ps2, c01, c02, cz1, cz2
dim as integer nc1, nc2, dcl, dlg, dis, pas, nps
dim as string c03, c04, c05
dim as integer ocb(63), hcs
dim as string ps3, ps4, c07
dim as string ps5, ps50, c050, ps30, ps40
dim as integer oc0(63), schach0, rq0(3)
dim as integer ji, jj, jk, ki, kj, kk, ll, lm, ln, ml, mm, mn, nl, nm, nn
dim as single n01, n02, n03, n04, n05, n06, n07, n08, n09
dim as single n10, n11, n12, n13, n14, n15, n16, n17, n18
dim as single n19, n20, n21, n22, n23, n24, n25, n26, n27
dim as single n28, n29, n30, n31, n32, n98
dim as integer nc3, nc4, nc5, nc6, nc7, nc8, nc9
dim as string cz3, cz4, cz5, cz6, c08, c09, c10, c11, c12
dim as byte jeu=0, fmt(3)
dim as byte pris_en_passant, roque, prise
dim as string coups_naturels_blanc, coups_naturels_noir
dim as integer diff_coups_naturels(63)

const x0=24, y0=24
dim as integer x1, x2, y1, y2, pix(1599)

dim as byte aff=0, crd=1, edc=0
const av3="ESCHEC"
const av4="PAT"
const av5="ESCHEC ET MAT"
const av6="PARTIE REMISE"
dim as string rps

const c1=rgb(239,239,239)
const c2=rgb(000,000,063)
const c3=rgb(151,000,000)
const c4=rgb(223,223,223)

screenres 568,426,16
windowtitle "ESCHECS 0.2"
color c4,c1
cls

' grillage
dim as integer i
for i=0 to 87
    line (8*i,0)-(8*i,527)
    line (8*i+4,0)-(8*i+4,527)
next i
for i=0 to 65
    line (0,8*i)-(703,8*i)
    line (0,8*i+4)-(703,8*i+4)
next i

' bordure
color c2
line (0,0)-(567,425),,b

' cadres
line (0,0)-(703,527),,b
for i=1 to 4
    color c2
    if i=2 then color c1
    line (024-i,024-i)-(343+i,343+i),,b' fenêtre tablier
    line (384-i,024-i)-(543+i,343+i),,b' fenêtre prises
    line (024-i,376-i)-(143+i,399+i),,b' fenêtre avis
    line (296-i,376-i)-(343+i,399+i),,b' fenêtre coup
    line (384-i,376-i)-(407+i,399+i),,b' bouton 1
    line (420-i,376-i)-(443+i,399+i),,b' bouton 2
    line (456-i,376-i)-(479+i,399+i),,b' bouton 3
    line (520-i,376-i)-(543+i,399+i),,b' bouton 4
next i

' voyants
draw string (352,24),chr(219)
draw string (352,336),chr(219)
color c3
draw string (352,32+2),chr(219)
draw string (352,328-2),chr(219)
color c2
draw string (492-4,376-2),chr(219)
draw string (492-4,384),chr(219)
draw string (492-4,392+2),chr(219)

sub efface_rectangle(x as integer, y as integer, hau as integer, lar as integer)
    dim as integer compteur
    color c1
    for compteur=1 to lar
        line (x+compteur-1,y)-(x+compteur-1,y+hau-1)
    next compteur
    color c2
end sub

sub efface_rectangle_bis(x as integer, y as integer, hau as integer, lar as integer)
    dim as integer compteur
    color c3
    for compteur=1 to lar
        line (x+compteur-1,y)-(x+compteur-1,y+hau-1)
    next compteur
    color c2
end sub

efface_rectangle(24,24,320,320)
efface_rectangle(384,24,320,160)
efface_rectangle(24,376,24,120)
efface_rectangle(296,376,24,48)
efface_rectangle(384,376,24,24)
efface_rectangle(420,376,24,24)
efface_rectangle(456,376,24,24)
efface_rectangle_bis(520,376,24,24)

draw string (384,392),chr(1)+chr(1)+chr(1)' bouton 1
draw string (392+1,380),"n"
draw string (420,392),chr(2)+chr(2)+chr(2)' bouton 2
draw string (428+1,380),"n"
draw string (460,380),"au"' bouton 3
draw string (460,388),"to"
color c1
draw string (528,385),chr(26)' bouton 4
color c2

sub trouve_image(nfg as integer)
    if nfg=00 then restore fg00 : end if : if nfg=01 then restore fg01
    if nfg=02 then restore fg02 : end if : if nfg=03 then restore fg03
    if nfg=04 then restore fg04 : end if : if nfg=05 then restore fg05
    if nfg=06 then restore fg06 : end if : if nfg=07 then restore fg07
    if nfg=08 then restore fg08 : end if : if nfg=09 then restore fg09
    if nfg=10 then restore fg10 : end if : if nfg=11 then restore fg11
    if nfg=12 then restore fg12 : end if : if nfg=13 then restore fg13
    if nfg=14 then restore fg14 : end if : if nfg=15 then restore fg15
    if nfg=16 then restore fg16 : end if : if nfg=17 then restore fg17
    if nfg=18 then restore fg18 : end if : if nfg=19 then restore fg19
    if nfg=20 then restore fg20 : end if : if nfg=21 then restore fg21
    if nfg=22 then restore fg22 : end if : if nfg=23 then restore fg23
    if nfg=24 then restore fg24 : end if : if nfg=25 then restore fg25
end sub
sub efface_ligne(uu as integer,uv as integer,uw as integer)
    dim vu as string, vv as integer
    vu="" : for vv=1 to uw
        vu=vu+chr(219)
    next vv
    color c1 : draw string (uu,uv),vu : color c2
end sub

dim as integer x, y, buttons, res' souris

nouvelle :

' coordonnées
for i=0 to 7
    color c1
    draw string (i*40+40,8+1), chr(219)
    draw string (8,i*40+41), chr(219)
    color c2
    select case as const aff
    case 0
        draw string (i*40+40,8+1),chr(97+i)
        draw string (8,i*40+41),chr(56-i)
    case 1
        draw string (i*40+40,8+1),chr(104-i)
        draw string (8,i*40+41),chr(49+i)        
    end select
next i

' voyants mode de jeu
color c1
draw string (488,374),chr(254)
draw string (488,384),chr(254)
draw string (488,394),chr(254)
color c2
if jeu=0 then draw string (488,374),chr(219)
if jeu=1 then draw string (488,384),chr(219)
if jeu=2 then draw string (488,394),chr(219)

occupant(00)=tn : occupant(01)=cn : occupant(02)=fn : occupant(03)=dn
occupant(04)=rn : occupant(05)=fn : occupant(06)=cn : occupant(07)=tn
occupant(08)=pn : occupant(09)=pn : occupant(10)=pn : occupant(11)=pn
occupant(12)=pn : occupant(13)=pn : occupant(14)=pn : occupant(15)=pn
occupant(16)=00 : occupant(17)=00 : occupant(18)=00 : occupant(19)=00
occupant(20)=00 : occupant(21)=00 : occupant(22)=00 : occupant(23)=00
occupant(24)=00 : occupant(25)=00 : occupant(26)=00 : occupant(27)=00
occupant(28)=00 : occupant(29)=00 : occupant(30)=00 : occupant(31)=00
occupant(32)=00 : occupant(33)=00 : occupant(34)=00 : occupant(35)=00
occupant(36)=00 : occupant(37)=00 : occupant(38)=00 : occupant(39)=00
occupant(40)=00 : occupant(41)=00 : occupant(42)=00 : occupant(43)=00
occupant(44)=00 : occupant(45)=00 : occupant(46)=00 : occupant(47)=00
occupant(48)=pb : occupant(49)=pb : occupant(50)=pb : occupant(51)=pb
occupant(52)=pb : occupant(53)=pb : occupant(54)=pb : occupant(55)=pb
occupant(56)=tb : occupant(57)=cb : occupant(58)=fb : occupant(59)=db
occupant(60)=rb : occupant(61)=fb : occupant(62)=cb : occupant(63)=tb

for ii=0 to 3
    rq(ii)=1
next ii
c11=""
pst=""
trait=0
nombre_coups=0