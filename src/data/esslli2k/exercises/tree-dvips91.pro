%!
TeXDict begin /@beginspec{gsave Resolution 72 div VResolution -72 div scale
treedict begin}def /@endspec{end grestore}def /treedict 200 dict def treedict
begin /pt{72 mul 72.07 div}def /nodemargin 2 def /nodes 100 dict def /node{
/dpth exch def /hght exch def /wdth exch def 4 dict dup begin /x /y
currentpoint dpth sub nodemargin sub exch 3 1 roll def nodemargin sub def /h
hght dpth add nodemargin dup add add def /w wdth nodemargin dup add add def
end nodes 3 1 roll put}def /nodebottom{begin x w 2 div add y end}def /nodetop{
begin x w 2 div add y h add end}def /nodeleft{begin x y h 2 div add end}def
/noderight{begin x w add y h 2 div add end}def /nodetopleft{begin x y h add
end}def /nodetopright{begin x w add y h add end}def /nodebottomleft{begin x y
end}def /nodebottomright{begin x w add y end}def /farright{begin x w add depth
add h 2 div y add end}def /farleft{begin x depth sub h 2 div y add end}def
/farbottom{begin x w 2 div add y depth sub end}def /fartop{begin x w 2 div add
y h add depth add end}def /farbottomleft{begin x depth 45 cos mul sub y depth
45 sin mul sub end}def /farbottomright{begin x w add depth 45 cos mul add y
depth 45 sin mul sub end}def /fartopright{begin x w add depth 45 cos mul add y
h add depth 45 sin mul add end}def /fartopleft{begin x depth 45 cos mul sub y
h add depth 45 sin mul add end}def /alignpoint{2 copy sub abs 1 le{add 2 div
round dup}if}def /getnode{nodes exch get}def /nodeconnect{gsave transform 4 2
roll transform exch 4 1 roll alignpoint 4 2 roll alignpoint 4 1 roll exch
itransform moveto itransform lineto stroke grestore}def /arrowdict 15 dict def
arrowdict begin /mtrx matrix def end /arrow{arrowdict begin /insetlength exch
def /headlength exch def /halfheadthickness exch 2 div def /tipy exch def
/tipx exch def /taily exch def /tailx exch def /dx tipx tailx sub def /dy tipy
taily sub def /angle dy dx atan def /savematrix mtrx currentmatrix def tipx
tipy translate angle rotate 0 0 moveto headlength neg halfheadthickness neg
lineto headlength insetlength sub neg 0 lineto headlength neg
halfheadthickness lineto closepath fill savematrix setmatrix end}def
/arrowfill{gsave newpath arrowwidth arrowlength arrowinset arrow grestore}def
/arrowline{arrowdict begin gsave newpath /tipfix exch def /tailfix exch def
/tipy exch def /tipx exch def /taily exch def /tailx exch def /dx tipx tailx
sub def /dy tipy taily sub def /angle dy dx atan def /savematrix mtrx
currentmatrix def tipx tipy translate angle rotate tipfix neg 0 moveto
savematrix setmatrix /savematrix mtrx currentmatrix def tailx taily translate
angle rotate tailfix 0 lineto savematrix setmatrix stroke grestore end}def
/arrownodeconnect{gsave transform 4 2 roll transform exch 4 1 roll alignpoint
4 2 roll alignpoint 4 1 roll exch itransform 4 2 roll itransform 4 copy 0
arrowdict begin arrowlength arrowinset sub end arrowline gsave newpath
arrowfill grestore grestore}def /doublearrownodeconnect{gsave transform 4 2
roll transform exch 4 1 roll alignpoint 4 2 roll alignpoint 4 1 roll exch
itransform 4 2 roll itransform 4 copy arrowdict begin arrowlength arrowinset
sub end dup arrowline 4 copy gsave newpath arrowfill grestore gsave newpath 4
2 roll arrowfill grestore grestore}def /barnodeconnect{4 2 roll 2 copy moveto
5 -1 roll add dup 3 1 roll lineto 2 index exch lineto lineto stroke}def
/arrowbarnodeconnect{4 2 roll 2 copy moveto 5 -1 roll add dup 3 1 roll lineto
2 index exch 4 copy lineto lineto gsave newpath 4 2 roll arrowfill fill
grestore stroke}def /nodetriangle{gsave exch nodes exch get nodebottom moveto
dup nodes exch get nodetopleft lineto nodes exch get nodetopright lineto
closepath stroke grestore}def /slope{/y1 exch def /x1 exch def /y0 exch def
/x0 exch def y1 y0 sub x1 x0 sub div}def /midpoint{/y1 exch def /x1 exch def
/y0 exch def /x0 exch def x1 x0 sub abs x1 x0 ge{x0 add}{x1 add}ifelse y1 y0
sub abs y1 y0 ge{y0 add}{y1 add}ifelse}def /tancurveto{1 index exch curveto}
def /nodetancurve{/depth exch def /to exch def /from exch def gsave nodes from
get noderight moveto nodes to get noderight tancurveto stroke grestore}def
/getloc{3 -1 roll nodes exch get dup begin depth end 3 1 roll exch cvx exec 4
2 roll}def /rightcur{0 /noderight getloc}def /leftcur{180 /nodeleft getloc}
def /topcur{90 /nodetop getloc}def /bottomcur{270 /nodebottom getloc}def
/topleftcur{135 /nodetopleft getloc}def /toprightcur{45 /nodetopright getloc}
def /bottomleftcur{225 /nodebottomleft getloc}def /bottomrightcur{315
/nodebottomright getloc}def /arrowfiddledict 10 dict def /arrowfiddle{
arrowfiddledict begin /twist exch def /shift exch def /shaft exch def /angle
exch twist add def /tipy exch def /tipx exch def tipx angle cos shaft mul add
tipy angle sin shaft mul add tipx angle cos shift mul add tipy angle sin shift
mul add end}def /nodecurve{gsave 0 fromangle arrowfiddle moveto 6 2 roll 0
toangle arrowfiddle curveto stroke grestore}def /arrownodecurve{gsave 0
fromangle arrowfiddle moveto 6 2 roll 4 copy 10 4 roll arrowlength arrowinset
sub toangle arrowfiddle curveto 0 toangle arrowfiddle arrowfill stroke
grestore}def /arrow2nodecurve{gsave 4 copy 0 fromangle arrowfiddle arrowfill
arrowlength arrowinset sub fromangle arrowfiddle moveto 6 2 roll 4 copy 10 4
roll arrowlength arrowinset sub toangle arrowfiddle curveto 0 toangle
arrowfiddle arrowfill stroke grestore}def /delink{gsave 4 copy newslope 6 2
roll newmidpoint delinkline}def /delinkline{moveto dup 0 eq{blong 2 div neg 0
rmoveto blong 0 rlineto 2 pop}{exch dup 0 eq{0 blong 2 div neg rmoveto 0 blong
rlineto 2 pop}{div neg dup dup mul 1 add sqrt blong exch div 2 copy mul 2 copy
2 div neg exch 2 div neg exch rmoveto rlineto 1 pop}ifelse}ifelse stroke
grestore}def /delinkcurve{8 copy 8 copy bezslope 10 2 roll bezmidpoint
delinkline}def /xyarrange{exch 3 1 roll 5 -1 roll 7 -1 roll exch 4 2 roll}def
/calcx{exch 3 mul 3 -1 roll 3 mul add add add 8 mul}def /calcy{add exch sub
exch sub}def /bezmidpoint{xyarrange calcx 5 1 roll calcx exch}def /bezslope{
xyarrange calcy 5 1 roll calcy exch}def /newmidpoint{exch 4 -1 roll add 2 div
3 1 roll add 2 div}def /newslope{4 -1 roll exch 4 1 roll sub 3 1 roll sub exch
}def /nodebox{nodes exch get begin gsave newpath x y moveto h w dobox cleanup
stroke grestore end}def /nodecircle{nodes exch get begin gsave newpath w 2 div
x add h 2 div y add w w mul h h mul add sqrt 2 div 4 -1 roll add 360 0 arcn
cleanup stroke grestore end}def /nodecircletrans{nodes exch get begin gsave
newpath w 2 div x add h 2 div y add w w mul h h mul add sqrt 2 div 4 -1 roll
add 360 0 arcn stroke grestore end}def /nodeoval{nodes exch get begin gsave
newpath x 2 sub y 2 sub moveto h 4 add w 4 add dooval cleanup stroke grestore
end}def /testnodeoval{nodes exch get begin gsave newpath h h mul w w mul add
sqrt div dup dup dup w mul neg x add exch h mul neg y add moveto dup h mul 2
mul h add exch w mul 2 mul w add testdooval stroke grestore end}def /cleanup{
gsave x y moveto h nodemargin sub .5 add w nodemargin sub .5 add doccbox 1
setgray fill grestore}def /boxdict 4 dict def boxdict /mtrx matrix put /dobox{
boxdict begin /w exch def /h exch def /savematrix mtrx currentmatrix def 0 h
rlineto w 0 rlineto 0 h neg rlineto closepath savematrix setmatrix end}def
/doccbox{boxdict begin /w exch def /h exch def /savematrix mtrx currentmatrix
def w 0 rlineto 0 h rlineto w neg 0 rlineto closepath savematrix setmatrix end
}def /ovaldict 6 dict def ovaldict /mtrx matrix put /dooval{ovaldict begin /w
exch def /h exch def /savematrix mtrx currentmatrix def 0 h 2 div rmoveto 0 h
2 div nodemargin sub nodemargin h 2 div w 2 div h 2 div rcurveto w 2 div
nodemargin sub 0 w 2 div nodemargin neg w 2 div h 2 div neg rcurveto 0 h 2 div
neg nodemargin add nodemargin neg h 2 div neg w 2 div neg h 2 div neg rcurveto
w 2 div neg nodemargin add 0 w 2 div neg nodemargin w 2 div neg h 2 div
rcurveto savematrix setmatrix end}def /testdooval{ovaldict begin /w exch def
/h exch def /r exch def h 2 div neg r mul dup h 2 div dup r mul 3 1 roll 2 r
sub mul 0 h rcurveto w 2 div dup r mul exch 2 r sub mul w 2 div r mul dup 3 1
roll w 0 rcurveto h 2 div r mul dup h 2 div neg dup r mul 3 1 roll 2 r sub mul
0 h neg rcurveto w 2 div neg dup r mul exch 2 r sub mul w 2 div neg r mul dup
3 1 roll w neg 0 rcurveto end}def end end
