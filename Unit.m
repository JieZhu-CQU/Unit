
(*Created by Jie Zhu*)
(*email: jiezhu@cqu.edu.cn*)

$ContextAliases["u`"] = "Unit`SIValue`";
$ContextAliases["d`"] = "Unit`Dimention`";


BeginPackage["Unit`"]

Print["For value analysis, using u`F, where Fs are: c, h, hbar, ..."]
Print["For dimentional analysis, using d`F, where Fs are: m, l, t, ..."]
Print["The Hubble constant is set to h0=70."]
Print["CHRules, GCRules and GCHRules helps."]

CHRules::usage="Rules to express dimension of a physical quantity with dimension of c and h."
GCRules::usage="Rules to express dimension of a physical quantity with dimension of G and c."
GCHRules::usage="Rules to express dimension of a physical quantity with dimension of G, c and h."

SetH0::usage="Set the value of h0 in the Hubble constant. The default value is h0=70."

Begin["`SIValue`"]

c = 299792458.;(*m/s*)
c::usage="Light speed in SI."

h = 6.62607015*^-34;(*kg m^2/s*)
h::usage="Plank constant in SI."

hbar = h/(2 Pi);
G=6.6743`*^-11;

eV = 1.602176634*^-19;(*eV to J, J*)
keV = 1*^3 eV;
MeV = 1*^3 keV;
GeV = 1*^3 MeV;
TeV = 1*^3 GeV;
PeV = 1*^3 TeV;
Mpl = 1.220890*^19 GeV;

me = 0.5110 MeV(*mass of electron*);

m = 1;
cm = 1.*^-2;
um = 1.*^-6;(*micro meter, m*)
km = 1.*^3;

Mpc = 3.08567758*^22;(*Mpc to meter,m*)
kpc = Mpc/1000;
pc = kpc/1000;

(*Cosmology parameters*)
Om = 0.315;
Ol = 0.685;

h0 = 70;
H0 := h0*km/1/Mpc;(*km/s/Mpc to Hz*)
SetH0[x_]:= (h0=x;Print["The h0 is changed to ", h0, "."])


End[]

Begin["`Dimention`"]


Unprotect[M,L,T]

M /: MakeBoxes[M, form : (StandardForm | TraditionalForm)]:=StyleBox["[M]", FontColor -> Blue]
L /: MakeBoxes[L, form : (StandardForm | TraditionalForm)]:=StyleBox["[L]", FontColor -> Blue]
T /: MakeBoxes[T, form : (StandardForm | TraditionalForm)]:=StyleBox["[T]", FontColor -> Blue]

Protect[M,L,T]



Unprotect[H, d`C, G0]

H /: MakeBoxes[H, form : (StandardForm | TraditionalForm)]:=StyleBox["[h]", FontColor -> Red]
d`C /: MakeBoxes[d`C, form : (StandardForm | TraditionalForm)]:=StyleBox["[c]", FontColor -> Red]
G0 /: MakeBoxes[G0, form : (StandardForm | TraditionalForm)]:=StyleBox["[G]", FontColor -> Red]

Protect[H, d`C, G0]

assump={M>0,L>0,T>0,H>0,d`C>0, G0>0};
If[$Assumptions, $Assumptions=assump, AppendTo[$Assumptions, assump]];


m=M;

l=L;
r=l;

d`Area=r^2;
d`Volume=r^3;

t=T;

v=l/t;
c=v;

a=v/t;

F=m a;

p=F/l^2;

\[Rho]=m/l^3;
rho=\[Rho];

\[Omega]=1/t;
\[Nu]=\[Omega];

d`E=m v^2;
J=d`E;
eV=d`E;
keV=eV;
MeV=eV;
TeV=eV;
PeV=eV;


h=d`E/\[Omega];
G=F l^2/m^2;

Tmn=p;

CHRules={M -> H/(d`C^2 T), L -> d`C T}
GCRules={L->d`C T, M->d`C^3 T/ G0}
GCHRules={L -> (Sqrt[G0] Sqrt[H])/d`C^(3/2), M -> (Sqrt[d`C] Sqrt[H])/Sqrt[G0], T -> (Sqrt[G0] Sqrt[H])/d`C^(5/2)}

End[]



EndPackage[]

