set encoding iso_8859_1
set terminal postscript enhanced
set output 'espectro_normphi_curva_0528u.eps'

set logscale
unset key

set title 'Espectro de Energia Normalizado[14/08/04-05:28h]'
set xlabel 'nz/U'
set ylabel 'nS_u/u_{*}^2 {/Symbol f}_{/Symbol e}^{2/3}'

ustar=0.6069
phiepsilon=1.
z=10.
U=7.50725286

fmu=0.039
cu=0.27

Gu(x)=((1.5)*cu*phiepsilon**(2./3)*x)/( (1+( (1.5*x**(5./3))/(fmu**(5./3)) ) )*(fmu**(5./3)))

plot [][0.001:10] 'S_Esp_sN03_Ta30_a2004d229h0528.dat' u ($1*z/U):($2*$1/(ustar**2*phiepsilon**(2./3))) w p,Gu(x)




set output 'espectro_normphi_curva_0528v.eps'
set title 'Espectro de Energia Normalizado[14/08/04-05:28h]'
set xlabel 'nz/U'
set ylabel 'nS_v/u_{*}^2 {/Symbol f}_{/Symbol e}^{2/3}'

ustar=0.6069
phiepsilon=1.
z=10.
U=7.50725286


fmv=0.12
cv=0.36

Gv(x)=((1.5)*cv*phiepsilon**(2./3)*x)/( (1+( (1.5*x**(5./3))/(fmv**(5./3)) ) )*(fmv**(5./3)))
plot [][0.001:10] 'S_Esp_sN03_Ta30_a2004d229h0528.dat' u ($1*z/U):($3*$1/(ustar**2*phiepsilon**(2./3))) w p,Gv(x)




set output 'espectro_normphi_curva_0528w.eps'
set title 'Espectro de Energia Normalizado[02/08/04-05:28h]'
set xlabel 'nz/U'
set ylabel 'nS_w/u_{*}^2 {/Symbol f}_{/Symbol e}^{2/3}'

ustar=0.6069
phiepsilon=1.
z=10.
U=7.50725286


fmw=0.32
cw=0.36

Gw(x)=((1.5)*cw*phiepsilon**(2./3)*x)/( (1+( (1.5*x**(5./3))/(fmw**(5./3)) ) )*(fmw**(5./3))    )
plot [][0.001:10] 'S_Esp_sN03_Ta30_a2004d229h0528.dat' u ($1*z/U):($4*$1/(ustar**2*phiepsilon**(2./3))) w p,Gw(x)	







