echo off
load saida1211.dat
A=saida1211; 
u =A(:,5);
v =A(:,6);
w=A(:,7);
t=A(:,8);
c=A(:,9);
q=A(:,10);
ventomedio1211 = sqrt((mean(u))^2+ (mean(v))^2);
save ventomedio1211 -ASCII ventomedio1211
%if ventomedio <= 1
 ds = (ventomedio1211)/10;
     k1= (2*pi)/(2^1*ds)
    k2= (2*pi)/(2^2*ds);
     k3= (2*pi)/(2^3*ds);
    k4= (2*pi)/(2^4*ds);
   k5= (2*pi)/(2^5*ds);
    k6= (2*pi)/(2^6*ds);
    k7= (2*pi)/(2^7*ds);
    k8= (2*pi)/(2^8*ds);
     k9= (2*pi)/(2^9*ds);
     k10= (2*pi)/(2^10*ds);
    k11= (2*pi)/(2^11*ds);
    k12= (2*pi)/(2^12*ds);
    k13= (2*pi)/(2^13*ds);
   k14= (2*pi)/(2^14*ds);
    K1211 = [k1; k2; k3; k4; k5; k6; k7; k8; k9; k10; k11; k12; k13; k14];
    save K1211 -ASCII K1211
 % analise para T    
    t = A(:,8);
    N=length(t);
    [C,L] = wavedec(t,1,'haar');
    W1= detcoef(C,L,1);
    [C,L] = wavedec(t,2,'haar');
    W2= detcoef(C,L,2);
    [C,L] = wavedec(t,3,'haar');
    W3= detcoef(C,L,3);
    [C,L] = wavedec(t,4,'haar');
    W4= detcoef(C,L,4);
    [C,L] = wavedec(t,5,'haar');
    W5= detcoef(C,L,5);
    [C,L] = wavedec(t,6,'haar');
    W6= detcoef(C,L,6);
    [C,L] = wavedec(t,7,'haar');
    W7= detcoef(C,L,7);
    [C,L] = wavedec(t,8,'haar');
    W8= detcoef(C,L,8);
    [C,L] = wavedec(t,9,'haar');
    W9= detcoef(C,L,9);
    [C,L] = wavedec(t,10,'haar');
    W10= detcoef(C,L,10);
    [C,L] = wavedec(t,11,'haar');
    W11= detcoef(C,L,11);
    [C,L] = wavedec(t,12,'haar');
    W12= detcoef(C,L,12);
    [C,L] = wavedec(t,13,'haar');
    W13= detcoef(C,L,13);
    [C,L] = wavedec(t,14,'haar');
    W14= detcoef(C,L,14);
    Wt1211=[W1; W2; W3; W4; W5; W6; W7; W8; W9; W10; W11; W12; W13; W14;];
    save Wt1211 -ASCII Wt1211
    % calculo do quadrado dos coeficienTes de da somatoria
    WW1=W1.*W1;
    WW2=W2.*W2;
    WW3=W3.*W3;
    WW4=W4.*W4;
    WW5=W5.*W5;
    WW6=W6.*W6;
    WW7=W7.*W7;
    WW8=W8.*W8;
    WW9=W9.*W9;
    WW10=W10.*W10;
    WW11=W11.*W11;
    WW12=W12.*W12;
    WW13=W13.*W13;
    WW14=W14.*W14;
    WWt1211=[W1; WW2; WW3; WW4; WW5; WW6; WW7; WW8; WW9; WW10; WW11; WW12; WW13; WW14;];
    save WWt1211 -ASCII WWt1211
    % calculo da funçao de densidade de energia
    EK1= mean(WW1)*ds/(2*pi*log(2));
    EK2= mean(WW2)*ds/(2*pi*log(2));
    EK3= mean(WW3)*ds/(2*pi*log(2));
    EK4= mean(WW4)*ds/(2*pi*log(2));
    EK5= mean(WW5)*ds/(2*pi*log(2));
    EK6= mean(WW6)*ds/(2*pi*log(2));
    EK7= mean(WW7)*ds/(2*pi*log(2));
    EK8= mean(WW8)*ds/(2*pi*log(2));
    EK9= mean(WW9)*ds/(2*pi*log(2));
    EK10=mean(WW10)*ds/(2*pi*log(2));
    EK11= mean(WW11)*ds/(2*pi*log(2));
    EK12= mean(WW12)*ds/(2*pi*log(2));
    EK13= mean(WW13)*ds/(2*pi*log(2));
    EK14= mean(WW14)*ds/(2*pi*log(2));
    EKt1211=[EK1; EK2; EK3; EK4; EK5; EK6; EK7; EK8; EK9; EK10; EK11; EK12; EK13; EK14;];
    save EKt1211 -ASCII EKt1211
 % analise para U
    u = A(:,5);
    N=length(u);
    [C,L] = wavedec(u,1,'haar');
    W1= detcoef(C,L,1);
    [C,L] = wavedec(u,2,'haar');
    W2= detcoef(C,L,2);
    [C,L] = wavedec(u,3,'haar');
    W3= detcoef(C,L,3);
    [C,L] = wavedec(u,4,'haar');
    W4= detcoef(C,L,4);
    [C,L] = wavedec(u,5,'haar');
    W5= detcoef(C,L,5);
    [C,L] = wavedec(u,6,'haar');
    W6= detcoef(C,L,6);
    [C,L] = wavedec(u,7,'haar');
    W7= detcoef(C,L,7);
    [C,L] = wavedec(u,8,'haar');
    W8= detcoef(C,L,8);
    [C,L] = wavedec(u,9,'haar');
    W9= detcoef(C,L,9);
    [C,L] = wavedec(u,10,'haar');
    W10= detcoef(C,L,10);
    [C,L] = wavedec(u,11,'haar');
    W11= detcoef(C,L,11);
    [C,L] = wavedec(u,12,'haar');
    W12= detcoef(C,L,12);
    [C,L] = wavedec(u,13,'haar');
    W13= detcoef(C,L,13);
    [C,L] = wavedec(u,14,'haar');
    W14= detcoef(C,L,14);
    Wu1211=[W1; W2; W3; W4; W5; W6; W7; W8; W9; W10; W11; W12; W13; W14;];
    save Wu1211 -ASCII Wu1211
     % calculo do quadrado dos coeficienTes de da somatoria
    WW1=W1.*W1;
    WW2=W2.*W2;
    WW3=W3.*W3;
    WW4=W4.*W4;
    WW5=W5.*W5;
    WW6=W6.*W6;
    WW7=W7.*W7;
    WW8=W8.*W8;
    WW9=W9.*W9;
    WW10=W10.*W10;
    WW11=W11.*W11;
    WW12=W12.*W12;
    WW13=W13.*W13;
    WW14=W14.*W14;
    WWu1211=[WW1; WW2; WW3; WW4; WW5; WW6; WW7; WW8; WW9; WW10; WW11; WW12; WW13; WW14;];
    save WWu1211 -ASCII WWu1211
% calculo da funçao de densidade de energia
    EK1= mean(WW1)*ds/(2*pi*log(2));
    EK2= mean(WW2)*ds/(2*pi*log(2));
    EK3= mean(WW3)*ds/(2*pi*log(2));
    EK4= mean(WW4)*ds/(2*pi*log(2));
    EK5= mean(WW5)*ds/(2*pi*log(2));
    EK6= mean(WW6)*ds/(2*pi*log(2));
    EK7= mean(WW7)*ds/(2*pi*log(2));
    EK8= mean(WW8)*ds/(2*pi*log(2));
    EK9= mean(WW9)*ds/(2*pi*log(2));
    EK10=mean(WW10)*ds/(2*pi*log(2));
    EK11= mean(WW11)*ds/(2*pi*log(2));
    EK12= mean(WW12)*ds/(2*pi*log(2));
    EK13= mean(WW13)*ds/(2*pi*log(2));
    EK14= mean(WW14)*ds/(2*pi*log(2));
    EKu1211=[EK1; EK2; EK3; EK4; EK5; EK6; EK7; EK8; EK9; EK10; EK11; EK12; EK13; EK14;];
% analise para V
     v = A(:,6);
    N=length(v);
    [C,L] = wavedec(v,1,'haar');
    W1= detcoef(C,L,1);
    [C,L] = wavedec(v,2,'haar');
    W2= detcoef(C,L,2);
    [C,L] = wavedec(v,3,'haar');
    W3= detcoef(C,L,3);
    [C,L] = wavedec(v,4,'haar');
    W4= detcoef(C,L,4);
    [C,L] = wavedec(v,5,'haar');
    W5= detcoef(C,L,5);
    [C,L] = wavedec(v,6,'haar');
    W6= detcoef(C,L,6);
    [C,L] = wavedec(v,7,'haar');
    W7= detcoef(C,L,7);
    [C,L] = wavedec(v,8,'haar');
    W8= detcoef(C,L,8);
    [C,L] = wavedec(v,9,'haar');
    W9= detcoef(C,L,9);
    [C,L] = wavedec(v,10,'haar');
    W10= detcoef(C,L,10);
    [C,L] = wavedec(v,11,'haar');
    W11= detcoef(C,L,11);
    [C,L] = wavedec(v,12,'haar');
    W12= detcoef(C,L,12);
    [C,L] = wavedec(v,13,'haar');
    W13= detcoef(C,L,13);
    [C,L] = wavedec(v,14,'haar');
    W14= detcoef(C,L,14);
    Wv1211=[W1; W2; W3; W4; W5; W6; W7; W8; W9; W10; W11; W12; W13; W14;];
    save Wv1211 -ASCII Wv1211
    % calculo do quadrado dos coeficienTes de da somatoria
    WW1=W1.*W1;
    WW2=W2.*W2;
    WW3=W3.*W3;
    WW4=W4.*W4;
    WW5=W5.*W5;
    WW6=W6.*W6;
    WW7=W7.*W7;
    WW8=W8.*W8;
    WW9=W9.*W9;
    WW10=W10.*W10;
    WW11=W11.*W11;
    WW12=W12.*W12;
    WW13=W13.*W13;
    WW14=W14.*W14;
    WWv1211=[WW1; WW2; WW3; WW4; WW5; WW6; WW7; WW8; WW9; WW10; WW11; WW12; WW13; WW14;];
    save WWv1211 -ASCII WWv1211
% calculo da funçao de densidade de energia
    EK1= mean(WW1)*ds/(2*pi*log(2));
    EK2= mean(WW2)*ds/(2*pi*log(2));
    EK3= mean(WW3)*ds/(2*pi*log(2));
    EK4= mean(WW4)*ds/(2*pi*log(2));
    EK5= mean(WW5)*ds/(2*pi*log(2));
    EK6= mean(WW6)*ds/(2*pi*log(2));
    EK7= mean(WW7)*ds/(2*pi*log(2));
    EK8= mean(WW8)*ds/(2*pi*log(2));
    EK9= mean(WW9)*ds/(2*pi*log(2));
    EK10=mean(WW10)*ds/(2*pi*log(2));
    EK11= mean(WW11)*ds/(2*pi*log(2));
    EK12= mean(WW12)*ds/(2*pi*log(2));
    EK13= mean(WW13)*ds/(2*pi*log(2));
    EK14= mean(WW14)*ds/(2*pi*log(2));
     EKv1211=[EK1; EK2; EK3; EK4; EK5; EK6; EK7; EK8; EK9; EK10; EK11; EK12; EK13; EK14;];
% analise para W
    w = A(:,7);
    N=length(w);
    [C,L] = wavedec(w,1,'haar');
    W1= detcoef(C,L,1);
    [C,L] = wavedec(w,2,'haar');
    W2= detcoef(C,L,2);
    [C,L] = wavedec(w,3,'haar');
    W3= detcoef(C,L,3);
    [C,L] = wavedec(w,4,'haar');
    W4= detcoef(C,L,4);
    [C,L] = wavedec(w,5,'haar');
    W5= detcoef(C,L,5);
    [C,L] = wavedec(w,6,'haar');
    W6= detcoef(C,L,6);
    [C,L] = wavedec(w,7,'haar');
    W7= detcoef(C,L,7);
    [C,L] = wavedec(w,8,'haar');
    W8= detcoef(C,L,8);
    [C,L] = wavedec(w,9,'haar');
    W9= detcoef(C,L,9);
    [C,L] = wavedec(w,10,'haar');
    W10= detcoef(C,L,10);
    [C,L] = wavedec(w,11,'haar');
    W11= detcoef(C,L,11);
    [C,L] = wavedec(w,12,'haar');
    W12= detcoef(C,L,12);
    [C,L] = wavedec(w,13,'haar');
    W13= detcoef(C,L,13);
    [C,L] = wavedec(w,14,'haar');
    W14= detcoef(C,L,14);
    Ww1211=[W1; W2; W3; W4; W5; W6; W7; W8; W9; W10; W11; W12; W13; W14;];
    save Ww1211 -ASCII Ww1211
     % calculo do quadrado dos coeficienTes de da somatoria
    WW1=W1.*W1;
    WW2=W2.*W2;
    WW3=W3.*W3;
    WW4=W4.*W4;
    WW5=W5.*W5;
    WW6=W6.*W6;
    WW7=W7.*W7;
    WW8=W8.*W8;
    WW9=W9.*W9;
    WW10=W10.*W10;
    WW11=W11.*W11;
    WW12=W12.*W12;
    WW13=W13.*W13;
    WW14=W14.*W14;
    WWw1211=[WW1; WW2; WW3; WW4; WW5; WW6; WW7; WW8; WW9; WW10; WW11; WW12; WW13; WW14;];
    save WWw1211 -ASCII WWw1211
% calculo da funçao de densidade de energia
    EK1= mean(WW1)*ds/(2*pi*log(2));
    EK2= mean(WW2)*ds/(2*pi*log(2));
    EK3= mean(WW3)*ds/(2*pi*log(2));
    EK4= mean(WW4)*ds/(2*pi*log(2));
    EK5= mean(WW5)*ds/(2*pi*log(2));
    EK6= mean(WW6)*ds/(2*pi*log(2));
    EK7= mean(WW7)*ds/(2*pi*log(2));
    EK8= mean(WW8)*ds/(2*pi*log(2));
    EK9= mean(WW9)*ds/(2*pi*log(2));
    EK10=mean(WW10)*ds/(2*pi*log(2));
    EK11= mean(WW11)*ds/(2*pi*log(2));
    EK12= mean(WW12)*ds/(2*pi*log(2));
    EK13= mean(WW13)*ds/(2*pi*log(2));
    EK14= mean(WW14)*ds/(2*pi*log(2));
    EKw1211=[EK1; EK2; EK3; EK4; EK5; EK6; EK7; EK8; EK9; EK10; EK11; EK12; EK13; EK14;];
   F1211= (ventomedio1211/2*pi).*K1211;
   EFT1211=(2*pi/ventomedio1211)*EKt1211;
   EFU1211=(2*pi/ventomedio1211)*EKu1211;
   EFV1211=(2*pi/ventomedio1211)*EKv1211;
   EFW1211=(2*pi/ventomedio1211)*EKw1211;
 
save EFT1211 -ASCII EFT1211
save EFU1211 -ASCII EFU1211
save EFV1211 -ASCII EFV1211
save EFW1211 -ASCII EFW1211
save F1211 -ASCII F1211

end
echo off
hold off

loglog(F1211,EFW1211,'.r-')
title('SPECTRA W  ')
xlabel('f(Hz)')
ylabel('S(f)') 
 saveas(gcf, 'SPECTRA W1211', 'jpg')

pause

hold off

loglog(F1211,EFU1211,'.r-')
title('SPECTRA U  ')
xlabel('f(Hz)')
ylabel('S(f)') 
saveas(gcf, 'SPECTRA U1211', 'jpg')

pause

hold off

loglog(F1211,EFV1211,'.r-')
title('SPECTRA V  ')
xlabel('f(Hz)')
ylabel('S(f)') 
saveas(gcf, 'SPECTRA V1211', 'jpg')
pause

hold off


loglog(F1211,EFT1211,'.r-')
title('SPECTRA T  ')
xlabel('f(Hz)')
ylabel('S(f)') 
saveas(gcf, 'SPECTRA T1211', 'jpg')

echo off

