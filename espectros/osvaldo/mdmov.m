%CÁLCULO DA MEDIA DE 3 MINUTOS
%
load noite12244.dat
t=noite12244(:,8)
%clear i;
TT=0;
J=1;
if TT==1,
SUAVE=input('Entre com o número de elementos por Intervalo (multiplo de 30): ');
	for i=1:16384,
		mediau(i,8)=mean(t(i:(i+(SUAVE-1)),1));
		if i>=J,
         i		
			J=J+1800;
		end
    end
end

