'reinit'
'open conveccao.ctl'
ti = 1
tf = 5
while(ti<=tf)
'c'
'set t 'ti
'set gxout shaded'
'd teta'
'set gxout vector'
'd u;w'
'run /usr/local/lib/grads/cbarn.gs'
pull k
ti = ti+1
endwhile

