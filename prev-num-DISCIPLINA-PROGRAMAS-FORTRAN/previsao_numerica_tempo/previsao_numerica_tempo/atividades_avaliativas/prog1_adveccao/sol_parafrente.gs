'reinit'

'open sol_verdadeira_1h.ctl'
'open sol_verdadeira_2m.ctl'
'open sol_parafrente_2m.ctl'

'set t 1 25'
'set x 1 101'
'd temp.1'
'printim sol_verdadeira_1h.gif white'

'c'
'set t 1 10'
'set x 1 101'
'd temp.2'
'printim sol_verdadeira_2m.gif white'

'c'
'd temp.3'
'printim sol_parafrente_2m.gif white'
