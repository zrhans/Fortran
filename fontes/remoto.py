# from urllib.request import urlopen, Request
# url = 'http://dl.dropboxusercontent.com/s/kj5w60vnmd5ikgn/dados.txt'
# request = Request(url)
# response = urlopen(request) # Retorna um objeto HttpResponse
# html = response.read() # Aplica o metodo read que retorna a html como string
# print(html)
# response.close()

# Pacote Requests (possui uma API rica)
import io
import numpy
import pandas as pd

import matplotlib.pyplot as plt
from matplotlib import pylab
import requests

url = 'http://dl.dropboxusercontent.com/s/kj5w60vnmd5ikgn/dados.txt'
r = requests.get(url) # envia o resquest e pega o retorno em uma só vez
text = r.text # o metodo text retorna o html como string

print(r'text')

print(10*"=====")
f = pd.read_csv(io.StringIO(text),header=0,index_col=0)
#f = pd.read_csv(text,header=0,index_col=0, sep=',')
# data = 'col1,col2,col3\na,b,1\na,b,2\nc,d,3'
# a = pd.read_csv(io.StringIO(data))
print(f)
