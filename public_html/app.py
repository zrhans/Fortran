#! /bin/env python3
# -*- encoding: utf-8 -*-
from flask import Flask, render_template, json, request
from flask.ext.mysql import MySQL
# Before calling the create user stored procedure, let's make our password salted using a helper provided by Werkzeug. Import the module into app.py:
from werkzeug import generate_password_hash, check_password_hash

mysql = MySQL()
app = Flask(__name__)

#--------------------------------------------------
#  Configuraçoes do MySQL
#--------------------------------------------------

 
# MySQL configurations
app.config['MYSQL_DATABASE_USER'] = 'jay'
app.config['MYSQL_DATABASE_PASSWORD'] = 'jay'
app.config['MYSQL_DATABASE_DB'] = 'BucketList'
app.config['MYSQL_DATABASE_HOST'] = 'localhost'
mysql.init_app(app)




#--------------------------------------------------
#  Rotas
#--------------------------------------------------

# Definindo a rota basica /
@app.route("/")
def main():
  #return "Bem vindo!"
  return render_template('index.html')


# showSignUp to render the signup page once a request comes to /showSignUp:
@app.route('/showSignUp')
def showSignUp():
  return render_template('signup.html')


# Implementaç~ao do metodo sigUp
@app.route('/signUp',methods=['POST'])
def signUp():
  try:
 
    # read the posted values from the UI
    _name = request.form['inputName']
    _email = request.form['inputEmail']
    _password = request.form['inputPassword']
 
    # validate the received values
    if _name and _email and _password:
      # Criando uma conexao
      conn = mysql.connect()

      # Se a conexao for criada, necessitaremos um curso 
      # para iquirir (query) as "stored procedure". 
      # entao, usando a conexao 'conn', criamos um cursor.
      cursor = conn.cursor()
      #Use the salting module to create the hashed password.
      _hashed_password = generate_password_hash(_password)

      #Now, let's call the procedure sp_createUser:  
      cursor.callproc('sp_createUser',(_name,_email,_hashed_password))

      #If the procedure is executed successfully, then we'll commit the changes and return the success message.
      data = cursor.fetchall()
       
      if len(data) is 0:
          conn.commit()
          return json.dumps({'message':'User created successfully !'})
      else:
          return json.dumps({'error':str(data[0])})

        #return json.dumps({'html':'<span>All fields good !!</span>'})
    else:
        return json.dumps({'html':'<span>Enter the required fields</span>'})

  except Exception as e:
    return json.dumps({'error':str(e)})
  finally:
    cursor.close()
    conn.close()


#--------------------------------------------------
#        App Principal
#--------------------------------------------------
# Verificação de que o comando executado seja o principal
# para rodar o app

if __name__ == "__main__": app.run()
