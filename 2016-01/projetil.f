      program lancamento_projetil
      implicit none
      real g,x,y,vx,vy,k,del_t,h,F1,F2,F3,F4, H1, H2, H3, H4
      real G1,G2,G3,G4,I1, I2, I3, I4
      integer i
      real v
      
      open(7,file="data.dat")

         x=0 !vx=v*cosA                     
         y=0

          write(*,*) "x0 = ?"
           read(*,*) x
          write(*,*) "y0 = ?"
           read(*,*) y
           write(*,*) "v0 = ?"
            read(*,*) v
             write(*,*) "k = ?"
              read(*,*) k
               del_t=0.001
                h=del_t/2
                 g=9.81

             do 500 i=1,20000
             
              F1=v
              F2=v+h*F1
              F3=v+h*F2
              F4=v+del_t*F3


                   x=x+del_t*0.16666*(F1+2*F2+2*F3+F4)

            G1=-k*vx
            G2=-k*vx+h*G1
            G3=-k*vx+h*G2
            G4=-k*vx+del_t*G3

            vx=vx+del_t*0.16666*(G1+2*G2+2*G3+G4)
             
             
              H1=vy
              H2=vy+h*H1
              H3=vy+h*H2
              H4=vy+del_t*H3


                   y=y+del_t*0.16666*(F1+2*F2+2*F3+F4)

            I1=-k*v-g
            I2=-k*v-g+h*I1
            I3=-k*v-g+h*I2
            I4=-k*v-g+del_t*I3

            vy=vy+del_t*0.16666*(G1+2*G2+2*G3+G4)
            write(7,*) i,x,y,vx,vy

            if (x.lt.0) then
            stop
            endif

            if (y.lt.0) then
            stop
            endif

500        continue
             close(7)
      stop
      end