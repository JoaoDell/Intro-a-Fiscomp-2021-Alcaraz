
      program main
          implicit double precision (a-h, o-z)

          
          
          
          g = 98d-1
          pi = 4*datan(1d0)
          
                           

          rl = 98d-1
          m = 1d0
          gamma = 5d-1
          omega = 2d0/3d0
          f01 = 5d-1
          f02 = 12d-1
          
          
          theta01 = pi/10d0
          theta02 = theta01 + 1d-3
          
          !f01 = 5d-1
          theta11 = theta01
          theta21 = theta02
          
          w11 = 0d0
          w21 = 0d0
          
          !f02 = 12d-1
          theta12 = theta01
          theta22 = theta02
          
          w12 = 0d0
          w22 = 0d0
          
          
          dt = 3d-2
          
          open(27, file = "saida-C-1-10799783.dat")
          open(28, file = "saida-C-2-10799783.dat")
          
          
          do i = 1, 800
          
          !f01 = 5d-1
          
              !pendulo 1
              w11 = w11 + (-(g/rl)*dsin(theta11)-gamma*w11 
     &+f01*dsin(i*dt*omega))*dt
              theta11 = theta11 + w11*dt
              
              !pendulo 2
              w21 = w21 + (-(g/rl)*dsin(theta21)-gamma*w21 
     &+f01*dsin(i*dt*omega))*dt
              theta21 = theta21 + w21*dt
              
              
              
          !f02 = 12d-1
              
              !pendulo 1
              w12 = w12 + (-(g/rl)*dsin(theta12)-gamma*w12 
     &+f02*dsin(i*dt*omega))*dt
              theta12 = theta12 + w12*dt
              
              !pendulo 2
              w22 = w22 + (-(g/rl)*dsin(theta22)-gamma*w22 
     &+f02*dsin(i*dt*omega))*dt
              theta22 = theta22 + w22*dt
              

              
              deltat1 = theta21 - theta11
              deltat2 = theta22 - theta12
              
              
              write(27,*) i*dt, deltat1, deltat2 
              write(28,*) i*dt, theta11, theta21, theta12, theta22
          
          
          end do  
          
          
          
          write(*,*) " "

          
          close(27)
          close(28)
          
          
          
          
          

          
          
          
          
          
          

          
          
          stop
      end program main