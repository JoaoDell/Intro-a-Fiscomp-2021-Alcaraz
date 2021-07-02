      program main
          implicit double precision (a-h, o-z)
          
          
          
          
          g = 98d-1
          pi = 4d0*datan(1d0)
          
          rl = 98d-1
          m = 1d0
          theta = pi/8d0
          w = 0d0 
          e = 0d0
          
          dt = 1d-2
          
          
          
          
          open(20, file = "saida-A-1-10799783.dat")
          open(21, file = "saida-A-2-10799783.dat")
          
          
          do i = 1, 55000 
              
              w0 = w              
              w = w - (g/rl)*theta*dt
              theta = theta + w0*dt
              
              !atualizacao do theta para que não passe 2*pi
              theta = dmod(theta, 2d0*pi)
              

              e = (m*(rl**2)*w**2)/2d0 + m*g*rl*(1d0 - dcos(theta))
              
              write(20,*) i*dt, theta
              write(21,*) i*dt, e 
              
          
          end do
          
          close(20)
          close(21)
          
          
          
          !conserto ao método anterior
          theta = pi/8d0
          w = 0d0 
          e = 0d0
          
          dt = 1d-2

          
          
          
          open(22, file = "saida-A-3-10799783.dat")
          open(23, file = "saida-A-4-10799783.dat")
          
          
          do i = 1, 55000
                         
              w = w - (g/rl)*theta*dt
              theta = theta + w*dt
              

              
              e = (m*(rl**2d0)*w**2)/2d0 + m*g*rl*(1d0 - dcos(theta))
              
              write(22,*) i*dt, theta
              write(23,*) i*dt, e 
              
          
          end do
          
          close(22)
          close(23)
      

      
          stop
      end program main