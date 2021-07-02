      
      double precision function f(theta, theta0, rl)
          implicit double precision (a-h, o-z)
          
          g = 980665d-5
          
          f = dsqrt(2d0*rl/g)*(1d0/dsqrt(dcos(theta) - dcos(theta0)))
          
          return

      end function f
      
      
      
      double precision function f1(theta, theta0, rl)
          implicit double precision (a-h, o-z)
          
          g = 98d-1
          
          f = dsqrt(2d0*rl/g)*(1d0/dsqrt(dsin(theta0)*(theta)))
          
          return

      end function f1
      
      
      double precision function cf(theta0, rl)
          implicit double precision (a-h, o-z)
          
          !regra de simpson
          iseg = 1536
          rint = 0d0
          E = 5d-4  
          a = -theta0 + E 
          b = +theta0 - E
          dh = (b - a)/(2d0*iseg)

          do i = 1, iseg
          
              rint = rint + (dh/3d0)*(f(a, theta0, rl)
     & + 4d0*f(a + dh, theta0, rl) + f(a + 2d0*dh, theta0, rl))
              a = a + 2d0*dh
              
          end do
          
          cf = rint

          return
      
      end function cf 
      


      
      program main
          implicit double precision (a-h, o-z)
          double precision coord(2)
          
          
          
          g = 98d-1
          pi = 4*datan(1d0)
          
          
          !B1
          rl = 98d-1
          m = 1d0
          gamma = 0d0
          omega = 0d0
          f0 = 0d0
          
          theta0 = pi/12d0
          theta01 = pi/24d0
          theta02 = pi/16d0
          theta03 = pi/10d0
          theta  = theta0
          w = 0d0 
          e = 0d0
          
          dt = 5d-2
          
          !open(20, file = "saida-B-1-10799783.dat")
          !open(21, file = "saida-B-2-10799783.dat")
          
          
          do i = 1, 1000
          
              w = w + (-(g/rl)*dsin(theta)-gamma*w +f0*dsin(i*omega))*dt
              theta = theta + w*dt
              
              !write(*,*) theta
              coord(1) = rl*dsin(theta)
              coord(2) = -rl*dcos(theta)
              
              e = (m*(rl**2d0)*w**2)/2d0 + m*g*rl*(1d0 - dcos(theta))
              
              !write(20,*) coord
              !write(21,*) i*dt, e 
          
          
          end do  
          
          !write(*,*) cf(theta0, rl)
          
          T  = 2d0*pi*dsqrt(rl/g)
          Tc = cf(theta0, rl)
          
          write(*,*) Tc
          
          write(*,*) " "
          write(*,*) "Erro do período B1 = ", dabs(T - Tc)
          
          !close(20)
          !close(21)
          
          
          
          
          
          !B2
          rl = 1d0
          m = 1d0
          gamma = 0d0
          omega = 0d0
          f0 = 0d0
          
          theta0 = pi/12d0
          theta  = theta0
          w = 0d0 
          e = 0d0
          
          dt = 5d-2
          
          !open(23, file = "saida-B-3-10799783.dat")
          
          
          do i = 1, 1000
          
              w = w + (-(g/rl)*dsin(theta)-gamma*w 
     & + f0*dsin(i*dt*omega))*dt
              theta = theta + w*dt
              
              !write(*,*) theta
              coord(1) = rl*dsin(theta)
              coord(2) = -rl*dcos(theta)
              
              e = (m*(rl**2d0)*w**2)/2d0 + m*g*rl*(1d0 - dcos(theta))
              
              !write(23,*) i*dt, e 
          
          
          end do  
          
          !write(*,*) cf(theta0, rl)
          
          T  = 2d0*pi*dsqrt(rl/g)
          !T = 2d0*pi*dsqrt(rl/g)*(1d0 + (theta0**2)/16d0)
          Tc = 2d0*pi*dsqrt(rl/g)*(1d0 + (theta0**2)/16d0)
          
          write(*,*) " "
          write(*,*) "Erro do período B2 = ", dabs(T - Tc)
          
          !close(23)
          
          
          
          
          
          
          !B3
          rl = 1d0
          m = 1d0
          gamma = 5d-1
          omega = 0d0
          f0 = 0d0
          
          theta0 = pi/12d0
          theta  = theta0
          w = 0d0 
          e = 0d0
          
          dt = 5d-2
          
          !open(24, file = "saida-B-4-10799783.dat")
          open(25, file = "saida-B-5-10799783.dat")
          
          
          do i = 1, 1000
          
              w = w + (-(g/rl)*dsin(theta)-gamma*w 
     & +f0*dsin(i*dt*omega))*dt
              theta = theta + w*dt
              

              coord(1) = rl*dsin(theta)
              coord(2) = -rl*dcos(theta)
              
              e = (m*(rl**2d0)*w**2)/2d0 + m*g*rl*(1d0 - dcos(theta))
              
              !write(24,*) coord
              write(25,*) i*dt, theta 
          
          
          end do  
          
          
          
          write(*,*) " "
          write(*,*) "B3 impressa com sucesso."

          
          !close(24)
          close(25)
          
          
          
          
          
          
          
          !B4
          rl = 1d0
          m = 1d0
          gamma = 5d-1
          omega = 2d0/3d0
          f01 = 0d0
          f02 = 5d-1
          f03 = 12d-1
          
          theta0 = pi/12d0
          theta1 = theta0
          theta2 = theta0
          theta3 = theta0
          w1 = 0d0
          w2 = 0d0
          w3 = 0d0 
          e  = 0d0
          
          dt = 3d-2
          
          open(26, file = "saida-B-6-10799783.dat")
          open(27, file = "saida-B-7-10799783.dat")
          
          
          do i = 1, 500
          
              !Pendulo para f01 = 0d0
              w1 = w1 + (-(g/rl)*dsin(theta1)-gamma*w1 
     &+f01*dsin(i*dt*omega))*dt
              theta1 = theta1 + w1*dt
              
              !Pendulo para f02 = 5d-1
              w2 = w2 + (-(g/rl)*dsin(theta2)-gamma*w2 
     &+f02*dsin(i*dt*omega))*dt
              theta2 = theta2 + w2*dt
              
              !Pendulo para f03 = 12d-1
              w3 = w3 + (-(g/rl)*dsin(theta3)-gamma*w3 
     &+f03*dsin(i*dt*omega))*dt
              theta3 = theta3 + w3*dt
              

              coord(1) = rl*dsin(theta)
              coord(2) = -rl*dcos(theta)
              
              
              write(26,*) i*dt, w1, w2, w3
              write(27,*) i*dt, theta1, theta2, theta3 
          
          
          end do  
          
          
          
          write(*,*) " "
          write(*,*) "B4 impressa com sucesso."

          
          close(26)
          close(27)
          
          
          
          
          

          
          
          
          
          
          

          
          
          stop
      end program main