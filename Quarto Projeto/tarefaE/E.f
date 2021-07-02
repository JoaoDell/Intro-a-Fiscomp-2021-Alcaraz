      program main
          implicit double precision (a-h, o-z)

          
          
          
          g = 98d-1
          pi = 4*datan(1d0)
          
                           
          !B4
          rl = 98d-1
          m = 1d0
          gamma = 5d-1
          omega = pi/10d0
          f01 = 5d-1
          f02 = 12d-1
          
          theta0 = pi/12d0
          
          !pendulo 1
          theta11 = theta0

          
          w11 = 0d0
          w21 = 0d0
          
          !pendulo 2
          theta12 = theta0
          
          sec = 0d0

          
          w12 = 0d0
          w22 = 0d0
          

          
          dt = 1d-3
          

          open(28, file = "saida-D-1-10799783.dat")
          open(29, file = "saida-D-2-10799783.dat")
          open(30, file = "saida-D-3-10799783.dat")
          open(31, file = "saida-D-4-10799783.dat")
          open(32, file = "saida-D-5-10799783.dat")
          
          
          
          do j = 0, 4
              call srand(3053*j)
          
              theta11 = theta0*rand()
              theta12 = theta0*rand()
          
          
              do i = 1, 100000
                  
                  !write(*,*) i*dt*omega
                  sec = i*pi
              
                  w11 = w11 + (-(g/rl)*dsin(theta11)-gamma*w11 
     &+f01*dsin(i*dt*omega))*dt
                  theta11 = theta11 + w11*dt
                  
    
                  
                  w12 = w12 + (-(g/rl)*dsin(theta12)-gamma*w12 
     &+f02*dsin(i*dt*omega))*dt
                  theta12 = theta12 + w12*dt
                  
    
                  n = 28 + j
        
                  if ( dabs(i*dt- sec/omega) .LT. dt/2) then
                      write(n,*) theta11, w11!, theta12, w12 
                  end if
                   
              end do
              
              
              
          end do  
          
          
          write(*,*) " "

          
          close(28)
          close(29)
          close(30)
          close(31)
          close(32)
          
 
          
          stop
      end program main