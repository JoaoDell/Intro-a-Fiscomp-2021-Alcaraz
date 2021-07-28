      program main
      
          
          complex z
          real zm
          real angle
          integer N
          real pi
          
          pi = 4*atan(1.0)
          zm = 1.0
          angle = 0.0
                
          
          
          
          write(*,*) "Digite o N:"
          read(*,*) N
    
          
          !loop para calcular as raízes baseado no resultado já conhecido      
          do j = 1, N
              zm = 3.0**(1e0/N)
              angle = j*pi*2.0/N
              z = cmplx(zm*cos(angle) +2.0, zm*sin(angle))    
              write(*,*) z  
          end do 
               
          
      
      
      
      end program main