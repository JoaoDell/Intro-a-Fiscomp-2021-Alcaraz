      
      program main
      
          real r
          real vec
          real s
          real area
          integer d
          integer M
          integer counter
          
          r = 1.0
          vec = 0.0
          M = 10000000
          counter = 0
          
          write(*,*) "Quantas dimensoes gostaria de calcular o volume?"
          read(*,*) d
          
             
          
          !inicializando a seed do rand() baseado no horário do computador
          call srand(time())
          do i = 1, M
              do j = 1, d
                  vec = vec + ((r*rand())**2)
              end do
              if( sqrt(vec) .LE. r) then
                  counter = counter + 1
              end if
              vec = 0.0
          end do
          
          s = real(counter)/real(M)
          area = (2**d)*s*(r**d)
         
          write(*,*) area
          
      
      
      
      end program main