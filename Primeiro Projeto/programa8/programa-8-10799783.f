     
      program main
          
          real r
          real pi
          real gamma
          real y
          real volume
          integer d
          
          !definindo pi e inicializando o volume
          pi = 4*atan(1.0)
          volume = 0.0
          
          write(*,*) "Qual o raio do objeto a ser calculado?"
          read(*,*) r
          
          write(*,*) "Qual a dimensão?"
          read(*,*) d
          
          
          open(8, file = "dimensoes-esferas.dat")
          
          
          !Loop para calcular o volume das esferas e as salvar em um arquivo
          do i = 2, d
              gamma = 1.0
                  
              y = real(i)/2 + 1.0
                            
              do while(y .GT. 1.0)
                 y = y - 1
                 gamma = y*gamma
              end do
                  
              if (y .LT. 1) then
                 gamma = sqrt(pi)*gamma
              else 
                 gamma = 1*gamma
              end if
              
              volume = (pi**(real(i)/2)*r**i)/gamma
                
              write(8,*) volume
          end do
          
          close(8)
          
      end program main