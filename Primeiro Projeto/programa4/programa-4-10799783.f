      program main
          
          integer N
          integer counter
          
          write(*,*) "Digite um número:"
          read(*,*) N
          
          open(2, file = "saida-1-10799783")
          do i = 1, N
          
              counter = 0
              
              do j = 1, i
                  if ( mod(i,j) .EQ. 0) then
                      counter = counter + 1
                  end if
              end do
              
              !se o contador tiver valor de apenas 2, quer dizer que o numero foi 
              !dividido por apenas 2 numeros: 1 e ele mesmo
              if ( counter .LE. 2) then
                  write(2,*) i
              end if
              
          end do
          close(2)
      
      
      end program main