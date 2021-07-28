      program main
      
          
          !Aqui estaremos lendo apenas 20 números de um arquivo
          parameter(N = 20)
          integer M
          real list(N)
          real aux
          
          open(20, file = "entrada-1-10799783")
          read(20, *) (list(i), i = 1, N)
          close(20)
          
          
          write(*,*) "Quantos numeros irá querer ordenar?"
          read(*,*) M
          
          
          do i = 1, M
              do j = 1, M-1
                  if(list(j) .GE. list(j+1)) then
                  
                      aux = list(j+1)
                      list(j+1) = list(j)
                      list(j) = aux 
                  
                  end if
              end do
          end do
          open(21, file = "saida-1-10799783")
          write(21,*) (list(k), k = 1, M)
          close(21)
                 
          
          
          
      end program main