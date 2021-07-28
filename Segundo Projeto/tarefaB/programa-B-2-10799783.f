      program main
      
      
      
          parameter (jseg = 100)
          parameter (M = 100000)
          integer iandarilho(M)
          integer iespaco(jseg)
          real xmed(M)
          real x2med(M)

          
          
          
          
          
          r = 0.0
          p = 0.333
          rmed = 0.0
          rmed2 = 0.0
          
    
          idx = 1 !step que consiga acessar o domínio inteiro de -N a N
          N = 1000
          
          
          !inicializaremos todos os andarilhos em 0
          do k = 1, M
              iandarilho(k) = 0
              xmed(k) = 0.0
              x2med(k) = 0.0
          end do
          
          
          
          call srand(time())
          do j = 1, M
              do i = 1, N
                  r = rand()
                  if(r .LT. p) then
                      iandarilho(j) = iandarilho(j) + idx
                  else 
                      iandarilho(j) = iandarilho(j) - idx 
                  end if
              end do
              xmed(j) = xmed(j) + iandarilho(j)
              x2med(j) = x2med(j) + iandarilho(j)*iandarilho(j)
          end do
         
          
          
          
          do i = 1, M 
              rmed = rmed + xmed(i)
              rmed2 = rmed2 + x2med(i)
          end do
          
          rmed = rmed/M
          rmed2 = rmed2/M
          
          write(*,*) "rmed = ", rmed
          write(*,*) "rmed2 = ", rmed2
          
          
          istart = -N !extremo negativo do dominio
          iseg = -1*2*istart/jseg !tamanho do reticulado em função do número de passos (istart = N)          
          
          open(21, file = "saida-B-2-10799783.dat")
          do i = 1, jseg
              istart = istart + iseg
              iespaco(i) = 0
              do j = 1, M
                  if(iandarilho(j) .LE. istart) then
                      if(iandarilho(j) .GT. (istart -iseg)) then
                          iespaco(i) = iespaco(i) + 1
                      end if
                  end if
              end do     
              
              if(iespaco(i) .GT. 0) then 
                  write(21,*) istart, iespaco(i)
              end if
          end do
          close(21)
                
              
          
          
          stop
      end program main