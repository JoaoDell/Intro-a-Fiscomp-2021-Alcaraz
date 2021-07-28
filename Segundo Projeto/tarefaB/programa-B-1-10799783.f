      program main
      
      
      

          parameter (jseg = 100) !numero de divisoes a serem feitas espaco para o plot do histograma
          parameter (M = 100000)
          integer iandarilho(M)
          integer iespaco(jseg) 
          real xmed(M)
          real x2med(M)
          
          

          r = 0.0 !numero 
          p = 0.50 !probabilidade p
          rmed = 0.0 !média das posições medias
          rmed2 = 0.0 !média das posições quadráticas médias
          N = 1000 !número de passos, definindo o tamanho máximo do domínio (de -1000 a 1000)
          idx = 1 !step que consiga acessar o domínio inteiro de -1000 a 1000
          
          
          
          
              
          !inicializaremos todos os andarilhos em 0
          do k = 1, M
              iandarilho(k) = 0
              xmed(k) = 0
              x2med(k) = 0
          end do
          
          
          !Andarilhos andando
          call srand(1001)
          do j = 1, M
              do i = 1, N
                  r = rand()
                  if(r .GT. p) then
                      iandarilho(j) = iandarilho(j) + idx
                  else 
                      iandarilho(j) = iandarilho(j) - idx 
                  end if
              end do
              xmed(j) = xmed(j) + iandarilho(j)
              x2med(j) = x2med(j) + iandarilho(j)**2
          end do
          
          
          
          !do para calcular as posicoes medias, posicoes quadraticas medias
          !e a media das posicoes medias e quadraticas medias
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
          
          open(20, file = "saida-B-1-10799783.dat")
          
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
                  write(20,*) istart, iespaco(i)
              end if
              
          end do
        
          close(20)
                              
          
          
          stop
      end program main