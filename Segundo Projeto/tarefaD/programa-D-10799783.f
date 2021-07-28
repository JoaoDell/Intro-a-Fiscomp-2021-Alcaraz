      program main 

          parameter (jseg = 100) !segmentações a serem feitas no espaço
          parameter (M = 100) !número de andarilhos
          dimension iandarilho(M,2)
          dimension rmed(M,2)
          dimension r2med(M,2)
          dimension omed(2)
          dimension omed2(2)
          dimension iespaco(jseg,jseg)
          
          
          
          
          p = 0.25
          r = 0.0
          idx = 1
          N = 10000 !número de passos por ciclo
          S = 0.0
          ibound = N
          ixstart = -ibound
          iystart = -ibound
          
          
          omed(1)  = 0.0
          omed(2)  = 0.0
          omed2(1) = 0.0
          omed2(2) = 0.0
          
          do i = 1, M
              do j = 1, 2
                  iandarilho(i, j) = 0.0
                  rmed(i, j)       = 0.0
                  r2med(i, j)      = 0.0
              end do
          end do
          
          
          call srand(1003)
          open(21, file = "saida-D-1-10799783.dat")
          do l = 1, 100 !ciclo para aumentar o número de passos e calcular a entropia
          
              do i = 1, M
                  do j = 1, N
                      r = rand()
                      if(r .LE. p) then
                          
                          iandarilho(i, 1) = iandarilho(i, 1) + idx 
                                        
                      else if (r .LE. p + p) then
                      
                          iandarilho(i, 2) = iandarilho(i, 2) + idx
                      
                      else if (r .LE. p + p + p) then
                      
                          iandarilho(i, 1) = iandarilho(i, 1) - idx
                      
                      else
                      
                          iandarilho(i, 2) = iandarilho(i, 2) - idx
                      
                      end if
                      
                      
                  end do
                  
                  rmed(i, 1) = rmed(i, 1) + iandarilho(i, 1)
                  rmed(i, 2) = rmed(i, 2) + iandarilho(i, 2)
                      
                  r2med(i, 1) = r2med(i, 1) + iandarilho(i, 1)**2
                  r2med(i, 2) = r2med(i, 2) + iandarilho(i, 2)**2
                  
              end do
              
              
              
              
              
              iseg = -1*2*ixstart/jseg
              
              
              
              
              do j = 1, jseg
                  ixstart = ixstart + iseg
                  iystart = -ibound
                  do i = 1, jseg
                  
                      iespaco(j,i) = 0
                      iystart = iystart + iseg
                      
                      do k = 1, M
                      if(iandarilho(k,1) .LE. ixstart .AND.
     &iandarilho(k,1) .GT. (ixstart -iseg) .AND. iandarilho(k,2) .LE.  
     &iystart .AND.  iandarilho(k,2) .GT. (iystart -iseg)) then
                            iespaco(j,i) = iespaco(j,i) + 1
                          end if
                      end do     
                      
            
                  end do
              end do
             
              
                  
                  
                 
              
              !calculo da entropia
              do i = 1, jseg
                  do j = 1, jseg
                      e = iespaco(j,i)
                      resp = e/M
                      if(resp .GT. 0.0) then
                          S = S - (resp)*log(resp)
                      end if
                  end do
              end do
              
              
              !escrita dos valores das entropias num documento externo para o plot
              if(S .NE. 0.0) then
                  write(*,*) l*N, ", S = ", S
                  write(21,*) l*N, S
              end if
              S = 0.0
              
          end do
          close(21)
          
          
          
     
          stop
      end program main