      program main 
     

          parameter (M = 100) !numero de andarilhos
          dimension iandarilho(M,2) !posicoes dos andarilhos
          dimension rmed(M,2)
          dimension r2med(M,2)
          dimension omed(2)
          dimension omed2(2)
          
          
          p = 0.25 !probabilidade de ir para cima, baixo, esquerda ou direita
          r = 0.0  !número aleatório
          idx = 1  !step
          
          N = 1000 !número de passos
          
          omed(1)  = 0.0
          omed(2)  = 0.0
          omed2(1) = 0.0
          omed2(2) = 0.0
          delta2 = 0.0 !variavel para caulcular a delta2
          
          do i = 1, M
              do j = 1, 2
                  iandarilho(i, j) = 0.0
                  rmed(i, j)       = 0.0
                  r2med(i, j)      = 0.0
              end do
          end do
          
          
          call srand(1001)
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
          
          
          
          do i = 1, M
              omed(1) = omed(1) + rmed(i, 1)
              omed(2) = omed(2) + rmed(i, 2)
              
              omed2(1) = omed2(1) + r2med(i, 1)
              omed2(2) = omed2(2) + r2med(i, 2)
          

          end do
          
          omed = omed/M
          omed2 = omed2/M
          
          write(*,*) "<x> = ", omed(1), "<y> = ", omed(2)
          write(*,*) "<x**2> = ", omed2(1), "<y**2> = ", omed2(2)
          
          delta2 = omed2(1) + omed2(2) - omed(1)**2 - omed(2)**2
          write(*,*) "delta**2 = ",delta2          
          
          open(20, file = "saída-C-1-10799783.dat")
          do i = 1, M
              write(20, *) iandarilho(i, 1), iandarilho(i, 2)
          end do
          close(20)
          
          
     
          stop
      end program main