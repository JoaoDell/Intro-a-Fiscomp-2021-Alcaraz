      !declaração de funções para ajudar na simplicidade do código
      
      !função a ser usada
      double precision function dfx(x)       
          implicit double precision (a-h, o-z) 
          
          dfx = x**3d0 - 14d0*x - 20d0
      
          return     
      end function dfx
      
      
      !derivada da função a ser usada calculada por metodos anteriores
      double precision function deri(x) 
       
          implicit double precision (a-h, o-z)      

          double precision dfx
          

          n = 1
          
          h = 1d-2          

          !derivada simetrica de 5 pontos
          deri = (dfx(x -2d0*n*h) - 8d0*dfx(x -n*h) + 8d0*dfx(x + n*h) 
     &-dfx(x +2d0*n*h))/(12d0*h)

          return     
      end function deri
      
      
 
           
      !programa
      program main
      
      
          implicit double precision (a-h, o-z)  
          
          
      
          !valores de referencia: -2, 4.31662, -2.31662
          
          x0 = -10d0
          x = x0
          r0 = 5d-1
          rint = r0

          
          open(20, file = "saida-C-10799783.dat")
          
                
          
          
          !busca direta
          l = 1
          
          write(20,*) "Busca Direta"
          do i = 1, 50


                            

              if( (dfx(x) .GE. 0d0 .AND. dfx(x + rint) .LT.0d0).OR. 
     & ( dfx(x + rint) .GE. 0d0 .AND. dfx(x) .LT. 0d0) ) then
        
              

                  x = x + rint/2d0
                  rint = rint/2d0
                  k = 1

              
                  write(20,1) "r", l 
                  l = l + 1
                  
                  !buscar a raiz até que o erro seja menor que 1d-6
                  do while( rint .GT. 1d-6)
                  
                      
                      if( (dfx(x) .GE. 0d0 .AND. dfx(x + rint).LT.0d0)  
     & .OR.( dfx(x + rint) .GE. 0d0 .AND. dfx(x) .LT. 0d0) ) then
                    

                         
                         x = x + rint/2d0
                      
                      else if((dfx(x).GE.0d0.AND.dfx(x - rint).LT.0d0)  
     &.OR. ( dfx(x - rint) .GE. 0d0 .AND. dfx(x) .LT. 0d0)) then


                                              
                         x = x - rint/2d0 
                      
                      end if
                      
                      
                      
                      rint= rint/2d0 
                      
                      if (k .LE. 6) then
                          write(20,2) "i = ", k, x
                      end if
                      k = k + 1
                                
                  
                  end do
                  
   
              end if
 
              rint = r0  

              x = x0 + i*r0
              
          end do

          
          
          
          
          
          
          !Newtown-Raphson
          write(20,*) " "
          write(20,*) "Newton-Raphson"
          
          x0 = -10d0
          x = x0
          r0 = 5d-1

          
          l = 1 !variavel para o arquivo de saída
          
          do i = 1, 50
          
              k = 1
              
          
              if( (dfx(x0) .GE. 0d0 .AND. dfx(x0 + r0) .LT.0d0).OR. 
     & ( dfx(x0 + r0) .GE. 0d0 .AND. dfx(x0) .LT. 0d0) ) then
     
                  write(20,1) "r", l 
                  l = l + 1
                  do while(dabs(xant - x0) .GT. 1d-6)
                      
                      xant = x
                      x = x0 - dfx(x0)/deri(x0)
                      x0 = x
                          
                      if (k .LE. 6) then
                          write(20,2) "i = ", k, x
                      end if
                      k = k + 1
                                

                  end do

              
              end if
              
              
              xant = 0d0
              x0 = -10d0
              x0 = x0 + i*r0
          end do
          





 
          !Secante
          write(20,*) " "
          write(20,*) "Secante"
          
          x0 = -10d0
          r0 = 5d-1
          xm = x0 + r0
          
          l = 1 !variavel para o arquivo de saída
          
          
          do i = 1, 50
          
              k = 1
              
              
              if( (dfx(x0) .GE. 0d0 .AND. dfx(x0 + r0) .LT.0d0).OR. 
     & ( dfx(x0 + r0) .GE. 0d0 .AND. dfx(x0) .LT. 0d0) ) then
     
                  write(20,1) "r", l 
                  l = l + 1
                  
                  do while(dabs(xant - x0) .GT. 1d-6) 
                  
                      if((dfx(x)- dfx(xm)) .EQ. 0d0) then
                          exit
                      end if
        
                      xant = x    
                      x = x0 - dfx(x0)*(x0 - xm)/(dfx(x0) - dfx(xm))
                      xm = x0
                      x0 = x
                      
                      if (k .LE. 6) then
                          write(20,2) "i = ", k, x
                      end if
                      k = k + 1
                      
                          
                  end do
              
              end if
                

              
              
              xant = 0d0
              x0 = -10d0
              x0 = x0 + i*r0
              xm = x0 - r0
          end do
          
          
          
1         format(A1, I1)
2         format(A4, I1, F20.15)

          close(20)
          

      
      
          stop
      
      end program main