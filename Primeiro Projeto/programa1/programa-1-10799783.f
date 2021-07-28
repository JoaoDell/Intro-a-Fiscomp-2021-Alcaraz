      program main
      
          real*4 a
          real*4 b
          real*4 c
          
          real*4 delta
          real*4 pos_result
          real*4 neg_result
          
          write(*,*) "Digite o valor de a"
          read(*,*) a
          write(*,*) "Digite o valor de b"
          read(*,*) b
          write(*,*) "Digite o valor de c"
          read(*,*) c
          

          delta = b**2 -4*a*c
          
          if (delta.LT.0) then
              write(*,*) "A equação não possui raízes reais!"
          else
               
              if (delta.EQ.0) then
                  write(*,*) "A equação possui apenas uma raiz real!"
                  write(*,*) "x = ", -b/2*a
              else
              
                  pos_result = (-b + sqrt(delta))/(2*a)
                  neg_result = (-b - sqrt(delta))/(2*a)
         
                  write(*,*) "A equação possui duas raizes reais!"
                  write(*,*) "x1 = ", pos_result
                  write(*,*) "x2 = ", neg_result
                  
              end if           
          end if 
          
      end program main