      program main
          
          real v1(3)
          real v2(3)
          
          real i
          real j
          real k
          
          real p_area = 0
          
          
          write(*,*) "Digite as 3 coordenadas do vetor v1"
          read(*,*) v1(1)
          read(*,*) v1(2)
          read(*,*) v1(3)
          
          
          write(*,*) "Digite as 3 coordenadas do vetor v2"
          read(*,*) v2(1)
          read(*,*) v2(2)
          read(*,*) v2(3)
          
          
          i = v1(2)*v2(3) - v2(2)*v1(3)
          j = v1(3)*v2(1) - v2(3)*v1(1)
          k = v1(1)*v2(2) - v2(1)*v1(2) 
          
                    
          p_area = sqrt(i**2 + j**2 + k**2)
     
          write(*,*) "A área do triangulo formado por v1 e v2 é: "
          write(*,*) p_area/2

         
      
      
      end program main