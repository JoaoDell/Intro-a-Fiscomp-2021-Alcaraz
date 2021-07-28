      !Declaração das funções para ajudar na simplicidade do código
      
      !função a ser usada
      double precision function dfx(x)       
          implicit double precision (a-h, o-z)
          
          
          dfx = dcosh(3d0*x)*dsin((25d-2)*x)
      
          return     
      end function dfx
      
      
      !função derivada primeira analitica
      double precision function dref(x)
          implicit double precision (a-h, o-z)
          
          
          dref = 3d0*dsin(x/4d0)*dsinh(3d0*x) 
     & + dcos(x/4d0)*dcosh(x*3d0)/4d0
      
      
          return      
      end function dref
      
      !função derivada segunda analítica
      double precision function d2ref(x)
          implicit double precision (a-h, o-z)
          
          
          d2ref = (3d0/2d0)*dcos(x/4d0)*dsinh(3d0*x) 
     & + (143d0/16d0)*dsin(x/4d0)*dcosh(x*3d0)
      
      
          return      
      end function d2ref
      
      !função derivada terceira analítica
      double precision function d3ref(x)
          implicit double precision (a-h, o-z)
          
          
          d3ref = (1d0/64d0)*(1692d0*dsin(x/4d0)*dsinh(3d0*x) 
     & + 431d0*dcos(x/4d0)*dcosh(x*3d0))
      
      
          return      
      end function d3ref
      
      

       
      !programa geral 
      program main
               
          implicit double precision (a-h, o-z)    
          double precision hvalues(14)
          double precision rvalues(14, 6)
          
          
        
        
          open(7, file = "saida-A-10799783.dat")
        
        
        
              
          
          x0 = 5d-1
          h = 0d0
          raux = 0d0
                    
          hvalues(1) = 5d-1
          hvalues(2) = 2d-1
          hvalues(3) = 1d-1
          hvalues(4) = 5d-2
          hvalues(5) = 1d-2
          hvalues(6) = 5d-3
          hvalues(7) = 1d-3
          hvalues(8) = 5d-4
          hvalues(9) = 1d-4
          hvalues(10)= 5d-5
          hvalues(11)= 1d-5
          hvalues(12)= 1d-6
          hvalues(13)= 1d-7
          hvalues(14)= 1d-8
          
          
          
          !valores de referência:
          !primeira derivada:     1.37992
          !segunda derivada:      5.79024
          !terceira derivada:    22.7367
         
        
          !começo da tabela
          write(7, 5) "h", "sim 3 pontos", "frente 2 pontos", 
     &"tras 2 pontos", "sim 5 pontos", "seg sim 5 pontos", 
     &"terc antisim" 
          
          !loop para calcular as derivadas para cada um dos 14 hs
          do i = 1, 14
          
              h = hvalues(i)
              er = 0d0
          
              !derivada simetrica de 3 pontos
              raux = (dfx(x0 + h) -dfx(x0 - h))/(2*h)
              
              rvalues(i, 1) = dabs(dref(x0) - raux)
              
              
              write(*,*) "Derivada simetrica de 3 pontos: ", raux
              
              
              !derivada pra frente de 2 pontos
              raux = (dfx(x0 + h) - dfx(x0))/h
              
              rvalues(i, 2) = dabs(dref(x0) - raux)
              
              
              write(*,*) "Derivada pra frente de 2 pontos: ", raux
              
              
              !derivada pra tras de 2 pontos
              raux = (dfx(x0) - dfx(x0 - h))/h
              
              rvalues(i, 3) = dabs(dref(x0) - raux)             
              
              
              write(*,*) "Derivada pra tras de 2 pontos: ", raux
              
              
              !derivada simetrica de 5 pontos
              raux = (dfx(x0 -2d0*h) - 8d0*dfx(x0 -h) + 8d0*dfx(x0 + h) 
     &-dfx(x0 +2d0*h))/(12d0*h)
     
              rvalues(i, 4) = dabs(dref(x0) - raux)
              
              write(*,*) "Derivada simetrica de 5 pontos: ", raux
              
              
              !derivada segunda simetrica de 5 pontos              
              raux = (-dfx(x0 -2d0*h) +16d0*dfx(x0 -h) -30d0*dfx(x0) 
     & +16d0*dfx(x0 + h) -dfx(x0 + 2d0*h))/(12d0*(h**2))
     
              rvalues(i, 5) = dabs(d2ref(x0) - raux)
              
              write(*,*) "Derivada segunda simetrica de 5 pontos: ",raux
              
              !derivada terceira anti-simetrica de 5 pontos
              raux = (-dfx(x0 -2d0*h) +2d0*dfx(x0 -h) -2d0*dfx(x0 +h) 
     & +dfx(x0 +2d0*h))/(2d0*(h**3))
     
              rvalues(i, 6) = dabs(d3ref(x0) - raux) 
              
              write(*,*) "Derivada terceira anti-sim de 5 pontos: ",raux
              
              
              write(*,*) " "
              
              !escrita dos valores na tabela
              write(7, 4) hvalues(i), (rvalues(i, k), k =1, 6)
              write(7, *)
              
          end do
          
          !escrita dos valores exatos na tabela      
          write(7, 6) "exatos", dref(x0), dref(x0), dref(x0), dref(x0),
     &d2ref(x0), d3ref(x0)
          
          !formatos de escrita
4         format(F22.11 , 6F22.11)
5         format(7A22)
6         format(A22, 6F22.11)

          
          close(7)
          



          stop
      end program main