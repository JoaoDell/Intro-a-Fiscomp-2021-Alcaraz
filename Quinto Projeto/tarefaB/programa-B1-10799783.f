      double precision function d2ft(x, xj, r, rj)
          implicit double precision (a-h, o-z)
          
          !Função para calcular a aceleração da Terra
          
          pi = 4d0*datan(1d0)
          GMs = 4d0*(pi**2) !UA**3/ano**2
          GMj = GMs/1d3
          GMt = GMs/3d5

          
          
          d2ft = -(GMs*x)/(r**3) -GMj*(x-xj)/(rj**3)
          
          
          return
      end function d2ft
      
      
      double precision function d2fj(x, xj, r, rj)
          implicit double precision (a-h, o-z)
          
          !Função para calcular a aceleração de Júpiter
          
          pi = 4d0*datan(1d0)
          GMs = 4d0*(pi**2) !UA**3/ano**2
          GMj = GMs/1d3
          GMt = GMs/3d5
          
          
          d2fj = -(GMs*x)/(r**3) -GMt*(x-xj)/(rj**3)
          
          
          return
      end function d2fj
      
      
      double precision function e(ex)
          implicit double precision (a-h, o-z)
          
          !função para o cálculo do termo da excentricidade no calculo da velocidade
          
          
          e =(1d0 -(ex**2)/4d0 -(ex**4)*(3d0/64d0) -(ex**6)*(5d0/256d0))
          
          
          return
      end function e
      
      

      
      program main
          implicit double precision (a-h, o-z)
          double precision sol(2)
          double precision ter(2)
          double precision ter1(2)
          double precision ter0(2)
          double precision velter(2)
          double precision jup(2)
          double precision jup1(2)
          double precision jup0(2)
          double precision veljup(2)
          
          dt = 1d-3
          
          pi = 4d0*datan(1d0)
          
          sol(1) = 0d0
          sol(2) = 0d0
          
          jup0(1) = 520d-2
          jup0(2) =   0d0
          
          veljup(1) = 0d0
          veljup(2) = 4d0*pi/4562d-3
          
          jup1(1) = jup0(1) + veljup(1)*dt
          jup1(2) = jup0(2) + veljup(2)*dt


          
          ter0(1) = 1d0
          ter0(2) = 0d0
          
          velter(1) = 0d0
          velter(2) = (2d0*pi*ter0(1)/1d0)*e(17d-3)
          
          ter1(1) = ter0(1) + velter(1)*dt
          ter1(2) = ter0(2) + velter(2)*dt
          
          
          
      
      
          open(20, file = "saida-B1-1-10799783.dat")
          open(21, file = "saida-B1-2-10799783.dat")
      
          do i = 1, 25000
          
              r  = dsqrt((ter1(1) - sol(1))**2 + (ter1(2) - sol(2))**2)
              rtj = dsqrt((ter1(1) - jup(1))**2 + (ter1(2) - jup(2))**2)
                  
              ter(1) = 2d0*ter1(1) - ter0(1) 
     &+ d2ft(ter1(1), jup1(1), r, rtj)*(dt**2)
              ter(2) = 2d0*ter1(2) - ter0(2)
     &+ d2ft(ter1(2), jup1(2), r, rtj)*(dt**2)
     
     
              rj  = dsqrt((jup1(1) - sol(1))**2 + (jup1(2) - sol(2))**2)
              
              jup(1) = 2d0*jup1(1) - jup0(1) 
     &+ d2fj(jup1(1), ter1(1), rj, rtj)*(dt**2)
              jup(2) = 2d0*jup1(2) - jup0(2)
     &+ d2fj(jup1(2), ter1(2), rj, rtj)*(dt**2)

                  
                  

              ter0 = ter1
                  
              ter1 = ter
              
              
              jup0 = jup1
                  
              jup1 = jup
                  
              write(20, *) ter(1), ter(2)
              write(21, *) jup(1), jup(2)
           
          end do
          
          close(20)
          close(21)
      
      
      
          stop
      end program main