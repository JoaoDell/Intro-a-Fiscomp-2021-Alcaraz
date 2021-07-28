      double precision function d2f(x, r)
          implicit double precision (a-h, o-z)
          !Função para calcular a aceleração gravitacional dos planetas
          
          pi = 4d0*datan(1d0)
          GMs = 4d0*(pi**2d0) !UA**3/ano**2
          
          
          d2f = -(GMs*x)/(r**3d0)
          
          
          return
      end function d2f
      
      
      double precision function e(ex)
          implicit double precision (a-h, o-z)
          !função para o cálculo do termo da excentricidade no calculo da velocidade
          
          
          e =(1d0 -(ex**2)/4d0 -(ex**4)*(3d0/64d0) -(ex**6)*(5d0/256d0))
          
          
          return
      end function e


      program main
          implicit double precision (a-h, o-z)
          double precision sol(2)
          
          double precision raios(9)
          double precision velocidades(9)
          integer iters(9)
          
          pi = 4d0*datan(1d0)
          
          
          
          sol(1) = 0d0
          sol(2) = 0d0
          
          
          
          
          raios(1) = 39d-2 !mercurio
          velocidades(1) = (2d0*pi*raios(1)/24d-2)*e(206d-3)
          !velocidades(1) = 2d0*pi/dsqrt(raios(1))
          iters(1) = 1000
          
          
          raios(2)    = 72d-2 !venus
          velocidades(2) = (2d0*pi*raios(2)/615d-3)*e(7d-3)
          iters(2) = 1000
           
           
          raios(3)    = 1d0 !terra
          velocidades(3) = (2d0*pi*raios(3)/1d0)*e(17d-3)
          iters(3) = 1000
          

          raios(4)    = 152d-2 !marte
          velocidades(4) = (2d0*pi*raios(4)/188d-2)*e(93d-3)
          iters(4) = 5000


          raios(5)  = 520d-2 !jupiter
          velocidades(5) = (2d0*pi*raios(5)/1186d-2)*e(48d-3)
          iters(5) = 30000
          
          
          raios(6)  = 924d-2 !saturno
          velocidades(6) = (2d0*pi*raios(6)/29d0)*e(56d-3)
          iters(6) = 50000
          
   
          raios(7)  = 1919d-2 !urano
          velocidades(7) = (2d0*pi*raios(7)/8432d-2)*e(46d-3)
          iters(7) = 100000
          
          
          raios(8)   = 3006d-2 !netuno
          velocidades(8) = (2d0*pi*raios(8)/165d0)*e(10d-3)
          iters(8) = 170000
          

          raios(9)  = 3953d-2 !plutao
          velocidades(9) = (2d0*pi*raios(9)/248d0)*e(248d-3)
          iters(9) = 250000
          
          
          
          
          
          
          open(25, file = "saida-A2-1-10799783.dat")
          
          do j = 1, 9        
              dt = 1d-3
              v0x = 0d0
              v0y = velocidades(j)
              
              x0 = raios(j)
              y0 = 0d0
              
              x1 = x0 + v0x*dt
              y1 = y0 + v0y*dt
              
              x = 0d0
              y = 0d0
    
    
              T0 = 0d0
              
              rK0 = 0d0
              rK = 1d0
              
              open(24, file = "saida-A2-2-10799783.dat")
              
              write(25,*) "planeta ", j
              do i = 1, iters(j)
              
                  
                  r = dsqrt((x1 - sol(1))**2 + (y1 - sol(2))**2)
                  
                  x = 2d0*x1 - x0 + d2f(x1, r)*(dt**2d0)
                  y = 2d0*y1 - y0 + d2f(y1, r)*(dt**2d0)
                  
                  x0 = x1
                  y0 = y1
                  
                  x1 = x
                  y1 = y
                  
                  
                  
                  write(24,*) x, y
                  
                  if(dabs(x - raios(j)) .LT. 1d-3
     & .AND. dabs(y) .LT. 5d-3) then
                  
                      T = i*dt
                      rK = ((T-T0)**2)/(r**3)
                      
                      if((rK - rK0) .GT. 1d-2) then
                          write(25,*) rK
                      end if
                      
                      T0 = T
                      rK0 = rK
                  end if
                  
                  
                  
              end do
          
              close(24)   
          end do
          close(25)
        
      
          stop
      end program main