      double precision function d2f(x, r)
          implicit double precision (a-h, o-z)
          !Função para calcular a aceleração gravitacional dos planetas
          
          pi = 4d0*datan(1d0)
          GMs = 4d0*(pi**2d0) !UA**3/ano**2
          
          
          d2f = -(GMs*x)/(r**3d0)
          
          
          return
      end function d2f


      program main
          implicit double precision (a-h, o-z)
          double precision sol(2)
          
          double precision raios(9)
          double precision velocidades(9)
          integer iters(9)
          
          pi = 4d0*datan(1d0)
          
          
          
         
          sol(1) = 0d0
          sol(2) = 0d0
          
          
          dt = 1d-3
          
          raios(1) = 39d-2 !mercurio
          velocidades(1) = 4d0*pi/1252d-3
          iters(1) = 1000
          
          raios(2)    = 72d-2 !venus
          velocidades(2) = 4d0*pi/1705d-3
          iters(2) = 1000
           
          raios(3)    = 1d0 !terra
          velocidades(3) = 4d0*pi/2d0
          iters(3) = 1000
          
          raios(4)    = 152d-2 !marte
          velocidades(4) = 4d0*pi/2467d-3
          iters(4) = 5000
           
          raios(5)  = 520d-2 !jupiter
          velocidades(5) = 4d0*pi/4562d-3
          iters(5) = 30000
          
          raios(6)  = 924d-2 !saturno
          velocidades(6) = 4d0*pi/6080d-3
          iters(6) = 50000
          
          raios(7)  = 1919d-2 !urano
          velocidades(7) = 4d0*pi/8761d-3
          iters(7) = 100000
          
          raios(8)   = 3006d-2 !netuno
          velocidades(8) = 4d0*pi/10966d-3
          iters(8) = 170000
          
          raios(9)  = 3953d-2 !plutao
          velocidades(9) = 4d0*pi/12576d-3
          iters(9) = 250000
          
          
          open(25, file = "saida-A1-1-10799783.dat")
          
          do j = 9, 9        
              v0x = 0d0
              v0y = velocidades(j)
              
              x0 = raios(j)
              y0 = 0d0
              
              x1 = x0 + v0x*dt
              y1 = y0 + v0y*dt
              
              x = 0d0
              y = 0d0
    
              !variáveis para a Lei de Kepler
              T0 = 0d0
              rK0 = 0d0
              rK = 1d0
              
               
              open(24, file = "saida-A1-2-10799783.dat")
              
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
                  
                  
                  if(dabs(x - raios(j)) .LT. 1d-1
     & .AND. dabs(y) .LT. 4d-3) then
     
                      T = i*dt
                      rK = ((T-T0)**2)/(r**3)
                      
                      if((rK - rK0) .GT. 1d-5) then
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