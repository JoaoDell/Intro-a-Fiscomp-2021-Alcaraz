      double precision function d2ft(x, xj, r, rj)
          implicit double precision (a-h, o-z)
          
          !Função para calcular a aceleração dos asteroides
          
          pi = 4d0*datan(1d0)
          GMs = 4d0*(pi**2) !UA**3/ano**2
          GMj = (GMs/1d3)
          GMt = GMs/3d5

          
          
          d2ft = -(GMs*x)/(r**3) -GMj*(x-xj)/(rj**3)
          
          
          return
      end function d2ft
      
      
      double precision function d2fj(x, r)
          implicit double precision (a-h, o-z)
          
          !Função para calcular a aceleração de Júpiter
          
          pi = 4d0*datan(1d0)
          GMs = 4d0*(pi**2) !UA**3/ano**2
          GMj = GMs/1d3
          GMt = GMs/3d5
          
          
          d2fj = -(GMs*x)/(r**3)
          
          
          return
      end function d2fj
      

      
      program main
          implicit double precision (a-h, o-z)
          double precision sol(2)
          double precision asterI(2)
          double precision asterI1(2)
          double precision asterI0(2)
          
          double precision asterII(2)
          double precision asterII1(2)
          double precision asterII0(2)
          
          double precision asterIII(2)
          double precision asterIII1(2)
          double precision asterIII0(2)

          
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
          veljup(2) = 2755d-3
          
          jup1(1) = jup0(1) + veljup(1)*dt
          jup1(2) = jup0(2) + veljup(2)*dt


          
          asterI0(1) = 3d0
          asterI0(2) = 0d0
          velasterI = 3628d-3
          
          asterII0(1) = 3276d-3
          asterII0(2) = 0d0
          velasterII = 3471d-3
          
          asterIII0(1) = 37d-1
          asterIII0(2) = 0d0
          velasterIII = 3267d-3
          
          
          asterI1(1) = asterI0(1) + 0d0*dt
          asterI1(2) = asterI0(2) + velasterI*dt
          
          asterII1(1) = asterII0(1) + 0d0*dt
          asterII1(2) = asterII0(2) + velasterII*dt
          
          asterIII1(1) = asterIII0(1) + 0d0*dt
          asterIII1(2) = asterIII0(2) + velasterIII*dt
          
          
          
              
          
          
          
      
      
      
          open(20, file = "saida-B3-1-10799783.dat")
          open(21, file = "saida-B3-2-10799783.dat")
          open(22, file = "saida-B3-3-10799783.dat")
          open(23, file = "saida-B3-4-10799783.dat")
      
          do i = 1, 25000
          
              r  = dsqrt((asterI1(1) - sol(1))**2 
     &+ (asterI1(2) - sol(2))**2)
              rIj = dsqrt((asterI1(1) - jup(1))**2 
     &+ (asterI1(2) - jup(2))**2)
                  
                  
              asterI(1) = 2d0*asterI1(1) - asterI0(1) 
     &+ d2ft(asterI1(1), jup1(1), r, rIj)*(dt**2)
              asterI(2) = 2d0*asterI1(2) - asterI0(2)
     &+ d2ft(asterI1(2), jup1(2), r, rIj)*(dt**2)
     
     
     
     
              r  = dsqrt((asterII1(1) - sol(1))**2 
     &+ (asterII1(2) - sol(2))**2)
              rIIj = dsqrt((asterII1(1) - jup(1))**2 
     &+ (asterII1(2) - jup(2))**2)
                  
              asterII(1) = 2d0*asterII1(1) - asterII0(1) 
     &+ d2ft(asterII1(1), jup1(1), r, rIIj)*(dt**2)
              asterII(2) = 2d0*asterII1(2) - asterII0(2)
     &+ d2ft(asterII1(2), jup1(2), r, rIIj)*(dt**2)
     
     
     
     
              r  = dsqrt((asterIII1(1) - sol(1))**2 
     &+ (asterIII1(2) - sol(2))**2)
              rIIIj = dsqrt((asterIII1(1) - jup(1))**2 
     &+ (asterIII1(2) - jup(2))**2)
                  
              asterIII(1) = 2d0*asterIII1(1) - asterIII0(1) 
     &+ d2ft(asterIII1(1), jup1(1), r, rIIIj)*(dt**2)
              asterIII(2) = 2d0*asterIII1(2) - asterIII0(2)
     &+ d2ft(asterIII1(2), jup1(2), r, rIIIj)*(dt**2)
     
     
     
     
              rj  = dsqrt((jup1(1) - sol(1))**2 + (jup1(2) - sol(2))**2)
              
              jup(1) = 2d0*jup1(1) - jup0(1) 
     &+ d2fj(jup1(1), rj)*(dt**2)
              jup(2) = 2d0*jup1(2) - jup0(2)
     &+ d2fj(jup1(2), rj)*(dt**2)

                  
                  

              asterI0 = asterI1
                  
              asterI1 = asterI
              
              
              asterII0 = asterII1
                  
              asterII1 = asterII
              
              
              asterIII0 = asterIII1
                  
              asterIII1 = asterIII
              
              
              jup0 = jup1
                  
              jup1 = jup
                  
              write(20, *) asterI(1), asterI(2)
              write(21, *) asterII(1), asterII(2)
              write(22, *) asterIII(1), asterIII(2)
              write(23, *) jup(1), jup(2)
           
          end do
          
          close(20)
          close(21)
          close(22)
          close(23)
      
      
      
          stop
      end program main