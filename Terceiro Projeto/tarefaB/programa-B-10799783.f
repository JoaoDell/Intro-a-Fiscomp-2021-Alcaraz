      !declaração de funções para ajudar na simplicidade do código
      
      !função a ser usada
      double precision function dfx(x)       
          implicit double precision(a-h, o-z)
          
          pi = 4d0*datan(1d0)
          
          dfx = dexp(x/2)*dsin(pi*x)
      
          return     
      end function dfx
      
      
      !integral analítica da função a ser usada
      double precision function drefx(a, b)       
          implicit double precision(a-h, o-z)
          
          pi = 4d0*datan(1d0)
          
          drefx = -(2d0*dexp(b/2d0)*(2d0*pi*dcos(pi*b) - dsin(pi*b)))
     &/(1d0 + 4d0*(pi**2d0)) +(2d0*dexp(a/2d0)*(2d0*pi*dcos(pi*a) 
     &- dsin(pi*a)))/(1d0 + 4d0*(pi**2d0))
      
          return     
      end function drefx
      
      
      
      
      !programa
      program main
      
      implicit double precision(a-h, o-z)
      integer rnvalues(10)
      
      double precision evalues(10,3)
      
      rnvalues(1)= 12
      rnvalues(2)= 24
      rnvalues(3)= 48
      rnvalues(4)= 96
      rnvalues(5)= 192
      rnvalues(6)= 384
      rnvalues(7)= 768
      rnvalues(8)= 1536
      rnvalues(9)= 3072
      rnvalues(10)= 6144
      
      open(20, file = "saida-B-10799783.dat")
      
      write(20, 5) "N", "trapezio", "simpson", "boole"
      
      do j = 1, 10
      
      
          a = 0d0
          b = 1d0
          rint = 0d0
          iseg = rnvalues(j)
          dh = 1d0/iseg

          
          write(*,*) "N = ", iseg
          
          
          !regra do trapezio
          do i = 1, iseg
    
              a = a + dh
              rint = rint + (dh/2)*(dfx(a - dh) + 2*dfx(a) +dfx(a + dh))
              
          end do
          
          a = 0d0
          b = 1d0
          
          x = dabs(rint/2 - drefx(a, b))
          evalues(j, 1) = x
          
          write(*,*) "Regra do trapézio:    ", x
          
          
          
          
          !regra de simpson
          rint = 0d0  
          a = 0d0
          dh = 1d0/(2*iseg)
          b = 1d0
          
          do i = 1, iseg
          
              rint = rint + (dh/3)*(dfx(a) + 4*dfx(a + dh)
     & + dfx(a + 2*dh))
              a = a + 2*dh
              !a = a + 2*dh
              
          end do
          
          a = 0d0
          b = 1d0
          
          x = dabs(rint - drefx(a, b))
          evalues(j, 2) = x
          
          write(*,*) "Regra de simpson:     ", x
          
          
          
          
          !regra de simpson 3/8
          rint = 0d0  
          a = 0d0
          b = 1d0
          dh = 1d0/(3*iseg)
          
          do i = 1, iseg
          
              rint = rint + (3*dh/8)*(dfx(a) + 3*dfx(a + dh) 
     & + 3*dfx(a + 2*dh) + dfx(a + 3*dh))
              a = a + 3*dh
          
          end do
          a = 0d0
          b = 1d0
          
          x = dabs(rint - drefx(a, b))
          
          write(*,*) "Regra de simpson 3/8: ", x
          
          
          
          
          !regra de Boole
          rint = 0d0  
          a = 0d0
          b = 1d0
          dh = 1d0/(4*iseg)
          
          do i = 1, iseg
          
              rint  = rint + (2*dh/45)*(7*dfx(a) + 32*dfx(a + dh) 
     &+ 12*dfx(a + 2*dh) + 32*dfx(a + 3*dh) + 7*dfx(a + 4*dh))
    
              
              a = a + 4*dh
          
          end do
          a = 0d0
          b = 1d0
          
          x = dabs(rint - drefx(a, b))
          evalues(j, 3) = x
          
          write(*,*) "Regra de boole:       ", x
          
          write(20, 4) rnvalues(j) ,(evalues(j, k), k = 1, 3)
          write(20, *) " "
          
          
          
          
      end do
      
      write(20, 6) "exatos",drefx(a, b) ,drefx(a, b), drefx(a, b) 
      
      
4     format(I22, 3F22.15)     
5     format(A22, 3A22)
6     format(A22, 3F22.11)

      close(20) 
      
      
      stop
      end program main