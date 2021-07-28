      program main
      
      
          integer N
          real r
          
          
          N = 10000000
          r = 0.0
          
          
          call srand(1001)
          
          
          
          do i = 1, 4
              r = 0.0
              do j = 0, N
                  r = r + rand()**i
              end do
              write(*,*) "i = ", i, "<x**i> = ", r/N
          end do
          
          
         
      
      
          stop
      end program main