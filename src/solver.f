module gauss_elimination
    implicit none

contains
    subroutine gauss(m,n,a,b)
        ! Gaussian elimination.
        integer*8 m,n,i,j,k
        complex*16 a(m,m),b(m)
        ! Downward elimination.
        do i=1,n
            if(i.lt.n)call pivot(m,n,i,a,b)
            a(i,i)=1.0d0/a(i,i)
            b(i)=b(i)*a(i,i)
            if(i.lt.n)then
                do j=i+1,n
                    a(i,j)=a(i,j)*a(i,i)
                end do
                do k=i+1,n
                    b(k)=b(k)-a(k,i)*b(i)
                    do j=i+1,n
                        a(k,j)=a(k,j)-a(k,i)*a(i,j)
                    end do
                end do
            end if
        end do
        ! Back substitution.
        do i=n-1,1,-1
            do j=i+1,n
                b(i)=b(i)-a(i,j)*b(j)
            end do
        end do
    end subroutine

    subroutine pivot(m,n,i,a,b)
        !Rows are interchanged for stability.
        integer*8 m,n,i,j,i0
        real*8 amp0,amp
        complex*16 temp,a(m,m),b(m)
        i0=i
        amp0=cdabs(a(i,i))
        do j=i+1,n
            amp=cdabs(a(j,i))
            if(amp.gt.amp0)then
                i0=j
                amp0=amp
            end if
        end do
        if(i0.eq.i)return

        temp=b(i)
        b(i)=b(i0)
        b(i0)=temp
        do j=i,n
            temp=a(i,j)
            a(i,j)=a(i0,j)
            a(i0,j)=temp
        end do
    end subroutine
