        program sparstest
c
        implicit none
c
        integer     MAX_NRA,MAX_NNZA
        parameter(  MAX_NRA     = 10)
        parameter(  MAX_NNZA    = 100)
c
        real*8      dns(5,5)
        integer     m,n
        integer     ia(MAX_NRA+1),ja(MAX_NNZA)
        integer     iao(MAX_NRA+1),jao(MAX_NNZA)
        real*8      a(MAX_NNZA)
        real*8      ao(MAX_NNZA)
c
        integer     i,j,k,ierr
c
        data        dns / 5,0,0,3,0,0,3,0,0,1,0,0,4,1,1,3,0,1,4,0,
     &                    0,1,1,0,2/
c
        do i=1,5
           write(*,'(5f8.1)') (dns(i,k),k=1,5)
        enddo
c
        call dnscsr (5,5,100,dns,5,a,ja,ia,ierr)
c
        call csrssr (5,a,ja,ia,100,ao,jao,iao,ierr)
c
        write(*,*) 'low triangular'
c
        write(*,'( 6I6)')   (iao(k),k=1,6 )
        write(*,'(25I6)')   (jao(k),k=1,25)
        write(*,'(25f6.1)') ( ao(k),k=1,25)
c
        call csrssr2(5,a,ja,ia,100,ao,jao,iao,ierr)
c
        write(*,*) 'upper triangular'
c
        write(*,'( 6I6)')   (iao(k),k=1,6 )
        write(*,'(25I6)')   (jao(k),k=1,25)
        write(*,'(25f6.1)') ( ao(k),k=1,25)
c
        do i=1, 5
           write(*,*) (jao(k),k=iao(i),iao(i+1)-1)
           write(*,*) ( ao(k),k=iao(i),iao(i+1)-1)
        enddo
c
        return
c
        end
