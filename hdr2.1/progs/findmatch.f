
      parameter(nmax=1000000)
      real w(nmax),xf(nmax),sn(nmax),sb(nmax),sr(nmax)
      real*8 dra(nmax),ddec(nmax),drad,dx1,dx2
      integer id(nmax)
      character a1*3,a8*12,aname(nmax)*12

      parameter(radtodeg=57.29578)

      rad=4.0

      open(unit=1,file='rext1',status='old')

      n=0
      do i=1,nmax
         read(1,*,end=666) a1,dx1,dx2,i4,i5,i6,i7,a8,x9,x10,x11,x12,
     $        x13,x14,x15,x16
         n=n+1
         dra(n)=dx1
         ddec(n)=dx2
         sb(n)=x15
         sr(n)=x16
         sn(n)=sb(n)+sr(n)
         id(n)=i7
         aname(n)=a8
      enddo
 666  continue
      close(1)

      cosd=cos(sngl(ddec(1))/radtodeg)

      do i=1,n-1
         do j=i+1,n
            drad=(dble(cosd)*(dra(i)-dra(j)))**2+(ddec(i)-ddec(j))**2
            drad=3600.d0*dsqrt(drad)
            radc=sngl(drad)
            if(radc.lt.rad.and.aname(i).eq.aname(j)) then
               write(*,1011) id(i),id(j),radc,sn(i),sn(j),
     $              aname(i),aname(j)
            endif
         enddo
      enddo

 1011 format(i6,1x,i6,1x,f7.3,1x,f11.1,1x,f11.1,2(1x,a12))
      end
