
      parameter (narrm1=15000,narrm2=15000)
      real xd(narrm1,narrm2),xin(100000),xprof(15)
      integer naxes(2)
      character file1*120
      logical simple,extend,anyf

c- for cxbt
      nx1=733
      ny1=1
      nx2=733
      ny2=112
c- for cwbt
      nx1=20
      ny1=2
      nx2=1000
      ny2=111

      read *,itype
      if(itype.eq.2) then
         read *,nx1,nx2,ny1,ny2
      elseif(itype.eq.3) then
         read *,ifib,icol
      elseif(itype.eq.4) then
         read *,wmin,wmax
      elseif(itype.eq.5) then
         read *,nx1,ny1,nx2,ny2
      endif

      iext=1

      open(unit=1,file='list',status='old')
      open(unit=11,file='out',status='unknown')

      do ia=1,10000
         read(1,*,end=666) file1
         if(itype.eq.4) then
            open(unit=2,file=file1,status='old')
            sum=0.
            nin=0
            do i=1,10000
               read(2,*,end=555) x1,x2
               if(x1.gt.wmin.and.x1.lt.wmax) then
                  nin=nin+1
                  sum=sum+x2
               endif
            enddo
 555        continue
            close(2)
            if(sum.gt.0.and.nin.gt.0) then
               sum=sum/float(nin)
            else
               sum=0.
            endif
            write(11,1103) sum,file1
         else

         im1=51
         ier=0
c         call ftgiou(im1,ier)
         iread=0
         call ftopen(im1,file1,iread,iblock,ier)
         if(ier.ne.0) then
            call ftclos(im1,ier)
            write(*,*) 'Error opening image : ',file1
            goto 706
         endif
         call ftmahd(im1,iext,ihd,ier)
         ier=0
         call ftghpr(im1,2,simple,ibit,naxis,naxes,ipc,igc,extend,ier)
         if(naxis.eq.1) naxes(2)=1
         ncol=naxes(1)
         nrow=naxes(2)
         call ftg2de(im1,igc,0.,narrm1,ncol,nrow,xd,anyf,ier)
         call ftclos(im1,ier)

         if(itype.eq.1) then
            p1=xd(nx1,ny1)
            p2=xd(nx2,ny2)
            print *,p1,p2-p1,file1
         elseif(itype.eq.2) then
            nin=0
            do j=ny1,ny2
               do i=nx1,nx2
                  nin=nin+1
                  xin(nin)=xd(i,j)
               enddo
            enddo
            call biwgt(xin,nin,xb,xs)
            write(11,1101) xb,xs,file1
         elseif(itype.eq.3) then
            do k=1,15
               jp=(ifib-1)*15+k
               xprof(k)=xd(icol,jp)
            enddo
            write(11,1102) (xprof(i),i=1,15),file1
         elseif(itype.eq.5) then
            p1=xd(nx1,ny1)
            p2=xd(nx2,ny2)
            if(p1.gt.0.and.p1.le.2.0.and.p2.gt.0.and.p2.le.2.0) then
               write(11,1104) p2/p1,p1,file1
            else
               write(11,1104) 0.,0.,file1
            endif
         elseif(itype.eq.6) then
            p1=xd(nx1,ny1)
            p2=xd(nx2,ny2)
            if(p1.gt.-50.and.p1.le.50.and.p2.gt.-50.and.p2.le.50.) then
               write(11,1105) abs(p2-p1),p1,file1
            else
               write(11,1105) 0.,0.,file1
            endif
         elseif(itype.eq.7) then
            p1=xd(nx1,ny1)
            p2=xd(nx2,ny2)
            write(11,1105) abs(p2-p1),p1,file1
         endif
 706     continue
         endif
      enddo
 666  continue
      close(1)
      close(11)

 1101 format(f9.3,1x,f9.3,2x,a80)
 1102 format(15(f5.3,1x),a80)
 1103 format(f5.3,1x,a80)
 1104 format(f5.3,1x,f5.3,1x,a80)
 1105 format(f7.2,1x,f6.2,1x,a80)

      end
