
      parameter (narrm1=14000,narrm2=4000)
      real xd(narrm1,narrm2),xd2(narrm1,narrm2),xd3(narrm1,narrm2)
      real xtrace(narrm1,narrm2),xspec(narrm1,narrm2)
      integer naxes(2),naxest(2)
      character file1*120
      logical simple,extend,anyf

      rcut=3.5
      dflag=0.

 1    call qc1('image ','imar.def',file1)
      call savdef
      iext1=1
      iext2=4

      im1=0
      ier=0
      call ftgiou(im1,ier)
      iread=0
      call ftopen(im1,file1,iread,iblock,ier)
      if(ier.ne.0) then
         write(*,*) 'Error opening image : ',file1
         goto 706
      endif
      call ftmahd(im1,iext1,ihd,ier)
      call ftghpr(im1,2,simple,ibit,naxis,naxes,ipc,igc,extend,ier)
      if(naxis.eq.1) naxes(2)=1
      if(naxes(1).gt.narrm1.or.naxes(2).gt.narrm2) then
         print *,naxes(1),naxes(2)
         write(*,"('Arrays too small - make narrm bigger')")
         goto 706
      endif
      ncol=naxes(1)
      nrow=max(1,naxes(2))
      call ftg2de(im1,igc,0.,narrm1,ncol,nrow,xd,anyf,ier)

      call ftmahd(im1,iext2,ihd,ier)
      call ftghpr(im1,2,simple,ibit,naxis,naxes,ipc,igc,extend,ier)
      ncol=naxes(1)
      nrow=naxes(2)
      call ftg2de(im1,igc,0.,narrm1,ncol,nrow,xd2,anyf,ier)

      call ftmahd(im1,13,ihd,ier)
      call ftghpr(im1,2,simple,ibit,naxis,naxest,ipc,igc,extend,ier)
      ncolt=naxest(1)
      nrowt=naxest(2)
      call ftg2de(im1,igc,0.,narrm1,ncolt,nrowt,xtrace,anyf,ier)

      call ftmahd(im1,11,ihd,ier)
      call ftghpr(im1,2,simple,ibit,naxis,naxest,ipc,igc,extend,ier)
      ncolt=naxest(1)
      nrowt=naxest(2)
      call ftg2de(im1,igc,0.,narrm1,ncolt,nrowt,xspec,anyf,ier)

      do j=1,nrow
         do i=1,ncol
            if(xd(i,j).ne.dflag) then
               ymin=1e10
               do jt=1,nrowt
                  diff=abs(float(j)-xtrace(i,jt))
                  if(diff.lt.ymin) then
                     ymin=diff
                     jspec=jt
                  endif
c                  ymin=min(ymin,abs(float(j)-xtrace(i,jt)))
               enddo
               sum=xspec(i,jspec)
               if(xd2(i,j).ne.0..and.sum.ne.0.) then
                  xd3(i,j)=(xd(i,j)-xd2(i,j))/sum+1.
                  xd(i,j)=xd(i,j)/xd2(i,j)
c                  print *,j,i,sum,xd3(i,j)
               else
                  xd(i,j)=1.
                  xd3(i,j)=1.
               endif
               if(ymin.gt.rcut) then
                  xd(i,j)=1.
                  xd3(i,j)=1.
               endif
            else
               xd(i,j)=1.
               xd3(i,j)=1.
            endif
         enddo
      enddo

      call ftclos(im1,ier)

c-- open the output file
      ier=0
      call ftgiou(im1,ier)
      call ftinit(im1,'imar.fits',iblock,ier)
      call ftphps(im1,-32,naxis,naxes,ier)
      call ftp2de(im1,igc,narrm1,naxes(1),naxes(2),xd,ier)
      call ftclos(im1,ier)

      ier=0
      call ftgiou(im1,ier)
      call ftinit(im1,'imar2.fits',iblock,ier)
      call ftphps(im1,-32,naxis,naxes,ier)
      call ftp2de(im1,igc,narrm1,naxes(1),naxes(2),xd3,ier)
      call ftclos(im1,ier)

 706  continue
      end
