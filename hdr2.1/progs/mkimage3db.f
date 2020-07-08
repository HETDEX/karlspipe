
      parameter(nmax=70)
      real xarr(1036,1344),yarr(1036,1344),xflim(1036,1344)
      real flima(1036,1344)
      real arr(nmax,nmax,1036),xin(1344*1036)
      integer naxes(3)
      character file1*40,comm*132
      parameter(pi=3.14159,radtodeg=57.29578)

      open(unit=1,file='radec.in',status='old')
      read(1,*) ra0,dec0
      close(1)
      open(unit=1,file='radec2.dat',status='old')
      read(1,*) x1,x2,pa0
      close(1)

      cdec=cos(dec0/radtodeg)
      cosp=cos(pa0/radtodeg)
      sinp=sin(pa0/radtodeg)

      inum=1
      radcut=2.0
      rfw=1.8
      rsig=rfw/2.35
      w=radcut/rsig
      gaus0=exp(-w*w/2.)

      do i=1,nmax
         do j=1,nmax
            do k=1,1036
               arr(i,j,k)=0.
c               arr2(i,j,k)=0.
            enddo
         enddo
      enddo

      dx=2.0
      nx=31
      ny=31
      nw=1036
      xcen=2.*float(nx)/dx
      xcenwcs=float(nx)/2.

      open(unit=1,file='in',status='old')

      nin=0
      nfib=0
      do j=1,1344*1036
         do i=1,nw
            read(1,*,end=666) x1,x2,x3,x4,x5
            xd=3600.*cdec*(x1-ra0)
            yd=3600.*(x2-dec0)
            xdn=cosp*xd-sinp*yd
            ydn=sinp*xd+cosp*yd
            xarr(i,j)=xdn
            yarr(i,j)=ydn

            flima(i,j)=x5
            if(x4.gt.0.) then
               xflim(i,j)=1./x4
            else
               xflim(i,j)=0.
            endif
            if(x3.gt.4600..and.x3.lt.5000.) then
               nin=nin+1
               xin(nin)=x5
            endif
         enddo
         nfib=nfib+1
      enddo
 666  continue
      close(1)
      print *,"read in",nfib

      call biwgt(xin,nin,xb,xs)
      print *,xb,nin

      naxis=3
      naxes(1)=nx
      naxes(2)=ny
      naxes(3)=nw
      iblock=1
      igc=0
      ier=0

      xoff=xcen
      yoff=xcen
      do iz=1,nw
         do i=1,nx
            xp=float(i)*dx-xoff
            do j=1,ny
               yp=float(j)*dx-yoff
               diff=3.0
               radmin=1e10
               do k=1,nfib
                  x=xarr(iz,k)
                  y=yarr(iz,k)
                  rad=sqrt((xp-x)**2+(yp-y)**2)
                  if(rad.lt.radmin) then
                     radmin=rad
                     kmin=k
                  endif
               enddo
               w=radmin/rsig
               gaus=(exp(-w*w/2.))/gaus0
               if(radmin.lt.radcut) gaus=1.
               apcor=xb/flima(iz,kmin)
               arr(i,j,iz)=xflim(iz,kmin)*gaus/apcor
            enddo
         enddo
      enddo

       pa0=-pa0
       wave0=3470.
       dwave=2.
       radfac=1./(3600./dx)
       cd11=radfac*cos(pa0/radtodeg)
       cd12=-radfac*sin(pa0/radtodeg)
       cd21=radfac*sin(pa0/radtodeg)
       cd22=radfac*cos(pa0/radtodeg)
       
       call ftinit(50,'image3d.fits',iblock,ier)
       call ftphps(50,-32,naxis,naxes,ier)
       call ftpkye(50,'CRVAL1',ra0,5,"RA",ier)
       call ftpkye(50,'CRVAL2',dec0,5,"DEC",ier)
       call ftpkye(50,'CRVAL3',wave0,5,"WAVE",ier)
       call ftpkye(50,'CROTA2',pa0,5,"PA",ier)
       call ftpkye(50,'CD1_1',cd11,5,"",ier)
       call ftpkye(50,'CD1_2',cd12,5,"",ier)
       call ftpkye(50,'CD2_2',cd22,5,"",ier)
       call ftpkye(50,'CD2_1',cd21,5,"",ier)
       call ftpkye(50,'CDELT3',dwave,5,"dWAVE",ier)
       call ftpkye(50,'CRPIX1',xcenwcs,5,"X0",ier)
       call ftpkye(50,'CRPIX2',xcenwcs,5,"Y0",ier)
       call ftpkye(50,'CRPIX3',1.,5,"W0",ier)
       call ftpkys(50,'CUNIT1',"deg","",ier)
       call ftpkys(50,'CUNIT2',"deg","",ier)
       call ftpkys(50,'CTYPE1',"RA---TAN","",ier)
       call ftpkys(50,'CTYPE2',"DEC--TAN","",ier)
       call ftpkys(50,'CTYPE3',"Wave","",ier)
       call ftpkye(50,'APCOR0',xb,5,"Average aperture correction",ier)

       if(ier.ne.0) then
          print *,'Error in output file ',ier
       endif
       print *,naxes(1),naxes(2)
       call ftp3de(50,igc,nmax,nmax,naxes(1),naxes(2),naxes(3),arr,ier)
       call ftclos(50,ier)

c      call ftinit(50,'image3df.fits',iblock,ier)
c      call ftphps(50,-32,naxis,naxes,ier)
c      if(ier.ne.0) then
c         print *,'Error in output file ',ier
c      endif
c      print *,naxes(1),naxes(2)
c      call ftp3de(50,igc,100,100,naxes(1),naxes(2),ntot,arr2,ier)
c      call ftclos(50,ier)

      end
