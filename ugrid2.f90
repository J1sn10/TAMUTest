SUBROUTINE ugrid(ugridfile,imax,jmax,kmax,x,y,z)
  !
  !  Generates ugrid file - cropped from ugrid_orig.f90
  !
  !  pgc - 5/5/20
  !
  IMPLICIT NONE

  INTEGER imax,jmax,kmax
  REAL(8) x(imax,jmax,kmax)
  REAL(8) y(imax,jmax,kmax)
  REAL(8) z(imax,jmax,kmax)
  INTEGER ii,jj,kk,index, i, j, jnd
  INTEGER nnode,ncell,nbface
  REAL(8),ALLOCATABLE :: xnd(:),ynd(:),znd(:)
  INTEGER,ALLOCATABLE :: ijk(:,:,:)
  INTEGER,ALLOCATABLE :: ip_bface(:,:),np_bface(:),idbcs(:)
  INTEGER,ALLOCATABLE :: ip_cell(:,:),np_cell(:)
  character(*) ugridfile
  integer, parameter :: io=999
  !---------------------------------------------------------------------------
  nnode = imax * jmax * kmax
  ncell = (imax-1) * (jmax-1) * (kmax-1)
  nbface = 2*(imax-1)*(jmax-1) + 2*(jmax-1)*(kmax-1) + &
       2*(kmax-1)*(imax-1)

  ALLOCATE(xnd(nnode),ynd(nnode),znd(nnode))
  ALLOCATE(ijk(imax,jmax,kmax))
  ALLOCATE(ip_bface(nbface,4),np_bface(nbface),idbcs(nbface))
  ALLOCATE(ip_cell(ncell,8),np_cell(ncell))

  !=============================  NODES  ===========================

  index = 0

  DO kk = 1,kmax
     DO jj = 1,jmax 
        DO ii = 1,imax
           index = index + 1
           xnd(index)    = x(ii,jj,kk)
           ynd(index)    = y(ii,jj,kk)
           znd(index)    = z(ii,jj,kk)
           ijk(ii,jj,kk) = index
        END DO
     END DO
  END DO
  !===========================  BOUNDARIES  ========================

  index = 0

  ! NEGATIVE Z-FACE
  kk = 1
  DO jj = 1,jmax-1
     DO ii = 1,imax-1
        index = index + 1
        np_bface(index) = 4
        ip_bface(index,1) = ijk(ii  , jj  , kk)
        ip_bface(index,2) = ijk(ii+1, jj  , kk)
        ip_bface(index,3) = ijk(ii+1, jj+1, kk)
        ip_bface(index,4) = ijk(ii  , jj+1, kk)
        idbcs(index) = -100 ! master
     END DO
  END DO

  ! POSITIVE Z-FACE
  kk = kmax
  DO jj = 1,jmax-1
     DO ii = 1,imax-1
        index = index + 1
        np_bface(index) = 4
        ip_bface(index,1) = ijk(ii  , jj  , kk)
        ip_bface(index,2) = ijk(ii+1, jj  , kk)
        ip_bface(index,3) = ijk(ii+1, jj+1, kk)
        ip_bface(index,4) = ijk(ii  , jj+1, kk)
        idbcs(index) = 0 ! slave
     END DO
  END DO

  ! BOTTOM WALL (-Y)
  jj = 1
  DO kk = 1,kmax-1
     DO ii = 1,imax-1
        index = index + 1
        np_bface(index) = 4
        ip_bface(index,1) = ijk(ii  , jj  , kk  )
        ip_bface(index,2) = ijk(ii  , jj  , kk+1)
        ip_bface(index,3) = ijk(ii+1, jj  , kk+1)
        ip_bface(index,4) = ijk(ii+1, jj  , kk  )
        idbcs(index) = -4
     END DO
  END DO

  ! TOP WALL (+Y)
  jj = jmax
  DO kk = 1,kmax-1
     DO ii = 1,imax-1
        index = index + 1
        np_bface(index) = 4
        ip_bface(index,1) = ijk(ii  , jj  , kk  )
        ip_bface(index,2) = ijk(ii  , jj  , kk+1)
        ip_bface(index,3) = ijk(ii+1, jj  , kk+1)
        ip_bface(index,4) = ijk(ii+1, jj  , kk  )
        idbcs(index) = -3
     END DO
  END DO

  ! INLET (-X)
  ii = 1
  DO kk = 1,kmax-1
     DO jj = 1,jmax-1
        index = index + 1
        np_bface(index) = 4
        ip_bface(index,1) = ijk(ii, jj  , kk  )
        ip_bface(index,2) = ijk(ii, jj+1, kk  )
        ip_bface(index,3) = ijk(ii, jj+1, kk+1)
        ip_bface(index,4) = ijk(ii, jj  , kk+1)
        idbcs(index) = -1
     END DO
  END DO

  ! OUTLET (+X)
  ii = imax
  DO kk = 1,kmax-1
     DO jj = 1,jmax-1
        index = index + 1
        np_bface(index) = 4
        ip_bface(index,1) = ijk(ii, jj  , kk  )
        ip_bface(index,2) = ijk(ii, jj+1, kk  )
        ip_bface(index,3) = ijk(ii, jj+1, kk+1)
        ip_bface(index,4) = ijk(ii, jj  , kk+1)
        idbcs(index) = -2
     END DO
  END DO

  !==========================  CELLS  ==============================

  index = 0

  DO kk = 1,kmax-1
     DO jj = 1,jmax-1
        DO ii = 1,imax-1
           index = index + 1
           np_cell(index) = 8
           ip_cell(index,1) = ijk(ii  , jj  , kk  )
           ip_cell(index,2) = ijk(ii+1, jj  , kk  )
           ip_cell(index,3) = ijk(ii+1, jj+1, kk  )
           ip_cell(index,4) = ijk(ii  , jj+1, kk  )
           ip_cell(index,5) = ijk(ii  , jj  , kk+1)
           ip_cell(index,6) = ijk(ii+1, jj  , kk+1)
           ip_cell(index,7) = ijk(ii+1, jj+1, kk+1)
           ip_cell(index,8) = ijk(ii  , jj+1, kk+1)
        END DO
     END DO
  END DO

  !=======================  WRITE OUT  =============================

  OPEN(1,file=ugridfile)
  WRITE(1,*) nnode,0,nbface,0,0,0,ncell

  DO ii = 1,nnode
     WRITE(1,*) xnd(ii),ynd(ii),znd(ii)
  END DO

  DO ii = 1,nbface
     WRITE(1,*) (ip_bface(ii,jj),jj=1,4)
  END DO
  DO ii = 1,nbface
!     write(102,*) ii, idbcs(ii)
     WRITE(1,*) idbcs(ii)
  END DO

  DO ii = 1,ncell
     WRITE(1,*) (ip_cell(ii,jj),jj=1,np_cell(ii))
  END DO

  CLOSE(1,STATUS='KEEP')

  ! Write a file with boundary face values
  open(io, file='output/idbcs0.tec')
  write(io, *) "variables = x, y, z, bc"
  write(io, *) "zone f = fepoint, et = quadrilateral, n = " , nbface*4, &
       ", e = ", nbface

  do i = 1, nbface
     do j = 1, 4
        jnd = ip_bface(i, j)
        if (jnd .eq. 0)  jnd = ip_bface(i, 3)
        write(io, *) xnd(jnd), ynd(jnd), znd(jnd), idbcs(i)
     end do
  end do

  do i = 1, nbface
     write(io,*) (4*(i-1) + j, j = 1, 4)
  end do
  close(io)

  deallocate(xnd,ynd,znd,ijk,ip_bface,np_bface,idbcs,ip_cell,np_cell)

END SUBROUTINE ugrid
