subroutine grdgen
  !
  ! Generates grid given the up boundary and shaft location
  !
  ! pgc - 5/15/20
  use constants, only: deg2rad
  use coord, only: imax, jmax, kmax, x, y, z
  use geom,  only: angle, rdown

  implicit none
  !
  ! local
  real(8) :: dist, delt, theta, cost, sint, y1, distmin, factor, yy
  real(8), allocatable :: ys(:)
  integer :: i, j, k, j2, status
  character(10) :: ugridfilename
  !---------------------------------------------------------------------------
  if (.true.) write(*,*) 'calling grdgen'

  j2 = (jmax + 1) / 2 ! number of grid points for half spacing
  allocate (ys(j2), stat = status)
  if (status /= 0) stop 'failure to allocate memory in grdgen'

  ! find a common y1 based on smallest distance
  distmin = (y(1,jmax,1) - rdown) * 0.5d0
  do i = 2, imax
     dist = (y(i,jmax,1) - rdown) * 0.5d0  ! dist for 2*half
     if (dist < distmin) distmin = dist
  end do
  factor = 15.0d0 ! if 1, then equally spaced
  y1 = distmin / (real(j2-1) * factor) ! y1 smaller than equally spaced if f>1
  ! an alternative is to specify y1 as input parameter
  
  do i = 1, imax
     dist = (y(i,jmax,1) - rdown) * 0.5d0  ! dist for half 
!     y1   = dist / (real(j2-1) * 2) ! for equal spacing
     call space1(dist, y1, ys, j2)
     do j = 1, j2
        x(i,j,1) = x(i,jmax,1)
        y(i,j,1) = rdown + ys(j)
     end do
     do j = 2, j2 - 1
        x(i,jmax-j+1,1) = x(i,jmax,1)
        y(i,jmax-j+1,1) = y(i,jmax,1) - ys(j)
     end do
     ! dist = (y(i,jmax,1) - rdown) / real(jmax-1) ! for equal spacing
     ! do j = 1, jmax-1
     !    x(i,j,1) = x(i,jmax,1)
     !    y(i,j,1) = rdown + dist * (j-1)
     ! end do
  end do
     
  deallocate (ys, stat = status)
  if (status /= 0) stop 'failure to deallocate memory in grdgen'

  ! 2D Grid written in Plot3D format
  open (8, file='output/slice.x') ! Plot3D format data
  write(8,*) imax, jmax
  write(8,*) ((x(i,j,1), i=1,imax), j=1,jmax), &
             ((y(i,j,1), i=1,imax), j=1,jmax)
  close(8)

  ! 3D grid
  ! z = 0.d0
  ! delt = angle * deg2rad / real(kmax-1)
  ! do k = 2, kmax
  !    theta = delt * real(k-1)
  !    cost = cos(theta); sint = sin(theta)
  !    do j = 1, jmax
  !       do i = 1, imax
  !          x(i,j,k) = x(i,j,1)
  !          y(i,j,k) = y(i,j,1) * cost
  !          z(i,j,k) = y(i,j,1) * sint
  !       end do
  !    end do
  ! end do
  
  z = 0.d0
  delt = angle * deg2rad / real(kmax-1)
  do k = 1, kmax
     theta = -angle * deg2rad * 0.5d0 + delt * real(k-1)
     cost = cos(theta); sint = sin(theta)
     do j = 1, jmax
        do i = 1, imax
           x(i,j,k) = x(i,j,1)
           yy       = y(i,j,1) * cost
           z(i,j,k) = y(i,j,1) * sint
           y(i,j,k) = yy
        end do
     end do
  end do

  ! 3D Grid written in Plot3D format
  open (8, file='output/sector.xyz') ! Plot3D format data
  write(8,*) imax, jmax, kmax
  write(8,*) (((x(i,j,k), i=1,imax), j=1,jmax), k=1,kmax), &
             (((y(i,j,k), i=1,imax), j=1,jmax), k=1,kmax), &
             (((z(i,j,k), i=1,imax), j=1,jmax), k=1,kmax)
  close(8)

  ! Write ugrid file
  ugridfilename = 'output/csim.ugrid'
  call ugrid(ugridfilename,imax,jmax,kmax,x,y,z)

end subroutine grdgen
