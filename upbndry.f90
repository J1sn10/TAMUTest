subroutine upbndry
  !
  ! Generates the upper boundary of seal
  !
  ! pgc - 4/29/20
  use coord,only: imax, jmax, x, y
  use geom, only: r1, rt, tar, ctar, l1, i1, i21, i22, i3, i4, dbr, tccd, &
       twit, l5, i5, ntee

  implicit none
  integer :: i, ii, j, status
  real(8) :: xtang, ytang, wtg, dx, dy, dxt, th, l22, htt, dt, ds, ddbr
  real(8) :: xtop, ytop, xcent, ycent, leng, lbegin
  real(8), allocatable :: xs(:)
  !-----------------------------------------------------------------------------
  dt = ctar / real(i21)
  leng = rt * dt
!  leng = l1 / real(i1) ! uniform grid

  allocate(xs(i1+1), stat = status)
  if (status /= 0) stop 'failure to allocate memory 1 in upbndry'

  call space1(l1, leng, xs, i1+1) ! spreads i1+1 points on length l1

  x(1,jmax,1) = 0.d0
  y(1,jmax,1) = r1
  do i = 2, i1+1
     x(i,jmax,1) = l1 - xs(i1+1-(i-1))
     y(i,jmax,1) = r1
  end do
  ii = i1+1

  deallocate(xs, stat = status)
  if (status /= 0) stop 'failure to deallocate memory 1 in upbndry'

  teeth: do j = 1, ntee
     ! i21 - sector of circle downwards
     th = 0.d0; xtop = x(ii,jmax,1); ytop = y(ii,jmax,1)
     do i = 1, i21
        th = th + dt
        x(ii+1,jmax,1) = xtop + rt * sin(th)
        y(ii+1,jmax,1) = ytop - rt * (1.d0 - cos(th))
        ii = ii + 1
     end do
     ! tooth straight line downwards
     wtg = tan(tar) * (tccd + rt * sin(tar))
     htt = tccd + rt * sin(tar)
     l22 = sqrt(htt**2 + wtg**2)
     ds  = l22 / real(i22)
     dx  = wtg / real(i22)
     dy  = htt / real(i22)
     do i = 1, i22
        x(ii+1,jmax,1) = x(ii,jmax,1) + dx
        y(ii+1,jmax,1) = y(ii,jmax,1) - dy
        ii = ii + 1
     end do
     ! top of tooth
     dxt = twit / real(i3)
     do i = 1, i3
        x(ii+1,jmax,1) = x(ii,jmax,1) + dxt
        y(ii+1,jmax,1) = y(ii,jmax,1)
        ii = ii + 1
     end do
     ! tooth straight line upwards
     do i = 1, i22
        x(ii+1,jmax,1) = x(ii,jmax,1) + dx
        y(ii+1,jmax,1) = y(ii,jmax,1) + dy
        ii = ii + 1
     end do
     ! sector of circle upwards
     xtang = x(ii,jmax,1); ytang = y(ii,jmax,1)
     xcent = xtang + rt * cos(tar); ycent = ytang - rt * sin(tar)
     th = tar; dt = ctar / real(i21)
     do i = 1, i21
        th = th + dt
        x(ii+1,jmax,1) = xcent - rt * cos(th)
        y(ii+1,jmax,1) = ycent + rt * sin(th)
        ii = ii + 1
     end do
     ! space between tangent points
     if (j == ntee) cycle
     ddbr = dbr / real(i4)
     do i = 1, i4
        x(ii+1,jmax,1) = x(ii,jmax,1) + ddbr
        y(ii+1,jmax,1) = y(ii,jmax,1)
        ii = ii + 1
     end do
  end do teeth
  ! space between last tooth and outlet
  leng = sqrt((x(ii-1,jmax,1)-x(ii,jmax,1))**2 + &
       (y(ii-1,jmax,1)-y(ii,jmax,1))**2) * 1.2 ! give it a 1.2 growth

!  leng = l5 / real(i5) ! uniform grid

  allocate(xs(i5+1), stat = status)
  if (status /= 0) stop 'failure to allocate memory in upbndry'

  call space1(l5, leng, xs, i5+1) ! spreads i5+1 points on length l5 

  lbegin = x(ii,jmax,1)
  do i = 1, i5
     x(ii+1,jmax,1) = lbegin + xs(i+1)
     y(ii+1,jmax,1) = r1
     ii = ii + 1
  end do

  if (ii /= imax) then
     print *, 'Mismatch in upbndry', ii, '<>', imax
     stop 'Terminated in upbndry'
  end if

  open(1, file='output/upbndry.dat')
  do i = 1, ii
     write(1,*) x(i,jmax,1), y(i,jmax,1)
  end do
  close(1)

end subroutine upbndry
