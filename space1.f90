subroutine space1(smax, seta, sval, nmax)
  ! Given the total length smax of the curve, the lenght seta of the first
  ! segment and the total number of points nmax along the curve, this
  ! routine calculates the growth ratio rold and then the lenght of
  ! each segment.  It uses a Newtow-Raphson iteration method.
  implicit none
  ! input 
  integer, intent(in) :: nmax
  real(8), intent(in) :: smax, seta
  ! input-output
  real(8), intent(inout) :: sval(nmax)
  ! local
  integer :: nmaxm2, niter, n
  real(8) :: fval ! function value
  real(8) :: fprm ! derivative of function value
  real(8) :: rold ! ratio between lenght of s_i+1 and s_i
  real(8) :: az
  real(8) :: drold ! delta r (variation of ratio r between iterations) 
  real(8) :: err   ! exit criterion for Newton-Raphson iteration
  !--------------------------------------------------------------------
  err    = 1.0d-5
  nmaxm2 = nmax-2
  rold   = 1.1d0    ! initial guess for ratio r
  do niter = 1, 10000
     fval = 0.0
     fprm = 0.0
     do n = 1, nmaxm2
        az   = rold**float(n-1)
        fval = fval + az
        fprm = fprm + float(n) * az
     end do
!     fval1 = (rold**real(n-1) - 1.0d0) / (rold - 1.0d0)
     fval = seta * (fval + rold**nmaxm2) - smax
     fprm = seta * fprm
     drold=-fval / fprm
     rold = rold + drold  
     if (abs(drold) < err) go to 30
  end do
  write (*,*) 'diverged in space1; rold=', rold, ' drold=', drold
  write (*,*) 'input values: smax =', smax, 'seta =', seta, 'nmax = ', nmax
  stop
30 continue
  if (rold < 0.7d0 .or. rold > 1.3d0) then
     write(*,*) 'In space1', rold, ' is a poor ratio of adjacent element sizes'
     write(*,*) 'Consider changing dh, jomax, dsbod or dteob, ilmax'
     !pause ! NRM
  end if
  sval(1) = 0.0
  do n = 2, nmax
     sval(n) = sval(n-1) + seta * rold**(n-2)
  end do
  return
end subroutine space1
