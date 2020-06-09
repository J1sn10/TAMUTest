subroutine initia
  !
  ! Initializes seal dimensions
  !
  ! pgc - 4/29/20
  use coord,only: imax, jmax, kmax
  use geom, only: r1, rt, tar, ctar, l1, i1, i21, i22, i3, i4, dbr, tccd, twit, &
       tdpt, l5, i5, ntee, angle, rdown
  use constants, only: deg2rad
  
  implicit none
  integer :: istat
  real(8) :: tad, ctad
  namelist /input_data/ r1, rt, l1, dbr, twit, tad, tdpt, i1, i21, i22, i3, i4, &
       l5, i5, ntee, angle, rdown, jmax, kmax
  !-----------------------------------------------------------------------------
  ! input_data default values
  
  r1   = 61.646d0  ! radius
  rt   = 1.016d0
  l1   = 30.0736d0
  dbr  = 0.18d0
  twit = 0.254
  tad  = 15.d0
  tdpt = 4.293d0
  i1   = 15
  i21  = 9
  i22  = 15
  i3   = 3
  i4   = 2
  l5   = 30.0736d0
  i5   = 15
  ntee = 6
  angle= 2.d0
  rdown= 57.15d0
  jmax = 19
  kmax = 7

  open (1, file='input.dat', status = 'old', iostat = istat)
  if (istat /=0) then
     print *, 'Dude, I could not find file input.dat'
     stop 'I am terminating in initia - Bummer'
  end if
  read (1, input_data); print *, 'read input_data'
  close(1)

  if (mod(jmax,2) == 0) then
     jmax = jmax + 1
     print *, 'jmax increased by 1 to be odd number', jmax
  end if
  tccd = tdpt - rt

  ctad = 90.d0 - tad
  tar  = tad  * deg2rad
  ctar = ctad * deg2rad

  imax = i1 + ntee * ((i21 + i22) * 2 + i3 + i4) - i4 + i5 + 1 ! # elements+1
  print *, 'imax=', imax, ' jmax=', jmax, ' kmax=', kmax
  print *, 'Grid nodes =', imax * jmax * kmax
end subroutine initia
