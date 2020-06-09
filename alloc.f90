subroutine alloc
    !
    ! Allocates arrays
    !
    ! pgc - 5/15/20
    use coord, only: imax, jmax, kmax, x, y, z

    implicit none

    ! local
    integer :: status
    !---------------------------------------------------------------------------
    allocate(x(imax,jmax,kmax), y(imax,jmax,kmax), z(imax,jmax,kmax), stat=status)
    if (status /= 0) stop 'failure to allocate memory 1 alloc'
end subroutine alloc

