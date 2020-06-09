module coord
  implicit none
  save
  integer :: imax, jmax, kmax
  real(8), allocatable :: x(:,:,:), y(:,:,:), z(:,:,:)
end module coord
