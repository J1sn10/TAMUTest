module geom
  implicit none
  save
  real(8) :: l1 ! length b/w inlet & first tooth
  integer :: i1 ! number of intervals the inlet is divided into
  real(8) :: r1 ! radius to wall
  integer :: i21  ! number of intervals the fillet is divided into
  integer :: i22  ! number of intervals the straight tooth side is divided into
  real(8) :: tar  ! tooth angle wrt radial direction
  real(8) :: ctar ! complement of tooth angle tar
  real(8) :: tccd ! distance from tip of tooth to center of circle
  real(8) :: rt   ! tooth fillet radius
  real(8) :: twit ! tooth width at top of tooth (lower part)
  integer :: i3   ! number of intervals the tooth tip is divided into
  real(8) :: dbr  ! distance between radia (fillets)
  integer :: i4   ! no. of intervals the upper part b/w fillets is divided into
  real(8) :: tdpt ! tooth depth = tccd + rt
  real(8) :: l5 ! length b/w last tooth & outlet
  integer :: i5 ! number of intervals the outlet is divided into
  integer :: ntee ! no. of teeth
  real(8) :: angle! angle of the slice of seal, in deg.
  real(8) :: rdown! radius at lower side of domain (r_hub)
end module geom
