program sealmesh
    !
    ! Generates mesh for a generic seal
    !
    ! pgc - 4/29/20
    implicit none
    !-----------------------------------------------------------------------------
    call initia
    call alloc
    call upbndry
    call grdgen
end program sealmesh