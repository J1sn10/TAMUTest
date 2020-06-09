cmake_minimum_required(VERSION 3.16)
project(TAMLearning Fortran)


enable_language(Fortran)
add_executable(TAMLearning main.f90 alloc.f90 grdgen.f90 initia.f90 space1.f90 ugrid2.f90
        upbndry.f90 mod/coord.f90 mod/constants.f90 mod/geom.f90)
file(COPY dat/adolfo2/input.dat DESTINATION ${CMAKE_CURRENT_BINARY_DIR})