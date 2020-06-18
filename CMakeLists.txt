cmake_minimum_required(VERSION 3.16)
project(TAMULearning Fortran)


enable_language(Fortran)
add_executable(TAMULearning main.f90 alloc.f90 grdgen.f90 initia.f90 space1.f90 ugrid2.f90
        upbndry.f90 mod/coord.f90 mod/constants.f90 mod/geom.f90)
file(COPY input.dat DESTINATION ${CMAKE_CURRENT_BINARY_DIR})