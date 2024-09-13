!> === parameters.f90
!>
!> define some parameters that are useful for the whole program
!>
!> ===

module parameters
    use iso_fortran_env, only: real64
    implicit none
    
    private
    public  :: wp, MAX_VERTICES

    integer, parameter :: wp = real64       !> working precision
    integer, parameter :: MAX_VERTICES = 12 !> maximum number of vertices for polygons

contains

end module parameters
