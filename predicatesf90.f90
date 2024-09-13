!> === predicatesf90.f90
!>
!> define the orientation test,
!> either from predicates.c or by using a regular cross product
!>
!> ===

module predicatesf90
    use parameters, only: wp

    implicit none

    private
    public  :: orient2df90

#ifdef ROBUST_PREDICATES
    public  :: exactinit
    interface
        function orient2d(pa, pb, pc) bind(C, name="orient2d")
            use, intrinsic :: iso_c_binding, only: c_ptr, c_double
            type(c_ptr),   value, intent(in) :: pa, pb, pc
            real(c_double)                   :: orient2d
        end function orient2d
    end interface

    interface
        subroutine exactinit() bind(C, name="exactinit")
        end subroutine exactinit
    end interface
#endif

contains

#ifdef ROBUST_PREDICATES
    function orient2df90(a, b, c)
        !> returns a negative value if the triangle a,b,c is oriented clockwise, 0 if it is flat
        !> and a positive value if it is oriented counterclockwise
        !>
        !> a,b,c      : three vertices
        !> orient2df90: the result of the intersection test

        use, intrinsic :: iso_c_binding, only: c_ptr, c_loc
        real(wp), dimension(2), target, intent(in) :: a, b, c
        real(wp)                                   :: orient2df90

        type(c_ptr)                                :: pa, pb, pc

        pa = c_loc(a(1))
        pb = c_loc(b(1))
        pc = c_loc(c(1))

        orient2df90 = orient2d(pa, pb, pc)
    end function orient2df90
#else
    function orient2df90(a, b, c)
        !> returns a negative value if the triangle a,b,c is oriented clockwise, 0 if it is flat
        !> and a positive value if it is oriented counterclockwise
        !>
        !> a,b,c      : three vertices
        !> orient2df90: the result of the intersection test

        real(wp), dimension(2), target, intent(in) :: a, b, c
        real(wp)                                   :: orient2df90

        orient2df90 = cross_product(a, b, a, c)
    contains
        function cross_product(A1, B1, A2, B2)
            real(wp), dimension(2), intent(in) :: A1, B1, A2, B2
            real(wp)                           :: cross_product

            real(wp), dimension(2)             :: vec1, vec2

            vec1 = B1-A1
            vec2 = B2-A2

            cross_product = vec1(1)*vec2(2) - vec1(2)*vec2(1)
        end function cross_product
    end function orient2df90
#endif

end module predicatesf90
