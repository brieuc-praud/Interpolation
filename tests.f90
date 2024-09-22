!> === tests.f90
!>
!> define and perform some tests on mesh intersections
!> and the conservative interpolation
!>
!> ===

program tests
    use parameters,      only: wp
    use polygon_tools,   only: overlapping_area, polygon_area
    use mesh,            only: T_mesh, read_mesh, write_mesh, clean
    use interpolation,   only: conservative_interpolation
    use iso_fortran_env, only: output_unit, error_unit
#ifdef ROBUST_PREDICATES
    use predicatesf90,   only: exactinit
#endif

    implicit none

    type(T_Mesh)       :: Mesh1, Mesh2
    character(len=256) :: source_mesh_file, target_mesh_file
    real(wp)           :: t1,t2

    if (command_argument_count() /= 2) error stop 'invalid number of arguments (2 arguments expected)'

    call get_command_argument(1, source_mesh_file)
    call get_command_argument(2, target_mesh_file)

#ifdef ROBUST_PREDICATES
    call exactinit()
#endif

    call read_mesh(Mesh1, source_mesh_file)
    !call TEST_self_intersection(Mesh1)
    call read_mesh(Mesh2, target_mesh_file)
    !call TEST_self_intersection(Mesh2)

    !call TEST_cross_intersection(Mesh1, Mesh2)
    !call TEST_cross_intersection(Mesh2, Mesh1)

    call cpu_time(t1)
    call conservative_interpolation(Mesh1, Mesh2)
    call cpu_time(t2)
    write(output_unit,*) "interpolation error:", TEST_compute_error(Mesh1, Mesh2)
    write(output_unit,*) "computation time:"   , t2-t1

    call write_mesh(Mesh2, target_mesh_file)

    call clean(Mesh1)
    call clean(Mesh2)

contains

    subroutine TEST_self_intersection(Mesh)
        !> mesh self-intersection test
        !>
        !> Mesh: mesh to self-intersect

        type(T_Mesh), intent(in) :: Mesh

        real(wp) :: total_area, overlap_area, real_area
        integer  :: npolygons
        integer  :: i,j

        npolygons = size(Mesh%polygons, dim=2)
        do i=1,npolygons
            total_area = 0._wp
            associate(si => Mesh%sizes(i))
                do j=1,npolygons
                    associate(sj => Mesh%sizes(j))
                        overlap_area = overlapping_area(Mesh%vertices(:,:), Mesh%polygons(:si,i), &
                            & Mesh%vertices(:,:), Mesh%polygons(:sj,j))
                    end associate
                    total_area = total_area + overlap_area
                end do
                real_area = polygon_area(Mesh%vertices, Mesh%polygons(:si,i))
            end associate
            if (abs(total_area - real_area) > epsilon(total_area)) then
                write(error_unit,*) "TEST_self_intersection -> fail"
                write(error_unit,*) "index", i
                write(error_unit,*) "computed", total_area
                write(error_unit,*) "real", real_area
                error stop
            end if
        end do
        write(output_unit,*) "TEST_self_intersection -> success"
    end subroutine TEST_self_intersection

    subroutine TEST_cross_intersection(Mesh1, Mesh2)
        !> mesh cross-intersection test
        !>
        !> Mesh1, Mesh2: meshes to intersect

        type(T_Mesh), intent(in) :: Mesh1, Mesh2

        real(wp) :: total_area, overlap_area, real_area
        integer  :: n1,n2
        integer  :: i,j

        n1 = size(Mesh1%polygons, dim=2)
        n2 = size(Mesh2%polygons, dim=2)
        do i=1,n2
            total_area = 0._wp
            associate(si => Mesh2%sizes(i))
                do j=1,n1
                    associate(sj => Mesh1%sizes(j))
                        overlap_area = overlapping_area(Mesh2%vertices(:,:), Mesh2%polygons(:si,i), &
                            & Mesh1%vertices(:,:), Mesh1%polygons(:sj,j))
                        total_area = total_area + overlap_area
                    end associate
                end do
                real_area = polygon_area(Mesh2%vertices, Mesh2%polygons(:si,i))
            end associate
            if (abs(total_area - real_area) > epsilon(total_area)) then
                write(error_unit,*) "TEST_cross_intersection -> fail"
                write(error_unit,*) "index", i
                write(error_unit,*) "computed", total_area
                write(error_unit,*) "real", real_area
                error stop
            end if
        end do
        write(output_unit,*) "TEST_cross_intersection -> success"
    end subroutine TEST_cross_intersection

    function TEST_compute_error(source_Mesh, target_Mesh) result(error)
        !> compute the L2 error between a solution defined on a source mesh
        !> to a target mesh where the source solution has been interpolated to
        !>
        !> source_Mesh: the source mesh and solution
        !> target_Mesh: the target mesh and the interpolated solution
        !> error      : the L2 interpolation error

        type(T_Mesh), intent(in)    :: source_Mesh
        type(T_Mesh), intent(inout) :: target_Mesh
        real(wp)                    :: error

        real(wp) :: total_area, overlap_area
        integer  :: i,j
        integer  :: ns,nt

        ns = size(source_Mesh%polygons, dim=2)
        nt = size(target_Mesh%polygons, dim=2)
        error = 0._wp
        total_area = 0._wp
        do i=1,nt
            do j=1,ns
                associate(si => target_Mesh%sizes(i), sj => source_Mesh%sizes(j))
                    overlap_area = overlapping_area(target_Mesh%vertices(:,:), target_Mesh%polygons(:si,i), &
                        & source_Mesh%vertices(:,:), source_Mesh%polygons(:sj,j))
                end associate
                error = error + (overlap_area * (target_Mesh%solution(i) - source_Mesh%solution(i)))**2
                total_area = total_area + overlap_area
            end do
        end do
        error = sqrt(error) / total_area
    end function TEST_compute_error

end program tests
