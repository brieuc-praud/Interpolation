!> === main.f90
!>
!> perform a conservative interpolation
!> from the mesh passed as the first argument
!> to the mesh passed as the second argument
!>
!> ===

program main
    use parameters,    only: wp
    use mesh,          only: T_mesh, read_mesh, write_mesh, clean
    use interpolation, only: conservative_interpolation, interpolation_error
#ifdef ROBUST_PREDICATES
    use predicatesf90, only: exactinit
#endif
    use iso_fortran_env, only: output_unit, error_unit

    implicit none

    type(T_Mesh)       :: source_Mesh, source_Mesh_bis, target_Mesh
    character(len=256) :: source_mesh_file, target_mesh_file
    real(wp)           :: t1,t2
    real(wp)           :: error

    if (command_argument_count() /= 2) error stop 'invalid number of arguments (2 arguments expected)'

    call get_command_argument(1, source_mesh_file)
    call get_command_argument(2, target_mesh_file)

#ifdef ROBUST_PREDICATES
    call exactinit()
#endif

    call read_mesh(source_Mesh, source_mesh_file)
    call read_mesh(source_Mesh_bis, source_mesh_file)
    call read_mesh(target_Mesh, target_mesh_file)

    call cpu_time(t1)
    call conservative_interpolation(source_Mesh, source_Mesh_bis)
    call cpu_time(t2)
    error = interpolation_error(source_Mesh, source_Mesh_bis)
    write(output_unit,*) "self-interpolation error:",            error
    write(output_unit,*) "self-interpolation computation time:", t2-t1

    if (error > epsilon(error)) error stop "significant self-interpolation error"

    call cpu_time(t1)
    call conservative_interpolation(source_Mesh, target_Mesh)
    call cpu_time(t2)
    error = interpolation_error(source_Mesh, target_Mesh)
    write(output_unit,*) "cross-interpolation error:",            error
    write(output_unit,*) "cross-interpolation computation time:", t2-t1

    ! overwrite the target mesh with target mesh and solution
    call write_mesh(target_Mesh, target_mesh_file)

    ! clean Mesh structures
    call clean(source_Mesh)
    call clean(target_Mesh)

contains

end program main
