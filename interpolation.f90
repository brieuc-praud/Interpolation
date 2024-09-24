!> === interpolation.f90
!>
!> provide the conservative interpolation subroutine
!>
!> ===

module interpolation
    use parameters,    only: wp
    use polygon_tools, only: overlapping_area, polygon_area
    use miscellaneous, only: T_Stack
    use mesh,          only: T_Mesh

    implicit none

    private
    public  :: conservative_interpolation, interpolation_error

contains

    subroutine conservative_interpolation(source_Mesh, target_Mesh)
        !> perform a conservative interpolation from source_Mesh to target_Mesh
        !>
        !> it performs this operation in O(n) thanks to an advancing front algorithm
        !> (see Farrell, P. E., & Maddison, J. R. "Conservative interpolation between volume meshes
        !> by local Galerkin projection" (2011) for details).
        !>
        !> source_Mesh: mesh to interpolate from
        !> target_Mesh: mesh to interpolate to

        real(wp), parameter         :: THRESHOLD = epsilon(1._wp) !> tolerance to avoid numerical issues
        type(T_Mesh), intent(in)    :: source_Mesh
        type(T_Mesh), intent(inout) :: target_Mesh

        real(wp)                                 :: total_area, overlap_area
        integer                                  :: i,j,k
        integer                                  :: ns,nt
        type(T_Stack)                            :: stack_F, stack_FF
        type(T_Stack), dimension(:), allocatable :: stack_I
        integer,       dimension(:), allocatable :: processed

        ns = size(source_Mesh%polygons, dim=2)
        nt = size(target_Mesh%polygons, dim=2)
        allocate(processed(nt), stack_I(nt))
        if (.not.allocated(target_Mesh%solution)) allocate(target_Mesh%solution(nt))
        target_Mesh%solution(:) = 0._wp

        ! mark all elements as unprocessed
        ! -1: unprocessed and we do not know how to for now
        !  0: processed
        ! >0: unprocessed but we know how to process it
        processed(:) = -1

        i=1
        ! loop over the target mesh
        do while (i > 0)
            if (i == 1) then
                ! brute force the first intersection (i.e. the seed)
                j=0
                overlap_area = 0._wp
                do while (overlap_area <= THRESHOLD)
                    j = j+1
                    associate(si => target_Mesh%sizes(i), sj => source_Mesh%sizes(j))
                        overlap_area = overlapping_area(target_Mesh%vertices(:,:), target_Mesh%polygons(:si,i), &
                            & source_Mesh%vertices(:,:), source_Mesh%polygons(:sj,j))
                    end associate
                end do
            else
                ! use the stacks to efficiently find a new seed
                k=0
                overlap_area = 0._wp
                do while (overlap_area <= THRESHOLD)
                    k = k+1
                    j = stack_I(processed(i))%get(k)
                    associate(si => target_Mesh%sizes(i), sj => source_Mesh%sizes(j))
                        overlap_area = overlapping_area(target_Mesh%vertices(:,:), target_Mesh%polygons(:si,i), &
                            & source_Mesh%vertices(:,:), source_Mesh%polygons(:sj,j))
                    end associate
                end do
            end if
            call stack_F%push(j)

            total_area = 0._wp ! the area of element i
            do while (associated(stack_F%next))
                j = stack_F%pop()
                associate(si => target_Mesh%sizes(i), sj => source_Mesh%sizes(j))
                    overlap_area = overlapping_area(target_Mesh%vertices(:,:), target_Mesh%polygons(:si,i), &
                        & source_Mesh%vertices(:,:), source_Mesh%polygons(:sj,j))
                    if (overlap_area > THRESHOLD) then
                        target_Mesh%solution(i) = target_Mesh%solution(i) + overlap_area*source_Mesh%solution(j)
                        total_area = total_area + overlap_area

                        ! The boolean condition passed as an argument is there to handle degenerated cases where
                        ! some elements perfectly match between the two meshes
                        call update_stacks(stack_I(i), stack_F, stack_FF, j, source_Mesh%neighbors(:,:), &
                           & abs(overlap_area - polygon_area(source_Mesh%vertices(:,:), &
                           & source_Mesh%polygons(:sj,j))) <= THRESHOLD)
                    end if
                end associate
            end do

            target_Mesh%solution(i) = target_Mesh%solution(i) / total_area

            call stack_FF%free()

            ! mark element i as processed
            processed(i) = 0

            associate(nb => target_Mesh%neighbors)
                ! mark element i as a reference element for its neighbors if they need one
                do k=1,nb(1,i)
                    if (processed(nb(k+1,i)) == -1) processed(nb(k+1,i)) = i
                end do
            end associate

            ! find a new element to process
            i = findloc(processed(:) > 0, .true., dim=1)
        end do

        do i=1,nt
            call stack_I(i)%free()
        end do
        deallocate(stack_I)
        deallocate(processed)
    contains
        subroutine update_stacks(stack_I, stack_F, stack_FF, element, neighbors, add_neighbors)
            !> add the relevant elements to each stack
            !>
            !> stack_I      : stack which keeps track of the intersecting elements
            !> stack_F      : stack which keeps track of the advancing front
            !> stack_FF     : stack which keeps track of all the visited elements during the round
            !> element      : the id of the element to add
            !> neighbors    : the table containing the neighbors
            !> add_neighbors: add the neighbors of element to stack_I or not

            type(T_Stack),           intent(inout) :: stack_I
            type(T_Stack),           intent(inout) :: stack_F
            type(T_Stack),           intent(inout) :: stack_FF
            integer,                 intent(in)    :: element
            integer, dimension(:,:), intent(in)    :: neighbors
            logical,                 intent(in)    :: add_neighbors

            integer :: k

            call stack_I%push(element)
            do k=1,neighbors(1,element)
                associate(nb => neighbors(k+1,element))
                    ! avoid adding the same element multiple times
                    if (.not.stack_FF%scontains(nb)) then
                        call stack_F%push(nb)
                        call stack_FF%push(nb)
                        if (add_neighbors) call stack_I%push(nb)
                    end if
                end associate
            end do
        end subroutine update_stacks
        
    end subroutine conservative_interpolation

    function interpolation_error(source_Mesh, target_Mesh) result(error)
        !> compute the L2 error between a solution defined on a source mesh
        !> to a target mesh where the source solution has been interpolated to
        !>
        !> this is a naive quadratic time complexity error computation
        !> so that advancing front issues that could arise in conservative_interpolation()
        !> would not be present here
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
    end function interpolation_error

end module interpolation
