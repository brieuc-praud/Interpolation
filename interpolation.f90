!> === interpolation.f90
!>
!> provide the conservative interpolation subroutine
!>
!> ===

module interpolation
    use parameters,    only: wp
    use polygon_tools, only: overlapping_area
    use mesh,          only: T_mesh

    implicit none

    private
    public  :: conservative_interpolation

contains

    subroutine conservative_interpolation(source_Mesh, target_Mesh)
        !> perform a conservative interpolation from source_Mesh to target_Mesh
        !>
        !> source_Mesh: mesh to interpolate from
        !> target_Mesh: mesh to interpolate to

        type(T_Mesh), intent(in)    :: source_Mesh
        type(T_Mesh), intent(inout) :: target_Mesh

        real(wp) :: total_area, overlap_area
        integer  :: i,j
        integer  :: ns,nt

        ns = size(source_Mesh%polygons, dim=2)
        nt = size(target_Mesh%polygons, dim=2)
        allocate(target_Mesh%solution(nt))
        target_Mesh%solution(:) = 0._wp
        do i=1,nt
            total_area = 0._wp
            do j=1,ns
                overlap_area = overlapping_area(target_Mesh%vertices(:,:), target_Mesh%polygons(:,i), &
                    & source_Mesh%vertices(:,:), source_Mesh%polygons(:,j))
                target_Mesh%solution(i) = target_Mesh%solution(i) + overlap_area*source_Mesh%solution(j)
                total_area = total_area + overlap_area
            end do
            target_Mesh%solution(i) = target_Mesh%solution(i) / total_area
        end do
    end subroutine conservative_interpolation

end module interpolation
