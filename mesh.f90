!> === mesh.f90
!>
!> define the T_Mesh type and provides a cleaning function for it
!>
!> also provide functions to read and write meshes and solutions to a file
!>
!> ===

module mesh
    use parameters, only: wp, MAX_VERTICES

    implicit none

    private
    public  :: T_Mesh, read_mesh, write_mesh, clean

    type :: T_Mesh
        real(wp), dimension(:,:), allocatable :: vertices  !> coordinates of the vertices used to define mesh cells
        integer,  dimension(:,:), allocatable :: polygons  !> defined as indices in the vertices array
        integer,  dimension(:),   allocatable :: sizes     !> true size of the polygons
        real(wp), dimension(:),   allocatable :: solution  !> a scalar solution defined on each cell of the mesh
        integer,  dimension(:,:), allocatable :: neighbors !> indexes of the neighboring elements
    end type

contains
    subroutine clean(Mesh)
        !> clean the allocated arrays from Mesh
        !>
        !> Mesh: the T_Mesh to clean

        type(T_Mesh), intent(out) :: Mesh

        if (allocated(Mesh%vertices))  deallocate(Mesh%vertices)
        if (allocated(Mesh%polygons))  deallocate(Mesh%polygons)
        if (allocated(Mesh%sizes))     deallocate(Mesh%sizes)
        if (allocated(Mesh%solution))  deallocate(Mesh%solution)
        if (allocated(Mesh%neighbors)) deallocate(Mesh%neighbors)
    end subroutine clean

    subroutine read_mesh(Mesh, mesh_file)
        !> read a mesh and solution from file, then build the connectivity
        !>
        !> Mesh      : T_Mesh object to read to
        !> mesh_file : name of the file to read from

        type(T_Mesh),             intent(out) :: Mesh
        character(len=*),         intent(in)  :: mesh_file

        integer                               :: runit, ios, i
        character(len=64)                     :: keyword
        integer                               :: nvertices, npolygons

        open(newunit=runit, file=trim(adjustl(mesh_file)), action="read")
        read(runit,'(A)',iostat=ios) keyword
        do while (trim(adjustl(keyword)) /= "Vertices")
            read(runit,'(A)',iostat=ios) keyword
        end do
        read(runit,*,iostat=ios) nvertices
        allocate(Mesh%vertices(2,nvertices))
        do i=1,nvertices
            read(runit,*,iostat=ios) Mesh%vertices(:,i)
        end do
        do while (trim(adjustl(keyword)) /= "Polygons")
            read(runit,'(A)',iostat=ios) keyword
        end do
        read(runit,*,iostat=ios) npolygons
        allocate(Mesh%polygons(MAX_VERTICES,npolygons))
        do i=1,npolygons
            read(runit,*,iostat=ios) Mesh%polygons(:,i)
        end do

        ! the 'Solution' keyword is not necessarily present in the file
        do while (trim(adjustl(keyword)) /= "Solution" .and. ios == 0)
            read(runit,'(A)',iostat=ios) keyword
        end do
        if (ios == 0) then
            read(runit,*,iostat=ios) npolygons
            allocate(Mesh%solution(npolygons))
            do i=1,npolygons
                read(runit,*,iostat=ios) Mesh%solution(i)
            end do
        end if

        close(runit)

        allocate(Mesh%sizes(npolygons))
        do i=1,npolygons
            Mesh%sizes(i) = polygon_size(Mesh%polygons(:,i))
        end do

        call build_connectivity(Mesh)
    end subroutine read_mesh

    subroutine write_mesh(Mesh, mesh_file)
        !> write a mesh and solution to file
        !>
        !> Mesh      : T_Mesh object to write to the file
        !> mesh_file : name of the file to write to

        type(T_Mesh),             intent(inout) :: Mesh
        character(len=*),         intent(in)    :: mesh_file

        integer                                 :: wunit, i
        integer                                 :: nvertices, npolygons

        open(newunit=wunit, file=trim(adjustl(mesh_file)), action="write")
        if (allocated(Mesh%vertices)) then
            nvertices = size(Mesh%vertices, dim=2)
            write(wunit,'(A)') "Vertices"
            write(wunit,'(I8)') nvertices
            do i=1,nvertices
                write(wunit,'(3(E23.16,1X))') Mesh%vertices(:,i), 0._wp
            end do
            write(wunit,*) ""
        end if
        if (allocated(Mesh%polygons)) then
            npolygons = size(Mesh%polygons, dim=2)
            write(wunit,'(A)') "Polygons"
            write(wunit,'(I8)') npolygons
            do i=1,npolygons
                write(wunit,'(*(I8,1X))') Mesh%polygons(:,i)
            end do
            write(wunit,*) ""
        end if
        if (allocated(Mesh%solution)) then
            write(wunit,'(A)') "Solution"
            write(wunit,'(I8)') npolygons
            do i=1,npolygons
                write(wunit,'(E23.16)') Mesh%solution(i)
            end do
            write(wunit,*) ""
        end if
        close(wunit)
    end subroutine write_mesh

    subroutine build_connectivity(Mesh)
        !> fill the Mesh%neighbors array with the indexes of the adjacent cells
        !>
        !> Mesh      : T_Mesh object to build the connectivity for

        type(T_Mesh), intent(inout) :: Mesh

        integer :: n,ni,nj
        integer :: i,j,ki,kj,kip,kjp

        n = size(Mesh%polygons, dim=2)

        ! the number of neighbors is bounded by the number of edges, which is equal to the number of vertices
        ! the first index is the number of neighbors
        allocate(Mesh%neighbors(MAX_VERTICES+1, n))
        Mesh%neighbors(:,:) = 0

        do i=1,n
            ni = Mesh%sizes(i)
            do j=1,n
                nj = Mesh%sizes(j)
                do ki=1,ni
                    kip = mod(ki,ni)+1 ! next vertex index, looping
                    do kj=1,nj
                        kjp = mod(kj,nj)+1 ! next vertex index, looping
                        if ( (Mesh%polygons(ki,i) == Mesh%polygons(kjp,j)) &
                            & .and. (Mesh%polygons(kip,i) == Mesh%polygons(kj,j)) ) then
                            associate (nn => Mesh%neighbors(1,i))
                                if (nn >= 1) then
                                    if (all(Mesh%neighbors(2:nn,i) /= j)) then
                                        nn = nn+1
                                        Mesh%neighbors(nn+1,i) = j
                                    end if
                                else
                                    nn = 1
                                    Mesh%neighbors(nn+1,i) = j
                                end if
                            end associate
                        end if
                    end do
                end do
            end do
        end do

    end subroutine build_connectivity

    function polygon_size(polygon)
        !> return the true 'size' of the polygon (i.e. the number of vertices not set to 0)
        !>
        !> polygon     : the polygon of size MAX_VERTICES (i.e. padded with 0 if needed)
        !> polygon_size: the number of non-zero vertex indices

        integer, dimension(:), intent(in) :: polygon
        integer                           :: polygon_size

        polygon_size = findloc(polygon, 0, dim=1) - 1
        if (polygon_size == -1) polygon_size = MAX_VERTICES
    end function polygon_size


end module mesh
