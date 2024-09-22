!> === polygon_tools.f90
!>
!> provide some usual algorithms and formulas for polygons
!>
!> ===

module polygon_tools
    use parameters,    only: wp, MAX_VERTICES
    use predicatesf90, only: orient2df90

    implicit none

    private
    public  :: sutherland_hodgman, overlapping_area, polygon_area

    interface polygon_area
        module procedure polygon_area_poly
        module procedure polygon_area_rpoly
    end interface polygon_area

contains

    function rpolygon(vertices, polygon)
        !> return the polygon using the coordinates and not the indexes of its vertices
        !>
        !> vertices: the array containing the vertices
        !> polygon : the polygon, defined by its vertex indices
        !> rpolygon: the polygon, defined by the coordinates of its vertices

        real(wp), dimension(:,:),            intent(in) :: vertices
        integer,  dimension(:),              intent(in) :: polygon
        real(wp), dimension(2,size(polygon))            :: rpolygon

        integer                                         :: i

        do concurrent (i=1:size(polygon))
            rpolygon(:,i) = vertices(:,polygon(i))
        end do
    end function rpolygon

    function overlapping_area(vertices1, polygon1, vertices2, polygon2)
        !> return the area of the intersection of two polygons
        !>
        !> vertices1       : array of vertices used to define polygon1
        !> polygon1        : first polygon, defined by its indices in the vertices1 array
        !> vertices2       : array of vertices used to define polygon2
        !> polygon2        : second polygon, defined by its indices in the vertices2 array
        !> overlapping_area: the area of the intersection of the two polygons

        real(wp), dimension(:,:), intent(in) :: vertices1, vertices2
        integer,  dimension(:),   intent(in) :: polygon1, polygon2
        real(wp)                             :: overlapping_area

        real(wp), dimension(2,size(polygon1)) :: rpolygon1
        real(wp), dimension(2,size(polygon2)) :: rpolygon2
        real(wp), dimension(2,MAX_VERTICES)   :: poly_clipped
        integer                               :: nclipped

        ! extract the coordinates of the vertices
        rpolygon1(:,:) = rpolygon(vertices1(:,:), polygon1(:))
        rpolygon2(:,:) = rpolygon(vertices2(:,:), polygon2(:))

        ! compute the intersection of the two polygons
        call sutherland_hodgman(rpolygon1(:,:), rpolygon2(:,:), poly_clipped(:,:), nclipped)

        overlapping_area = polygon_area(poly_clipped(:,:nclipped))

    end function overlapping_area

    subroutine sutherland_hodgman(poly1, poly2, poly_clipped, nclipped)
        !> get the polygon defined as the intersection of poly1 and poly2 
        !> 
        !> see Sutherland-Hodgman, Wikipedia for more infos
        !>
        !> poly1       : the clipping polygon
        !> poly2       : the polygon to be clipped
        !> poly_clipped: the clipped polygon
        !> nclipped    : the 'true size' of the clipped polygon, as given by the function polygon_size(...)

        real(wp), parameter                              :: THRESHOLD = epsilon(1._wp) !> tolerance to avoid numerical issues
        real(wp), dimension(:,:),            intent(in)  :: poly1,poly2
        real(wp), dimension(2,MAX_VERTICES), intent(out) :: poly_clipped
        integer,                             intent(out) :: nclipped

        integer                                          :: ninputs,n1,n2
        real(wp),  dimension(2,MAX_VERTICES)             :: inputs
        integer                                          :: i,j
        real(wp), dimension(2)                           :: m1,m2,p1,p2, inter
        real(wp)                                         :: cp1,cp2

        n1 = size(poly1,dim=2)
        n2 = size(poly2,dim=2)
        poly_clipped(:,:n2) = poly2(:,:)
        nclipped = n2
        do i=1,n1
            m1 = poly1(:,i)
            m2 = poly1(:,mod(i,n1)+1)

            ninputs = nclipped
            inputs(:,:nclipped) = poly_clipped(:,:nclipped)

            nclipped = 0
            do j=1,ninputs
                p2 = inputs(:,j)
                p1 = inputs(:,modulo(j-2,ninputs)+1)
                inter = intersection(m1, m2, p1, p2)

                cp1 = orient2df90(m1,m2,p1)
                cp2 = orient2df90(m1,m2,p2)
                if (cp2 > THRESHOLD) then
                    if (cp1 <= THRESHOLD) then
                        call add_to_polygon(poly_clipped(:,:), nclipped, inter(:))
                    end if
                    call add_to_polygon(poly_clipped(:,:), nclipped, p2(:))
                else if (cp1 > THRESHOLD) then
                    call add_to_polygon(poly_clipped(:,:), nclipped, inter(:))
                end if
            end do
        end do

    contains
        subroutine add_to_polygon(polygon, n, vertex)
            !> append a vertex to a polygon while avoiding duplicates
            !>
            !> polygon: the polygon to append the vertex to
            !> n      : the 'true size' of the polygon, as given by the function polygon_size(...)
            !> vertex : the vertex to append to the polygon

            real(wp), dimension(2,MAX_VERTICES), intent(inout) :: polygon
            integer,                             intent(inout) :: n
            real(wp), dimension(2),              intent(in)    :: vertex
            real(wp), dimension(2)                             :: diff

            if (n == 0) then
                ! the first vertex cannot be a duplicate
                n = 1
                polygon(:,1) = vertex(:)
            else
                ! avoid adding a duplicate
                diff(:) = vertex(:) - polygon(:,n)
                if (hypot(diff(1), diff(2)) > epsilon(diff(1))) then
                    n = n + 1
                    polygon(:,n) = vertex(:)
                end if
            end if
        end subroutine add_to_polygon
    end subroutine sutherland_hodgman

    function intersection(A1, B1, A2, B2)
        !> compute the intersection point between two lines each defined by 2 points
        !>
        !> warning: the intersection is not actually computed if the two lines are parallel
        !>
        !> A1, B1      : two points defining the first line
        !> A2, B2      : two points defining the second line
        !> intersection: the intersection of the two lines

        real(wp), parameter :: THRESHOLD = epsilon(1._wp)
        real(wp), dimension(2), intent(in) :: A1, B1, A2, B2
        real(wp), dimension(2)             :: intersection

        real(wp)                           :: dx1, dy1, dx2, dy2, l1, l2, det

        dx1 = B1(1)-A1(1)
        dy1 = B1(2)-A1(2)
        dx2 = B2(1)-A2(1)
        dy2 = B2(2)-A2(2)

        det = dy2*dx1 - dy1*dx2

        if (abs(det) >= THRESHOLD) then ! if lines are not parallel
            l1 = dx1*A1(2) - dy1*A1(1)
            l2 = dx2*A2(2) - dy2*A2(1)
            intersection(1) = dx2*l1 - dx1*l2
            intersection(2) = dy2*l1 - dy1*l2
            intersection = intersection/det
        else if (all(A1 == A2) .or. all(A1 == B2)) then
            intersection = A1
        else if (all(B1 == A2) .or. all(B1 == B2)) then
            intersection = B1
        end if
    end function intersection

    function polygon_area_rpoly(polygon) result(polygon_area)
        !> compute the area of a polygon (defined by the coordinates of its vertices) using the trapezoid formula
        !>
        !> polygon     : the polygon to compute the area from
        !> polygon_area: the area of the polygon, computed by the trapezoid formula

        real(wp), dimension(:,:), intent(in) :: polygon
        real(wp)                             :: polygon_area

        integer                              :: n
        integer                              :: i,j

        n = size(polygon,dim=2)
        polygon_area = 0._wp
        do i=1,n
            j = mod(i,n)+1 ! next vertex index, looping
            polygon_area = polygon_area + (polygon(1,j)+polygon(1,i))*(polygon(2,j)-polygon(2,i))
        end do
        polygon_area = .5_wp*polygon_area
    end function polygon_area_rpoly

    function polygon_area_poly(vertices, polygon) result(polygon_area)
        !> compute the area of a polygon (defined by the indices of its vertices) using the trapezoid formula
        !>
        !> polygon     : the polygon to compute the area from
        !> vertices    : the array containing the vertices
        !> polygon_area: the area of the polygon, computed by the trapezoid formula

        real(wp), dimension(:,:),            intent(in) :: vertices
        integer,  dimension(:),              intent(in) :: polygon
        real(wp), dimension(2,size(polygon))            :: rpoly
        real(wp)                                        :: polygon_area

        ! get a polygon defined by its vertices
        rpoly(:,:) = rpolygon(vertices(:,:), polygon(:))

        polygon_area = polygon_area_rpoly(rpoly(:,:))
    end function polygon_area_poly

end module polygon_tools
