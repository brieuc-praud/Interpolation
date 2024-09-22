!> === miscellaneous.f90
!>
!> provide the implementation of a stack structure
!>
!> ===

module miscellaneous
    use parameters, only: wp

    implicit none

    private
    public  :: T_Stack

    type :: T_Stack
        integer                :: val
        type(T_Stack), pointer :: next => null()
    contains
        procedure :: ssize        => stack_size
        procedure :: scontains    => stack_contains
        procedure :: push         => stack_push
        procedure :: push_unique  => stack_push_unique
        procedure :: pop          => stack_pop
        procedure :: get          => stack_get
        procedure :: free         => stack_free
    end type

contains
    recursive function stack_size(this) result(st_size)
        !> returns the size of the stack
        !>
        !> this: the stack to get the size of
        !> n   : the size counter
        class(T_Stack), target, intent(inout) :: this
        integer                               :: st_size

        if (associated(this%next)) then
            st_size = stack_size(this%next) + 1
        else
            st_size = 0
        end if
    end function stack_size

    subroutine stack_push(this, val)
        !> push a value onto the stack
        !>
        !> this: the stack to push onto
        !> val : the value to push
        class(T_Stack), target, intent(inout) :: this
        integer,                intent(in)    :: val

        type(T_Stack), pointer :: new_link => null()

        allocate(new_link)

        new_link%val  =  val
        new_link%next => this%next
        this%next     => new_link

    end subroutine stack_push

    function stack_contains(this, val)
        !> check if a value is in the stack
        !>
        !> this           : the stack to check in
        !> val            : the value to check against
        !> stack_contains : .true. if val in the stack, .false. otherwise
        class(T_Stack), target, intent(inout) :: this
        integer,                intent(in)    :: val
        logical                               :: stack_contains

        integer :: s,k

        stack_contains = .false.

        s = this%ssize()
        if (s > 0) then
            do k=1,s
                stack_contains = stack_contains .or. (this%get(k) == val)
                if (stack_contains) exit
            end do
        end if
    end function stack_contains

    subroutine stack_push_unique(this, val)
        !> push a value onto the stack only if it is not already in
        !>
        !> this: the stack to push onto
        !> val : the value to push
        class(T_Stack), target, intent(inout) :: this
        integer,                intent(in)    :: val

        if (.not.this%scontains(val)) call this%push(val)

    end subroutine stack_push_unique

    function stack_pop(this) result(val)
        !> pop a value out of the stack
        !>
        !> this: the stack to pop the value from
        !> val : the popped value
        class(T_Stack), target, intent(inout) :: this
        integer                               :: val

        class(T_Stack), pointer :: tmp

        tmp       => this%next
        val       =  tmp%val
        this%next => tmp%next
        deallocate(tmp)

    end function stack_pop

    recursive function stack_get(this, n) result(val)
        !> get the n-th value in the stack
        !>
        !> this: the stack to pop the value from
        !> n   : the index of the desired value
        !> val : the n-th value
        class(T_Stack), target, intent(inout) :: this
        integer               , intent(in)    :: n
        integer                               :: val

        if (n == 1) then
            val = this%next%val
        else
            val = this%next%get(n-1)
        end if

    end function stack_get

    subroutine stack_free(this)
        class(T_Stack), intent(inout) :: this

        integer :: dummy

        do while (associated(this%next))
            dummy = this%pop()
        end do
    end subroutine stack_free

end module miscellaneous
