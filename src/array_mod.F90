module array_mod

  implicit none

  type array_item_type
    logical :: internal_memory = .true.
    class(*), pointer :: value => null()
  contains
    final :: array_item_finalize
  end type array_item_type

  type array_type
    integer :: capacity = 0
    integer :: size = 0
    type(array_item_type), allocatable :: items(:)
  contains
    procedure :: expand_by => array_expand_by
    procedure :: expand_to => array_expand_to
    procedure :: append => array_append
    procedure :: append_ptr => array_append_ptr
    procedure :: insert_at => array_insert_at
    procedure :: insert_ptr_at => array_insert_ptr_at
    procedure :: value_at => array_value_at
    final :: array_finalize
  end type array_type

  integer, parameter :: initial_size = 1000
  integer, parameter :: increase_size = 100

contains

  function array(capacity)

    integer, intent(in) :: capacity
    type(array_type) array

    allocate(array%items(capacity))
    array%capacity = capacity

  end function array

  subroutine array_expand_by(this, increase_size)

    class(array_type), intent(inout) :: this
    integer, intent(in) :: increase_size

    type(array_item_type), allocatable :: tmp_items(:)
    integer i

    allocate(tmp_items(this%size))
    do i = 1, this%size
      tmp_items(i) = this%items(i)
    end do
    if (allocated(this%items)) deallocate(this%items)
    allocate(this%items(this%capacity + increase_size))
    do i = 1, this%size
      this%items(i) = tmp_items(i)
    end do
    deallocate(tmp_items)
    this%capacity = this%capacity + increase_size

  end subroutine array_expand_by

  subroutine array_expand_to(this, capacity)

    class(array_type), intent(inout) :: this
    integer, intent(in) :: capacity

    call this%expand_by(capacity - this%capacity)

  end subroutine array_expand_to

  subroutine array_append(this, value)

    class(array_type), intent(inout) :: this
    class(*), intent(in) :: value

    if (this%size == this%capacity) call this%expand_by(increase_size)
    this%size = this%size + 1
    allocate(this%items(this%size)%value, source=value)
    this%items(this%size)%internal_memory = .true.

  end subroutine array_append

  subroutine array_append_ptr(this, value)

    class(array_type), intent(inout) :: this
    class(*), intent(in), target :: value

    if (this%size == this%capacity) call this%expand_by(increase_size)
    this%size = this%size + 1
    this%items(this%size)%value => value
    this%items(this%size)%internal_memory = .false.

  end subroutine array_append_ptr

  subroutine array_insert_at(this, index, value)

    class(array_type), intent(inout) :: this
    integer, intent(in) :: index
    class(*), intent(in) :: value

    if (index > this%size) stop __FILE__ // ': Index exceeds array size!'
    deallocate(this%items(index)%value)
    allocate(this%items(index)%value, source=value)
    this%items(index)%internal_memory = .true.

  end subroutine array_insert_at

  subroutine array_insert_ptr_at(this, index, value)

    class(array_type), intent(inout) :: this
    integer, intent(in) :: index
    class(*), intent(in), target :: value

    if (index > this%size) stop __FILE__ // ': Index exceeds array size!'
    this%items(index)%value => value
    this%items(index)%internal_memory = .false.

  end subroutine array_insert_ptr_at

  function array_value_at(this, index) result(res)

    class(array_type), intent(inout) :: this
    integer, intent(in) :: index
    class(*), pointer :: res

    if (index > this%size) stop __FILE__ // ': Index exceeds array size!'
    res => this%items(index)%value

  end function array_value_at

  subroutine array_item_finalize(this)

    type(array_item_type), intent(inout) :: this

    if (this%internal_memory .and. associated(this%value)) deallocate(this%value)

  end subroutine array_item_finalize

  subroutine array_finalize(this)

    type(array_type), intent(inout) :: this

    if (allocated(this%items)) deallocate(this%items)

  end subroutine array_finalize

end module array_mod
