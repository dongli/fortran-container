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
    procedure :: last_value => array_last_value
    procedure :: index_ptr => array_index_ptr
    procedure :: replace_ptr => array_replace_ptr
    procedure :: replace_ptr_at => array_replace_ptr_at
    procedure, private :: array_assign
    generic :: assignment(=) => array_assign
    procedure :: clear => array_clear
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
    if (this%items(index)%internal_memory) deallocate(this%items(index)%value)
    allocate(this%items(index)%value, source=value)
    this%items(index)%internal_memory = .true.

  end subroutine array_insert_at

  subroutine array_insert_ptr_at(this, index, value)

    class(array_type), intent(inout) :: this
    integer, intent(in) :: index
    class(*), intent(in), target :: value

    if (index > this%size) stop __FILE__ // ': Index exceeds array size!'
    if (this%items(index)%internal_memory) deallocate(this%items(index)%value)
    this%items(index)%value => value
    this%items(index)%internal_memory = .false.

  end subroutine array_insert_ptr_at

  function array_value_at(this, index) result(res)

    class(array_type), intent(in) :: this
    integer, intent(in) :: index
    class(*), pointer :: res

    if (index > this%size) stop __FILE__ // ': Index exceeds array size!'
    res => this%items(index)%value

  end function array_value_at

  function array_last_value(this) result(res)

    class(array_type), intent(in) :: this
    class(*), pointer :: res

    if (this%size == 0) stop __FILE__ // ': Array is empty!'
    res => this%items(this%size)%value

  end function array_last_value

  integer function array_index_ptr(this, value) result(res)

    class(array_type), intent(in) :: this
    class(*), intent(in), target :: value

    integer i

    do i = 1, this%size
      if (associated(this%items(i)%value, value)) then
        res = i
        return
      end if
    end do
    res = -1

  end function array_index_ptr

  subroutine array_replace_ptr(this, old_value, new_value)

    class(array_type), intent(inout) :: this
    class(*), intent(in), target :: old_value
    class(*), intent(in), target :: new_value

    integer i

    do i = 1, this%size
      if (associated(this%items(i)%value, old_value)) then
        if (this%items(i)%internal_memory) deallocate(this%items(i)%value)
        this%items(i)%value => new_value
        this%items(i)%internal_memory = .false.
        return
      end if
    end do
    write(*, *) trim(__FILE__) // ': array_replace_ptr: array failed to replace pointer!'
    stop 1

  end subroutine array_replace_ptr

  subroutine array_replace_ptr_at(this, index, new_value)

    class(array_type), intent(inout) :: this
    integer, intent(in) :: index
    class(*), intent(in), target :: new_value

    if (this%size >= index) then
      if (this%items(index)%internal_memory) deallocate(this%items(index)%value)
      this%items(index)%value => new_value
      this%items(index)%internal_memory = .false.
    else
      write(*, *) trim(__FILE__) // ': array_replace_ptr_at: array failed to replace pointer!'
      stop 1
    end if

  end subroutine array_replace_ptr_at

  subroutine array_assign(this, that)

    class(array_type), intent(inout) :: this
    class(array_type), intent(in) :: that

    integer i

    call this%clear()
    allocate(this%items(that%capacity))
    this%capacity = that%capacity
    do i = 1, that%size
      if (that%items(i)%internal_memory) then
        call this%append(that%items(i)%value)
      else
        call this%append_ptr(that%items(i)%value)
      end if
    end do

  end subroutine array_assign

  subroutine array_clear(this)

    class(array_type), intent(inout) :: this

    if (allocated(this%items)) deallocate(this%items)
    this%capacity = 0
    this%size = 0

  end subroutine array_clear

  ! ----------------------------------------------------------------------------
  !                               Finalizers
  ! ----------------------------------------------------------------------------

  recursive subroutine array_item_finalize(this)

    type(array_item_type), intent(inout) :: this

    if (this%internal_memory .and. associated(this%value)) deallocate(this%value)

  end subroutine array_item_finalize

  subroutine array_finalize(this)

    type(array_type), intent(inout) :: this

    call this%clear()

  end subroutine array_finalize

end module array_mod
