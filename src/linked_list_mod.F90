! ------------------------------------------------------------------------------
! linked_list_mod
!
! Description:
!
!   This is an implementation of double linked list data structure in Fortran.
!   It uses polymorphic pointer heavily, so a modern Fortran compiler is needed.
!
! Author:
!
!   - Li Dong <dongli@lasg.iap.ac.cn>
! ------------------------------------------------------------------------------

module linked_list_mod

  implicit none

  type linked_list_item_type
    type(linked_list_item_type), pointer :: prev => null()
    type(linked_list_item_type), pointer :: next => null()
    character(:), allocatable :: key
    logical :: internal_memory = .true.
    class(*), pointer :: value => null()
  contains
    final :: linked_list_item_finalize
  end type linked_list_item_type

  type linked_list_iterator_type
    type(linked_list_type), pointer :: list => null()
    type(linked_list_item_type), pointer :: item
    type(linked_list_item_type), pointer :: next_item
    class(*), pointer :: value
  contains
    procedure :: ended => linked_list_iterator_ended
    procedure :: next => linked_list_iterator_next
  end type linked_list_iterator_type

  type linked_list_type
    integer :: size = 0
    type(linked_list_item_type), pointer :: first_item => null()
    type(linked_list_item_type), pointer :: last_item => null()
  contains
    ! Internal item methods
    procedure :: append_item => linked_list_append_item
    procedure :: insert_item_after => linked_list_insert_item_after
    procedure :: remove_item => linked_list_remove_item
    procedure :: item => linked_list_item
    procedure :: item_at => linked_list_item_at
    ! Append methods
    ! TODO: Replace insert by append if not given index.
    procedure, private :: append1 => linked_list_append1
    procedure, private :: append2 => linked_list_append2
    generic :: append => append1, append2
    procedure, private :: append_ptr1 => linked_list_append_ptr1
    procedure, private :: append_ptr2 => linked_list_append_ptr2
    generic :: append_ptr => append_ptr1, append_ptr2
    ! Close methods
    procedure :: close_ptr => linked_list_close_ptr
    procedure :: closed => linked_list_closed
    ! Insert methods
    procedure, private :: insert1 => linked_list_insert1
    procedure, private :: insert2 => linked_list_insert2
    generic :: insert => insert1, insert2
    procedure, private :: insert_ptr1 => linked_list_insert_ptr1
    procedure, private :: insert_ptr2 => linked_list_insert_ptr2
    generic :: insert_ptr => insert_ptr1, insert_ptr2
    procedure :: insert_ptr_at => linked_list_insert_ptr_at
    procedure :: insert_ptr_after => linked_list_insert_ptr_after
    ! Value methods
    procedure :: value => linked_list_value
    procedure :: value_at => linked_list_value_at
    procedure :: first_value => linked_list_first_value
    procedure :: last_value => linked_list_last_value
    ! Delete methods
    procedure :: delete_ptr => linked_list_delete_ptr
    procedure :: delete_at => linked_list_delete_at
    ! Replace methods
    procedure :: replace_ptr => linked_list_replace_ptr
    ! Clear method
    procedure :: clear => linked_list_clear
    ! Finalizer
    final :: linked_list_finalize
  end type linked_list_type

contains

  function linked_list_iterator(list)

    type(linked_list_type), intent(in), target :: list
    type(linked_list_iterator_type) linked_list_iterator

    linked_list_iterator%list => list
    linked_list_iterator%item => list%first_item
    if (associated(list%first_item)) then
      linked_list_iterator%next_item => list%first_item%next
      linked_list_iterator%value => list%first_item%value
    end if

  end function linked_list_iterator

  function linked_list_iterator_ended(this)

    class(linked_list_iterator_type), intent(in) :: this
    logical linked_list_iterator_ended

    linked_list_iterator_ended = .not. associated(this%item)

  end function linked_list_iterator_ended

  subroutine linked_list_iterator_next(this)

    class(linked_list_iterator_type), intent(inout) :: this

    this%item => this%next_item
    if (associated(this%item)) then
      this%value => this%item%value
      this%next_item => this%item%next
    else
      this%value => null()
      this%next_item => null()
    end if

  end subroutine linked_list_iterator_next

  function linked_list_first_value(this) result(res)

    class(linked_list_type), intent(in) :: this
    class(*), pointer :: res

    if (associated(this%first_item)) then
      res => this%first_item%value
    else
      res => null()
    end if

  end function linked_list_first_value

  function linked_list_last_value(this) result(res)

    class(linked_list_type), intent(in) :: this
    class(*), pointer :: res

    if (associated(this%first_item)) then
      res => this%last_item%value
    else
      res => null()
    end if

  end function linked_list_last_value

  function linked_list_item(this, key) result(res)

    class(linked_list_type), intent(in) :: this
    character(*), intent(in) :: key
    type(linked_list_item_type), pointer :: res

    type(linked_list_item_type), pointer :: item

    item => this%first_item
    do while (associated(item))
      if (item%key == key) then
        res => item
        return
      end if
      item => item%next
    end do
    res => null()

  end function linked_list_item

  function linked_list_item_at(this, index) result(res)

    class(linked_list_type), intent(in) :: this
    integer, intent(in) :: index
    type(linked_list_item_type), pointer :: res

    type(linked_list_item_type), pointer :: item
    integer i

    item => this%first_item
    i = 1
    do while (associated(item))
      if (i == index) then
        res => item
        return
      end if
      item => item%next
      i = i + 1
    end do
    res => null()

  end function linked_list_item_at

  subroutine linked_list_append1(this, key, value)

    class(linked_list_type), intent(inout) :: this
    character(*), intent(in) :: key
    class(*), intent(in) :: value

    type(linked_list_item_type), pointer :: item

    allocate(item)
    item%key = key
    call this%append_item(item)
    allocate(item%value, source=value)
    item%internal_memory = .true.

  end subroutine linked_list_append1

  subroutine linked_list_append2(this, value)

    class(linked_list_type), intent(inout) :: this
    class(*), intent(in) :: value

    type(linked_list_item_type), pointer :: item

    allocate(item)
    call this%append_item(item)
    allocate(item%value, source=value)
    item%internal_memory = .true.

  end subroutine linked_list_append2

  subroutine linked_list_append_ptr1(this, key, value)

    class(linked_list_type), intent(inout) :: this
    character(*), intent(in) :: key
    class(*), intent(in), target :: value

    type(linked_list_item_type), pointer :: item

    allocate(item)
    item%key = key
    call this%append_item(item)
    item%value => value
    item%internal_memory = .false.

  end subroutine linked_list_append_ptr1

  subroutine linked_list_append_ptr2(this, value)

    class(linked_list_type), intent(inout) :: this
    class(*), intent(in), target :: value

    type(linked_list_item_type), pointer :: item

    allocate(item)
    call this%append_item(item)
    item%value => value
    item%internal_memory = .false.

  end subroutine linked_list_append_ptr2

  subroutine linked_list_close_ptr(this, value)

    class(linked_list_type), intent(inout) :: this
    class(*), intent(in), optional :: value

    if (present(value)) call this%append_ptr(value)

    ! Connect first and last item.
    this%first_item%prev => this%last_item
    this%last_item%next => this%first_item

  end subroutine linked_list_close_ptr

  logical function linked_list_closed(this) result(res)

    class(linked_list_type), intent(in) :: this

    res = associated(this%first_item%prev, this%last_item) .and. associated(this%last_item%next, this%first_item)

  end function linked_list_closed

  subroutine linked_list_insert1(this, key, value, nodup)

    class(linked_list_type), intent(inout) :: this
    character(*), intent(in) :: key
    class(*), intent(in) :: value
    logical, intent(in), optional :: nodup

    type(linked_list_item_type), pointer :: item

    if (present(nodup) .and. nodup) then
      item => this%item(key)
      if (associated(item)) then
        if (item%internal_memory) deallocate(item%value)
        allocate(item%value, source=value)
        item%internal_memory = .true.
        return
      end if
    end if
    allocate(item)
    item%key = key
    call this%append_item(item)
    allocate(item%value, source=value)
    item%internal_memory = .true.

  end subroutine linked_list_insert1

  subroutine linked_list_insert2(this, value)

    class(linked_list_type), intent(inout) :: this
    class(*), intent(in) :: value

    type(linked_list_item_type), pointer :: item

    allocate(item)
    call this%append_item(item)
    allocate(item%value, source=value)
    item%internal_memory = .true.

  end subroutine linked_list_insert2

  subroutine linked_list_insert_ptr1(this, key, value, nodup)

    class(linked_list_type), intent(inout) :: this
    character(*), intent(in) :: key
    class(*), intent(in), target :: value
    logical, intent(in), optional :: nodup

    type(linked_list_item_type), pointer :: item

    if (present(nodup) .and. nodup) then
      item => this%item(key)
      if (associated(item)) then
        if (item%internal_memory) deallocate(item%value)
        item%value => value
        item%internal_memory = .false.
      end if
    else
      allocate(item)
      item%key = key
      call this%append_item(item)
      item%value => value
      item%internal_memory = .false.
    end if

  end subroutine linked_list_insert_ptr1

  subroutine linked_list_insert_ptr2(this, value)

    class(linked_list_type), intent(inout) :: this
    class(*), intent(in), target :: value

    type(linked_list_item_type), pointer :: item

    allocate(item)
    call this%append_item(item)
    item%value => value
    item%internal_memory = .false.

  end subroutine linked_list_insert_ptr2

  subroutine linked_list_insert_ptr_at(this, index, value)

    class(linked_list_type), intent(inout) :: this
    integer, intent(in) :: index
    class(*), intent(in), target :: value

    integer i
    type(linked_list_item_type), pointer :: item

    item => this%first_item
    do i = 1, this%size
      if (i == index) then
        if (item%internal_memory) deallocate(item%value)
        item%value => value
        item%internal_memory = .false.
        return
      else if (i > index) then
        return
      end if
      item => item%next
    end do

  end subroutine linked_list_insert_ptr_at

  subroutine linked_list_insert_ptr_after(this, value1, value2)

    class(linked_list_type), intent(inout) :: this
    class(*), intent(in), target :: value1
    class(*), intent(in), target :: value2

    integer i
    type(linked_list_item_type), pointer :: item1, item2

    item1 => this%first_item
    do i = 1, this%size
      if (associated(item1%value, value1)) then
        allocate(item2)
        call this%insert_item_after(item1, item2)
        item2%value => value2
        item2%internal_memory = .false.
        return
      end if
      item1 => item1%next
    end do

  end subroutine linked_list_insert_ptr_after

  function linked_list_value(this, key) result(res)

    class(linked_list_type), intent(in) :: this
    character(*), intent(in) :: key
    class(*), pointer :: res

    type(linked_list_item_type), pointer :: item

    item => this%item(key)
    if (associated(item)) then
      res => item%value
    else
      res => null()
    end if

  end function linked_list_value

  function linked_list_value_at(this, index) result(res)

    class(linked_list_type), intent(in) :: this
    integer, intent(in) :: index
    class(*), pointer :: res

    integer i
    type(linked_list_item_type), pointer :: item

    item => this%first_item
    do i = 1, this%size
      if (i == index) then
        res => item%value
        return
      else if (i > index) then
        res => null()
        return
      end if
      item => item%next
    end do

  end function linked_list_value_at

  subroutine linked_list_append_item(this, item)

    class(linked_list_type), intent(inout) :: this
    type(linked_list_item_type), intent(inout), pointer :: item

    item%next => null()
    if (associated(this%last_item)) then
      item%prev => this%last_item
      this%last_item%next => item
    end if
    this%last_item => item
    if (.not. associated(this%first_item)) then
      this%first_item => item
    end if
    this%size = this%size + 1

  end subroutine linked_list_append_item

  subroutine linked_list_insert_item_after(this, item1, item2)

    class(linked_list_type), intent(inout) :: this
    type(linked_list_item_type), intent(inout), pointer :: item1
    type(linked_list_item_type), intent(inout), pointer :: item2

    item2%prev => item1
    item2%next => item1%next
    item1%next => item2
    item1%next%prev => item2
    this%size = this%size + 1

  end subroutine linked_list_insert_item_after

  subroutine linked_list_remove_item(this, item)

    class(linked_list_type), intent(inout) :: this
    type(linked_list_item_type), intent(inout), pointer :: item

    if (item%internal_memory) deallocate(item%value)
    if (associated(this%first_item, item)) then
      if (associated(this%first_item%next)) then
        this%first_item => this%first_item%next
        this%first_item%prev => null()
      else
        this%first_item => null()
      end if
    else if (associated(this%last_item, item)) then
      if (associated(this%last_item%prev)) then
        this%last_item => this%last_item%prev
        this%last_item%next => null()
      else
        this%last_item => null()
      end if
    else
      if (associated(item%prev)) item%prev%next => item%next
      if (associated(item%next)) item%next%prev => item%prev
    end if
    deallocate(item)
    this%size = this%size - 1

  end subroutine linked_list_remove_item

  subroutine linked_list_delete_ptr(this, value)

    class(linked_list_type), intent(inout) :: this
    class(*), intent(in), target :: value

    integer i
    type(linked_list_item_type), pointer :: item

    item => this%first_item
    do i = 1, this%size
      if (associated(item%value, value)) then
        call this%remove_item(item)
        return
      end if
      item => item%next
    end do

  end subroutine linked_list_delete_ptr

  subroutine linked_list_delete_at(this, index)

    class(linked_list_type), intent(inout) :: this
    integer, intent(in) :: index

    integer i
    type(linked_list_item_type), pointer :: item

    item => this%first_item
    do i = 1, this%size
      if (i == index) then
        call this%remove_item(item)
        return
      end if
      item => item%next
    end do

  end subroutine linked_list_delete_at

  ! ------------------------------------------------------------------------------
  !                                Clear method
  ! ------------------------------------------------------------------------------

  subroutine linked_list_clear(this)

    class(linked_list_type), intent(inout) :: this

    call linked_list_finalize(this)

  end subroutine linked_list_clear

  ! ----------------------------------------------------------------------------
  !                              Replace methods
  ! ----------------------------------------------------------------------------

  subroutine linked_list_replace_ptr(this, old_value, new_value, old_value2)

    class(linked_list_type), intent(inout) :: this
    class(*), intent(in), target :: old_value
    class(*), intent(in), target :: new_value
    class(*), intent(in), target, optional :: old_value2

    type(linked_list_iterator_type) iterator

    iterator = linked_list_iterator(this)
    do while (.not. iterator%ended())
      if (associated(iterator%value, old_value) .or. (present(old_value2) .and. associated(iterator%value, old_value2))) then
        if (iterator%item%internal_memory) deallocate(iterator%item%value)
        iterator%item%value => new_value
        iterator%item%internal_memory = .false.
        return
      end if
      call iterator%next()
    end do

  end subroutine linked_list_replace_ptr

  ! ----------------------------------------------------------------------------
  !                               Finalizers
  ! ----------------------------------------------------------------------------

  subroutine linked_list_item_finalize(this)

    type(linked_list_item_type), intent(inout) :: this

    if (this%internal_memory .and. associated(this%value)) deallocate(this%value)

  end subroutine linked_list_item_finalize

  subroutine linked_list_finalize(this)

    type(linked_list_type), intent(inout) :: this

    type(linked_list_item_type), pointer :: item1, item2

    item1 => this%first_item
    do while (associated(item1))
      item2 => item1%next
      deallocate(item1)
      item1 => item2
    end do
    this%size = 0
    this%first_item => null()
    this%last_item => null()

  end subroutine linked_list_finalize

end module linked_list_mod
