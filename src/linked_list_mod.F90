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
    procedure :: append_item => linked_list_append_item
    procedure :: insert_item_after => linked_list_insert_item_after
    procedure :: remove_item => linked_list_remove_item
    procedure :: item => linked_list_item
    ! Append
    ! TODO: Replace insert by append if not given index.
    procedure, private :: append1 => linked_list_append1
    procedure, private :: append2 => linked_list_append2
    generic :: append => append1, append2
    procedure, private :: append_ptr1 => linked_list_append_ptr1
    procedure, private :: append_ptr2 => linked_list_append_ptr2
    generic :: append_ptr => append_ptr1, append_ptr2
    ! Insert
    procedure, private :: insert1 => linked_list_insert1
    procedure, private :: insert2 => linked_list_insert2
    generic :: insert => insert1, insert2
    procedure, private :: insert_ptr1 => linked_list_insert_ptr1
    procedure, private :: insert_ptr2 => linked_list_insert_ptr2
    generic :: insert_ptr => insert_ptr1, insert_ptr2
    procedure :: insert_ptr_at => linked_list_insert_ptr_at
    procedure :: insert_ptr_after => linked_list_insert_ptr_after
    ! Value
    procedure :: value => linked_list_value
    procedure :: value_at => linked_list_value_at
    procedure :: first_value => linked_list_first_value
    procedure :: last_value => linked_list_last_value
    ! Delete
    procedure :: delete => linked_list_delete
    procedure :: delete_at => linked_list_delete_at
    ! Replace
    procedure :: replace => linked_list_replace
    ! Clear
    procedure :: clear => linked_list_clear
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
    nullify(this%value)
    nullify(this%next_item)
    if (associated(this%item)) then
      this%value => this%item%value
      this%next_item => this%item%next
    end if

  end subroutine linked_list_iterator_next

  function linked_list_first_value(this)

    class(linked_list_type), intent(in) :: this
    class(*), pointer :: linked_list_first_value

    nullify(linked_list_first_value)
    if (associated(this%first_item)) then
      linked_list_first_value => this%first_item%value
    end if

  end function linked_list_first_value

  function linked_list_last_value(this)

    class(linked_list_type), intent(in) :: this
    class(*), pointer :: linked_list_last_value

    nullify(linked_list_last_value)
    if (associated(this%first_item)) then
      linked_list_last_value => this%last_item%value
    end if

  end function linked_list_last_value

  function linked_list_item(this, key)

    class(linked_list_type), intent(in) :: this
    character(*), intent(in) :: key
    type(linked_list_item_type), pointer :: linked_list_item

    type(linked_list_item_type), pointer :: item

    item => this%first_item
    do while (associated(item))
      if (item%key == key) then
        linked_list_item => item
        return
      end if
      item => item%next
    end do
    nullify(linked_list_item)

  end function linked_list_item

  subroutine linked_list_append1(this, key, value, nodup)

    class(linked_list_type), intent(inout) :: this
    character(*), intent(in) :: key
    class(*), intent(in) :: value
    logical, intent(in), optional :: nodup

    type(linked_list_item_type), pointer :: item

    if (present(nodup) .and. nodup) then
      item => this%item(key)
      if (associated(item)) then
        if (same_type_as(value, item%value)) then
          deallocate(item%value)
          allocate(item%value, source=value)
          item%internal_memory = .true.
        else
          call this%remove_item(item)
        end if
      end if
    else
      nullify(item)
    end if

    if (.not. associated(item)) then
      ! NOTE: We need to allocate the memory for the item with correct type.
      allocate(item)
      item%key = key
      call this%append_item(item)
      allocate(item%value, source=value)
    end if

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

  subroutine linked_list_append_ptr1(this, key, value, nodup)

    class(linked_list_type), intent(inout) :: this
    character(*), intent(in) :: key
    class(*), intent(in), target :: value
    logical, intent(in), optional :: nodup

    type(linked_list_item_type), pointer :: item

    if (present(nodup) .and. nodup) then
      item => this%item(key)
      if (associated(item)) then
        if (same_type_as(value, item%value)) then
          item%value => value
          item%internal_memory = .false.
        else
          call this%remove_item(item)
        end if
      end if
    else
      nullify(item)
    end if

    if (.not. associated(item)) then
      allocate(item)
      item%key = key
      call this%append_item(item)
      item%value => value
      item%internal_memory = .false.
    end if

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

  subroutine linked_list_insert1(this, key, value, nodup)

    class(linked_list_type), intent(inout) :: this
    character(*), intent(in) :: key
    class(*), intent(in) :: value
    logical, intent(in), optional :: nodup

    type(linked_list_item_type), pointer :: item

    if (present(nodup) .and. nodup) then
      item => this%item(key)
      if (associated(item)) then
        if (same_type_as(value, item%value)) then
          deallocate(item%value)
          allocate(item%value, source=value)
          item%internal_memory = .true.
        else
          call this%remove_item(item)
        end if
      end if
    else
      nullify(item)
    end if

    if (.not. associated(item)) then
      ! NOTE: We need to allocate the memory for the item with correct type.
      allocate(item)
      item%key = key
      call this%append_item(item)
      allocate(item%value, source=value)
    end if

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
        if (same_type_as(value, item%value)) then
          item%value => value
          item%internal_memory = .false.
        else
          call this%remove_item(item)
        end if
      end if
    else
      nullify(item)
    end if

    if (.not. associated(item)) then
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
        call this%append_item(item2)
        item2%value => value2
        item2%internal_memory = .false.
        return
      end if
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
      nullify(res)
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
        nullify(res)
        return
      end if
      item => item%next
    end do

  end function linked_list_value_at

  subroutine linked_list_append_item(this, item)

    class(linked_list_type), intent(inout) :: this
    type(linked_list_item_type), intent(inout), pointer :: item

    nullify(item%next)
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
    item2%next%prev => item2
    this%size = this%size + 1

  end subroutine linked_list_insert_item_after

  subroutine linked_list_remove_item(this, item)

    class(linked_list_type), intent(inout) :: this
    type(linked_list_item_type), intent(inout), pointer :: item

    if (item%internal_memory) deallocate(item%value)
    if (associated(this%first_item, item)) then
      if (associated(this%first_item%next)) then
        this%first_item => this%first_item%next
        nullify(this%first_item%prev)
      else
        nullify(this%first_item)
      end if
    end if
    if (associated(this%last_item, item)) then
      if (associated(this%last_item%prev)) then
        this%last_item => this%last_item%prev
        nullify(this%last_item%next)
      else
        nullify(this%last_item)
      end if
    end if
    deallocate(item)
    this%size = this%size - 1

  end subroutine linked_list_remove_item

  subroutine linked_list_delete(this, value)

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

  end subroutine linked_list_delete

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

  subroutine linked_list_clear(this)

    class(linked_list_type), intent(inout) :: this

    call linked_list_finalize(this)

  end subroutine linked_list_clear

  subroutine linked_list_replace(this, old_value, new_value)

    class(linked_list_type), intent(inout) :: this
    class(*), intent(in), target :: old_value
    class(*), intent(in), target :: new_value

    type(linked_list_iterator_type) iterator

    iterator = linked_list_iterator(this)
    do while (.not. iterator%ended())
      if (associated(iterator%value, old_value)) then
        if (iterator%item%internal_memory) deallocate(iterator%item%value)
        iterator%item%value => new_value
        return
      end if
      call iterator%next()
    end do

  end subroutine linked_list_replace

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

  end subroutine linked_list_finalize

end module linked_list_mod
