module linked_list_mod

  implicit none

  type linked_list_item_type
    type(linked_list_item_type), pointer :: prev => null()
    type(linked_list_item_type), pointer :: next => null()
    character(:), allocatable :: key
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
    procedure :: insert_item => linked_list_insert_item
    procedure :: remove_item => linked_list_remove_item
    procedure :: item => linked_list_item
    procedure, private :: insert1 => linked_list_insert1
    procedure, private :: insert2 => linked_list_insert2
    generic :: insert => insert1, insert2
    procedure, private :: insert_ptr1 => linked_list_insert_ptr1
    procedure, private :: insert_ptr2 => linked_list_insert_ptr2
    generic :: insert_ptr => insert_ptr1, insert_ptr2
    procedure :: value => linked_list_value
    procedure :: first_value => linked_list_first_value
    procedure :: last_value => linked_list_last_value
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
      call this%insert_item(item)
      allocate(item%value, source=value)
    end if

  end subroutine linked_list_insert1

  subroutine linked_list_insert2(this, value)

    class(linked_list_type), intent(inout) :: this
    class(*), intent(in) :: value

    type(linked_list_item_type), pointer :: item

    allocate(item)
    call this%insert_item(item)
    allocate(item%value, source=value)

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
          deallocate(item%value)
          allocate(item%value, source=value)
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
      call this%insert_item(item)
      item%value => value
    end if

  end subroutine linked_list_insert_ptr1

  subroutine linked_list_insert_ptr2(this, value)

    class(linked_list_type), intent(inout) :: this
    class(*), intent(in), target :: value

    type(linked_list_item_type), pointer :: item

    allocate(item)
    call this%insert_item(item)
    item%value => value

  end subroutine linked_list_insert_ptr2

  function linked_list_value(this, key)

    class(linked_list_type), intent(in) :: this
    character(*), intent(in) :: key
    class(*), pointer :: linked_list_value

    type(linked_list_item_type), pointer :: item

    item => this%item(key)
    if (associated(item)) then
      linked_list_value => item%value
    else
      nullify(linked_list_value)
    end if

  end function linked_list_value

  subroutine linked_list_insert_item(this, item)

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

  end subroutine linked_list_insert_item

  subroutine linked_list_remove_item(this, item)

    class(linked_list_type), intent(inout) :: this
    type(linked_list_item_type), intent(inout), pointer :: item

    deallocate(item%value)
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

  subroutine linked_list_clear(this)

    class(linked_list_type), intent(inout) :: this

    call linked_list_finalize(this)

  end subroutine linked_list_clear

  subroutine linked_list_item_finalize(this)

    type(linked_list_item_type), intent(inout) :: this

    if (associated(this%value)) deallocate(this%value)

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

  end subroutine linked_list_finalize

end module linked_list_mod
