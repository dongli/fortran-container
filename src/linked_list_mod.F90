module linked_list_mod

  implicit none

  type linked_list_item_type
    class(linked_list_item_type), pointer :: prev => null()
    class(linked_list_item_type), pointer :: next => null()
    character(:), allocatable :: key
    class(*), pointer :: value => null()
  contains
    final :: linked_list_item_finalize
  end type linked_list_item_type

  type linked_list_type
    integer :: size = 0
    class(linked_list_item_type), pointer :: first_item => null()
    class(linked_list_item_type), pointer :: last_item => null()
  contains
    procedure :: insert_item => linked_list_insert_item
    procedure :: remove_item => linked_list_remove_item
    procedure :: item => linked_list_item
    procedure :: insert => linked_list_insert
    procedure :: value => linked_list_value
    final :: linked_list_finalize
  end type linked_list_type

contains

  function linked_list_item(this, key)

    class(linked_list_type), intent(in) :: this
    character(*), intent(in) :: key
    class(linked_list_item_type), pointer :: linked_list_item

    class(linked_list_item_type), pointer :: item

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

  subroutine linked_list_insert(this, key, value)

    class(linked_list_type), intent(inout) :: this
    character(*), intent(in) :: key
    class(*), intent(in) :: value

    type(linked_list_item_type), pointer :: item

    item => this%item(key)

    if (associated(item)) then
      if (same_type_as(value, item%value)) then
        deallocate(item%value)
        allocate(item%value, source=value)
      else
        call this%remove_item(item)
      end if
    end if

    if (.not. associated(item)) then
      ! NOTE: We need to allocate the memory for the item with correct type.
      allocate(item)
      item%key = key
      call this%insert_item(item)
      allocate(item%value, source=value)
    end if

  end subroutine linked_list_insert

  function linked_list_value(this, key)

    class(linked_list_type), intent(in) :: this
    character(*), intent(in) :: key
    class(*), pointer :: linked_list_value

    class(linked_list_item_type), pointer :: item

    item => this%item(key)
    if (associated(item)) then
      linked_list_value => item%value
    else
      nullify(linked_list_value)
    end if

  end function linked_list_value

  subroutine linked_list_insert_item(this, item)

    class(linked_list_type), intent(inout) :: this
    class(linked_list_item_type), intent(inout), pointer :: item

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
    class(linked_list_item_type), intent(inout), pointer :: item

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
