module hash_table_mod

  use linked_list_mod

  implicit none

  type hash_table_item_type
    type(linked_list_type) chain
  end type hash_table_item_type

  type hash_table_type
    type(hash_table_item_type), allocatable :: items(:)
    integer :: chunk_size = 1000
    integer :: size = 0
    real :: max_load_factor = 0.6
  contains
    procedure :: index_number => hash_table_index_number
    procedure :: expand => hash_table_expand
    procedure :: insert => hash_table_insert
    procedure :: value => hash_table_value
    final :: hash_table_finalize
  end type hash_table_type

contains

  ! key -> hash code -> index
  function hash_code(key)

    character(*), intent(in) :: key
    integer hash_code

    integer i

    hash_code = 0
    do i = 1, len_trim(key)
      hash_code = hash_code + i * iachar(key(i:i))
    end do

  end function hash_code

  ! index_number = hash_code mod table_size
  function hash_table_index_number(this, hash_code)

    class(hash_table_type), intent(in) :: this
    integer, intent(in) :: hash_code
    integer hash_table_index_number

    hash_table_index_number = mod(hash_code, size(this%items))

  end function hash_table_index_number

  function hash_table(chunk_size, max_load_factor)

    integer, intent(in), optional :: chunk_size
    real, intent(in), optional :: max_load_factor
    type(hash_table_type) hash_table

    if (present(chunk_size)) hash_table%chunk_size = chunk_size
    if (present(max_load_factor)) hash_table%max_load_factor = max_load_factor

    allocate(hash_table%items(hash_table%chunk_size))

  end function hash_table

  subroutine hash_table_expand(this)

    class(hash_table_type), intent(inout) :: this

  end subroutine hash_table_expand

  subroutine hash_table_insert(this, key, value)

    class(hash_table_type), intent(inout) :: this
    character(*), intent(in) :: key
    class(*), intent(in) :: value

    integer i

    i = this%index_number(hash_code(key))
    call this%items(i)%chain%insert(key, value)
    this%size = this%size + 1

  end subroutine hash_table_insert

  function hash_table_value(this, key)

    class(hash_table_type), intent(in) :: this
    character(*), intent(in) :: key
    class(*), pointer :: hash_table_value

    integer i

    i = this%index_number(hash_code(key))
    hash_table_value => this%items(i)%chain%value(key)

  end function hash_table_value

  subroutine hash_table_finalize(this)

    type(hash_table_type), intent(inout) :: this

    deallocate(this%items)
    this%size = 0

  end subroutine hash_table_finalize

end module hash_table_mod