program list_test

  use unit_test
  use linked_list_mod

  implicit none

  type test_type
    real i
    integer j
  end type test_type

  call test_suite_init('Linked list tests')

  call test_value()

  call test_append()

  call test_insert()

  call test_delete()

  call test_replace()

  call test_suite_report()

  call test_suite_final()

contains

  subroutine test_value()

    type(linked_list_type) a

    call test_case_create('Test linked list value methods')

    ! Initial size should be zero.
    call assert_equal(a%size, 0, __FILE__, __LINE__)

    call a%append(5)

    select type (val => a%first_value())
    type is (integer)
      call assert_equal(val, 5, __FILE__, __LINE__)
    class default
      call assert_failure(__FILE__, __LINE__)
    end select

    select type (val => a%last_value())
    type is (integer)
      call assert_equal(val, 5, __FILE__, __LINE__)
    class default
      call assert_failure(__FILE__, __LINE__)
    end select

    select type (val => a%value_at(1))
    type is (integer)
      call assert_equal(val, 5, __FILE__, __LINE__)
    class default
      call assert_failure(__FILE__, __LINE__)
    end select

    call a%append('foo', 6)

    select type (val => a%value('foo'))
    type is (integer)
      call assert_equal(val, 6, __FILE__, __LINE__)
    class default
      call assert_failure(__FILE__, __LINE__)
    end select

  end subroutine test_value

  subroutine test_append()

    type(linked_list_type) a
    type(test_type), target :: x
    type(linked_list_item_type), pointer :: item

    call test_case_create('Test linked list append methods')

    call a%append(1)
    call assert_equal(a%size, 1, __FILE__, __LINE__)
    call assert_true(a%last_item%internal_memory, __FILE__, __LINE__)
    select type (val => a%last_value())
    type is (integer)
      call assert_equal(val, 1, __FILE__, __LINE__)
    class default
      call assert_failure(__FILE__, __LINE__)
    end select

    call a%append_ptr(x)
    x%i = 3.324
    x%j = 32
    call assert_equal(a%size, 2, __FILE__, __LINE__)
    call assert_false(a%last_item%internal_memory, __FILE__, __LINE__)
    select type (val => a%last_value())
    type is (test_type)
      call assert_equal(val%i, x%i, __FILE__, __LINE__)
      call assert_equal(val%j, x%j, __FILE__, __LINE__)
    class default
      call assert_failure(__FILE__, __LINE__)
    end select

    call a%append('a', 3.2)
    call assert_equal(a%size, 3, __FILE__, __LINE__)
    call assert_true(a%last_item%internal_memory, __FILE__, __LINE__)
    select type (val => a%last_value())
    type is (real)
      call assert_equal(val, 3.2, __FILE__, __LINE__)
    class default
      call assert_failure(__FILE__, __LINE__)
    end select

  end subroutine test_append

  subroutine test_insert()

    type(linked_list_type) a
    type(test_type), pointer :: x
    type(test_type), target :: y
    type(linked_list_item_type), pointer :: item

    call test_case_create('Test linked list insert methods')

    ! Item 1
    ! Insert a string.
    call a%insert('foo')
    call assert_equal(a%size, 1, __FILE__, __LINE__)
    select type (val => a%last_value())
    type is (character(*))
      call assert_equal(val, 'foo', __FILE__, __LINE__)
    class default
      call assert_failure(__FILE__, __LINE__)
    end select

    ! Item 2
    ! Insert an integer with key.
    call a%insert('2', 1)
    call assert_equal(a%size, 2, __FILE__, __LINE__)
    select type (val => a%last_value())
    type is (integer)
      call assert_equal(val, 1, __FILE__, __LINE__)
    class default
      call assert_failure(__FILE__, __LINE__)
    end select

    ! Change the value to a real number.
    call a%insert('2', 4.2, nodup=.true.)
    call assert_equal(a%size, 2, __FILE__, __LINE__)
    call assert_true(associated(a%item('2')), __FILE__, __LINE__)
    call assert_true(associated(a%value('2')), __FILE__, __LINE__)
    select type (val => a%last_value())
    type is (real)
      call assert_equal(val, 4.2, __FILE__, __LINE__)
    class default
      call assert_failure(__FILE__, __LINE__)
    end select

    ! Item 3
    ! Insert a new item although its key is the same with the previous one.
    call a%insert('3', .true.)
    call assert_equal(a%size, 3, __FILE__, __LINE__)
    select type (val => a%last_value())
    type is (logical)
      call assert_true(val, __FILE__, __LINE__)
    class default
      call assert_failure(__FILE__, __LINE__)
    end select
    select type (val => a%last_value())
    type is (logical)
      call assert_true(val, __FILE__, __LINE__)
    class default
      call assert_failure(__FILE__, __LINE__)
    end select

    ! Item 4
    ! Insert a defined type with its address.
    allocate(x)
    x%i = 1.2
    x%j = 19
    call a%insert_ptr('4', x)
    call assert_equal(a%size, 4, __FILE__, __LINE__)
    call assert_false(a%last_item%internal_memory, __FILE__, __LINE__)
    call assert_true(associated(a%last_item%value, x), __FILE__, __LINE__)
    select type (val => a%last_value())
    type is (test_type)
      call assert_equal(val%i, x%i, __FILE__, __LINE__)
      call assert_equal(val%j, x%j, __FILE__, __LINE__)
    class default
      call assert_failure(__FILE__, __LINE__)
    end select

    ! Item 5
    y%i = 1.5
    y%j = 5
    call a%insert_ptr_after(x, y)
    call assert_equal(a%size, 5, __FILE__, __LINE__)
    item => a%item_at(5)
    call assert_false(item%internal_memory, __FILE__, __LINE__)
    call assert_true(associated(a%value_at(5), y), __FILE__, __LINE__)
    select type (val => a%value_at(5))
    type is (test_type)
      call assert_equal(val%i, y%i, __FILE__, __LINE__)
      call assert_equal(val%j, y%j, __FILE__, __LINE__)
    class default
      call assert_failure(__FILE__, __LINE__)
    end select

    call a%clear()
    call assert_equal(a%size, 0, __FILE__, __LINE__)

    ! Check if the external objects are not freed.
    call assert_true(associated(x), __FILE__, __LINE__)
    call assert_equal(y%i, y%i, __FILE__, __LINE__)

    if (associated(x)) deallocate(x)

  end subroutine test_insert

  subroutine test_delete()

    type(linked_list_type) a
    type(test_type), target :: x

    call test_case_create('Test linked list delete methods')

    call a%append(1)
    call a%append(3.2)
    call a%append(.true.)
    call a%append_ptr(x)

    call assert_equal(a%size, 4, __FILE__, __LINE__)

    call a%delete_ptr(x)
    call assert_equal(a%size, 3, __FILE__, __LINE__)
    select type (val => a%last_value())
    type is (logical)
      call assert_true(val, __FILE__, __LINE__)
    class default
      call assert_failure(__FILE__, __LINE__)
    end select

    call a%delete_at(2)
    call assert_equal(a%size, 2, __FILE__, __LINE__)
    select type (val => a%first_value())
    type is (integer)
      call assert_equal(val, 1, __FILE__, __LINE__)
    class default
      call assert_failure(__FILE__, __LINE__)
    end select
    select type (val => a%last_value())
    type is (logical)
      call assert_true(val, __FILE__, __LINE__)
    class default
      call assert_failure(__FILE__, __LINE__)
    end select

  end subroutine test_delete

  subroutine test_replace()

    type(linked_list_type) a
    type(test_type), target :: x
    type(test_type), target :: y

    call test_case_create('Test linked list replace method')

    call a%append(3.2)
    call a%append_ptr(x)

    call assert_equal(a%size, 2, __FILE__, __LINE__)

    x%i = 1
    y%i = 2
    call a%replace_ptr(x, y)
    call assert_equal(a%size, 2, __FILE__, __LINE__)
    select type (val => a%last_value())
    type is (test_type)
      call assert_equal(val%i, y%i, __FILE__, __LINE__)
    class default
      call assert_failure(__FILE__, __LINE__)
    end select

  end subroutine test_replace

end program list_test
