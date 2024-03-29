program gameoflife

  use sdl2
  use iso_fortran_env, stderr => error_unit
  implicit none

  type :: history_point
    integer :: n = 0, nchg = 0
  end type

  integer, parameter :: n1 = 190, n2 = 330, n3 = 1, sqpix = 5
  integer :: i, j
  logical(1), allocatable :: plane(:,:), plane2(:,:)
  type(c_ptr)     :: window, renderer
  type(sdl_event) :: event
  integer :: rc, timer
  logical :: autoplay = .false.
  character(len = 8) :: ruleset
  type(history_point), allocatable :: his(:)
  integer :: it

  call get_command_argument(1, ruleset)
  if (ruleset == "") ruleset = 'original'

  rc = sdl_init(ior(SDL_INIT_VIDEO, SDL_INIT_TIMER))

  window = sdl_create_window('game of life (' // trim(ruleset) // ')' // char(0), &
  &   SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, &
  &   n2 * sqpix - 1, n1 * sqpix - 1, SDL_WINDOW_SHOWN)

  if (.not. c_associated(window)) then
    write (stderr, *) 'SDL Error: ', sdl_get_error()
    error stop
  end if

  renderer = sdl_create_renderer(window, -1, 0)

  call random_seed()

  allocate(plane(0:n1+1, 0:n2+1), plane2(0:n1+1, 0:n2+1))

  ! populate_plane: block
  !   real, dimension(0:n1+1, 0:n2+1) :: a, b, c
  !   real :: thr
  !   call random_number(a)
  !   call random_number(thr)
  !   thr = 0.2 + 0.6 * thr
  !   plane(:,:) = (a < thr)
  ! end block populate_plane

  plane(:,:) = .false.

  call repaint(renderer, plane)
  timer = 0
  it = 0

  event_loop: do
    if (sdl_poll_event(event) > 0) then
      select case (event % type)
      case (SDL_QUITEVENT)
        exit event_loop
      case (SDL_MOUSEBUTTONDOWN)
        edit_plane: block
          integer :: i, j
          integer, parameter :: r = 5
          real, allocatable :: a(:,:)
          i = floor(event % button % y / real(sqpix)) + 1
          j = floor(event % button % x / real(sqpix)) + 1
          if (i >= 1 .and. i <= n1 .and. j >= 1 .and. j <= n2) then
            associate (ilo => max(i - r, 1), ihi => min(i + r, n1), &
              & jlo => max(j - r, 1), jhi => min(j + r, n2))
              allocate(a(ilo:ihi,jlo:jhi))
              call random_number(a)
              plane(ilo:ihi,jlo:jhi) = plane(ilo:ihi,jlo:jhi) .neqv. (a < 0.33)
              deallocate(a)
            end associate
          end if
          call repaint(renderer, plane)
        end block edit_plane
      case (SDL_KEYDOWN)
        select case (event % key % key_sym % sym)
        case (ichar('q'))
          exit event_loop
        case (32)
          call one_step
        case (ichar('s'))
          autoplay = .not. autoplay
        end select
      end select
    else if (autoplay .and. sdl_get_ticks() - timer > 32) then
      call one_step
      timer = sdl_get_ticks()
    end if
  end do event_loop

  call sdl_destroy_window(window)
  call sdl_quit()

contains

  subroutine one_step
    integer :: i, rc
    type(history_point) :: h

    it = it + 1
    call advance(ruleset, plane, plane2)

    h = history_point(n = count(plane2(1:n1,1:n2)), &
    & nchg = count(plane(1:n1,1:n2) .neqv. plane2(1:n1,1:n2)))

    print '(i4,2i6,f5.2)', it, h % n, h % nchg, h % nchg / real(h % n)

    if (h % nchg == 0) autoplay = .false.

    if (.not. allocated(his)) then
      his = [h]
    else
      his = [his, h]
    end if

    plane(:,:) = plane2
    call repaint(renderer, plane)

  end subroutine

  subroutine repaint(renderer, plane)
    type(c_ptr), intent(inout) :: renderer
    logical(1), contiguous, intent(in) :: plane(0:,0:)
    integer :: i, j, rc

    rc = sdl_set_render_draw_color(renderer, uint8(0), uint8(0), uint8(0), uint8(SDL_ALPHA_OPAQUE))
    rc = sdl_render_clear(renderer)

    do j = lbound(plane, 2) + 1, ubound(plane, 2) - 1
      do i = lbound(plane, 1) + 1, ubound(plane, 1) - 1
        if (plane(i,j)) then
          rc = sdl_set_render_draw_color(renderer, uint8(255), uint8(127), uint8(255), uint8(SDL_ALPHA_OPAQUE))
          rc = sdl_render_fill_rect(renderer, sdl_rect(x = sqpix * (j - 1), &
          &   y = sqpix * (i - 1), w = sqpix - 1, h = sqpix - 1))
        end if
      end do
    end do

    if (allocated(his)) then
      do i = 1, size(his)
        rc = sdl_set_render_draw_color(renderer, uint8(200), uint8(200), uint8(200), uint8(160))
        associate (xc => n2 * sqpix * (0.02 + 0.96 * (i - 1) / real(size(his))), &
          & yc => n1 * sqpix - 3 - 0.5 * his(i) % nchg)
          rc = sdl_render_fill_rect(renderer, sdl_rect(x = xc - 1, &
          &   y = yc - 1, w = 3, h = 3))
        end associate

        rc = sdl_set_render_draw_color(renderer, uint8(250), uint8(0), uint8(0), uint8(160))
        associate (xc => n2 * sqpix * (0.02 + 0.96 * (i - 1) / real(size(his))), &
          & yc => n1 * sqpix - 3 - 50 * his(i) % nchg / real(his(i) % n))
          rc = sdl_render_fill_rect(renderer, sdl_rect(x = xc - 1, &
          &   y = yc - 1, w = 3, h = 3))
        end associate
      end do
    end if

    call sdl_render_present(renderer)
  end subroutine

  subroutine advance(ruleset, plane, plane2)
    character(len = *), intent(in) :: ruleset
    logical(1), contiguous, intent(inout) :: plane(0:,0:)
    logical(1), contiguous, intent(out) :: plane2(0:,0:)
    integer :: n1, n2

    n1 = size(plane, 1) - 2
    n2 = size(plane, 2) - 2

    plane(0,   :) = plane(n1,:)
    plane(n1+1,:) = plane(1, :)
    plane(:,   0) = plane(:,n2)
    plane(:,n2+1) = plane(:, 1)

    select case (ruleset)
    case ('original')
      do concurrent (i = 1:n1, j = 1:n2)
        associate (naround => count(plane(i-1:i+1,j-1:j+1)) - merge(1, 0, plane(i,j)))
          plane2(i,j) = rule_classic(plane(i,j), naround)
        end associate
      end do
    case ('highlife')
      do concurrent (i = 1:n1, j = 1:n2)
        associate (naround => count(plane(i-1:i+1,j-1:j+1)) - merge(1, 0, plane(i,j)))
          plane2(i,j) = rule_highlife(plane(i,j), naround)
        end associate
      end do
    case ('cellular')
      do concurrent (i = 1:n1, j = 1:n2)
        associate (naround => count(plane(i-1:i+1,j-1:j+1)) - merge(1, 0, plane(i,j)))
          plane2(i,j) = rule_cellular(plane(i,j), naround)
        end associate
      end do
    case default
      error stop
    end select


  end subroutine

  elemental function rule_classic(alive, n) result(stay)
    logical(1), intent(in) :: alive
    integer, intent(in) :: n
    logical(1) :: stay
    if (alive) then
      stay = n == 2 .or. n == 3
    else
      stay = n == 3
    end if
  end function
  elemental function rule_highlife(alive, n) result(stay)
    logical(1), intent(in) :: alive
    integer, intent(in) :: n
    logical(1) :: stay
    if (alive) then
      stay = n == 2 .or. n == 3
    else
      stay = n == 3 .or. n == 6
    end if
  end function

  elemental function rule_cellular(alive, n) result(stay)
    logical(1), intent(in) :: alive
    integer, intent(in) :: n
    logical(1) :: stay
    stay = n >= 4 .and. n <= 7
  end function

  elemental integer function mod1(a, n)
    integer, intent(in) :: a, n
    mod1 = mod(a - 1, n) + 1
  end function


end program gameoflife
