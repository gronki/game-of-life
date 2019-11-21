program sdltest

  use sdl2
  use iso_fortran_env, stderr => error_unit
  implicit none

  type(c_ptr)     :: window, renderer
  type(sdl_event) :: event
  integer :: rc

  rc = sdl_init(ior(SDL_INIT_VIDEO, SDL_INIT_TIMER))

  window = sdl_create_window('game of life' // char(0), &
  & SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 640, 480, SDL_WINDOW_SHOWN)

  if (.not. c_associated(window)) then
      write (stderr, *) 'SDL Error: ', sdl_get_error()
      error stop
  end if

  renderer = sdl_create_renderer(window, -1, 0)

  event_loop: do
    if (sdl_poll_event(event) > 0) then
        select case (event % type)
        case (SDL_QUITEVENT)
          exit event_loop
        case (SDL_MOUSEBUTTONDOWN)
          if (event % button % button == 1) then
            rc = sdl_set_render_draw_color(renderer, uint8(0), uint8(0), uint8(0), uint8(SDL_ALPHA_OPAQUE))
            rc = sdl_render_clear(renderer)
            rc = sdl_set_render_draw_color(renderer, uint8(255), uint8(127), uint8(255), uint8(SDL_ALPHA_OPAQUE))
            rc = sdl_render_fill_rect(renderer, sdl_rect(x = event % button % x - 5, &
            &   y = event % button % y - 5, w = 10, h = 10))
            call sdl_render_present(renderer)
            print *, '(x,y) = ', event % button % x, event % button % y
          end if
        case (SDL_KEYDOWN)
          if (event % key % key_sym % sym == ichar('q')) exit event_loop
          print *, event % key % key_sym % sym
        end select
    end if
  end do event_loop

  call sdl_destroy_window(window)
  call sdl_quit()

end program
