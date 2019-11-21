FC := gfortran
FFLAGS := -O3 -march=native
# FFLAGS := -g -fcheck=all

f08sdl2 := ./f08sdl2
INCLUDE := -I $(f08sdl2) $(shell pkg-config --cflags sdl2)
LDLIBS := -L $(f08sdl2) -lf08sdl2 $(shell pkg-config --libs sdl2)
VPATH = . $(f08sdl2)

PROGRAMS := sdltest gameoflife

all: $(PROGRAMS)

$(PROGRAMS): %: %.f90 | f08sdl2
	$(FC) $(INCLUDE) $(FFLAGS) $^ $(LDFLAGS) $(LDLIBS) -o $@

f08sdl2:
	$(MAKE) -C $(f08sdl2) FC="$(FC)" FFLAGS="$(FFLAGS) -fpic"
	cd $(f08sdl2) && $(AR) rcs libf08sdl2.a sdl2.o

clean:
	$(RM) *.mod *.smod *.o $(PROGRAMS)
	$(MAKE) -C f08sdl2 clean

.PHONY: all f08sdl2 clean
