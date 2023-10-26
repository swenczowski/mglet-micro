FC=/home/swenczowski/Software/openmpi_gcc11_cuda/bin/mpif90
CC=/home/swenczowski/Software/openmpi_gcc11_cuda/bin/mpicc

FFLAGS=-c -g -fopenmp -foffload=nvptx-none
LFLAGS=-fopenmp -foffload=nvptx-none -fPIC -lmpi_usempif08 -L/home/swenczowski/Software/openmpi_gcc11_cuda/lib64
# unified memory management with -ta=tesla:cc80,managed
# passing standard parallel Fortan operation to GPU -stdpar=gpu

# flags to run on CPU only
# FFLAGS=-c -O2 -Minfo=all
# LFLAGS=-O2 -Minfo=all -lnvToolsExt

mglet.exe: mglet.o field_mod.o gpu_openmp_mod.o precision_mod.o timeloop_mod.o
	$(FC) mglet.o field_mod.o gpu_openmp_mod.o precision_mod.o timeloop_mod.o $(LFLAGS) -o mglet.exe

mglet.o: src/mglet.F90 field_mod.o gpu_openmp_mod.o precision_mod.o timeloop_mod.o
	$(FC) $(FFLAGS) src/mglet.F90

field_mod.o: src/field_mod.F90 gpu_openmp_mod.o precision_mod.o
	$(FC) $(FFLAGS) src/field_mod.F90

gpu_openmp_mod.o: src/gpu_openmp_mod.F90 precision_mod.o
	$(FC) $(FFLAGS) src/gpu_openmp_mod.F90

precision_mod.o: src/precision_mod.F90
	$(FC) $(FFLAGS) src/precision_mod.F90

timeloop_mod.o: src/timeloop_mod.F90
	$(FC) $(FFLAGS) src/timeloop_mod.F90

# Clean current directory, but leave executable file
.PHONY: clean
clean:
	rm -f *.o *.f *.f90 *.mod *.s *~ *.exe *_t1

