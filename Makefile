MAKEFILE = Makefile
exe = ising		
fcomp = gfortran #ifort # /opt/intel/compiler70/ia32/bin/ifc  
# Warning: the debugger doesn't get along with the optimization options
# So: not use -O3 WITH -g option
flags =  -O3  
# Remote compilation
OBJS = ziggurat.o ising.o init_system.o init_energy.o init_magnet.o delta_e.o

.SUFFIXES:            # this deletes the default suffixes 
.SUFFIXES: .f90 .o    # this defines the extensions I want 

.f90.o:  
	$(fcomp) -c $(flags) $< 
        

$(exe):  $(OBJS) Makefile 
	$(fcomp) $(flags) -o $(exe) $(OBJS) 


clean:
	rm ./*.o ./*.mod	

init_system.o: init_system.f90 
init_energy.o: init_energy.f90
init_magnet.o: init_magnet.f90
delta_e.o: delta_e.f90
ising.o: ising.f90 ziggurat.o init_system.o init_energy.o init_magnet.o delta_e.o
