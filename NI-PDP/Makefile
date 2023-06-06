all:
	g++ -std=c++17 -o main main.cpp

seq:
	g++ -I/opt/share/boost/include -L/opt/share/boost/lib -lboost_serialization -o seq.out sequential.cpp

mpi:
	mpic++ -I/opt/share/boost/include -L/opt/share/boost/lib -lboost_mpi -lboost_serialization -fopenmp -o mpi.out main.cpp

task:
	g++ -I/opt/share/boost/include -L/opt/share/boost/lib -lboost_mpi -lboost_serialization -fopenmp -o task.out task.cpp

data:
	g++ -I/opt/share/boost/include -L/opt/share/boost/lib -lboost_mpi -lboost_serialization -fopenmp -o data.out data.cpp

mpi2:
	OMP_NUM_THREADS=2 mpirun -np 2 --bind-to core --map-by ppr:2:node:pe=thds -x OMP_NUM_THREADS ./boostmpi graphs/graf_15_5.txt