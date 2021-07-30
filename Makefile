REGRESSION_PATH?=./
VEXRISCV_FILE?=VexRiscv.v
IBUS?=CACHED
IBUS_DATA_WIDTH?=32
DBUS?=CACHED
DBUS_DATA_WIDTH?=32
THREAD_COUNT?=$(shell nproc)

ADDCFLAGS += -CFLAGS -DREGRESSION_PATH='\"$(REGRESSION_PATH)/\"'
ADDCFLAGS += -CFLAGS -DIBUS_${IBUS}
ADDCFLAGS += -CFLAGS -DIBUS_DATA_WIDTH=${IBUS_DATA_WIDTH}
ADDCFLAGS += -CFLAGS -DDBUS_DATA_WIDTH=${DBUS_DATA_WIDTH}

ADDCFLAGS += -CFLAGS -DDBUS_${DBUS}
ADDCFLAGS += -CFLAGS -DREDO=${REDO}
ADDCFLAGS += -CFLAGS -pthread
ADDCFLAGS += -CFLAGS -Wno-unused-result

ADDCFLAGS += -CFLAGS -DTHREAD_COUNT=${THREAD_COUNT}
ADDCFLAGS += -CFLAGS -DSTALL=1
ADDCFLAGS += -CFLAGS -O3

all: obj_dir/VVexRiscv

obj_dir/VVexRiscv.cpp: ${VEXRISCV_FILE}
	cp ${VEXRISCV_FILE}*.bin . | true
	verilator -cc  ${VEXRISCV_FILE} -O3 -CFLAGS -std=c++11 -LDFLAGS -pthread ${ADDCFLAGS} --gdbbt ${VERILATOR_ARGS} -Wno-UNOPTFLAT -Wno-WIDTH  --exe profile.cpp --x-assign unique

obj_dir/VVexRiscv: obj_dir/VVexRiscv.cpp
	$(MAKE)  -j${THREAD_COUNT} -C obj_dir/ -f VVexRiscv.mk VVexRiscv

clean:
	rm -rf obj_dir
	rm -f *.debugTrace
	rm -f *.logTrace
