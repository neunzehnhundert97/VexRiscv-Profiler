#include "VVexRiscv.h"
#include "VVexRiscv_VexRiscv.h"
#ifdef REF
#include "VVexRiscv_RiscvCore.h"
#endif
#include "verilated.h"
#include "verilated_vcd_c.h"
#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <stdint.h>
#include <cstring>
#include <string.h>
#include <iostream>
#include <fstream>
#include <vector>
#include <mutex>
#include <iomanip>
#include <queue>
#include <time.h>
#include <stack>
#include "encoding.h"

//#define FLOW_INFO

int profileMode = 1;
uint32_t debuggedFunction = 0;
int desiredFunctionCalls = 0;
uint32_t *registeredLabels = NULL;
uint32_t numberOfLabels = 0;

std::stack<uint32_t> functionStack;
int currentPCCounter = 0;
int32_t lastPc = 0;
int32_t lastIns = 0;
int functionOccurred = 0;

using namespace std;

struct timespec timer_get()
{
	struct timespec start_time;
	clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start_time);
	return start_time;
}

class Memory
{
public:
	uint8_t *mem[1 << 12];

	Memory()
	{
		for (uint32_t i = 0; i < (1 << 12); i++)
			mem[i] = NULL;
	}
	~Memory()
	{
		for (uint32_t i = 0; i < (1 << 12); i++)
			if (mem[i])
				delete[] mem[i];
	}

	uint8_t *get(uint32_t address)
	{
		if (mem[address >> 20] == NULL)
		{
			uint8_t *ptr = new uint8_t[1024 * 1024];
			for (uint32_t i = 0; i < 1024 * 1024; i += 4)
			{
				ptr[i + 0] = 0xFF;
				ptr[i + 1] = 0xFF;
				ptr[i + 2] = 0xFF;
				ptr[i + 3] = 0xFF;
			}
			mem[address >> 20] = ptr;
		}
		return &mem[address >> 20][address & 0xFFFFF];
	}

	void read(uint32_t address, uint32_t length, uint8_t *data)
	{
		for (int i = 0; i < length; i++)
		{
			data[i] = (*this)[address + i];
		}
	}

	void write(uint32_t address, uint32_t length, uint8_t *data)
	{
		for (int i = 0; i < length; i++)
		{
			(*this)[address + i] = data[i];
		}
	}

	uint8_t &operator[](uint32_t address)
	{
		return *get(address);
	}
};

uint32_t hti(char c)
{
	if (c >= 'A' && c <= 'F')
		return c - 'A' + 10;
	if (c >= 'a' && c <= 'f')
		return c - 'a' + 10;
	return c - '0';
}

uint32_t hToI(char *c, uint32_t size)
{
	uint32_t value = 0;
	for (uint32_t i = 0; i < size; i++)
	{
		value += hti(c[i]) << ((size - i - 1) * 4);
	}
	return value;
}

void loadHexImpl(string path, Memory *mem)
{
	FILE *fp = fopen(&path[0], "r");
	if (fp == 0)
	{
		cout << path << " not found" << endl;
	}
	//Preload 0x0 <-> 0x80000000 jumps
	((uint32_t *)mem->get(0))[0] = 0x800000b7;
	((uint32_t *)mem->get(0))[1] = 0x000080e7;
	((uint32_t *)mem->get(0x80000000))[0] = 0x00000097;

	fseek(fp, 0, SEEK_END);
	uint32_t size = ftell(fp);
	fseek(fp, 0, SEEK_SET);
	char *content = new char[size];
	fread(content, 1, size, fp);
	fclose(fp);

	int offset = 0;
	char *line = content;
	while (1)
	{
		if (line[0] == ':')
		{
			uint32_t byteCount = hToI(line + 1, 2);
			uint32_t nextAddr = hToI(line + 3, 4) + offset;
			uint32_t key = hToI(line + 7, 2);
			//			printf("%d %d %d\n", byteCount, nextAddr,key);
			switch (key)
			{
			case 0:
				for (uint32_t i = 0; i < byteCount; i++)
				{
					*(mem->get(nextAddr + i)) = hToI(line + 9 + i * 2, 2);
					//printf("%x %x %c%c\n",nextAddr + i,hToI(line + 9 + i*2,2),line[9 + i * 2],line[9 + i * 2+1]);
				}
				break;
			case 2:
				//				cout << offset << endl;
				offset = hToI(line + 9, 4) << 4;
				break;
			case 4:
				//				cout << offset << endl;
				offset = hToI(line + 9, 4) << 16;
				break;
			default:
				//				cout << "??? " << key << endl;
				break;
			}
		}

		while (*line != '\n' && size != 0)
		{
			line++;
			size--;
		}
		if (size <= 1)
			break;
		line++;
		size--;
	}

	delete[] content;
}

void loadBinImpl(string path, Memory *mem, uint32_t offset)
{
	FILE *fp = fopen(&path[0], "r");
	if (fp == 0)
	{
		cout << path << " not found" << endl;
	}

	fseek(fp, 0, SEEK_END);
	uint32_t size = ftell(fp);
	fseek(fp, 0, SEEK_SET);
	char *content = new char[size];
	fread(content, 1, size, fp);
	fclose(fp);

	for (int byteId = 0; byteId < size; byteId++)
	{
		*(mem->get(offset + byteId)) = content[byteId];
	}

	delete[] content;
}

#define TEXTIFY(A) #A

#define assertEq(x, ref)                                                       \
	if (x != ref)                                                              \
	{                                                                          \
		printf("\n*** %s is %d but should be %d ***\n\n", TEXTIFY(x), x, ref); \
		throw std::exception();                                                \
	}

class success : public std::exception
{
};

#define MVENDORID 0xF11 // MRO Vendor ID.
#define MARCHID 0xF12	// MRO Architecture ID.
#define MIMPID 0xF13	// MRO Implementation ID.
#define MHARTID 0xF14	// MRO Hardware thread ID.Machine Trap Setup
#define MSTATUS 0x300	// MRW Machine status register.
#define MISA 0x301		// MRW ISA and extensions
#define MEDELEG 0x302	// MRW Machine exception delegation register.
#define MIDELEG 0x303	// MRW Machine interrupt delegation register.
#define MIE 0x304		// MRW Machine interrupt-enable register.
#define MTVEC 0x305		// MRW Machine trap-handler base address. Machine Trap Handling
#define MSCRATCH 0x340	// MRW Scratch register for machine trap handlers.
#define MEPC 0x341		// MRW Machine exception program counter.
#define MCAUSE 0x342	// MRW Machine trap cause.
#define MBADADDR 0x343	// MRW Machine bad address.
#define MIP 0x344		// MRW Machine interrupt pending.
#define MBASE 0x380		// MRW Base register.
#define MBOUND 0x381	// MRW Bound register.
#define MIBASE 0x382	// MRW Instruction base register.
#define MIBOUND 0x383	// MRW Instruction bound register.
#define MDBASE 0x384	// MRW Data base register.
#define MDBOUND 0x385	// MRW Data bound register.
#define MCYCLE 0xB00	// MRW Machine cycle counter.
#define MINSTRET 0xB02	// MRW Machine instructions-retired counter.
#define MCYCLEH 0xB80	// MRW Upper 32 bits of mcycle, RV32I only.
#define MINSTRETH 0xB82 // MRW Upper 32 bits of minstret, RV32I only.

#define SSTATUS 0x100
#define SIE 0x104
#define STVEC 0x105
#define SCOUNTEREN 0x106
#define SSCRATCH 0x140
#define SEPC 0x141
#define SCAUSE 0x142
#define STVAL 0x143
#define SIP 0x144
#define SATP 0x180

#define SSTATUS_SIE 0x00000002
#define SSTATUS_SPIE 0x00000020
#define SSTATUS_SPP 0x00000100

#ifdef SUPERVISOR
#define MSTATUS_READ_MASK 0xFFFFFFFF
#else
#define MSTATUS_READ_MASK 0x1888
#endif

class RiscvGolden
{
public:
	int32_t pc, lastPc;
	uint32_t lastInstruction;
	int32_t regs[32];
	uint64_t stepCounter;

	uint32_t mscratch, sscratch;
	uint32_t misa;
	uint32_t privilege;

	uint32_t medeleg;
	uint32_t mideleg;

	union status
	{
		uint32_t raw;
		struct
		{
			uint32_t _1a : 1;
			uint32_t sie : 1;
			uint32_t _1b : 1;
			uint32_t mie : 1;
			uint32_t _2a : 1;
			uint32_t spie : 1;
			uint32_t _2b : 1;
			uint32_t mpie : 1;
			uint32_t spp : 1;
			uint32_t _3 : 2;
			uint32_t mpp : 2;
			uint32_t _4 : 4;
			uint32_t mprv : 1;
			uint32_t sum : 1;
			uint32_t mxr : 1;
		};
	} __attribute__((packed)) status;

	uint32_t ipInput;
	uint32_t ipSoft;
	union IpOr
	{
		uint32_t raw;
		struct
		{
			uint32_t _1a : 1;
			uint32_t ssip : 1;
			uint32_t _1b : 1;
			uint32_t msip : 1;
			uint32_t _2a : 1;
			uint32_t stip : 1;
			uint32_t _2b : 1;
			uint32_t mtip : 1;
			uint32_t _3a : 1;
			uint32_t seip : 1;
			uint32_t _3b : 1;
			uint32_t meip : 1;
		};
	} __attribute__((packed));

	IpOr getIp()
	{
		IpOr ret;
		ret.raw = ipSoft | ipInput;
		return ret;
	}

	union mie
	{
		uint32_t raw;
		struct
		{
			uint32_t _1a : 1;
			uint32_t ssie : 1;
			uint32_t _1b : 1;
			uint32_t msie : 1;
			uint32_t _2a : 1;
			uint32_t stie : 1;
			uint32_t _2b : 1;
			uint32_t mtie : 1;
			uint32_t _3a : 1;
			uint32_t seie : 1;
			uint32_t _3b : 1;
			uint32_t meie : 1;
		};
	} __attribute__((packed)) ie;

	union Xtvec
	{
		uint32_t raw;
		struct __attribute__((packed))
		{
			uint32_t _1 : 2;
			uint32_t base : 30;
		};
	};

	Xtvec mtvec, stvec;

	union mcause
	{
		uint32_t raw;
		struct __attribute__((packed))
		{
			uint32_t exceptionCode : 31;
			uint32_t interrupt : 1;
		};
	} mcause;

	union scause
	{
		uint32_t raw;
		struct __attribute__((packed))
		{
			uint32_t exceptionCode : 31;
			uint32_t interrupt : 1;
		};
	} scause;

	union satp
	{
		uint32_t raw;
		struct __attribute__((packed))
		{
			uint32_t ppn : 22;
			uint32_t _x : 9;
			uint32_t mode : 1;
		};
	} satp;

	union Tlb
	{
		uint32_t raw;
		struct __attribute__((packed))
		{
			uint32_t v : 1;
			uint32_t r : 1;
			uint32_t w : 1;
			uint32_t x : 1;
			uint32_t u : 1;
			uint32_t _dummy : 5;
			uint32_t ppn : 22;
		};
		struct __attribute__((packed))
		{
			uint32_t _dummyX : 10;
			uint32_t ppn0 : 10;
			uint32_t ppn1 : 12;
		};
	};

	bool lrscReserved;
	uint32_t lrscReservedAddress;

	RiscvGolden()
	{
		pc = 0x80000000;
		regs[0] = 0;
		for (int i = 0; i < 32; i++)
			regs[i] = 0;

		status.raw = 0;
		ie.raw = 0;
		mtvec.raw = 0x80000020;
		mcause.raw = 0;
		mbadaddr = 0;
		mepc = 0;
		misa = 0x40041101; //TODO
		status.raw = 0;
		status.mpp = 3;
		status.spp = 1;
		privilege = 3;
		medeleg = 0;
		mideleg = 0;
		satp.mode = 0;
		ipSoft = 0;
		ipInput = 0;
		stepCounter = 0;
		sbadaddr = 42;
		lrscReserved = false;
	}

	virtual void rfWrite(int32_t address, int32_t data)
	{
		if (address != 0)
			regs[address] = data;
	}

	virtual void pcWrite(int32_t target)
	{
		if (isPcAligned(target))
		{
			lastPc = pc;
			pc = target;
		}
		else
		{
			trap(0, 0, target);
		}
	}
	uint32_t mbadaddr, sbadaddr;
	uint32_t mepc, sepc;

	virtual bool iRead(int32_t address, uint32_t *data) = 0;
	virtual bool dRead(int32_t address, int32_t size, uint32_t *data) = 0;
	virtual void dWrite(int32_t address, int32_t size, uint32_t data) = 0;

	enum AccessKind
	{
		READ,
		WRITE,
		EXECUTE,
		READ_WRITE
	};
	virtual bool isMmuRegion(uint32_t v) = 0;
	bool v2p(uint32_t v, uint32_t *p, AccessKind kind)
	{
		uint32_t effectivePrivilege = status.mprv && kind != EXECUTE ? status.mpp : privilege;
		if (effectivePrivilege == 3 || satp.mode == 0 || !isMmuRegion(v))
		{
			*p = v;
		}
		else
		{
			Tlb tlb;
			dRead((satp.ppn << 12) | ((v >> 22) << 2), 4, &tlb.raw);
			if (!tlb.v)
				return true;
			bool superPage = true;
			if (!tlb.x && !tlb.r && !tlb.w)
			{
				dRead((tlb.ppn << 12) | (((v >> 12) & 0x3FF) << 2), 4, &tlb.raw);
				if (!tlb.v)
					return true;
				superPage = false;
			}
			if (!tlb.u && effectivePrivilege == 0)
				return true;
			if (tlb.u && effectivePrivilege == 1 && !status.sum)
				return true;
			if (superPage && tlb.ppn0 != 0)
				return true;
			if (kind == READ || kind == READ_WRITE)
				if (!tlb.r && !(status.mxr && tlb.x))
					return true;
			if (kind == WRITE || kind == READ_WRITE)
				if (!tlb.w)
					return true;
			if (kind == EXECUTE)
				if (!tlb.x)
					return true;

			*p = (tlb.ppn1 << 22) | (superPage ? v & 0x3FF000 : tlb.ppn0 << 12) | (v & 0xFFF);
		}
		return false;
	}

	void trap(bool interrupt, int32_t cause)
	{
		trap(interrupt, cause, false, 0);
	}
	void trap(bool interrupt, int32_t cause, uint32_t value)
	{
		trap(interrupt, cause, true, value);
	}
	void trap(bool interrupt, int32_t cause, bool valueWrite, uint32_t value)
	{
#ifdef FLOW_INFO
		cout << "TRAP " << (interrupt ? "interrupt" : "exception") << " cause=" << cause << " PC=0x" << hex << pc << " val=0x" << hex << value << dec << endl;
		if (cause == 9)
		{
			cout << hex << " a7=0x" << regs[17] << " a0=0x" << regs[10] << " a1=0x" << regs[11] << " a2=0x" << regs[12] << dec << endl;
		}
#endif
		//Check leguality of the interrupt
		if (interrupt)
		{
			bool hit = false;
			for (int i = 0; i < 5; i++)
			{
				if (pendingInterrupts[i] == 1 << cause)
				{
					hit = true;
					break;
				}
			}
			if (!hit)
			{
				cout << "DUT had trigger an interrupts which wasn't by the REF" << endl;
				fail();
			}
		}

		uint32_t deleg = interrupt ? mideleg : medeleg;
		uint32_t targetPrivilege = 3;
		if (deleg & (1 << cause))
			targetPrivilege = 1;
		targetPrivilege = max(targetPrivilege, privilege);
		Xtvec xtvec = targetPrivilege == 3 ? mtvec : stvec;

		switch (targetPrivilege)
		{
		case 3:
			if (valueWrite)
				mbadaddr = value;
			mcause.interrupt = interrupt;
			mcause.exceptionCode = cause;
			status.mpie = status.mie;
			status.mie = false;
			status.mpp = privilege;
			mepc = pc;
			break;
		case 1:
			if (valueWrite)
				sbadaddr = value;
			scause.interrupt = interrupt;
			scause.exceptionCode = cause;
			status.spie = status.sie;
			status.sie = false;
			status.spp = privilege;
			sepc = pc;
			break;
		}

		privilege = targetPrivilege;
		pcWrite(xtvec.base << 2);
		if (interrupt)
			livenessInterrupt = 0;

		//		if(!interrupt) step(); //As VexRiscv instruction which trap do not reach writeback stage fire
	}

	uint32_t currentInstruction;
	void ilegalInstruction()
	{
		trap(0, 2, currentInstruction);
	}

	virtual void fail()
	{
	}

	virtual bool csrRead(int32_t csr, uint32_t *value)
	{
		if (((csr >> 8) & 0x3) > privilege)
			return true;
		switch (csr)
		{
		case MSTATUS:
			*value = status.raw & MSTATUS_READ_MASK;
			break;
		case MIP:
			*value = getIp().raw;
			break;
		case MIE:
			*value = ie.raw;
			break;
		case MTVEC:
			*value = mtvec.raw;
			break;
		case MCAUSE:
			*value = mcause.raw;
			break;
		case MBADADDR:
			*value = mbadaddr;
			break;
		case MEPC:
			*value = mepc;
			break;
		case MSCRATCH:
			*value = mscratch;
			break;
		case MISA:
			*value = misa;
			break;
		case MEDELEG:
			*value = medeleg;
			break;
		case MIDELEG:
			*value = mideleg;
			break;
		case MHARTID:
			*value = 0;
			break;

		case SSTATUS:
			*value = status.raw & 0xC0133;
			break;
		case SIP:
			*value = getIp().raw & 0x333;
			break;
		case SIE:
			*value = ie.raw & 0x333;
			break;
		case STVEC:
			*value = stvec.raw;
			break;
		case SCAUSE:
			*value = scause.raw;
			break;
		case STVAL:
			*value = sbadaddr;
			break;
		case SEPC:
			*value = sepc;
			break;
		case SSCRATCH:
			*value = sscratch;
			break;
		case SATP:
			*value = satp.raw;
			break;
		default:
			return true;
			break;
		}
		return false;
	}

	virtual uint32_t csrReadToWriteOverride(int32_t csr, uint32_t value)
	{
		if (((csr >> 8) & 0x3) > privilege)
			return true;
		switch (csr)
		{
		case MIP:
			return ipSoft;
			break;
		case SIP:
			return ipSoft & 0x333;
			break;
		};
		return value;
	}

#define maskedWrite(dst, src, mask) dst = (dst & ~mask) | (src & mask);

	virtual bool csrWrite(int32_t csr, uint32_t value)
	{
		if (((csr >> 8) & 0x3) > privilege)
			return true;
		switch (csr)
		{
		case MSTATUS:
			status.raw = value;
			break;
		case MIP:
			ipSoft = value;
			break;
		case MIE:
			ie.raw = value;
			break;
		case MTVEC:
			mtvec.raw = value;
			break;
		case MCAUSE:
			mcause.raw = value;
			break;
		case MBADADDR:
			mbadaddr = value;
			break;
		case MEPC:
			mepc = value;
			break;
		case MSCRATCH:
			mscratch = value;
			break;
		case MISA:
			misa = value;
			break;
		case MEDELEG:
			medeleg = value & (~0x8);
			break;
		case MIDELEG:
			mideleg = value;
			break;

		case SSTATUS:
			maskedWrite(status.raw, value, 0xC0133);
			break;
		case SIP:
			maskedWrite(ipSoft, value, 0x333);
			break;
		case SIE:
			maskedWrite(ie.raw, value, 0x333);
			break;
		case STVEC:
			stvec.raw = value;
			break;
		case SCAUSE:
			scause.raw = value;
			break;
		case STVAL:
			sbadaddr = value;
			break;
		case SEPC:
			sepc = value;
			break;
		case SSCRATCH:
			sscratch = value;
			break;
		case SATP:
			satp.raw = value;
			break;

		default:
			ilegalInstruction();
			return true;
			break;
		}
		return false;
	}

	int livenessStep = 0;
	int livenessInterrupt = 0;
	uint32_t pendingInterruptsPtr = 0;
	uint32_t pendingInterrupts[5] = {0, 0, 0, 0, 0};
	virtual void liveness(bool inWfi)
	{
		uint32_t pendingInterrupt = getPendingInterrupt();
		pendingInterrupts[pendingInterruptsPtr++] = getPendingInterrupt();
		if (pendingInterruptsPtr >= 5)
			pendingInterruptsPtr = 0;
		if (pendingInterrupt)
			livenessInterrupt++;
		else
			livenessInterrupt = 0;
		if (!inWfi)
			livenessStep++;
		else
			livenessStep = 0;

		if (livenessStep > 10000)
		{
			cout << "Liveness step failure" << endl;
			fail();
		}

		if (livenessInterrupt > 1000)
		{
			cout << "Liveness interrupt failure" << endl;
			fail();
		}
	}

	uint32_t getPendingInterrupt()
	{
		uint32_t mEnabled = status.mie && privilege == 3 || privilege < 3;
		uint32_t sEnabled = status.sie && privilege == 1 || privilege < 1;

		uint32_t masked = getIp().raw & ~mideleg & -mEnabled & ie.raw;
		if (masked == 0)
			masked = getIp().raw & mideleg & -sEnabled & ie.raw & 0x333;

		if (masked)
		{
			if (masked & MIP_MEIP)
				masked &= MIP_MEIP;
			else if (masked & MIP_MSIP)
				masked &= MIP_MSIP;
			else if (masked & MIP_MTIP)
				masked &= MIP_MTIP;
			else if (masked & MIP_SEIP)
				masked &= MIP_SEIP;
			else if (masked & MIP_SSIP)
				masked &= MIP_SSIP;
			else if (masked & MIP_STIP)
				masked &= MIP_STIP;
			else
				fail();
		}

		return masked;
	}

	bool isPcAligned(uint32_t pc)
	{
#ifdef COMPRESSED
		return (pc & 1) == 0;
#else
		return (pc & 3) == 0;
#endif
	}

	virtual void step()
	{
		stepCounter++;
		livenessStep = 0;
#define rd32 ((i >> 7) & 0x1F)
#define iBits(lo, len) ((i >> lo) & ((1 << len) - 1))
#define iBitsSigned(lo, len) int32_t(i) << (32 - lo - len) >> (32 - len)
#define iSign() iBitsSigned(31, 1)
#define i32_rs1 regs[(i >> 15) & 0x1F]
#define i32_rs2 regs[(i >> 20) & 0x1F]
#define i32_i_imm (int32_t(i) >> 20)
#define i32_s_imm (iBits(7, 5) + (iBitsSigned(25, 7) << 5))
#define i32_shamt ((i >> 20) & 0x1F)
#define i32_sb_imm ((iBits(8, 4) << 1) + (iBits(25, 6) << 5) + (iBits(7, 1) << 11) + (iSign() << 12))
#define i32_csr iBits(20, 12)
#define i32_func3 iBits(12, 3)
#define i16_addi4spn_imm ((iBits(6, 1) << 2) + (iBits(5, 1) << 3) + (iBits(11, 2) << 4) + (iBits(7, 4) << 6))
#define i16_lw_imm ((iBits(6, 1) << 2) + (iBits(10, 3) << 3) + (iBits(5, 1) << 6))
#define i16_addr2 (iBits(2, 3) + 8)
#define i16_addr1 (iBits(7, 3) + 8)
#define i16_rf1 regs[i16_addr1]
#define i16_rf2 regs[i16_addr2]
#define rf_sp regs[2]
#define i16_imm (iBits(2, 5) + (iBitsSigned(12, 1) << 5))
#define i16_j_imm ((iBits(3, 3) << 1) + (iBits(11, 1) << 4) + (iBits(2, 1) << 5) + (iBits(7, 1) << 6) + (iBits(6, 1) << 7) + (iBits(9, 2) << 8) + (iBits(8, 1) << 10) + (iBitsSigned(12, 1) << 11))
#define i16_addi16sp_imm ((iBits(6, 1) << 4) + (iBits(2, 1) << 5) + (iBits(5, 1) << 6) + (iBits(3, 2) << 7) + (iBitsSigned(12, 1) << 9))
#define i16_zimm (iBits(2, 5))
#define i16_b_imm ((iBits(3, 2) << 1) + (iBits(10, 2) << 3) + (iBits(2, 1) << 5) + (iBits(5, 2) << 6) + (iBitsSigned(12, 1) << 8))
#define i16_lwsp_imm ((iBits(4, 3) << 2) + (iBits(12, 1) << 5) + (iBits(2, 2) << 6))
#define i16_swsp_imm ((iBits(9, 4) << 2) + (iBits(7, 2) << 6))
		uint32_t i;
		uint32_t u32Buf;
		uint32_t pAddr;
		if (pc & 2)
		{
			if (v2p(pc - 2, &pAddr, EXECUTE))
			{
				trap(0, 12, pc - 2);
				return;
			}
			if (iRead(pAddr, &i))
			{
				trap(0, 1, 0);
				return;
			}
			i >>= 16;
			if (i & 3 == 3)
			{
				uint32_t u32Buf;
				if (v2p(pc + 2, &pAddr, EXECUTE))
				{
					trap(0, 12, pc + 2);
					return;
				}
				if (iRead(pAddr, &u32Buf))
				{
					trap(0, 1, 0);
					return;
				}
				i |= u32Buf << 16;
			}
		}
		else
		{
			if (v2p(pc, &pAddr, EXECUTE))
			{
				trap(0, 12, pc);
				return;
			}
			if (iRead(pAddr, &i))
			{
				trap(0, 1, 0);
				return;
			}
		}
		lastInstruction = i;
		currentInstruction = i;
		if ((i & 0x3) == 0x3)
		{
			//32 bit
			switch (i & 0x7F)
			{
			case 0x37:
				rfWrite(rd32, i & 0xFFFFF000);
				pcWrite(pc + 4);
				break; // LUI
			case 0x17:
				rfWrite(rd32, (i & 0xFFFFF000) + pc);
				pcWrite(pc + 4);
				break; //AUIPC
			case 0x6F:
				rfWrite(rd32, pc + 4);
				pcWrite(pc + (iBits(21, 10) << 1) + (iBits(20, 1) << 11) + (iBits(12, 8) << 12) + (iSign() << 20));
				break; //JAL
			case 0x67:
			{
				uint32_t target = (i32_rs1 + i32_i_imm) & ~1;
				if (isPcAligned(target))
					rfWrite(rd32, pc + 4);
				pcWrite(target);
			}
			break; //JALR
			case 0x63:
				switch ((i >> 12) & 0x7)
				{
				case 0x0:
					if (i32_rs1 == i32_rs2)
						pcWrite(pc + i32_sb_imm);
					else
						pcWrite(pc + 4);
					break;
				case 0x1:
					if (i32_rs1 != i32_rs2)
						pcWrite(pc + i32_sb_imm);
					else
						pcWrite(pc + 4);
					break;
				case 0x4:
					if (i32_rs1 < i32_rs2)
						pcWrite(pc + i32_sb_imm);
					else
						pcWrite(pc + 4);
					break;
				case 0x5:
					if (i32_rs1 >= i32_rs2)
						pcWrite(pc + i32_sb_imm);
					else
						pcWrite(pc + 4);
					break;
				case 0x6:
					if (uint32_t(i32_rs1) < uint32_t(i32_rs2))
						pcWrite(pc + i32_sb_imm);
					else
						pcWrite(pc + 4);
					break;
				case 0x7:
					if (uint32_t(i32_rs1) >= uint32_t(i32_rs2))
						pcWrite(pc + i32_sb_imm);
					else
						pcWrite(pc + 4);
					break;
				}
				break;
			case 0x03:
			{ //LOADS
				uint32_t data;
				uint32_t address = i32_rs1 + i32_i_imm;
				uint32_t size = 1 << ((i >> 12) & 0x3);
				if (address & (size - 1))
				{
					trap(0, 4, address);
				}
				else
				{
					if (v2p(address, &pAddr, READ))
					{
						trap(0, 13, address);
						return;
					}
					if (dRead(pAddr, size, &data))
					{
						trap(0, 5, address);
					}
					else
					{
						switch ((i >> 12) & 0x7)
						{
						case 0x0:
							rfWrite(rd32, int8_t(data));
							pcWrite(pc + 4);
							break;
						case 0x1:
							rfWrite(rd32, int16_t(data));
							pcWrite(pc + 4);
							break;
						case 0x2:
							rfWrite(rd32, int32_t(data));
							pcWrite(pc + 4);
							break;
						case 0x4:
							rfWrite(rd32, uint8_t(data));
							pcWrite(pc + 4);
							break;
						case 0x5:
							rfWrite(rd32, uint16_t(data));
							pcWrite(pc + 4);
							break;
						}
					}
				}
			}
			break;
			case 0x23:
			{ //STORE
				uint32_t address = i32_rs1 + i32_s_imm;
				uint32_t size = 1 << ((i >> 12) & 0x3);
				if (address & (size - 1))
				{
					trap(0, 6, address);
				}
				else
				{
					if (v2p(address, &pAddr, WRITE))
					{
						trap(0, 15, address);
						return;
					}
					dWrite(pAddr, size, i32_rs2);
					pcWrite(pc + 4);
				}
			}
			break;
			case 0x13: //ALUi
				switch ((i >> 12) & 0x7)
				{
				case 0x0:
					rfWrite(rd32, i32_rs1 + i32_i_imm);
					pcWrite(pc + 4);
					break;
				case 0x1:
					switch ((i >> 25) & 0x7F)
					{
					case 0x00:
						rfWrite(rd32, i32_rs1 << i32_shamt);
						pcWrite(pc + 4);
						break;
					}
					break;
				case 0x2:
					rfWrite(rd32, i32_rs1 < i32_i_imm);
					pcWrite(pc + 4);
					break;
				case 0x3:
					rfWrite(rd32, uint32_t(i32_rs1) < uint32_t(i32_i_imm));
					pcWrite(pc + 4);
					break;
				case 0x4:
					rfWrite(rd32, i32_rs1 ^ i32_i_imm);
					pcWrite(pc + 4);
					break;
				case 0x5:
					switch ((i >> 25) & 0x7F)
					{
					case 0x00:
						rfWrite(rd32, uint32_t(i32_rs1) >> i32_shamt);
						pcWrite(pc + 4);
						break;
					case 0x20:
						rfWrite(rd32, i32_rs1 >> i32_shamt);
						pcWrite(pc + 4);
						break;
					}
					break;
				case 0x6:
					rfWrite(rd32, i32_rs1 | i32_i_imm);
					pcWrite(pc + 4);
					break;
				case 0x7:
					rfWrite(rd32, i32_rs1 & i32_i_imm);
					pcWrite(pc + 4);
					break;
				}
				break;
			case 0x33: //ALU
				if (((i >> 25) & 0x7F) == 0x01)
				{
					switch ((i >> 12) & 0x7)
					{
					case 0x0:
						rfWrite(rd32, int32_t(i32_rs1) * int32_t(i32_rs2));
						pcWrite(pc + 4);
						break;
					case 0x1:
						rfWrite(rd32, (int64_t(i32_rs1) * int64_t(i32_rs2)) >> 32);
						pcWrite(pc + 4);
						break;
					case 0x2:
						rfWrite(rd32, (int64_t(i32_rs1) * uint64_t(uint32_t(i32_rs2))) >> 32);
						pcWrite(pc + 4);
						break;
					case 0x3:
						rfWrite(rd32, (uint64_t(uint32_t(i32_rs1)) * uint64_t(uint32_t(i32_rs2))) >> 32);
						pcWrite(pc + 4);
						break;
					case 0x4:
						rfWrite(rd32, i32_rs2 == 0 ? -1 : int64_t(i32_rs1) / int64_t(i32_rs2));
						pcWrite(pc + 4);
						break;
					case 0x5:
						rfWrite(rd32, i32_rs2 == 0 ? -1 : uint32_t(i32_rs1) / uint32_t(i32_rs2));
						pcWrite(pc + 4);
						break;
					case 0x6:
						rfWrite(rd32, i32_rs2 == 0 ? i32_rs1 : int64_t(i32_rs1) % int64_t(i32_rs2));
						pcWrite(pc + 4);
						break;
					case 0x7:
						rfWrite(rd32, i32_rs2 == 0 ? i32_rs1 : uint32_t(i32_rs1) % uint32_t(i32_rs2));
						pcWrite(pc + 4);
						break;
					}
				}
				else
				{
					switch ((i >> 12) & 0x7)
					{
					case 0x0:
						switch ((i >> 25) & 0x7F)
						{
						case 0x00:
							rfWrite(rd32, i32_rs1 + i32_rs2);
							pcWrite(pc + 4);
							break;
						case 0x20:
							rfWrite(rd32, i32_rs1 - i32_rs2);
							pcWrite(pc + 4);
							break;
						}
						break;
					case 0x1:
						rfWrite(rd32, i32_rs1 << (i32_rs2 & 0x1F));
						pcWrite(pc + 4);
						break;
					case 0x2:
						rfWrite(rd32, i32_rs1 < i32_rs2);
						pcWrite(pc + 4);
						break;
					case 0x3:
						rfWrite(rd32, uint32_t(i32_rs1) < uint32_t(i32_rs2));
						pcWrite(pc + 4);
						break;
					case 0x4:
						rfWrite(rd32, i32_rs1 ^ i32_rs2);
						pcWrite(pc + 4);
						break;
					case 0x5:
						switch ((i >> 25) & 0x7F)
						{
						case 0x00:
							rfWrite(rd32, uint32_t(i32_rs1) >> (i32_rs2 & 0x1F));
							pcWrite(pc + 4);
							break;
						case 0x20:
							rfWrite(rd32, i32_rs1 >> (i32_rs2 & 0x1F));
							pcWrite(pc + 4);
							break;
						}
						break;
					case 0x6:
						rfWrite(rd32, i32_rs1 | i32_rs2);
						pcWrite(pc + 4);
						break;
					case 0x7:
						rfWrite(rd32, i32_rs1 & i32_rs2);
						pcWrite(pc + 4);
						break;
					}
				}
				break;
			case 0x73:
			{
				if (i32_func3 == 0)
				{
					switch (i)
					{
					case 0x30200073:
					{ //MRET
						if (privilege < 3)
						{
							ilegalInstruction();
							return;
						}
						privilege = status.mpp;
						status.mie = status.mpie;
						status.mpie = 1;
						status.mpp = 0;
						pcWrite(mepc);
					}
					break;
					case 0x10200073:
					{ //SRET
						if (privilege < 1)
						{
							ilegalInstruction();
							return;
						}
						privilege = status.spp;
						status.sie = status.spie;
						status.spie = 1;
						status.spp = 0;
						pcWrite(sepc);
					}
					break;
					case 0x00000073:
					{										//ECALL
						trap(0, 8 + privilege, 0x00000073); //To follow the VexRiscv area saving implementation
					}
					break;
					case 0x10500073:
					{ //WFI
						pcWrite(pc + 4);
					}
					break;
					default:
						if ((i & 0xFE007FFF) == 0x12000073)
						{ //SFENCE.VMA
							pcWrite(pc + 4);
						}
						else
						{
							ilegalInstruction();
						}
						break;
					}
				}
				else
				{
					//CSR
					uint32_t input = (i & 0x4000) ? ((i >> 15) & 0x1F) : i32_rs1;
					uint32_t clear, set;
					bool write;
					switch ((i >> 12) & 0x3)
					{
					case 1:
						clear = ~0;
						set = input;
						write = true;
						break;
					case 2:
						clear = 0;
						set = input;
						write = ((i >> 15) & 0x1F) != 0;
						break;
					case 3:
						clear = input;
						set = 0;
						write = ((i >> 15) & 0x1F) != 0;
						break;
					}
					uint32_t csrAddress = i32_csr;
					uint32_t old;
					if (csrRead(i32_csr, &old))
					{
						ilegalInstruction();
						return;
					}
					if (write)
						if (csrWrite(i32_csr, (csrReadToWriteOverride(i32_csr, old) & ~clear) | set))
						{
							ilegalInstruction();
							return;
						}
					rfWrite(rd32, old);
					pcWrite(pc + 4);
				}
				break;
			}
			case 0x2F: // Atomic stuff
				switch (i32_func3)
				{
				case 0x2:
					switch (iBits(27, 5))
					{
					case 0x2:
					{ //LR
						uint32_t data;
						uint32_t address = i32_rs1;
						if (address & 3)
						{
							trap(0, 4, address);
						}
						else
						{
							if (v2p(address, &pAddr, READ))
							{
								trap(0, 13, address);
								return;
							}
							if (dRead(pAddr, 4, &data))
							{
								trap(0, 5, address);
							}
							else
							{
								lrscReserved = true;
								lrscReservedAddress = pAddr;
								rfWrite(rd32, data);
								pcWrite(pc + 4);
							}
						}
					}
					break;
					case 0x3:
					{ //SC
						uint32_t address = i32_rs1;
						if (address & 3)
						{
							trap(0, 6, address);
						}
						else
						{
							if (v2p(address, &pAddr, WRITE))
							{
								trap(0, 15, address);
								return;
							}
#ifdef DBUS_EXCLUSIVE
							bool hit = lrscReserved && lrscReservedAddress == pAddr;
#else
							bool hit = lrscReserved;
#endif
							if (hit)
							{
								dWrite(pAddr, 4, i32_rs2);
							}
							lrscReserved = false;
							rfWrite(rd32, !hit);
							pcWrite(pc + 4);
						}
					}
					break;
					default:
					{
#ifndef AMO
						ilegalInstruction();
#else
						uint32_t sel = (i >> 27) & 0x1F;
						uint32_t addr = i32_rs1;
						int32_t src = i32_rs2;
						int32_t readValue;

#ifdef DBUS_EXCLUSIVE
						lrscReserved = false;
#endif

						uint32_t pAddr;
						if (v2p(addr, &pAddr, READ_WRITE))
						{
							trap(0, 15, addr);
							return;
						}
						if (dRead(pAddr, 4, (uint32_t *)&readValue))
						{
							trap(0, 15, addr);
							return;
							return;
						}
						int writeValue;
						switch (sel)
						{
						case 0x0:
							writeValue = src + readValue;
							break;
						case 0x1:
							writeValue = src;
							break;
						case 0x4:
							writeValue = src ^ readValue;
							break;
						case 0xC:
							writeValue = src & readValue;
							break;
						case 0x8:
							writeValue = src | readValue;
							break;
						case 0x10:
							writeValue = min(src, readValue);
							break;
						case 0x14:
							writeValue = max(src, readValue);
							break;
						case 0x18:
							writeValue = min((unsigned int)src, (unsigned int)readValue);
							break;
						case 0x1C:
							writeValue = max((unsigned int)src, (unsigned int)readValue);
							break;
						default:
							ilegalInstruction();
							return;
							break;
						}
						dWrite(pAddr, 4, writeValue);
						rfWrite(rd32, readValue);
						pcWrite(pc + 4);
#endif
					}
					break;
					}
					break;
				default:
					ilegalInstruction();
					break;
				}
				break;
			case 0x0f:
				if (i == 0x100F || (i & 0xF00FFFFF) == 0x000F)
				{ // FENCE FENCE.I
					pcWrite(pc + 4);
				}
				else
				{
					ilegalInstruction();
				}
				break;
			default:
				ilegalInstruction();
				break;
			}
		}
		else
		{
#ifndef COMPRESSED
			cout << "ERROR : RiscvGolden got a RVC instruction while the CPU isn't RVC ready" << endl;
			ilegalInstruction();
			return;
#endif
			switch ((iBits(0, 2) << 3) + iBits(13, 3))
			{
			case 0:
				rfWrite(i16_addr2, rf_sp + i16_addi4spn_imm);
				pcWrite(pc + 2);
				break;
			case 2:
			{
				uint32_t data;
				uint32_t address = i16_rf1 + i16_lw_imm;
				if (address & 0x3)
				{
					trap(0, 4, address);
				}
				else
				{
					if (v2p(address, &pAddr, READ))
					{
						trap(0, 13, address);
						return;
					}
					if (dRead(pAddr, 4, &data))
					{
						trap(0, 5, address);
					}
					else
					{
						rfWrite(i16_addr2, data);
						pcWrite(pc + 2);
					}
				}
			}
			break;
			case 6:
			{
				uint32_t address = i16_rf1 + i16_lw_imm;
				if (address & 0x3)
				{
					trap(0, 6, address);
				}
				else
				{
					if (v2p(address, &pAddr, WRITE))
					{
						trap(0, 15, address);
						return;
					}
					dWrite(pAddr, 4, i16_rf2);
					pcWrite(pc + 2);
				}
			}
			break;
			case 8:
				rfWrite(rd32, regs[rd32] + i16_imm);
				pcWrite(pc + 2);
				break;
			case 9:
				rfWrite(1, pc + 2);
				pcWrite(pc + i16_j_imm);
				break;
			case 10:
				rfWrite(rd32, i16_imm);
				pcWrite(pc + 2);
				break;
			case 11:
				if (rd32 == 2)
				{
					rfWrite(2, rf_sp + i16_addi16sp_imm);
					pcWrite(pc + 2);
				}
				else
				{
					rfWrite(rd32, i16_imm << 12);
					pcWrite(pc + 2);
				}
				break;
			case 12:
				switch (iBits(10, 2))
				{
				case 0:
					rfWrite(i16_addr1, uint32_t(i16_rf1) >> i16_zimm);
					pcWrite(pc + 2);
					break;
				case 1:
					rfWrite(i16_addr1, i16_rf1 >> i16_zimm);
					pcWrite(pc + 2);
					break;
				case 2:
					rfWrite(i16_addr1, i16_rf1 & i16_imm);
					pcWrite(pc + 2);
					break;
				case 3:
					switch (iBits(5, 2))
					{
					case 0:
						rfWrite(i16_addr1, i16_rf1 - i16_rf2);
						pcWrite(pc + 2);
						break;
					case 1:
						rfWrite(i16_addr1, i16_rf1 ^ i16_rf2);
						pcWrite(pc + 2);
						break;
					case 2:
						rfWrite(i16_addr1, i16_rf1 | i16_rf2);
						pcWrite(pc + 2);
						break;
					case 3:
						rfWrite(i16_addr1, i16_rf1 & i16_rf2);
						pcWrite(pc + 2);
						break;
					}
					break;
				}
				break;
			case 13:
				pcWrite(pc + i16_j_imm);
				break;
			case 14:
				pcWrite(i16_rf1 == 0 ? pc + i16_b_imm : pc + 2);
				break;
			case 15:
				pcWrite(i16_rf1 != 0 ? pc + i16_b_imm : pc + 2);
				break;
			case 16:
				rfWrite(rd32, regs[rd32] << i16_zimm);
				pcWrite(pc + 2);
				break;
			case 18:
			{
				uint32_t data;
				uint32_t address = rf_sp + i16_lwsp_imm;
				if (address & 0x3)
				{
					trap(0, 4, address);
				}
				else
				{
					if (v2p(address, &pAddr, READ))
					{
						trap(0, 13, address);
						return;
					}
					if (dRead(pAddr, 4, &data))
					{
						trap(0, 5, address);
					}
					else
					{
						rfWrite(rd32, data);
						pcWrite(pc + 2);
					}
				}
			}
			break;
			case 20:
				if (i & 0x1000)
				{
					if (iBits(2, 10) == 0)
					{
					}
					else if (iBits(2, 5) == 0)
					{
						rfWrite(1, pc + 2);
						pcWrite(regs[rd32] & ~1);
					}
					else
					{
						rfWrite(rd32, regs[rd32] + regs[iBits(2, 5)]);
						pcWrite(pc + 2);
					}
				}
				else
				{
					if (iBits(2, 5) == 0)
					{
						pcWrite(regs[rd32] & ~1);
					}
					else
					{
						rfWrite(rd32, regs[iBits(2, 5)]);
						pcWrite(pc + 2);
					}
				}
				break;
			case 22:
			{
				uint32_t address = rf_sp + i16_swsp_imm;
				if (address & 3)
				{
					trap(0, 6, address);
				}
				else
				{
					if (v2p(address, &pAddr, WRITE))
					{
						trap(0, 15, address);
						return;
					}
					dWrite(pAddr, 4, regs[iBits(2, 5)]);
					pcWrite(pc + 2);
				}
			}
			break;
			}
		}
	}
};

class SimElement
{
public:
	virtual ~SimElement() {}
	virtual void onReset() {}
	virtual void postReset() {}
	virtual void preCycle() {}
	virtual void postCycle() {}
};

class Workspace
{
public:
	static mutex staticMutex;
	static uint32_t testsCounter, successCounter;
	static uint64_t cycles;
	uint64_t instanceCycles = 0;
	vector<SimElement *> simElements;
	Memory mem;
	string name;
	uint64_t currentTime = 22;
	uint64_t mTimeCmp = 0;
	uint64_t mTime = 0;
	VVexRiscv *top;
	bool resetDone = false;
	bool riscvRefEnable = false;
	uint64_t i;
	double cyclesPerSecond = 10e6;
	double allowedCycles = 0.0;
	uint32_t bootPc = -1;
	uint32_t iStall = STALL, dStall = STALL;
#ifdef TRACE
	VerilatedVcdC *tfp;
#endif
	bool allowInvalidate = true;

	uint32_t seed;

	Workspace *setIStall(bool enable)
	{
		iStall = enable;
		return this;
	}
	Workspace *setDStall(bool enable)
	{
		dStall = enable;
		return this;
	}

	ofstream regTraces;
	ofstream memTraces;
	//ofstream logTraces;
	//ofstream debugLog;

	struct timespec start_time;

	class CpuRef : public RiscvGolden
	{
	public:
		Memory mem;

		class MemWrite
		{
		public:
			int32_t address, size;
			uint32_t data;
		};

		class MemRead
		{
		public:
			int32_t address, size;
			uint32_t data;
			bool error;
		};

		uint32_t periphWriteTimer = 0;
		queue<MemWrite> periphWritesGolden;
		queue<MemWrite> periphWrites;
		queue<MemRead> periphRead;
		Workspace *ws;
		CpuRef(Workspace *ws)
		{
			this->ws = ws;
		}

		virtual void fail() { ws->fail(); }

		virtual bool isMmuRegion(uint32_t v) { return ws->isMmuRegion(v); }

		bool rfWriteValid;
		int32_t rfWriteAddress;
		int32_t rfWriteData;
		virtual void rfWrite(int32_t address, int32_t data)
		{
			rfWriteValid = address != 0;
			rfWriteAddress = address;
			rfWriteData = data;
			RiscvGolden::rfWrite(address, data);
		}

		virtual bool iRead(int32_t address, uint32_t *data)
		{
			bool error;
			ws->iBusAccess(address, data, &error);
			//    		ws->iBusAccessPatch(address,data,&error);
			return error;
		}

		virtual bool dRead(int32_t address, int32_t size, uint32_t *data)
		{
			if (size < 1 || size > 4)
			{
				cout << "dRead size=" << size << endl;
				fail();
			}
			if (address & (size - 1) != 0)
				cout << "Ref did a unaligned read" << endl;
			if (ws->isPerifRegion(address))
			{
				MemRead t = periphRead.front();
				if (t.address != address || t.size != size)
				{
					cout << "DRead missmatch" << hex << endl;
					cout << " REF : address=" << address << " size=" << size << endl;
					cout << " DUT : address=" << t.address << " size=" << t.size << endl;
					fail();
				}
				*data = t.data;
				periphRead.pop();
				return t.error;
			}
			else
			{
				mem.read(address, size, (uint8_t *)data);
			}
			return false;
		}
		virtual void dWrite(int32_t address, int32_t size, uint32_t data)
		{
			if (address & (size - 1) != 0)
				cout << "Ref did a unaligned write" << endl;

			if (!ws->isPerifRegion(address))
			{
				mem.write(address, size, (uint8_t *)&data);
			}
			if (ws->isDBusCheckedRegion(address))
			{
				MemWrite w;
				w.address = address;
				w.size = size;
				switch (size)
				{
				case 1:
					w.data = data & 0xFF;
					break;
				case 2:
					w.data = data & 0xFFFF;
					break;
				case 4:
					w.data = data;
					break;
				}
				periphWritesGolden.push(w);
				if (periphWritesGolden.size() > 10)
				{
					cout << "??? periphWritesGolden" << endl;
					fail();
				}
			}
		}

		void step()
		{
			rfWriteValid = false;
			RiscvGolden::step();

			switch (periphWrites.empty() + uint32_t(periphWritesGolden.empty()) * 2)
			{
			case 3:
				periphWriteTimer = 0;
				break;
			case 1:
			case 2:
				if (periphWriteTimer++ == 20)
				{
					cout << "periphWrite timout" << endl;
					fail();
				}
				break;
			case 0:
				MemWrite t = periphWrites.front();
				MemWrite t2 = periphWritesGolden.front();
				if (t.address != t2.address || t.size != t2.size || t.data != t2.data)
				{
					cout << hex << "periphWrite missmatch" << endl;
					cout << " DUT address=" << t.address << " size=" << t.size << " data=" << t.data << endl;
					cout << " REF address=" << t2.address << " size=" << t2.size << " data=" << t2.data << endl;
					fail();
				}
				periphWrites.pop();
				periphWritesGolden.pop();
				periphWriteTimer = 0;
				break;
			}
		}
	};

	string vcdName;
	Workspace *setVcdName(string name)
	{
		vcdName = name;
		return this;
	}
	Workspace(string name)
	{
		vcdName = name;
		//seed = VL_RANDOM_I(32)^VL_RANDOM_I(32)^0x1093472;
		//srand48(seed);
		//    setIStall(false);
		//     setDStall(false);
		staticMutex.lock();
		testsCounter++;
		staticMutex.unlock();
		this->name = name;
		top = new VVexRiscv;
#ifdef TRACE_ACCESS
		regTraces.open(name + ".regTrace");
		memTraces.open(name + ".memTrace");
#endif
		//logTraces.open(name + ".logTrace");
		//debugLog.open(name + ".debugTrace");
		fillSimELements();
		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start_time);
	}

	virtual ~Workspace()
	{
		delete top;
#ifdef TRACE
		delete tfp;
#endif

		for (SimElement *simElement : simElements)
		{
			delete simElement;
		}
	}

	Workspace *loadHex(string path)
	{
		loadHexImpl(path, &mem);
		return this;
	}

	Workspace *loadBin(string path, uint32_t offset)
	{
		loadBinImpl(path, &mem, offset);
		return this;
	}

	Workspace *setCyclesPerSecond(double value)
	{
		cyclesPerSecond = value;
		return this;
	}

	Workspace *bootAt(uint32_t pc)
	{
		bootPc = pc;
		return this;
	}

	Workspace *withInvalidation()
	{
		allowInvalidate = true;
		return this;
	}
	Workspace *withoutInvalidation()
	{
		allowInvalidate = false;
		return this;
	}
	virtual bool isPerifRegion(uint32_t addr) { return false; }
	virtual bool isMmuRegion(uint32_t addr) { return true; }
	virtual void iBusAccess(uint32_t addr, uint32_t *data, bool *error)
	{
		if (addr % 4 != 0)
		{
			cout << "Warning, unaligned IBusAccess : " << addr << endl;
			fail();
		}
		*data = ((mem[addr + 0] << 0) | (mem[addr + 1] << 8) | (mem[addr + 2] << 16) | (mem[addr + 3] << 24));
		*error = false;
	}

	virtual bool isDBusCheckedRegion(uint32_t address) { return isPerifRegion(address); }
	virtual void dBusAccess(uint32_t addr, bool wr, uint32_t size, uint32_t mask, uint32_t *data, bool *error)
	{
		assertEq(addr % (1 << size), 0);
		if (!isPerifRegion(addr))
		{
			if (wr)
			{
				memTraces <<
#ifdef TRACE_WITH_TIME
					(currentTime
#ifdef REF
					 - 2
#endif
					 ) <<
#endif
					" : WRITE mem" << hex << (1 << size) << "[" << addr << "] = " << *data << dec << endl;
				for (uint32_t b = 0; b < (1 << size); b++)
				{
					uint32_t offset = (addr + b) & 0x3;
					if ((mask >> offset) & 1 == 1)
						*mem.get(addr + b) = *data >> (offset * 8);
				}
			}
			else
			{
				*data = VL_RANDOM_I(32);
				for (uint32_t b = 0; b < (1 << size); b++)
				{
					uint32_t offset = (addr + b) & 0x3;
					*data &= ~(0xFF << (offset * 8));
					*data |= mem[addr + b] << (offset * 8);
				}
				/*
				memTraces <<
				#ifdef TRACE_WITH_TIME
				(currentTime
				#ifdef REF
				-2
				 #endif
				 ) <<
				 #endif
				  " : READ  mem" << (1 << size) << "[" << addr << "] = " << *data << endl;*/
			}
		}

		if (wr)
		{
			if (isDBusCheckedRegion(addr))
			{
				CpuRef::MemWrite w;
				w.address = addr;
				while ((mask & 1) == 0)
				{
					mask >>= 1;
					w.address++;
					w.data >>= 8;
				}
				switch (mask)
				{
				case 1:
					size = 0;
					break;
				case 3:
					size = min(1u, size);
					break;
				case 15:
					size = min(2u, size);
					break;
				}
				w.size = 1 << size;
				switch (size)
				{
				case 0:
					w.data = *data & 0xFF;
					break;
				case 1:
					w.data = *data & 0xFFFF;
					break;
				case 2:
					w.data = *data;
					break;
				}
			}
		}
		else
		{
			if (isPerifRegion(addr))
			{
				CpuRef::MemRead r;
				r.address = addr;
				r.size = 1 << size;
				r.data = *data;
				r.error = *error;
			}
		}
	}

	virtual void postReset() {}
	virtual void checks() {}
	virtual void pass() { throw success(); }
	virtual void fail()
	{
		throw std::exception();
	}
	virtual void fillSimELements();
	void dump(uint64_t i)
	{
#ifdef TRACE
		if (i == TRACE_START && i != 0)
			cout << "**" << endl
				 << "**" << endl
				 << "**" << endl
				 << "**" << endl
				 << "**" << endl
				 << "START TRACE" << endl;
		if (i >= TRACE_START)
			tfp->dump(i);
#ifdef TRACE_SPORADIC
		else if (i % 1000000 < 100)
			tfp->dump(i);
#endif
#endif
	}

	uint64_t privilegeCounters[4] = {0, 0, 0, 0};
	Workspace *run(uint64_t timeout = 5000)
	{
		//		cout << "Start " << name << endl;
		if (timeout == 0)
			timeout = 0x7FFFFFFFFFFFFFFF;

		currentTime = 4;
// init trace dump
#ifdef TRACE
		Verilated::traceEverOn(true);
		tfp = new VerilatedVcdC;
		top->trace(tfp, 99);
		tfp->open((vcdName + ".vcd").c_str());
#endif

		// Reset
		top->clk = 0;
		top->reset = 0;

		top->eval();
		currentTime = 3;
		for (SimElement *simElement : simElements)
			simElement->onReset();

		top->reset = 1;
		top->eval();
		top->clk = 1;
		top->eval();
		top->clk = 0;
		top->eval();
		dump(0);
		top->reset = 0;
		for (SimElement *simElement : simElements)
			simElement->postReset();

		top->eval();
		currentTime = 2;

		postReset();

		resetDone = true;

#ifdef REF
		if (bootPc != -1)
			top->VexRiscv->core->prefetch_pc = bootPc;
#else
		if (bootPc != -1)
		{
#if defined(IBUS_SIMPLE) || defined(IBUS_SIMPLE_WISHBONE) || defined(IBUS_SIMPLE_AHBLITE3)
			top->VexRiscv->IBusSimplePlugin_fetchPc_pcReg = bootPc;
#ifdef COMPRESSED
			top->VexRiscv->IBusSimplePlugin_decodePc_pcReg = bootPc;
#endif
#else
			top->VexRiscv->IBusCachedPlugin_fetchPc_pcReg = bootPc;
#ifdef COMPRESSED
			top->VexRiscv->IBusCachedPlugin_decodePc_pcReg = bootPc;
#endif
#endif
		}
#endif

		bool failed = false;

		try
		{
			// run simulation for 100 clock periods
			for (i = 16; i < timeout * 2; i += 2)
			{
				/*while(allowedCycles <= 0.0){
					struct timespec end_time;
					clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end_time);
					uint64_t diffInNanos = end_time.tv_sec*1e9 + end_time.tv_nsec -  start_time.tv_sec*1e9 - start_time.tv_nsec;
					start_time = end_time;
					double dt = diffInNanos*1e-9;
					allowedCycles += dt*cyclesPerSecond;
					if(allowedCycles > cyclesPerSecond/100) allowedCycles = cyclesPerSecond/100;
				}
				allowedCycles-=1.0;*/

#ifndef REF_TIME
#ifndef MTIME_INSTR_FACTOR
				mTime = i / 2;
#else
				mTime += top->VexRiscv->lastStageIsFiring * MTIME_INSTR_FACTOR;
#endif
#endif
#ifdef TIMER_INTERRUPT
				top->timerInterrupt = mTime >= mTimeCmp ? 1 : 0;
//if(mTime == mTimeCmp) printf("SIM timer tick\n");
#endif

				currentTime = i;

				//printf("PC: %08X %d\n", top->VexRiscv->lastStagePc, top->VexRiscv->lastStageIsFiring);

				// Print trace for entering a function
				if (profileMode != 3)
				{
					if (top->VexRiscv->CsrPlugin_mscratch > 1)
					{
						if (profileMode == 1)
							printf("E:%08X:%lu\n", top->VexRiscv->CsrPlugin_mscratch, instanceCycles);
						functionStack.push(top->VexRiscv->CsrPlugin_mscratch);
						if (top->VexRiscv->CsrPlugin_mscratch == debuggedFunction)
							functionOccurred += 1;
						top->VexRiscv->CsrPlugin_mscratch = 0;
					}
					// Print trace for leaving a function
					else if (top->VexRiscv->CsrPlugin_mscratch == 1)
					{
						uint32_t currentFunc = functionStack.top();
						if (profileMode == 1)
							printf("L:%08X:%lu\n", currentFunc, instanceCycles);
						if (functionStack.top() == debuggedFunction && functionOccurred == desiredFunctionCalls)
							pass();
						functionStack.pop();
						top->VexRiscv->CsrPlugin_mscratch = 0;
					}

					if (profileMode == 2 && !functionStack.empty() && functionStack.top() == debuggedFunction)
					{
						if (currentPCCounter != 0 && top->VexRiscv->lastStagePc != lastPc)
						{
							printf("%08X:%d\n", lastPc, currentPCCounter);
							currentPCCounter = 1;
						}
						else if (top->VexRiscv->lastStagePc == lastPc || top->VexRiscv->lastStagePc == 0)
							currentPCCounter += 1;
						else
							currentPCCounter = 1;
						lastPc = top->VexRiscv->lastStagePc;
					}
				}
				// Detect jump
				else if (profileMode == 3 && top->VexRiscv->lastStageIsFiring)
				{
					// Determine current function
					int index = -1;
					for (int x = 0; x < numberOfLabels; ++x)
						if (top->VexRiscv->lastStagePc >= registeredLabels[x])
						{
							index = x;
							break;
						}

					// Function entered
					if (index != -1 && top->VexRiscv->lastStagePc == registeredLabels[index] && (functionStack.empty() || functionStack.top() != index))
					{
						printf("E:%08X:%lu\n", registeredLabels[index], instanceCycles);
						functionStack.push(index);
					}
					// Function left
					else if (index != -1 && !functionStack.empty() && functionStack.top() != index)
					{
						printf("L:%08X:%lu\n", registeredLabels[functionStack.top()], instanceCycles);
						functionStack.pop();
						while (!functionStack.empty() && index != functionStack.top())
						{
							// For multiple returns at once rewind the stack until we are back on course
							printf("Popping stack\n");
							printf("L:%08X:%lu\n", registeredLabels[functionStack.top()], instanceCycles);
							functionStack.pop();
						}
					}
				}

				lastPc = top->VexRiscv->lastStagePc;

#ifdef FLOW_INFO
				if (i % 2000000 == 0)
					cout
						<< "**" << endl
						<< "**" << endl
						<< "**" << endl
						<< "**" << endl
						<< "**" << endl
						<< "PROGRESS TRACE_START=" << i << endl;
#endif

				// dump variables into VCD file and toggle clock

				dump(i);
				//top->eval();
				top->clk = 0;
				top->eval();

				if (top->VexRiscv->lastStageIsFiring)
				{
					bool rfWriteValid = false;
					int32_t rfWriteAddress;
					int32_t rfWriteData;

					if (top->VexRiscv->lastStageRegFileWrite_valid == 1 && top->VexRiscv->lastStageRegFileWrite_payload_address != 0)
					{
						rfWriteValid = true;
						rfWriteAddress = top->VexRiscv->lastStageRegFileWrite_payload_address;
						rfWriteData = top->VexRiscv->lastStageRegFileWrite_payload_data;
#ifdef TRACE_ACCESS
						regTraces <<
#ifdef TRACE_WITH_TIME
							currentTime <<
#endif
							" PC " << hex << setw(8) << top->VexRiscv->lastStagePc << " : reg[" << dec << setw(2) << (uint32_t)top->VexRiscv->lastStageRegFileWrite_payload_address << "] = " << hex << setw(8) << top->VexRiscv->lastStageRegFileWrite_payload_data << dec << endl;
#endif
					}
					else
					{
#ifdef TRACE_ACCESS
						regTraces <<
#ifdef TRACE_WITH_TIME
							currentTime <<
#endif
							" PC " << hex << setw(8) << top->VexRiscv->lastStagePc << dec << endl;
#endif
					}
				}

				for (SimElement *simElement : simElements)
					simElement->preCycle();

				dump(i + 1);

				checks();
				//top->eval();
				top->clk = 1;
				top->eval();

				instanceCycles += 1;

				for (SimElement *simElement : simElements)
					simElement->postCycle();

				if (Verilated::gotFinish())
					exit(0);
			}
			cout << "timeout" << endl;
			fail();
		}
		catch (const success e)
		{
			staticMutex.lock();
			successCounter++;
			cycles += instanceCycles;
			staticMutex.unlock();
		}
		catch (const std::exception &e)
		{
			staticMutex.lock();
			cycles += instanceCycles;
			staticMutex.unlock();
			failed = true;
		}

		dump(i + 2);
		dump(i + 10);
#ifdef TRACE
		tfp->close();
#endif
#ifdef STOP_ON_ERROR
		if (failed)
		{
			sleep(1);
			exit(-1);
		}
#endif
		return this;
	}
};

class WorkspaceRegression : public Workspace
{
public:
	WorkspaceRegression(string name) : Workspace(name)
	{
	}

	virtual bool isPerifRegion(uint32_t addr) { return (addr & 0xF0000000) == 0xF0000000; }

	virtual void iBusAccess(uint32_t addr, uint32_t *data, bool *error)
	{
		Workspace::iBusAccess(addr, data, error);
		*error = addr == 0xF00FFF60u;
	}

	virtual void dutPutChar(char c) {}

	virtual void dBusAccess(uint32_t addr, bool wr, uint32_t size, uint32_t mask, uint32_t *data, bool *error)
	{
		if (wr)
		{
			switch (addr)
			{
			case 0xF0010000u:
			{
				cout << (char)*data;
				//logTraces << (char)*data;
				dutPutChar((char)*data);
				break;
			}
#ifdef EXTERNAL_INTERRUPT
			case 0xF0011000u:
				top->externalInterrupt = *data & 1;
				break;
#endif
#ifdef SUPERVISOR
			case 0xF0012000u:
				top->externalInterruptS = *data & 1;
				break;
#endif
#ifdef CSR
			case 0xF0013000u:
				top->softwareInterrupt = *data & 1;
				break;
#endif
			case 0xF00FFF00u:
			{
				cout << (char)*data;
				//logTraces << (char)*data;
				dutPutChar((char)*data);
				break;
			}
#ifndef DEBUG_PLUGIN_EXTERNAL
			case 0xF00FFF20u:
				if (*data == 0)
					pass();
				else
					fail();
				break;
			case 0xF00FFF24u:
				cout << "TEST ERROR CODE " << *data << endl;
				fail();
				break;
#endif
			case 0xF00FFF48u:
				mTimeCmp = (mTimeCmp & 0xFFFFFFFF00000000) | *data;
				break;
			case 0xF00FFF4Cu:
				mTimeCmp = (mTimeCmp & 0x00000000FFFFFFFF) | (((uint64_t)*data) << 32);
				break;
			case 0xF00FFF50u:
				cout << "mTime " << *data << " : " << mTime << endl;
			}
			if ((addr & 0xFFFFF000) == 0xF5670000)
			{
				uint32_t t = 0x900FF000 | (addr & 0xFFF);
				uint32_t old = (*mem.get(t + 3) << 24) | (*mem.get(t + 2) << 16) | (*mem.get(t + 1) << 8) | (*mem.get(t + 0) << 0);
				old++;
				*mem.get(t + 0) = old & 0xFF;
				old >>= 8;
				*mem.get(t + 1) = old & 0xFF;
				old >>= 8;
				*mem.get(t + 2) = old & 0xFF;
				old >>= 8;
				*mem.get(t + 3) = old & 0xFF;
				old >>= 8;
			}
		}
		else
		{
			switch (addr)
			{
			case 0xF00FFF10u:
				*data = mTime;
#ifdef REF_TIME
				mTime += 100000;
#endif
				break;
			case 0xF00FFF40u:
				*data = mTime;
				break;
			case 0xF00FFF44u:
				*data = mTime >> 32;
				break;
			case 0xF00FFF48u:
				*data = mTimeCmp;
				break;
			case 0xF00FFF4Cu:
				*data = mTimeCmp >> 32;
				break;
			case 0xF0010004u:
				*data = ~0;
				break;
			}
			memTraces <<
#ifdef TRACE_WITH_TIME
				(currentTime
#ifdef REF
				 - 2
#endif
				 ) <<
#endif
				" : READ  mem" << (1 << size) << "[" << addr << "] = " << *data << endl;
		}

		*error = addr == 0xF00FFF60u;
		Workspace::dBusAccess(addr, wr, size, mask, data, error);
	}
};

#ifdef IBUS_SIMPLE
class IBusSimple : public SimElement
{
public:
	uint32_t pendings[256];
	uint32_t rPtr = 0, wPtr = 0;

	Workspace *ws;
	VVexRiscv *top;
	IBusSimple(Workspace *ws)
	{
		this->ws = ws;
		this->top = ws->top;
	}

	virtual void onReset()
	{
		top->iBus_cmd_ready = 1;
		top->iBus_rsp_valid = 0;
	}

	virtual void preCycle()
	{
		if (top->iBus_cmd_valid && top->iBus_cmd_ready)
		{
			//assertEq(top->iBus_cmd_payload_pc & 3,0);
			pendings[wPtr] = (top->iBus_cmd_payload_pc);
			wPtr = (wPtr + 1) & 0xFF;
			//ws->iBusAccess(top->iBus_cmd_payload_pc,&inst_next,&error_next);
		}
	}
	//TODO doesn't catch when instruction removed ?
	virtual void postCycle()
	{
		top->iBus_rsp_valid = 0;
		if (rPtr != wPtr && (!ws->iStall || VL_RANDOM_I(7) < 100))
		{
			uint32_t inst_next;
			bool error_next;
			ws->iBusAccess(pendings[rPtr], &inst_next, &error_next);
			rPtr = (rPtr + 1) & 0xFF;
			top->iBus_rsp_payload_inst = inst_next;
			top->iBus_rsp_valid = 1;
			top->iBus_rsp_payload_error = error_next;
		}
		else
		{
			top->iBus_rsp_payload_inst = VL_RANDOM_I(32);
			top->iBus_rsp_payload_error = VL_RANDOM_I(1);
		}
		if (ws->iStall)
			top->iBus_cmd_ready = VL_RANDOM_I(7) < 100;
	}
};
#endif

#ifdef IBUS_TC

class IBusTc : public SimElement
{
public:
	uint32_t nextData;

	Workspace *ws;
	VVexRiscv *top;
	IBusTc(Workspace *ws)
	{
		this->ws = ws;
		this->top = ws->top;
	}

	virtual void onReset()
	{
	}

	virtual void preCycle()
	{
		if (top->iBusTc_enable)
		{
			if ((top->iBusTc_address & 0x70000000) != 0)
			{
				printf("IBusTc access out of range\n");
				ws->fail();
			}
			bool error_next;
			ws->iBusAccess(top->iBusTc_address, &nextData, &error_next);
		}
	}

	virtual void postCycle()
	{
		top->iBusTc_data = nextData;
	}
};

#endif

#ifdef IBUS_SIMPLE_AVALON

struct IBusSimpleAvalonRsp
{
	uint32_t data;
	bool error;
};

class IBusSimpleAvalon : public SimElement
{
public:
	queue<IBusSimpleAvalonRsp> rsps;

	Workspace *ws;
	VVexRiscv *top;
	IBusSimpleAvalon(Workspace *ws)
	{
		this->ws = ws;
		this->top = ws->top;
	}

	virtual void onReset()
	{
		top->iBusAvalon_waitRequestn = 1;
		top->iBusAvalon_readDataValid = 0;
	}

	virtual void preCycle()
	{
		if (top->iBusAvalon_read && top->iBusAvalon_waitRequestn)
		{
			IBusSimpleAvalonRsp rsp;
			ws->iBusAccess(top->iBusAvalon_address, &rsp.data, &rsp.error);
			rsps.push(rsp);
		}
	}
	//TODO doesn't catch when instruction removed ?
	virtual void postCycle()
	{
		if (!rsps.empty() && (!ws->iStall || VL_RANDOM_I(7) < 100))
		{
			IBusSimpleAvalonRsp rsp = rsps.front();
			rsps.pop();
			top->iBusAvalon_readDataValid = 1;
			top->iBusAvalon_readData = rsp.data;
			top->iBusAvalon_response = rsp.error ? 3 : 0;
		}
		else
		{
			top->iBusAvalon_readDataValid = 0;
			top->iBusAvalon_readData = VL_RANDOM_I(32);
			top->iBusAvalon_response = VL_RANDOM_I(2);
		}
		if (ws->iStall)
			top->iBusAvalon_waitRequestn = VL_RANDOM_I(7) < 100;
	}
};
#endif

#ifdef IBUS_SIMPLE_AHBLITE3
class IBusSimpleAhbLite3 : public SimElement
{
public:
	Workspace *ws;
	VVexRiscv *top;

	uint32_t iBusAhbLite3_HRDATA;
	bool iBusAhbLite3_HRESP;
	bool pending;

	IBusSimpleAhbLite3(Workspace *ws)
	{
		this->ws = ws;
		this->top = ws->top;
	}

	virtual void onReset()
	{
		pending = false;
		top->iBusAhbLite3_HREADY = 1;
		top->iBusAhbLite3_HRESP = 0;
	}

	virtual void preCycle()
	{
		if (top->iBusAhbLite3_HTRANS == 2 && top->iBusAhbLite3_HREADY && !top->iBusAhbLite3_HWRITE)
		{
			ws->iBusAccess(top->iBusAhbLite3_HADDR, &iBusAhbLite3_HRDATA, &iBusAhbLite3_HRESP);
			pending = true;
		}
	}

	virtual void postCycle()
	{
		if (ws->iStall)
			top->iBusAhbLite3_HREADY = (!ws->iStall || VL_RANDOM_I(7) < 100);

		if (pending && top->iBusAhbLite3_HREADY)
		{
			top->iBusAhbLite3_HRDATA = iBusAhbLite3_HRDATA;
			top->iBusAhbLite3_HRESP = iBusAhbLite3_HRESP;
			pending = false;
		}
		else
		{
			top->iBusAhbLite3_HRDATA = VL_RANDOM_I(32);
			top->iBusAhbLite3_HRESP = VL_RANDOM_I(1);
		}
	}
};
#endif

#ifdef IBUS_CACHED
class IBusCached : public SimElement
{
public:
	bool error_next = false;
	uint32_t pendingCount = 0;
	uint32_t address;

	Workspace *ws;
	VVexRiscv *top;
	IBusCached(Workspace *ws)
	{
		this->ws = ws;
		this->top = ws->top;
	}

	virtual void onReset()
	{
		top->iBus_cmd_ready = 1;
		top->iBus_rsp_valid = 0;
	}

	virtual void preCycle()
	{
		if (top->iBus_cmd_valid && top->iBus_cmd_ready && pendingCount == 0)
		{
			assertEq(top->iBus_cmd_payload_address & 3, 0);
			pendingCount = (1 << top->iBus_cmd_payload_size) / 4;
			address = top->iBus_cmd_payload_address;
		}
	}

	virtual void postCycle()
	{
		bool error;
		top->iBus_rsp_valid = 0;
		if (pendingCount != 0 && (!ws->iStall || VL_RANDOM_I(7) < 100))
		{
#ifdef IBUS_TC
			if ((address & 0x70000000) == 0)
			{
				printf("IBUS_CACHED access out of range\n");
				ws->fail();
			}
#endif
			error = false;
			for (int idx = 0; idx < IBUS_DATA_WIDTH / 32; idx++)
			{
				bool localError = false;
				ws->iBusAccess(address + idx * 4, ((uint32_t *)&top->iBus_rsp_payload_data) + idx, &localError);
				error |= localError;
			}
			top->iBus_rsp_payload_error = error;
			pendingCount -= IBUS_DATA_WIDTH / 32;
			address = address + IBUS_DATA_WIDTH / 8;
			top->iBus_rsp_valid = 1;
		}
		if (ws->iStall)
			top->iBus_cmd_ready = VL_RANDOM_I(7) < 100 && pendingCount == 0;
	}
};
#endif

#ifdef IBUS_CACHED_AVALON
#include <queue>

struct IBusCachedAvalonTask
{
	uint32_t address;
	uint32_t pendingCount;
};

class IBusCachedAvalon : public SimElement
{
public:
	uint32_t inst_next = VL_RANDOM_I(32);
	bool error_next = false;

	queue<IBusCachedAvalonTask> tasks;
	Workspace *ws;
	VVexRiscv *top;

	IBusCachedAvalon(Workspace *ws)
	{
		this->ws = ws;
		this->top = ws->top;
	}

	virtual void onReset()
	{
		top->iBusAvalon_waitRequestn = 1;
		top->iBusAvalon_readDataValid = 0;
	}

	virtual void preCycle()
	{
		if (top->iBusAvalon_read && top->iBusAvalon_waitRequestn)
		{
			assertEq(top->iBusAvalon_address & 3, 0);
			IBusCachedAvalonTask task;
			task.address = top->iBusAvalon_address;
			task.pendingCount = top->iBusAvalon_burstCount;
			tasks.push(task);
		}
	}

	virtual void postCycle()
	{
		bool error;
		top->iBusAvalon_readDataValid = 0;
		if (!tasks.empty() && (!ws->iStall || VL_RANDOM_I(7) < 100))
		{
			uint32_t &address = tasks.front().address;
			uint32_t &pendingCount = tasks.front().pendingCount;
			bool error;
			ws->iBusAccess(address, &top->iBusAvalon_readData, &error);
			top->iBusAvalon_response = error ? 3 : 0;
			pendingCount--;
			address = (address & ~0x1F) + ((address + 4) & 0x1F);
			top->iBusAvalon_readDataValid = 1;
			if (pendingCount == 0)
				tasks.pop();
		}
		if (ws->iStall)
			top->iBusAvalon_waitRequestn = VL_RANDOM_I(7) < 100;
	}
};
#endif

#if defined(IBUS_CACHED_WISHBONE) || defined(IBUS_SIMPLE_WISHBONE)
#include <queue>

class IBusCachedWishbone : public SimElement
{
public:
	Workspace *ws;
	VVexRiscv *top;

	IBusCachedWishbone(Workspace *ws)
	{
		this->ws = ws;
		this->top = ws->top;
	}

	virtual void onReset()
	{
		top->iBusWishbone_ACK = !ws->iStall;
		top->iBusWishbone_ERR = 0;
	}

	virtual void preCycle()
	{
	}

	virtual void postCycle()
	{

		if (ws->iStall)
			top->iBusWishbone_ACK = VL_RANDOM_I(7) < 100;

		top->iBusWishbone_DAT_MISO = VL_RANDOM_I(32);
		if (top->iBusWishbone_CYC && top->iBusWishbone_STB && top->iBusWishbone_ACK)
		{
			if (top->iBusWishbone_WE)
			{
			}
			else
			{
				bool error;
				ws->iBusAccess(top->iBusWishbone_ADR << 2, &top->iBusWishbone_DAT_MISO, &error);
				top->iBusWishbone_ERR = error;
			}
		}
	}
};
#endif

#ifdef DBUS_SIMPLE
class DBusSimple : public SimElement
{
public:
	uint32_t data_next = VL_RANDOM_I(32);
	bool error_next = false;
	bool pending = false;

	Workspace *ws;
	VVexRiscv *top;
	DBusSimple(Workspace *ws)
	{
		this->ws = ws;
		this->top = ws->top;
	}

	virtual void onReset()
	{
		top->dBus_cmd_ready = 1;
		top->dBus_rsp_ready = 1;
	}

	virtual void preCycle()
	{
		if (top->dBus_cmd_valid && top->dBus_cmd_ready)
		{
			pending = true;
			data_next = top->dBus_cmd_payload_data;
			ws->dBusAccess(top->dBus_cmd_payload_address, top->dBus_cmd_payload_wr, top->dBus_cmd_payload_size, 0xF, &data_next, &error_next);
		}
	}

	virtual void postCycle()
	{
		top->dBus_rsp_ready = 0;
		if (pending && (!ws->dStall || VL_RANDOM_I(7) < 100))
		{
			pending = false;
			top->dBus_rsp_ready = 1;
			top->dBus_rsp_data = data_next;
			top->dBus_rsp_error = error_next;
		}
		else
		{
			top->dBus_rsp_data = VL_RANDOM_I(32);
		}

		if (ws->dStall)
			top->dBus_cmd_ready = VL_RANDOM_I(7) < 100 && !pending;
	}
};
#endif

#ifdef DBUS_SIMPLE_AVALON
#include <queue>
struct DBusSimpleAvalonRsp
{
	uint32_t data;
	bool error;
};

class DBusSimpleAvalon : public SimElement
{
public:
	queue<DBusSimpleAvalonRsp> rsps;

	Workspace *ws;
	VVexRiscv *top;
	DBusSimpleAvalon(Workspace *ws)
	{
		this->ws = ws;
		this->top = ws->top;
	}

	virtual void onReset()
	{
		top->dBusAvalon_waitRequestn = 1;
		top->dBusAvalon_readDataValid = 0;
	}

	virtual void preCycle()
	{
		if (top->dBusAvalon_write && top->dBusAvalon_waitRequestn)
		{
			bool dummy;
			ws->dBusAccess(top->dBusAvalon_address, 1, 2, top->dBusAvalon_byteEnable, &top->dBusAvalon_writeData, &dummy);
		}
		if (top->dBusAvalon_read && top->dBusAvalon_waitRequestn)
		{
			DBusSimpleAvalonRsp rsp;
			ws->dBusAccess(top->dBusAvalon_address, 0, 2, 0xF, &rsp.data, &rsp.error);
			rsps.push(rsp);
		}
	}
	//TODO doesn't catch when instruction removed ?
	virtual void postCycle()
	{
		if (!rsps.empty() && (!ws->iStall || VL_RANDOM_I(7) < 100))
		{
			DBusSimpleAvalonRsp rsp = rsps.front();
			rsps.pop();
			top->dBusAvalon_readDataValid = 1;
			top->dBusAvalon_readData = rsp.data;
			top->dBusAvalon_response = rsp.error ? 3 : 0;
		}
		else
		{
			top->dBusAvalon_readDataValid = 0;
			top->dBusAvalon_readData = VL_RANDOM_I(32);
			top->dBusAvalon_response = VL_RANDOM_I(2);
		}
		if (ws->iStall)
			top->dBusAvalon_waitRequestn = VL_RANDOM_I(7) < 100;
	}
};
#endif

#ifdef DBUS_SIMPLE_AHBLITE3
class DBusSimpleAhbLite3 : public SimElement
{
public:
	Workspace *ws;
	VVexRiscv *top;

	uint32_t dBusAhbLite3_HADDR, dBusAhbLite3_HSIZE, dBusAhbLite3_HTRANS, dBusAhbLite3_HWRITE;

	DBusSimpleAhbLite3(Workspace *ws)
	{
		this->ws = ws;
		this->top = ws->top;
	}

	virtual void onReset()
	{
		top->dBusAhbLite3_HREADY = 1;
		top->dBusAhbLite3_HRESP = 0;
		dBusAhbLite3_HTRANS = 0;
	}

	virtual void preCycle()
	{
		if (top->dBusAhbLite3_HREADY && dBusAhbLite3_HTRANS == 2 && dBusAhbLite3_HWRITE)
		{
			uint32_t data = top->dBusAhbLite3_HWDATA;
			bool error;
			ws->dBusAccess(dBusAhbLite3_HADDR, 1, dBusAhbLite3_HSIZE, ((1 << (1 << dBusAhbLite3_HSIZE)) - 1) << (dBusAhbLite3_HADDR & 0x3), &data, &error);
		}

		if (top->dBusAhbLite3_HREADY)
		{
			dBusAhbLite3_HADDR = top->dBusAhbLite3_HADDR;
			dBusAhbLite3_HSIZE = top->dBusAhbLite3_HSIZE;
			dBusAhbLite3_HTRANS = top->dBusAhbLite3_HTRANS;
			dBusAhbLite3_HWRITE = top->dBusAhbLite3_HWRITE;
		}
	}

	virtual void postCycle()
	{
		if (ws->iStall)
			top->dBusAhbLite3_HREADY = (!ws->iStall || VL_RANDOM_I(7) < 100);

		top->dBusAhbLite3_HRDATA = VL_RANDOM_I(32);
		top->dBusAhbLite3_HRESP = VL_RANDOM_I(1);

		if (top->dBusAhbLite3_HREADY && dBusAhbLite3_HTRANS == 2 && !dBusAhbLite3_HWRITE)
		{

			bool error;
			ws->dBusAccess(dBusAhbLite3_HADDR, 0, dBusAhbLite3_HSIZE, ((1 << (1 << dBusAhbLite3_HSIZE)) - 1) << (dBusAhbLite3_HADDR & 0x3), &top->dBusAhbLite3_HRDATA, &error);
			top->dBusAhbLite3_HRESP = error;
		}
	}
};
#endif

#if defined(DBUS_CACHED_WISHBONE) || defined(DBUS_SIMPLE_WISHBONE)
#include <queue>

class DBusCachedWishbone : public SimElement
{
public:
	Workspace *ws;
	VVexRiscv *top;

	DBusCachedWishbone(Workspace *ws)
	{
		this->ws = ws;
		this->top = ws->top;
	}

	virtual void onReset()
	{
		top->dBusWishbone_ACK = !ws->iStall;
		top->dBusWishbone_ERR = 0;
	}

	virtual void preCycle()
	{
	}

	virtual void postCycle()
	{
		if (ws->iStall)
			top->dBusWishbone_ACK = VL_RANDOM_I(7) < 100;
		top->dBusWishbone_DAT_MISO = VL_RANDOM_I(32);
		if (top->dBusWishbone_CYC && top->dBusWishbone_STB && top->dBusWishbone_ACK)
		{
			if (top->dBusWishbone_WE)
			{
				bool dummy;
				ws->dBusAccess(top->dBusWishbone_ADR << 2, 1, 2, top->dBusWishbone_SEL, &top->dBusWishbone_DAT_MOSI, &dummy);
			}
			else
			{
				bool error;
				ws->dBusAccess(top->dBusWishbone_ADR << 2, 0, 2, 0xF, &top->dBusWishbone_DAT_MISO, &error);
				top->dBusWishbone_ERR = error;
			}
		}
	}
};
#endif

#ifdef DBUS_CACHED

//#include "VVexRiscv_DataCache.h"
#include <queue>

struct DBusCachedTask
{
	char data[DBUS_DATA_WIDTH / 8];
	bool error;
	bool last;
	bool exclusive;
};

class DBusCached : public SimElement
{
public:
	queue<DBusCachedTask> rsps;
	queue<uint32_t> invalidationHint;

	bool reservationValid = false;
	uint32_t reservationAddress;
	uint32_t pendingSync = 0;

	Workspace *ws;
	VVexRiscv *top;
	DBusCachedTask rsp;

	DBusCached(Workspace *ws)
	{
		this->ws = ws;
		this->top = ws->top;
	}

	virtual void onReset()
	{
		top->dBus_cmd_ready = 1;
		top->dBus_rsp_valid = 0;
#ifdef DBUS_INVALIDATE
		top->dBus_inv_valid = 0;
		top->dBus_ack_ready = 0;
		top->dBus_sync_valid = 0;
#endif
	}

	virtual void preCycle()
	{
		if (top->dBus_cmd_valid && top->dBus_cmd_ready)
		{
			if (top->dBus_cmd_payload_wr)
			{
#ifdef DBUS_INVALIDATE
				pendingSync += 1;
#endif
#ifndef DBUS_EXCLUSIVE
				bool error;
				ws->dBusAccess(top->dBus_cmd_payload_address, 1, 2, top->dBus_cmd_payload_mask, &top->dBus_cmd_payload_data, &error);
#else
				bool cancel = false, error = false;
				if (top->dBus_cmd_payload_exclusive)
				{
					bool hit = reservationValid && reservationAddress == top->dBus_cmd_payload_address;
					rsp.exclusive = hit;
					cancel = !hit;
					reservationValid = false;
				}
				if (!cancel)
				{
					for (int idx = 0; idx < 1; idx++)
					{
						bool localError = false;
						ws->dBusAccess(top->dBus_cmd_payload_address + idx * 4, 1, 2, top->dBus_cmd_payload_mask >> idx * 4, ((uint32_t *)&top->dBus_cmd_payload_data) + idx, &localError);
						error |= localError;

						//printf("%d ", (int)localError);
					}
				}

				// printf("%x %d\n", top->dBus_cmd_payload_address, (int)error);
				rsp.last = true;
				rsp.error = error;
				rsps.push(rsp);
#endif
			}
			else
			{
				bool error = false;
				uint32_t beatCount = top->dBus_cmd_payload_length * 32 / DBUS_DATA_WIDTH;
				for (int beat = 0; beat <= beatCount; beat++)
				{
					if (top->dBus_cmd_payload_length == 0)
					{
						uint32_t sel = (top->dBus_cmd_payload_address >> 2) & (DBUS_DATA_WIDTH / 32 - 1);
						ws->dBusAccess(top->dBus_cmd_payload_address, 0, 2, 0, ((uint32_t *)rsp.data) + sel, &error);
					}
					else
					{
						for (int idx = 0; idx < DBUS_DATA_WIDTH / 32; idx++)
						{
							bool localError = false;
							ws->dBusAccess(top->dBus_cmd_payload_address + beat * DBUS_DATA_WIDTH / 8 + idx * 4, 0, 2, 0, ((uint32_t *)rsp.data) + idx, &localError);
							error |= localError;
						}
					}
					rsp.last = beat == beatCount;
#ifdef DBUS_EXCLUSIVE
					if (top->dBus_cmd_payload_exclusive)
					{
						rsp.exclusive = true;
						reservationValid = true;
						reservationAddress = top->dBus_cmd_payload_address;
					}
#endif
					rsp.error = error;
					rsps.push(rsp);
				}

#ifdef DBUS_INVALIDATE
				if (ws->allowInvalidate)
				{
					if (VL_RANDOM_I(7) < 10)
					{
						invalidationHint.push(top->dBus_cmd_payload_address + VL_RANDOM_I(5));
					}
				}
#endif
			}
		}
#ifdef DBUS_INVALIDATE
		if (top->dBus_sync_valid && top->dBus_sync_ready)
		{
			pendingSync -= 1;
		}
#endif
	}

	virtual void postCycle()
	{

		if (!rsps.empty() && (!ws->dStall || VL_RANDOM_I(7) < 100))
		{
			DBusCachedTask rsp = rsps.front();
			rsps.pop();
			top->dBus_rsp_valid = 1;
			top->dBus_rsp_payload_error = rsp.error;
			for (int idx = 0; idx < DBUS_DATA_WIDTH / 32; idx++)
			{
				((uint32_t *)&top->dBus_rsp_payload_data)[idx] = ((uint32_t *)rsp.data)[idx];
			}
			top->dBus_rsp_payload_last = rsp.last;
#ifdef DBUS_EXCLUSIVE
			top->dBus_rsp_payload_exclusive = rsp.exclusive;
#endif
		}
		else
		{
			top->dBus_rsp_valid = 0;
			for (int idx = 0; idx < DBUS_DATA_WIDTH / 32; idx++)
			{
				((uint32_t *)&top->dBus_rsp_payload_data)[idx] = VL_RANDOM_I(32);
			}
			top->dBus_rsp_payload_error = VL_RANDOM_I(1);
			top->dBus_rsp_payload_last = VL_RANDOM_I(1);
#ifdef DBUS_EXCLUSIVE
			top->dBus_rsp_payload_exclusive = VL_RANDOM_I(1);
#endif
		}
		top->dBus_cmd_ready = (ws->dStall ? VL_RANDOM_I(7) < 100 : 1);

#ifdef DBUS_INVALIDATE
		if (ws->allowInvalidate)
		{
			if (top->dBus_inv_ready)
				top->dBus_inv_valid = 0;
			if (top->dBus_inv_valid == 0 && VL_RANDOM_I(7) < 5)
			{
				top->dBus_inv_valid = 1;
				top->dBus_inv_payload_fragment_enable = VL_RANDOM_I(7) < 100;
				if (!invalidationHint.empty())
				{
					top->dBus_inv_payload_fragment_address = invalidationHint.front();
					invalidationHint.pop();
				}
				else
				{
					top->dBus_inv_payload_fragment_address = VL_RANDOM_I(32);
				}
			}
		}
		top->dBus_ack_ready = (ws->dStall ? VL_RANDOM_I(7) < 100 : 1);
		if (top->dBus_sync_ready)
			top->dBus_sync_valid = 0;
		if (top->dBus_sync_valid == 0 && pendingSync != 0 && (ws->dStall ? VL_RANDOM_I(7) < 80 : 1))
		{
			top->dBus_sync_valid = 1;
		}
#endif
	}
};
#endif

#ifdef DBUS_CACHED_AVALON
#include <queue>

struct DBusCachedAvalonTask
{
	uint32_t data;
	bool error;
};

class DBusCachedAvalon : public SimElement
{
public:
	uint32_t beatCounter = 0;
	queue<DBusCachedAvalonTask> rsps;

	Workspace *ws;
	VVexRiscv *top;
	DBusCachedAvalon(Workspace *ws)
	{
		this->ws = ws;
		this->top = ws->top;
	}

	virtual void onReset()
	{
		top->dBusAvalon_waitRequestn = 1;
		top->dBusAvalon_readDataValid = 0;
	}

	virtual void preCycle()
	{
		if ((top->dBusAvalon_read || top->dBusAvalon_write) && top->dBusAvalon_waitRequestn)
		{
			if (top->dBusAvalon_write)
			{
				bool error_next = false;
				ws->dBusAccess(top->dBusAvalon_address + beatCounter * 4, 1, 2, top->dBusAvalon_byteEnable, &top->dBusAvalon_writeData, &error_next);
				beatCounter++;
				if (beatCounter == top->dBusAvalon_burstCount)
				{
					beatCounter = 0;
				}
			}
			else
			{
				for (int beat = 0; beat < top->dBusAvalon_burstCount; beat++)
				{
					DBusCachedAvalonTask rsp;
					ws->dBusAccess(top->dBusAvalon_address + beat * 4, 0, 2, 0, &rsp.data, &rsp.error);
					rsps.push(rsp);
				}
			}
		}
	}

	virtual void postCycle()
	{
		if (!rsps.empty() && (!ws->dStall || VL_RANDOM_I(7) < 100))
		{
			DBusCachedAvalonTask rsp = rsps.front();
			rsps.pop();
			top->dBusAvalon_response = rsp.error ? 3 : 0;
			top->dBusAvalon_readData = rsp.data;
			top->dBusAvalon_readDataValid = 1;
		}
		else
		{
			top->dBusAvalon_readDataValid = 0;
			top->dBusAvalon_readData = VL_RANDOM_I(32);
			top->dBusAvalon_response = VL_RANDOM_I(2); //TODO
		}

		top->dBusAvalon_waitRequestn = (ws->dStall ? VL_RANDOM_I(7) < 100 : 1);
	}
};
#endif

void Workspace::fillSimELements()
{
#ifdef IBUS_SIMPLE
	simElements.push_back(new IBusSimple(this));
#endif
#ifdef IBUS_SIMPLE_AVALON
	simElements.push_back(new IBusSimpleAvalon(this));
#endif
#ifdef IBUS_SIMPLE_AHBLITE3
	simElements.push_back(new IBusSimpleAhbLite3(this));
#endif

#ifdef IBUS_CACHED
	simElements.push_back(new IBusCached(this));
#endif
#ifdef IBUS_CACHED_AVALON
	simElements.push_back(new IBusCachedAvalon(this));
#endif
#if defined(IBUS_CACHED_WISHBONE) || defined(IBUS_SIMPLE_WISHBONE)
	simElements.push_back(new IBusCachedWishbone(this));
#endif

#ifdef IBUS_TC
	simElements.push_back(new IBusTc(this));
#endif

#ifdef DBUS_SIMPLE
	simElements.push_back(new DBusSimple(this));
#endif
#ifdef DBUS_SIMPLE_AVALON
	simElements.push_back(new DBusSimpleAvalon(this));
#endif
#ifdef DBUS_SIMPLE_AHBLITE3
	simElements.push_back(new DBusSimpleAhbLite3(this));
#endif
#ifdef DBUS_CACHED
	simElements.push_back(new DBusCached(this));
#endif
#ifdef DBUS_CACHED_AVALON
	simElements.push_back(new DBusCachedAvalon(this));
#endif
#if defined(DBUS_CACHED_WISHBONE) || defined(DBUS_SIMPLE_WISHBONE)
	simElements.push_back(new DBusCachedWishbone(this));
#endif
#ifdef DEBUG_PLUGIN_STD
	simElements.push_back(new DebugPluginStd(this));
#endif
#ifdef DEBUG_PLUGIN_AVALON
	simElements.push_back(new DebugPluginAvalon(this));
#endif
}

mutex Workspace::staticMutex;
uint64_t Workspace::cycles = 0;
uint32_t Workspace::testsCounter = 0, Workspace::successCounter = 0;

#include <unistd.h>
#include <termios.h>
#include <fcntl.h>
termios stdinRestoreSettings;
void stdinNonBuffered()
{
	static struct termios old, new1;
	tcgetattr(STDIN_FILENO, &old); // grab old terminal i/o settings
	new1 = old;					   // make new settings same as old settings
	new1.c_lflag &= ~ICANON;	   // disable buffered i/o
	new1.c_lflag &= ~ECHO;
	tcsetattr(STDIN_FILENO, TCSANOW, &new1); // use these new terminal i/o settings now
	setvbuf(stdin, NULL, _IONBF, 0);
	stdinRestoreSettings = old;
}

bool stdinNonEmpty()
{
	struct timeval tv;
	fd_set fds;
	tv.tv_sec = 0;
	tv.tv_usec = 0;
	FD_ZERO(&fds);
	FD_SET(STDIN_FILENO, &fds);
	select(STDIN_FILENO + 1, &fds, NULL, NULL, &tv);
	return (FD_ISSET(0, &fds));
}

void stdoutNonBuffered()
{
	setvbuf(stdout, NULL, _IONBF, 0);
}

void stdinRestore()
{
	tcsetattr(STDIN_FILENO, TCSANOW, &stdinRestoreSettings);
}

void my_handler(int s)
{
	printf("Caught signal %d\n", s);
	stdinRestore();
	exit(1);
}
#include <signal.h>

void captureCtrlC()
{
	struct sigaction sigIntHandler;

	sigIntHandler.sa_handler = my_handler;
	sigemptyset(&sigIntHandler.sa_mask);
	sigIntHandler.sa_flags = 0;

	sigaction(SIGINT, &sigIntHandler, NULL);
}

struct timespec timer_start()
{
	struct timespec start_time;
	clock_gettime(CLOCK_REALTIME, &start_time); //CLOCK_PROCESS_CPUTIME_ID
	return start_time;
}

long timer_end(struct timespec start_time)
{
	struct timespec end_time;
	clock_gettime(CLOCK_REALTIME, &end_time);
	uint64_t diffInNanos = end_time.tv_sec * 1e9 + end_time.tv_nsec - start_time.tv_sec * 1e9 - start_time.tv_nsec;
	return diffInNanos;
}

int main(int argc, char **argv, char **env)
{
#ifdef SEED
	srand48(SEED);
#endif
	Verilated::randReset(2);
	//Verilated::commandArgs(argc, argv);

	timespec startedAt = timer_start();

	if (argc > 3)
		profileMode = atoi(argv[3]);

	if (profileMode == 2 && argc > 5)
	{
		debuggedFunction = strtol(argv[4], NULL, 16);
		desiredFunctionCalls = atoi(argv[5]);
	}

	if (profileMode == 3 && argc > 4)
	{
		registeredLabels = new uint32_t[argc - 4];
		numberOfLabels = argc - 4;
		for (int x = 0; x < argc - 4; ++x)
		{
			registeredLabels[x] = strtol(argv[x + 4], NULL, 16);
			printf("Label[%02d] = %08X\n", x, registeredLabels[x]);
			if (x > 0 && registeredLabels[x] > registeredLabels[x - 1])
			{
				printf("Labels are not sorted correctly\n");
				exit(11);
			}
		}
		printf("%d labels read\n", argc - 4);

		// Printing synthetic wrapper function
		printf("E:FFFFFFFF:0\n");
	}

	WorkspaceRegression(argv[1])
		.loadHex(argv[1])
		->bootAt((uint32_t)strtol(argv[2], NULL, 16))
		->run(50e10);

	uint64_t duration = timer_end(startedAt);

	// Append the last leaving call
	if (profileMode == 3)
	{
		if (!functionStack.empty())
		{
			printf("Finish stack with index %d, length left %lu\n", functionStack.top(), functionStack.size());
			printf("L:%08X:%lu\n", registeredLabels[functionStack.top()], Workspace::cycles);
		}
		delete[] registeredLabels;

		// Leaving synthetic wrapper function
		printf("L:FFFFFFFF:%lu\n", Workspace::cycles);
	}
	if (Workspace::successCounter == Workspace::testsCounter)
		printf("SUCCESS, %lu clock cycles in %.2f s (%f Khz)\n", Workspace::cycles, duration * 1e-9, Workspace::cycles / (duration * 1e-6));
	else
		printf("FAILURE, %lu clock cycles in %.2f s (%f Khz)\n", Workspace::cycles, duration * 1e-9, Workspace::cycles / (duration * 1e-6));

	exit(0);
}
