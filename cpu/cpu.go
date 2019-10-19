package cpu

import "github.com/welldias/gones/bus"

// Instruction is the opcode translation table.
type Instruction struct {
	OperType   OperateType
	AddrMdType AddrModeType
	Cycles     uint8
	Operate    func() uint8
	Addrmode   func() uint8
}

// CPU is The 6502 CPU Core registers
type CPU struct {
	bus            bus.BUS
	a              uint8  // Accumulator Register
	x              uint8  // X Register
	y              uint8  // Y Register
	stackPtr       uint8  // Stack Pointer (points to location on bus)
	programCounter uint16 // Program Counter
	status         uint8  // Status Register
	instructions   [256]Instruction
}

var (
	fetched    uint8  // Represents the working input value to the ALU
	temp       uint16 // A convenience variable used everywhere
	addrAbs    uint16 // All used memory addresses end up in here
	addrRel    uint16 // Represents absolute address following a branch
	opcode     uint8  // Is the instruction byte
	cycles     uint8  // Counts how many cycles the instruction has remaining
	clockCount uint32 // A global accumulation of the number of clocks
)

// Reset Interrupt - Forces CPU into known state
func (cpu CPU) reset() {

	// Get address to set program counter to
	addrAbs = 0xfffc

	var lo uint16 = uint16(cpu.Read(addrAbs + 0))
	var hi uint16 = uint16(cpu.Read(addrAbs + 1))

	// Set it
	cpu.programCounter = (hi << 8) | lo

	// Reset internal registers
	cpu.a = 0x00
	cpu.x = 0x00
	cpu.y = 0x00
	cpu.stackPtr = 0xfd
	cpu.status = uint8(0x00 | FlagU)

	// Clear internal helper variables
	addrRel = 0x0000
	addrAbs = 0x0000
	fetched = 0x00

	// Reset takes time
	cycles = 8
}

// Interrupt Request - Executes an instruction at a specific location
func (cpu CPU) irq() {

	// If interrupts are allowed
	if cpu.GetFlag(FlagI) == 0 {
		var tempVal16 uint16
		var tempVal8 uint8

		// Push the program counter to the stack. It's 16-bits dont
		// forget so that takes two pushes
		tempVal16 = 0x0100 + uint16(cpu.stackPtr)
		tempVal8 = uint8(cpu.programCounter>>8) & 0x00ff
		cpu.Write(tempVal16, tempVal8)
		cpu.stackPtr--

		tempVal16 = 0x0100 + uint16(cpu.stackPtr)
		tempVal8 = uint8(cpu.programCounter) & 0x00ff
		cpu.Write(tempVal16, tempVal8)
		cpu.stackPtr--

		// Then Push the status register to the stack
		cpu.SetFlag(FlagB, false)
		cpu.SetFlag(FlagU, true)
		cpu.SetFlag(FlagI, true)

		tempVal16 = 0x0100 + uint16(cpu.stackPtr)
		cpu.Write(tempVal16, cpu.status)
		cpu.stackPtr--

		// Read new program counter location from fixed address
		addrAbs = 0xfffe
		var lo uint16 = uint16(cpu.Read(addrAbs + 0))
		var hi uint16 = uint16(cpu.Read(addrAbs + 1))
		cpu.programCounter = (hi << 8) | lo

		// IRQs take time
		cycles = 7
	}
}

// Non-Maskable Interrupt Request - As above, but cannot be disabled
func (cpu CPU) nmi() {
	var tempVal16 uint16
	var tempVal8 uint8

	tempVal16 = 0x0100 + uint16(cpu.stackPtr)
	tempVal8 = uint8(cpu.programCounter>>8) & 0x00ff
	cpu.Write(tempVal16, tempVal8)
	cpu.stackPtr--

	tempVal16 = 0x0100 + uint16(cpu.stackPtr)
	tempVal8 = uint8(cpu.programCounter) & 0x00ff
	cpu.Write(tempVal16, tempVal8)
	cpu.stackPtr--

	cpu.SetFlag(FlagB, false)
	cpu.SetFlag(FlagU, true)
	cpu.SetFlag(FlagI, true)

	tempVal16 = 0x0100 + uint16(cpu.stackPtr)
	cpu.Write(tempVal16, cpu.status)
	cpu.stackPtr--

	addrAbs = 0xfffa
	var lo uint16 = uint16(cpu.Read(addrAbs + 0))
	var hi uint16 = uint16(cpu.Read(addrAbs + 1))
	cpu.programCounter = (hi << 8) | lo

	cycles = 8
}

// Perform one clock cycle's worth of update
func (cpu CPU) clock() {

	// Each instruction requires a variable number of clock cycles to execute.
	// In my emulation, I only care about the final result and so I perform
	// the entire computation in one hit. In hardware, each clock cycle would
	// perform "microcode" style transformations of the CPUs state.
	//
	// To remain compliant with connected devices, it's important that the
	// emulation also takes "time" in order to execute Instructions, so I
	// implement that delay by simply counting down the cycles required by
	// the instruction. When it reaches 0, the instruction is complete, and
	// the next one is ready to be executed.
	if cycles == 0 {
		// Read next instruction byte. This 8-bit value is used to index
		// the translation table to get the relevant information about
		// how to implement the instruction
		opcode = cpu.Read(cpu.programCounter)

		// Always set the unused status flag bit to 1
		cpu.SetFlag(FlagU, true)

		// Increment program counter, we read the opcode byte
		cpu.programCounter++

		// Get Starting number of cycles
		cycles = cpu.instructions[opcode].Cycles

		// Perform fetch of intermmediate data using the
		// required addressing mode
		var additionalCycle1 uint8 = cpu.instructions[opcode].Addrmode()

		// Perform operation
		var additionalCycle2 uint8 = cpu.instructions[opcode].Operate()

		// The addressmode and opcode may have altered the number
		// of cycles this instruction requires before its completed
		cycles += (additionalCycle1 & additionalCycle2)

		// Always set the unused status flag bit to 1
		cpu.SetFlag(FlagU, true)
	}

	// Increment global clock count - This is actually unused unless logging is enabled
	// but I've kept it in because its a handy watch variable for debugging
	clockCount++

	// Decrement the number of cycles remaining for this instruction
	cycles--
}

// GetFlag returns the value of a specific bit of the status register
func (cpu CPU) GetFlag(flag Flag) uint8 {
	if (cpu.status & uint8(flag)) > 0 {
		return 1
	}
	return 0
}

// SetFlag sets or clears a specific bit of the status register
func (cpu CPU) SetFlag(flag Flag, v bool) {
	if v {
		cpu.status |= uint8(flag)
	} else {
		cpu.status &= uint8(^flag)
	}
}

// Imp is Address Mode: Implied
// There is no additional data required for this instruction. The instruction
// does something very simple like like sets a status bit. However, we will
// target the accumulator, for Instructions like PHA
func (cpu CPU) Imp() uint8 {
	fetched = cpu.a
	return 0
}

// Imm is Address Mode: Immediate
// The instruction expects the next byte to be used as a value, so we'll prep
// the read address to point to the next byte
func (cpu CPU) Imm() uint8 {
	cpu.programCounter++
	addrAbs = cpu.programCounter
	return 0
}

// Zp0 is Address Mode: Zero Page
// To save program bytes, zero page addressing allows you to absolutely address
// a location in first 0xFF bytes of address range. Clearly this only requires
// one byte instead of the usual two.
func (cpu CPU) Zp0() uint8 {
	addrAbs = uint16(cpu.Read(cpu.programCounter))
	cpu.programCounter++
	addrAbs &= 0x00ff
	return 0
}

// Zpx is Address Mode: Zero Page with X Offset
// Fundamentally the same as Zero Page addressing, but the contents of the X Register
// is added to the supplied single byte address. This is useful for iterating through
// ranges within the first page.
func (cpu CPU) Zpx() uint8 {
	addrAbs = uint16((cpu.Read(cpu.programCounter) + cpu.x))
	cpu.programCounter++
	addrAbs &= 0x00ff
	return 0
}

// Zpy is Address Mode: Zero Page with Y Offset
// Same as above but uses Y Register for offset
func (cpu CPU) Zpy() uint8 {
	addrAbs = uint16((cpu.Read(cpu.programCounter) + cpu.y))
	cpu.programCounter++
	addrAbs &= 0x00ff
	return 0
}

// Rel is Address Mode: Relative
// This address mode is exclusive to branch Instructions. The address
// must reside within -128 to +127 of the branch instruction, i.e.
// you cant directly branch to any address in the addressable range.
func (cpu CPU) Rel() uint8 {
	addrRel = uint16(cpu.Read(cpu.programCounter))
	cpu.programCounter++
	if (addrRel & 0x80) != 0 {
		addrRel |= 0xff00
	}
	return 0
}

// Abs Address Mode: Absolute
// A full 16-bit address is loaded and used
func (cpu CPU) Abs() uint8 {
	var lo uint16 = uint16(cpu.Read(cpu.programCounter))
	cpu.programCounter++
	var hi uint16 = uint16(cpu.Read(cpu.programCounter))
	cpu.programCounter++

	addrAbs = (hi << 8) | lo

	return 0
}

// Abx is Address Mode: Absolute with X Offset
// Fundamentally the same as absolute addressing, but the contents of the X Register
// is added to the supplied two byte address. If the resulting address changes
// the page, an additional clock cycle is required
func (cpu CPU) Abx() uint8 {
	var lo uint16 = uint16(cpu.Read(cpu.programCounter))
	cpu.programCounter++
	var hi uint16 = uint16(cpu.Read(cpu.programCounter))
	cpu.programCounter++

	addrAbs = (hi << 8) | lo
	addrAbs += uint16(cpu.x)

	if (addrAbs & 0xff00) != (hi << 8) {
		return 1
	}
	return 0
}

// Aby is Address Mode: Absolute with Y Offset
// Fundamentally the same as absolute addressing, but the contents of the Y Register
// is added to the supplied two byte address. If the resulting address changes
// the page, an additional clock cycle is required
func (cpu CPU) Aby() uint8 {
	var lo uint16 = uint16(cpu.Read(cpu.programCounter))
	cpu.programCounter++
	var hi uint16 = uint16(cpu.Read(cpu.programCounter))
	cpu.programCounter++

	addrAbs = (hi << 8) | lo
	addrAbs += uint16(cpu.y)

	if (addrAbs & 0xff00) != (hi << 8) {
		return 1
	}
	return 0
}

// Ind is Address Mode: Indirect
// The supplied 16-bit address is read to get the actual 16-bit address. This is
// instruction is unusual in that it has a bug in the hardware! To emulate its
// function accurately, we also need to emulate this bug. If the low byte of the
// supplied address is 0xFF, then to read the high byte of the actual address
// we need to cross a page boundary. This doesnt actually work on the chip as
// designed, instead it wraps back around in the same page, yielding an
// invalid actual address
func (cpu CPU) Ind() uint8 {
	var lo uint16 = uint16(cpu.Read(cpu.programCounter))
	cpu.programCounter++
	var hi uint16 = uint16(cpu.Read(cpu.programCounter))
	cpu.programCounter++

	var ptr uint16 = (hi << 8) | lo

	if lo == 0x00ff { // Simulate page boundary hardware bug
		tempVal16 := uint16(ptr & 0xff00)
		addrAbs = uint16(cpu.Read(tempVal16))<<8 | uint16(cpu.Read(ptr+0))
	} else { // Behave normally
		tempVal16 := uint16(ptr + 1)
		addrAbs = uint16(cpu.Read(tempVal16))<<8 | uint16(cpu.Read(ptr+0))
	}

	return 0
}

// Izx is Address Mode: Indirect X
// The supplied 8-bit address is offset by X Register to index
// a location in page 0x00. The actual 16-bit address is read
// from this location
func (cpu CPU) Izx() uint8 {
	var t uint16 = uint16(cpu.Read(cpu.programCounter))
	cpu.programCounter++

	var lo uint16 = uint16(cpu.Read((t + uint16(cpu.x)) & 0x00ff))
	var hi uint16 = uint16(cpu.Read((t + uint16(cpu.x) + 1) & 0x00ff))

	addrAbs = (hi << 8) | lo

	return 0
}

// Izy is Address Mode: Indirect Y
// The supplied 8-bit address indexes a location in page 0x00. From
// here the actual 16-bit address is read, and the contents of
// Y Register is added to it to offset it. If the offset causes a
// change in page then an additional clock cycle is required.
func (cpu CPU) Izy() uint8 {
	var t uint16 = uint16(cpu.Read(cpu.programCounter))
	cpu.programCounter++

	var lo uint16 = uint16(cpu.Read(t & 0x00ff))
	var hi uint16 = uint16(cpu.Read((t + 1) & 0x00ff))

	addrAbs = (hi << 8) | lo
	addrAbs += uint16(cpu.y)

	if (addrAbs & 0xff00) != (hi << 8) {
		return 1
	}
	return 0
}

// Fetch is function sources the data used by the instruction into
// a convenient numeric variable. Some Instructions dont have to
// fetch data as the source is implied by the instruction. For example
// "INX" increments the X register. There is no additional data
// required. For all other addressing modes, the data resides at
// the location held within addr_abs, so it is read from there.
// Immediate adress mode exploits this slightly, as that has
// set addr_abs = pc + 1, so it fetches the data from the
// next byte for example "LDA $FF" just loads the accumulator with
// 256, i.e. no far reaching memory fetch is required. "fetched"
// is a variable global to the CPU, and is set by calling this
// function. It also returns it for convenience.
func (cpu CPU) Fetch() uint8 {

	if cpu.instructions[opcode].AddrMdType != AddrModeTypeImp {
		fetched = cpu.Read(addrAbs)
	}

	return fetched
}

///////////////////////////////////////////////////////////////////////////////
// INSTRUCTION IMPLEMENTATIONS

// Note: Ive started with the two most complicated Instructions to emulate, which
// ironically is addition and subtraction! Ive tried to include a detailed
// explanation as to why they are so complex, yet so fundamental. Im also NOT
// going to do this through the explanation of 1 and 2's complement.

// Instruction: Add with Carry In
// Function:    A = A + M + C
// Flags Out:   C, V, N, Z
//
// Explanation:
// The purpose of this function is to add a value to the accumulator and a carry bit. If
// the result is > 255 there is an overflow setting the carry bit. Ths allows you to
// chain together ADC Instructions to add numbers larger than 8-bits. This in itself is
// simple, however the 6502 supports the concepts of Negativity/Positivity and Signed Overflow.
//
// 10000100 = 128 + 4 = 132 in normal circumstances, we know this as unsigned and it allows
// us to represent numbers between 0 and 255 (given 8 bits). The 6502 can also interpret
// this word as something else if we assume those 8 bits represent the range -128 to +127,
// i.e. it has become signed.
//
// Since 132 > 127, it effectively wraps around, through -128, to -124. This wraparound is
// called overflow, and this is a useful to know as it indicates that the calculation has
// gone outside the permissable range, and therefore no longer makes numeric sense.
//
// Note the implementation of ADD is the same in binary, this is just about how the numbers
// are represented, so the word 10000100 can be both -124 and 132 depending upon the
// context the programming is using it in. We can prove this!
//
//  10000100 =  132  or  -124
// +00010001 = + 17      + 17
//  ========    ===       ===     See, both are valid additions, but our interpretation of
//  10010101 =  149  or  -107     the context changes the value, not the hardware!
//
// In principle under the -128 to 127 range:
// 10000000 = -128, 11111111 = -1, 00000000 = 0, 00000000 = +1, 01111111 = +127
// therefore negative numbers have the most significant set, positive numbers do not
//
// To assist us, the 6502 can set the overflow flag, if the result of the addition has
// wrapped around. V <- ~(A^M) & A^(A+M+C) :D lol, let's work out why!
//
// Let's suppose we have A = 30, M = 10 and C = 0
//          A = 30 = 00011110
//          M = 10 = 00001010+
//     RESULT = 40 = 00101000
//
// Here we have not gone out of range. The resulting significant bit has not changed.
// So let's make a truth table to understand when overflow has occurred. Here I take
// the MSB of each component, where R is RESULT.
//
// A  M  R | V | A^R | A^M |~(A^M) |
// 0  0  0 | 0 |  0  |  0  |   1   |
// 0  0  1 | 1 |  1  |  0  |   1   |
// 0  1  0 | 0 |  0  |  1  |   0   |
// 0  1  1 | 0 |  1  |  1  |   0   |  so V = ~(A^M) & (A^R)
// 1  0  0 | 0 |  1  |  1  |   0   |
// 1  0  1 | 0 |  0  |  1  |   0   |
// 1  1  0 | 1 |  1  |  0  |   1   |
// 1  1  1 | 0 |  0  |  0  |   1   |
//
// We can see how the above equation calculates V, based on A, M and R. V was chosen
// based on the following hypothesis:
//       Positive Number + Positive Number = Negative Result -> Overflow
//       Negative Number + Negative Number = Positive Result -> Overflow
//       Positive Number + Negative Number = Either Result -> Cannot Overflow
//       Positive Number + Positive Number = Positive Result -> OK! No Overflow
//       Negative Number + Negative Number = Negative Result -> OK! NO Overflow

// Adc is add numbers larger than 8-bits
func (cpu CPU) Adc() uint8 {
	// Grab the data that we are adding to the accumulator
	cpu.Fetch()

	// Add is performed in 16-bit domain for emulation to capture any
	// carry bit, which will exist in bit 8 of the 16-bit word
	temp = uint16(cpu.a+fetched) + uint16(cpu.GetFlag(FlagC))

	// The carry flag out exists in the high byte bit 0
	cpu.SetFlag(FlagC, temp > 255)

	// The Zero flag is set if the result is 0
	cpu.SetFlag(FlagZ, (temp&0x00ff) == 0)

	// The signed Overflow flag is set based on all that up there! :D
	//cpu.SetFlag(FlagV, (~((uint16_t)cpu.a ^ (uint16_t)fetched) & ((uint16_t)cpu.a ^ (uint16_t)temp)) & 0x0080);

	// The negative flag is set to the most significant bit of the result
	cpu.SetFlag(FlagN, ((temp & uint16(0x80)) != 0))

	// Load the result into the accumulator (it's 8-bit dont forget!)
	cpu.a = uint8(temp & 0x00ff)

	// This instruction has the potential to require an additional clock cycle
	return 1
}

// Sbc is Instruction: Subtraction with Borrow In
// Function:    A = A - M - (1 - C)
// Flags Out:   C, V, N, Z
//
// Explanation:
// Given the explanation for ADC above, we can reorganise our data
// to use the same computation for addition, for subtraction by multiplying
// the data by -1, i.e. make it negative
//
// A = A - M - (1 - C)  ->  A = A + -1 * (M - (1 - C))  ->  A = A + (-M + 1 + C)
//
// To make a signed positive number negative, we can invert the bits and add 1
// (OK, I lied, a little bit of 1 and 2s complement :P)
//
//  5 = 00000101
// -5 = 11111010 + 00000001 = 11111011 (or 251 in our 0 to 255 range)
//
// The range is actually unimportant, because if I take the value 15, and add 251
// to it, given we wrap around at 256, the result is 10, so it has effectively
// subtracted 5, which was the original intention. (15 + 251) % 256 = 10
//
// Note that the equation above used (1-C), but this got converted to + 1 + C.
// This means we already have the +1, so all we need to do is invert the bits
// of M, the data(!) therfore we can simply add, exactly the same way we did
// before.
func (cpu CPU) Sbc() uint8 {
	cpu.Fetch()

	// Operating in 16-bit domain to capture carry out

	// We can invert the bottom 8 bits with bitwise xor
	var value uint16 = uint16(fetched) ^ 0x00ff

	// Notice this is exactly the same as addition from here!
	temp = uint16(cpu.a) + value + uint16(cpu.GetFlag(FlagC))
	cpu.SetFlag(FlagC, ((temp & 0xff00) != 0))
	cpu.SetFlag(FlagZ, ((temp & 0x00ff) == 0))
	cpu.SetFlag(FlagV, ((temp^uint16(cpu.a))&(temp^value)&0x0080) != 0)
	cpu.SetFlag(FlagN, ((temp & 0x0080) != 0))

	cpu.a = uint8(temp & 0x00ff)

	return 1
}

// OK! Complicated operations are done! the following are much simpler
// and conventional. The typical order of events is:
// 1) Fetch the data you are working with
// 2) Perform calculation
// 3) Store the result in desired place
// 4) Set Flags of the status register
// 5) Return if instruction has potential to require additional
//    clock cycle

// And is Instruction: Bitwise Logic AND
// Function:    A = A & M
// Flags Out:   N, Z
func (cpu CPU) And() uint8 {
	cpu.Fetch()

	cpu.a = cpu.a & fetched

	cpu.SetFlag(FlagZ, (cpu.a == 0x00))
	cpu.SetFlag(FlagN, ((cpu.a & 0x80) != 0))
	return 1
}

// Asl is Instruction: Arithmetic Shift Left
// Function:    A = C <- (A << 1) <- 0
// Flags Out:   N, Z, C
func (cpu CPU) Asl() uint8 {
	cpu.Fetch()

	temp = uint16(fetched) << 1

	cpu.SetFlag(FlagC, ((temp & 0xff00) > 0))
	cpu.SetFlag(FlagZ, ((temp & 0x00ff) == 0x00))
	cpu.SetFlag(FlagN, ((temp & 0x80) != 0))

	if cpu.instructions[opcode].AddrMdType == AddrModeTypeImp {
		cpu.a = uint8(temp & 0x00ff)
	} else {
		cpu.Write(addrAbs, uint8(temp&0x00ff))
	}

	return 0
}

// Bcc is Instruction: Branch if Carry Clear
// Function:    if(C == 0) pc = address
func (cpu CPU) Bcc() uint8 {
	if cpu.GetFlag(FlagC) == 0 {
		cycles++
		addrAbs = cpu.programCounter + addrRel

		if (addrAbs & 0xff00) != (cpu.programCounter & 0xff00) {
			cycles++
		}

		cpu.programCounter = addrAbs
	}
	return 0
}

// Bcs is Instruction: Branch if Carry Set
// Function:    if(C == 1) pc = address
func (cpu CPU) Bcs() uint8 {
	if cpu.GetFlag(FlagC) == 1 {
		cycles++

		addrAbs = cpu.programCounter + addrRel

		if (addrAbs & 0xff00) != (cpu.programCounter & 0xff00) {
			cycles++
		}

		cpu.programCounter = addrAbs
	}
	return 0
}

// Beq is Instruction: Branch if Equal
// Function:    if(Z == 1) pc = address
func (cpu CPU) Beq() uint8 {
	if cpu.GetFlag(FlagZ) == 1 {
		cycles++

		addrAbs = cpu.programCounter + addrRel

		if (addrAbs & 0xff00) != (cpu.programCounter & 0xff00) {
			cycles++
		}

		cpu.programCounter = addrAbs
	}
	return 0
}

// Bit is Instruction:
func (cpu CPU) Bit() uint8 {
	cpu.Fetch()

	temp = uint16(cpu.a & fetched)

	cpu.SetFlag(FlagZ, ((temp & 0x00FF) == 0x00))
	cpu.SetFlag(FlagN, (fetched&(1<<7)) != 0x00)
	cpu.SetFlag(FlagV, (fetched&(1<<6)) != 0x00)
	return 0
}

// Bmi is Instruction: Branch if Negative
// Function:    if(N == 1) pc = address
func (cpu CPU) Bmi() uint8 {
	if cpu.GetFlag(FlagN) == 1 {
		cycles++

		addrAbs = cpu.programCounter + addrRel

		if (addrAbs & 0xff00) != (cpu.programCounter & 0xff00) {
			cycles++
		}

		cpu.programCounter = addrAbs
	}
	return 0
}

// Bne is Instruction: Branch if Not Equal
// Function:    if(Z == 0) pc = address
func (cpu CPU) Bne() uint8 {
	if cpu.GetFlag(FlagZ) == 0 {
		cycles++

		addrAbs = cpu.programCounter + addrRel

		if (addrAbs & 0xff00) != (cpu.programCounter & 0xff00) {
			cycles++
		}

		cpu.programCounter = addrAbs
	}
	return 0
}

// Bpl is Instruction: Branch if Positive
// Function:    if(N == 0) pc = address
func (cpu CPU) Bpl() uint8 {
	if cpu.GetFlag(FlagN) == 0 {
		cycles++

		addrAbs = cpu.programCounter + addrRel

		if (addrAbs & 0xff00) != (cpu.programCounter & 0xff00) {
			cycles++
		}

		cpu.programCounter = addrAbs
	}
	return 0
}

// Brk is Instruction: Break
// Function:    Program Sourced Interrupt
func (cpu CPU) Brk() uint8 {
	cpu.programCounter++

	cpu.SetFlag(FlagI, true)

	cpu.Write(0x0100+uint16(cpu.stackPtr), uint8((cpu.programCounter>>8)&0x00ff))
	cpu.stackPtr--

	cpu.Write(0x0100+uint16(cpu.stackPtr), uint8((cpu.programCounter & 0x00ff)))
	cpu.stackPtr--

	cpu.SetFlag(FlagB, true)
	cpu.Write(0x0100+uint16(cpu.stackPtr), cpu.status)
	cpu.stackPtr--
	cpu.SetFlag(FlagB, false)

	cpu.programCounter = uint16(cpu.Read(0xfffe)) | uint16(cpu.Read(0xffff))<<8

	return 0
}

// Bvc is Instruction: Branch if Overflow Clear
// Function:    if(V == 0) cpu.programCounter = address
func (cpu CPU) Bvc() uint8 {
	if cpu.GetFlag(FlagV) == 0 {
		cycles++
		addrAbs = cpu.programCounter + addrRel

		if (addrAbs & 0xFF00) != (cpu.programCounter & 0xFF00) {
			cycles++
		}

		cpu.programCounter = addrAbs
	}
	return 0
}

// Bvs is Instruction: Branch if Overflow Set
// Function:    if(V == 1) cpu.programCounter = address
func (cpu CPU) Bvs() uint8 {
	if cpu.GetFlag(FlagV) == 1 {
		cycles++
		addrAbs = cpu.programCounter + addrRel

		if (addrAbs & 0xFF00) != (cpu.programCounter & 0xFF00) {
			cycles++
		}

		cpu.programCounter = addrAbs
	}
	return 0
}

// Clc is Instruction: Clear Carry Flag
// Function:    C = 0
func (cpu CPU) Clc() uint8 {
	cpu.SetFlag(FlagC, false)
	return 0
}

// Cld is Instruction: Clear Decimal Flag
// Function:    D = 0
func (cpu CPU) Cld() uint8 {
	cpu.SetFlag(FlagD, false)
	return 0
}

// Cli is Instruction: Disable Interrupts / Clear Interrupt Flag
// Function:    I = 0
func (cpu CPU) Cli() uint8 {
	cpu.SetFlag(FlagI, false)
	return 0
}

// Clv is Instruction: Clear Overflow Flag
// Function:    V = 0
func (cpu CPU) Clv() uint8 {
	cpu.SetFlag(FlagV, false)
	return 0
}

// Cmp is Instruction: Compare Accumulator
// Function:    C <- A >= M      Z <- (A - M) == 0
// Flags Out:   N, C, Z
func (cpu CPU) Cmp() uint8 {
	cpu.Fetch()

	temp = uint16(cpu.a) - uint16(fetched)

	cpu.SetFlag(FlagC, cpu.a >= fetched)
	cpu.SetFlag(FlagZ, (temp&0x00FF) == 0x0000)
	cpu.SetFlag(FlagN, (temp&0x0080) != 0x00)

	return 1
}

// Cpx is Instruction: Compare X Register
// Function:    C <- X >= M      Z <- (X - M) == 0
// Flags Out:   N, C, Z
func (cpu CPU) Cpx() uint8 {
	cpu.Fetch()
	temp = uint16(cpu.x - fetched)

	cpu.SetFlag(FlagC, cpu.x >= fetched)
	cpu.SetFlag(FlagZ, (temp&0x00FF) == 0x0000)
	cpu.SetFlag(FlagN, (temp&0x0080) != 0x00)

	return 0
}

// Cpy is Instruction: Compare Y Register
// Function:    C <- Y >= M      Z <- (Y - M) == 0
// Flags Out:   N, C, Z
func (cpu CPU) Cpy() uint8 {
	cpu.Fetch()
	temp = uint16(cpu.y - fetched)

	cpu.SetFlag(FlagC, cpu.y >= fetched)
	cpu.SetFlag(FlagZ, (temp&0x00FF) == 0x0000)
	cpu.SetFlag(FlagN, (temp&0x0080) != 0x00)

	return 0
}

// Dec is Instruction: Decrement Value at Memory Location
// Function:    M = M - 1
// Flags Out:   N, Z
func (cpu CPU) Dec() uint8 {
	cpu.Fetch()

	temp = uint16(fetched - 1)

	cpu.Write(addrAbs, uint8(temp&0x00ff))
	cpu.SetFlag(FlagZ, (temp&0x00FF) == 0x0000)
	cpu.SetFlag(FlagN, (temp&0x0080) != 0x00)

	return 0
}

// Dex is Instruction: Decrement X Register
// Function:    X = X - 1
// Flags Out:   N, Z
func (cpu CPU) Dex() uint8 {
	cpu.x--
	cpu.SetFlag(FlagZ, cpu.x == 0x00)
	cpu.SetFlag(FlagN, (cpu.x&0x80) != 0x00)

	return 0
}

// Dey is Instruction: Decrement Y Register
// Function:    Y = Y - 1
// Flags Out:   N, Z
func (cpu CPU) Dey() uint8 {
	cpu.y--
	cpu.SetFlag(FlagZ, cpu.y == 0x00)
	cpu.SetFlag(FlagN, (cpu.y&0x80) != 0x00)

	return 0
}

// Eor is Instruction: Bitwise Logic XOR
// Function:    A = A xor M
// Flags Out:   N, Z
func (cpu CPU) Eor() uint8 {
	cpu.Fetch()

	cpu.a = cpu.a ^ fetched

	cpu.SetFlag(FlagZ, cpu.a == 0x00)
	cpu.SetFlag(FlagN, (cpu.a&0x80) != 0x00)

	return 1
}

// Inc is Instruction: Increment Value at Memory Location
// Function:    M = M + 1
// Flags Out:   N, Z
func (cpu CPU) Inc() uint8 {
	cpu.Fetch()

	temp = uint16(fetched + 1)

	cpu.Write(addrAbs, uint8(temp&0x00FF))
	cpu.SetFlag(FlagZ, (temp&0x00ff) == 0x0000)
	cpu.SetFlag(FlagN, (temp&0x0080) != 0x00)

	return 0
}

// Inx is Instruction: Increment X Register
// Function:    X = X + 1
// Flags Out:   N, Z
func (cpu CPU) Inx() uint8 {
	cpu.x++
	cpu.SetFlag(FlagZ, cpu.x == 0x00)
	cpu.SetFlag(FlagN, (cpu.x&0x80) != 0x00)

	return 0
}

// Iny is Instruction: Increment Y Register
// Function:    Y = Y + 1
// Flags Out:   N, Z
func (cpu CPU) Iny() uint8 {
	cpu.y++
	cpu.SetFlag(FlagZ, cpu.y == 0x00)
	cpu.SetFlag(FlagN, (cpu.y&0x80) != 0x00)

	return 0
}

// Jmp is Instruction: Jump To Location
// Function:    cpu.programCounter = address
func (cpu CPU) Jmp() uint8 {
	cpu.programCounter = addrAbs

	return 0
}

// Jsr is Instruction: Jump To Sub-Routine
// Function:    Push current cpu.programCounter to stack, cpu.programCounter = address
func (cpu CPU) Jsr() uint8 {
	cpu.programCounter--

	cpu.Write(0x0100+uint16(cpu.stackPtr), uint8((cpu.programCounter>>8)&0x00ff))
	cpu.stackPtr--

	cpu.Write(0x0100+uint16(cpu.stackPtr), uint8((cpu.programCounter & 0x00ff)))
	cpu.stackPtr--

	cpu.programCounter = addrAbs
	return 0
}

// Lda is Instruction: Load The Accumulator
// Function:    A = M
// Flags Out:   N, Z
func (cpu CPU) Lda() uint8 {
	cpu.Fetch()

	cpu.a = fetched

	cpu.SetFlag(FlagZ, cpu.a == 0x00)
	cpu.SetFlag(FlagN, (cpu.a&0x80) != 0)

	return 1
}

// Ldx is Instruction: Load The X Register
// Function:    X = M
// Flags Out:   N, Z
func (cpu CPU) Ldx() uint8 {
	cpu.Fetch()

	cpu.x = fetched

	cpu.SetFlag(FlagZ, cpu.x == 0x00)
	cpu.SetFlag(FlagN, (cpu.x&0x80) != 0)

	return 1
}

// Ldy is Instruction: Load The Y Register
// Function:    Y = M
// Flags Out:   N, Z
func (cpu CPU) Ldy() uint8 {
	cpu.Fetch()

	cpu.y = fetched

	cpu.SetFlag(FlagZ, cpu.y == 0x00)
	cpu.SetFlag(FlagN, (cpu.y&0x80) != 0)

	return 1
}

// Lsr is Instruciton
func (cpu CPU) Lsr() uint8 {
	cpu.Fetch()

	cpu.SetFlag(FlagC, (fetched&0x0001) != 0)

	temp = uint16(fetched) >> 1
	cpu.SetFlag(FlagZ, (temp&0x00FF) == 0x0000)
	cpu.SetFlag(FlagN, (temp&0x0080) != 0)

	if cpu.instructions[opcode].AddrMdType == AddrModeTypeImp {
		cpu.a = uint8(temp & 0x00ff)
	} else {
		cpu.Write(addrAbs, uint8(temp&0x00ff))
	}

	return 0
}

// Nop is Instruction
func (cpu CPU) Nop() uint8 {
	// Sadly not all NOPs are equal, Ive added cpu.a few here
	// based on https://wiki.nesdev.com/w/index.php/CPU_unofficial_opcodes
	// and will add more based on game compatibility, and ultimately
	// I'd like to cover all illegal opcodes too
	switch opcode {
	case 0x1C, 0x3C, 0x5C, 0x7C, 0xDC, 0xFC:
		return 1
	}

	return 0
}

// Ora is Instruction: Bitwise Logic OR
// Function:    A = A | M
// Flags Out:   N, Z
func (cpu CPU) Ora() uint8 {
	cpu.Fetch()

	cpu.a = cpu.a | fetched

	cpu.SetFlag(FlagZ, cpu.a == 0x00)
	cpu.SetFlag(FlagN, (cpu.a&0x80) != 0)

	return 1
}

// Pha is Instruction: Push Accumulator to Stack
// Function:    A -> stack
func (cpu CPU) Pha() uint8 {
	cpu.Write(0x0100+uint16(cpu.stackPtr), cpu.a)
	cpu.stackPtr--

	return 0
}

// Php is Instruction: Push cpu.status Register to Stack
// Function:    cpu.status -> stack
// Note:        Break flag is set to 1 before push
func (cpu CPU) Php() uint8 {
	cpu.Write(0x0100+uint16(cpu.stackPtr), cpu.status|uint8(FlagB)|uint8(FlagU))

	cpu.SetFlag(FlagB, false)
	cpu.SetFlag(FlagU, false)
	cpu.stackPtr--

	return 0
}

// Pla is Instruction: Pop Accumulator off Stack
// Function:    A <- stack
// Flags Out:   N, Z
func (cpu CPU) Pla() uint8 {
	cpu.stackPtr++
	cpu.a = cpu.Read(0x0100 + uint16(cpu.stackPtr))
	cpu.SetFlag(FlagZ, cpu.a == 0x00)
	cpu.SetFlag(FlagN, (cpu.a&0x80) != 0)

	return 0
}

// Plp is Instruction: Pop cpu.status Register off Stack
// Function:    cpu.status <- stack
func (cpu CPU) Plp() uint8 {
	cpu.stackPtr++
	cpu.status = cpu.Read(0x0100 + uint16(cpu.stackPtr))
	cpu.SetFlag(FlagU, true)

	return 0
}

// Rol is Instsruction
func (cpu CPU) Rol() uint8 {
	cpu.Fetch()

	temp = uint16((fetched << 1) | cpu.GetFlag(FlagC))

	cpu.SetFlag(FlagC, (temp&0xff00) != 0x00)
	cpu.SetFlag(FlagZ, (temp&0x00ff) == 0x0000)
	cpu.SetFlag(FlagN, (temp&0x0080) != 0x00)

	if cpu.instructions[opcode].AddrMdType == AddrModeTypeImp {
		cpu.a = uint8(temp & 0x00ff)
	} else {
		cpu.Write(addrAbs, uint8(temp&0x00ff))
	}

	return 0
}

// Ror is Instruction
func (cpu CPU) Ror() uint8 {
	cpu.Fetch()

	temp = uint16((cpu.GetFlag(FlagC) << 7) | (fetched >> 1))

	cpu.SetFlag(FlagC, (fetched&0x01) != 0x00)
	cpu.SetFlag(FlagZ, (temp&0x00FF) == 0x00)
	cpu.SetFlag(FlagN, (temp&0x0080) != 0x00)

	if cpu.instructions[opcode].AddrMdType == AddrModeTypeImp {
		cpu.a = uint8(temp & 0x00ff)
	} else {
		cpu.Write(addrAbs, uint8(temp&0x00ff))
	}

	return 0
}

// Rti is Instruction
func (cpu CPU) Rti() uint8 {
	cpu.stackPtr++
	cpu.status = cpu.Read(0x0100 + uint16(cpu.stackPtr))
	cpu.status &= ^uint8(FlagB)
	cpu.status &= ^uint8(FlagU)

	cpu.stackPtr++
	cpu.programCounter = uint16(cpu.Read(0x0100 + uint16(cpu.stackPtr)))
	cpu.stackPtr++
	cpu.programCounter |= uint16(cpu.Read(0x0100+uint16(cpu.stackPtr))) << 8

	return 0
}

// Rts is Instruction
func (cpu CPU) Rts() uint8 {
	cpu.stackPtr++
	cpu.programCounter = uint16(cpu.Read(0x0100 + uint16(cpu.stackPtr)))

	cpu.stackPtr++
	cpu.programCounter |= uint16(cpu.Read(0x0100+uint16(cpu.stackPtr))) << 8

	cpu.programCounter++

	return 0
}

// Sec is Instruction: Set Carry Flag
// Function:    C = 1
func (cpu CPU) Sec() uint8 {
	cpu.SetFlag(FlagC, true)

	return 0
}

// Sed is Instruction: Set Decimal Flag
// Function:    D = 1
func (cpu CPU) Sed() uint8 {
	cpu.SetFlag(FlagD, true)

	return 0
}

// Sei is Instruction: Set Interrupt Flag / Enable Interrupts
// Function:    I = 1
func (cpu CPU) Sei() uint8 {
	cpu.SetFlag(FlagI, true)
	return 0
}

// Sta is Instruction: Store Accumulator at Address
// Function:    M = A
func (cpu CPU) Sta() uint8 {
	cpu.Write(addrAbs, cpu.a)

	return 0
}

// Stx is Instruction: Store X Register at Address
// Function:    M = X
func (cpu CPU) Stx() uint8 {
	cpu.Write(addrAbs, cpu.x)

	return 0
}

// Sty is Instruction: Store Y Register at Address
// Function:    M = Y
func (cpu CPU) Sty() uint8 {
	cpu.Write(addrAbs, cpu.y)

	return 0
}

// Tax is Instruction: Transfer Accumulator to X Register
// Function:    X = A
// Flags Out:   N, Z
func (cpu CPU) Tax() uint8 {
	cpu.x = cpu.a
	cpu.SetFlag(FlagZ, cpu.x == 0x00)
	cpu.SetFlag(FlagN, (cpu.x&0x80) != 0x00)

	return 0
}

// Tay is Instruction: Transfer Accumulator to Y Register
// Function:    Y = A
// Flags Out:   N, Z
func (cpu CPU) Tay() uint8 {
	cpu.y = cpu.a
	cpu.SetFlag(FlagZ, cpu.y == 0x00)
	cpu.SetFlag(FlagN, (cpu.y&0x80) != 0x00)

	return 0
}

// Tsx is Instruction: Transfer Stack Pointer to X Register
// Function:    X = stack pointer
// Flags Out:   N, Z
func (cpu CPU) Tsx() uint8 {
	cpu.x = cpu.stackPtr
	cpu.SetFlag(FlagZ, cpu.x == 0x00)
	cpu.SetFlag(FlagN, (cpu.x&0x80) != 0x00)

	return 0
}

// Txa is Instruction: Transfer X Register to Accumulator
// Function:    A = X
// Flags Out:   N, Z
func (cpu CPU) Txa() uint8 {
	cpu.a = cpu.x
	cpu.SetFlag(FlagZ, cpu.a == 0x00)
	cpu.SetFlag(FlagN, (cpu.a&0x80) != 0x00)

	return 0
}

// Txs is Instruction: Transfer X Register to Stack Pointer
// Function:    stack pointer = X
func (cpu CPU) Txs() uint8 {
	cpu.stackPtr = cpu.x

	return 0
}

// Tya is Instruction: Transfer Y Register to Accumulator
// Function:    A = Y
// Flags Out:   N, Z
func (cpu CPU) Tya() uint8 {
	cpu.a = cpu.y
	cpu.SetFlag(FlagZ, cpu.a == 0x00)
	cpu.SetFlag(FlagN, (cpu.a&0x80) != 0x00)

	return 0
}

// Xxx is This function captures illegal opcodes
func (cpu CPU) Xxx() uint8 {
	return 0
}

// Complete is helper function
func (cpu CPU) Complete() bool {
	return cycles == 0
}

// Reads an 8-bit byte from the bus, located at the specified 16-bit address
func (cpu CPU) Read(a uint16) uint8 {
	return cpu.bus.Read(a)
}

// Writes a byte to the bus at the specified address
func (cpu CPU) Write(a uint16, d uint8) {
	cpu.bus.Write(a, d)
}

// GetOperateType returns the instruction of struct
func (cpu CPU) GetOperateType(opcode uint8) Instruction {
	return cpu.instructions[opcode]
}

// GetProgramCounter is ...
func (cpu CPU) GetProgramCounter() uint16 {
	return cpu.programCounter
}

// GetRegisterA is ...
func (cpu CPU) GetRegisterA() uint8 {
	return cpu.a
}

// GetRegisterX is ...
func (cpu CPU) GetRegisterX() uint8 {
	return cpu.x
}

// GetRegisterY is ...
func (cpu CPU) GetRegisterY() uint8 {
	return cpu.y
}

func (cpu CPU) GetStackPtr() uint8 {
	return cpu.stackPtr
}

//Config is the method that init the cpu params
func (cpu CPU) Config() {
	cpu.bus.Config()
	cpu.createInstructions()
}

// CreateInstructions is the method that fill 6502 Instructions table
func (cpu *CPU) createInstructions() {
	cpu.instructions = [256]Instruction{
		Instruction{OperateTypeBrk, AddrModeTypeImm, 7, cpu.Brk, cpu.Imm},
		Instruction{OperateTypeOra, AddrModeTypeIzx, 6, cpu.Ora, cpu.Izx},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 8, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 3, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeOra, AddrModeTypeZp0, 3, cpu.Ora, cpu.Zp0},
		Instruction{OperateTypeAsl, AddrModeTypeZp0, 5, cpu.Asl, cpu.Zp0},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 5, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypePhp, AddrModeTypeImp, 3, cpu.Php, cpu.Imp},
		Instruction{OperateTypeOra, AddrModeTypeImm, 2, cpu.Ora, cpu.Imm},
		Instruction{OperateTypeAsl, AddrModeTypeImp, 2, cpu.Asl, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 4, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeOra, AddrModeTypeAbs, 4, cpu.Ora, cpu.Abs},
		Instruction{OperateTypeAsl, AddrModeTypeAbs, 6, cpu.Asl, cpu.Abs},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 6, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeBpl, AddrModeTypeRel, 2, cpu.Bpl, cpu.Rel},
		Instruction{OperateTypeOra, AddrModeTypeIzy, 5, cpu.Ora, cpu.Izy},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 8, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 4, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeOra, AddrModeTypeZpx, 4, cpu.Ora, cpu.Zpx},
		Instruction{OperateTypeAsl, AddrModeTypeZpx, 6, cpu.Asl, cpu.Zpx},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 6, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeClc, AddrModeTypeImp, 2, cpu.Clc, cpu.Imp},
		Instruction{OperateTypeOra, AddrModeTypeAby, 4, cpu.Ora, cpu.Aby},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 7, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 4, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeOra, AddrModeTypeAbx, 4, cpu.Ora, cpu.Abx},
		Instruction{OperateTypeAsl, AddrModeTypeAbx, 7, cpu.Asl, cpu.Abx},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 7, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeJsr, AddrModeTypeAbs, 6, cpu.Jsr, cpu.Abs},
		Instruction{OperateTypeAnd, AddrModeTypeIzx, 6, cpu.And, cpu.Izx},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 8, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeBit, AddrModeTypeZp0, 3, cpu.Bit, cpu.Zp0},
		Instruction{OperateTypeAnd, AddrModeTypeZp0, 3, cpu.And, cpu.Zp0},
		Instruction{OperateTypeRol, AddrModeTypeZp0, 5, cpu.Rol, cpu.Zp0},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 5, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypePlp, AddrModeTypeImp, 4, cpu.Plp, cpu.Imp},
		Instruction{OperateTypeAnd, AddrModeTypeImm, 2, cpu.And, cpu.Imm},
		Instruction{OperateTypeRol, AddrModeTypeImp, 2, cpu.Rol, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeBit, AddrModeTypeAbs, 4, cpu.Bit, cpu.Abs},
		Instruction{OperateTypeAnd, AddrModeTypeAbs, 4, cpu.And, cpu.Abs},
		Instruction{OperateTypeRol, AddrModeTypeAbs, 6, cpu.Rol, cpu.Abs},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 6, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeBmi, AddrModeTypeRel, 2, cpu.Bmi, cpu.Rel},
		Instruction{OperateTypeAnd, AddrModeTypeIzy, 5, cpu.And, cpu.Izy},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 8, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 4, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeAnd, AddrModeTypeZpx, 4, cpu.And, cpu.Zpx},
		Instruction{OperateTypeRol, AddrModeTypeZpx, 6, cpu.Rol, cpu.Zpx},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 6, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeSec, AddrModeTypeImp, 2, cpu.Sec, cpu.Imp},
		Instruction{OperateTypeAnd, AddrModeTypeAby, 4, cpu.And, cpu.Aby},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 7, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 4, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeAnd, AddrModeTypeAbx, 4, cpu.And, cpu.Abx},
		Instruction{OperateTypeRol, AddrModeTypeAbx, 7, cpu.Rol, cpu.Abx},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 7, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeRti, AddrModeTypeImp, 6, cpu.Rti, cpu.Imp},
		Instruction{OperateTypeEor, AddrModeTypeIzx, 6, cpu.Eor, cpu.Izx},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 8, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 3, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeEor, AddrModeTypeZp0, 3, cpu.Eor, cpu.Zp0},
		Instruction{OperateTypeLsr, AddrModeTypeZp0, 5, cpu.Lsr, cpu.Zp0},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 5, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypePha, AddrModeTypeImp, 3, cpu.Pha, cpu.Imp},
		Instruction{OperateTypeEor, AddrModeTypeImm, 2, cpu.Eor, cpu.Imm},
		Instruction{OperateTypeLsr, AddrModeTypeImp, 2, cpu.Lsr, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeJmp, AddrModeTypeAbs, 3, cpu.Jmp, cpu.Abs},
		Instruction{OperateTypeEor, AddrModeTypeAbs, 4, cpu.Eor, cpu.Abs},
		Instruction{OperateTypeLsr, AddrModeTypeAbs, 6, cpu.Lsr, cpu.Abs},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 6, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeBvc, AddrModeTypeRel, 2, cpu.Bvc, cpu.Rel},
		Instruction{OperateTypeEor, AddrModeTypeIzy, 5, cpu.Eor, cpu.Izy},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 8, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 4, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeEor, AddrModeTypeZpx, 4, cpu.Eor, cpu.Zpx},
		Instruction{OperateTypeLsr, AddrModeTypeZpx, 6, cpu.Lsr, cpu.Zpx},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 6, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeCli, AddrModeTypeImp, 2, cpu.Cli, cpu.Imp},
		Instruction{OperateTypeEor, AddrModeTypeAby, 4, cpu.Eor, cpu.Aby},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 7, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 4, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeEor, AddrModeTypeAbx, 4, cpu.Eor, cpu.Abx},
		Instruction{OperateTypeLsr, AddrModeTypeAbx, 7, cpu.Lsr, cpu.Abx},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 7, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeRts, AddrModeTypeImp, 6, cpu.Rts, cpu.Imp},
		Instruction{OperateTypeAdc, AddrModeTypeIzx, 6, cpu.Adc, cpu.Izx},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 8, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 3, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeAdc, AddrModeTypeZp0, 3, cpu.Adc, cpu.Zp0},
		Instruction{OperateTypeRor, AddrModeTypeZp0, 5, cpu.Ror, cpu.Zp0},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 5, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypePla, AddrModeTypeImp, 4, cpu.Pla, cpu.Imp},
		Instruction{OperateTypeAdc, AddrModeTypeImm, 2, cpu.Adc, cpu.Imm},
		Instruction{OperateTypeRor, AddrModeTypeImp, 2, cpu.Ror, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeJmp, AddrModeTypeInd, 5, cpu.Jmp, cpu.Ind},
		Instruction{OperateTypeAdc, AddrModeTypeAbs, 4, cpu.Adc, cpu.Abs},
		Instruction{OperateTypeRor, AddrModeTypeAbs, 6, cpu.Ror, cpu.Abs},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 6, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeBvs, AddrModeTypeRel, 2, cpu.Bvs, cpu.Rel},
		Instruction{OperateTypeAdc, AddrModeTypeIzy, 5, cpu.Adc, cpu.Izy},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 8, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 4, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeAdc, AddrModeTypeZpx, 4, cpu.Adc, cpu.Zpx},
		Instruction{OperateTypeRor, AddrModeTypeZpx, 6, cpu.Ror, cpu.Zpx},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 6, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeSei, AddrModeTypeImp, 2, cpu.Sei, cpu.Imp},
		Instruction{OperateTypeAdc, AddrModeTypeAby, 4, cpu.Adc, cpu.Aby},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 7, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 4, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeAdc, AddrModeTypeAbx, 4, cpu.Adc, cpu.Abx},
		Instruction{OperateTypeRor, AddrModeTypeAbx, 7, cpu.Ror, cpu.Abx},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 7, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeSta, AddrModeTypeIzx, 6, cpu.Sta, cpu.Izx},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 6, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeSty, AddrModeTypeZp0, 3, cpu.Sty, cpu.Zp0},
		Instruction{OperateTypeSta, AddrModeTypeZp0, 3, cpu.Sta, cpu.Zp0},
		Instruction{OperateTypeStx, AddrModeTypeZp0, 3, cpu.Stx, cpu.Zp0},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 3, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeDey, AddrModeTypeImp, 2, cpu.Dey, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeTxa, AddrModeTypeImp, 2, cpu.Txa, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeSty, AddrModeTypeAbs, 4, cpu.Sty, cpu.Abs},
		Instruction{OperateTypeSta, AddrModeTypeAbs, 4, cpu.Sta, cpu.Abs},
		Instruction{OperateTypeStx, AddrModeTypeAbs, 4, cpu.Stx, cpu.Abs},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 4, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeBcc, AddrModeTypeRel, 2, cpu.Bcc, cpu.Rel},
		Instruction{OperateTypeSta, AddrModeTypeIzy, 6, cpu.Sta, cpu.Izy},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 6, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeSty, AddrModeTypeZpx, 4, cpu.Sty, cpu.Zpx},
		Instruction{OperateTypeSta, AddrModeTypeZpx, 4, cpu.Sta, cpu.Zpx},
		Instruction{OperateTypeStx, AddrModeTypeZpy, 4, cpu.Stx, cpu.Zpy},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 4, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeTya, AddrModeTypeImp, 2, cpu.Tya, cpu.Imp},
		Instruction{OperateTypeSta, AddrModeTypeAby, 5, cpu.Sta, cpu.Aby},
		Instruction{OperateTypeTxs, AddrModeTypeImp, 2, cpu.Txs, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 5, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 5, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeSta, AddrModeTypeAbx, 5, cpu.Sta, cpu.Abx},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 5, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 5, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeLdy, AddrModeTypeImm, 2, cpu.Ldy, cpu.Imm},
		Instruction{OperateTypeLda, AddrModeTypeIzx, 6, cpu.Lda, cpu.Izx},
		Instruction{OperateTypeLdx, AddrModeTypeImm, 2, cpu.Ldx, cpu.Imm},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 6, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeLdy, AddrModeTypeZp0, 3, cpu.Ldy, cpu.Zp0},
		Instruction{OperateTypeLda, AddrModeTypeZp0, 3, cpu.Lda, cpu.Zp0},
		Instruction{OperateTypeLdx, AddrModeTypeZp0, 3, cpu.Ldx, cpu.Zp0},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 3, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeTay, AddrModeTypeImp, 2, cpu.Tay, cpu.Imp},
		Instruction{OperateTypeLda, AddrModeTypeImm, 2, cpu.Lda, cpu.Imm},
		Instruction{OperateTypeTax, AddrModeTypeImp, 2, cpu.Tax, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeLdy, AddrModeTypeAbs, 4, cpu.Ldy, cpu.Abs},
		Instruction{OperateTypeLda, AddrModeTypeAbs, 4, cpu.Lda, cpu.Abs},
		Instruction{OperateTypeLdx, AddrModeTypeAbs, 4, cpu.Ldx, cpu.Abs},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 4, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeBcs, AddrModeTypeRel, 2, cpu.Bcs, cpu.Rel},
		Instruction{OperateTypeLda, AddrModeTypeIzy, 5, cpu.Lda, cpu.Izy},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 5, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeLdy, AddrModeTypeZpx, 4, cpu.Ldy, cpu.Zpx},
		Instruction{OperateTypeLda, AddrModeTypeZpx, 4, cpu.Lda, cpu.Zpx},
		Instruction{OperateTypeLdx, AddrModeTypeZpy, 4, cpu.Ldx, cpu.Zpy},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 4, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeClv, AddrModeTypeImp, 2, cpu.Clv, cpu.Imp},
		Instruction{OperateTypeLda, AddrModeTypeAby, 4, cpu.Lda, cpu.Aby},
		Instruction{OperateTypeTsx, AddrModeTypeImp, 2, cpu.Tsx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 4, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeLdy, AddrModeTypeAbx, 4, cpu.Ldy, cpu.Abx},
		Instruction{OperateTypeLda, AddrModeTypeAbx, 4, cpu.Lda, cpu.Abx},
		Instruction{OperateTypeLdx, AddrModeTypeAby, 4, cpu.Ldx, cpu.Aby},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 4, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeCpy, AddrModeTypeImm, 2, cpu.Cpy, cpu.Imm},
		Instruction{OperateTypeCmp, AddrModeTypeIzx, 6, cpu.Cmp, cpu.Izx},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 8, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeCpy, AddrModeTypeZp0, 3, cpu.Cpy, cpu.Zp0},
		Instruction{OperateTypeCmp, AddrModeTypeZp0, 3, cpu.Cmp, cpu.Zp0},
		Instruction{OperateTypeDec, AddrModeTypeZp0, 5, cpu.Dec, cpu.Zp0},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 5, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeIny, AddrModeTypeImp, 2, cpu.Iny, cpu.Imp},
		Instruction{OperateTypeCmp, AddrModeTypeImm, 2, cpu.Cmp, cpu.Imm},
		Instruction{OperateTypeDex, AddrModeTypeImp, 2, cpu.Dex, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeCpy, AddrModeTypeAbs, 4, cpu.Cpy, cpu.Abs},
		Instruction{OperateTypeCmp, AddrModeTypeAbs, 4, cpu.Cmp, cpu.Abs},
		Instruction{OperateTypeDec, AddrModeTypeAbs, 6, cpu.Dec, cpu.Abs},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 6, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeBne, AddrModeTypeRel, 2, cpu.Bne, cpu.Rel},
		Instruction{OperateTypeCmp, AddrModeTypeIzy, 5, cpu.Cmp, cpu.Izy},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 8, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 4, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeCmp, AddrModeTypeZpx, 4, cpu.Cmp, cpu.Zpx},
		Instruction{OperateTypeDec, AddrModeTypeZpx, 6, cpu.Dec, cpu.Zpx},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 6, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeCld, AddrModeTypeImp, 2, cpu.Cld, cpu.Imp},
		Instruction{OperateTypeCmp, AddrModeTypeAby, 4, cpu.Cmp, cpu.Aby},
		Instruction{OperateTypeNop, AddrModeTypeImp, 2, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 7, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 4, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeCmp, AddrModeTypeAbx, 4, cpu.Cmp, cpu.Abx},
		Instruction{OperateTypeDec, AddrModeTypeAbx, 7, cpu.Dec, cpu.Abx},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 7, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeCpx, AddrModeTypeImm, 2, cpu.Cpx, cpu.Imm},
		Instruction{OperateTypeSbc, AddrModeTypeIzx, 6, cpu.Sbc, cpu.Izx},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 8, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeCpx, AddrModeTypeZp0, 3, cpu.Cpx, cpu.Zp0},
		Instruction{OperateTypeSbc, AddrModeTypeZp0, 3, cpu.Sbc, cpu.Zp0},
		Instruction{OperateTypeInc, AddrModeTypeZp0, 5, cpu.Inc, cpu.Zp0},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 5, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeInx, AddrModeTypeImp, 2, cpu.Inx, cpu.Imp},
		Instruction{OperateTypeSbc, AddrModeTypeImm, 2, cpu.Sbc, cpu.Imm},
		Instruction{OperateTypeNop, AddrModeTypeImp, 2, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Sbc, cpu.Imp},
		Instruction{OperateTypeCpx, AddrModeTypeAbs, 4, cpu.Cpx, cpu.Abs},
		Instruction{OperateTypeSbc, AddrModeTypeAbs, 4, cpu.Sbc, cpu.Abs},
		Instruction{OperateTypeInc, AddrModeTypeAbs, 6, cpu.Inc, cpu.Abs},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 6, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeBeq, AddrModeTypeRel, 2, cpu.Beq, cpu.Rel},
		Instruction{OperateTypeSbc, AddrModeTypeIzy, 5, cpu.Sbc, cpu.Izy},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 2, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 8, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 4, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeSbc, AddrModeTypeZpx, 4, cpu.Sbc, cpu.Zpx},
		Instruction{OperateTypeInc, AddrModeTypeZpx, 6, cpu.Inc, cpu.Zpx},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 6, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeSed, AddrModeTypeImp, 2, cpu.Sed, cpu.Imp},
		Instruction{OperateTypeSbc, AddrModeTypeAby, 4, cpu.Sbc, cpu.Aby},
		Instruction{OperateTypeNop, AddrModeTypeImp, 2, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 7, cpu.Xxx, cpu.Imp},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 4, cpu.Nop, cpu.Imp},
		Instruction{OperateTypeSbc, AddrModeTypeAbx, 4, cpu.Sbc, cpu.Abx},
		Instruction{OperateTypeInc, AddrModeTypeAbx, 7, cpu.Inc, cpu.Abx},
		Instruction{OperateTypeXxx, AddrModeTypeImp, 7, cpu.Xxx, cpu.Imp},
	}
}
