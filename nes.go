package main

// Instruction is the opcode translation table.
type Instruction struct {
	name     string
	cycles   uint8
	Operate  func() uint8
	Addrmode func() uint8
}

// The6502 CPU Core registers
type The6502 struct {
	a              uint8  // Accumulator Register
	x              uint8  // X Register
	y              uint8  // Y Register
	stackPtr       uint8  // Stack Pointer (points to location on bus)
	programCounter uint16 // Program Counter
	status         uint8  // Status Register
	instructions   []*Instruction
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

// Flag is the status register flag.
type Flag uint8

// The status register stores 8 flags.
const (
	FlagC Flag = (1 << 0) // Carry Bit
	FlagZ Flag = (1 << 1) // Zero
	FlagI Flag = (1 << 2) // Disable Interrupts
	FlagD Flag = (1 << 3) // Decimal Mode (unused in this implementation)
	FlagB Flag = (1 << 4) // Break
	FlagU Flag = (1 << 5) // Unused
	FlagV Flag = (1 << 6) // Overflow
	FlagN Flag = (1 << 7) // Negative
)

// Reset Interrupt - Forces CPU into known state
func (the6502 The6502) reset() {

	// Get address to set program counter to
	addrAbs = 0xfffc

	var lo uint16 = uint16(read(addrAbs + 0))
	var hi uint16 = uint16(read(addrAbs + 1))

	// Set it
	the6502.programCounter = (hi << 8) | lo

	// Reset internal registers
	the6502.a = 0x00
	the6502.x = 0x00
	the6502.y = 0x00
	the6502.stackPtr = 0xfd
	the6502.status = uint8(0x00 | FlagU)

	// Clear internal helper variables
	addrRel = 0x0000
	addrAbs = 0x0000
	fetched = 0x00

	// Reset takes time
	cycles = 8
}

// Interrupt Request - Executes an instruction at a specific location
func (the6502 The6502) irq() {

	// If interrupts are allowed
	if the6502.GetFlag(FlagI) == 0 {
		var tempVal16 uint16
		var tempVal8 uint8

		// Push the program counter to the stack. It's 16-bits dont
		// forget so that takes two pushes
		tempVal16 = 0x0100 + uint16(the6502.stackPtr)
		tempVal8 = uint8(the6502.programCounter>>8) & 0x00ff
		write(tempVal16, tempVal8)
		the6502.stackPtr--

		tempVal16 = 0x0100 + uint16(the6502.stackPtr)
		tempVal8 = uint8(the6502.programCounter) & 0x00ff
		write(tempVal16, tempVal8)
		the6502.stackPtr--

		// Then Push the status register to the stack
		the6502.SetFlag(FlagB, false)
		the6502.SetFlag(FlagU, true)
		the6502.SetFlag(FlagI, true)

		tempVal16 = 0x0100 + uint16(the6502.stackPtr)
		write(tempVal16, the6502.status)
		the6502.stackPtr--

		// Read new program counter location from fixed address
		addrAbs = 0xfffe
		var lo uint16 = uint16(read(addrAbs + 0))
		var hi uint16 = uint16(read(addrAbs + 1))
		the6502.programCounter = (hi << 8) | lo

		// IRQs take time
		cycles = 7
	}
}

// Non-Maskable Interrupt Request - As above, but cannot be disabled
func (the6502 The6502) nmi() {
	var tempVal16 uint16
	var tempVal8 uint8

	tempVal16 = 0x0100 + uint16(the6502.stackPtr)
	tempVal8 = uint8(the6502.programCounter>>8) & 0x00ff
	write(tempVal16, tempVal8)
	the6502.stackPtr--

	tempVal16 = 0x0100 + uint16(the6502.stackPtr)
	tempVal8 = uint8(the6502.programCounter) & 0x00ff
	write(tempVal16, tempVal8)
	the6502.stackPtr--

	the6502.SetFlag(FlagB, false)
	the6502.SetFlag(FlagU, true)
	the6502.SetFlag(FlagI, true)

	tempVal16 = 0x0100 + uint16(the6502.stackPtr)
	write(tempVal16, the6502.status)
	the6502.stackPtr--

	addrAbs = 0xfffa
	var lo uint16 = uint16(read(addrAbs + 0))
	var hi uint16 = uint16(read(addrAbs + 1))
	the6502.programCounter = (hi << 8) | lo

	cycles = 8
}

// Perform one clock cycle's worth of update
func (the6502 The6502) clock() {

	// Each instruction requires a variable number of clock cycles to execute.
	// In my emulation, I only care about the final result and so I perform
	// the entire computation in one hit. In hardware, each clock cycle would
	// perform "microcode" style transformations of the CPUs state.
	//
	// To remain compliant with connected devices, it's important that the
	// emulation also takes "time" in order to execute instructions, so I
	// implement that delay by simply counting down the cycles required by
	// the instruction. When it reaches 0, the instruction is complete, and
	// the next one is ready to be executed.
	if cycles == 0 {
		// Read next instruction byte. This 8-bit value is used to index
		// the translation table to get the relevant information about
		// how to implement the instruction
		opcode = read(the6502.programCounter)

		// Always set the unused status flag bit to 1
		the6502.SetFlag(FlagU, true)

		// Increment program counter, we read the opcode byte
		the6502.programCounter++

		// Get Starting number of cycles
		cycles = the6502.instructions[opcode].cycles

		// Perform fetch of intermmediate data using the
		// required addressing mode
		var additionalCycle1 uint8 = the6502.instructions[opcode].Addrmode()

		// Perform operation
		var additionalCycle2 uint8 = the6502.instructions[opcode].Operate()

		// The addressmode and opcode may have altered the number
		// of cycles this instruction requires before its completed
		cycles += (additionalCycle1 & additionalCycle2)

		// Always set the unused status flag bit to 1
		the6502.SetFlag(FlagU, true)
	}

	// Increment global clock count - This is actually unused unless logging is enabled
	// but I've kept it in because its a handy watch variable for debugging
	clockCount++

	// Decrement the number of cycles remaining for this instruction
	cycles--
}

// GetFlag returns the value of a specific bit of the status register
func (the6502 The6502) GetFlag(flag Flag) uint8 {
	if (the6502.status & uint8(flag)) > 0 {
		return 1
	}
	return 0
}

// SetFlag sets or clears a specific bit of the status register
func (the6502 The6502) SetFlag(flag Flag, v bool) {
	if v {
		the6502.status |= uint8(flag)
	} else {
		the6502.status &= uint8(^flag)
	}
}

// Imp is Address Mode: Implied
// There is no additional data required for this instruction. The instruction
// does something very simple like like sets a status bit. However, we will
// target the accumulator, for instructions like PHA
func (the6502 The6502) Imp() uint8 {
	fetched = the6502.a
	return 0
}

// Imm is Address Mode: Immediate
// The instruction expects the next byte to be used as a value, so we'll prep
// the read address to point to the next byte
func (the6502 The6502) Imm() uint8 {
	the6502.programCounter++
	addrAbs = the6502.programCounter
	return 0
}

// Zp0 is Address Mode: Zero Page
// To save program bytes, zero page addressing allows you to absolutely address
// a location in first 0xFF bytes of address range. Clearly this only requires
// one byte instead of the usual two.
func (the6502 The6502) Zp0() uint8 {
	addrAbs = uint16(read(the6502.programCounter))
	the6502.programCounter++
	addrAbs &= 0x00ff
	return 0
}

// Zpx is Address Mode: Zero Page with X Offset
// Fundamentally the same as Zero Page addressing, but the contents of the X Register
// is added to the supplied single byte address. This is useful for iterating through
// ranges within the first page.
func (the6502 The6502) Zpx() uint8 {
	addrAbs = uint16((read(the6502.programCounter) + the6502.x))
	the6502.programCounter++
	addrAbs &= 0x00ff
	return 0
}

// Zpy is Address Mode: Zero Page with Y Offset
// Same as above but uses Y Register for offset
func (the6502 The6502) Zpy() uint8 {
	addrAbs = uint16((read(the6502.programCounter) + the6502.y))
	the6502.programCounter++
	addrAbs &= 0x00ff
	return 0
}

// Rel is Address Mode: Relative
// This address mode is exclusive to branch instructions. The address
// must reside within -128 to +127 of the branch instruction, i.e.
// you cant directly branch to any address in the addressable range.
func (the6502 The6502) Rel() uint8 {
	addrRel = uint16(read(the6502.programCounter))
	the6502.programCounter++
	if (addrRel & 0x80) != 0 {
		addrRel |= 0xff00
	}
	return 0
}

// Abs Address Mode: Absolute
// A full 16-bit address is loaded and used
func (the6502 The6502) Abs() uint8 {
	var lo uint16 = uint16(read(the6502.programCounter))
	the6502.programCounter++
	var hi uint16 = uint16(read(the6502.programCounter))
	the6502.programCounter++

	addrAbs = (hi << 8) | lo

	return 0
}

// Abx is Address Mode: Absolute with X Offset
// Fundamentally the same as absolute addressing, but the contents of the X Register
// is added to the supplied two byte address. If the resulting address changes
// the page, an additional clock cycle is required
func (the6502 The6502) Abx() uint8 {
	var lo uint16 = uint16(read(the6502.programCounter))
	the6502.programCounter++
	var hi uint16 = uint16(read(the6502.programCounter))
	the6502.programCounter++

	addrAbs = (hi << 8) | lo
	addrAbs += uint16(the6502.x)

	if (addrAbs & 0xff00) != (hi << 8) {
		return 1
	}
	return 0
}

// Aby is Address Mode: Absolute with Y Offset
// Fundamentally the same as absolute addressing, but the contents of the Y Register
// is added to the supplied two byte address. If the resulting address changes
// the page, an additional clock cycle is required
func (the6502 The6502) Aby() uint8 {
	var lo uint16 = uint16(read(the6502.programCounter))
	the6502.programCounter++
	var hi uint16 = uint16(read(the6502.programCounter))
	the6502.programCounter++

	addrAbs = (hi << 8) | lo
	addrAbs += uint16(the6502.y)

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
func (the6502 The6502) Ind() uint8 {
	var lo uint16 = uint16(read(the6502.programCounter))
	the6502.programCounter++
	var hi uint16 = uint16(read(the6502.programCounter))
	the6502.programCounter++

	var ptr uint16 = (hi << 8) | lo

	if lo == 0x00ff { // Simulate page boundary hardware bug
		tempVal16 := uint16(ptr & 0xff00)
		addrAbs = uint16(read(tempVal16)<<8 | read(ptr+0))
	} else { // Behave normally
		tempVal16 := uint16(ptr + 1)
		addrAbs = uint16((read(tempVal16) << 8) | read(ptr+0))
	}

	return 0
}

// Izx is Address Mode: Indirect X
// The supplied 8-bit address is offset by X Register to index
// a location in page 0x00. The actual 16-bit address is read
// from this location
func (the6502 The6502) Izx() uint8 {
	var t uint16 = uint16(read(the6502.programCounter))
	the6502.programCounter++

	var lo uint16 = uint16(read((t + uint16(the6502.x)) & 0x00ff))
	var hi uint16 = uint16(read((t + uint16(the6502.x) + 1) & 0x00ff))

	addrAbs = (hi << 8) | lo

	return 0
}

// Izy is Address Mode: Indirect Y
// The supplied 8-bit address indexes a location in page 0x00. From
// here the actual 16-bit address is read, and the contents of
// Y Register is added to it to offset it. If the offset causes a
// change in page then an additional clock cycle is required.
func (the6502 The6502) Izy() uint8 {
	var t uint16 = uint16(read(the6502.programCounter))
	the6502.programCounter++

	var lo uint16 = uint16(read(t & 0x00ff))
	var hi uint16 = uint16(read((t + 1) & 0x00ff))

	addrAbs = (hi << 8) | lo
	addrAbs += uint16(the6502.y)

	if (addrAbs & 0xff00) != (hi << 8) {
		return 1
	}
	return 0
}

// Fetch is function sources the data used by the instruction into
// a convenient numeric variable. Some instructions dont have to
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
func (the6502 The6502) Fetch() uint8 {

	/* can't do that, refactory is needed
	if the6502.instructions[opcode].Addrmode != the6502.Imp {
		fetched = read(addrAbs)
	}
	*/

	return fetched
}

///////////////////////////////////////////////////////////////////////////////
// INSTRUCTION IMPLEMENTATIONS

// Note: Ive started with the two most complicated instructions to emulate, which
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
// chain together ADC instructions to add numbers larger than 8-bits. This in itself is
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
func (the6502 The6502) Adc() uint8 {
	// Grab the data that we are adding to the accumulator
	the6502.Fetch()

	// Add is performed in 16-bit domain for emulation to capture any
	// carry bit, which will exist in bit 8 of the 16-bit word
	temp = uint16(the6502.a+fetched) + uint16(the6502.GetFlag(FlagC))

	// The carry flag out exists in the high byte bit 0
	the6502.SetFlag(FlagC, temp > 255)

	// The Zero flag is set if the result is 0
	the6502.SetFlag(FlagZ, (temp&0x00ff) == 0)

	// The signed Overflow flag is set based on all that up there! :D
	//the6502.SetFlag(FlagV, (~((uint16_t)the6502.a ^ (uint16_t)fetched) & ((uint16_t)the6502.a ^ (uint16_t)temp)) & 0x0080);

	// The negative flag is set to the most significant bit of the result
	the6502.SetFlag(FlagN, temp&uint16(0x80))

	// Load the result into the accumulator (it's 8-bit dont forget!)
	the6502.a = temp & uint16(0x00ff)

	// This instruction has the potential to require an additional clock cycle
	return 1
}

// CreateInstructions is the method that fill 6502 Instructions table
func (the6502 The6502) CreateInstructions() {
	the6502.instructions = []*Instruction{
		&Instruction{"BRK", 7, the6502.Brk, the6502.Imm},
		&Instruction{"ORA", 6, the6502.Ora, the6502.Izx},
		&Instruction{"???", 2, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 8, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 3, the6502.Nop, the6502.Imp},
		&Instruction{"ORA", 3, the6502.Ora, the6502.Zp0},
		&Instruction{"ASL", 5, the6502.Asl, the6502.Zp0},
		&Instruction{"???", 5, the6502.Xxx, the6502.Imp},
		&Instruction{"PHP", 3, the6502.Php, the6502.Imp},
		&Instruction{"ORA", 2, the6502.Ora, the6502.Imm},
		&Instruction{"ASL", 2, the6502.Asl, the6502.Imp},
		&Instruction{"???", 2, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 4, the6502.Nop, the6502.Imp},
		&Instruction{"ORA", 4, the6502.Ora, the6502.Abs},
		&Instruction{"ASL", 6, the6502.Asl, the6502.Abs},
		&Instruction{"???", 6, the6502.Xxx, the6502.Imp},
		&Instruction{"BPL", 2, the6502.Bpl, the6502.Rel},
		&Instruction{"ORA", 5, the6502.Ora, the6502.Izy},
		&Instruction{"???", 2, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 8, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 4, the6502.Nop, the6502.Imp},
		&Instruction{"ORA", 4, the6502.Ora, the6502.Zpx},
		&Instruction{"ASL", 6, the6502.Asl, the6502.Zpx},
		&Instruction{"???", 6, the6502.Xxx, the6502.Imp},
		&Instruction{"CLC", 2, the6502.Clc, the6502.Imp},
		&Instruction{"ORA", 4, the6502.Ora, the6502.Aby},
		&Instruction{"???", 2, the6502.Nop, the6502.Imp},
		&Instruction{"???", 7, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 4, the6502.Nop, the6502.Imp},
		&Instruction{"ORA", 4, the6502.Ora, the6502.Abx},
		&Instruction{"ASL", 7, the6502.Asl, the6502.Abx},
		&Instruction{"???", 7, the6502.Xxx, the6502.Imp},
		&Instruction{"JSR", 6, the6502.Jsr, the6502.Abs},
		&Instruction{"AND", 6, the6502.And, the6502.Izx},
		&Instruction{"???", 2, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 8, the6502.Xxx, the6502.Imp},
		&Instruction{"BIT", 3, the6502.Bit, the6502.Zp0},
		&Instruction{"AND", 3, the6502.And, the6502.Zp0},
		&Instruction{"ROL", 5, the6502.Rol, the6502.Zp0},
		&Instruction{"???", 5, the6502.Xxx, the6502.Imp},
		&Instruction{"PLP", 4, the6502.Plp, the6502.Imp},
		&Instruction{"AND", 2, the6502.And, the6502.Imm},
		&Instruction{"ROL", 2, the6502.Rol, the6502.Imp},
		&Instruction{"???", 2, the6502.Xxx, the6502.Imp},
		&Instruction{"BIT", 4, the6502.Bit, the6502.Abs},
		&Instruction{"AND", 4, the6502.And, the6502.Abs},
		&Instruction{"ROL", 6, the6502.Rol, the6502.Abs},
		&Instruction{"???", 6, the6502.Xxx, the6502.Imp},
		&Instruction{"BMI", 2, the6502.Bmi, the6502.Rel},
		&Instruction{"AND", 5, the6502.And, the6502.Izy},
		&Instruction{"???", 2, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 8, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 4, the6502.Nop, the6502.Imp},
		&Instruction{"AND", 4, the6502.And, the6502.Zpx},
		&Instruction{"ROL", 6, the6502.Rol, the6502.Zpx},
		&Instruction{"???", 6, the6502.Xxx, the6502.Imp},
		&Instruction{"SEC", 2, the6502.Sec, the6502.Imp},
		&Instruction{"AND", 4, the6502.And, the6502.Aby},
		&Instruction{"???", 2, the6502.Nop, the6502.Imp},
		&Instruction{"???", 7, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 4, the6502.Nop, the6502.Imp},
		&Instruction{"AND", 4, the6502.And, the6502.Abx},
		&Instruction{"ROL", 7, the6502.Rol, the6502.Abx},
		&Instruction{"???", 7, the6502.Xxx, the6502.Imp},
		&Instruction{"RTI", 6, the6502.Rti, the6502.Imp},
		&Instruction{"EOR", 6, the6502.Eor, the6502.Izx},
		&Instruction{"???", 2, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 8, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 3, the6502.Nop, the6502.Imp},
		&Instruction{"EOR", 3, the6502.Eor, the6502.Zp0},
		&Instruction{"LSR", 5, the6502.Lsr, the6502.Zp0},
		&Instruction{"???", 5, the6502.Xxx, the6502.Imp},
		&Instruction{"PHA", 3, the6502.Pha, the6502.Imp},
		&Instruction{"EOR", 2, the6502.Eor, the6502.Imm},
		&Instruction{"LSR", 2, the6502.Lsr, the6502.Imp},
		&Instruction{"???", 2, the6502.Xxx, the6502.Imp},
		&Instruction{"JMP", 3, the6502.Jmp, the6502.Abs},
		&Instruction{"EOR", 4, the6502.Eor, the6502.Abs},
		&Instruction{"LSR", 6, the6502.Lsr, the6502.Abs},
		&Instruction{"???", 6, the6502.Xxx, the6502.Imp},
		&Instruction{"BVC", 2, the6502.Bvc, the6502.Rel},
		&Instruction{"EOR", 5, the6502.Eor, the6502.Izy},
		&Instruction{"???", 2, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 8, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 4, the6502.Nop, the6502.Imp},
		&Instruction{"EOR", 4, the6502.Eor, the6502.Zpx},
		&Instruction{"LSR", 6, the6502.Lsr, the6502.Zpx},
		&Instruction{"???", 6, the6502.Xxx, the6502.Imp},
		&Instruction{"CLI", 2, the6502.Cli, the6502.Imp},
		&Instruction{"EOR", 4, the6502.Eor, the6502.Aby},
		&Instruction{"???", 2, the6502.Nop, the6502.Imp},
		&Instruction{"???", 7, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 4, the6502.Nop, the6502.Imp},
		&Instruction{"EOR", 4, the6502.Eor, the6502.Abx},
		&Instruction{"LSR", 7, the6502.Lsr, the6502.Abx},
		&Instruction{"???", 7, the6502.Xxx, the6502.Imp},
		&Instruction{"RTS", 6, the6502.Rts, the6502.Imp},
		&Instruction{"ADC", 6, the6502.Adc, the6502.Izx},
		&Instruction{"???", 2, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 8, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 3, the6502.Nop, the6502.Imp},
		&Instruction{"ADC", 3, the6502.Adc, the6502.Zp0},
		&Instruction{"ROR", 5, the6502.Ror, the6502.Zp0},
		&Instruction{"???", 5, the6502.Xxx, the6502.Imp},
		&Instruction{"PLA", 4, the6502.Pla, the6502.Imp},
		&Instruction{"ADC", 2, the6502.Adc, the6502.Imm},
		&Instruction{"ROR", 2, the6502.Ror, the6502.Imp},
		&Instruction{"???", 2, the6502.Xxx, the6502.Imp},
		&Instruction{"JMP", 5, the6502.Jmp, the6502.Ind},
		&Instruction{"ADC", 4, the6502.Adc, the6502.Abs},
		&Instruction{"ROR", 6, the6502.Ror, the6502.Abs},
		&Instruction{"???", 6, the6502.Xxx, the6502.Imp},
		&Instruction{"BVS", 2, the6502.Bvs, the6502.Rel},
		&Instruction{"ADC", 5, the6502.Adc, the6502.Izy},
		&Instruction{"???", 2, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 8, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 4, the6502.Nop, the6502.Imp},
		&Instruction{"ADC", 4, the6502.Adc, the6502.Zpx},
		&Instruction{"ROR", 6, the6502.Ror, the6502.Zpx},
		&Instruction{"???", 6, the6502.Xxx, the6502.Imp},
		&Instruction{"SEI", 2, the6502.Sei, the6502.Imp},
		&Instruction{"ADC", 4, the6502.Adc, the6502.Aby},
		&Instruction{"???", 2, the6502.Nop, the6502.Imp},
		&Instruction{"???", 7, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 4, the6502.Nop, the6502.Imp},
		&Instruction{"ADC", 4, the6502.Adc, the6502.Abx},
		&Instruction{"ROR", 7, the6502.Ror, the6502.Abx},
		&Instruction{"???", 7, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 2, the6502.Nop, the6502.Imp},
		&Instruction{"STA", 6, the6502.Sta, the6502.Izx},
		&Instruction{"???", 2, the6502.Nop, the6502.Imp},
		&Instruction{"???", 6, the6502.Xxx, the6502.Imp},
		&Instruction{"STY", 3, the6502.Sty, the6502.Zp0},
		&Instruction{"STA", 3, the6502.Sta, the6502.Zp0},
		&Instruction{"STX", 3, the6502.Stx, the6502.Zp0},
		&Instruction{"???", 3, the6502.Xxx, the6502.Imp},
		&Instruction{"DEY", 2, the6502.Dey, the6502.Imp},
		&Instruction{"???", 2, the6502.Nop, the6502.Imp},
		&Instruction{"TXA", 2, the6502.Txa, the6502.Imp},
		&Instruction{"???", 2, the6502.Xxx, the6502.Imp},
		&Instruction{"STY", 4, the6502.Sty, the6502.Abs},
		&Instruction{"STA", 4, the6502.Sta, the6502.Abs},
		&Instruction{"STX", 4, the6502.Stx, the6502.Abs},
		&Instruction{"???", 4, the6502.Xxx, the6502.Imp},
		&Instruction{"BCC", 2, the6502.Bcc, the6502.Rel},
		&Instruction{"STA", 6, the6502.Sta, the6502.Izy},
		&Instruction{"???", 2, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 6, the6502.Xxx, the6502.Imp},
		&Instruction{"STY", 4, the6502.Sty, the6502.Zpx},
		&Instruction{"STA", 4, the6502.Sta, the6502.Zpx},
		&Instruction{"STX", 4, the6502.Stx, the6502.Zpy},
		&Instruction{"???", 4, the6502.Xxx, the6502.Imp},
		&Instruction{"TYA", 2, the6502.Tya, the6502.Imp},
		&Instruction{"STA", 5, the6502.Sta, the6502.Aby},
		&Instruction{"TXS", 2, the6502.Txs, the6502.Imp},
		&Instruction{"???", 5, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 5, the6502.Nop, the6502.Imp},
		&Instruction{"STA", 5, the6502.Sta, the6502.Abx},
		&Instruction{"???", 5, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 5, the6502.Xxx, the6502.Imp},
		&Instruction{"LDY", 2, the6502.Ldy, the6502.Imm},
		&Instruction{"LDA", 6, the6502.Lda, the6502.Izx},
		&Instruction{"LDX", 2, the6502.Ldx, the6502.Imm},
		&Instruction{"???", 6, the6502.Xxx, the6502.Imp},
		&Instruction{"LDY", 3, the6502.Ldy, the6502.Zp0},
		&Instruction{"LDA", 3, the6502.Lda, the6502.Zp0},
		&Instruction{"LDX", 3, the6502.Ldx, the6502.Zp0},
		&Instruction{"???", 3, the6502.Xxx, the6502.Imp},
		&Instruction{"TAY", 2, the6502.Tay, the6502.Imp},
		&Instruction{"LDA", 2, the6502.Lda, the6502.Imm},
		&Instruction{"TAX", 2, the6502.Tax, the6502.Imp},
		&Instruction{"???", 2, the6502.Xxx, the6502.Imp},
		&Instruction{"LDY", 4, the6502.Ldy, the6502.Abs},
		&Instruction{"LDA", 4, the6502.Lda, the6502.Abs},
		&Instruction{"LDX", 4, the6502.Ldx, the6502.Abs},
		&Instruction{"???", 4, the6502.Xxx, the6502.Imp},
		&Instruction{"BCS", 2, the6502.Bcs, the6502.Rel},
		&Instruction{"LDA", 5, the6502.Lda, the6502.Izy},
		&Instruction{"???", 2, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 5, the6502.Xxx, the6502.Imp},
		&Instruction{"LDY", 4, the6502.Ldy, the6502.Zpx},
		&Instruction{"LDA", 4, the6502.Lda, the6502.Zpx},
		&Instruction{"LDX", 4, the6502.Ldx, the6502.Zpy},
		&Instruction{"???", 4, the6502.Xxx, the6502.Imp},
		&Instruction{"CLV", 2, the6502.Clv, the6502.Imp},
		&Instruction{"LDA", 4, the6502.Lda, the6502.Aby},
		&Instruction{"TSX", 2, the6502.Tsx, the6502.Imp},
		&Instruction{"???", 4, the6502.Xxx, the6502.Imp},
		&Instruction{"LDY", 4, the6502.Ldy, the6502.Abx},
		&Instruction{"LDA", 4, the6502.Lda, the6502.Abx},
		&Instruction{"LDX", 4, the6502.Ldx, the6502.Aby},
		&Instruction{"???", 4, the6502.Xxx, the6502.Imp},
		&Instruction{"CPY", 2, the6502.Cpy, the6502.Imm},
		&Instruction{"CMP", 6, the6502.Cmp, the6502.Izx},
		&Instruction{"???", 2, the6502.Nop, the6502.Imp},
		&Instruction{"???", 8, the6502.Xxx, the6502.Imp},
		&Instruction{"CPY", 3, the6502.Cpy, the6502.Zp0},
		&Instruction{"CMP", 3, the6502.Cmp, the6502.Zp0},
		&Instruction{"DEC", 5, the6502.Dec, the6502.Zp0},
		&Instruction{"???", 5, the6502.Xxx, the6502.Imp},
		&Instruction{"INY", 2, the6502.Iny, the6502.Imp},
		&Instruction{"CMP", 2, the6502.Cmp, the6502.Imm},
		&Instruction{"DEX", 2, the6502.Dex, the6502.Imp},
		&Instruction{"???", 2, the6502.Xxx, the6502.Imp},
		&Instruction{"CPY", 4, the6502.Cpy, the6502.Abs},
		&Instruction{"CMP", 4, the6502.Cmp, the6502.Abs},
		&Instruction{"DEC", 6, the6502.Dec, the6502.Abs},
		&Instruction{"???", 6, the6502.Xxx, the6502.Imp},
		&Instruction{"BNE", 2, the6502.Bne, the6502.Rel},
		&Instruction{"CMP", 5, the6502.Cmp, the6502.Izy},
		&Instruction{"???", 2, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 8, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 4, the6502.Nop, the6502.Imp},
		&Instruction{"CMP", 4, the6502.Cmp, the6502.Zpx},
		&Instruction{"DEC", 6, the6502.Dec, the6502.Zpx},
		&Instruction{"???", 6, the6502.Xxx, the6502.Imp},
		&Instruction{"CLD", 2, the6502.Cld, the6502.Imp},
		&Instruction{"CMP", 4, the6502.Cmp, the6502.Aby},
		&Instruction{"NOP", 2, the6502.Nop, the6502.Imp},
		&Instruction{"???", 7, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 4, the6502.Nop, the6502.Imp},
		&Instruction{"CMP", 4, the6502.Cmp, the6502.Abx},
		&Instruction{"DEC", 7, the6502.Dec, the6502.Abx},
		&Instruction{"???", 7, the6502.Xxx, the6502.Imp},
		&Instruction{"CPX", 2, the6502.Cpx, the6502.Imm},
		&Instruction{"SBC", 6, the6502.Sbc, the6502.Izx},
		&Instruction{"???", 2, the6502.Nop, the6502.Imp},
		&Instruction{"???", 8, the6502.Xxx, the6502.Imp},
		&Instruction{"CPX", 3, the6502.Cpx, the6502.Zp0},
		&Instruction{"SBC", 3, the6502.Sbc, the6502.Zp0},
		&Instruction{"INC", 5, the6502.Inc, the6502.Zp0},
		&Instruction{"???", 5, the6502.Xxx, the6502.Imp},
		&Instruction{"INX", 2, the6502.Inx, the6502.Imp},
		&Instruction{"SBC", 2, the6502.Sbc, the6502.Imm},
		&Instruction{"NOP", 2, the6502.Nop, the6502.Imp},
		&Instruction{"???", 2, the6502.Sbc, the6502.Imp},
		&Instruction{"CPX", 4, the6502.Cpx, the6502.Abs},
		&Instruction{"SBC", 4, the6502.Sbc, the6502.Abs},
		&Instruction{"INC", 6, the6502.Inc, the6502.Abs},
		&Instruction{"???", 6, the6502.Xxx, the6502.Imp},
		&Instruction{"BEQ", 2, the6502.Beq, the6502.Rel},
		&Instruction{"SBC", 5, the6502.Sbc, the6502.Izy},
		&Instruction{"???", 2, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 8, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 4, the6502.Nop, the6502.Imp},
		&Instruction{"SBC", 4, the6502.Sbc, the6502.Zpx},
		&Instruction{"INC", 6, the6502.Inc, the6502.Zpx},
		&Instruction{"???", 6, the6502.Xxx, the6502.Imp},
		&Instruction{"SED", 2, the6502.Sed, the6502.Imp},
		&Instruction{"SBC", 4, the6502.Sbc, the6502.Aby},
		&Instruction{"NOP", 2, the6502.Nop, the6502.Imp},
		&Instruction{"???", 7, the6502.Xxx, the6502.Imp},
		&Instruction{"???", 4, the6502.Nop, the6502.Imp},
		&Instruction{"SBC", 4, the6502.Sbc, the6502.Abx},
		&Instruction{"INC", 7, the6502.Inc, the6502.Abx},
		&Instruction{"???", 7, the6502.Xxx, the6502.Imp},
	}
}

// Reads an 8-bit byte from the bus, located at the specified 16-bit address
func read(a uint16) uint8 {
	return 0
}

// Writes a byte to the bus at the specified address
func write(a uint16, d uint8) {

}

func main() {
	the6502 := The6502{}

	the6502.CreateInstructions()

	the6502.instructions[0].Addrmode()
	the6502.instructions[0].Operate()

	the6502.reset()
	the6502.irq()
	the6502.nmi()
	the6502.clock()
}
