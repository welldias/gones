package main

// The6502Instruction is the opcode translation table.
type The6502Instruction struct {
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
	instructions   [256]The6502Instruction
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

// Flags6502 is the status register flag.
type Flags6502 uint8

// The status register stores 8 flags.
const (
	FlagC Flags6502 = (1 << 0) // Carry Bit
	FlagZ Flags6502 = (1 << 1) // Zero
	FlagI Flags6502 = (1 << 2) // Disable Interrupts
	FlagD Flags6502 = (1 << 3) // Decimal Mode (unused in this implementation)
	FlagB Flags6502 = (1 << 4) // Break
	FlagU Flags6502 = (1 << 5) // Unused
	FlagV Flags6502 = (1 << 6) // Overflow
	FlagN Flags6502 = (1 << 7) // Negative
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
func (the6502 The6502) GetFlag(flag Flags6502) uint8 {
	if (the6502.status & uint8(flag)) > 0 {
		return 1
	}
	return 0
}

// SetFlag sets or clears a specific bit of the status register
func (the6502 The6502) SetFlag(flag Flags6502, v bool) {
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

// Reads an 8-bit byte from the bus, located at the specified 16-bit address
func read(a uint16) uint8 {
	return 0
}

// Writes a byte to the bus at the specified address
func write(a uint16, d uint8) {

}

func main() {
	the6502 := The6502{}

	the6502.reset()
	the6502.irq()
	the6502.nmi()
	the6502.clock()
}
