package bus

// BUS is the bus of nes system
type BUS struct {
	ram []uint8
}

// Config is
func (bus BUS) Config() {
	bus.ram = make([]uint8, 64*1024)
}

// Reads an 8-bit byte from the bus, located at the specified 16-bit address
func (bus BUS) Read(a uint16) uint8 {
	return bus.ram[a]
}

// Writes a byte to the bus at the specified address
func (bus BUS) Write(a uint16, d uint8) {
	bus.ram[a] = d
}
