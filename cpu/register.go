package cpu

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
