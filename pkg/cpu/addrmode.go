package cpu

// AddrModeType is the type of AddrMode instructions.
type AddrModeType string

// The enum list of AddrMode instructions
const (
	AddrModeTypeAbs AddrModeType = "Abs"
	AddrModeTypeAbx AddrModeType = "Abx"
	AddrModeTypeAby AddrModeType = "Aby"
	AddrModeTypeImm AddrModeType = "Imm"
	AddrModeTypeImp AddrModeType = "Imp"
	AddrModeTypeInd AddrModeType = "Ind"
	AddrModeTypeIzx AddrModeType = "Izx"
	AddrModeTypeIzy AddrModeType = "Izy"
	AddrModeTypeRel AddrModeType = "Rel"
	AddrModeTypeZp0 AddrModeType = "Zp0"
	AddrModeTypeZpx AddrModeType = "Zpx"
	AddrModeTypeZpy AddrModeType = "Zpy"
)
