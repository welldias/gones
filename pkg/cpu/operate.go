package cpu

// OperateType is the type of Operate instructions.
type OperateType string

// The enum list of Operate instructions
const (
	OperateTypeAdc OperateType = "ADC"
	OperateTypeAnd OperateType = "AND"
	OperateTypeAsl OperateType = "ASL"
	OperateTypeBcc OperateType = "BCC"
	OperateTypeBcs OperateType = "BCS"
	OperateTypeBeq OperateType = "BEQ"
	OperateTypeBit OperateType = "BIT"
	OperateTypeBmi OperateType = "BMI"
	OperateTypeBne OperateType = "BNE"
	OperateTypeBpl OperateType = "BPL"
	OperateTypeBrk OperateType = "BRK"
	OperateTypeBvc OperateType = "BVC"
	OperateTypeBvs OperateType = "BVS"
	OperateTypeClc OperateType = "CLC"
	OperateTypeCld OperateType = "CLD"
	OperateTypeCli OperateType = "CLI"
	OperateTypeClv OperateType = "CLV"
	OperateTypeCmp OperateType = "CMP"
	OperateTypeCpx OperateType = "CPX"
	OperateTypeCpy OperateType = "CPY"
	OperateTypeDec OperateType = "DEC"
	OperateTypeDex OperateType = "DEX"
	OperateTypeDey OperateType = "DEY"
	OperateTypeEor OperateType = "EOR"
	OperateTypeInc OperateType = "INC"
	OperateTypeInx OperateType = "INX"
	OperateTypeIny OperateType = "INY"
	OperateTypeJmp OperateType = "JMP"
	OperateTypeJsr OperateType = "JSR"
	OperateTypeLda OperateType = "LDA"
	OperateTypeLdx OperateType = "LDX"
	OperateTypeLdy OperateType = "LDY"
	OperateTypeLsr OperateType = "LSR"
	OperateTypeNop OperateType = "NOP"
	OperateTypeOra OperateType = "ORA"
	OperateTypePha OperateType = "PHA"
	OperateTypePhp OperateType = "PHP"
	OperateTypePla OperateType = "PLA"
	OperateTypePlp OperateType = "PLP"
	OperateTypeRol OperateType = "ROL"
	OperateTypeRor OperateType = "ROR"
	OperateTypeRti OperateType = "RTI"
	OperateTypeRts OperateType = "RTS"
	OperateTypeSbc OperateType = "SBC"
	OperateTypeSec OperateType = "SEC"
	OperateTypeSed OperateType = "SED"
	OperateTypeSei OperateType = "SEI"
	OperateTypeSta OperateType = "STA"
	OperateTypeStx OperateType = "STX"
	OperateTypeSty OperateType = "STY"
	OperateTypeTax OperateType = "TAX"
	OperateTypeTay OperateType = "TAY"
	OperateTypeTsx OperateType = "TSX"
	OperateTypeTxa OperateType = "TXA"
	OperateTypeTxs OperateType = "TXS"
	OperateTypeTya OperateType = "TYA"
	OperateTypeUnk OperateType = "???"
)
