package tinyui

// Draw is simple
type Draw struct {
	pixelMode PixelMode
	value     Color
}

// SetPixelMode change the pixel mode for different optimisations
func (draw Draw) SetPixelMode(mode PixelMode) {
	draw.pixelMode = mode
}

// DrawString draws a single line of text
func (draw Draw) DrawString(x int, y int, text string, color Color, scale uint32) {

}
