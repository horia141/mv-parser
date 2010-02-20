module DCM(clock,reset,locked,clock_o0,clock_o90,clock_o180,clock_o270,clock_o2x,clock_o2x180);
   input wire [('b1) - ('b1):0] clock;
   input wire [('b1) - ('b1):0] reset;

   output wire [('b1) - ('b1):0] locked;

   output wire [('b1) - ('b1):0] clock_o0;
   output wire [('b1) - ('b1):0] clock_o90;
   output wire [('b1) - ('b1):0] clock_o180;
   output wire [('b1) - ('b1):0] clock_o270;
   output wire [('b1) - ('b1):0] clock_o2x;
   output wire [('b1) - ('b1):0] clock_o2x180;

   wire FPGABUFG_o;
   wire FPGADCM_CLK0;

   assign clock_o0 = FPGADCM_CLK0;
   
   DCM_SP #(.CLKDV_DIVIDE(2.0), 
	    .CLKFX_DIVIDE(1),
	    .CLKFX_MULTIPLY(4),
	    .CLKIN_DIVIDE_BY_2("FALSE"),
	    .CLKIN_PERIOD(0.0),
	    .CLKOUT_PHASE_SHIFT("NONE"),
	    .CLK_FEEDBACK("1X"),
	    .DESKEW_ADJUST("SYSTEM_SYNCHRONOUS"),
	    .DLL_FREQUENCY_MODE("LOW"),
	    .DUTY_CYCLE_CORRECTION("TRUE"),
	    .PHASE_SHIFT(0),
	    .STARTUP_WAIT("FALSE"))
   FPGADCM (.CLKIN(clock),
	    .CLKFB(FPGABUFG_o),
	    .RST(reset),
	    .DSSEN(0),
	    .PSINCDEC(0),
	    .PSEN(0),
	    .PSCLK(0),

	    .LOCKED(locked),
	    .CLK0(FPGADCM_CLK0),
	    .CLK90(clock_o90),
	    .CLK180(clock_o180),
	    .CLK270(clock_o270),
	    .CLK2X(clock_o2x),
	    .CLK2X180(clock_o2x180));

   BUFG
   FPGABUFG (.I(FPGADCM_CLK0),
	     .O(FPGABUFG_o));
endmodule // DCM
