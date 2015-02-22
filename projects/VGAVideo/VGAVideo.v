module UpCounter(clock,reset,count,data_o);
   parameter Size = 8;

   input wire [('b1) - ('b1):0] clock;
   input wire [('b1) - ('b1):0] reset;
   input wire [('b1) - ('b1):0] count;
   
   output reg [(Size) - ('b1):0] data_o;

   always @ (posedge clock) begin
      if (reset) begin
	 data_o <= {Size{1'b0}};
      end
      else begin
	 if (count) begin
	    data_o <= data_o + 1;
	 end
      end
   end
endmodule // UpCounter

module UDCounter(clock,reset,count,direction,data_o);
   parameter Size = 8;

   input wire [('b1) - ('b1):0] clock;
   input wire [('b1) - ('b1):0] reset;
   input wire [('b1) - ('b1):0] count;
   input wire [('b1) - ('b1):0] direction;

   output reg [(Size) - ('b1):0] data_o;

   always @ (posedge clock) begin
      if (reset) begin
	 data_o <= {Size{1'b0}};
      end
      else begin
	 if (count) begin
	    case (direction)
	      'b0: data_o <= data_o + 1;
	      'b1: data_o <= data_o - 1;
	    endcase
	 end
      end
   end
endmodule // UDCounter

module Mux2(select,data_i00,data_i01,data_o);
   parameter Size = 8;

   input wire [('d1) - ('b1):0] select;
   input wire [(Size) - ('b1):0] data_i00;
   input wire [(Size) - ('b1):0] data_i01;

   output reg [(Size) - ('b1):0] data_o;

   always @ (select or data_i00 or data_i01) begin
     case (select)
       'b0:data_o = data_i00;
       'b1:data_o = data_i01;
     endcase // case (select)
   end
endmodule // Mux2

module Mux4(select,data_i00,data_i01,data_i02,data_i03,data_o);
   parameter Size = 8;

   input wire [('d2) - ('b1):0] select;
   input wire [(Size) - ('b1):0] data_i00;
   input wire [(Size) - ('b1):0] data_i01;
   input wire [(Size) - ('b1):0] data_i02;
   input wire [(Size) - ('b1):0] data_i03;

   output reg [(Size) - ('b1):0] data_o;

   always @ (select or 
	     data_i00 or data_i01 or data_i02 or data_i03) begin
      case (select)
	'b00: data_o = data_i00;
	'b01: data_o = data_i01;
	'b10: data_o = data_i02;
	'b11: data_o = data_i03;
      endcase
   end
endmodule // Mux4

module Mux8(select,data_i00,data_i01,data_i02,data_i03,data_i04,data_i05,data_i06,data_i07,data_o);
   parameter Size = 8;

   input wire [('d3) - ('b1):0] select;
   input wire [(Size) - ('b1):0] data_i00;
   input wire [(Size) - ('b1):0] data_i01;
   input wire [(Size) - ('b1):0] data_i02;
   input wire [(Size) - ('b1):0] data_i03;
   input wire [(Size) - ('b1):0] data_i04;
   input wire [(Size) - ('b1):0] data_i05;
   input wire [(Size) - ('b1):0] data_i06;
   input wire [(Size) - ('b1):0] data_i07;

   output reg [(Size) - ('b1):0] data_o;

   always @ (select or 
	     data_i00 or data_i01 or data_i02 or data_i03 or data_i04 or data_i05 or data_i06 or data_i07) begin
      case (select)
	'b000: data_o = data_i00;
	'b001: data_o = data_i01;
	'b010: data_o = data_i02;
	'b011: data_o = data_i03;
	'b100: data_o = data_i04;
	'b101: data_o = data_i05;
	'b110: data_o = data_i06;
	'b111: data_o = data_i07;
      endcase
   end
endmodule // Mux8

module Mux16(select,data_i00,data_i01,data_i02,data_i03,data_i04,data_i05,data_i06,data_i07,data_i08,data_i09,data_i10,data_i11,data_i12,data_i13,data_i14,data_i15,data_o);
   parameter Size = 8;

   input wire [('d4) - ('b1):0] select;
   input wire [(Size) - ('b1):0] data_i00;
   input wire [(Size) - ('b1):0] data_i01;
   input wire [(Size) - ('b1):0] data_i02;
   input wire [(Size) - ('b1):0] data_i03;
   input wire [(Size) - ('b1):0] data_i04;
   input wire [(Size) - ('b1):0] data_i05;
   input wire [(Size) - ('b1):0] data_i06;
   input wire [(Size) - ('b1):0] data_i07;
   input wire [(Size) - ('b1):0] data_i08;
   input wire [(Size) - ('b1):0] data_i09;
   input wire [(Size) - ('b1):0] data_i10;
   input wire [(Size) - ('b1):0] data_i11;
   input wire [(Size) - ('b1):0] data_i12;
   input wire [(Size) - ('b1):0] data_i13;
   input wire [(Size) - ('b1):0] data_i14;
   input wire [(Size) - ('b1):0] data_i15;

   output reg [(Size) - ('b1):0] data_o;

   always @ (select or 
	     data_i00 or data_i01 or data_i02 or data_i03 or data_i04 or data_i05 or data_i06 or data_i07 or
	     data_i08 or data_i09 or data_i10 or data_i11 or data_i12 or data_i13 or data_i14 or data_i15) begin
      case (select)
	'b0000: data_o = data_i00;
	'b0001: data_o = data_i01;
	'b0010: data_o = data_i02;
	'b0011: data_o = data_i03;
	'b0100: data_o = data_i04;
	'b0101: data_o = data_i05;
	'b0110: data_o = data_i06;
	'b0111: data_o = data_i07;
	'b1000: data_o = data_i08;
	'b1001: data_o = data_i09;
	'b1010: data_o = data_i10;
	'b1011: data_o = data_i11;
	'b1100: data_o = data_i12;
	'b1101: data_o = data_i13;
	'b1110: data_o = data_i14;
	'b1111: data_o = data_i15;
      endcase
   end
endmodule // Mux16

module Mux32(select,data_i00,data_i01,data_i02,data_i03,data_i04,data_i05,data_i06,data_i07,data_i08,data_i09,data_i10,data_i11,data_i12,data_i13,data_i14,data_i15,data_i16,data_i17,data_i18,data_i19,data_i20,data_i21,data_i22,data_i23,data_i24,data_i25,data_i26,data_i27,data_i28,data_i29,data_i30,data_i31,data_o);
   parameter Size = 8;

   input wire [('d5) - ('b1):0] select;
   input wire [(Size) - ('b1):0] data_i00;
   input wire [(Size) - ('b1):0] data_i01;
   input wire [(Size) - ('b1):0] data_i02;
   input wire [(Size) - ('b1):0] data_i03;
   input wire [(Size) - ('b1):0] data_i04;
   input wire [(Size) - ('b1):0] data_i05;
   input wire [(Size) - ('b1):0] data_i06;
   input wire [(Size) - ('b1):0] data_i07;
   input wire [(Size) - ('b1):0] data_i08;
   input wire [(Size) - ('b1):0] data_i09;
   input wire [(Size) - ('b1):0] data_i10;
   input wire [(Size) - ('b1):0] data_i11;
   input wire [(Size) - ('b1):0] data_i12;
   input wire [(Size) - ('b1):0] data_i13;
   input wire [(Size) - ('b1):0] data_i14;
   input wire [(Size) - ('b1):0] data_i15;
   input wire [(Size) - ('b1):0] data_i16;
   input wire [(Size) - ('b1):0] data_i17;
   input wire [(Size) - ('b1):0] data_i18;
   input wire [(Size) - ('b1):0] data_i19;
   input wire [(Size) - ('b1):0] data_i20;
   input wire [(Size) - ('b1):0] data_i21;
   input wire [(Size) - ('b1):0] data_i22;
   input wire [(Size) - ('b1):0] data_i23;
   input wire [(Size) - ('b1):0] data_i24;
   input wire [(Size) - ('b1):0] data_i25;
   input wire [(Size) - ('b1):0] data_i26;
   input wire [(Size) - ('b1):0] data_i27;
   input wire [(Size) - ('b1):0] data_i28;
   input wire [(Size) - ('b1):0] data_i29;
   input wire [(Size) - ('b1):0] data_i30;
   input wire [(Size) - ('b1):0] data_i31;

   output reg [(Size) - ('b1):0] data_o;

   always @ (select or 
	     data_i00 or data_i01 or data_i02 or data_i03 or data_i04 or data_i05 or data_i06 or data_i07 or
	     data_i08 or data_i09 or data_i10 or data_i11 or data_i12 or data_i13 or data_i14 or data_i15 or
	     data_i16 or data_i17 or data_i18 or data_i19 or data_i20 or data_i21 or data_i22 or data_i23 or
	     data_i24 or data_i25 or data_i26 or data_i27 or data_i28 or data_i29 or data_i30 or data_i31) begin
      case (select)
	'b00000: data_o = data_i00;
	'b00001: data_o = data_i01;
	'b00010: data_o = data_i02;
	'b00011: data_o = data_i03;
	'b00100: data_o = data_i04;
	'b00101: data_o = data_i05;
	'b00110: data_o = data_i06;
	'b00111: data_o = data_i07;
	'b01000: data_o = data_i08;
	'b01001: data_o = data_i09;
	'b01010: data_o = data_i10;
	'b01011: data_o = data_i11;
	'b01100: data_o = data_i12;
	'b01101: data_o = data_i13;
	'b01110: data_o = data_i14;
	'b01111: data_o = data_i15;
	'b10000: data_o = data_i16;
	'b10001: data_o = data_i17;
	'b10010: data_o = data_i18;
	'b10011: data_o = data_i19;
	'b10100: data_o = data_i20;
	'b10101: data_o = data_i21;
	'b10110: data_o = data_i22;
	'b10111: data_o = data_i23;
	'b11000: data_o = data_i24;
	'b11001: data_o = data_i25;
	'b11010: data_o = data_i26;
	'b11011: data_o = data_i27;
	'b11100: data_o = data_i28;
	'b11101: data_o = data_i29;
	'b11110: data_o = data_i30;
	'b11111: data_o = data_i31;
      endcase
   end
endmodule // Mux32

module Reg(clock,reset,data_i,writeEn,data_o);
   parameter Size = 8;

   input wire [('d1) - ('b1):0]  clock;
   input wire [('d1) - ('b1):0]  reset;
   input wire [(Size) - ('b1):0] data_i;
   input wire [('d1) - ('b1):0]  writeEn;

   output reg [(Size) - ('b1):0] data_o;

   always @ (posedge clock) begin
      if (reset) begin
	 data_o <= {Size{1'b0}};
      end
      else begin
	if (writeEn) begin
	   data_o <= data_i;
	end
      end
   end
endmodule // Reg

module FPGADCM(clock,reset,locked,clock_o0,clock_o90,clock_o180,clock_o270,clock_o2x,clock_o2x180);
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
   wire FPGASPDCM_CLK0;

   assign clock_o0 = FPGASPDCM_CLK0;
   
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
   FPGASPDCM (.CLKIN(clock),
	      .CLKFB(FPGABUFG_o),
	      .RST(reset),
	      .DSSEN(0),
	      .PSINCDEC(0),
	      .PSEN(0),
	      .PSCLK(0),

	      .LOCKED(locked),
	      .CLK0(FPGASPDCM_CLK0),
	      .CLK90(clock_o90),
	      .CLK180(clock_o180),
	      .CLK270(clock_o270),
	      .CLK2X(clock_o2x),
	      .CLK2X180(clock_o2x180));

   BUFG
   FPGABUFG (.I(FPGASPDCM_CLK0),
	     .O(FPGABUFG_o));
endmodule // DCM

module VGAVideo(
    input wire [('b1) - ('b1):0] clock,
    input wire [('b1) - ('b1):0] reset,
    
    output wire [('b1) - ('b1):0] vga_r,
    output wire [('b1) - ('b1):0] vga_g,
    output wire [('b1) - ('b1):0] vga_b,
    output wire [('b1) - ('b1):0] vga_hsync,
    output wire [('b1) - ('b1):0] vga_vsync);

  parameter hack_hcounter_Size = 'b1011;
  parameter hack_vcounter_Size = 'b1010;

  wire [(hack_hcounter_Size) - ('b1):0] hcounter_data_o;
  wire [(hack_vcounter_Size) - ('b1):0] vcounter_data_o;

  assign vga_r = 'b1;
  assign vga_g = 'b0;
  assign vga_b = 'b0;
  assign vga_hsync = (hcounter_data_o['b1010:'b101] == 'b0);
  assign vga_vsync = (vcounter_data_o == 'b0);

  UpCounter #(
      .Size('b1011))
    hcounter(
      .clock(clock),
      .reset((hcounter_data_o == 'b1011111111)),
      .count('b1),
    
      .data_o(hcounter_data_o));
  UpCounter #(
      .Size('b1010))
    vcounter(
      .clock(clock),
      .reset('b0),
      .count('b1),
    
      .data_o(vcounter_data_o));

endmodule // VGAVideo
