`timescale 1ns/1ps

module top;
   reg clock;
   reg clock90;
   reg reset;

   wire dac_sck;
   wire dac_cs;
   wire dac_clr;
   wire dac_mosi;

   wire dis_spi_ss_b;
   wire dis_amp_cs;
   wire dis_conv;
   wire dis_sf_ce0;
   wire dis_fpga_init_b;
   
   initial begin
      $dumpfile("./projects/RickRoll/RickRoll.vcd");
      $dumpvars;
            
      #400000 $finish;
   end

   initial begin
      #0 clock = 0;
      
      forever #2 clock = ~clock;
   end

   initial begin
      #1 clock90 = 0;

      forever #2 clock90 = ~clock90;
   end

   initial begin
      #0 reset = 0;
      #2 reset = 1;
      #6 reset = 0;
   end

   RR_Module rr(clock,clock90,reset,dac_sck,dac_cs,dac_clr,dac_mosi,dis_spi_ss_b,dis_amp_cs,dis_conv,dis_sf_ce0,dis_fpga_init_b);
endmodule // top

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

`define DACCommand 'b0011

`define DACAddress 'b0000

`define TicksPerCommand 'b11111

`define TicksPerNote 'b11100001

`define NoteAmplitudeMin 'b0

`define NoteAmplitudeMax 'b111111111111

module RR_Control(
    input wire[('b1) - ('b1):0] clock,
    input wire[('b1) - ('b1):0] reset,
    input wire[('b1) - ('b1):0] dcm_locked,
    input wire[('b1000) - ('b1):0] tickCounter,
    input wire[('b1100) - ('b1):0] noteCounter,
    
    output wire[('b1) - ('b1):0] dac_cs,
    output wire[('b1) - ('b1):0] dac_clr,
    output wire[('b1) - ('b1):0] tickCounter_reset,
    output wire[('b1) - ('b1):0] tickCounter_count,
    output wire[('b1) - ('b1):0] noteCounter_reset,
    output wire[('b1) - ('b1):0] noteCounter_count,
    output wire[('b1) - ('b1):0] noteCounter_direction);

  reg[4-1:0] fsm_state;
  reg[64 * 8 - 1:0] fsm_state_t;
  reg[7-1:0] fsm_output;

  assign dac_cs = fsm_output[(('b0) + ('b1)) - ('b1):'b0];
  assign dac_clr = fsm_output[(('b1) + ('b1)) - ('b1):'b1];
  assign tickCounter_reset = fsm_output[(('b10) + ('b1)) - ('b1):'b10];
  assign tickCounter_count = fsm_output[(('b11) + ('b1)) - ('b1):'b11];
  assign noteCounter_reset = fsm_output[(('b100) + ('b1)) - ('b1):'b100];
  assign noteCounter_count = fsm_output[(('b101) + ('b1)) - ('b1):'b101];
  assign noteCounter_direction = fsm_output[(('b110) + ('b1)) - ('b1):'b110];
  `define RR_Control_Reset 0
  `define RR_Control_WaitDCMLock 1
  `define RR_Control_SendCommandUp 2
  `define RR_Control_PostSendCommandUp 3
  `define RR_Control_WaitCommandUp 4
  `define RR_Control_PostWaitCommandUp 5
  `define RR_Control_PostWaitCommandUp2Up 6
  `define RR_Control_PostWaitCommandUp2Down 7
  `define RR_Control_SendCommandDown 8
  `define RR_Control_PostSendCommandDown 9
  `define RR_Control_WaitCommandDown 10
  `define RR_Control_PostWaitCommandDown 11
  `define RR_Control_PostWaitCommandDown2Down 12
  `define RR_Control_PostWaitCommandDown2Up 13

  `define RR_Control_Reset_t "Reset"
  `define RR_Control_WaitDCMLock_t "WaitDCMLock"
  `define RR_Control_SendCommandUp_t "SendCommandUp"
  `define RR_Control_PostSendCommandUp_t "PostSendCommandUp"
  `define RR_Control_WaitCommandUp_t "WaitCommandUp"
  `define RR_Control_PostWaitCommandUp_t "PostWaitCommandUp"
  `define RR_Control_PostWaitCommandUp2Up_t "PostWaitCommandUp2Up"
  `define RR_Control_PostWaitCommandUp2Down_t "PostWaitCommandUp2Down"
  `define RR_Control_SendCommandDown_t "SendCommandDown"
  `define RR_Control_PostSendCommandDown_t "PostSendCommandDown"
  `define RR_Control_WaitCommandDown_t "WaitCommandDown"
  `define RR_Control_PostWaitCommandDown_t "PostWaitCommandDown"
  `define RR_Control_PostWaitCommandDown2Down_t "PostWaitCommandDown2Down"
  `define RR_Control_PostWaitCommandDown2Up_t "PostWaitCommandDown2Up"

  always @ (posedge clock) begin
    if (reset) begin
      fsm_state <= `RR_Control_Reset;
    end
    else begin
      case (fsm_state)
        `RR_Control_Reset:begin
          fsm_state <= `RR_Control_WaitDCMLock;
          fsm_state_t <= `RR_Control_WaitDCMLock_t;
        end
        `RR_Control_WaitDCMLock:begin
          if ((dcm_locked == 'b0)) begin
            fsm_state <= `RR_Control_WaitDCMLock;
            fsm_state_t <= `RR_Control_WaitDCMLock_t;
          end
          else begin
            fsm_state <= `RR_Control_SendCommandUp;
            fsm_state_t <= `RR_Control_SendCommandUp_t;
          end
        end
        `RR_Control_SendCommandUp:begin
          if ((tickCounter < ('b11111) - ('b1))) begin
            fsm_state <= `RR_Control_SendCommandUp;
            fsm_state_t <= `RR_Control_SendCommandUp_t;
          end
          else begin
            fsm_state <= `RR_Control_PostSendCommandUp;
            fsm_state_t <= `RR_Control_PostSendCommandUp_t;
          end
        end
        `RR_Control_PostSendCommandUp:begin
          fsm_state <= `RR_Control_WaitCommandUp;
          fsm_state_t <= `RR_Control_WaitCommandUp_t;
        end
        `RR_Control_WaitCommandUp:begin
          if ((tickCounter < ('b11100001) - ('b10))) begin
            fsm_state <= `RR_Control_WaitCommandUp;
            fsm_state_t <= `RR_Control_WaitCommandUp_t;
          end
          else begin
            fsm_state <= `RR_Control_PostWaitCommandUp;
            fsm_state_t <= `RR_Control_PostWaitCommandUp_t;
          end
        end
        `RR_Control_PostWaitCommandUp:begin
          if ((noteCounter < 'b111111111111)) begin
            fsm_state <= `RR_Control_PostWaitCommandUp2Up;
            fsm_state_t <= `RR_Control_PostWaitCommandUp2Up_t;
          end
          else begin
            fsm_state <= `RR_Control_PostWaitCommandUp2Down;
            fsm_state_t <= `RR_Control_PostWaitCommandUp2Down_t;
          end
        end
        `RR_Control_PostWaitCommandUp2Up:begin
          fsm_state <= `RR_Control_SendCommandUp;
          fsm_state_t <= `RR_Control_SendCommandUp_t;
        end
        `RR_Control_PostWaitCommandUp2Down:begin
          fsm_state <= `RR_Control_SendCommandDown;
          fsm_state_t <= `RR_Control_SendCommandDown_t;
        end
        `RR_Control_SendCommandDown:begin
          if ((tickCounter < ('b11111) - ('b1))) begin
            fsm_state <= `RR_Control_SendCommandDown;
            fsm_state_t <= `RR_Control_SendCommandDown_t;
          end
          else begin
            fsm_state <= `RR_Control_PostSendCommandDown;
            fsm_state_t <= `RR_Control_PostSendCommandDown_t;
          end
        end
        `RR_Control_PostSendCommandDown:begin
          fsm_state <= `RR_Control_WaitCommandDown;
          fsm_state_t <= `RR_Control_WaitCommandDown_t;
        end
        `RR_Control_WaitCommandDown:begin
          if ((tickCounter < ('b11100001) - ('b1))) begin
            fsm_state <= `RR_Control_WaitCommandDown;
            fsm_state_t <= `RR_Control_WaitCommandDown_t;
          end
          else begin
            fsm_state <= `RR_Control_PostWaitCommandDown;
            fsm_state_t <= `RR_Control_PostWaitCommandDown_t;
          end
        end
        `RR_Control_PostWaitCommandDown:begin
          if ((noteCounter > 'b0)) begin
            fsm_state <= `RR_Control_PostWaitCommandDown2Down;
            fsm_state_t <= `RR_Control_PostWaitCommandDown2Down_t;
          end
          else begin
            fsm_state <= `RR_Control_PostWaitCommandDown2Up;
            fsm_state_t <= `RR_Control_PostWaitCommandDown2Up_t;
          end
        end
        `RR_Control_PostWaitCommandDown2Down:begin
          fsm_state <= `RR_Control_SendCommandDown;
          fsm_state_t <= `RR_Control_SendCommandDown_t;
        end
        `RR_Control_PostWaitCommandDown2Up:begin
          fsm_state <= `RR_Control_SendCommandUp;
          fsm_state_t <= `RR_Control_SendCommandUp_t;
        end
      endcase
    end
  end

  always @ (fsm_state) begin
    case (fsm_state)
      `RR_Control_Reset: begin
        fsm_output = 'b0010111;
      end
      `RR_Control_WaitDCMLock: begin
        fsm_output = 'b0010111;
      end
      `RR_Control_SendCommandUp: begin
        fsm_output = 'b0001000;
      end
      `RR_Control_PostSendCommandUp: begin
        fsm_output = 'b0001000;
      end
      `RR_Control_WaitCommandUp: begin
        fsm_output = 'b0001001;
      end
      `RR_Control_PostWaitCommandUp: begin
        fsm_output = 'b0001001;
      end
      `RR_Control_PostWaitCommandUp2Up: begin
        fsm_output = 'b0100101;
      end
      `RR_Control_PostWaitCommandUp2Down: begin
        fsm_output = 'b1100101;
      end
      `RR_Control_SendCommandDown: begin
        fsm_output = 'b1001000;
      end
      `RR_Control_PostSendCommandDown: begin
        fsm_output = 'b1001000;
      end
      `RR_Control_WaitCommandDown: begin
        fsm_output = 'b1001001;
      end
      `RR_Control_PostWaitCommandDown: begin
        fsm_output = 'b1001001;
      end
      `RR_Control_PostWaitCommandDown2Down: begin
        fsm_output = 'b1100101;
      end
      `RR_Control_PostWaitCommandDown2Up: begin
        fsm_output = 'b0100101;
      end
    endcase
  end
endmodule // RR_Control

module RR_Module(
    input wire [('b1) - ('b1):0] clock,
    input wire [('b1) - ('b1):0] reset,
    
    output wire [('b1) - ('b1):0] dac_sck,
    output wire [('b1) - ('b1):0] dac_cs,
    output wire [('b1) - ('b1):0] dac_clr,
    output wire [('b1) - ('b1):0] dac_mosi,
    output wire [('b1) - ('b1):0] dis_spi_ss_b,
    output wire [('b1) - ('b1):0] dis_amp_cs,
    output wire [('b1) - ('b1):0] dis_conv,
    output wire [('b1) - ('b1):0] dis_sf_ce0,
    output wire [('b1) - ('b1):0] dis_fpga_init_b);



  parameter hack_tickCounter_Size = 'b1000;
  parameter hack_noteCounter_Size = 'b1100;
  parameter hack_cmdSelect_Size = 'b1;

  wire [('b1) - ('b1):0] dcm_locked;
  wire [('b1) - ('b1):0] dcm_clock_o0;
  wire [('b1) - ('b1):0] dcm_clock_o90;
  wire [('b1) - ('b1):0] dcm_clock_o180;
  wire [('b1) - ('b1):0] dcm_clock_o270;
  wire [('b1) - ('b1):0] dcm_clock_o2x;
  wire [('b1) - ('b1):0] dcm_clock_o2x180;
  wire [('b1) - ('b1):0] controlFSM_dac_cs;
  wire [('b1) - ('b1):0] controlFSM_dac_clr;
  wire [('b1) - ('b1):0] controlFSM_tickCounter_reset;
  wire [('b1) - ('b1):0] controlFSM_tickCounter_count;
  wire [('b1) - ('b1):0] controlFSM_noteCounter_reset;
  wire [('b1) - ('b1):0] controlFSM_noteCounter_count;
  wire [('b1) - ('b1):0] controlFSM_noteCounter_direction;
  wire [(hack_tickCounter_Size) - ('b1):0] tickCounter_data_o;
  wire [(hack_noteCounter_Size) - ('b1):0] noteCounter_data_o;
  wire [(hack_cmdSelect_Size) - ('b1):0] cmdSelect_data_o;

  assign dac_sck = dcm_clock_o90;
  assign dac_cs = controlFSM_dac_cs;
  assign dac_clr = controlFSM_dac_clr;
  assign dac_mosi = cmdSelect_data_o;
  assign dis_spi_ss_b = 'b1;
  assign dis_amp_cs = 'b1;
  assign dis_conv = 'b0;
  assign dis_sf_ce0 = 'b1;
  assign dis_fpga_init_b = 'b1;

  FPGADCM
    dcm(
      .clock(clock),
      .reset(reset),
    
      .locked(dcm_locked),
      .clock_o0(dcm_clock_o0),
      .clock_o90(dcm_clock_o90),
      .clock_o180(dcm_clock_o180),
      .clock_o270(dcm_clock_o270),
      .clock_o2x(dcm_clock_o2x),
      .clock_o2x180(dcm_clock_o2x180));
  RR_Control
    controlFSM(
      .clock(clock),
      .reset(reset),
      .dcm_locked(dcm_locked),
      .tickCounter(tickCounter_data_o),
      .noteCounter(noteCounter_data_o),
    
      .dac_cs(controlFSM_dac_cs),
      .dac_clr(controlFSM_dac_clr),
      .tickCounter_reset(controlFSM_tickCounter_reset),
      .tickCounter_count(controlFSM_tickCounter_count),
      .noteCounter_reset(controlFSM_noteCounter_reset),
      .noteCounter_count(controlFSM_noteCounter_count),
      .noteCounter_direction(controlFSM_noteCounter_direction));
  UpCounter #(
      .Size('b1000))
    tickCounter(
      .clock(clock),
      .reset(controlFSM_tickCounter_reset),
      .count(controlFSM_tickCounter_count),
    
      .data_o(tickCounter_data_o));
  UDCounter #(
      .Size('b1100))
    noteCounter(
      .clock(clock),
      .reset(controlFSM_noteCounter_reset),
      .count(controlFSM_noteCounter_count),
      .direction(controlFSM_noteCounter_direction),
    
      .data_o(noteCounter_data_o));
  Mux32 #(
      .Size('b1))
    cmdSelect(
      .select(tickCounter_data_o['b100:'b0]),
      .data_i00('b0),
      .data_i01('b0),
      .data_i02('b0),
      .data_i03('b0),
      .data_i04('b0),
      .data_i05('b0),
      .data_i06('b0),
      .data_i07('b0),
      .data_i08('b0),
      .data_i09('b0),
      .data_i10('b1),
      .data_i11('b1),
      .data_i12('b0),
      .data_i13('b0),
      .data_i14('b0),
      .data_i15('b0),
      .data_i16(noteCounter_data_o['b1011]),
      .data_i17(noteCounter_data_o['b1010]),
      .data_i18(noteCounter_data_o['b1001]),
      .data_i19(noteCounter_data_o['b1000]),
      .data_i20(noteCounter_data_o['b111]),
      .data_i21(noteCounter_data_o['b110]),
      .data_i22(noteCounter_data_o['b101]),
      .data_i23(noteCounter_data_o['b100]),
      .data_i24(noteCounter_data_o['b11]),
      .data_i25(noteCounter_data_o['b10]),
      .data_i26(noteCounter_data_o['b1]),
      .data_i27(noteCounter_data_o['b0]),
      .data_i28('b0),
      .data_i29('b0),
      .data_i30('b0),
      .data_i31('b0),
    
      .data_o(cmdSelect_data_o));

endmodule // RR_Module
